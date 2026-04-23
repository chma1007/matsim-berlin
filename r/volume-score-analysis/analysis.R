# ==== MATSim 1% | Base vs. Maut vs. Parking ====
# analysis.R — 对齐 SimWrapper 口径（居民识别 + 模式集合 + 可选权重）+ 诊断输出

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(networkD3)
  library(htmlwidgets)
})

# ---------- 文件路径 ----------
base_trips   <- "C:/Users/Mia/berlin output new/base-1pct-500/base-1pct-500.output_trips.csv.gz"
base_persons <- "C:/Users/Mia/berlin output new/base-1pct-500/base-1pct-500.output_persons.csv.gz"
base_money   <- "C:/Users/Mia/berlin output new/base-1pct-500/base-1pct-500.output_personMoneyEvents.tsv.gz"

maut_trips   <- "C:/Users/Mia/berlin output new/maut-1pct-500/maut-1pct-500.output_trips.csv.gz"
maut_persons <- "C:/Users/Mia/berlin output new/maut-1pct-500/maut-1pct-500.output_persons.csv.gz"
maut_money   <- "C:/Users/Mia/berlin output new/maut-1pct-500/maut-1pct-500.output_personMoneyEvents.tsv.gz"

park_trips   <- "C:/Users/Mia/berlin output new/parking-1pct-500/parking-1pct-500.output_trips.csv.gz"
park_persons <- "C:/Users/Mia/berlin output new/parking-1pct-500/parking-1pct-500.output_persons.csv.gz"
park_money   <- "C:/Users/Mia/berlin output new/parking-1pct-500/parking-1pct-500.output_personMoneyEvents.tsv.gz"

# ---------- 读入 ----------
message("Reading trips & persons ...")
tr_base <- fread(base_trips)
tr_maut <- fread(maut_trips)
tr_park <- fread(park_trips)

persons_base <- fread(base_persons)
persons_maut <- fread(maut_persons)
persons_park <- fread(park_persons)

# ---------- 工具函数 ----------
find_col <- function(dt, candidates, required = TRUE, fallback = NULL){
  hit <- intersect(candidates, names(dt))
  if (length(hit) >= 1) return(hit[1])
  if (!is.null(fallback)) return(fallback)
  if (required) stop(sprintf("缺少这些列之一：%s", paste(candidates, collapse=", ")))
  return(NULL)
}

# 统一 trips：person / trip_id / mode / trav_time(sec) / distance(m)
standardize_trips <- function(dt){
  person_col <- find_col(dt, c("person","person_id","agent","agent_id","id"))
  mode_col   <- find_col(dt, c("mode","main_mode","legMode","leg_mode","transport_mode"))
  if (!"trip_id" %in% names(dt)) {
    dt <- dt %>% group_by(.data[[person_col]]) %>% mutate(trip_id = dplyr::row_number()) %>% ungroup()
  }
  tripid_col <- "trip_id"
  trav_col <- find_col(dt, c("trav_time","travel_time","leg_trav_time","duration"))
  dist_col <- find_col(dt, c("traveled_distance","distance","dist","leg_distance",
                             "euclidean_distance","beelineDistance",
                             "route_distance","network_distance","link_distance"),
                       required = FALSE, fallback = NA_character_)
  
  out <- dt %>%
    rename(person    = all_of(person_col),
           mode_raw  = all_of(mode_col),
           trip_id   = all_of(tripid_col),
           trav_time = all_of(trav_col))
  
  if (!is.na(dist_col)) out <- out %>% rename(distance = all_of(dist_col)) else out$distance <- NA_real_
  
  out$mode <- tolower(as.character(out$mode_raw))
  out$mode[out$mode %in% c("bicycle")] <- "bike"
  
  to_num <- function(x){
    if (is.numeric(x)) return(as.numeric(x))
    suppressWarnings({
      hhmmss <- grepl("^\\d{1,2}:\\d{2}:\\d{2}$", x)
      y <- rep(NA_real_, length(x))
      if (any(hhmmss)) {
        parts <- do.call(rbind, strsplit(x[hhmmss], ":"))
        secs <- as.numeric(parts[,1])*3600 + as.numeric(parts[,2])*60 + as.numeric(parts[,3])
        y[hhmmss] <- secs
      }
      y[!hhmmss] <- as.numeric(x[!hhmmss])
      y
    })
  }
  out$trav_time <- to_num(out$trav_time)
  out$distance  <- to_num(out$distance)
  
  out %>% select(person, trip_id, mode, trav_time, distance, everything())
}

tr_base <- standardize_trips(tr_base)
tr_maut <- standardize_trips(tr_maut)
tr_park <- standardize_trips(tr_park)

# ---------- 从 persons 精确识别“居民”和“权重” ----------
# id 列：兼容 person/id/person_id/agent/agent_id
detect_person_id_col <- function(dt){
  find_col(dt, c("person","id","person_id","agent","agent_id"))
}
# 居民标签列
detect_resident_col <- function(dt){
  hits <- intersect(c("resident","is_resident","isResident","isresident","subpopulation"), names(dt))
  if (length(hits)) hits[1] else NULL
}
# 权重列（可选）
detect_weight_col <- function(dt){
  hits <- intersect(c("weight","sample_weight","sampleWeight","w","personWeight","expansion_factor"), names(dt))
  if (length(hits)) hits[1] else NULL
}

get_resident_ids_and_weights <- function(persons_dt){
  id_col  <- detect_person_id_col(persons_dt)
  if (is.null(id_col)) stop("persons 文件里找不到 person id 列（person/id/person_id/agent/agent_id）")
  res_col <- detect_resident_col(persons_dt)
  w_col   <- detect_weight_col(persons_dt)
  
  ids <- persons_dt[[id_col]]
  # 识别居民
  if (!is.null(res_col)) {
    vals <- tolower(as.character(persons_dt[[res_col]]))
    is_res <- rep(FALSE, length(vals))
    if (any(vals %in% c("resident","residents"))) {
      is_res <- vals %in% c("resident","residents")
    } else if (all(na.omit(unique(vals)) %in% c("0","1","true","false"))) {
      is_res <- vals %in% c("1","true")
    } else {
      # 不识别的情况下默认全体为居民（再靠前缀法兜底）
      is_res <- rep(TRUE, length(vals))
    }
  } else {
    # 没居民列：先全体 TRUE，后续在 trips 里用前缀法再过滤一次
    is_res <- rep(TRUE, length(ids))
  }
  
  weight <- if (!is.null(w_col)) suppressWarnings(as.numeric(persons_dt[[w_col]])) else rep(1, length(ids))
  weight[!is.finite(weight)] <- 1
  
  data.frame(person = as.character(ids), is_resident = is_res, weight = weight, stringsAsFactors = FALSE)
}

res_base <- get_resident_ids_and_weights(persons_base)
res_maut <- get_resident_ids_and_weights(persons_maut)
res_park <- get_resident_ids_and_weights(persons_park)

# ---------- 合并 persons → trips，精确筛选居民 + 权重 ----------
merge_people <- function(trips_dt, res_df){
  dt <- trips_dt %>% left_join(res_df, by="person")
  # 兜底：若 persons 没居民列（全 TRUE），再用前缀法剔除明显的非居民
  dt$is_resident[is.na(dt$is_resident)] <- TRUE
  dt <- dt %>% filter(is_resident, !grepl("^(commercial|freight|goods)_", person))
  # 权重缺失→1
  dt$weight[is.na(dt$weight)] <- 1
  dt
}
rb <- merge_people(tr_base, res_base)
rm <- merge_people(tr_maut, res_maut)
rp <- merge_people(tr_park, res_park)

# ---------- 只保留五大模式（贴近 SimWrapper） ----------
valid_modes <- c("walk","bike","pt","car","ride")
rb <- rb %>% filter(mode %in% valid_modes)
rm <- rm %>% filter(mode %in% valid_modes)
rp <- rp %>% filter(mode %in% valid_modes)

# ---------- 诊断输出 ----------
diag <- data.frame(
  scenario = c("Base","Maut","Parking"),
  trips_total = c(nrow(tr_base), nrow(tr_maut), nrow(tr_park)),
  trips_after_resident_filter = c(nrow(rb), nrow(rm), nrow(rp)),
  residents_in_persons = c(sum(res_base$is_resident), sum(res_maut$is_resident), sum(res_park$is_resident)),
  weight_mean = c(mean(rb$weight, na.rm=TRUE), mean(rm$weight, na.rm=TRUE), mean(rp$weight, na.rm=TRUE))
)
fwrite(diag, "diagnostics_modal_split.csv")

# ---------- Modal Split（加权计数） ----------
modal_split_w <- function(dt, scen){
  dt %>%
    group_by(mode) %>%
    summarise(w = sum(weight, na.rm=TRUE), .groups="drop") %>%
    mutate(share = w / sum(w), scenario = scen)
}
ms <- bind_rows(
  modal_split_w(rb,"Base"),
  modal_split_w(rm,"Maut"),
  modal_split_w(rp,"Parking")
)

p_ms <- ggplot(ms, aes(x=scenario, y=share, fill=mode)) +
  geom_col(position="fill") +
  scale_y_continuous(labels=percent) +
  labs(title="Modal Split (Residents, SimWrapper-like modes, weighted if available)",
       y="Share", x=NULL) +
  theme_minimal()
ggsave("modal_split_residents_simwrapper_like.png", p_ms, width=8, height=5, dpi=200)

ms_wide <- ms %>% pivot_wider(names_from=mode, values_from=share)
fwrite(ms_wide, "modal_split_residents_simwrapper_like.csv")

# ---------- Car KPI（仍按居民+五大模式；不加权，保持直观）
kpi_car <- function(dt, scen){
  car <- dt %>% filter(mode=="car")
  car <- car %>%
    mutate(trav_time_min = trav_time/60,
           dist_km       = distance/1000,
           speed_kmh     = ifelse(is.finite(dist_km) & trav_time_min>0, dist_km/(trav_time_min/60), NA_real_))
  summarise(car,
            trips         = n(),
            avg_time_min  = mean(trav_time_min, na.rm=TRUE),
            p95_time_min  = quantile(trav_time_min, 0.95, na.rm=TRUE),
            avg_dist_km   = mean(dist_km, na.rm=TRUE),
            p95_dist_km   = quantile(dist_km, 0.95, na.rm=TRUE),
            avg_speed_kmh = mean(speed_kmh, na.rm=TRUE),
            VKT_million   = sum(dist_km, na.rm=TRUE)/1e6,
            VHT_thousand  = sum(trav_time_min, na.rm=TRUE)/60
  ) %>% mutate(scenario=scen)
}
car_kpis <- bind_rows(kpi_car(rb,"Base"), kpi_car(rm,"Maut"), kpi_car(rp,"Parking"))
fwrite(car_kpis, "car_kpis.csv")

# ---------- Mode shift（Base -> Maut / Parking），同口径（不加权）
prep_core <- function(dt, scen) dt %>% select(person, trip_id, mode) %>% mutate(scenario=scen)
core <- bind_rows(prep_core(rb,"Base"), prep_core(rm,"Maut"), prep_core(rp,"Parking"))
keep_ids <- core %>% count(person, trip_id) %>% filter(n==3) %>% select(person, trip_id)
core2 <- core %>% inner_join(keep_ids, by=c("person","trip_id")) %>% pivot_wider(names_from=scenario, values_from=mode)

shift_matrix <- function(df, to_col){
  df %>% count(Base, !!rlang::sym(to_col)) %>%
    group_by(Base) %>% mutate(share = n/sum(n)) %>% ungroup() %>% arrange(desc(n))
}
shift_maut <- shift_matrix(core2, "Maut")
shift_park <- shift_matrix(core2, "Parking")
fwrite(shift_maut, "shift_base_to_maut.csv")
fwrite(shift_park, "shift_base_to_parking.csv")

# ---------- 支付汇总（负值=个人支出=系统收入） ----------
safe_read_money <- function(file, scen){
  if (file.exists(file)) {
    out <- tryCatch(fread(file), error=function(e) data.table())
    if(nrow(out)) out$scenario <- scen
    out
  } else data.table()
}
pm <- rbindlist(list(
  safe_read_money(maut_money, "Maut"),
  safe_read_money(park_money, "Parking")
), use.names=TRUE, fill=TRUE)

if (nrow(pm) > 0){
  pay_summary <- pm %>%
    group_by(scenario) %>%
    summarise(total_payment = sum(amount, na.rm=TRUE),
              persons = n_distinct(person),
              avg_payment_per_person = total_payment / persons) %>%
    arrange(desc(total_payment))
  fwrite(pay_summary, "payments_summary.csv")
}

# ---------- 桑基图（HTML） ----------
build_sankey <- function(shift_df, to_name, top_n = 12, file_out = "sankey.html"){
  if(nrow(shift_df)==0) return(invisible(NULL))
  df <- shift_df %>% arrange(desc(n)) %>% head(top_n)
  nodes <- data.frame(name = unique(c(as.character(df$Base), as.character(df[[to_name]]))), stringsAsFactors = FALSE)
  get_id <- function(x) match(x, nodes$name) - 1
  links <- data.frame(
    source = get_id(as.character(df$Base)),
    target = get_id(as.character(df[[to_name]])),
    value  = df$n
  )
  p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "source", Target = "target",
                     Value = "value", NodeID = "name", fontSize = 12, nodeWidth = 24)
  htmlwidgets::saveWidget(p, file_out, selfcontained = TRUE)
}
build_sankey(shift_maut, "Maut",    12, "sankey_base_to_maut.html")
build_sankey(shift_park, "Parking", 12, "sankey_base_to_parking.html")

message("Done. Files written: ",
        paste(c("modal_split_residents_simwrapper_like.png",
                "modal_split_residents_simwrapper_like.csv",
                "diagnostics_modal_split.csv",
                "car_kpis.csv",
                "shift_base_to_maut.csv",
                "shift_base_to_parking.csv",
                if(nrow(pm)>0) "payments_summary.csv" else NULL,
                "sankey_base_to_maut.html",
                "sankey_base_to_parking.html"), collapse=", "))
