library(data.table)
library(ggplot2)

# =========================
# 1. 文件路径
# =========================
base_file    <- "D:/10pct/base-10pct-500/base-10pct-500.output_trips.csv.gz"
maut_file    <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_trips.csv.gz"
parking_file <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_trips.csv.gz"

out_dir <- "D:/10pct/travel_time_analysis"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =========================
# 2. 时间转换函数
# =========================
time_to_seconds <- function(x) {
  if (is.numeric(x)) return(x)
  
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | is.na(x)] <- NA_character_
  
  suppressWarnings(x_num <- as.numeric(x))
  idx <- is.na(x_num) & !is.na(x)
  
  if (any(idx)) {
    parts <- tstrsplit(x[idx], ":", fixed = TRUE)
    if (length(parts) == 3) {
      h <- as.numeric(parts[[1]])
      m <- as.numeric(parts[[2]])
      s <- as.numeric(parts[[3]])
      x_num[idx] <- h * 3600 + m * 60 + s
    } else {
      warning("Some time values are neither numeric nor HH:MM:SS.")
    }
  }
  
  return(x_num)
}

# =========================
# 3. 读取单个 scenario
# =========================
read_trips <- function(file_path, scenario_name) {
  cat("Reading:", scenario_name, "\n")
  
  dt <- fread(file_path)
  
  required_cols <- c("person", "trav_time", "main_mode")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "File %s is missing required columns: %s",
      scenario_name,
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  dt[, scenario := scenario_name]
  dt[, trav_time_sec := time_to_seconds(trav_time)]
  
  dt <- dt[!is.na(trav_time_sec) & !is.na(main_mode) & main_mode != ""]
  
  # 如需排除 freight/commercial 等，可打开下面这段
  # dt <- dt[!grepl("^(freight|commercial|goodsTraffic|service|taxi|pt)", person)]
  
  return(dt)
}

# =========================
# 4. 读取三个 scenario
# =========================
base_dt    <- read_trips(base_file, "Base")
maut_dt    <- read_trips(maut_file, "Maut")
parking_dt <- read_trips(parking_file, "Parking")

trips_all <- rbindlist(list(base_dt, maut_dt, parking_dt), use.names = TRUE, fill = TRUE)

# =========================
# 5. 看一下有哪些 mode
# =========================
cat("\nModes found:\n")
print(sort(unique(trips_all$main_mode)))

# =========================
# 6. 按 scenario + mode 汇总
# =========================
mode_summary <- trips_all[, .(
  n_trips = .N,
  n_persons = uniqueN(person),
  total_travel_time_hr = sum(trav_time_sec, na.rm = TRUE) / 3600,
  avg_travel_time_per_trip_min = mean(trav_time_sec, na.rm = TRUE) / 60,
  median_travel_time_per_trip_min = median(trav_time_sec, na.rm = TRUE) / 60
), by = .(scenario, main_mode)]

# =========================
# 7. 计算相对 Base 的变化
# =========================
base_ref <- mode_summary[scenario == "Base", .(
  main_mode,
  base_avg_trip_min = avg_travel_time_per_trip_min,
  base_total_hr = total_travel_time_hr,
  base_n_trips = n_trips
)]

mode_compare <- merge(
  mode_summary,
  base_ref,
  by = "main_mode",
  all.x = TRUE
)

mode_compare[, delta_avg_trip_min := avg_travel_time_per_trip_min - base_avg_trip_min]
mode_compare[, delta_total_hr := total_travel_time_hr - base_total_hr]
mode_compare[, delta_n_trips := n_trips - base_n_trips]

# 排序
setorder(mode_compare, main_mode, scenario)

# 四舍五入
num_cols <- setdiff(names(mode_compare), c("scenario", "main_mode"))
mode_compare[, (num_cols) := lapply(.SD, function(x) round(x, 2)), .SDcols = num_cols]

cat("\n========== Travel Time by Mode ==========\n")
print(mode_compare)

# =========================
# 8. 导出结果
# =========================
fwrite(mode_compare, file.path(out_dir, "travel_time_by_mode.csv"))

cat("\nSaved:\n")
cat(file.path(out_dir, "travel_time_by_mode.csv"), "\n")

# =========================
# 9. 宽表（更适合看对比）
# =========================
mode_wide <- dcast(
  mode_compare,
  main_mode ~ scenario,
  value.var = "avg_travel_time_per_trip_min"
)

cat("\n========== Avg Travel Time per Trip (Wide) ==========\n")
print(mode_wide)

fwrite(mode_wide, file.path(out_dir, "travel_time_by_mode_wide.csv"))

# =========================
# 10. 画图：不同 scenario 下各 mode 平均 travel time
# =========================
p1 <- ggplot(mode_compare, aes(x = main_mode, y = avg_travel_time_per_trip_min, fill = scenario)) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Travel Time per Trip by Mode",
    x = "Mode",
    y = "Average travel time per trip (min)"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(out_dir, "travel_time_by_mode_barplot.png"),
  plot = p1,
  width = 10,
  height = 6,
  dpi = 300
)

# =========================
# 11. 只看相对 Base 的变化图（更适合汇报）
# =========================
delta_plot_dt <- mode_compare[scenario != "Base"]

p2 <- ggplot(delta_plot_dt, aes(x = main_mode, y = delta_avg_trip_min, fill = scenario)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Change in Average Travel Time per Trip Relative to Base",
    x = "Mode",
    y = "Delta avg travel time per trip (min vs Base)"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(out_dir, "travel_time_by_mode_delta_barplot.png"),
  plot = p2,
  width = 10,
  height = 6,
  dpi = 300
)

cat("\nPlots saved:\n")
cat(file.path(out_dir, "travel_time_by_mode_barplot.png"), "\n")
cat(file.path(out_dir, "travel_time_by_mode_delta_barplot.png"), "\n")