library(xml2)
library(data.table)

# =========================
# 1. 文件路径
# =========================
base_cfg    <- "D:/10pct/base-10pct-500/base-10pct-500.output_config.xml"
maut_cfg    <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_config.xml"
parking_cfg <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_config.xml"

# =========================
# 2. 提取 scoring 模块信息
# =========================
extract_scoring_module <- function(config_file, scenario_name) {
  doc <- read_xml(config_file)
  
  # --- 2.1 scoring 模块下的直接 param ---
  general_nodes <- xml_find_all(doc, ".//module[@name='scoring']/param")
  general_dt <- data.table(
    scenario = scenario_name,
    name = xml_attr(general_nodes, "name"),
    value = xml_attr(general_nodes, "value")
  )
  
  # --- 2.2 scoringParameters ---
  sp_nodes <- xml_find_all(doc, ".//module[@name='scoring']//parameterset[@type='scoringParameters']")
  
  sp_list <- lapply(seq_along(sp_nodes), function(i) {
    node <- sp_nodes[[i]]
    p <- xml_find_all(node, "./param")
    vals <- setNames(xml_attr(p, "value"), xml_attr(p, "name"))
    
    # 全部 param 原样展开
    if (length(vals) == 0) {
      return(NULL)
    }
    
    data.table(
      scenario = scenario_name,
      scoring_set_id = i,
      param_name = names(vals),
      param_value = as.character(vals)
    )
  })
  
  scoring_params_dt <- rbindlist(sp_list, fill = TRUE)
  
  # --- 2.3 activityParams ---
  act_nodes <- xml_find_all(doc, ".//module[@name='scoring']//parameterset[@type='activityParams']")
  
  act_list <- lapply(seq_along(act_nodes), function(i) {
    node <- act_nodes[[i]]
    p <- xml_find_all(node, "./param")
    vals <- setNames(xml_attr(p, "value"), xml_attr(p, "name"))
    
    data.table(
      scenario = scenario_name,
      activity_id = i,
      activityType = if ("activityType" %in% names(vals)) vals["activityType"] else NA_character_,
      typicalDuration = if ("typicalDuration" %in% names(vals)) vals["typicalDuration"] else NA_character_,
      openingTime = if ("openingTime" %in% names(vals)) vals["openingTime"] else NA_character_,
      closingTime = if ("closingTime" %in% names(vals)) vals["closingTime"] else NA_character_,
      latestStartTime = if ("latestStartTime" %in% names(vals)) vals["latestStartTime"] else NA_character_,
      earliestEndTime = if ("earliestEndTime" %in% names(vals)) vals["earliestEndTime"] else NA_character_
    )
  })
  
  activity_dt <- rbindlist(act_list, fill = TRUE)
  
  # --- 2.4 mode-related params（不预设结构，直接抓含 mode/travel 的 scoring params）---
  mode_like_dt <- scoring_params_dt[
    grepl("mode|travel|distance|money|constant|marginal", param_name, ignore.case = TRUE)
  ]
  
  return(list(
    general = general_dt,
    scoring_params = scoring_params_dt,
    activity = activity_dt,
    mode_like = mode_like_dt
  ))
}

# =========================
# 3. 提取三个 scenario
# =========================
base_res    <- extract_scoring_module(base_cfg, "Base")
maut_res    <- extract_scoring_module(maut_cfg, "Maut")
parking_res <- extract_scoring_module(parking_cfg, "Parking")

# =========================
# 4. 合并
# =========================
general_all <- rbindlist(list(base_res$general, maut_res$general, parking_res$general), fill = TRUE)
scoring_all <- rbindlist(list(base_res$scoring_params, maut_res$scoring_params, parking_res$scoring_params), fill = TRUE)
activity_all <- rbindlist(list(base_res$activity, maut_res$activity, parking_res$activity), fill = TRUE)
mode_like_all <- rbindlist(list(base_res$mode_like, maut_res$mode_like, parking_res$mode_like), fill = TRUE)

# =========================
# 5. 看最关键的 general scoring 参数
# =========================
general_key <- general_all[
  grepl("money|perform|late|early|wait|travel|line", name, ignore.case = TRUE)
]

cat("\n===== General scoring parameters =====\n")
print(general_key)

cat("\n===== Scoring parameters (first 100 rows) =====\n")
print(head(scoring_all, 100))

cat("\n===== Activity parameters (first 50 rows) =====\n")
print(head(activity_all, 50))

cat("\n===== Mode / travel / money related scoring params =====\n")
print(mode_like_all)

# =========================
# 6. 导出
# =========================
out_dir <- "D:/10pct/travel_time_analysis"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

fwrite(general_all, file.path(out_dir, "scoring_general_all.csv"))
fwrite(general_key, file.path(out_dir, "scoring_general_key.csv"))
fwrite(scoring_all, file.path(out_dir, "scoring_parameters_all.csv"))
fwrite(activity_all, file.path(out_dir, "scoring_activity_parameters.csv"))
fwrite(mode_like_all, file.path(out_dir, "scoring_mode_like_parameters.csv"))

cat("\nSaved files:\n")
cat(file.path(out_dir, "scoring_general_all.csv"), "\n")
cat(file.path(out_dir, "scoring_general_key.csv"), "\n")
cat(file.path(out_dir, "scoring_parameters_all.csv"), "\n")
cat(file.path(out_dir, "scoring_activity_parameters.csv"), "\n")
cat(file.path(out_dir, "scoring_mode_like_parameters.csv"), "\n")