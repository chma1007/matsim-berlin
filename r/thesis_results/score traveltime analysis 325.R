library(data.table)

# =========================
# 1. 文件路径
# =========================
base_file    <- "D:/10pct/base-10pct-500/base-10pct-500.output_trips.csv.gz"
maut_file    <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_trips.csv.gz"
parking_file <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_trips.csv.gz"

# =========================
# 2. 时间转换函数
#    支持:
#    - 数值秒
#    - "HH:MM:SS" 字符串
# =========================
time_to_seconds <- function(x) {
  # 如果本身就是数值，直接返回
  if (is.numeric(x)) return(x)
  
  x <- as.character(x)
  x <- trimws(x)
  
  # 空值处理
  x[x == "" | is.na(x)] <- NA_character_
  
  # 尝试直接转数值
  suppressWarnings(x_num <- as.numeric(x))
  
  # 哪些是非数值格式（通常是 HH:MM:SS）
  idx <- is.na(x_num) & !is.na(x)
  
  if (any(idx)) {
    parts <- tstrsplit(x[idx], ":", fixed = TRUE)
    
    # 防止不是标准三段格式
    if (length(parts) == 3) {
      h <- as.numeric(parts[[1]])
      m <- as.numeric(parts[[2]])
      s <- as.numeric(parts[[3]])
      x_num[idx] <- h * 3600 + m * 60 + s
    } else {
      warning("Some time values are neither numeric nor HH:MM:SS format.")
    }
  }
  
  return(x_num)
}

# =========================
# 3. 单个 scenario 处理函数
# =========================
analyze_travel_time <- function(file_path, scenario_name) {
  
  cat("Reading:", scenario_name, "\n")
  dt <- fread(file_path)
  
  # 检查关键列
  required_cols <- c("person", "trav_time")
  missing_cols <- setdiff(required_cols, names(dt))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "File %s is missing required columns: %s",
      scenario_name,
      paste(missing_cols, collapse = ", ")
    ))
  }
  
  # 转换 travel time 为秒
  dt[, trav_time_sec := time_to_seconds(trav_time)]
  
  # 删除缺失 travel time 的记录
  dt_valid <- dt[!is.na(trav_time_sec)]
  
  # 如果你想排除非私人出行者，可取消下面注释并按你的 person id 规则调整
  # dt_valid <- dt_valid[
  #   !grepl("^(freight|commercial|goodsTraffic|service|taxi|pt)", person)
  # ]
  
  # 每人总 travel time
  tt_person <- dt_valid[, .(
    total_tt_sec = sum(trav_time_sec, na.rm = TRUE),
    n_trips = .N
  ), by = person]
  
  # 汇总指标
  summary_dt <- data.table(
    scenario = scenario_name,
    n_trips = nrow(dt_valid),
    n_persons = uniqueN(dt_valid$person),
    total_travel_time_sec = sum(dt_valid$trav_time_sec, na.rm = TRUE),
    total_travel_time_hr = sum(dt_valid$trav_time_sec, na.rm = TRUE) / 3600,
    avg_travel_time_per_trip_sec = mean(dt_valid$trav_time_sec, na.rm = TRUE),
    avg_travel_time_per_trip_min = mean(dt_valid$trav_time_sec, na.rm = TRUE) / 60,
    avg_travel_time_per_person_sec = mean(tt_person$total_tt_sec, na.rm = TRUE),
    avg_travel_time_per_person_min = mean(tt_person$total_tt_sec, na.rm = TRUE) / 60,
    median_travel_time_per_person_min = median(tt_person$total_tt_sec, na.rm = TRUE) / 60
  )
  
  # 返回结果
  return(list(
    trips = dt_valid,
    person_tt = tt_person,
    summary = summary_dt
  ))
}

# =========================
# 4. 分别运行三个 scenario
# =========================
base_res    <- analyze_travel_time(base_file, "Base")
maut_res    <- analyze_travel_time(maut_file, "Maut")
parking_res <- analyze_travel_time(parking_file, "Parking")

# =========================
# 5. 合并 summary
# =========================
summary_all <- rbindlist(list(
  base_res$summary,
  maut_res$summary,
  parking_res$summary
), use.names = TRUE, fill = TRUE)

# =========================
# 6. 计算相对 Base 的变化
# =========================
base_avg_person_min <- summary_all[scenario == "Base", avg_travel_time_per_person_min]
base_avg_trip_min   <- summary_all[scenario == "Base", avg_travel_time_per_trip_min]
base_total_hr       <- summary_all[scenario == "Base", total_travel_time_hr]

summary_all[, delta_avg_tt_person_min := avg_travel_time_per_person_min - base_avg_person_min]
summary_all[, delta_avg_tt_trip_min   := avg_travel_time_per_trip_min - base_avg_trip_min]
summary_all[, delta_total_tt_hr       := total_travel_time_hr - base_total_hr]

# 保留更适合展示的列
summary_show <- summary_all[, .(
  scenario,
  n_persons,
  n_trips,
  total_travel_time_hr,
  delta_total_tt_hr,
  avg_travel_time_per_person_min,
  delta_avg_tt_person_min,
  avg_travel_time_per_trip_min,
  delta_avg_tt_trip_min,
  median_travel_time_per_person_min
)]

# 四舍五入
num_cols <- setdiff(names(summary_show), c("scenario"))
summary_show[, (num_cols) := lapply(.SD, function(x) round(x, 2)), .SDcols = num_cols]

# 输出结果
cat("\n========== Travel Time Comparison ==========\n")
print(summary_show)

# =========================
# 7. 导出结果
# =========================
out_dir <- "D:/10pct/travel_time_analysis"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

fwrite(summary_show, file.path(out_dir, "travel_time_summary.csv"))

# 每人 travel time 也导出，方便后面画分布图/箱线图
base_res$person_tt[, scenario := "Base"]
maut_res$person_tt[, scenario := "Maut"]
parking_res$person_tt[, scenario := "Parking"]

person_tt_all <- rbindlist(list(
  base_res$person_tt,
  maut_res$person_tt,
  parking_res$person_tt
), use.names = TRUE, fill = TRUE)

person_tt_all[, total_tt_min := total_tt_sec / 60]
person_tt_all[, total_tt_hr  := total_tt_sec / 3600]

fwrite(person_tt_all, file.path(out_dir, "travel_time_per_person.csv"))

cat("\nResults saved to:\n")
cat(file.path(out_dir, "travel_time_summary.csv"), "\n")
cat(file.path(out_dir, "travel_time_per_person.csv"), "\n")