# ================================================
# Hundekopf mode share analysis (residents only)
# 输出 raw table + 差值对比表
# ================================================

rm(list = ls())
gc()

library(data.table)
library(sf)

# -----------------------------
# 文件路径
# -----------------------------
paths <- list(
  hundekopf = "D:/berlin/input/v6.4/berlin hundekopf/berlin_hundekopf_ONLY_25832.shp",
  base_trips = "D:/10pct/base-10pct-500/base-10pct-500.output_trips.csv.gz",
  base_persons = "D:/10pct/base-10pct-500/base-10pct-500.output_persons.csv.gz",
  maut_trips = "D:/10pct/maut-10pct-500/maut-10pct-500.output_trips.csv.gz",
  maut_persons = "D:/10pct/maut-10pct-500/maut-10pct-500.output_persons.csv.gz",
  park_trips = "D:/10pct/parking-10pct-500/parking-10pct-500.output_trips.csv.gz",
  park_persons = "D:/10pct/parking-10pct-500/parking-10pct-500.output_persons.csv.gz"
)

output_dir <- "D:/10pct"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -----------------------------
# 读取 Hundekopf shapefile
# -----------------------------
hundekopf <- st_read(paths$hundekopf, quiet = TRUE)
hundekopf <- st_transform(hundekopf, 25832)

# -----------------------------
# 分析函数
# -----------------------------
analyze_scenario <- function(trips_path, persons_path, scenario_name) {
  cat("Processing", scenario_name, "...\n")
  
  persons <- fread(persons_path)
  trips <- fread(trips_path)
  
  # 自动识别 persons 的 id 列
  id_col <- intersect(c("id", "Id", "personId", "person"), names(persons))
  if (length(id_col) == 0) {
    stop(paste("❌ 无法在", scenario_name, "的 persons 文件中找到 id 列"))
  }
  setnames(persons, id_col[1], "person_id")
  
  # trips 中 person 列统一改名
  if ("person" %in% names(trips)) setnames(trips, "person", "person_id")
  
  # 只保留居民
  persons_resident <- persons[!grepl("freight|commercial|goods", person_id)]
  resident_ids <- persons_resident$person_id
  trips <- trips[person_id %in% resident_ids]
  
  # 检查坐标列
  if (!all(c("start_x", "start_y") %in% names(trips))) {
    stop(paste("❌ Missing start_x/start_y in", scenario_name))
  }
  
  # 转为 sf 点
  trips_sf <- st_as_sf(
    trips,
    coords = c("start_x", "start_y"),
    crs = 25832,
    remove = FALSE
  )
  
  # 筛选起点在 Hundekopf 内的 trip
  trips_in <- trips_sf[hundekopf, , op = st_within]
  trips_in <- as.data.table(st_drop_geometry(trips_in))
  
  # 汇总：每种 mode 的 trip 数 & person 数
  summary <- trips_in[, .(
    trips_in_Hundekopf = .N,
    persons_in_Hundekopf = uniqueN(person_id)
  ), by = .(main_mode)]
  
  summary[, scenario := scenario_name]
  setnames(summary, "main_mode", "mode")
  
  return(summary)
}

# -----------------------------
# 执行三个 scenario
# -----------------------------
base_res <- analyze_scenario(paths$base_trips, paths$base_persons, "base")
maut_res <- analyze_scenario(paths$maut_trips, paths$maut_persons, "maut")
park_res <- analyze_scenario(paths$park_trips, paths$park_persons, "park")

# -----------------------------
# 合并 raw 结果
# -----------------------------
all_results <- rbindlist(list(base_res, maut_res, park_res), fill = TRUE)

# 保存 raw table
raw_csv <- file.path(output_dir, "hundekopf_mode_share_residents_raw.csv")
fwrite(all_results, raw_csv)

cat("✅ 已输出 raw 结果表:\n", raw_csv, "\n")
print(all_results)

# -----------------------------
# 生成 trip 数对比表（不是百分比）
# -----------------------------
trip_compare <- dcast(
  all_results,
  mode ~ scenario,
  value.var = "trips_in_Hundekopf",
  fill = 0
)

# 计算差值
trip_compare[, delta_maut := maut - base]
trip_compare[, delta_park := park - base]

# 调整列顺序
setcolorder(trip_compare, c("mode", "base", "maut", "delta_maut", "park", "delta_park"))

# 改列名
setnames(
  trip_compare,
  c("mode", "base", "maut", "delta_maut", "park", "delta_park"),
  c("Mode", "Base", "Maut", "ΔMaut", "Parking", "ΔParking")
)

# 保存 trip 对比表
trip_compare_csv <- file.path(output_dir, "hundekopf_mode_shift_residents_tripcount.csv")
fwrite(trip_compare, trip_compare_csv)

cat("✅ 已输出 trip count 对比表:\n", trip_compare_csv, "\n")
print(trip_compare)

# -----------------------------
# 如有需要，也可生成人数对比表
# -----------------------------
person_compare <- dcast(
  all_results,
  mode ~ scenario,
  value.var = "persons_in_Hundekopf",
  fill = 0
)

person_compare[, delta_maut := maut - base]
person_compare[, delta_park := park - base]

setcolorder(person_compare, c("mode", "base", "maut", "delta_maut", "park", "delta_park"))

setnames(
  person_compare,
  c("mode", "base", "maut", "delta_maut", "park", "delta_park"),
  c("Mode", "Base", "Maut", "ΔMaut", "Parking", "ΔParking")
)

person_compare_csv <- file.path(output_dir, "hundekopf_mode_shift_residents_personcount.csv")
fwrite(person_compare, person_compare_csv)

cat("✅ 已输出 person count 对比表:\n", person_compare_csv, "\n")
print(person_compare)