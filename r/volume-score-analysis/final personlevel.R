# ================================================
# Person-level Mode Shift Analysis (Base vs Maut / Park)
# ================================================

library(data.table)

# 文件路径
paths <- list(
  base_trips = "D:/10pct/base-10pct-500/base-10pct-500.output_trips.csv.gz",
  maut_trips = "D:/10pct/maut-10pct-500/maut-10pct-500.output_trips.csv.gz",
  park_trips = "D:/10pct/parking-10pct-500/parking-10pct-500.output_trips.csv.gz"
)

# 提取每人主mode函数
get_dominant_mode <- function(trip_file, scenario){
  trips <- fread(trip_file)
  trips <- trips[!grepl("freight|commercial|goods", person)]
  trips_main <- trips[, .N, by = .(person, main_mode)]
  dominant <- trips_main[, .SD[which.max(N)], by = person]
  dominant[, scenario := scenario]
  return(dominant[, .(person, main_mode, scenario)])
}

# 分别读取三个场景
base_modes <- get_dominant_mode(paths$base_trips, "base")
maut_modes <- get_dominant_mode(paths$maut_trips, "maut")
park_modes <- get_dominant_mode(paths$park_trips, "park")

# 合并 base vs maut
merge_base_maut <- merge(base_modes, maut_modes, by = "person", suffixes = c("_base", "_maut"))
merge_base_park <- merge(base_modes, park_modes, by = "person", suffixes = c("_base", "_park"))

# 统计转移矩阵
shift_maut <- merge_base_maut[, .N, by = .(main_mode_base, main_mode_maut)]
shift_park <- merge_base_park[, .N, by = .(main_mode_base, main_mode_park)]

# 输出结果
output_dir <- "C:/Users/Mia/Desktop/10pctresults"
fwrite(shift_maut, file.path(output_dir, "person_mode_shift_base_maut.csv"))
fwrite(shift_park, file.path(output_dir, "person_mode_shift_base_park.csv"))

cat("✅ 输出两个转移表:\n")
cat("- Base → Maut: person_mode_shift_base_maut.csv\n")
cat("- Base → Park: person_mode_shift_base_park.csv\n")
