# ================================================
# Berlin-wide Person-Level Mode Use
# (Residents only, no Hundekopf filter)
# ================================================

rm(list = ls())
gc()

library(data.table)

# ------------------------------------------------
# 文件路径
# ------------------------------------------------

paths <- list(
  base_trips = "D:/10pct/base-10pct-500/base-10pct-500.output_trips.csv.gz",
  base_persons = "D:/10pct/base-10pct-500/base-10pct-500.output_persons.csv.gz",
  
  maut_trips = "D:/10pct/maut-10pct-500/maut-10pct-500.output_trips.csv.gz",
  maut_persons = "D:/10pct/maut-10pct-500/maut-10pct-500.output_persons.csv.gz",
  
  park_trips = "D:/10pct/parking-10pct-500/parking-10pct-500.output_trips.csv.gz",
  park_persons = "D:/10pct/parking-10pct-500/parking-10pct-500.output_persons.csv.gz"
)

output_dir <- "D:/10pct"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# ------------------------------------------------
# 分析函数
# ------------------------------------------------

analyze_scenario <- function(trips_path, persons_path, scenario_name){
  
  cat("Processing", scenario_name, "...\n")
  
  persons <- fread(persons_path)
  trips <- fread(trips_path)
  
  # 自动识别 id 列
  id_col <- intersect(c("id","Id","personId","person"), names(persons))
  setnames(persons, id_col[1], "person_id")
  
  if("person" %in% names(trips)) setnames(trips,"person","person_id")
  
  # ------------------------------------------------
  # 只保留居民
  # ------------------------------------------------
  
  persons_resident <- persons[!grepl("freight|commercial|goods", person_id)]
  
  resident_ids <- persons_resident$person_id
  
  trips <- trips[person_id %in% resident_ids]
  
  # ------------------------------------------------
  # person-level mode use
  # ------------------------------------------------
  
  summary <- trips[, .(
    persons_using_mode = uniqueN(person_id)
  ), by = main_mode]
  
  summary[, scenario := scenario_name]
  
  setnames(summary, "main_mode", "mode")
  
  return(summary)
}

# ------------------------------------------------
# 运行三个场景
# ------------------------------------------------

base_res <- analyze_scenario(paths$base_trips, paths$base_persons, "base")
maut_res <- analyze_scenario(paths$maut_trips, paths$maut_persons, "maut")
park_res <- analyze_scenario(paths$park_trips, paths$park_persons, "park")

# ------------------------------------------------
# 合并结果
# ------------------------------------------------

all_results <- rbindlist(list(base_res, maut_res, park_res), fill = TRUE)

# ------------------------------------------------
# 转换为对比表
# ------------------------------------------------

person_compare <- dcast(
  all_results,
  mode ~ scenario,
  value.var = "persons_using_mode",
  fill = 0
)

# 计算变化

person_compare[, delta_maut := maut - base]
person_compare[, delta_park := park - base]

setcolorder(person_compare,
            c("mode","base","maut","delta_maut","park","delta_park"))

setnames(person_compare,
         c("mode","base","maut","delta_maut","park","delta_park"),
         c("Mode","Base","Maut","ΔMaut","Parking","ΔParking"))

# ------------------------------------------------
# 保存结果
# ------------------------------------------------

output_file <- file.path(output_dir,
                         "berlin_person_mode_use_residents.csv")

fwrite(person_compare, output_file)

cat("✅ Person-level Berlin results saved to:\n", output_file, "\n")

print(person_compare)