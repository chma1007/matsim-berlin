rm(list = ls())
gc()

library(data.table)
library(ggplot2)

cat("🚀 Starting Activity-based Charge Analysis...\n")

# -----------------------------
# 路径
# -----------------------------
maut_events_path <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_personMoneyEvents.tsv.gz"
park_events_path <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_personMoneyEvents.tsv.gz"

maut_trips_path  <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_trips.csv.gz"
park_trips_path  <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_trips.csv.gz"

# -----------------------------
# 读取数据
# -----------------------------
cat("📥 Reading events...\n")
maut <- fread(maut_events_path)
park <- fread(park_events_path)

# 只保留支出
maut <- maut[amount < 0]
park <- park[amount < 0]

maut[, amount := abs(amount)]
park[, amount := abs(amount)]

cat("📥 Reading trips...\n")
maut_trips <- fread(maut_trips_path, sep=";")
park_trips <- fread(park_trips_path, sep=";")

# -----------------------------
# 计算 arrival time
# -----------------------------
maut_trips[, arr_time := dep_time + trav_time]
park_trips[, arr_time := dep_time + trav_time]

# 只保留必要字段
maut_trips <- maut_trips[, .(person, arr_time, end_activity_type)]
park_trips <- park_trips[, .(person, arr_time, end_activity_type)]

# -----------------------------
# 排序用于 rolling join
# -----------------------------
setkey(maut_trips, person, arr_time)
setkey(park_trips, person, arr_time)

setkey(maut, person, time)
setkey(park, person, time)

# -----------------------------
# 匹配收费事件到最近 arrival
# -----------------------------
cat("🔄 Matching Maut events to activities...\n")
maut_merged <- maut_trips[maut, roll = "nearest"]

cat("🔄 Matching Parking events to activities...\n")
park_merged <- park_trips[park, roll = "nearest"]

# -----------------------------
# 按 activity 汇总
# -----------------------------
maut_activity <- maut_merged[, .(total_charge = sum(amount)), 
                             by = end_activity_type]
maut_activity[, policy := "Maut"]

park_activity <- park_merged[, .(total_charge = sum(amount)), 
                             by = end_activity_type]
park_activity[, policy := "Parking"]

activity_df <- rbind(maut_activity, park_activity)

# -----------------------------
# 输出结果
# -----------------------------
print(activity_df)

# -----------------------------
# 可视化
# -----------------------------
ggplot(activity_df, 
       aes(x = end_activity_type, 
           y = total_charge/1000, 
           fill = policy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Charges by Activity Type (10%)",
       x = "Activity Type",
       y = "Total Charge (×1000 €)") +
  theme_minimal(base_size = 13)