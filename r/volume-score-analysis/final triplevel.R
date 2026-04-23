# ===============================================================
# Activity-based Charge Analysis (10% Sample)
# Maut vs Parking
# ===============================================================

rm(list = ls())
gc()

library(data.table)
library(ggplot2)

cat("🚀 Starting Activity-based Charge Analysis...\n")

# ---------------------------------------------------------------
# 1️⃣ 路径
# ---------------------------------------------------------------

maut_events_path <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_personMoneyEvents.tsv.gz"
park_events_path <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_personMoneyEvents.tsv.gz"

maut_trips_path  <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_trips.csv.gz"
park_trips_path  <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_trips.csv.gz"

# ---------------------------------------------------------------
# 2️⃣ 读取 PersonMoneyEvents
# ---------------------------------------------------------------

cat("📥 Reading PersonMoneyEvents...\n")

maut <- fread(maut_events_path)
park <- fread(park_events_path)

# 只保留支出
maut <- maut[amount < 0]
park <- park[amount < 0]

maut[, amount := abs(amount)]
park[, amount := abs(amount)]

# 保留必要字段
maut <- maut[, .(person, time, amount)]
park <- park[, .(person, time, amount)]

# ---------------------------------------------------------------
# 3️⃣ 读取 Trips（注意 sep=";"）
# ---------------------------------------------------------------

cat("📥 Reading Trips...\n")

maut_trips <- fread(maut_trips_path, sep=";")
park_trips <- fread(park_trips_path, sep=";")

# 强制转 numeric（解决你之前报错问题）
maut_trips[, dep_time := as.numeric(dep_time)]
maut_trips[, trav_time := as.numeric(trav_time)]

park_trips[, dep_time := as.numeric(dep_time)]
park_trips[, trav_time := as.numeric(trav_time)]

# 删除 NA（防止出错）
maut_trips <- maut_trips[!is.na(dep_time) & !is.na(trav_time)]
park_trips <- park_trips[!is.na(dep_time) & !is.na(trav_time)]

# 计算 arrival time
maut_trips[, arr_time := dep_time + trav_time]
park_trips[, arr_time := dep_time + trav_time]

# 只保留必要字段
maut_trips <- maut_trips[, .(person, arr_time, end_activity_type)]
park_trips <- park_trips[, .(person, arr_time, end_activity_type)]

# ---------------------------------------------------------------
# 4️⃣ 排序用于 rolling join
# ---------------------------------------------------------------

setkey(maut_trips, person, arr_time)
setkey(park_trips, person, arr_time)

setkey(maut, person, time)
setkey(park, person, time)

# ---------------------------------------------------------------
# 5️⃣ 关键步骤：匹配收费事件到“到达后的 activity”
# ---------------------------------------------------------------
# roll = -Inf 表示：
# 找 arr_time >= event_time 的第一条记录
# 这是最严谨的做法

cat("🔄 Matching Maut events to activities...\n")
maut_merged <- maut_trips[maut, roll = -Inf]

cat("🔄 Matching Parking events to activities...\n")
park_merged <- park_trips[park, roll = -Inf]

# 去掉无法匹配的
maut_merged <- maut_merged[!is.na(end_activity_type)]
park_merged <- park_merged[!is.na(end_activity_type)]

# ---------------------------------------------------------------
# 6️⃣ 按 Activity 汇总收费
# ---------------------------------------------------------------

maut_activity <- maut_merged[, .(total_charge = sum(amount)), 
                             by = end_activity_type]
maut_activity[, policy := "Maut"]

park_activity <- park_merged[, .(total_charge = sum(amount)), 
                             by = end_activity_type]
park_activity[, policy := "Parking"]

activity_df <- rbind(maut_activity, park_activity)

# ---------------------------------------------------------------
# 7️⃣ 输出结果
# ---------------------------------------------------------------

cat("\n=== Activity-based Charge Summary ===\n")
print(activity_df)

# ---------------------------------------------------------------
# 8️⃣ 可视化
# ---------------------------------------------------------------

p <- ggplot(activity_df, 
            aes(x = end_activity_type, 
                y = total_charge/1000, 
                fill = policy)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Charges by Activity Type (10%)",
       x = "Activity Type",
       y = "Total Charge (×1000 €)") +
  theme_minimal(base_size = 13)

print(p)

# ---------------------------------------------------------------
# 9️⃣ 保存表格
# ---------------------------------------------------------------

output_path <- "D:/10pct/results/activity_charge_comparison_10pct.csv"
fwrite(activity_df, output_path)

cat("✅ Activity charge table saved to:\n", output_path, "\n")
