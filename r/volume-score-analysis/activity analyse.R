library(data.table)

############################
# 1️⃣ 读取 events
############################

read_events <- function(path, scenario_name){
  
  events <- fread(path)
  
  # 只保留居民
  events <- events[!grepl("^freight", person)]
  
  events[, scenario := scenario_name]
  
  return(events)
}

base  <- read_events("D:/10pct/base-10pct-500/base-10pct-500.output_events.xml.gz", "base")
maut  <- read_events("D:/10pct/maut-10pct-500/maut-10pct-500.output_events.xml.gz", "maut")
park  <- read_events("D:/10pct/parking-10pct-500/parking-10pct-500.output_events.xml.gz", "parking")

all_events <- rbindlist(list(base, maut, park), use.names=TRUE, fill=TRUE)

############################
# 2️⃣ 提取 activity start/end
############################

acts <- all_events[type %in% c("actstart","actend")]

# 提取大类（去掉 _600 这种）
acts[, act_group := sub("_.*","", actType)]

############################
# 3️⃣ 计算 activity duration
############################

# 为每个 activity 排序
setorder(acts, person, scenario, time)

# 给每个 activity 编号
acts[, act_id := cumsum(type == "actstart"), 
     by=.(person, scenario)]

# 分离 start / end
start <- acts[type=="actstart"]
end   <- acts[type=="actend"]

setnames(start, "time", "start_time")
setnames(end,   "time", "end_time")

dur <- merge(start, end,
             by=c("person","scenario","act_id","act_group"),
             allow.cartesian=TRUE)

dur[, duration := end_time - start_time]

# 去掉异常值
dur <- dur[duration > 0 & duration < 24*3600]

############################
# 4️⃣ Person-level activity summary
############################

person_activity <- dur[, .(
  total_hours = sum(duration)/3600
), by=.(person, scenario, act_group)]

############################
# 5️⃣ 提取收费信息（PersonMoneyEvent）
############################

money <- all_events[type=="personMoney"]

money_summary <- money[, .(
  total_euro = sum(amount)
), by=.(person, scenario)]

############################
# 6️⃣ 合并活动和收费
############################

analysis <- merge(person_activity, money_summary,
                  by=c("person","scenario"),
                  all.x=TRUE)

analysis[is.na(total_euro), total_euro := 0]

analysis[, paid := ifelse(total_euro < 0, 1, 0)]

############################
# 7️⃣ 场景对比分析
############################

# 平均活动时间
mean_activity <- analysis[, .(
  mean_hours = mean(total_hours)
), by=.(scenario, act_group)]

print(mean_activity)

############################
# 8️⃣ 只看付费人群的变化
############################

paid_analysis <- analysis[paid==1,
                          .(mean_hours = mean(total_hours)),
                          by=.(scenario, act_group)]

print(paid_analysis)

############################
# 9️⃣ 宽表形式（方便做差值）
############################

library(reshape2)

wide <- dcast(mean_activity,
              act_group ~ scenario,
              value.var="mean_hours")

wide[, delta_maut := maut - base]
wide[, delta_park := parking - base]

print(wide)

############################
# 🔟 找一个具体 person 示例
############################

example_person <- analysis[
  act_group=="shopping",
  .(shopping_hours = total_hours),
  by=.(person, scenario)
]

example_person_wide <- dcast(example_person,
                             person ~ scenario,
                             value.var="shopping_hours")

# 找变化最大的
example_person_wide[, change_maut := maut - base]
example_person_wide[order(-abs(change_maut))][1:5]
