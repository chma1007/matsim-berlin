library(data.table)
library(stringr)

# 读你刚才生成的结果
car_leg_dest_act <- fread("D:/10pct/hotspot_cause_analysis_v2/13_hotspot_related_car_leg_destination_activity.csv")
car_leg_orig_act <- fread("D:/10pct/hotspot_cause_analysis_v2/14_hotspot_related_car_leg_origin_activity.csv")
trip_dest_act    <- fread("D:/10pct/hotspot_cause_analysis_v2/05_hotspot_related_trip_destination_activity.csv")
trip_orig_act    <- fread("D:/10pct/hotspot_cause_analysis_v2/06_hotspot_related_trip_origin_activity.csv")

collapse_act <- function(x) {
  x <- as.character(x)
  
  fcase(
    str_starts(x, "home"), "home",
    str_starts(x, "work_business"), "work_business",
    str_starts(x, "work"), "work",
    str_starts(x, "education"), "education",
    str_starts(x, "university"), "education",
    str_starts(x, "school"), "education",
    str_starts(x, "shop"), "shopping",
    str_starts(x, "commercial"), "commercial",
    str_starts(x, "service"), "service",
    str_starts(x, "leisure"), "leisure",
    str_starts(x, "outside_recreation"), "recreation",
    str_starts(x, "dining"), "dining",
    str_starts(x, "personal_business"), "personal_business",
    str_starts(x, "transport"), "transport",
    str_starts(x, "other"), "other",
    default = "other_or_unknown"
  )
}

# destination
car_leg_dest_act[, act_group := collapse_act(to_activity_type)]
trip_dest_act[, act_group := collapse_act(to_activity_type)]

# origin
car_leg_orig_act[, act_group := collapse_act(from_activity_type)]
trip_orig_act[, act_group := collapse_act(from_activity_type)]

# 聚合
car_leg_dest_grouped <- car_leg_dest_act[, .(N = sum(N)), by = .(scenario, act_group)]
car_leg_dest_grouped[, share := N / sum(N), by = scenario]

car_leg_orig_grouped <- car_leg_orig_act[, .(N = sum(N)), by = .(scenario, act_group)]
car_leg_orig_grouped[, share := N / sum(N), by = scenario]

trip_dest_grouped <- trip_dest_act[, .(N = sum(N)), by = .(scenario, act_group)]
trip_dest_grouped[, share := N / sum(N), by = scenario]

trip_orig_grouped <- trip_orig_act[, .(N = sum(N)), by = .(scenario, act_group)]
trip_orig_grouped[, share := N / sum(N), by = scenario]

# 输出
fwrite(car_leg_dest_grouped, "D:/10pct/hotspot_cause_analysis_v2/30_car_leg_dest_grouped.csv")
fwrite(car_leg_orig_grouped, "D:/10pct/hotspot_cause_analysis_v2/31_car_leg_orig_grouped.csv")
fwrite(trip_dest_grouped,    "D:/10pct/hotspot_cause_analysis_v2/32_trip_dest_grouped.csv")
fwrite(trip_orig_grouped,    "D:/10pct/hotspot_cause_analysis_v2/33_trip_orig_grouped.csv")

# growth table
make_growth <- function(dt, outfile) {
  wide <- dcast(dt, act_group ~ scenario, value.var = "N", fill = 0)
  if (all(c("base", "parking") %in% names(wide))) {
    wide[, diff := parking - base]
    wide[, rel_diff_pct := 100 * diff / pmax(base, 1)]
    setorder(wide, -diff)
  }
  fwrite(wide, outfile)
}

make_growth(car_leg_dest_grouped, "D:/10pct/hotspot_cause_analysis_v2/34_car_leg_dest_grouped_growth.csv")
make_growth(car_leg_orig_grouped, "D:/10pct/hotspot_cause_analysis_v2/35_car_leg_orig_grouped_growth.csv")
make_growth(trip_dest_grouped,    "D:/10pct/hotspot_cause_analysis_v2/36_trip_dest_grouped_growth.csv")
make_growth(trip_orig_grouped,    "D:/10pct/hotspot_cause_analysis_v2/37_trip_orig_grouped_growth.csv")

print(car_leg_dest_grouped[order(scenario, -N)])
print(car_leg_orig_grouped[order(scenario, -N)])