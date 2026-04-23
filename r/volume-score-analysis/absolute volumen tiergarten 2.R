suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(xml2)
  library(stringr)
})

# =========================================================
# 0. PATHS
# =========================================================

base_dir <- "D:/10pct/base-10pct-500"
park_dir <- "D:/10pct/parking-10pct-500"
out_dir  <- "D:/10pct/hotspot_cause_analysis_v2"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

base_links_file <- file.path(base_dir, "base-10pct-500.output_links.csv.gz")
park_links_file <- file.path(park_dir, "parking-10pct-500.output_links.csv.gz")

base_trips_file <- file.path(base_dir, "base-10pct-500.output_trips.csv.gz")
park_trips_file <- file.path(park_dir, "parking-10pct-500.output_trips.csv.gz")

base_legs_file  <- file.path(base_dir, "base-10pct-500.output_legs.csv.gz")
park_legs_file  <- file.path(park_dir, "parking-10pct-500.output_legs.csv.gz")

network_file    <- file.path(park_dir, "parking-10pct-500.output_network.xml.gz")

# =========================================================
# 1. SETTINGS
# =========================================================

top_n_hotspots <- 30
trip_buffer_m  <- 600
leg_buffer_m   <- 400

use_bbox <- TRUE
bbox <- list(
  xmin = 794000,
  xmax = 796800,
  ymin = 5826500,
  ymax = 5827600
)

# =========================================================
# 2. HELPERS
# =========================================================

safe_num <- function(x) suppressWarnings(as.numeric(x))

parse_hms_to_hour <- function(x) {
  if (is.numeric(x)) return(as.integer(floor(x)))
  x <- as.character(x)
  hh <- str_extract(x, "^\\d{1,2}")
  out <- suppressWarnings(as.integer(hh))
  idx <- is.na(out)
  if (any(idx)) {
    val <- suppressWarnings(as.numeric(x[idx]))
    out[idx] <- as.integer(floor(val / 3600))
  }
  out
}

euclid_dist <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

make_trip_key <- function(dt) {
  if ("person_id" %in% names(dt) && "trip_number" %in% names(dt)) {
    dt[, trip_key := paste(person_id, trip_number, sep = "__")]
  } else if ("person_id" %in% names(dt)) {
    dt[, trip_key := paste0(person_id, "__row", .I)]
  } else {
    dt[, trip_key := paste0("row__", .I)]
  }
  dt
}

# =========================================================
# 3. NETWORK
# =========================================================

cat("Reading network...\n")
net <- read_xml(gzfile(network_file))

nodes <- xml_find_all(net, ".//node")
nodes_dt <- data.table(
  node_id = xml_attr(nodes, "id"),
  x = safe_num(xml_attr(nodes, "x")),
  y = safe_num(xml_attr(nodes, "y"))
)

# =========================================================
# 4. LINKS
# =========================================================

cat("Reading links...\n")
base_links <- fread(base_links_file)
park_links <- fread(park_links_file)

base_links[, scenario := "base"]
park_links[, scenario := "parking"]

setnames(base_links,
         old = c("link", "from_node", "to_node"),
         new = c("link_id", "from", "to"),
         skip_absent = TRUE)

setnames(park_links,
         old = c("link", "from_node", "to_node"),
         new = c("link_id", "from", "to"),
         skip_absent = TRUE)

keep_link_cols <- c(
  "link_id", "from", "to", "length", "freespeed", "capacity", "lanes",
  "vol_car", "allowed_speed", "speed_factor", "geometry", "scenario"
)

base_links <- base_links[, intersect(keep_link_cols, names(base_links)), with = FALSE]
park_links <- park_links[, intersect(keep_link_cols, names(park_links)), with = FALSE]

add_coords <- function(dt, nodes_dt) {
  dt <- merge(dt, nodes_dt[, .(from = node_id, x_from = x, y_from = y)], by = "from", all.x = TRUE)
  dt <- merge(dt, nodes_dt[, .(to = node_id, x_to = x, y_to = y)], by = "to", all.x = TRUE)
  dt
}

base_links <- add_coords(base_links, nodes_dt)
park_links <- add_coords(park_links, nodes_dt)

base_links[, allowed_speed_kmh := safe_num(allowed_speed) * 3.6]
park_links[, allowed_speed_kmh := safe_num(allowed_speed) * 3.6]

base_links[, est_speed_kmh := safe_num(speed_factor) * allowed_speed_kmh]
park_links[, est_speed_kmh := safe_num(speed_factor) * allowed_speed_kmh]

wide_links <- merge(
  base_links, park_links,
  by = "link_id",
  suffixes = c("_base", "_parking"),
  all = FALSE
)

wide_links[, `:=`(
  x_from = x_from_base,
  y_from = y_from_base,
  x_to   = x_to_base,
  y_to   = y_to_base
)]

wide_links[, vol_diff := safe_num(vol_car_parking) - safe_num(vol_car_base)]
wide_links[, rel_diff_pct := 100 * vol_diff / pmax(safe_num(vol_car_base), 1)]

summary_all_links <- wide_links[, .(
  from = from_base,
  to = to_base,
  x_from = x_from,
  y_from = y_from,
  x_to = x_to,
  y_to = y_to,
  vol_car_base = safe_num(vol_car_base),
  vol_car_parking = safe_num(vol_car_parking),
  vol_diff = vol_diff,
  rel_diff_pct = rel_diff_pct
), by = link_id]

setorder(summary_all_links, -vol_diff)
fwrite(summary_all_links, file.path(out_dir, "01_all_links_summary.csv"))

hotspot_pool <- copy(summary_all_links)
if (use_bbox) {
  hotspot_pool <- hotspot_pool[
    x_from >= bbox$xmin & x_from <= bbox$xmax &
      y_from >= bbox$ymin & y_from <= bbox$ymax
  ]
}
if (nrow(hotspot_pool) == 0) hotspot_pool <- copy(summary_all_links)

setorder(hotspot_pool, -vol_diff)
hotspot_links <- hotspot_pool[1:min(top_n_hotspots, .N)]
fwrite(hotspot_links, file.path(out_dir, "02_selected_hotspot_links.csv"))

hotspot_centroid <- hotspot_links[, .(
  cx = mean((x_from + x_to) / 2, na.rm = TRUE),
  cy = mean((y_from + y_to) / 2, na.rm = TRUE)
)]
hotspot_cx <- hotspot_centroid$cx[1]
hotspot_cy <- hotspot_centroid$cy[1]

p_hot <- ggplot(wide_links[link_id %in% hotspot_links$link_id],
                aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = vol_diff)) +
  geom_segment(linewidth = 1.0) +
  coord_equal() +
  scale_color_gradient2(midpoint = 0) +
  theme_minimal() +
  labs(title = "Selected hotspot links", color = "vol diff")
ggsave(file.path(out_dir, "03_selected_hotspot_map.png"), p_hot, width = 9, height = 8, dpi = 300)

# =========================================================
# 5. TRIPS
# =========================================================

cat("Reading trips...\n")
base_trips <- fread(base_trips_file)
park_trips <- fread(park_trips_file)

base_trips[, scenario := "base"]
park_trips[, scenario := "parking"]

standardize_trips <- function(dt) {
  dt <- copy(dt)
  
  setnames(dt, old = c("person"), new = c("person_id"), skip_absent = TRUE)
  
  # 统一活动类型列
  setnames(dt, old = c("start_activity_type", "end_activity_type"),
           new = c("from_activity_type", "to_activity_type"),
           skip_absent = TRUE)
  
  # 保证列存在
  needed <- c("person_id","trip_number","trip_id","dep_time","trav_time",
              "traveled_distance","main_mode",
              "from_activity_type","to_activity_type",
              "start_x","start_y","end_x","end_y","scenario")
  for (nm in needed) {
    if (!nm %in% names(dt)) dt[, (nm) := NA]
  }
  
  dt[, dep_hour := parse_hms_to_hour(dep_time)]
  dt[, start_x := safe_num(start_x)]
  dt[, start_y := safe_num(start_y)]
  dt[, end_x   := safe_num(end_x)]
  dt[, end_y   := safe_num(end_y)]
  dt[, traveled_distance := safe_num(traveled_distance)]
  dt[, trav_time := safe_num(trav_time)]
  
  dt <- make_trip_key(dt)
  dt
}

base_trips <- standardize_trips(base_trips)
park_trips <- standardize_trips(park_trips)

all_trips <- rbindlist(list(base_trips, park_trips), fill = TRUE)

all_trips[, origin_dist_to_hotspot := euclid_dist(start_x, start_y, hotspot_cx, hotspot_cy)]
all_trips[, dest_dist_to_hotspot   := euclid_dist(end_x, end_y, hotspot_cx, hotspot_cy)]

all_trips[, origin_near_hotspot := origin_dist_to_hotspot <= trip_buffer_m]
all_trips[, dest_near_hotspot   := dest_dist_to_hotspot <= trip_buffer_m]
all_trips[, hotspot_related_trip := origin_near_hotspot | dest_near_hotspot]

hotspot_trips <- all_trips[hotspot_related_trip == TRUE]

# trip summaries
mode_summary <- hotspot_trips[, .N, by = .(scenario, main_mode)][order(scenario, -N)]
mode_summary[, share := N / sum(N), by = scenario]
fwrite(mode_summary, file.path(out_dir, "04_hotspot_related_trip_mode_share.csv"))

dest_act_summary <- hotspot_trips[, .N, by = .(scenario, to_activity_type)][order(scenario, -N)]
dest_act_summary[, share := N / sum(N), by = scenario]
fwrite(dest_act_summary, file.path(out_dir, "05_hotspot_related_trip_destination_activity.csv"))

orig_act_summary <- hotspot_trips[, .N, by = .(scenario, from_activity_type)][order(scenario, -N)]
orig_act_summary[, share := N / sum(N), by = scenario]
fwrite(orig_act_summary, file.path(out_dir, "06_hotspot_related_trip_origin_activity.csv"))

dep_summary <- hotspot_trips[, .N, by = .(scenario, dep_hour)][order(scenario, dep_hour)]
fwrite(dep_summary, file.path(out_dir, "07_hotspot_related_trip_departure_hour.csv"))

# 只看 car trips
car_trip_summary <- hotspot_trips[main_mode == "car", .(
  trips = .N,
  avg_distance_m = mean(traveled_distance, na.rm = TRUE),
  median_distance_m = median(traveled_distance, na.rm = TRUE),
  avg_trav_time = mean(trav_time, na.rm = TRUE)
), by = scenario]
fwrite(car_trip_summary, file.path(out_dir, "08_hotspot_related_car_trip_summary.csv"))

# =========================================================
# 6. LEGS
# =========================================================

cat("Reading legs...\n")
base_legs <- fread(base_legs_file)
park_legs <- fread(park_legs_file)

base_legs[, scenario := "base"]
park_legs[, scenario := "parking"]

standardize_legs <- function(dt) {
  dt <- copy(dt)
  
  setnames(dt, old = c("person"), new = c("person_id"), skip_absent = TRUE)
  
  needed <- c("person_id","trip_id","dep_time","trav_time","distance","mode",
              "start_link","start_x","start_y","end_link","end_x","end_y","scenario")
  for (nm in needed) {
    if (!nm %in% names(dt)) dt[, (nm) := NA]
  }
  
  dt[, dep_hour := parse_hms_to_hour(dep_time)]
  dt[, start_x := safe_num(start_x)]
  dt[, start_y := safe_num(start_y)]
  dt[, end_x   := safe_num(end_x)]
  dt[, end_y   := safe_num(end_y)]
  dt[, distance := safe_num(distance)]
  dt[, trav_time := safe_num(trav_time)]
  
  dt
}

base_legs <- standardize_legs(base_legs)
park_legs <- standardize_legs(park_legs)

all_legs <- rbindlist(list(base_legs, park_legs), fill = TRUE)

# leg 与热点关系
all_legs[, start_dist_to_hotspot := euclid_dist(start_x, start_y, hotspot_cx, hotspot_cy)]
all_legs[, end_dist_to_hotspot   := euclid_dist(end_x, end_y, hotspot_cx, hotspot_cy)]

all_legs[, start_near_hotspot := start_dist_to_hotspot <= leg_buffer_m]
all_legs[, end_near_hotspot   := end_dist_to_hotspot <= leg_buffer_m]
all_legs[, hotspot_related_leg := start_near_hotspot | end_near_hotspot]

hotspot_legs <- all_legs[hotspot_related_leg == TRUE]

# leg mode share
leg_mode_summary <- hotspot_legs[, .N, by = .(scenario, mode)][order(scenario, -N)]
leg_mode_summary[, share := N / sum(N), by = scenario]
fwrite(leg_mode_summary, file.path(out_dir, "09_hotspot_related_leg_mode_share.csv"))

# car legs only
car_leg_summary <- hotspot_legs[mode == "car", .(
  legs = .N,
  avg_distance_m = mean(distance, na.rm = TRUE),
  median_distance_m = median(distance, na.rm = TRUE),
  avg_trav_time = mean(trav_time, na.rm = TRUE)
), by = scenario]
fwrite(car_leg_summary, file.path(out_dir, "10_hotspot_related_car_leg_summary.csv"))

car_leg_dep <- hotspot_legs[mode == "car", .N, by = .(scenario, dep_hour)][order(scenario, dep_hour)]
fwrite(car_leg_dep, file.path(out_dir, "11_hotspot_related_car_leg_departure_hour.csv"))

# =========================================================
# 7. MERGE LEGS WITH TRIPS VIA trip_id + scenario
# =========================================================

trip_context <- all_trips[, .(
  scenario, trip_id, main_mode, from_activity_type, to_activity_type,
  trip_dep_hour = dep_hour, trip_distance = traveled_distance, trip_time = trav_time
)]

hotspot_legs_with_trip <- merge(
  hotspot_legs,
  trip_context,
  by = c("scenario", "trip_id"),
  all.x = TRUE
)

fwrite(hotspot_legs_with_trip, file.path(out_dir, "12_hotspot_legs_with_trip_context.csv"))

# hotspot-related car legs 属于什么 trip purpose
car_leg_dest_act <- hotspot_legs_with_trip[mode == "car", .N, by = .(scenario, to_activity_type)][order(scenario, -N)]
car_leg_dest_act[, share := N / sum(N), by = scenario]
fwrite(car_leg_dest_act, file.path(out_dir, "13_hotspot_related_car_leg_destination_activity.csv"))

car_leg_orig_act <- hotspot_legs_with_trip[mode == "car", .N, by = .(scenario, from_activity_type)][order(scenario, -N)]
car_leg_orig_act[, share := N / sum(N), by = scenario]
fwrite(car_leg_orig_act, file.path(out_dir, "14_hotspot_related_car_leg_origin_activity.csv"))

# =========================================================
# 8. DIRECTION ANALYSIS
# =========================================================

# trip-level
hotspot_trips[, origin_side := fifelse(start_x < hotspot_cx & start_y >= hotspot_cy, "NW",
                                       fifelse(start_x >= hotspot_cx & start_y >= hotspot_cy, "NE",
                                               fifelse(start_x < hotspot_cx & start_y < hotspot_cy, "SW", "SE")))]
hotspot_trips[, dest_side := fifelse(end_x < hotspot_cx & end_y >= hotspot_cy, "NW",
                                     fifelse(end_x >= hotspot_cx & end_y >= hotspot_cy, "NE",
                                             fifelse(end_x < hotspot_cx & end_y < hotspot_cy, "SW", "SE")))]

trip_od_side <- hotspot_trips[, .N, by = .(scenario, origin_side, dest_side)][order(scenario, -N)]
fwrite(trip_od_side, file.path(out_dir, "15_hotspot_related_trip_OD_side.csv"))

# leg-level
hotspot_legs_with_trip[, leg_start_side := fifelse(start_x < hotspot_cx & start_y >= hotspot_cy, "NW",
                                                   fifelse(start_x >= hotspot_cx & start_y >= hotspot_cy, "NE",
                                                           fifelse(start_x < hotspot_cx & start_y < hotspot_cy, "SW", "SE")))]
hotspot_legs_with_trip[, leg_end_side := fifelse(end_x < hotspot_cx & end_y >= hotspot_cy, "NW",
                                                 fifelse(end_x >= hotspot_cx & end_y >= hotspot_cy, "NE",
                                                         fifelse(end_x < hotspot_cx & end_y < hotspot_cy, "SW", "SE")))]

car_leg_od_side <- hotspot_legs_with_trip[mode == "car", .N, by = .(scenario, leg_start_side, leg_end_side)][order(scenario, -N)]
fwrite(car_leg_od_side, file.path(out_dir, "16_hotspot_related_car_leg_OD_side.csv"))

# =========================================================
# 9. GROWTH TABLES
# =========================================================

make_growth_table <- function(dt, rowvars, file_name) {
  wide <- dcast(dt, as.formula(paste(paste(rowvars, collapse = " + "), "~ scenario")),
                value.var = "N", fill = 0)
  if (all(c("base", "parking") %in% names(wide))) {
    wide[, diff := parking - base]
    wide[, rel_diff_pct := 100 * diff / pmax(base, 1)]
    setorder(wide, -diff)
  }
  fwrite(wide, file.path(out_dir, file_name))
}

make_growth_table(dest_act_summary, "to_activity_type", "17_trip_destination_activity_growth.csv")
make_growth_table(orig_act_summary, "from_activity_type", "18_trip_origin_activity_growth.csv")
make_growth_table(car_leg_dest_act, "to_activity_type", "19_car_leg_destination_activity_growth.csv")
make_growth_table(car_leg_orig_act, "from_activity_type", "20_car_leg_origin_activity_growth.csv")
make_growth_table(trip_od_side, c("origin_side", "dest_side"), "21_trip_OD_side_growth.csv")
make_growth_table(car_leg_od_side, c("leg_start_side", "leg_end_side"), "22_car_leg_OD_side_growth.csv")

# =========================================================
# 10. PLOTS
# =========================================================

p_trip_mode <- ggplot(mode_summary, aes(main_mode, share, fill = scenario)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Hotspot-related trips: mode share", x = "main mode", y = "share")
ggsave(file.path(out_dir, "23_trip_mode_share.png"), p_trip_mode, width = 9, height = 5, dpi = 300)

p_leg_mode <- ggplot(leg_mode_summary, aes(mode, share, fill = scenario)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Hotspot-related legs: mode share", x = "leg mode", y = "share")
ggsave(file.path(out_dir, "24_leg_mode_share.png"), p_leg_mode, width = 9, height = 5, dpi = 300)

p_car_leg_dep <- ggplot(car_leg_dep, aes(dep_hour, N, color = scenario)) +
  geom_line(linewidth = 1) +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(breaks = 0:24) +
  labs(title = "Hotspot-related car legs: departure hour", x = "hour", y = "N")
ggsave(file.path(out_dir, "25_car_leg_departure_hour.png"), p_car_leg_dep, width = 9, height = 5, dpi = 300)

# =========================================================
# 11. QUICK DIAGNOSTIC
# =========================================================

cat("\n=========================================\n")
cat("Selected hotspot links:\n")
print(hotspot_links[1:min(10, .N), .(
  link_id, x_from, y_from, vol_car_base, vol_car_parking, vol_diff, rel_diff_pct
)])

cat("\nHotspot-related trip mode share:\n")
print(mode_summary)

cat("\nHotspot-related trip destination activity:\n")
print(dest_act_summary)

cat("\nHotspot-related leg mode share:\n")
print(leg_mode_summary)

cat("\nHotspot-related car leg destination activity:\n")
print(car_leg_dest_act)

cat("\nOutputs saved to:\n")
cat(out_dir, "\n")
cat("=========================================\n")