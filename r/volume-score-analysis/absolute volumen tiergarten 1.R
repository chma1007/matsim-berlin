# =========================================================
# MATSim hotspot cause analysis
# Author: ChatGPT
# Goal:
#   Analyze WHY car volume increases in a hotspot area under parking pricing
#
# Main outputs:
#   1) hotspot links and maps
#   2) hotspot-nearby trip origin/destination patterns
#   3) mode / departure time / activity type changes near hotspot
#   4) if possible: trips directly traversing hotspot links from leg routes
# =========================================================

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
out_dir  <- "D:/10pct/hotspot_cause_analysis"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

base_links_file <- file.path(base_dir, "base-10pct-500.output_links.csv.gz")
park_links_file <- file.path(park_dir, "parking-10pct-500.output_links.csv.gz")

base_trips_file <- file.path(base_dir, "base-10pct-500.output_trips.csv.gz")
park_trips_file <- file.path(park_dir, "parking-10pct-500.output_trips.csv.gz")

base_legs_file  <- file.path(base_dir, "base-10pct-500.output_legs.csv.gz")
park_legs_file  <- file.path(park_dir, "parking-10pct-500.output_legs.csv.gz")

network_file    <- file.path(park_dir, "parking-10pct-500.output_network.xml.gz")

# =========================================================
# 1. USER SETTINGS
# =========================================================

# --- hotspot definition ---
top_n_hotspots <- 30

# optional bbox for central area / Tiergarten-like candidate zone
# based on your previous outputs, this is a much better starting point
use_bbox <- TRUE
bbox <- list(
  xmin = 794000,
  xmax = 796800,
  ymin = 5826500,
  ymax = 5827600
)

# radius (meters) for selecting trips near hotspot start/end points
trip_buffer_m <- 600

# when route parsing is possible, how many top hotspot links to use
top_n_route_links <- 15

# =========================================================
# 2. HELPERS
# =========================================================

find_col <- function(nms, patterns) {
  hits <- unique(unlist(lapply(patterns, function(p) nms[grepl(p, nms, ignore.case = TRUE)])))
  if (length(hits) == 0) return(NA_character_)
  hits[1]
}

safe_num <- function(x) suppressWarnings(as.numeric(x))

parse_hms_to_hour <- function(x) {
  if (is.numeric(x)) return(as.integer(floor(x)))
  x <- as.character(x)
  # 07:13:00 / 7:13:00
  hh <- str_extract(x, "^\\d{1,2}")
  out <- suppressWarnings(as.integer(hh))
  # fallback: if values might be in seconds
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

# string contains one of many hotspot link ids
contains_any_link <- function(route_vec, ids) {
  if (length(ids) == 0) return(rep(FALSE, length(route_vec)))
  pat <- paste0("(^|\\s|,|;|\\|)(", paste(ids, collapse = "|"), ")(\\s|,|;|\\||$)")
  grepl(pat, route_vec)
}

make_trip_key <- function(dt) {
  nms <- names(dt)
  
  person_col <- find_col(nms, c("^person$", "^person_id$", "person"))
  tripnum_col <- find_col(nms, c("^trip_number$", "^trip_id$", "^trip$", "trip_number", "trip_id"))
  
  if (!is.na(person_col) && !is.na(tripnum_col)) {
    dt[, trip_key := paste(get(person_col), get(tripnum_col), sep = "__")]
  } else if (!is.na(person_col)) {
    dt[, trip_key := paste0(get(person_col), "__row", .I)]
  } else {
    dt[, trip_key := paste0("row__", .I)]
  }
  dt
}

# =========================================================
# 3. READ NETWORK
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
# 4. READ LINKS
# =========================================================

cat("Reading links...\n")
base_links <- fread(base_links_file)
park_links <- fread(park_links_file)

base_links[, scenario := "base"]
park_links[, scenario := "parking"]

# standardize names for links
setnames(base_links,
         old = intersect(c("link", "from_node", "to_node"), names(base_links)),
         new = c("link_id", "from", "to")[match(intersect(c("link", "from_node", "to_node"), names(base_links)),
                                                c("link", "from_node", "to_node"))])

setnames(park_links,
         old = intersect(c("link", "from_node", "to_node"), names(park_links)),
         new = c("link_id", "from", "to")[match(intersect(c("link", "from_node", "to_node"), names(park_links)),
                                                c("link", "from_node", "to_node"))])

# keep required cols if available
keep_link_cols <- unique(c(
  "link_id", "from", "to", "length", "freespeed", "capacity", "lanes",
  "vol_car", "allowed_speed", "speed_factor", "geometry", "scenario"
))

base_links <- base_links[, intersect(keep_link_cols, names(base_links)), with = FALSE]
park_links <- park_links[, intersect(keep_link_cols, names(park_links)), with = FALSE]

add_coords <- function(dt, nodes_dt) {
  dt <- merge(
    dt,
    nodes_dt[, .(from = node_id, x_from = x, y_from = y)],
    by = "from",
    all.x = TRUE
  )
  dt <- merge(
    dt,
    nodes_dt[, .(to = node_id, x_to = x, y_to = y)],
    by = "to",
    all.x = TRUE
  )
  dt
}

base_links <- add_coords(base_links, nodes_dt)
park_links <- add_coords(park_links, nodes_dt)

# speed proxy
if ("allowed_speed" %in% names(base_links)) {
  base_links[, allowed_speed_kmh := safe_num(allowed_speed) * 3.6]
}
if ("allowed_speed" %in% names(park_links)) {
  park_links[, allowed_speed_kmh := safe_num(allowed_speed) * 3.6]
}
if ("speed_factor" %in% names(base_links) && "allowed_speed_kmh" %in% names(base_links)) {
  base_links[, est_speed_kmh := safe_num(speed_factor) * allowed_speed_kmh]
}
if ("speed_factor" %in% names(park_links) && "allowed_speed_kmh" %in% names(park_links)) {
  park_links[, est_speed_kmh := safe_num(speed_factor) * allowed_speed_kmh]
}

# merge base + parking
wide_links <- merge(
  base_links,
  park_links,
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

if ("speed_factor_base" %in% names(wide_links) && "speed_factor_parking" %in% names(wide_links)) {
  wide_links[, speed_factor_diff := safe_num(speed_factor_parking) - safe_num(speed_factor_base)]
}
if ("est_speed_kmh_base" %in% names(wide_links) && "est_speed_kmh_parking" %in% names(wide_links)) {
  wide_links[, est_speed_diff_kmh := safe_num(est_speed_kmh_parking) - safe_num(est_speed_kmh_base)]
}

wide_links[, rel_diff_pct := 100 * vol_diff / pmax(safe_num(vol_car_base), 1)]

cat("\nCoordinate range:\n")
cat("x_from min:", min(wide_links$x_from, na.rm = TRUE), "\n")
cat("x_from max:", max(wide_links$x_from, na.rm = TRUE), "\n")
cat("y_from min:", min(wide_links$y_from, na.rm = TRUE), "\n")
cat("y_from max:", max(wide_links$y_from, na.rm = TRUE), "\n")

# network point plot
p_network <- ggplot(unique(wide_links[, .(link_id, x_from, y_from)]), aes(x_from, y_from)) +
  geom_point(size = 0.12, alpha = 0.45) +
  coord_equal() +
  theme_minimal() +
  labs(title = "Network link start points", x = "x", y = "y")

ggsave(file.path(out_dir, "01_network_points.png"), p_network, width = 8, height = 8, dpi = 300)

# =========================================================
# 5. HOTSPOT LINK IDENTIFICATION
# =========================================================

summary_all_links <- wide_links[, .(
  from = from_base,
  to = to_base,
  x_from = x_from,
  y_from = y_from,
  x_to = x_to,
  y_to = y_to,
  length_m = safe_num(length_base),
  capacity = safe_num(capacity_base),
  lanes = safe_num(lanes_base),
  vol_car_base = safe_num(vol_car_base),
  vol_car_parking = safe_num(vol_car_parking),
  vol_diff = vol_diff,
  rel_diff_pct = rel_diff_pct,
  speed_factor_base = if ("speed_factor_base" %in% names(wide_links)) safe_num(speed_factor_base) else NA_real_,
  speed_factor_parking = if ("speed_factor_parking" %in% names(wide_links)) safe_num(speed_factor_parking) else NA_real_,
  speed_factor_diff = if ("speed_factor_diff" %in% names(wide_links)) safe_num(speed_factor_diff) else NA_real_,
  est_speed_kmh_base = if ("est_speed_kmh_base" %in% names(wide_links)) safe_num(est_speed_kmh_base) else NA_real_,
  est_speed_kmh_parking = if ("est_speed_kmh_parking" %in% names(wide_links)) safe_num(est_speed_kmh_parking) else NA_real_,
  est_speed_diff_kmh = if ("est_speed_diff_kmh" %in% names(wide_links)) safe_num(est_speed_diff_kmh) else NA_real_
), by = link_id]

setorder(summary_all_links, -vol_diff)
fwrite(summary_all_links, file.path(out_dir, "02_all_links_summary.csv"))
fwrite(summary_all_links[1:100], file.path(out_dir, "03_top100_hotspots_whole_network.csv"))

hotspot_pool <- copy(summary_all_links)

if (use_bbox) {
  hotspot_pool <- hotspot_pool[
    x_from >= bbox$xmin & x_from <= bbox$xmax &
      y_from >= bbox$ymin & y_from <= bbox$ymax
  ]
}

if (nrow(hotspot_pool) == 0) {
  cat("\nWARNING: bbox produced 0 links. Falling back to whole-network top hotspots.\n")
  hotspot_pool <- copy(summary_all_links)
}

setorder(hotspot_pool, -vol_diff)
hotspot_links <- hotspot_pool[1:min(top_n_hotspots, .N)]

fwrite(hotspot_links, file.path(out_dir, "04_selected_hotspot_links.csv"))

# hotspot map
plot_hot <- wide_links[link_id %in% hotspot_links$link_id]

p_hot <- ggplot(plot_hot, aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = vol_diff)) +
  geom_segment(linewidth = 1.0, alpha = 0.95) +
  coord_equal() +
  scale_color_gradient2(midpoint = 0) +
  theme_minimal() +
  labs(title = "Selected hotspot links: Parking - Base car volume difference",
       x = "x", y = "y", color = "vol diff")

ggsave(file.path(out_dir, "05_selected_hotspot_map.png"), p_hot, width = 9, height = 8, dpi = 300)

# hotspot centroid
hotspot_centroid <- hotspot_links[, .(
  cx = mean((x_from + x_to) / 2, na.rm = TRUE),
  cy = mean((y_from + y_to) / 2, na.rm = TRUE)
)]

hotspot_cx <- hotspot_centroid$cx[1]
hotspot_cy <- hotspot_centroid$cy[1]

# =========================================================
# 6. READ TRIPS
# =========================================================

cat("Reading trips...\n")
base_trips <- fread(base_trips_file)
park_trips <- fread(park_trips_file)

base_trips[, scenario := "base"]
park_trips[, scenario := "parking"]

cat("\nTrip columns (base):\n")
print(names(base_trips))

standardize_trips <- function(dt) {
  dt <- copy(dt)
  nms <- names(dt)
  
  # candidate columns
  person_col <- find_col(nms, c("^person$", "^person_id$", "person"))
  tripnum_col <- find_col(nms, c("^trip_number$", "^trip_id$", "^trip$", "trip_number", "trip_id"))
  dep_col <- find_col(nms, c("^dep_time$", "^departure_time$", "dep_time", "departure"))
  mode_col <- find_col(nms, c("^main_mode$", "^mode$", "main_mode"))
  sx_col <- find_col(nms, c("^start_x$", "^from_x$", "start_x", "origin_x"))
  sy_col <- find_col(nms, c("^start_y$", "^from_y$", "start_y", "origin_y"))
  ex_col <- find_col(nms, c("^end_x$", "^to_x$", "end_x", "destination_x"))
  ey_col <- find_col(nms, c("^end_y$", "^to_y$", "end_y", "destination_y"))
  sd_col <- find_col(nms, c("^traveled_distance$", "^distance$", "distance"))
  tt_col <- find_col(nms, c("^trav_time$", "^travel_time$", "trav_time"))
  orig_act_col <- find_col(nms, c("^from_activity_type$", "^origin_activity_type$", "from_.*activity", "origin_.*activity"))
  dest_act_col <- find_col(nms, c("^to_activity_type$", "^destination_activity_type$", "to_.*activity", "destination_.*activity"))
  
  # standard names
  if (!is.na(person_col) && person_col != "person_id") setnames(dt, person_col, "person_id")
  if (!is.na(tripnum_col) && tripnum_col != "trip_number") setnames(dt, tripnum_col, "trip_number")
  if (!is.na(dep_col) && dep_col != "dep_time") setnames(dt, dep_col, "dep_time")
  if (!is.na(mode_col) && mode_col != "main_mode") setnames(dt, mode_col, "main_mode")
  if (!is.na(sx_col) && sx_col != "start_x") setnames(dt, sx_col, "start_x")
  if (!is.na(sy_col) && sy_col != "start_y") setnames(dt, sy_col, "start_y")
  if (!is.na(ex_col) && ex_col != "end_x") setnames(dt, ex_col, "end_x")
  if (!is.na(ey_col) && ey_col != "end_y") setnames(dt, ey_col, "end_y")
  if (!is.na(sd_col) && sd_col != "traveled_distance") setnames(dt, sd_col, "traveled_distance")
  if (!is.na(tt_col) && tt_col != "trav_time") setnames(dt, tt_col, "trav_time")
  if (!is.na(orig_act_col) && orig_act_col != "from_activity_type") setnames(dt, orig_act_col, "from_activity_type")
  if (!is.na(dest_act_col) && dest_act_col != "to_activity_type") setnames(dt, dest_act_col, "to_activity_type")
  
  # ensure columns exist
  for (nm in c("person_id","trip_number","dep_time","main_mode","start_x","start_y","end_x","end_y",
               "traveled_distance","trav_time","from_activity_type","to_activity_type")) {
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

# distances to hotspot centroid
all_trips[, origin_dist_to_hotspot := euclid_dist(start_x, start_y, hotspot_cx, hotspot_cy)]
all_trips[, dest_dist_to_hotspot   := euclid_dist(end_x, end_y, hotspot_cx, hotspot_cy)]

all_trips[, origin_near_hotspot := origin_dist_to_hotspot <= trip_buffer_m]
all_trips[, dest_near_hotspot   := dest_dist_to_hotspot <= trip_buffer_m]
all_trips[, hotspot_related_trip := origin_near_hotspot | dest_near_hotspot]

# =========================================================
# 7. TRIP-LEVEL ANALYSIS: NEAR HOTSPOT
# =========================================================

hotspot_trips <- all_trips[hotspot_related_trip == TRUE]

# overall counts
trip_count_summary <- hotspot_trips[, .N, by = .(scenario)]
fwrite(trip_count_summary, file.path(out_dir, "06_hotspot_related_trip_counts.csv"))

# mode share
mode_summary <- hotspot_trips[, .N, by = .(scenario, main_mode)][order(scenario, -N)]
mode_summary[, share := N / sum(N), by = scenario]
fwrite(mode_summary, file.path(out_dir, "07_hotspot_related_mode_share.csv"))

# departure time
dep_summary <- hotspot_trips[, .N, by = .(scenario, dep_hour)][order(scenario, dep_hour)]
fwrite(dep_summary, file.path(out_dir, "08_hotspot_related_departure_hour.csv"))

# destination activity type
dest_act_summary <- hotspot_trips[, .N, by = .(scenario, to_activity_type)][order(scenario, -N)]
dest_act_summary[, share := N / sum(N), by = scenario]
fwrite(dest_act_summary, file.path(out_dir, "09_hotspot_related_destination_activity.csv"))

# origin activity type
orig_act_summary <- hotspot_trips[, .N, by = .(scenario, from_activity_type)][order(scenario, -N)]
orig_act_summary[, share := N / sum(N), by = scenario]
fwrite(orig_act_summary, file.path(out_dir, "10_hotspot_related_origin_activity.csv"))

# avg distance/time by mode
dist_time_summary <- hotspot_trips[, .(
  n = .N,
  avg_distance_m = mean(traveled_distance, na.rm = TRUE),
  median_distance_m = median(traveled_distance, na.rm = TRUE),
  avg_trav_time = mean(trav_time, na.rm = TRUE)
), by = .(scenario, main_mode)][order(scenario, -n)]
fwrite(dist_time_summary, file.path(out_dir, "11_hotspot_related_distance_time_by_mode.csv"))

# OD quadrants around hotspot centroid: rough directional explanation
hotspot_trips[, origin_side := fifelse(start_x < hotspot_cx & start_y >= hotspot_cy, "NW",
                                       fifelse(start_x >= hotspot_cx & start_y >= hotspot_cy, "NE",
                                               fifelse(start_x < hotspot_cx & start_y < hotspot_cy, "SW", "SE")))]
hotspot_trips[, dest_side := fifelse(end_x < hotspot_cx & end_y >= hotspot_cy, "NW",
                                     fifelse(end_x >= hotspot_cx & end_y >= hotspot_cy, "NE",
                                             fifelse(end_x < hotspot_cx & end_y < hotspot_cy, "SW", "SE")))]

od_side_summary <- hotspot_trips[, .N, by = .(scenario, origin_side, dest_side)][order(scenario, -N)]
fwrite(od_side_summary, file.path(out_dir, "12_hotspot_related_OD_side_flows.csv"))

# car-only near hotspot
car_hotspot_trips <- hotspot_trips[main_mode == "car"]
car_mode_dest <- car_hotspot_trips[, .N, by = .(scenario, to_activity_type)][order(scenario, -N)]
car_mode_dest[, share := N / sum(N), by = scenario]
fwrite(car_mode_dest, file.path(out_dir, "13_hotspot_related_car_destination_activity.csv"))

# =========================================================
# 8. PLOTS FOR HOTSPOT-RELATED TRIPS
# =========================================================

p_mode <- ggplot(mode_summary, aes(main_mode, share, fill = scenario)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(title = "Mode share of hotspot-related trips",
       x = "Main mode", y = "Share")

ggsave(file.path(out_dir, "14_hotspot_related_mode_share.png"), p_mode, width = 9, height = 5, dpi = 300)

p_dep <- ggplot(dep_summary, aes(dep_hour, N, color = scenario)) +
  geom_line(linewidth = 1) +
  geom_point() +
  theme_minimal() +
  scale_x_continuous(breaks = 0:24) +
  labs(title = "Departure-hour distribution of hotspot-related trips",
       x = "Departure hour", y = "Trips")

ggsave(file.path(out_dir, "15_hotspot_related_departure_hour.png"), p_dep, width = 9, height = 5, dpi = 300)

p_dest <- ggplot(dest_act_summary, aes(reorder(to_activity_type, share), share, fill = scenario)) +
  geom_col(position = "dodge") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Destination activity type near hotspot",
       x = "Destination activity type", y = "Share")

ggsave(file.path(out_dir, "16_hotspot_related_destination_activity.png"), p_dest, width = 9, height = 6, dpi = 300)

# =========================================================
# 9. READ LEGS AND TRY ROUTE-BASED ANALYSIS
# =========================================================

cat("Reading legs...\n")
base_legs <- fread(base_legs_file)
park_legs <- fread(park_legs_file)

base_legs[, scenario := "base"]
park_legs[, scenario := "parking"]

cat("\nLeg columns (base):\n")
print(names(base_legs))

standardize_legs <- function(dt) {
  dt <- copy(dt)
  nms <- names(dt)
  
  person_col <- find_col(nms, c("^person$", "^person_id$", "person"))
  tripnum_col <- find_col(nms, c("^trip_number$", "^trip_id$", "^trip$", "trip_number", "trip_id"))
  legnum_col <- find_col(nms, c("^leg_number$", "^leg$", "leg_number", "leg_id"))
  mode_col <- find_col(nms, c("^mode$", "^leg_mode$", "mode"))
  dep_col <- find_col(nms, c("^dep_time$", "^departure_time$", "dep_time"))
  route_col <- find_col(nms, c("^route$", "network_route", "links"))
  dist_col <- find_col(nms, c("^distance$", "^route_distance$", "distance"))
  tt_col <- find_col(nms, c("^trav_time$", "^travel_time$", "trav_time"))
  
  if (!is.na(person_col) && person_col != "person_id") setnames(dt, person_col, "person_id")
  if (!is.na(tripnum_col) && tripnum_col != "trip_number") setnames(dt, tripnum_col, "trip_number")
  if (!is.na(legnum_col) && legnum_col != "leg_number") setnames(dt, legnum_col, "leg_number")
  if (!is.na(mode_col) && mode_col != "mode") setnames(dt, mode_col, "mode")
  if (!is.na(dep_col) && dep_col != "dep_time") setnames(dt, dep_col, "dep_time")
  if (!is.na(route_col) && route_col != "route") setnames(dt, route_col, "route")
  if (!is.na(dist_col) && dist_col != "distance") setnames(dt, dist_col, "distance")
  if (!is.na(tt_col) && tt_col != "trav_time") setnames(dt, tt_col, "trav_time")
  
  for (nm in c("person_id","trip_number","leg_number","mode","dep_time","route","distance","trav_time")) {
    if (!nm %in% names(dt)) dt[, (nm) := NA]
  }
  
  dt[, dep_hour := parse_hms_to_hour(dep_time)]
  dt[, distance := safe_num(distance)]
  dt[, trav_time := safe_num(trav_time)]
  
  dt <- make_trip_key(dt)
  dt
}

base_legs <- standardize_legs(base_legs)
park_legs <- standardize_legs(park_legs)

all_legs <- rbindlist(list(base_legs, park_legs), fill = TRUE)

# route-based hotspot traversal
route_available <- "route" %in% names(all_legs) && any(!is.na(all_legs$route) & all_legs$route != "")

if (route_available) {
  
  route_hotspot_ids <- hotspot_links[1:min(top_n_route_links, .N), link_id]
  cat("\nRoute-based analysis using hotspot links:\n")
  print(route_hotspot_ids)
  
  all_legs[, uses_hotspot_link := contains_any_link(as.character(route), route_hotspot_ids)]
  
  hotspot_route_legs <- all_legs[uses_hotspot_link == TRUE]
  fwrite(hotspot_route_legs, file.path(out_dir, "17_hotspot_route_legs_raw.csv"))
  
  route_leg_summary <- hotspot_route_legs[, .N, by = .(scenario, mode)][order(scenario, -N)]
  fwrite(route_leg_summary, file.path(out_dir, "18_hotspot_route_legs_by_mode.csv"))
  
  # merge with trips for trip purposes etc.
  trips_for_merge <- all_trips[, .(
    trip_key, scenario, person_id, trip_number, main_mode,
    dep_time, dep_hour, start_x, start_y, end_x, end_y,
    from_activity_type, to_activity_type, traveled_distance, trav_time
  )]
  
  hotspot_route_trips <- merge(
    unique(hotspot_route_legs[, .(trip_key, scenario)]),
    trips_for_merge,
    by = c("trip_key", "scenario"),
    all.x = TRUE
  )
  
  fwrite(hotspot_route_trips, file.path(out_dir, "19_hotspot_route_trips.csv"))
  
  # route-based summaries
  route_trip_mode <- hotspot_route_trips[, .N, by = .(scenario, main_mode)][order(scenario, -N)]
  route_trip_mode[, share := N / sum(N), by = scenario]
  fwrite(route_trip_mode, file.path(out_dir, "20_hotspot_route_trip_mode_share.csv"))
  
  route_trip_dep <- hotspot_route_trips[, .N, by = .(scenario, dep_hour)][order(scenario, dep_hour)]
  fwrite(route_trip_dep, file.path(out_dir, "21_hotspot_route_trip_departure_hour.csv"))
  
  route_trip_dest <- hotspot_route_trips[, .N, by = .(scenario, to_activity_type)][order(scenario, -N)]
  route_trip_dest[, share := N / sum(N), by = scenario]
  fwrite(route_trip_dest, file.path(out_dir, "22_hotspot_route_trip_destination_activity.csv"))
  
  # direction relative to hotspot centroid
  hotspot_route_trips[, origin_side := fifelse(start_x < hotspot_cx & start_y >= hotspot_cy, "NW",
                                               fifelse(start_x >= hotspot_cx & start_y >= hotspot_cy, "NE",
                                                       fifelse(start_x < hotspot_cx & start_y < hotspot_cy, "SW", "SE")))]
  hotspot_route_trips[, dest_side := fifelse(end_x < hotspot_cx & end_y >= hotspot_cy, "NW",
                                             fifelse(end_x >= hotspot_cx & end_y >= hotspot_cy, "NE",
                                                     fifelse(end_x < hotspot_cx & end_y < hotspot_cy, "SW", "SE")))]
  
  route_od_side <- hotspot_route_trips[, .N, by = .(scenario, origin_side, dest_side)][order(scenario, -N)]
  fwrite(route_od_side, file.path(out_dir, "23_hotspot_route_trip_OD_side_flows.csv"))
  
  # route-based plots
  p_route_mode <- ggplot(route_trip_mode, aes(main_mode, share, fill = scenario)) +
    geom_col(position = "dodge") +
    theme_minimal() +
    labs(title = "Trips traversing hotspot links: main mode share", x = "Main mode", y = "Share")
  
  ggsave(file.path(out_dir, "24_hotspot_route_trip_mode_share.png"), p_route_mode, width = 9, height = 5, dpi = 300)
  
  p_route_dep <- ggplot(route_trip_dep, aes(dep_hour, N, color = scenario)) +
    geom_line(linewidth = 1) +
    geom_point() +
    theme_minimal() +
    scale_x_continuous(breaks = 0:24) +
    labs(title = "Trips traversing hotspot links: departure hour", x = "Departure hour", y = "Trips")
  
  ggsave(file.path(out_dir, "25_hotspot_route_trip_departure_hour.png"), p_route_dep, width = 9, height = 5, dpi = 300)
  
} else {
  cat("\nNo usable route column found in legs. Route-based hotspot-trip analysis skipped.\n")
}

# =========================================================
# 10. SYNTHESIS TABLES: WHAT MOST LIKELY DRIVES THE HOTSPOT?
# =========================================================

# 10.1 Compare hotspot-related car trips between scenarios
car_hotspot_summary <- hotspot_trips[main_mode == "car", .(
  trips = .N,
  avg_distance_m = mean(traveled_distance, na.rm = TRUE),
  avg_trav_time = mean(trav_time, na.rm = TRUE),
  avg_origin_dist = mean(origin_dist_to_hotspot, na.rm = TRUE),
  avg_dest_dist = mean(dest_dist_to_hotspot, na.rm = TRUE)
), by = scenario]
fwrite(car_hotspot_summary, file.path(out_dir, "26_hotspot_related_car_trip_summary.csv"))

# 10.2 growth table for destination activity
dest_wide <- dcast(dest_act_summary, to_activity_type ~ scenario, value.var = "N", fill = 0)
if (all(c("base","parking") %in% names(dest_wide))) {
  dest_wide[, diff := parking - base]
  dest_wide[, rel_diff_pct := 100 * diff / pmax(base, 1)]
  setorder(dest_wide, -diff)
  fwrite(dest_wide, file.path(out_dir, "27_destination_activity_growth_table.csv"))
}

# 10.3 growth table for OD sides
od_wide <- dcast(od_side_summary, origin_side + dest_side ~ scenario, value.var = "N", fill = 0)
if (all(c("base","parking") %in% names(od_wide))) {
  od_wide[, diff := parking - base]
  od_wide[, rel_diff_pct := 100 * diff / pmax(base, 1)]
  setorder(od_wide, -diff)
  fwrite(od_wide, file.path(out_dir, "28_OD_side_growth_table.csv"))
}

# 10.4 if route-based results exist, synthesize exact traversing trips
if (file.exists(file.path(out_dir, "19_hotspot_route_trips.csv"))) {
  hotspot_route_trips <- fread(file.path(out_dir, "19_hotspot_route_trips.csv"))
  
  route_reason_summary <- hotspot_route_trips[, .N, by = .(scenario, main_mode, to_activity_type)][order(scenario, -N)]
  fwrite(route_reason_summary, file.path(out_dir, "29_route_based_reason_summary.csv"))
}

# =========================================================
# 11. PRINT QUICK DIAGNOSTIC
# =========================================================

cat("\n=================================================\n")
cat("Selected hotspot links:\n")
print(hotspot_links[1:min(10, .N), .(
  link_id, x_from, y_from, vol_car_base, vol_car_parking, vol_diff, rel_diff_pct
)])

cat("\nHotspot-related trip mode share:\n")
print(mode_summary)

cat("\nHotspot-related destination activities:\n")
print(dest_act_summary)

cat("\nOutputs saved to:\n")
cat(out_dir, "\n")
cat("=================================================\n")