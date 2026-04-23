suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

# =========================================================
# 0. PATHS
# =========================================================

base_dir <- "D:/10pct/base-10pct-500"
park_dir <- "D:/10pct/parking-10pct-500"

out_dir <- "D:/10pct/hotspot_events_analysis"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# hotspot links from full-network clustering
hotspot_links_file <- "D:/10pct/full_network_clustering/05_largest_cluster_links.csv"

# events files
base_events_file <- file.path(base_dir, "base-10pct-500.output_events.xml.gz")
park_events_file <- file.path(park_dir, "parking-10pct-500.output_events.xml.gz")

# =========================================================
# 1. SETTINGS
# =========================================================

chunk_size <- 100000

# =========================================================
# 2. HELPERS
# =========================================================

safe_num <- function(x) suppressWarnings(as.numeric(x))

# classify vehicle/person id into service/commercial/other
# !!! if your MATSim naming convention differs, modify this function !!!
classify_actor <- function(id_vec) {
  id_vec <- tolower(as.character(id_vec))
  
  fcase(
    str_detect(id_vec, "service"), "service",
    str_detect(id_vec, "commercial|freight|goods|truck"), "commercial",
    default = "other"
  )
}

# extract an XML attribute from an event line
extract_attr <- function(lines, attr) {
  pat <- paste0(attr, "=\"([^\"]+)\"")
  str_match(lines, pat)[, 2]
}

# parse MATSim link-enter events on hotspot links
parse_hotspot_link_enters <- function(events_file, hotspot_links, scenario_name, chunk_size = 100000) {
  
  cat("\nParsing events for scenario:", scenario_name, "\n")
  con <- gzfile(events_file, open = "rt")
  on.exit(close(con), add = TRUE)
  
  hotspot_set <- unique(as.character(hotspot_links))
  
  res_list <- list()
  chunk_id <- 1L
  total_lines <- 0L
  total_enter <- 0L
  total_hot <- 0L
  
  repeat {
    lines <- readLines(con, n = chunk_size, warn = FALSE)
    if (length(lines) == 0) break
    
    total_lines <- total_lines + length(lines)
    
    # keep only entered-link events
    idx_enter <- grepl("<event ", lines, fixed = TRUE) &
      grepl('type="entered link"', lines, fixed = TRUE)
    
    if (!any(idx_enter)) next
    
    lines_enter <- lines[idx_enter]
    total_enter <- total_enter + length(lines_enter)
    
    # extract link ids first, then filter by membership
    link_ids <- extract_attr(lines_enter, "link")
    idx_hot <- !is.na(link_ids) & link_ids %in% hotspot_set
    
    if (!any(idx_hot)) next
    
    lines_hot <- lines_enter[idx_hot]
    total_hot <- total_hot + length(lines_hot)
    
    dt <- data.table(
      scenario = scenario_name,
      time = safe_num(extract_attr(lines_hot, "time")),
      link_id = extract_attr(lines_hot, "link"),
      vehicle_id = extract_attr(lines_hot, "vehicle"),
      person_id = extract_attr(lines_hot, "person")
    )
    
    # prefer vehicle_id, otherwise person_id
    dt[, actor_id := fifelse(!is.na(vehicle_id) & vehicle_id != "", vehicle_id, person_id)]
    dt[, actor_type := classify_actor(actor_id)]
    
    res_list[[chunk_id]] <- dt
    chunk_id <- chunk_id + 1L
  }
  
  cat("Total lines read:", total_lines, "\n")
  cat("Entered-link event lines:", total_enter, "\n")
  cat("Hotspot entered-link events:", total_hot, "\n")
  
  if (length(res_list) == 0) {
    cat("No hotspot link-enter events found for", scenario_name, "\n")
    return(data.table())
  }
  
  out <- rbindlist(res_list, fill = TRUE)
  cat("Collected", nrow(out), "hotspot link-enter events for", scenario_name, "\n")
  out
}

# =========================================================
# 3. READ HOTSPOT LINKS
# =========================================================

hotspot_links_dt <- fread(hotspot_links_file)

if (!"link_id" %in% names(hotspot_links_dt)) {
  stop("Hotspot links file must contain column 'link_id'.")
}

hotspot_links <- unique(hotspot_links_dt$link_id)
cat("Number of hotspot links:", length(hotspot_links), "\n")

# =========================================================
# 4. PARSE EVENTS
# =========================================================

base_hot_events <- parse_hotspot_link_enters(
  events_file = base_events_file,
  hotspot_links = hotspot_links,
  scenario_name = "base",
  chunk_size = chunk_size
)

park_hot_events <- parse_hotspot_link_enters(
  events_file = park_events_file,
  hotspot_links = hotspot_links,
  scenario_name = "parking",
  chunk_size = chunk_size
)

all_hot_events <- rbindlist(list(base_hot_events, park_hot_events), fill = TRUE)

if (nrow(all_hot_events) == 0) {
  stop("No hotspot events parsed. Please check the event type name and hotspot link ids.")
}

fwrite(all_hot_events, file.path(out_dir, "01_hotspot_link_enter_events_raw.csv"))

# =========================================================
# 5. OVERALL ABSOLUTE COUNTS ON HOTSPOT LINKS
# =========================================================

# 5.1 total event counts by type
type_event_counts <- all_hot_events[, .(
  event_count = .N
), by = .(scenario, actor_type)][order(scenario, -event_count)]

fwrite(type_event_counts, file.path(out_dir, "02_hotspot_event_counts_by_type.csv"))

# 5.2 unique actors by type
type_unique_counts <- unique(all_hot_events[, .(scenario, actor_id, actor_type)])[
  , .(unique_actor_count = .N), by = .(scenario, actor_type)
][order(scenario, -unique_actor_count)]

fwrite(type_unique_counts, file.path(out_dir, "03_hotspot_unique_actors_by_type.csv"))

# 5.3 wide comparison table for event counts
type_event_wide <- dcast(
  type_event_counts,
  actor_type ~ scenario,
  value.var = "event_count",
  fill = 0
)

if (all(c("base", "parking") %in% names(type_event_wide))) {
  type_event_wide[, diff := parking - base]
  type_event_wide[, rel_diff_pct := 100 * diff / pmax(base, 1)]
}
setorder(type_event_wide, -diff)

fwrite(type_event_wide, file.path(out_dir, "04_hotspot_event_counts_by_type_wide.csv"))

# 5.4 wide comparison table for unique actors
type_unique_wide <- dcast(
  type_unique_counts,
  actor_type ~ scenario,
  value.var = "unique_actor_count",
  fill = 0
)

if (all(c("base", "parking") %in% names(type_unique_wide))) {
  type_unique_wide[, diff := parking - base]
  type_unique_wide[, rel_diff_pct := 100 * diff / pmax(base, 1)]
}
setorder(type_unique_wide, -diff)

fwrite(type_unique_wide, file.path(out_dir, "05_hotspot_unique_actors_by_type_wide.csv"))

# =========================================================
# 6. PER-LINK ABSOLUTE COUNTS
# =========================================================

link_type_counts <- all_hot_events[, .(
  event_count = .N
), by = .(scenario, link_id, actor_type)]

fwrite(link_type_counts, file.path(out_dir, "06_hotspot_link_type_event_counts_long.csv"))

link_type_wide <- dcast(
  link_type_counts,
  link_id + actor_type ~ scenario,
  value.var = "event_count",
  fill = 0
)

if (all(c("base", "parking") %in% names(link_type_wide))) {
  link_type_wide[, diff := parking - base]
  link_type_wide[, rel_diff_pct := 100 * diff / pmax(base, 1)]
}
setorder(link_type_wide, -diff)

fwrite(link_type_wide, file.path(out_dir, "07_hotspot_link_type_event_counts_wide.csv"))

# =========================================================
# 7. TIME DISTRIBUTION
# =========================================================

all_hot_events[, hour := as.integer(floor(time / 3600))]

type_hour_counts <- all_hot_events[, .(
  event_count = .N
), by = .(scenario, actor_type, hour)][order(scenario, actor_type, hour)]

fwrite(type_hour_counts, file.path(out_dir, "08_hotspot_type_hour_counts.csv"))

# =========================================================
# 8. MERGE WITH HOTSPOT LINK ATTRIBUTES
# =========================================================

merge_cols <- intersect(
  c("link_id", "x_from", "y_from", "x_to", "y_to", "cx", "cy", "vol_diff", "vol_car_base", "vol_car_parking"),
  names(hotspot_links_dt)
)

if (length(merge_cols) > 1) {
  link_type_wide_geo <- merge(
    link_type_wide,
    unique(hotspot_links_dt[, ..merge_cols]),
    by = "link_id",
    all.x = TRUE
  )
  fwrite(link_type_wide_geo, file.path(out_dir, "09_hotspot_link_type_event_counts_wide_with_geo.csv"))
}

# =========================================================
# 9. QUICK CONSOLE OUTPUT
# =========================================================

cat("\n=========================================\n")
cat("Absolute hotspot event counts by type:\n")
print(type_event_wide)

cat("\nUnique hotspot actors by type:\n")
print(type_unique_wide)

cat("\nTop 20 link-type increases on hotspot links:\n")
print(link_type_wide[1:min(20, .N)])

cat("\nOutputs saved to:\n")
cat(out_dir, "\n")
cat("=========================================\n")