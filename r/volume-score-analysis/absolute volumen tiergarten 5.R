# =========================================================
# Full-network hotspot clustering for MATSim links
# Goal:
#   Identify hotspot clusters across the ENTIRE network
#   without restricting to Tiergarten or any bbox
# =========================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(xml2)
  library(dbscan)
})

# =========================================================
# 0. PATHS
# =========================================================

base_dir <- "D:/10pct/base-10pct-500"
park_dir <- "D:/10pct/parking-10pct-500"
out_dir  <- "D:/10pct/full_network_clustering"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

base_links_file <- file.path(base_dir, "base-10pct-500.output_links.csv.gz")
park_links_file <- file.path(park_dir, "parking-10pct-500.output_links.csv.gz")
network_file    <- file.path(park_dir, "parking-10pct-500.output_network.xml.gz")

# =========================================================
# 1. SETTINGS
# =========================================================

# how many top links (by absolute volume increase) to cluster
top_n_links <- 100

# DBSCAN parameters
# eps is in map units (meters for your network)
dbscan_eps <- 800
dbscan_minPts <- 3

# =========================================================
# 2. HELPERS
# =========================================================

safe_num <- function(x) suppressWarnings(as.numeric(x))

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

# standardize names
setnames(base_links,
         old = c("link", "from_node", "to_node"),
         new = c("link_id", "from", "to"),
         skip_absent = TRUE)

setnames(park_links,
         old = c("link", "from_node", "to_node"),
         new = c("link_id", "from", "to"),
         skip_absent = TRUE)

# keep needed columns
keep_cols <- c(
  "link_id", "from", "to", "length", "capacity", "lanes", "vol_car"
)

base_links <- base_links[, intersect(keep_cols, names(base_links)), with = FALSE]
park_links <- park_links[, intersect(keep_cols, names(park_links)), with = FALSE]

# add coordinates
base_links <- add_coords(base_links, nodes_dt)
park_links <- add_coords(park_links, nodes_dt)

# =========================================================
# 5. MERGE BASE + PARKING AND COMPUTE VOLUME DIFFERENCE
# =========================================================

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

wide_links[, vol_car_base := safe_num(vol_car_base)]
wide_links[, vol_car_parking := safe_num(vol_car_parking)]
wide_links[, vol_diff := vol_car_parking - vol_car_base]
wide_links[, rel_diff_pct := 100 * vol_diff / pmax(vol_car_base, 1)]

# link midpoint
wide_links[, cx := (x_from + x_to) / 2]
wide_links[, cy := (y_from + y_to) / 2]

# =========================================================
# 6. SAVE FULL SUMMARY
# =========================================================

summary_all <- wide_links[, .(
  link_id,
  from = from_base,
  to = to_base,
  x_from, y_from, x_to, y_to, cx, cy,
  length_m = safe_num(length_base),
  capacity = safe_num(capacity_base),
  lanes = safe_num(lanes_base),
  vol_car_base,
  vol_car_parking,
  vol_diff,
  rel_diff_pct
)]

setorder(summary_all, -vol_diff)

fwrite(summary_all, file.path(out_dir, "01_all_links_summary.csv"))
fwrite(summary_all[1:min(500, .N)], file.path(out_dir, "02_top500_links_by_vol_diff.csv"))

cat("\nTop 10 links by vol_diff:\n")
print(summary_all[1:10])

# =========================================================
# 7. SELECT TOP N LINKS FOR FULL-NETWORK CLUSTERING
# =========================================================

top_links <- summary_all[1:min(top_n_links, .N)]

fwrite(top_links, file.path(out_dir, "03_top_links_for_clustering.csv"))

# =========================================================
# 8. DBSCAN CLUSTERING
# =========================================================

coords <- as.matrix(top_links[, .(cx, cy)])

cat("\nRunning DBSCAN...\n")
cl <- dbscan(coords, eps = dbscan_eps, minPts = dbscan_minPts)

top_links[, cluster := cl$cluster]

# cluster = 0 means noise / unclustered points
cluster_summary <- top_links[, .(
  n_links = .N,
  total_vol_diff = sum(vol_diff, na.rm = TRUE),
  avg_vol_diff = mean(vol_diff, na.rm = TRUE),
  min_x = min(cx, na.rm = TRUE),
  max_x = max(cx, na.rm = TRUE),
  min_y = min(cy, na.rm = TRUE),
  max_y = max(cy, na.rm = TRUE)
), by = cluster][order(-total_vol_diff)]

fwrite(cluster_summary, file.path(out_dir, "04_cluster_summary.csv"))

cat("\nCluster summary:\n")
print(cluster_summary)

# =========================================================
# 9. IDENTIFY LARGEST NON-NOISE CLUSTER
# =========================================================

non_noise_clusters <- cluster_summary[cluster != 0]

if (nrow(non_noise_clusters) > 0) {
  largest_cluster_id <- non_noise_clusters[1, cluster]
  largest_cluster_links <- top_links[cluster == largest_cluster_id]
  
  fwrite(largest_cluster_links, file.path(out_dir, "05_largest_cluster_links.csv"))
  
  cat("\nLargest non-noise cluster ID:", largest_cluster_id, "\n")
  cat("Number of links in largest cluster:", nrow(largest_cluster_links), "\n")
} else {
  largest_cluster_id <- NA_integer_
  largest_cluster_links <- data.table()
  cat("\nNo non-noise cluster found.\n")
}

# =========================================================
# 10. PLOTS
# =========================================================

# 10.1 Full network top links colored by cluster
p_cluster <- ggplot(top_links, aes(cx, cy, color = factor(cluster))) +
  geom_point(size = 2.2, alpha = 0.9) +
  coord_equal() +
  theme_minimal() +
  labs(
    title = paste0("Full-network hotspot clusters (Top ", top_n_links, " links, DBSCAN)"),
    x = "x",
    y = "y",
    color = "cluster"
  )

ggsave(file.path(out_dir, "06_full_network_cluster_map.png"),
       p_cluster, width = 9, height = 7, dpi = 300)

# 10.2 Full network top links colored by absolute volume increase
p_vol <- ggplot(top_links, aes(cx, cy, color = vol_diff)) +
  geom_point(size = 2.2, alpha = 0.9) +
  coord_equal() +
  theme_minimal() +
  scale_color_gradient2(midpoint = 0) +
  labs(
    title = paste0("Top ", top_n_links, " links by absolute volume increase"),
    x = "x",
    y = "y",
    color = "vol_diff"
  )

ggsave(file.path(out_dir, "07_full_network_top_links_by_vol_diff.png"),
       p_vol, width = 9, height = 7, dpi = 300)

# 10.3 Largest cluster as line segments
if (nrow(largest_cluster_links) > 0) {
  p_largest <- ggplot(largest_cluster_links,
                      aes(x = x_from, y = y_from, xend = x_to, yend = y_to, color = vol_diff)) +
    geom_segment(linewidth = 1.0, alpha = 0.95) +
    coord_equal() +
    theme_minimal() +
    scale_color_gradient2(midpoint = 0) +
    labs(
      title = paste0("Largest hotspot cluster (cluster ", largest_cluster_id, ")"),
      x = "x",
      y = "y",
      color = "vol_diff"
    )
  
  ggsave(file.path(out_dir, "08_largest_cluster_map.png"),
         p_largest, width = 9, height = 7, dpi = 300)
}

# =========================================================
# 11. OPTIONAL: COMPARE TOP CLUSTERS ONLY (EXCLUDING NOISE)
# =========================================================

top_links_non_noise <- top_links[cluster != 0]

if (nrow(top_links_non_noise) > 0) {
  cluster_ranked <- top_links_non_noise[, .(
    n_links = .N,
    total_vol_diff = sum(vol_diff, na.rm = TRUE),
    avg_vol_diff = mean(vol_diff, na.rm = TRUE)
  ), by = cluster][order(-total_vol_diff)]
  
  fwrite(cluster_ranked, file.path(out_dir, "09_ranked_non_noise_clusters.csv"))
  
  cat("\nRanked non-noise clusters:\n")
  print(cluster_ranked)
}

# =========================================================
# 12. QUICK INTERPRETATION TEXT FILE
# =========================================================

sink(file.path(out_dir, "10_interpretation_notes.txt"))

cat("Full-network hotspot clustering summary\n")
cat("=====================================\n\n")
cat("Top N links used for clustering:", top_n_links, "\n")
cat("DBSCAN eps:", dbscan_eps, "\n")
cat("DBSCAN minPts:", dbscan_minPts, "\n\n")

cat("Cluster summary:\n")
print(cluster_summary)

cat("\nInterpretation guide:\n")
cat("- cluster = 0 means noise / isolated links\n")
cat("- a large cluster with many links and high total_vol_diff suggests a dominant hotspot area\n")
cat("- multiple non-noise clusters suggest several hotspot regions in the network\n")
cat("- if one cluster clearly dominates the others, that supports the claim that one area (e.g. Tiergarten) is the main hotspot\n")

sink()

# =========================================================
# 13. FINAL CONSOLE OUTPUT
# =========================================================

cat("\n=========================================\n")
cat("Full-network clustering completed.\n")
cat("Outputs saved to:\n", out_dir, "\n")
cat("Main files:\n")
cat(" - 04_cluster_summary.csv\n")
cat(" - 06_full_network_cluster_map.png\n")
cat(" - 08_largest_cluster_map.png\n")
cat("=========================================\n")