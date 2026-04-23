library(data.table)
library(ggplot2)
library(dbscan)

# =========================================================
# 1. 读取 hotspot 数据（你之前生成的）
# =========================================================

hotspots <- fread("D:/10pct/hotspot_cause_analysis_v2/02_selected_hotspot_links.csv")

# 如果你想更稳一点，可以用全网 top 200
# hotspots <- fread("D:/10pct/hotspot_cause_analysis_v2/01_all_links_summary.csv")[1:200]

# =========================================================
# 2. 构造坐标（用 link 中点）
# =========================================================

hotspots[, cx := (x_from + x_to)/2]
hotspots[, cy := (y_from + y_to)/2]

coords <- as.matrix(hotspots[, .(cx, cy)])

# =========================================================
# 3. DBSCAN 聚类
# =========================================================

# eps = 聚类半径（单位：米，Berlin网络）
# 500–1000 可以试
cl <- dbscan(coords, eps = 800, minPts = 3)

hotspots[, cluster := cl$cluster]

# =========================================================
# 4. cluster summary
# =========================================================

cluster_summary <- hotspots[, .(
  n_links = .N,
  total_vol_diff = sum(vol_diff, na.rm = TRUE),
  avg_vol_diff = mean(vol_diff, na.rm = TRUE)
), by = cluster][order(-total_vol_diff)]

print(cluster_summary)

fwrite(cluster_summary, "D:/10pct/hotspot_cause_analysis_v2/40_cluster_summary.csv")

# =========================================================
# 5. 可视化
# =========================================================

p <- ggplot(hotspots, aes(cx, cy, color = factor(cluster))) +
  geom_point(size = 2) +
  theme_minimal() +
  coord_equal() +
  labs(
    title = "Hotspot clusters (DBSCAN)",
    color = "cluster"
  )

ggsave("D:/10pct/hotspot_cause_analysis_v2/41_cluster_map.png",
       p, width = 8, height = 7, dpi = 300)

# =========================================================
# 6. 标记最大 cluster（重点）
# =========================================================

largest_cluster <- cluster_summary$cluster[1]

cat("\nLargest cluster ID:", largest_cluster, "\n")

largest_links <- hotspots[cluster == largest_cluster]

fwrite(largest_links,
       "D:/10pct/hotspot_cause_analysis_v2/42_largest_cluster_links.csv")

# =========================================================
# 7. 单独画最大 cluster
# =========================================================

p2 <- ggplot(largest_links,
             aes(x = x_from, y = y_from,
                 xend = x_to, yend = y_to,
                 color = vol_diff)) +
  geom_segment(linewidth = 1) +
  coord_equal() +
  theme_minimal() +
  scale_color_gradient2(midpoint = 0) +
  labs(
    title = "Largest hotspot cluster",
    color = "vol diff"
  )

ggsave("D:/10pct/hotspot_cause_analysis_v2/43_largest_cluster_map.png",
       p2, width = 8, height = 7, dpi = 300)

cat("\nCluster analysis done.\n")