# ===============================================================
# Figure 2: Trip-level modal change within Hundekopf relative to base
# ===============================================================

rm(list = ls())
gc()

library(data.table)
library(ggplot2)
library(scales)

# ---------------------------------------------------------------
# 1. 数据
# ---------------------------------------------------------------

trip_df <- data.table(
  Mode = c("car", "pt", "bike", "walk", "ride"),
  Base = c(60567, 144163, 86442, 109713, 15531),
  Toll = c(19442, 169983, 97261, 113488, 16239),
  Parking = c(21227, 166206, 98881, 114013, 16089)
)

# 计算相对 base 的变化量
trip_df[, `Δ Toll` := Toll - Base]
trip_df[, `Δ Parking` := Parking - Base]

# 转成长表
trip_delta <- melt(
  trip_df[, .(Mode, `Δ Toll`, `Δ Parking`)],
  id.vars = "Mode",
  variable.name = "Scenario",
  value.name = "Delta"
)

# mode 顺序
trip_delta[, Mode := factor(Mode, levels = c("car", "pt", "bike", "walk", "ride"))]

# ---------------------------------------------------------------
# 2. 作图
# ---------------------------------------------------------------

p_trip_delta <- ggplot(trip_delta, aes(x = Delta, y = Mode, fill = Scenario)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = c("Δ Toll" = "#E74C3C", "Δ Parking" = "#1ABC9C")) +
  labs(
    title = "Trip-level modal change within Hundekopf relative to the base scenario",
    x = "Change in number of trips",
    y = "Mode",
    fill = "Scenario"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

print(p_trip_delta)

# ---------------------------------------------------------------
# 3. 保存
# ---------------------------------------------------------------

output_dir <- "D:/10pct/results"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  file.path(output_dir, "figure_trip_level_mode_change_hundekopf.png"),
  p_trip_delta,
  width = 9,
  height = 5.5,
  dpi = 300
)