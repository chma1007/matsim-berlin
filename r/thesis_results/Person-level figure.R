# ===============================================================
# Figure 1: Person-level mode change across scenarios
# (absolute number of persons, not percentages)
# ===============================================================

rm(list = ls())
gc()

library(data.table)
library(ggplot2)
library(scales)

# ---------------------------------------------------------------
# 1. 数据
# ---------------------------------------------------------------

person_df <- data.table(
  Mode = c("car", "pt", "bike", "walk", "ride"),
  Base = c(95193, 136353, 102852, 193254, 70191),
  Toll = c(76270, 152877, 108359, 196850, 70601),
  Parking = c(82854, 146618, 108775, 196247, 70193)
)

# 转成长表
person_long <- melt(
  person_df,
  id.vars = "Mode",
  variable.name = "Scenario",
  value.name = "Persons"
)

# x轴顺序
person_long[, Scenario := factor(Scenario, levels = c("Base", "Toll", "Parking"))]

# mode 顺序（图例顺序）
person_long[, Mode := factor(Mode, levels = c("car", "pt", "bike", "walk", "ride"))]

# ---------------------------------------------------------------
# 2. 作图
# ---------------------------------------------------------------

p_person_line <- ggplot(person_long, aes(x = Scenario, y = Persons, group = Mode, color = Mode)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Person-level mode change across scenarios (10% sample)",
    x = "Scenario",
    y = "Number of persons",
    color = "Mode"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(p_person_line)

# ---------------------------------------------------------------
# 3. 保存
# ---------------------------------------------------------------

output_dir <- "D:/10pct/results"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  file.path(output_dir, "figure_person_level_mode_change_line.png"),
  p_person_line,
  width = 9,
  height = 5.5,
  dpi = 300
)