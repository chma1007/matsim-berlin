# ===============================================================
# Figure: Total charges by activity type
# grouped horizontal bar chart
# ===============================================================

rm(list = ls())
gc()

library(data.table)
library(ggplot2)
library(scales)

# ---------------------------------------------------------------
# 1. 数据
# ---------------------------------------------------------------

activity_df <- data.table(
  Activity = c(
    "Service", "Commercial", "Home", "Work", "Leisure", "Transport",
    "Work (Business)", "Shop (Daily)", "Personal Business", "Shop (Other)",
    "Freight", "Dining", "Outside Recreation", "Other",
    "Education (Other)", "Higher Education", "Secondary School", "Kindergarten"
  ),
  Maut = c(
    138369.81, 30304.89, 24323.23, 12631.44, 9745.74, 4944.90,
    4613.57, 4263.32, 3715.56, 3304.55,
    2938.02, 2325.92, 1131.21, 1064.10,
    821.06, 650.11, 40.67, 50.15
  ),
  Parking = c(
    103612.50, 40785.00, 20997.50, 1712.50, 6567.50, 6605.00,
    2465.00, 3957.50, 2560.00, 2837.50,
    0.00, 1725.00, 1142.50, 1107.50,
    227.50, 107.50, 30.00, 25.00
  )
)

# ---------------------------------------------------------------
# 2. 转成长表
# ---------------------------------------------------------------

activity_long <- melt(
  activity_df,
  id.vars = "Activity",
  variable.name = "Policy",
  value.name = "Charge"
)

# 按 Maut+Parking 总和排序（从高到低）
activity_order <- activity_df[, .(total = Maut + Parking), by = Activity][order(total)]$Activity
activity_long[, Activity := factor(Activity, levels = activity_order)]

# ---------------------------------------------------------------
# 3. 作图
# ---------------------------------------------------------------

p_activity <- ggplot(activity_long, aes(x = Charge, y = Activity, fill = Policy)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = c("Maut" = "#E74C3C", "Parking" = "#1ABC9C")) +
  labs(
    title = "Total charges by activity type (10% sample)",
    subtitle = "Distance-based toll vs. parking fee policy",
    x = "Total charge (€)",
    y = "Activity type",
    fill = "Policy"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  )

print(p_activity)

# ---------------------------------------------------------------
# 4. 保存
# ---------------------------------------------------------------

output_dir <- "D:/10pct/results"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

ggsave(
  file.path(output_dir, "figure_activity_charge_comparison.png"),
  p_activity,
  width = 10,
  height = 7.5,
  dpi = 300
)