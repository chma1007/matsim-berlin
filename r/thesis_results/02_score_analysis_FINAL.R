# ============================================
# FINAL SCORE / WELFARE ANALYSIS
# ============================================

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)

# --------------------------------------------
# 1. READ FILES
# --------------------------------------------

base  <- fread("D:/10pct/base-10pct-500/base-10pct-500.output_persons.csv.gz")
maut  <- fread("D:/10pct/maut-10pct-500/maut-10pct-500.output_persons.csv.gz")
park  <- fread("D:/10pct/parking-10pct-500/parking-10pct-500.output_persons.csv.gz")

# --------------------------------------------
# 2. SELECT SCORE + INCOME
# --------------------------------------------

base_s  <- base  %>% select(person, income, executed_score)
maut_s  <- maut  %>% select(person, executed_score)
park_s  <- park  %>% select(person, executed_score)

colnames(base_s)[3] <- "score_base"
colnames(maut_s)[2] <- "score_maut"
colnames(park_s)[2] <- "score_park"

# --------------------------------------------
# 3. MERGE SCENARIOS
# --------------------------------------------

scores <- base_s %>%
  left_join(maut_s, by="person") %>%
  left_join(park_s, by="person")

# --------------------------------------------
# 4. CALCULATE SCORE CHANGE
# --------------------------------------------

scores <- scores %>%
  mutate(
    delta_maut = score_maut - score_base,
    delta_park = score_park - score_base
  )

# Remove NA income
scores_clean <- scores %>%
  filter(!is.na(income))

# --------------------------------------------
# 5. OVERALL WELFARE SUMMARY
# --------------------------------------------

overall_summary <- scores_clean %>%
  summarise(
    mean_delta_maut = mean(delta_maut, na.rm=TRUE),
    mean_delta_park = mean(delta_park, na.rm=TRUE),
    median_delta_maut = median(delta_maut, na.rm=TRUE),
    median_delta_park = median(delta_park, na.rm=TRUE)
  )

cat("\n================ OVERALL WELFARE =================\n")
print(overall_summary)

# --------------------------------------------
# 6. WELFARE BY INCOME GROUP
# --------------------------------------------

scores_clean <- scores_clean %>%
  mutate(income_group = ntile(income, 3))

welfare_by_income <- scores_clean %>%
  group_by(income_group) %>%
  summarise(
    avg_delta_maut = mean(delta_maut, na.rm=TRUE),
    avg_delta_park = mean(delta_park, na.rm=TRUE)
  )

cat("\n================ WELFARE BY INCOME GROUP =================\n")
print(welfare_by_income)

# --------------------------------------------
# 7. DENSITY PLOT — MAUT
# --------------------------------------------

plot_maut <- ggplot(scores_clean, aes(x = delta_maut)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-20, 20) +
  labs(
    title = "Distribution of Welfare Change (Maut)",
    x = "Delta Score (Maut - Base)",
    y = "Density"
  ) +
  theme_minimal()

print(plot_maut)

# --------------------------------------------
# 8. DENSITY PLOT — PARKING
# --------------------------------------------

plot_park <- ggplot(scores_clean, aes(x = delta_park)) +
  geom_density(fill = "darkred", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-20, 20) +
  labs(
    title = "Distribution of Welfare Change (Parking)",
    x = "Delta Score (Parking - Base)",
    y = "Density"
  ) +
  theme_minimal()

print(plot_park)

# --------------------------------------------
# 9. COMPARISON PLOT
# --------------------------------------------

scores_long <- scores_clean %>%
  select(delta_maut, delta_park) %>%
  pivot_longer(cols = everything(),
               names_to = "policy",
               values_to = "delta")

plot_compare <- ggplot(scores_long, aes(x = delta, fill = policy)) +
  geom_density(alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  xlim(-20, 20) +
  labs(
    title = "Distribution of Welfare Change (Comparison)",
    x = "Delta Score",
    y = "Density"
  ) +
  theme_minimal()

print(plot_compare)

# --------------------------------------------
# 10. SAVE RESULTS
# --------------------------------------------

write.csv(welfare_by_income, "welfare_by_income.csv", row.names = FALSE)

# ============================================
# END
# ============================================