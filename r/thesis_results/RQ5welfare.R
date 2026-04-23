# ===============================================================
# RQ5 - Welfare (Delta Score) by Income Quintile
# ===============================================================

rm(list = ls())
gc()

library(data.table)

cat("🚀 Starting RQ5 Welfare Analysis...\n")

# ---------------------------------------------------------------
# 1. File paths
# ---------------------------------------------------------------
base_path  <- "D:/10pct/base-10pct-500/base-10pct-500.output_persons.csv.gz"
maut_path  <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_persons.csv.gz"
park_path  <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_persons.csv.gz"

# ---------------------------------------------------------------
# 2. Read data
# ---------------------------------------------------------------
base  <- fread(base_path)
maut  <- fread(maut_path)
park  <- fread(park_path)

# keep only persons with income
base  <- base[!is.na(income)]
maut  <- maut[person %in% base$person]
park  <- park[person %in% base$person]

# ---------------------------------------------------------------
# 3. Create income quintile (based on base)
# ---------------------------------------------------------------
base[, income_quintile := cut(
  income,
  breaks = quantile(income, probs = seq(0, 1, 0.2), na.rm = TRUE),
  include.lowest = TRUE,
  labels = FALSE
)]

# ---------------------------------------------------------------
# 4. Merge scores
# ---------------------------------------------------------------
scores <- merge(
  base[, .(person, base_score = executed_score, income_quintile)],
  maut[, .(person, maut_score = executed_score)],
  by = "person"
)

scores <- merge(
  scores,
  park[, .(person, park_score = executed_score)],
  by = "person"
)

# ---------------------------------------------------------------
# 5. Calculate delta scores
# ---------------------------------------------------------------
scores[, delta_maut := maut_score - base_score]
scores[, delta_park := park_score - base_score]

# ---------------------------------------------------------------
# 6. Welfare by quintile
# ---------------------------------------------------------------
welfare_quintile <- scores[, .(
  avg_delta_maut = mean(delta_maut, na.rm = TRUE),
  avg_delta_park = mean(delta_park, na.rm = TRUE),
  median_delta_maut = median(delta_maut, na.rm = TRUE),
  median_delta_park = median(delta_park, na.rm = TRUE)
), by = income_quintile]

cat("\n=== Welfare Change by Income Quintile ===\n")
print(welfare_quintile)

# ---------------------------------------------------------------
# 7. Share of losers vs gainers
# ---------------------------------------------------------------
loss_gain <- scores[, .(
  share_loser_maut = mean(delta_maut < 0, na.rm = TRUE),
  share_loser_park = mean(delta_park < 0, na.rm = TRUE),
  share_gainer_maut = mean(delta_maut > 0, na.rm = TRUE),
  share_gainer_park = mean(delta_park > 0, na.rm = TRUE)
), by = income_quintile]

cat("\n=== Share of Losers / Gainers by Quintile ===\n")
print(loss_gain)

cat("\n🎉 RQ5 Welfare Analysis Complete.\n")