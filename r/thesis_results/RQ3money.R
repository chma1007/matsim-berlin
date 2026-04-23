# ===============================================================
# RQ3 - Distribution of Monetary Costs by Income Quintile
# (Corrected Version – NA Removed)
# ===============================================================

rm(list = ls())
gc()

library(data.table)

cat("🚀 Starting RQ3 Distribution Analysis (Corrected)...\n")

# ---------------------------------------------------------------
# 1. File paths
# ---------------------------------------------------------------
base_persons_path <- "D:/10pct/base-10pct-500/base-10pct-500.output_persons.csv.gz"
maut_events_path  <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_personMoneyEvents.tsv.gz"
park_events_path  <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_personMoneyEvents.tsv.gz"

# ---------------------------------------------------------------
# 2. Load persons and create income quintile
# ---------------------------------------------------------------
base_persons <- fread(base_persons_path)

# keep only persons with income
persons_inc <- base_persons[!is.na(income)]

# create quintiles
persons_inc[, income_quintile := cut(
  income,
  breaks = quantile(income, probs = seq(0, 1, 0.2), na.rm = TRUE),
  include.lowest = TRUE,
  labels = FALSE
)]

cat("✅ Income quintiles created.\n")
print(table(persons_inc$income_quintile))

# ---------------------------------------------------------------
# 3. Load PersonMoneyEvents
# ---------------------------------------------------------------
maut_events <- fread(maut_events_path)
park_events <- fread(park_events_path)

# keep expenditures only
maut_events <- maut_events[amount < 0]
park_events <- park_events[amount < 0]

maut_events[, amount := abs(amount)]
park_events[, amount := abs(amount)]

# aggregate to person level
maut_ind <- maut_events[, .(charge = sum(amount)), by = person]
park_ind <- park_events[, .(charge = sum(amount)), by = person]

# ---------------------------------------------------------------
# 4. Merge with income quintile
# ---------------------------------------------------------------
maut_income <- merge(
  maut_ind,
  persons_inc[, .(person, income, income_quintile)],
  by = "person",
  all.x = FALSE
)

park_income <- merge(
  park_ind,
  persons_inc[, .(person, income, income_quintile)],
  by = "person",
  all.x = FALSE
)

# ---------------------------------------------------------------
# 5. Share of payers
# ---------------------------------------------------------------
total_by_q <- persons_inc[, .N, by = income_quintile]
setnames(total_by_q, "N", "total_people")

payer_maut <- maut_income[, .N, by = income_quintile]
setnames(payer_maut, "N", "payers_maut")

payer_park <- park_income[, .N, by = income_quintile]
setnames(payer_park, "N", "payers_park")

share_df <- merge(total_by_q, payer_maut, by = "income_quintile", all.x = TRUE)
share_df <- merge(share_df, payer_park, by = "income_quintile", all.x = TRUE)

share_df[is.na(share_df)] <- 0

share_df[, share_maut := payers_maut / total_people]
share_df[, share_park := payers_park / total_people]

cat("\n=== Share of Payers by Quintile ===\n")
print(share_df[, .(income_quintile, share_maut, share_park)])

# ---------------------------------------------------------------
# 6. Average charge (payer only)
# ---------------------------------------------------------------
avg_maut <- maut_income[, .(
  avg_charge_maut = mean(charge, na.rm = TRUE)
), by = income_quintile]

avg_park <- park_income[, .(
  avg_charge_park = mean(charge, na.rm = TRUE)
), by = income_quintile]

avg_df <- merge(avg_maut, avg_park, by = "income_quintile")

cat("\n=== Average Charge (Payers Only) ===\n")
print(avg_df)

# ---------------------------------------------------------------
# 7. Burden (daily charge / monthly income)
# ---------------------------------------------------------------
maut_income[, burden := charge / income]
park_income[, burden := charge / income]

burden_maut <- maut_income[, .(
  avg_burden_maut = mean(burden, na.rm = TRUE)
), by = income_quintile]

burden_park <- park_income[, .(
  avg_burden_park = mean(burden, na.rm = TRUE)
), by = income_quintile]

burden_df <- merge(burden_maut, burden_park, by = "income_quintile")

cat("\n=== Average Burden (Charge / Monthly Income) ===\n")
print(burden_df)

cat("\n🎉 RQ3 Distribution Analysis Complete.\n")