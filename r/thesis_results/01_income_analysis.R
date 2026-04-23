# ============================================
# FINAL INCOME BURDEN ANALYSIS
# ============================================

library(data.table)
library(dplyr)

# --------------------------------------------
# 1. READ FILES
# --------------------------------------------

base  <- fread("D:/10pct/base-10pct-500/base-10pct-500.output_persons.csv.gz")
maut_money  <- fread("D:/10pct/maut-10pct-500/maut-10pct-500.output_personMoneyEvents.tsv.gz")
park_money  <- fread("D:/10pct/parking-10pct-500/parking-10pct-500.output_personMoneyEvents.tsv.gz")

# --------------------------------------------
# 2. AGGREGATE PAYMENTS PER PERSON
# --------------------------------------------

money_maut_person <- maut_money %>%
  group_by(person) %>%
  summarise(payment_maut = -sum(amount))

money_park_person <- park_money %>%
  group_by(person) %>%
  summarise(payment_park = -sum(amount))

# --------------------------------------------
# 3. MERGE WITH INCOME
# --------------------------------------------

income_data <- base %>%
  select(person, income) %>%
  left_join(money_maut_person, by="person") %>%
  left_join(money_park_person, by="person")

# Replace NA payments with 0
income_data$payment_maut[is.na(income_data$payment_maut)] <- 0
income_data$payment_park[is.na(income_data$payment_park)] <- 0

# Remove NA income
income_data <- income_data %>%
  filter(!is.na(income))

# --------------------------------------------
# 4. CREATE INCOME GROUPS (TERTILES)
# --------------------------------------------

income_data <- income_data %>%
  mutate(income_group = ntile(income, 5))

# ============================================
# PART A — FULL POPULATION AVERAGE
# ============================================

income_summary_full <- income_data %>%
  group_by(income_group) %>%
  summarise(
    avg_income = mean(income),
    avg_payment_maut = mean(payment_maut),
    avg_payment_park = mean(payment_park),
    share_maut = avg_payment_maut / avg_income,
    share_park = avg_payment_park / avg_income
  )

cat("\n================ FULL POPULATION =================\n")
print(income_summary_full)

# ============================================
# PART B — PAYERS ONLY
# ============================================

income_data_paid <- income_data %>%
  filter(payment_maut > 0 | payment_park > 0)

income_summary_paid <- income_data_paid %>%
  group_by(income_group) %>%
  summarise(
    avg_income = mean(income),
    avg_payment_maut = mean(payment_maut),
    avg_payment_park = mean(payment_park),
    share_maut = avg_payment_maut / avg_income,
    share_park = avg_payment_park / avg_income
  )

cat("\n================ PAYERS ONLY =================\n")
print(income_summary_paid)

# ============================================
# PART C — SHARE OF PAYERS
# ============================================

prop_payers <- income_data %>%
  group_by(income_group) %>%
  summarise(
    share_paying_maut = mean(payment_maut > 0),
    share_paying_park = mean(payment_park > 0)
  )

cat("\n================ SHARE OF PAYERS =================\n")
print(prop_payers)

# ============================================
# SAVE RESULTS
# ============================================

write.csv(income_summary_full, "income_summary_full.csv", row.names = FALSE)
write.csv(income_summary_paid, "income_summary_paid.csv", row.names = FALSE)
write.csv(prop_payers, "income_share_payers.csv", row.names = FALSE)

# ============================================
# END
# ============================================