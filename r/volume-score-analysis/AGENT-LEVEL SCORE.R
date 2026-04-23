# =========================================================
# AGENT-LEVEL SCORE / BEHAVIOUR ANALYSIS
# FINAL FIXED VERSION
# Base vs Maut / Base vs Parking
# =========================================================

library(data.table)
library(ggplot2)

# =========================================================
# 1. FILE PATHS
# =========================================================

# persons
file_base_persons    <- "D:/10pct/base-10pct-500/base-10pct-500.output_persons.csv.gz"
file_maut_persons    <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_persons.csv.gz"
file_parking_persons <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_persons.csv.gz"

# trips
file_base_trips    <- "D:/10pct/base-10pct-500/base-10pct-500.output_trips.csv.gz"
file_maut_trips    <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_trips.csv.gz"
file_parking_trips <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_trips.csv.gz"

# money events
file_base_money    <- "D:/10pct/base-10pct-500/base-10pct-500.output_personMoneyEvents.tsv.gz"
file_maut_money    <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_personMoneyEvents.tsv.gz"
file_parking_money <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_personMoneyEvents.tsv.gz"

# output folder
out_dir <- "D:/10pct/score_behaviour_analysis_final"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# =========================================================
# 2. HELPER FUNCTIONS
# =========================================================

find_person_col <- function(dt, dataset_name = "") {
  possible <- c("person", "person_id", "id", "agent", "agent_id")
  person_col <- possible[possible %in% names(dt)][1]
  if (is.na(person_col)) {
    stop(
      paste0(
        "No person id column found in ", dataset_name,
        ". Columns are: ", paste(names(dt), collapse = ", ")
      )
    )
  }
  return(person_col)
}

find_mode_col <- function(dt, dataset_name = "") {
  possible <- c("main_mode", "mode", "leg_mode", "trip_mode")
  mode_col <- possible[possible %in% names(dt)][1]
  if (is.na(mode_col)) {
    stop(
      paste0(
        "No mode column found in ", dataset_name,
        ". Columns are: ", paste(names(dt), collapse = ", ")
      )
    )
  }
  return(mode_col)
}

convert_trav_time_to_seconds <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  
  x <- as.character(x)
  
  suppressWarnings(num_x <- as.numeric(x))
  if (sum(!is.na(num_x)) > 0.9 * length(x)) return(num_x)
  
  parse_one <- function(tt) {
    if (is.na(tt) || tt == "") return(NA_real_)
    parts <- strsplit(tt, ":", fixed = TRUE)[[1]]
    if (length(parts) != 3) return(NA_real_)
    as.numeric(parts[1]) * 3600 +
      as.numeric(parts[2]) * 60 +
      as.numeric(parts[3])
  }
  
  return(as.numeric(sapply(x, parse_one)))
}

classify_score_change <- function(x, tol = 1e-6) {
  fifelse(
    is.na(x), "missing",
    fifelse(
      x > tol, "score_up",
      fifelse(x < -tol, "score_down", "score_no_change")
    )
  )
}

get_dominant_mode <- function(dt, person_col, mode_col) {
  tmp <- dt[, .N, by = c(person_col, mode_col)]
  setnames(tmp, c(person_col, mode_col), c("person", "mode"))
  tmp[, person := as.character(person)]
  tmp[, mode := as.character(mode)]
  setorder(tmp, person, -N, mode)
  dom <- tmp[, .SD[1], by = person]
  setnames(dom, "mode", "dominant_mode")
  dom[, person := as.character(person)]
  dom[, dominant_mode := as.character(dominant_mode)]
  return(dom[, .(person, dominant_mode)])
}

resolve_optional_col <- function(dt, base_name) {
  cand <- names(dt)[grepl(paste0("^", base_name, "(\\.|$)"), names(dt))]
  if (length(cand) == 0) return(dt)
  if (base_name %in% names(dt)) return(dt)
  
  dt[, (base_name) := NA]
  for (cc in cand) {
    dt[is.na(get(base_name)) & !is.na(get(cc)), (base_name) := get(cc)]
  }
  return(dt)
}

assign_fixed_income_group <- function(income) {
  fifelse(
    income >= 249 & income < 1167, "Q1",
    fifelse(
      income >= 1167 & income < 1571, "Q2",
      fifelse(
        income >= 1571 & income < 2050, "Q3",
        fifelse(
          income >= 2050 & income < 2733, "Q4",
          fifelse(
            income >= 2733 & income <= 6100, "Q5",
            NA_character_
          )
        )
      )
    )
  )
}

force_person_character <- function(dt) {
  if ("person" %in% names(dt)) {
    dt[, person := as.character(person)]
  }
  return(dt)
}

# =========================================================
# 3. READ PERSONS
# =========================================================

read_persons <- function(file, scenario_name) {
  dt <- fread(file)
  cat("\n============================\n")
  cat("Reading persons:", scenario_name, "\n")
  print(names(dt))
  
  person_col <- find_person_col(dt, paste0("persons_", scenario_name))
  
  if (!("executed_score" %in% names(dt))) {
    stop(paste0("'executed_score' not found in persons file for ", scenario_name))
  }
  
  optional_cols <- c("income", "home_x", "home_y", "subpopulation", "sex", "age")
  keep_optional <- optional_cols[optional_cols %in% names(dt)]
  
  dt <- dt[, c(person_col, "executed_score", keep_optional), with = FALSE]
  setnames(dt, person_col, "person")
  dt[, person := as.character(person)]
  dt[, executed_score := as.numeric(executed_score)]
  setnames(dt, "executed_score", paste0("executed_score_", scenario_name))
  
  return(dt)
}

base_persons    <- read_persons(file_base_persons, "base")
maut_persons    <- read_persons(file_maut_persons, "maut")
parking_persons <- read_persons(file_parking_persons, "parking")

# =========================================================
# 4. READ TRIPS
# =========================================================

read_trips_person_summary <- function(file, scenario_name) {
  dt <- fread(file)
  cat("\n============================\n")
  cat("Reading trips:", scenario_name, "\n")
  print(names(dt))
  
  person_col <- find_person_col(dt, paste0("trips_", scenario_name))
  mode_col   <- find_mode_col(dt, paste0("trips_", scenario_name))
  
  if (!("trav_time" %in% names(dt))) {
    stop(paste0("'trav_time' not found in trips file for ", scenario_name))
  }
  
  dt <- dt[!is.na(get(person_col))]
  dt[, (person_col) := as.character(get(person_col))]
  dt[, (mode_col) := as.character(get(mode_col))]
  dt[, trav_time_sec := convert_trav_time_to_seconds(trav_time)]
  
  person_summary <- dt[, .(
    n_trips = .N,
    total_travel_time_sec = sum(trav_time_sec, na.rm = TRUE),
    avg_travel_time_sec_per_trip = mean(trav_time_sec, na.rm = TRUE),
    median_travel_time_sec_per_trip = median(trav_time_sec, na.rm = TRUE)
  ), by = person_col]
  
  setnames(person_summary, person_col, "person")
  person_summary[, person := as.character(person)]
  
  dom_mode <- get_dominant_mode(dt, person_col, mode_col)
  dom_mode[, person := as.character(person)]
  
  person_summary <- merge(person_summary, dom_mode, by = "person", all.x = TRUE)
  person_summary[, dominant_mode := as.character(dominant_mode)]
  
  setnames(
    person_summary,
    old = c("n_trips",
            "total_travel_time_sec",
            "avg_travel_time_sec_per_trip",
            "median_travel_time_sec_per_trip",
            "dominant_mode"),
    new = c(paste0("n_trips_", scenario_name),
            paste0("total_travel_time_sec_", scenario_name),
            paste0("avg_travel_time_sec_per_trip_", scenario_name),
            paste0("median_travel_time_sec_per_trip_", scenario_name),
            paste0("dominant_mode_", scenario_name))
  )
  
  return(list(raw = dt, summary = person_summary))
}

base_trips_obj    <- read_trips_person_summary(file_base_trips, "base")
maut_trips_obj    <- read_trips_person_summary(file_maut_trips, "maut")
parking_trips_obj <- read_trips_person_summary(file_parking_trips, "parking")

base_trips_summary    <- force_person_character(base_trips_obj$summary)
maut_trips_summary    <- force_person_character(maut_trips_obj$summary)
parking_trips_summary <- force_person_character(parking_trips_obj$summary)

# =========================================================
# 5. READ MONEY EVENTS
# =========================================================

read_money_person_summary <- function(file, scenario_name) {
  
  dt <- fread(file, sep = ";")
  if (ncol(dt) == 1) dt <- fread(file, sep = "")
  if (ncol(dt) == 1) dt <- fread(file, sep = "\t")
  
  cat("\n============================\n")
  cat("Reading money events:", scenario_name, "\n")
  print(names(dt))
  
  person_col <- find_person_col(dt, paste0("money_", scenario_name))
  
  if (!("amount" %in% names(dt))) {
    stop(paste0("'amount' not found in money file for ", scenario_name))
  }
  
  dt <- dt[!is.na(get(person_col))]
  dt[, (person_col) := as.character(get(person_col))]
  dt[, amount := as.numeric(amount)]
  
  person_summary <- dt[, .(
    n_money_events = .N,
    total_money_amount = sum(amount, na.rm = TRUE),
    total_payment_positive = -sum(amount, na.rm = TRUE),
    avg_money_amount = mean(amount, na.rm = TRUE)
  ), by = person_col]
  
  setnames(person_summary, person_col, "person")
  person_summary[, person := as.character(person)]
  
  setnames(
    person_summary,
    old = c("n_money_events", "total_money_amount", "total_payment_positive", "avg_money_amount"),
    new = c(paste0("n_money_events_", scenario_name),
            paste0("total_money_amount_", scenario_name),
            paste0("total_payment_positive_", scenario_name),
            paste0("avg_money_amount_", scenario_name))
  )
  
  return(list(raw = dt, summary = person_summary))
}

base_money_obj    <- read_money_person_summary(file_base_money, "base")
maut_money_obj    <- read_money_person_summary(file_maut_money, "maut")
parking_money_obj <- read_money_person_summary(file_parking_money, "parking")

base_money_summary    <- force_person_character(base_money_obj$summary)
maut_money_summary    <- force_person_character(maut_money_obj$summary)
parking_money_summary <- force_person_character(parking_money_obj$summary)

# =========================================================
# 6. MERGE ALL PERSON-LEVEL DATA
# =========================================================

base_persons    <- force_person_character(base_persons)
maut_persons    <- force_person_character(maut_persons)
parking_persons <- force_person_character(parking_persons)

master <- merge(base_persons, maut_persons, by = "person", all = TRUE)
master <- merge(master, parking_persons, by = "person", all = TRUE)

master <- merge(master, base_trips_summary, by = "person", all = TRUE)
master <- merge(master, maut_trips_summary, by = "person", all = TRUE)
master <- merge(master, parking_trips_summary, by = "person", all = TRUE)

master <- merge(master, base_money_summary, by = "person", all = TRUE)
master <- merge(master, maut_money_summary, by = "person", all = TRUE)
master <- merge(master, parking_money_summary, by = "person", all = TRUE)

master[, person := as.character(person)]

for (v in c("income", "home_x", "home_y", "subpopulation", "sex", "age")) {
  master <- resolve_optional_col(master, v)
}

# fill missing money totals/counts with 0
for (cc in names(master)) {
  if (grepl("^n_money_events_|^total_money_amount_|^total_payment_positive_", cc)) {
    master[is.na(get(cc)), (cc) := 0]
  }
}

# fill some trip counters with 0 if missing
for (cc in names(master)) {
  if (grepl("^n_trips_", cc)) {
    master[is.na(get(cc)), (cc) := 0]
  }
}

# =========================================================
# 7. CALCULATE DELTAS
# =========================================================

master[, delta_score_maut := executed_score_maut - executed_score_base]
master[, delta_score_parking := executed_score_parking - executed_score_base]

master[, delta_total_tt_sec_maut := total_travel_time_sec_maut - total_travel_time_sec_base]
master[, delta_total_tt_sec_parking := total_travel_time_sec_parking - total_travel_time_sec_base]

master[, delta_avg_tt_sec_trip_maut := avg_travel_time_sec_per_trip_maut - avg_travel_time_sec_per_trip_base]
master[, delta_avg_tt_sec_trip_parking := avg_travel_time_sec_per_trip_parking - avg_travel_time_sec_per_trip_base]

master[, delta_n_trips_maut := n_trips_maut - n_trips_base]
master[, delta_n_trips_parking := n_trips_parking - n_trips_base]

master[, delta_payment_maut := total_payment_positive_maut - total_payment_positive_base]
master[, delta_payment_parking := total_payment_positive_parking - total_payment_positive_base]

master[, group_maut := classify_score_change(delta_score_maut)]
master[, group_parking := classify_score_change(delta_score_parking)]

master[, dominant_mode_base := as.character(dominant_mode_base)]
master[, dominant_mode_maut := as.character(dominant_mode_maut)]
master[, dominant_mode_parking := as.character(dominant_mode_parking)]

master[, mode_transition_maut := paste0(dominant_mode_base, " -> ", dominant_mode_maut)]
master[, mode_transition_parking := paste0(dominant_mode_base, " -> ", dominant_mode_parking)]

master[, switched_mode_maut := dominant_mode_base != dominant_mode_maut]
master[, switched_mode_parking := dominant_mode_base != dominant_mode_parking]

master[, total_travel_time_hr_base := total_travel_time_sec_base / 3600]
master[, total_travel_time_hr_maut := total_travel_time_sec_maut / 3600]
master[, total_travel_time_hr_parking := total_travel_time_sec_parking / 3600]

master[, delta_total_tt_hr_maut := delta_total_tt_sec_maut / 3600]
master[, delta_total_tt_hr_parking := delta_total_tt_sec_parking / 3600]

master[, delta_avg_tt_min_trip_maut := delta_avg_tt_sec_trip_maut / 60]
master[, delta_avg_tt_min_trip_parking := delta_avg_tt_sec_trip_parking / 60]

# =========================================================
# 8. FIXED INCOME GROUPS
# =========================================================

if ("income" %in% names(master)) {
  master[, income := as.numeric(income)]
  master[, income_quintile_fixed := assign_fixed_income_group(income)]
  master[, income_quintile_fixed := factor(
    income_quintile_fixed,
    levels = c("Q1", "Q2", "Q3", "Q4", "Q5")
  )]
}

# =========================================================
# 9. SUMMARY BY SCORE GROUP
# =========================================================

summary_by_group_maut <- master[, .(
  n_agents = .N,
  share = .N / nrow(master),
  mean_delta_score = mean(delta_score_maut, na.rm = TRUE),
  median_delta_score = median(delta_score_maut, na.rm = TRUE),
  mean_delta_payment = mean(delta_payment_maut, na.rm = TRUE),
  median_delta_payment = median(delta_payment_maut, na.rm = TRUE),
  mean_delta_total_tt_hr = mean(delta_total_tt_hr_maut, na.rm = TRUE),
  median_delta_total_tt_hr = median(delta_total_tt_hr_maut, na.rm = TRUE),
  mean_delta_avg_tt_min_trip = mean(delta_avg_tt_min_trip_maut, na.rm = TRUE),
  mean_delta_n_trips = mean(delta_n_trips_maut, na.rm = TRUE),
  share_switched_mode = mean(switched_mode_maut, na.rm = TRUE)
), by = group_maut][order(match(group_maut, c("score_up", "score_down", "score_no_change", "missing")))]

summary_by_group_parking <- master[, .(
  n_agents = .N,
  share = .N / nrow(master),
  mean_delta_score = mean(delta_score_parking, na.rm = TRUE),
  median_delta_score = median(delta_score_parking, na.rm = TRUE),
  mean_delta_payment = mean(delta_payment_parking, na.rm = TRUE),
  median_delta_payment = median(delta_payment_parking, na.rm = TRUE),
  mean_delta_total_tt_hr = mean(delta_total_tt_hr_parking, na.rm = TRUE),
  median_delta_total_tt_hr = median(delta_total_tt_hr_parking, na.rm = TRUE),
  mean_delta_avg_tt_min_trip = mean(delta_avg_tt_min_trip_parking, na.rm = TRUE),
  mean_delta_n_trips = mean(delta_n_trips_parking, na.rm = TRUE),
  share_switched_mode = mean(switched_mode_parking, na.rm = TRUE)
), by = group_parking][order(match(group_parking, c("score_up", "score_down", "score_no_change", "missing")))]

fwrite(summary_by_group_maut, file.path(out_dir, "summary_by_score_group_maut.csv"))
fwrite(summary_by_group_parking, file.path(out_dir, "summary_by_score_group_parking.csv"))

# =========================================================
# 10. MODE TRANSITION TABLES
# =========================================================

transition_maut_all <- master[, .N, by = .(group_maut, mode_transition_maut)][order(group_maut, -N)]
transition_parking_all <- master[, .N, by = .(group_parking, mode_transition_parking)][order(group_parking, -N)]

transition_maut_score_up <- master[group_maut == "score_up", .N, by = mode_transition_maut][order(-N)]
transition_maut_score_down <- master[group_maut == "score_down", .N, by = mode_transition_maut][order(-N)]

transition_parking_score_up <- master[group_parking == "score_up", .N, by = mode_transition_parking][order(-N)]
transition_parking_score_down <- master[group_parking == "score_down", .N, by = mode_transition_parking][order(-N)]

fwrite(transition_maut_all, file.path(out_dir, "mode_transition_by_score_group_maut.csv"))
fwrite(transition_parking_all, file.path(out_dir, "mode_transition_by_score_group_parking.csv"))

fwrite(transition_maut_score_up, file.path(out_dir, "top_mode_transitions_score_up_maut.csv"))
fwrite(transition_maut_score_down, file.path(out_dir, "top_mode_transitions_score_down_maut.csv"))
fwrite(transition_parking_score_up, file.path(out_dir, "top_mode_transitions_score_up_parking.csv"))
fwrite(transition_parking_score_down, file.path(out_dir, "top_mode_transitions_score_down_parking.csv"))

# =========================================================
# 11. SWITCH SUMMARY
# =========================================================

switch_summary_maut <- master[, .(
  n_agents = .N,
  share_switched = mean(switched_mode_maut, na.rm = TRUE),
  mean_delta_score = mean(delta_score_maut, na.rm = TRUE),
  mean_delta_payment = mean(delta_payment_maut, na.rm = TRUE),
  mean_delta_total_tt_hr = mean(delta_total_tt_hr_maut, na.rm = TRUE)
), by = group_maut]

switch_summary_parking <- master[, .(
  n_agents = .N,
  share_switched = mean(switched_mode_parking, na.rm = TRUE),
  mean_delta_score = mean(delta_score_parking, na.rm = TRUE),
  mean_delta_payment = mean(delta_payment_parking, na.rm = TRUE),
  mean_delta_total_tt_hr = mean(delta_total_tt_hr_parking, na.rm = TRUE)
), by = group_parking]

fwrite(switch_summary_maut, file.path(out_dir, "switch_summary_maut.csv"))
fwrite(switch_summary_parking, file.path(out_dir, "switch_summary_parking.csv"))

# =========================================================
# 12. FIXED INCOME GROUP SUMMARIES
# =========================================================

if ("income_quintile_fixed" %in% names(master)) {
  
  income_summary_maut <- master[!is.na(income_quintile_fixed), .(
    n_agents = .N,
    mean_delta_score = mean(delta_score_maut, na.rm = TRUE),
    median_delta_score = median(delta_score_maut, na.rm = TRUE),
    share_score_up = mean(group_maut == "score_up", na.rm = TRUE),
    share_score_down = mean(group_maut == "score_down", na.rm = TRUE),
    mean_delta_payment = mean(delta_payment_maut, na.rm = TRUE),
    median_delta_payment = median(delta_payment_maut, na.rm = TRUE),
    mean_delta_total_tt_hr = mean(delta_total_tt_hr_maut, na.rm = TRUE),
    mean_delta_avg_tt_min_trip = mean(delta_avg_tt_min_trip_maut, na.rm = TRUE),
    share_switched_mode = mean(switched_mode_maut, na.rm = TRUE)
  ), by = income_quintile_fixed][order(income_quintile_fixed)]
  
  income_summary_parking <- master[!is.na(income_quintile_fixed), .(
    n_agents = .N,
    mean_delta_score = mean(delta_score_parking, na.rm = TRUE),
    median_delta_score = median(delta_score_parking, na.rm = TRUE),
    share_score_up = mean(group_parking == "score_up", na.rm = TRUE),
    share_score_down = mean(group_parking == "score_down", na.rm = TRUE),
    mean_delta_payment = mean(delta_payment_parking, na.rm = TRUE),
    median_delta_payment = median(delta_payment_parking, na.rm = TRUE),
    mean_delta_total_tt_hr = mean(delta_total_tt_hr_parking, na.rm = TRUE),
    mean_delta_avg_tt_min_trip = mean(delta_avg_tt_min_trip_parking, na.rm = TRUE),
    share_switched_mode = mean(switched_mode_parking, na.rm = TRUE)
  ), by = income_quintile_fixed][order(income_quintile_fixed)]
  
  fwrite(income_summary_maut, file.path(out_dir, "income_summary_maut_fixed_groups.csv"))
  fwrite(income_summary_parking, file.path(out_dir, "income_summary_parking_fixed_groups.csv"))
}

# =========================================================
# 13. EXPORT AGENT LISTS
# =========================================================

fwrite(master[group_maut == "score_up"], file.path(out_dir, "agents_score_up_maut_full.csv"))
fwrite(master[group_maut == "score_down"], file.path(out_dir, "agents_score_down_maut_full.csv"))
fwrite(master[group_maut == "score_no_change"], file.path(out_dir, "agents_score_no_change_maut_full.csv"))

fwrite(master[group_parking == "score_up"], file.path(out_dir, "agents_score_up_parking_full.csv"))
fwrite(master[group_parking == "score_down"], file.path(out_dir, "agents_score_down_parking_full.csv"))
fwrite(master[group_parking == "score_no_change"], file.path(out_dir, "agents_score_no_change_parking_full.csv"))

fwrite(master[group_maut == "score_up", .(person)], file.path(out_dir, "agent_ids_score_up_maut.csv"))
fwrite(master[group_maut == "score_down", .(person)], file.path(out_dir, "agent_ids_score_down_maut.csv"))
fwrite(master[group_parking == "score_up", .(person)], file.path(out_dir, "agent_ids_score_up_parking.csv"))
fwrite(master[group_parking == "score_down", .(person)], file.path(out_dir, "agent_ids_score_down_parking.csv"))

# =========================================================
# 14. SAVE MASTER TABLE
# =========================================================

fwrite(master, file.path(out_dir, "master_agent_level_analysis.csv"))

# =========================================================
# 15. QUICK PLOTS
# =========================================================

p1 <- ggplot(master[!is.na(delta_score_maut)], aes(x = delta_score_maut)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribution of Delta Score: Maut - Base",
    x = "Delta score",
    y = "Number of agents"
  )

ggsave(file.path(out_dir, "hist_delta_score_maut.png"), p1, width = 10, height = 6, dpi = 300)

p2 <- ggplot(master[!is.na(delta_score_parking)], aes(x = delta_score_parking)) +
  geom_histogram(bins = 100) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Distribution of Delta Score: Parking - Base",
    x = "Delta score",
    y = "Number of agents"
  )

ggsave(file.path(out_dir, "hist_delta_score_parking.png"), p2, width = 10, height = 6, dpi = 300)

p3 <- ggplot(
  master[!is.na(delta_score_maut), .(
    switched = ifelse(switched_mode_maut, "switched_mode", "same_mode"),
    delta_score = delta_score_maut
  )],
  aes(x = switched, y = delta_score)
) +
  geom_boxplot() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Delta Score by Mode-Switch Status: Maut - Base",
    x = "",
    y = "Delta score"
  )

ggsave(file.path(out_dir, "box_delta_score_switched_maut.png"), p3, width = 8, height = 6, dpi = 300)

p4 <- ggplot(
  master[!is.na(delta_score_parking), .(
    switched = ifelse(switched_mode_parking, "switched_mode", "same_mode"),
    delta_score = delta_score_parking
  )],
  aes(x = switched, y = delta_score)
) +
  geom_boxplot() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Delta Score by Mode-Switch Status: Parking - Base",
    x = "",
    y = "Delta score"
  )

ggsave(file.path(out_dir, "box_delta_score_switched_parking.png"), p4, width = 8, height = 6, dpi = 300)

if ("income_quintile_fixed" %in% names(master)) {
  
  income_summary_maut_plot <- fread(file.path(out_dir, "income_summary_maut_fixed_groups.csv"))
  income_summary_parking_plot <- fread(file.path(out_dir, "income_summary_parking_fixed_groups.csv"))
  
  p5 <- ggplot(income_summary_maut_plot, aes(x = income_quintile_fixed, y = mean_delta_score, group = 1)) +
    geom_line() +
    geom_point(size = 2) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Mean Delta Score by Fixed Income Group: Maut - Base",
      x = "Income group",
      y = "Mean delta score"
    )
  
  ggsave(file.path(out_dir, "income_mean_delta_score_maut_fixed_groups.png"), p5, width = 8, height = 6, dpi = 300)
  
  p6 <- ggplot(income_summary_parking_plot, aes(x = income_quintile_fixed, y = mean_delta_score, group = 1)) +
    geom_line() +
    geom_point(size = 2) +
    theme_minimal(base_size = 14) +
    labs(
      title = "Mean Delta Score by Fixed Income Group: Parking - Base",
      x = "Income group",
      y = "Mean delta score"
    )
  
  ggsave(file.path(out_dir, "income_mean_delta_score_parking_fixed_groups.png"), p6, width = 8, height = 6, dpi = 300)
}

cat("\n====================================================\n")
cat("DONE. All outputs saved to:\n")
cat(out_dir, "\n")
cat("====================================================\n")