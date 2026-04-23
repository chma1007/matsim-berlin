library(data.table)

# ==============================
# 1. File paths
# ==============================
file_base_persons    <- "D:/10pct/base-10pct-500/base-10pct-500.output_persons.csv.gz"
file_maut_persons    <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_persons.csv.gz"
file_parking_persons <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_persons.csv.gz"

out_dir <- "D:/10pct/subpopulation_analysis"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==============================
# 2. Read data
# ==============================
base    <- fread(file_base_persons)
maut    <- fread(file_maut_persons)
parking <- fread(file_parking_persons)

base[, person := as.character(person)]
maut[, person := as.character(person)]
parking[, person := as.character(person)]

if (!("subpopulation" %in% names(base))) {
  stop("subpopulation column not found in base persons file.")
}

base2 <- base[, .(person, subpopulation, score_base = executed_score)]
maut2 <- maut[, .(person, score_maut = executed_score)]
parking2 <- parking[, .(person, score_parking = executed_score)]

# ==============================
# 3. Maut
# ==============================
dt_maut <- merge(base2, maut2, by = "person", all = FALSE)
dt_maut[, delta_score := score_maut - score_base]

summary_maut <- dt_maut[, .(
  n_agents = .N,
  mean_score_base = mean(score_base, na.rm = TRUE),
  mean_score_policy = mean(score_maut, na.rm = TRUE),
  mean_delta_score = mean(delta_score, na.rm = TRUE),
  median_delta_score = median(delta_score, na.rm = TRUE),
  sd_delta_score = sd(delta_score, na.rm = TRUE),
  share_score_up = mean(delta_score > 0, na.rm = TRUE),
  share_score_down = mean(delta_score < 0, na.rm = TRUE)
), by = subpopulation][order(subpopulation)]

fwrite(summary_maut, file.path(out_dir, "subpopulation_summary_maut.csv"))

# optional: personal / commercial only
personal_maut <- dt_maut[subpopulation %in% c("person", "personal", "default")]
commercial_maut <- dt_maut[grepl("commercial|freight|goods|service", subpopulation, ignore.case = TRUE)]

fwrite(personal_maut, file.path(out_dir, "personal_only_maut.csv"))
fwrite(commercial_maut, file.path(out_dir, "commercial_only_maut.csv"))

# ==============================
# 4. Parking
# ==============================
dt_parking <- merge(base2, parking2, by = "person", all = FALSE)
dt_parking[, delta_score := score_parking - score_base]

summary_parking <- dt_parking[, .(
  n_agents = .N,
  mean_score_base = mean(score_base, na.rm = TRUE),
  mean_score_policy = mean(score_parking, na.rm = TRUE),
  mean_delta_score = mean(delta_score, na.rm = TRUE),
  median_delta_score = median(delta_score, na.rm = TRUE),
  sd_delta_score = sd(delta_score, na.rm = TRUE),
  share_score_up = mean(delta_score > 0, na.rm = TRUE),
  share_score_down = mean(delta_score < 0, na.rm = TRUE)
), by = subpopulation][order(subpopulation)]

fwrite(summary_parking, file.path(out_dir, "subpopulation_summary_parking.csv"))

personal_parking <- dt_parking[subpopulation %in% c("person", "personal", "default")]
commercial_parking <- dt_parking[grepl("commercial|freight|goods|service", subpopulation, ignore.case = TRUE)]

fwrite(personal_parking, file.path(out_dir, "personal_only_parking.csv"))
fwrite(commercial_parking, file.path(out_dir, "commercial_only_parking.csv"))

# ==============================
# 5. Print
# ==============================
cat("\nMaut subpopulation summary:\n")
print(summary_maut)

cat("\nParking subpopulation summary:\n")
print(summary_parking)

cat("\nOutputs saved to:\n", out_dir, "\n")