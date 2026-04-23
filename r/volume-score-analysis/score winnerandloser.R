library(data.table)

# ==============================
# 1. File paths
# ==============================
file_base_persons    <- "D:/10pct/base-10pct-500/base-10pct-500.output_persons.csv.gz"
file_maut_persons    <- "D:/10pct/maut-10pct-500/maut-10pct-500.output_persons.csv.gz"
file_parking_persons <- "D:/10pct/parking-10pct-500/parking-10pct-500.output_persons.csv.gz"

out_dir <- "D:/10pct/top_winners_losers"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ==============================
# 2. Read persons
# ==============================
base    <- fread(file_base_persons)
maut    <- fread(file_maut_persons)
parking <- fread(file_parking_persons)

base[, person := as.character(person)]
maut[, person := as.character(person)]
parking[, person := as.character(person)]

# keep useful columns from base
base_keep <- c("person", "executed_score", "home_x", "home_y", "subpopulation", "income", "sex", "age")
base_keep <- base_keep[base_keep %in% names(base)]
base2 <- base[, ..base_keep]
setnames(base2, "executed_score", "score_base")

# ==============================
# 3. Maut ranking
# ==============================
maut2 <- maut[, .(person, score_maut = executed_score)]
dt_maut <- merge(base2, maut2, by = "person", all = FALSE)
dt_maut[, delta_score := score_maut - score_base]

setorder(dt_maut, -delta_score)

top5_winners_maut <- dt_maut[1:5]
top5_losers_maut  <- dt_maut[(.N-4):.N]

fwrite(dt_maut, file.path(out_dir, "score_ranking_maut.csv"))
fwrite(top5_winners_maut, file.path(out_dir, "top5_winners_maut.csv"))
fwrite(top5_losers_maut, file.path(out_dir, "top5_losers_maut.csv"))

fwrite(top5_winners_maut[, .(person)], file.path(out_dir, "top5_winner_ids_maut.csv"))
fwrite(top5_losers_maut[, .(person)], file.path(out_dir, "top5_loser_ids_maut.csv"))

# ==============================
# 4. Parking ranking
# ==============================
parking2 <- parking[, .(person, score_parking = executed_score)]
dt_parking <- merge(base2, parking2, by = "person", all = FALSE)
dt_parking[, delta_score := score_parking - score_base]

setorder(dt_parking, -delta_score)

top5_winners_parking <- dt_parking[1:5]
top5_losers_parking  <- dt_parking[(.N-4):.N]

fwrite(dt_parking, file.path(out_dir, "score_ranking_parking.csv"))
fwrite(top5_winners_parking, file.path(out_dir, "top5_winners_parking.csv"))
fwrite(top5_losers_parking, file.path(out_dir, "top5_losers_parking.csv"))

fwrite(top5_winners_parking[, .(person)], file.path(out_dir, "top5_winner_ids_parking.csv"))
fwrite(top5_losers_parking[, .(person)], file.path(out_dir, "top5_loser_ids_parking.csv"))

# ==============================
# 5. Print quick check
# ==============================
cat("\nTop 5 Winners - Maut\n")
print(top5_winners_maut[, .(person, score_base, score_maut, delta_score)])

cat("\nTop 5 Losers - Maut\n")
print(top5_losers_maut[, .(person, score_base, score_maut, delta_score)])

cat("\nTop 5 Winners - Parking\n")
print(top5_winners_parking[, .(person, score_base, score_parking, delta_score)])

cat("\nTop 5 Losers - Parking\n")
print(top5_losers_parking[, .(person, score_base, score_parking, delta_score)])

cat("\nOutputs saved to:\n", out_dir, "\n")