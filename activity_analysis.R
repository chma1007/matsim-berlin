library(data.table)
setDTthreads(4)

process_file <- function(path, scenario_name) {
  
  dt <- fread(
    cmd = paste("cat", path),
    sep = " ",
    header = FALSE,
    showProgress = TRUE
  )
  
  dt[, raw := V1]
  
  # Extract fields
  dt[, time := as.numeric(sub('.*time="([^"]*)".*', '\\1', raw))]
  dt[, type := sub('.*type="([^"]*)".*', '\\1', raw)]
  dt[, person := sub('.*person="([^"]*)".*', '\\1', raw)]
  dt[, actType := sub('.*actType="([^"]*)".*', '\\1', raw)]
  
  # Remove freight
  dt <- dt[!grepl("^freight_", person)]
  
  dt[, scenario := scenario_name]
  
  dt[, .(time, type, person, actType, scenario)]
}

base  <- process_file("base_acts.xml", "base")
maut  <- process_file("maut_acts.xml", "maut")
park  <- process_file("park_acts.xml", "parking")

all <- rbindlist(list(base, maut, park), use.names = TRUE)

# Sort for pairing
setorder(all, scenario, person, time)

# Compute duration by pairing actstart and actend
all[, duration := {
  if (.N < 2) return(rep(NA_real_, .N))
  c(diff(time), NA)
}, by = .(scenario, person)]

# Keep only actstart rows (duration belongs to activity starting there)
all <- all[type == "actstart"]

# Remove invalid durations
all <- all[duration > 0 & duration < 24*3600]

# Extract main activity group
all[, act_group := sub("_.*", "", actType)]

# Aggregate
summary_long <- all[, .(
  mean_hours = mean(duration)/3600,
  total_hours = sum(duration)/3600,
  N = .N
), by = .(scenario, act_group)]

summary_wide <- dcast(summary_long,
                      act_group ~ scenario,
                      value.var = "mean_hours")

summary_wide[, delta_maut := maut - base]
summary_wide[, delta_park := parking - base]

fwrite(summary_long, "activity_summary_long.csv")
fwrite(summary_wide, "activity_summary_wide.csv")

print(summary_wide)
