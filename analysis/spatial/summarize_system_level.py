import csv
from collections import defaultdict
import statistics

print("Summarizing system level...")

durations = defaultdict(list)

with open("base_activity_cleaned.csv") as f:
    reader = csv.DictReader(f)
    for row in reader:
        duration = float(row["duration"])
        act_group = row["act_group"]

        if duration > 0 and duration < 86400:
            durations[act_group].append(duration)

print("Results:\n")

for act in sorted(durations.keys()):
    total = sum(durations[act])
    mean = statistics.mean(durations[act])
    median = statistics.median(durations[act])
    n = len(durations[act])

    print(f"{act}")
    print(f"  N = {n}")
    print(f"  Total hours = {total/3600:.2f}")
    print(f"  Mean minutes = {mean/60:.2f}")
    print(f"  Median minutes = {median/60:.2f}")
    print()
