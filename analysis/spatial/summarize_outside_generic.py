import csv
import sys
from collections import defaultdict
import statistics

if len(sys.argv) != 2:
    print("Usage: python3 summarize_outside_generic.py scenario")
    sys.exit()

scenario = sys.argv[1]
input_file = f"{scenario}_activity_cleaned.csv"

durations = defaultdict(list)

with open(input_file) as f:
    reader = csv.DictReader(f)
    for row in reader:
        if row["in_hundekopf"] == "False":
            duration = float(row["duration"])
            act_group = row["act_group"]

            if duration > 0 and duration < 86400:
                durations[act_group].append(duration)

print(f"\n===== {scenario.upper()} — OUTSIDE HUNDEKOPF =====\n")

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
