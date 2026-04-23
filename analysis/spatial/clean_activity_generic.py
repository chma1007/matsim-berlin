import csv
import sys

if len(sys.argv) != 2:
    print("Usage: python3 clean_activity_generic.py scenario")
    sys.exit()

scenario = sys.argv[1]

exclude = [
    "pt interaction",
    "car interaction",
    "bike interaction",
    "ride interaction",
    "transport",
    "outside"
]

input_file = f"{scenario}_activity_dataset.csv"
output_file = f"{scenario}_activity_cleaned.csv"

rows = []

with open(input_file) as f:
    reader = csv.DictReader(f)
    for row in reader:
        if row["act_group"] not in exclude:
            rows.append(row)

with open(output_file, "w", newline="") as out:
    fieldnames = list(rows[0].keys())
    writer = csv.DictWriter(out, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(rows)

print("DONE")
print("Remaining activities:", len(rows))
