import csv

exclude = [
    "pt interaction",
    "car interaction",
    "bike interaction",
    "ride interaction",
    "transport",
    "outside"
]

print("Cleaning dataset...")

rows = []

with open("base_activity_grouped.csv") as f:
    reader = csv.DictReader(f)
    for row in reader:
        if row["act_group"] not in exclude:
            rows.append(row)

print("Writing cleaned dataset...")

with open("base_activity_cleaned.csv", "w", newline="") as out:
    fieldnames = list(rows[0].keys())
    writer = csv.DictWriter(out, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(rows)

print("DONE")
print("Remaining activities:", len(rows))
