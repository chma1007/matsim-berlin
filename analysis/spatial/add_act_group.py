import csv

print("Reading dataset...")

rows = []

with open("base_activity_dataset.csv") as f:
    reader = csv.DictReader(f)
    for row in reader:
        act_type = row["act_type"]
        act_group = act_type.split("_")[0]

        row["act_group"] = act_group
        rows.append(row)

print("Writing new dataset...")

with open("base_activity_grouped.csv", "w", newline="") as out:
    fieldnames = list(rows[0].keys())
    writer = csv.DictWriter(out, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(rows)

print("DONE")
