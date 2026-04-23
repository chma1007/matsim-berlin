import csv
import sys

if len(sys.argv) != 2:
    print("Usage: python3 identify_residents.py scenario")
    sys.exit()

scenario = sys.argv[1]
input_file = f"{scenario}_activity_cleaned.csv"
output_file = f"{scenario}_residents.txt"

residents = set()

with open(input_file) as f:
    reader = csv.DictReader(f)
    for row in reader:
        if row["act_group"] == "home" and row["in_hundekopf"] == "True":
            residents.add(row["person"])

with open(output_file, "w") as out:
    for r in residents:
        out.write(r + "\n")

print("DONE")
print("Residents count:", len(residents))
