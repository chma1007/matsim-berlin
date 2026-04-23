import csv

sample = set()
with open("sample_residents.txt") as f:
    for line in f:
        sample.add(line.strip())

def get_work_duration(filename):
    work_time = {}
    with open(filename) as f:
        reader = csv.DictReader(f)
        for row in reader:
            person = row["person"]
            if person in sample and row["act_group"] == "work":
                duration = float(row["duration"])
                work_time[person] = work_time.get(person, 0) + duration
    return work_time

base_work = get_work_duration("base_activity_cleaned.csv")
maut_work = get_work_duration("maut_activity_cleaned.csv")

print("\nResident comparison (minutes):\n")

decrease_count = 0
increase_count = 0

for person in sample:
    b = base_work.get(person, 0) / 60
    m = maut_work.get(person, 0) / 60

    diff = m - b

    print(f"{person}: Base={b:.1f} | Maut={m:.1f} | Diff={diff:.1f}")

    if diff < 0:
        decrease_count += 1
    elif diff > 0:
        increase_count += 1

print("\nSummary:")
print("Decrease:", decrease_count)
print("Increase:", increase_count)
