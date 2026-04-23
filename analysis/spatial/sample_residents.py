import random

residents = []

with open("base_residents.txt") as f:
    for line in f:
        residents.append(line.strip())

sample_size = 50
sample = random.sample(residents, sample_size)

with open("sample_residents.txt", "w") as out:
    for p in sample:
        out.write(p + "\n")

print("DONE")
print("Sample size:", sample_size)
