import re
import csv
import sys

if len(sys.argv) != 2:
    print("Usage: python3 build_activity_dataset_generic.py scenario")
    sys.exit()

scenario = sys.argv[1]

input_file = f"{scenario}_private_act_events.xml"
output_file = f"{scenario}_activity_dataset.csv"

print("Reading Hundekopf links...")

hundekopf_links = set()
with open("hundekopf_links.csv") as f:
    next(f)
    for line in f:
        link_id, inside = line.strip().split(",")
        if inside == "True":
            hundekopf_links.add(link_id)

print(f"Processing {scenario} events...")

activities = []
current_act = {}

with open(input_file) as f:
    for line in f:
        time_match = re.search(r'time="([^"]+)"', line)
        type_match = re.search(r'type="([^"]+)"', line)
        person_match = re.search(r'person="([^"]+)"', line)
        link_match = re.search(r'link="([^"]+)"', line)
        act_match = re.search(r'actType="([^"]+)"', line)

        if not (time_match and type_match and person_match and link_match and act_match):
            continue

        time = float(time_match.group(1))
        event_type = type_match.group(1)
        person = person_match.group(1)
        link = link_match.group(1)
        act_type = act_match.group(1)

        key = person

        if event_type == "actstart":
            current_act[key] = {
                "start_time": time,
                "act_type": act_type,
                "link": link
            }

        elif event_type == "actend":
            if key in current_act:
                start_time = current_act[key]["start_time"]
                duration = time - start_time

                activities.append([
                    person,
                    act_type.split("_")[0],
                    start_time,
                    time,
                    duration,
                    link,
                    link in hundekopf_links
                ])

                del current_act[key]

print("Writing output...")

with open(output_file, "w", newline="") as out:
    writer = csv.writer(out)
    writer.writerow([
        "person",
        "act_group",
        "start_time",
        "end_time",
        "duration",
        "link",
        "in_hundekopf"
    ])
    writer.writerows(activities)

print("DONE")
print("Total activities:", len(activities))
