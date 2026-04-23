import re
import csv

print("Reading Hundekopf links...")

hundekopf_links = set()
with open("hundekopf_links.csv") as f:
    next(f)
    for line in f:
        link_id, inside = line.strip().split(",")
        if inside == "True":
            hundekopf_links.add(link_id)

print("Processing events...")

activities = []

current_act = {}

with open("base_private_act_events.xml") as f:
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
                    current_act[key]["act_type"],
                    start_time,
                    time,
                    duration,
                    current_act[key]["link"],
                    current_act[key]["link"] in hundekopf_links
                ])

                del current_act[key]

print("Writing output...")

with open("base_activity_dataset.csv", "w", newline="") as out:
    writer = csv.writer(out)
    writer.writerow([
        "person",
        "act_type",
        "start_time",
        "end_time",
        "duration",
        "link",
        "in_hundekopf"
    ])
    writer.writerows(activities)

print("DONE")
print("Total activities:", len(activities))
