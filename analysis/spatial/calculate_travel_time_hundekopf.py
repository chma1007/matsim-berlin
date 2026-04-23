import re
import sys

if len(sys.argv) != 2:
    print("Usage: python3 calculate_travel_time_hundekopf.py scenario")
    sys.exit()

scenario = sys.argv[1]
input_file = f"{scenario}_travel_events.xml"

# 读取 Hundekopf links
hundekopf_links = set()
with open("hundekopf_links.csv") as f:
    next(f)
    for line in f:
        link_id, inside = line.strip().split(",")
        if inside == "True":
            hundekopf_links.add(link_id)

departures = {}
total_travel_time = 0
trip_count = 0

with open(input_file) as f:
    for line in f:
        time_match = re.search(r'time="([^"]+)"', line)
        type_match = re.search(r'type="([^"]+)"', line)
        person_match = re.search(r'person="([^"]+)"', line)
        link_match = re.search(r'link="([^"]+)"', line)

        if not (time_match and type_match and person_match and link_match):
            continue

        time = float(time_match.group(1))
        event_type = type_match.group(1)
        person = person_match.group(1)
        link = link_match.group(1)

        if event_type == "departure":
            departures[person] = (time, link)

        elif event_type == "arrival":
            if person in departures:
                dep_time, dep_link = departures[person]
                arr_link = link

                travel_time = time - dep_time

                # 判断是否与 Hundekopf 相关
                if (dep_link in hundekopf_links) or (arr_link in hundekopf_links):
                    if 0 < travel_time < 86400:
                        total_travel_time += travel_time
                        trip_count += 1

                del departures[person]

print(f"\n===== {scenario.upper()} HUNDEKOPF-RELATED TRAVEL =====")
print(f"Total travel hours: {total_travel_time/3600:.2f}")
print(f"Trip count: {trip_count}")
if trip_count > 0:
    print(f"Average travel minutes: {(total_travel_time/trip_count)/60:.2f}")
