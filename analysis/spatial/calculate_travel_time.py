import re
import sys

if len(sys.argv) != 2:
    print("Usage: python3 calculate_travel_time.py scenario")
    sys.exit()

scenario = sys.argv[1]
input_file = f"{scenario}_travel_events.xml"

departures = {}
total_travel_time = 0
trip_count = 0

with open(input_file) as f:
    for line in f:
        time_match = re.search(r'time="([^"]+)"', line)
        type_match = re.search(r'type="([^"]+)"', line)
        person_match = re.search(r'person="([^"]+)"', line)

        if not (time_match and type_match and person_match):
            continue

        time = float(time_match.group(1))
        event_type = type_match.group(1)
        person = person_match.group(1)

        if event_type == "departure":
            departures[person] = time

        elif event_type == "arrival":
            if person in departures:
                travel_time = time - departures[person]
                if 0 < travel_time < 86400:
                    total_travel_time += travel_time
                    trip_count += 1
                del departures[person]

print(f"\n===== {scenario.upper()} TRAVEL TIME =====")
print(f"Total travel hours: {total_travel_time/3600:.2f}")
print(f"Trip count: {trip_count}")
print(f"Average travel minutes: {(total_travel_time/trip_count)/60:.2f}")
