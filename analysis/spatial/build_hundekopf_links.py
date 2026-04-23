import re
import csv
import shapefile

print("Reading polygon...")

sf = shapefile.Reader("/net/work/mao/berlin/input/v6.4/berlin hundekopf/berlin_hundekopf_ONLY_25832.shp")
shape = sf.shapes()[0]
polygon = shape.points

def point_in_poly(x, y, poly):
    inside = False
    n = len(poly)
    px, py = zip(*poly)

    j = n - 1
    for i in range(n):
        if ((py[i] > y) != (py[j] > y)) and \
           (x < (px[j] - px[i]) * (y - py[i]) / (py[j] - py[i]) + px[i]):
            inside = not inside
        j = i
    return inside

print("Reading nodes...")

nodes = {}
with open("network_nodes.xml") as f:
    for line in f:
        node_id = re.search(r'id="([^"]+)"', line)
        x = re.search(r'x="([^"]+)"', line)
        y = re.search(r'y="([^"]+)"', line)
        if node_id and x and y:
            nodes[node_id.group(1)] = (
                float(x.group(1)),
                float(y.group(1))
            )

print("Processing links...")

total = 0
inside_count = 0

with open("hundekopf_links.csv", "w", newline="") as out:
    writer = csv.writer(out)
    writer.writerow(["link_id", "in_hundekopf"])

    with open("network_links.xml") as f:
        for line in f:
            link_id = re.search(r'id="([^"]+)"', line)
            from_node = re.search(r'from="([^"]+)"', line)

            if link_id and from_node:
                total += 1
                node = nodes.get(from_node.group(1))

                if node:
                    inside = point_in_poly(node[0], node[1], polygon)
                    if inside:
                        inside_count += 1
                else:
                    inside = False

                writer.writerow([link_id.group(1), inside])

print("DONE")
print("Total links:", total)
print("Inside Hundekopf:", inside_count)
