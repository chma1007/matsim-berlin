library(data.table)
library(stringr)
library(sf)

# 1. Read nodes
nodes_raw <- fread("network_nodes.xml", header = FALSE)

nodes <- nodes_raw[, .(
  node_id = str_extract(V1, 'id="[^"]+"') |> 
            str_remove_all('id="|"'),
  x = as.numeric(str_extract(V1, 'x="[^"]+"') |> 
                 str_remove_all('x="|"')),
  y = as.numeric(str_extract(V1, 'y="[^"]+"') |> 
                 str_remove_all('y="|"'))
)]

# 2. Read links
links_raw <- fread("network_links.xml", header = FALSE)

links <- links_raw[, .(
  link_id = str_extract(V1, 'id="[^"]+"') |> 
            str_remove_all('id="|"'),
  from_node = str_extract(V1, 'from="[^"]+"') |> 
              str_remove_all('from="|"')
)]

# 3. Merge
links_nodes <- merge(
  links,
  nodes,
  by.x = "from_node",
  by.y = "node_id",
  all.x = TRUE
)

# 4. Convert to sf
links_sf <- st_as_sf(
  links_nodes,
  coords = c("x", "y"),
  crs = 25832
)

# 5. Read Hundekopf shapefile
hundekopf <- st_read(
  "/net/work/mao/berlin/input/v6.4/berlin hundekopf/berlin_hundekopf_ONLY_25832.shp",
  quiet = TRUE
)

# 6. Spatial join
inside <- st_within(links_sf, hundekopf, sparse = FALSE)
links_nodes$in_hundekopf <- inside[,1]

# 7. Write output
fwrite(
  links_nodes[, .(link_id, in_hundekopf)],
  "hundekopf_links.csv"
)

cat("DONE\n")
cat("Total links:", nrow(links_nodes), "\n")
cat("Inside Hundekopf:", sum(links_nodes$in_hundekopf), "\n")
