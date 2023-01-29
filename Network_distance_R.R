library(RCurl)
library(sf)
library(igraph)
library(RCy3)
library(RColorBrewer)
library(tidyverse)
library(units)

network_data_1 <- getURL("https://raw.githubusercontent.com/RonaldVisser/ProvenanceNetworks/master/Network_analysis/data/means_network_1_simple.csv")
network_1 <- read.csv(text = network_data_1)
g_means_1 <- igraph::simplify(graph.data.frame(network_1, directed=FALSE))

download.file("https://raw.githubusercontent.com/RonaldVisser/ProvenanceNetworks/master/Network_analysis/export/network_1.gpkg", 
              destfile = "data/network_1.gpkg", method = "curl")
#network_1_lines = st_read("data/network_1.gpkg", layer="network_1_edges")
network_1_points = st_read("data/network_1.gpkg", layer="network_1_nodes")


network_1_distances <- network_1 %>%
  inner_join(network_1_points, by = c("Mean_A" = "node")) %>%
  inner_join(network_1_points, by = c("Mean_B" = "node")) %>%
  mutate(distance = set_units(st_distance(geom.x,geom.y, by_element = TRUE), km)) %>%
  mutate(names = paste0(Mean_A, " (interacts with) ", Mean_B)) %>%
  select(Mean_A, Mean_B, names, distance)

# histogram of first neighbours and distances
network_1_distances %>% ggplot(aes(x=distance)) + 
  geom_histogram(binwidth = 50) + geom_density(aes(y=50 * ..count..), colour = "blue") +
  xlab("distance") + ylab("count")
ggsave("export/means_network_1_firstneighbours_hist_density.png")

#average distances of first neigbours
node_avg_distance <- network_1_distances %>% 
  group_by(Mean_A) %>%
  mutate(dist_avg = mean(distance)) %>%
  select(Mean_A,dist_avg) %>%
  distinct()
write.csv(node_avg_distance,"export/node_avg_distance.csv")

# add data with imported material
transported <- read.csv("data/TransportedMaterial.csv")

transported %>% ggplot(aes(y=Transport)) + geom_bar()
ggsave("export/transported_bar.png")

# start Cytoscape (check Batch-file after update Cytoscape)
system("Cytoscape.bat")
source("CytoscapeStyles.R")

createNetworkFromIgraph(g_means_1, "network_means_1", collection = "network_means_1")
loadTableData(network_1_distances, data.key.column = "names", table = "edge")
copyVisualStyle("GreyNodesLabel", "lines_distance")
setVisualStyle("lines_distance")
max_d <- as.numeric(max(network_1_distances$distance))
#mean_d <- as.numeric(mean(network_1_distances$distance))
colour_mapped <- seq(0,max_d,75)
setEdgeColorMapping(table.column = "distance", 
                    colors= paletteColorBrewerYlGnBu,
                    table.column.values = colour_mapped,
                    mapping.type = "c",
                    style.name = "lines_distance")

loadTableData(transported, data.key.column = "Node", table = "node")
setNodeColorMapping(table.column = 'Transport', 
                    table.column.values = unique(transported$Transport),
                    colors = paletteColorBrewerSet3, 
                    mapping.type = "d", 
                    style.name = "lines_distance")

exportImage(paste0("export/Network_means_1_transported.svg"),type = "SVG")
exportImage(paste0("export/Network_means_1_transported.png"),type = "PNG")

#loadTableData(node_avg_distance, data.key.column = "Mean_A", table = "node")
#setNodeColorMapping(table.column = 'dist_avg', 
#                    colors = paletteColorBrewerPRGn, 
#                    mapping.type = "c", 
#                    style.name = "lines_distance")
