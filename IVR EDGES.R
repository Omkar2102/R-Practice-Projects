library(mongolite)
library(config)
library(dplyr)
library(tidyverse)
connection <- mongo(collection = "ivr_traversal_edges",
                    db = "care",
                    url = "mongodb://10.0.0.56:27017")
data_tree <- connection$find('{}', fields = '{}')

#separate out source and target coloumns
sources <- data_tree %>%
  distinct(source) %>%
  rename(label = source)

target <- data_tree %>%
  distinct(target) %>%
  rename(label = target)
#combine both source and target by nodes 
nodes <- full_join(sources , target , by= "label")
nodes 
nodes <- nodes %>% rowid_to_column("id")
nodes
per_route <- data_tree %>%
  group_by(source, target) %>%
  summarise(weight = n()) %>%
  ungroup()
per_route
edges <- per_route %>% 
  left_join(nodes, by = c("source" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("target" = "label")) %>% 
  rename(to = id)
edges <- select( edges, from, to, weight)
edges
library(network)
routes_network <- network(edges,vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE)
class(routes_network)
routes_network
summary(routes_network)
plot(routes_network,vertex.cex = 3 , mode ="circle")
library(igraph)
routes_igraph <- graph_from_data_frame(d= edges, vertices = nodes, directed = TRUE)
routes_igraph
plot(routes_igraph, edge.arrow.size = 0.2)
plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)
library(tidygraph)
library(ggraph)
library(visNetwork)
library(networkD3)
visNetwork(nodes, edges)


