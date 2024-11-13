##Loading Libraries
library("igraph")
library("dplyr")

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")


edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)


## Declutter name of edges into from and to ASVs
edge_list <- edges %>% select(name) %>%
  mutate(from_ASV = substring(name,1,12), to_ASV = substring(name,31,42)) %>%
  select(from_ASV, to_ASV) %>% as.matrix()


## Make igraph object from edgelist
edge_graph <- graph_from_edgelist(edge_list,directed=T)

## Calculate betweenness & closeness centrality
betweenness <- betweenness(edge_graph, directed = T, normalized = T)
closeness <- closeness(edge_graph, mode= "total", normalized = T)


## Put centrality measures into dataframe
centrality_measures <- matrix(data= c(names(edge_graph[1]), betweenness,closeness)
                              ,nrow=length(betweenness)) %>% as.data.frame()
colnames(centrality_measures) <- c("names", "betweenness", "closeness")
# export centrality measures
write.csv(centrality_measures, "generated/05_Network_Additions/Centrality_Measures.csv")



## Overview
# Betweenness
summary(betweenness)
betweenness[betweenness==min(betweenness)]
betweenness[betweenness==max(betweenness)]
sort(betweenness)

# Closeness
summary(closeness)
closeness[closeness==min(closeness)]
closeness[closeness==max(closeness)]
sort(closeness)