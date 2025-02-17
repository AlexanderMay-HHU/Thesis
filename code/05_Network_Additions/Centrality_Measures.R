##Loading Libraries
library("igraph")
library("dplyr")
library("ggplot2")

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")

nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)


## Declutter name of edges into from and to ASVs
edge_list <- edges %>% select(name) %>%
  mutate(from_ASV = substring(name,1,12), to_ASV = substring(name,31,42)) %>%
  select(from_ASV, to_ASV) %>% as.matrix()


## Make igraph object from edgelist
edge_graph <- graph_from_edgelist(edge_list,directed=T)

## Calculate betweenness & closeness centrality
betweenness <- betweenness(edge_graph, directed = T, normalized = F)
closeness <- closeness(edge_graph, mode= "total", normalized = F)


## Put centrality measures into dataframe
centrality_measures <- matrix(data= c(names(edge_graph[1]), betweenness,closeness)
                              ,nrow=length(betweenness)) %>% as.data.frame()
colnames(centrality_measures) <- c("name", "betweenness", "closeness")

# Add louvain clusters to nodes
centrality_measures <- merge(centrality_measures, nodes, by = "name", all.x = TRUE)
centrality_measures <- centrality_measures[, c("name", "betweenness", "closeness", "LouvainLabelD")]

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


data <- data.frame(
  name=c( rep("A",1000), rep("B",1000), rep("B",1000), rep("C",2000), rep('D', 10000)  ),
  value=c( rnorm(1000, 10, 5), rnorm(1000, 13, 1), rnorm(1000, 18, 1), rnorm(2000, 25, 4), rnorm(10000, 12, 1) ) %>% round(2)
)
ggplot(data, aes(x=name, y=value, fill=name)) + 
  geom_violin()


centrality_measures


class(centrality_measures$LouvainLabelD)

## Plots

gg_betweenness <- ggplot(centrality_measures,
                         aes(x=factor(LouvainLabelD),
                             y=betweenness,
                             fill=factor(LouvainLabelD)))+
                    geom_violin()

gg_betweenness