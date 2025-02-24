##Loading Libraries
library("igraph")
library("dplyr")
library("ggplot2")

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
colorblind_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                        "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                        "#009e73","#f0e442","#0072b2","#d55e00",
                        "#999999")


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
betweenness_norm <- betweenness(edge_graph, directed = T, normalized = T)
closeness <- closeness(edge_graph, mode= "total", normalized = F)
closeness_norm <- closeness(edge_graph, mode= "total", normalized = T)


## Put centrality measures into dataframe
centrality_measures <- data.frame(matrix(nrow=length(betweenness)))
centrality_measures <- centrality_measures %>% select(-matrix.nrow...length.betweenness..)
centrality_measures <- centrality_measures %>% mutate(name=names(closeness))
centrality_measures <- centrality_measures %>% mutate(betweennessValue=unname(betweenness))
centrality_measures <- centrality_measures %>% mutate(closenessValue=unname(closeness))


centrality_measures_norm <- data.frame(matrix(nrow=length(betweenness_norm)))
centrality_measures_norm <- centrality_measures_norm %>% select(-matrix.nrow...length.betweenness_norm..)
centrality_measures_norm <- centrality_measures_norm %>% mutate(name=names(closeness_norm))
centrality_measures_norm <- centrality_measures_norm %>% mutate(betweennessValue=unname(betweenness_norm))
centrality_measures_norm <- centrality_measures_norm %>% mutate(closenessValue=unname(closeness_norm))

# Add louvain clusters to nodes
centrality_measures <- merge(centrality_measures, nodes, by = "name", all.x = TRUE)
centrality_measures <- centrality_measures[, c("name", "betweennessValue", "closenessValue", "LouvainLabelD")]
names(centrality_measures)[names(centrality_measures) == 'LouvainLabelD'] <- 'LouvainCluster'


centrality_measures_norm <- merge(centrality_measures_norm, nodes, by = "name", all.x = TRUE)
centrality_measures_norm <- centrality_measures_norm[, c("name", "betweennessValue", "closenessValue", "LouvainLabelD")]
names(centrality_measures_norm)[names(centrality_measures_norm) == 'LouvainLabelD'] <- 'LouvainCluster'

# export centrality measures
write.csv(centrality_measures, "generated/05_Network_Additions/Centrality_Measures.csv")
write.csv(centrality_measures_norm, "generated/05_Network_Additions/Centrality_Measures_norm.csv")





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




## Plots
# Betweenness Violin
gg_betweenness <- ggplot(centrality_measures,
                         aes(x=as.factor(LouvainCluster),
                             y=betweennessValue,
                             fill=as.factor(LouvainCluster)))+
                    geom_violin()+
  labs(x="Louvain Cluster", y= "Betweenness")

gg_betweenness


gg_betweenness_norm <- ggplot(centrality_measures_norm,
                         aes(x=as.factor(LouvainCluster),
                             y=betweennessValue,
                             fill=as.factor(LouvainCluster)))+
  geom_violin()+
  labs(x="Louvain Cluster", y= "Normalized Betweenness")

gg_betweenness_norm



# Closeness Violin
gg_closeness <- ggplot(centrality_measures,
                         aes(x=as.factor(LouvainCluster),
                             y=closenessValue,
                             fill=as.factor(LouvainCluster)))+
  geom_violin()+
  labs(x="Louvain Cluster", y= "Closeness")

gg_closeness


gg_closeness_norm <- ggplot(centrality_measures_norm,
                              aes(x=as.factor(LouvainCluster),
                                  y=closenessValue,
                                  fill=as.factor(LouvainCluster)))+
  geom_violin()+
  labs(x="Louvain Cluster", y= "Normalized Closeness")

gg_closeness_norm