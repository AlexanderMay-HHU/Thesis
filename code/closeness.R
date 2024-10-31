##Loading Libraries
library("igraph")


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
plot_name <- "betweenness_centrality.png"



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)


#set edgelist size from data
edge_list <- matrix(nrow=length(edges[,1]),ncol=2)
colnames(edge_list) <- c("From","To")

#Get Interacting ASVs
for (interaction in 1:(length(edges$name))){
  #print(edges$name[interaction])
  edge_list[interaction,1] <- substring(edges$name[interaction],1,12)
  edge_list[interaction,2] <- substring(edges$name[interaction],31,42)
}

#Make igraph object from edgelist
edge_graph <- graph_from_edgelist(edge_list,directed=T)

#calculate closeness centrality
closeness <- closeness(edge_graph, mode= "all")


#Overview
summary(closeness)

closeness[closeness==min(closeness)]
closeness[closeness==max(closeness)]

sort(closeness)