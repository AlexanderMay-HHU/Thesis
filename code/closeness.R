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



close_graph <- graph_from_data_frame(edges, directed = TRUE)

closeness <- closeness(close_graph, vids=V(close_graph), mode= "all")

summary(closeness)