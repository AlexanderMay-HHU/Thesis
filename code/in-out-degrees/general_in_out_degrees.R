##Loading Libraries
library(circlize)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
plot_name <- "General_in_out_degrees.png"



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)


edges_from_to_different <- edges[edges$from_clu != edges$to_clu,]

#Calc number of In&Out Degrees
out_deg <- c()
in_deg <- c()
for (clu in 0:(max(edges_from_to_different$from_clu))){
  if(clu != 1){
    out_deg <- c(out_deg, length(edges_from_to_different[edges_from_to_different$from_clu == clu,9]))
    in_deg <- c(in_deg, length(edges_from_to_different[edges_from_to_different$to_clu == clu,2]))
    #cat(in_deg[if(clu!=0){clu}else{1}], " -> Cluster", clu, " -> ",
    #    out_deg[if(clu!=0){clu}else{1}], "\n",sep="")
  }else{
    next
  }
}

adjacencyData <- with(edges_from_to_different, table(from_clu, to_clu))


#Get Cluster Colors
cluster_colors <- c()
for (clu_col in c(0,2:max(nodes$LouvainLabelD))){
  cluster_colors <- c(cluster_colors, nodes$ColorLabel[min(which(nodes$LouvainLabelD == clu_col))])
  cat("Cluster ", clu_col, ": ", cluster_colors[if(clu_col!=0){clu_col}else{1}],"\n",sep="")
}
names(cluster_colors) <- as.character(sort(unique(nodes$LouvainLabelD)))
cluster_colors



## Generate Chord Plot
png(paste(plot_path,"/InOutDegrees/",plot_name,sep=""),width=900,height=900)
  chord_plot <- chordDiagram(adjacencyData,grid.col=cluster_colors, transparency = 0.5)
dev.off()