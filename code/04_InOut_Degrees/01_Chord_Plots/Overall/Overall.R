##Loading Libraries
library(circlize)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/04_InOut_Degrees/01_Chord_Plots/Overall/"
plot_name <- "Overall.png"
colorblind_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                        "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                        "#009e73","#f0e442","#0072b2","#d55e00",
                        "#999999")



edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)


#Only get interactions between different clusters
edges_from_to_different <- edges[edges$from_clu != edges$to_clu,]


# Make adjacency matrix
adjacencyData <- with(edges_from_to_different, table(from_clu, to_clu))



## Generate Chord Plot
png(paste(plot_path,plot_name,sep=""), width=900, height=900, res=200)
  chord_plot <- chordDiagram(adjacencyData,grid.col=colorblind_palette, transparency = 0.5)
  circos.clear()
dev.off()