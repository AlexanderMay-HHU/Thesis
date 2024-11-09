##Loading Libraries
library(circlize)
library(dplyr)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
plot_name <- "Euk_Euk.png"
colorblind_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                        "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                        "#009e73","#f0e442","#0072b2","#d55e00",
                        "#999999")



edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)


# declutter name of edges into from and to ASVs
edges <- edges %>% mutate(from_ASV = substring(name,1,12), to_ASV = substring(name,31,42))

#Split edges into Bacteria only
euk_edges <- edges %>% filter(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "EukEuk" |
                                paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "ArcArc")

#Only get interactions between different clusters
euk_edges_from_to_different <- euk_edges[euk_edges$from_clu != euk_edges$to_clu,]



# Make adjacency matrix
adjacencyData <- with(euk_edges_from_to_different, table(from_clu, to_clu))
adjacencyData



## Generate Chord Plot
png(paste(plot_path,"/InOutDegrees/",plot_name,sep=""), width=900, height=900, res=200)
chord_plot <- chordDiagram(adjacencyData,grid.col=colorblind_palette, transparency = 0.5)
circos.clear()
dev.off()