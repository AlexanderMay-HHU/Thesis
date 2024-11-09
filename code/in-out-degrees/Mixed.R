##Loading Libraries
library(circlize)
library(dplyr)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
plot_name_EukBac <- "Euk_Bac.png"
plot_name_BacEuk <- "Bac_Euk.png"
colorblind_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                        "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                        "#009e73","#f0e442","#0072b2","#d55e00",
                        "#999999")



edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)


# declutter name of edges into from and to ASVs
edges <- edges %>% mutate(from_ASV = substring(name,1,12), to_ASV = substring(name,31,42))

#Split edges into Eukaryote-Bacteria & Bacteria-Eukaryote only
euk_bac_edges <- edges %>% filter(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "EukBac" |
                                      paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "ArcBac")
bac_euk_edges <- edges %>% filter(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "BacEuk" |
                                      paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "BacArc")

#Only get interactions between different clusters
euk_bac_edges_from_to_different <- euk_bac_edges[euk_bac_edges$from_clu != euk_bac_edges$to_clu,]
bac_euk_edges_from_to_different <- bac_euk_edges[bac_euk_edges$from_clu != bac_euk_edges$to_clu,]


# Make adjacency matrices
adjacencyData_euk_bac <- with(euk_bac_edges_from_to_different, table(from_clu, to_clu))
adjacencyData_euk_bac

adjacencyData_bac_euk <- with(bac_euk_edges_from_to_different, table(from_clu, to_clu))
adjacencyData_bac_euk



## Generate Chord Plots
#Eukaryote-Bacteria
png(paste(plot_path,"/InOutDegrees/",plot_name_EukBac,sep=""), width=900, height=900, res=200)
chord_plot <- chordDiagram(adjacencyData_euk_bac,grid.col=colorblind_palette, transparency = 0.5)
circos.clear()
dev.off()


# Bacteria-Eukaryote
png(paste(plot_path,"/InOutDegrees/",plot_name_BacEuk,sep=""), width=900, height=900, res=200)
chord_plot <- chordDiagram(adjacencyData_bac_euk,grid.col=colorblind_palette, transparency = 0.5)
circos.clear()
dev.off()

