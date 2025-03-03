##Loading Libraries
library(circlize)
library(dplyr)


#Set Working Directory
project_folder <- dirname(dirname(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))))
plot_path <- paste0(project_folder,"/generated_plots/04_NMI/01_Chord_Plots/Single/")
plot_name <- "Prokaryote-Prokaryote.png"
setwd(paste0(project_folder,"/data"))


colorblind_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                        "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                        "#009e73","#f0e442","#0072b2","#d55e00",
                        "#999999")



edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)


# declutter name of edges into from and to ASVs
edges <- edges %>% mutate(from_ASV = substring(name,1,12), to_ASV = substring(name,31,42))

#Split edges into Prokaryotes only
prokaryote_edges <- edges %>% filter(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "BacBac"|
                                     paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "BacArc"|
                                     paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "ArcArc"|
                                     paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "ArcBac")

#Only get interactions between different clusters
prokaryote_edges_from_to_different <- prokaryote_edges[prokaryote_edges$from_clu != prokaryote_edges$to_clu,]



# Make adjacency matrix
adjacencyData <- with(prokaryote_edges_from_to_different, table(from_clu, to_clu))
adjacencyData



## Generate Chord Plot
png(paste(plot_path,plot_name,sep=""), width=900, height=900, res=200)
chord_plot <- chordDiagram(adjacencyData,grid.col=colorblind_palette, transparency = 0.5)
circos.clear()
dev.off()