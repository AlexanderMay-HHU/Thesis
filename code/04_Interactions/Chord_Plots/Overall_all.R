##Loading Libraries
library(circlize)


#Set Working Directory
project_folder <- dirname(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/original/Convergent_Cross_Mapping"))


color_clusters <- c("#00ffff","#191970","#c71585","#2f4f4f",
                    "#ff0000","#0000ff","#bdb76b","#006400",
                    "#ffa500","#1e90ff","#ffb6c1","#00fa9a",
                    "#ffff00","#631919","#ff00ff","#00ff00")

ccm_colors <- color_clusters[c(1,3:14)]



edges <- read.csv("CCM_edge_table.csv", header=TRUE)

#Only get interactions between different clusters
edges_from_to_different <- edges[edges$from_clu != edges$to_clu,]


# Make adjacency matrices
adjacencyData <- with(edges, table(from_clu, to_clu))
adjacencyData_diff <- with(edges_from_to_different, table(from_clu, to_clu))



### Generate Chord Plot
## Only Name and size
# All connections
png(paste(plot_path,"04_Interactions/ChordPlots/Overall_all.png",sep=""), width=900, height=900, res=200)
  chord_plot <- chordDiagram(adjacencyData,grid.col=ccm_colors, transparency = 0.5,annotationTrack = c("name", "grid"))
  circos.clear()
dev.off()

# Only connections to different clusters
png(paste(plot_path,"04_Interactions/ChordPlots/Overall_other.png",sep=""), width=900, height=900, res=200)
chord_plot <- chordDiagram(adjacencyData_diff,grid.col=ccm_colors, transparency = 0.5,annotationTrack = c("name", "grid"))
circos.clear()
dev.off()


## With all options (Name, Size, Numbers)
# All connections
png(paste(plot_path,"00_Appendix/04_Interactions/ChordPlots/Overall_all_with_numbers.png",sep=""), width=900, height=900, res=200)
chord_plot <- chordDiagram(adjacencyData,grid.col=ccm_colors, transparency = 0.5)
circos.clear()
dev.off()

# Only connections to different clusters
png(paste(plot_path,"00_Appendix/04_Interactions/ChordPlots/Overall_other_with_numbers.png",sep=""), width=900, height=900, res=200)
chord_plot <- chordDiagram(adjacencyData_diff,grid.col=ccm_colors, transparency = 0.5)
circos.clear()
dev.off()