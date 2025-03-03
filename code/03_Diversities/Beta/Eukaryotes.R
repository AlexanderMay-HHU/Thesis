## Loading Libraries
library(dplyr)
library(vegan)    #Used for Bray-Curtis Distance
library(ggplot2)

#Set Working Directory
project_folder <- dirname(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data"))


colorblind_gradient_palette <- c("#3300AA","#FFFFBF","#AA0033")

  

## Get Data
nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_eukaryotes <- subset(nodes,nodes$Kingdom == "Eukaryota")


## Make Diversity dataframe
abundance_of_phylum_in_cluster <- data.frame(matrix(ncol=length(unique(nodes_eukaryotes$Phylum))
                                                    ,nrow=length(unique(nodes_eukaryotes$LouvainLabelD))))
colnames(abundance_of_phylum_in_cluster) <- unique(nodes_eukaryotes$Phylum)
rownames(abundance_of_phylum_in_cluster) <- sort(unique(nodes_eukaryotes$LouvainLabelD))


#Calculate the Abundance of Phylums per Cluster
for (cluster in sort(unique(nodes_eukaryotes$LouvainLabelD))){
  for (phylum in unique(nodes_eukaryotes$Phylum)){
    #Abundance of Phylum (skip NA phylum)
    if(!is.na(phylum)){
      abundance_of_phylum_in_cluster[ifelse(cluster==0,1,cluster),phylum] <- sum(nodes_eukaryotes[nodes_eukaryotes$LouvainLabelD == cluster &
                                                                            nodes_eukaryotes$Phylum == phylum,1],na.rm = TRUE)
      cat("Done with calculating the sum of Phylum (", phylum,") from Cluster", cluster, "\n",sep="")
    }else{
      next
    }
  }
}



# Calculate Bray-Curtis dissimilarity
bray_curtis_dist <- vegdist(abundance_of_phylum_in_cluster, method = "bray",na.rm = TRUE) %>% as.matrix() %>% as.table()
# Remove lower triangle of matrix (duplicate values)
bray_lower <- bray_curtis_dist
bray_lower[lower.tri(bray_lower)] <- NA
bray_lower <- bray_lower %>% as.data.frame()
#Make full table in long format
bray_curtis_dist <- bray_curtis_dist %>% as.data.frame()



## Generate Heatmaps
# Full
heatmap_full <- ggplot(bray_curtis_dist, aes(x = Var2,
                                             y = Var1,
                                             fill = Freq,
                                             label = round(Freq,2))) +
  geom_tile() + # Create heatmap
  geom_tile(fill=NA, color="#000000",lwd=0.4,linetype=1)+ #Draw border  around data fields
  coord_fixed()+ # make it square
  geom_text(color = ifelse(bray_curtis_dist$Freq<=0.2 | bray_curtis_dist$Freq>=0.75 ,"white","black"), size = 4) + # Add text labels
  guides(fill = guide_colorbar(title="", barwidth=1.5 , barheight=35))+ #Adjust Legend Height & Width
  scale_fill_gradientn(colours = colorblind_gradient_palette,breaks=c(seq(0,1,0.1)))+ # Set color gradient
  labs(x = "Cluster", y = "Cluster", title = "Eukaryotes") + # Labels
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        panel.background = element_blank())# Rotate x-axis labels and remove plot background


# Lower
heatmap_lower <- ggplot(bray_lower, aes(x = Var2,
                                        y = Var1,
                                        fill = Freq,
                                        label = round(Freq,2))) +
  geom_tile(data = bray_lower[which(complete.cases(bray_lower)),]) + # Create heatmap
  geom_tile(data = bray_lower[which(complete.cases(bray_lower)),],
            fill=NA, color="#000000",lwd=0.4,linetype=1)+ #Draw border only around data fields
  coord_fixed()+ # make it square
  geom_text(color = ifelse(bray_lower$Freq<=0.2 | bray_lower$Freq>=0.75 ,"white","black"), size = 4) + # Add text labels
  guides(fill = guide_colorbar(title="", barwidth=1.5 , barheight=35))+ #Adjust Legend Height & Width
  scale_fill_gradientn(colours = colorblind_gradient_palette,breaks=c(seq(0,1,0.1)))+ # Set color gradient
  labs(x = "Cluster", y = "Cluster", title = "Eukaryotes") + # Labels
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        panel.background = element_blank())# Rotate x-axis labels and remove plot background


# Show it
heatmap_full
heatmap_lower

# Save it to plot_path
ggsave(filename="Bray_Curtis_Full.png", plot=heatmap_full, path=paste0(plot_path,"03_Diversities/Beta/Eukaryotes/"))
ggsave(filename="Bray_Curtis_Lower.png", plot=heatmap_lower, path=paste0(plot_path,"00_Appendix/03_Diversities/Beta/Eukaryotes/"))