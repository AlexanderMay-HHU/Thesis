## Loading Libraries
library(dplyr)
library(vegan)    #Used for Bray-Curtis Distance
library(ggplot2)

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/Diversities/Eukaryota/Beta_diversity"
plot_name <- "Bray_Curtis.png"
colorblind_gradient_palette <- hcl.colors(palette = "Blue-Red",n=100)



## Get Data
nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_euk <- subset(nodes,nodes$Kingdom == "Eukaryota" | nodes$Kingdom == "Archaea")


## Make Diversity dataframe
abundance_of_phylum_in_cluster <- data.frame(matrix(ncol=length(unique(nodes_euk$Phylum)),nrow=length(unique(nodes_euk$LouvainLabelD))))
colnames(abundance_of_phylum_in_cluster) <- unique(nodes_euk$Phylum)
rownames(abundance_of_phylum_in_cluster) <- sort(unique(nodes_euk$LouvainLabelD))


#Calculate the Abundance of Phylums per Cluster
for (cluster in sort(unique(nodes_euk$LouvainLabelD))){
  if(cluster == 0){
    cluster_fix = 1
  }else cluster_fix = cluster
  for (phylum in unique(nodes_euk$Phylum)){
    #Abundance of Phylum (skip NA phylum)
    if(!is.na(phylum)){
      abundance_of_phylum_in_cluster[cluster_fix,phylum] <- sum(nodes_euk[nodes_euk$LouvainLabelD == cluster &
                                                                            nodes_euk$Phylum == phylum,1],na.rm = TRUE)
      cat("Done with calculating the sum of Phylum (", phylum,") from Cluster", cluster, "\n",sep="")
    }else{
      next
    }
  }
}



# Calculate Bray-Curtis dissimilarity
bray_curtis_dist <- vegdist(abundance_of_phylum_in_cluster, method = "bray",na.rm = TRUE) %>% as.matrix() %>% as.table() %>% as.data.frame()


heatmap <- ggplot(bray_curtis_dist, aes(x = Var1, y = Var2, fill = Freq, label = round(Freq,2))) +
  geom_tile(color = "darkgray",lwd=1,linetype=1) + # Create heatmap
  coord_fixed()+
  geom_text(color = ifelse(bray_curtis_dist$Freq<=0.2 | bray_curtis_dist$Freq>=0.75 ,"white","black"), size = 4) + # Add text labels
  guides(fill = guide_colorbar(title="", barwidth=1.5 , barheight=35))+ #Adjust Legend Height & Width
  scale_fill_gradientn(colours = colorblind_gradient_palette,breaks=c(seq(0,1,0.1)))+ # Set color gradient
  theme_minimal() + # Set theme
  labs(x = "Cluster", y = "Cluster", title = "") + # Labels
  theme(axis.text.x = element_text(angle = 0, hjust = 1))# Rotate x-axis labels


heatmap

ggsave(plot_name,heatmap,path=plot_path)