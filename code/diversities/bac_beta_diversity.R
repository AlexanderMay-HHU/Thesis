## Loading Libraries
library(dplyr)
library(vegan)    #Used for Bray-Curtis Distance
library(pheatmap) #Used to plot the Heatmap
library(grid)     #Used to display Labels for Heatmap axis


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
plot_name <- "Bac_beta_diversity.png"



## Get Data
nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_bac <- subset(nodes,nodes$Kingdom == "Bacteria")


## Make Diversity dataframe
abundance_of_phylum_in_cluster <- data.frame(matrix(ncol=length(unique(nodes_bac$Phylum)),nrow=length(unique(nodes_bac$LouvainLabelD))))
colnames(abundance_of_phylum_in_cluster) <- unique(nodes_bac$Phylum)
rownames(abundance_of_phylum_in_cluster) <- sort(unique(nodes_bac$LouvainLabelD))


#Calculate the Abundance of Phylums per Cluster
for (cluster in sort(unique(nodes_bac$LouvainLabelD))){
  if(cluster == 0){
    cluster_fix = 1
  }else cluster_fix = cluster
  for (phylum in unique(nodes_bac$Phylum)){
    #Abundance of Phylum (skip NA phylum)
    if(!is.na(phylum)){
      abundance_of_phylum_in_cluster[cluster_fix,phylum] <- sum(nodes_bac[nodes_bac$LouvainLabelD == cluster &
                                                                            nodes_bac$Phylum == phylum,1],na.rm = TRUE)
      cat("Done with calculating the sum of Phylum (", phylum,") from Cluster", cluster, "\n",sep="")
    }else{
      next
    }
  }
}



# Calculate Bray-Curtis dissimilarity
bray_curtis_dist <- vegdist(abundance_of_phylum_in_cluster, method = "bray",na.rm = TRUE) %>% as.matrix() %>% as.data.frame()
bc_for_ggplot <- expand.grid(From_Cluster=colnames(bray_curtis_dist),To_Cluster=rownames(bray_curtis_dist))




#Make Heatmap & save with x-y Axis labels
png(paste(plot_path,"/Diversities/Bacteria/",plot_name,sep=""),width=900,height=900)
  setHook("grid.newpage", function() pushViewport(viewport(x=1,y=1,width=0.9, height=0.9, name="vp", just=c("right","top"))), action="prepend")
  bray_heatmap <- pheatmap(bray_curtis_dist,angle_col= 0, display_numbers= TRUE, ,cluster_rows=FALSE, cluster_cols=FALSE,
                           color = colorRampPalette(c("#3b4cc0", "#ffffff", "#F21A00"))(50),border_color = "#ffffff",number_color= "#303030",
                           main="Bray-Curtis beta diversity of bacteria between clusters")
  setHook("grid.newpage", NULL, "replace")
  grid.text("Cluster", y=-0.02, gp=gpar(fontsize=16))
  grid.text("Cluster", x=-0.02, rot=90, gp=gpar(fontsize=16))
dev.off()