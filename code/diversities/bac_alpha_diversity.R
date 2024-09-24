## Loading Libraries
library(ggplot2)  #Used to plot
library(dplyr)
library(vegan)    #Used for Shannon & Simpson Diversities
library(fossil)   #used for Chao1 - Richness


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"



## Get Data
nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_bac <- subset(nodes,nodes$Kingdom == "Bacteria")



## Make Diversity dataframe
df_diversity <- data.frame(matrix(ncol=4,nrow=length(unique(nodes_bac$LouvainLabelD))))
rownames(df_diversity) <- sort(unique(nodes_bac$LouvainLabelD))
colnames(df_diversity) <- c("Chao","Shannon","Simpson","Color")
df_diversity[,"Color"] <- nodes_bac[match(sort(unique(nodes_bac$LouvainLabelD)),nodes_bac$LouvainLabelD),"louvain_label_color"]



# Calculation of different alpha diversities per cluster
for (cluster in sort(unique(nodes_bac$LouvainLabelD))){
  if(cluster == 0){
    cluster_fix = 1
  }else cluster_fix = cluster
  #Shannon Entropy
  df_diversity[cluster_fix,"Shannon"] <- diversity(nodes_bac[which(nodes_bac$LouvainLabelD == cluster),1],index="shannon")
  #Simpson-Index
  df_diversity[cluster_fix,"Simpson"] <- diversity(nodes_bac[which(nodes_bac$LouvainLabelD == cluster),1],index="simpson")
  #Chao1 - Richness
  df_diversity[cluster_fix,"Chao"] <- chao1(nodes_bac[which(nodes_bac$LouvainLabelD == cluster),1],taxa.row=TRUE)
  
  cat("Done with calculating the alpha diversities of Cluster", cluster, "\n",sep="")
}


## Generate Barplots
#Chao1 - Richness
gg_chao_per_cluster <- ggplot(df_diversity, aes(fill=Color, y=Chao, x=as.integer(rownames(df_diversity)))) + 
  geom_bar(stat="identity")+
  labs(x="Louvain Cluster",y="Chao1 - Richness",title="Bacteria")+
  theme(legend.position="none")+
  scale_x_continuous(breaks=seq(0,13,by=1))

#Show it
gg_chao_per_cluster
#Save to plot_path
ggsave(filename="Bac_Chao_per_cluster.png", plot=gg_chao_per_cluster, path=paste(plot_path,"/Diversities/Bacteria",sep=""))




#Shannon Entropy
gg_shannon_per_cluster <- ggplot(df_diversity, aes(fill=Color, y=Shannon, x=as.integer(rownames(df_diversity)))) + 
  geom_bar(stat="identity")+
  labs(x="Louvain Cluster",y="Shannon-Entropy",title="Bacteria")+
  theme(legend.position="none")+
  scale_x_continuous(breaks=seq(0,13,by=1))

#Show it
gg_shannon_per_cluster
#Save to plot_path
ggsave(filename="Bac_Shannon_per_cluster.png", plot=gg_shannon_per_cluster, path=paste(plot_path,"/Diversities/Bacteria",sep=""))




#Simpson-Index
gg_simpson_per_cluster <- ggplot(df_diversity, aes(fill=Color, y=Simpson, x=as.integer(rownames(df_diversity)))) + 
  geom_bar(stat="identity")+
  labs(x="Louvain Cluster",y="Simpson-Index",title="Bacteria")+
  theme(legend.position="none")+
  scale_x_continuous(breaks=seq(0,13,by=1))

#Show it
gg_simpson_per_cluster
#Save to plot_path
ggsave(filename="Bac_Simpson_per_cluster.png", plot=gg_simpson_per_cluster, path=paste(plot_path,"/Diversities/Bacteria",sep=""))




#Number of ASvs
gg_asv_per_cluster <- ggplot(nodes_bac, aes(fill=louvain_label_color, y=Abundance4y, x=LouvainLabelD)) + 
  geom_bar(stat="identity")+
  labs(x="Louvain Cluster",y="No of ASVs",title="Bacteria")+
  theme(legend.position="none")+
  scale_x_continuous(breaks=seq(0,13,by=1))

#Show it
gg_asv_per_cluster
#Save to plot_path
ggsave(filename="Bac_ASV_per_cluster.png", plot=gg_asv_per_cluster, path=paste(plot_path,"/Overview",sep=""))