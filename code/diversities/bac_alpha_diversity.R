## Loading Libraries
library(ggplot2)  #Used to plot
library(dplyr)
library(vegan)    #Used for Shannon & Simpson Diversities
library(fossil)   #used for Chao1 - Richness


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/Diversities/Bacteria/Alpha_diversity"
colorblind_gradient_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                                 "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                                 "#009e73","#f0e442","#0072b2","#d55e00",
                                 "#999999")



## Get Data
nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_bac <- subset(nodes,nodes$Kingdom == "Bacteria")



## Make Diversity dataframe
df_diversity <- data.frame(matrix(ncol=5,nrow=length(unique(nodes_bac$LouvainLabelD))))
rownames(df_diversity) <- sort(unique(nodes_bac$LouvainLabelD))
colnames(df_diversity) <- c("Chao","Shannon","Simpson","ASV_Count","Color")
df_diversity[,"Color"] <- colorblind_gradient_palette



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
  #Number of ASVs
  df_diversity[cluster_fix,"ASV_Count"] <- sum(nodes_bac[which(nodes_bac$LouvainLabelD == cluster),1])
  
  cat("Done with calculating the alpha diversities of Cluster", cluster, "\n",sep="")
}


## Generate Barplots
#Chao1 - Richness
gg_chao_per_cluster <- ggplot(df_diversity, aes(x=as.integer(rownames(df_diversity)), y=Chao)) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Chao1 - Richness")+
  theme(legend.position="none")+
  scale_x_continuous(breaks = c(0,2:13))

#Show it
gg_chao_per_cluster
#Save to plot_path
ggsave(filename="Chao.png", plot=gg_chao_per_cluster, path=plot_path)




#Shannon Entropy
gg_shannon_per_cluster <- ggplot(df_diversity, aes(x=as.integer(rownames(df_diversity)), y=Shannon)) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Shannon-Entropy")+
  theme(legend.position="none")+
  scale_x_continuous(breaks = c(0,2:13))

#Show it
gg_shannon_per_cluster
#Save to plot_path
ggsave(filename="Shannon.png", plot=gg_shannon_per_cluster, path=plot_path)




#Simpson-Index
gg_simpson_per_cluster <- ggplot(df_diversity, aes(x=as.integer(rownames(df_diversity)), y=Simpson)) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Simpson-Index")+
  theme(legend.position="none")+
  scale_x_continuous(breaks = c(0,2:13))

#Show it
gg_simpson_per_cluster
#Save to plot_path
ggsave(filename="Simpson.png", plot=gg_simpson_per_cluster, path=plot_path)




#Number of ASvs
gg_asv_per_cluster <- ggplot(df_diversity, aes(x=as.integer(rownames(df_diversity)), y=ASV_Count)) + 
  geom_bar(stat="identity",fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="ASVs")+
  theme(legend.position="none")+
  scale_x_continuous(breaks = c(0,2:13))

#Show it
gg_asv_per_cluster
#Save to plot_path
ggsave(filename="Bac_ASV_Count.png", plot=gg_asv_per_cluster, path="R:/Studium/Bachelor/Thesis/generated_plots/Overview")