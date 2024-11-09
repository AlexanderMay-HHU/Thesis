## Loading Libraries
library(ggplot2)  #Used to plot
library(dplyr)
library(vegan)    #Used for Shannon & Simpson Diversities
library(fossil)   #used for Chao1 - Richness

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/Diversities/Eukaryota/Alpha_diversity"
colorblind_gradient_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                                 "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                                 "#009e73","#f0e442","#0072b2","#d55e00",
                                 "#999999")


## Get Data
nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_euk <- subset(nodes,nodes$Kingdom == "Eukaryota" | nodes$Kingdom == "Archaea")


# Calculation of different alpha diversities per cluster
chao1 <- c()
shannon <- c()
simpson <- c()

for (cluster in sort(unique(nodes_euk$LouvainLabelD))){
  if(cluster == 0){
    cluster_fix = 1
  }else cluster_fix = cluster
  #Chao1 - Richness
  chao1 <- c(chao1,chao1(nodes_euk[which(nodes_euk$LouvainLabelD == cluster),1],taxa.row=TRUE))
  #Shannon Entropy
  shannon <- c(shannon,diversity(nodes_euk[which(nodes_euk$LouvainLabelD == cluster),1],index="shannon"))
  #Simpson-Index
  simpson <- c(simpson,diversity(nodes_euk[which(nodes_euk$LouvainLabelD == cluster),1],index="simpson"))
  
  cat("Done with calculating the alpha diversities of Cluster", cluster, "\n",sep="")
}



# Put all data into a single dataframe
ASV_count <- nodes_euk %>% group_by(LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence)) %>% select(ASVs)
df_diversity <- nodes_euk %>% group_by(LouvainLabelD) %>% summarise(Abundance = sum(Abundance4y)) %>%
  select(Abundance) %>% mutate(Abundance = Abundance)
df_diversity <- df_diversity %>% mutate(ASV_count)
df_diversity <- df_diversity %>% mutate(Chao1 = chao1)
df_diversity <- df_diversity %>% mutate(Shannon = shannon)
df_diversity <- df_diversity %>% mutate(Simpson = simpson)
df_diversity <- df_diversity %>% mutate(Color = colorblind_gradient_palette)
df_diversity <- data.frame(df_diversity)
rownames(df_diversity) <- c(0,2:13)


## Generate Barplots
#Chao1 - Richness
gg_chao_per_cluster <- ggplot(df_diversity, aes(x=as.integer(rownames(df_diversity)),
                                                y=Chao1,
                                                label=Chao1)) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Chao1 - Richness")+
  geom_text(vjust=-0.5,size=5)+
  theme(legend.position="none")+
  scale_x_continuous(breaks = c(0,2:13))

#Show it
gg_chao_per_cluster
#Save to plot_path
ggsave(filename="Chao1.png", plot=gg_chao_per_cluster, path=plot_path)




#Shannon Entropy
gg_shannon_per_cluster <- ggplot(df_diversity, aes(x=as.integer(rownames(df_diversity)),
                                                   y=Shannon,
                                                   label=round(Shannon,2))) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Shannon-Entropy")+
  geom_text(vjust=-0.5,size=5)+
  theme(legend.position="none")+
  scale_x_continuous(breaks = c(0,2:13))

#Show it
gg_shannon_per_cluster
#Save to plot_path
ggsave(filename="Shannon.png", plot=gg_shannon_per_cluster, path=plot_path)




#Simpson-Index
gg_simpson_per_cluster <- ggplot(df_diversity, aes(x=as.integer(rownames(df_diversity)),
                                                   y=Simpson,
                                                   label=round(Simpson,2))) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Simpson-Index")+
  geom_text(vjust=-0.5,size=5)+
  theme(legend.position="none")+
  scale_x_continuous(breaks = c(0,2:13))

#Show it
gg_simpson_per_cluster
#Save to plot_path
ggsave(filename="Simpson.png", plot=gg_simpson_per_cluster, path=plot_path)




#Number of ASvs
gg_asv_per_cluster <- ggplot(df_diversity, aes(x=as.integer(rownames(df_diversity)),
                                               y=ASVs,
                                               label=ASVs)) + 
  geom_bar(stat="identity",fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="ASVs")+
  geom_text(vjust=-0.5,size=5)+
  theme(legend.position="none")+
  scale_x_continuous(breaks = c(0,2:13))

#Show it
gg_asv_per_cluster
#Save to plot_path
ggsave(filename="Euk_ASV_Count.png", plot=gg_asv_per_cluster, path="R:/Studium/Bachelor/Thesis/generated_plots/Overview")