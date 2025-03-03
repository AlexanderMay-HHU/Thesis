## Loading Libraries
library(ggplot2)  #Used to plot
library(dplyr)
library(vegan)    #Used for Shannon & Simpson Diversities
library(fossil)   #used for Chao1 - Richness
library(patchwork)

#Set Working Directory
project_folder <- dirname(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data"))


colorblind_gradient_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                                 "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                                 "#009e73","#f0e442","#0072b2","#d55e00",
                                 "#999999")


## Get Data
nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_prokaryotes <- subset(nodes,nodes$Kingdom == "Bacteria" | nodes$Kingdom == "Archaea")


# Calculation of different alpha diversities per cluster
chao1 <- c()
shannon <- c()
simpson <- c()

for (cluster in sort(unique(nodes_prokaryotes$LouvainLabelD))){
  if(cluster == 0){
    cluster_fix = 1
  }else cluster_fix = cluster
  #Chao1 - Richness
  chao1 <- c(chao1,chao1(nodes_prokaryotes[which(nodes_prokaryotes$LouvainLabelD == cluster),1],taxa.row=TRUE))
  #Shannon Entropy
  shannon <- c(shannon,diversity(nodes_prokaryotes[which(nodes_prokaryotes$LouvainLabelD == cluster),1],index="shannon"))
  #Simpson-Index
  simpson <- c(simpson,diversity(nodes_prokaryotes[which(nodes_prokaryotes$LouvainLabelD == cluster),1],index="simpson"))
  
  cat("Done with calculating the alpha diversities of Cluster", cluster, "\n",sep="")
}



# Put all data into a single dataframe
ASV_count <- nodes_prokaryotes %>% group_by(LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence)) %>% select(ASVs)
df_diversity <- nodes_prokaryotes %>% group_by(LouvainLabelD) %>% summarise(Abundance = sum(Abundance4y)) %>%
  select(Abundance) %>% mutate(Abundance = Abundance)
df_diversity <- df_diversity %>% mutate(Cluster = as.factor(c(0,2:13)), ASV_count, Chao1 = chao1, Shannon = shannon, Simpson = simpson, Color = colorblind_gradient_palette)
df_diversity <- data.frame(df_diversity)
rownames(df_diversity) <- c(0,2:13)

# Correlation
print(paste0("Korrelation [Anzahl-Shannon]: ", round(cor(df_diversity$Chao1,df_diversity$Shannon,method = "pearson"),3)))
print(paste0("Korrelation [Anzahl-Simpson]: ", round(cor(df_diversity$Chao1,df_diversity$Simpson,method = "pearson"),3)))


## Generate Barplots
#Number of ASvs
gg_asv_per_cluster <- ggplot(df_diversity, aes(x=Cluster,
                                               y=ASVs,
                                               label=ASVs)) + 
  geom_bar(stat="identity",fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="ASVs")+
  theme(legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#Show it
gg_asv_per_cluster+
  labs(title="Archaea + Bacteria (Prokaryotes)")+
  geom_text(vjust=-0.5,size=5)
#Save to plot_path
ggsave(filename="ASV_Count.png", plot=gg_asv_per_cluster+
                                        labs(title="Archaea + Bacteria (Prokaryotes)")+
                                        geom_text(vjust=-0.5,size=5),
       path=paste0(plot_path,"00_Appendix/03_Diversities/Alpha/Prokaryotes/"))




#Chao1 - Richness
gg_chao_per_cluster <- ggplot(df_diversity, aes(x=Cluster,
                                                y=Chao1,
                                                label=Chao1)) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Chao1 - Richness")+
  theme(legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#Show it
gg_chao_per_cluster+
  labs(title="Archaea + Bacteria (Prokaryotes)")+
  geom_text(vjust=-0.5,size=5)
#Save to plot_path
ggsave(filename="Chao1.png", plot=gg_chao_per_cluster+
                                      labs(title="Archaea + Bacteria (Prokaryotes)")+
                                      geom_text(vjust=-0.5,size=5),
       path=paste0(plot_path,"00_Appendix/03_Diversities/Alpha/Prokaryotes/"))




#Shannon Entropy
gg_shannon_per_cluster <- ggplot(df_diversity, aes(x=Cluster,
                                                   y=Shannon,
                                                   label=round(Shannon,2))) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Shannon-Entropy")+
  theme(legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#Show it
gg_shannon_per_cluster+
  labs(title="Archaea + Bacteria (Prokaryotes)")+
  geom_text(vjust=-0.5,size=5)
#Save to plot_path
ggsave(filename="Shannon.png", plot=gg_shannon_per_cluster+
                                        labs(title="Archaea + Bacteria (Prokaryotes)")+
                                        geom_text(vjust=-0.5,size=5),
       path=paste0(plot_path,"00_Appendix/03_Diversities/Alpha/Prokaryotes/"))




#Simpson-Index
gg_simpson_per_cluster <- ggplot(df_diversity, aes(x=Cluster,
                                                   y=Simpson,
                                                   label=round(Simpson,2))) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Simpson-Index")+
  theme(legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

#Show it
gg_simpson_per_cluster+
  labs(title="Archaea + Bacteria (Prokaryotes)")+
  geom_text(vjust=-0.5,size=5)
#Save to plot_path
ggsave(filename="Simpson.png", plot=gg_simpson_per_cluster+
                                        labs(title="Archaea + Bacteria (Prokaryotes)")+
                                        geom_text(vjust=-0.5,size=5),
       path=paste0(plot_path,"00_Appendix/03_Diversities/Alpha/Prokaryotes/"))



# Combined ASV Count & Chao Plot
count_alpha_divs <- (gg_asv_per_cluster+geom_text(vjust=-0.5,size=3.5) | gg_chao_per_cluster+geom_text(vjust=-0.5,size=3.5))
count_alpha_divs <- count_alpha_divs + plot_annotation(tag_levels = 'A')
# Show it
count_alpha_divs
# Save to plot_path
ggsave(filename="Combined_ASV_Chao.png", plot=count_alpha_divs,
       path=paste0(plot_path,"03_Diversities/Alpha/Prokaryotes/"))



# Combined Shannon Entropy & Simpson Index
val_alpha_divs <- (gg_shannon_per_cluster+geom_text(vjust=-0.5,size=3.5) | gg_simpson_per_cluster+geom_text(vjust=-0.5,size=3.5))
val_alpha_divs <- val_alpha_divs + plot_annotation(tag_levels = 'A')
# Show it
val_alpha_divs
# Save to plot_path
ggsave(filename="Combined_Shannon_Simpson.png", plot=val_alpha_divs,
       path=paste0(plot_path,"03_Diversities/Alpha/Prokaryotes/"))