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

nodes_archaea <- subset(nodes,nodes$Kingdom == "Archaea")
nodes_bacteria <- subset(nodes,nodes$Kingdom == "Bacteria")
nodes_euk <- subset(nodes,nodes$Kingdom == "Eukaryota")

cluster_numbers <- as.factor(c(0,2:13))

# Calculation of different alpha diversities per cluster
# Archaea
arc_chao1 <- c()
arc_shannon <- c()
arc_simpson <- c()

for (cluster in sort(unique(nodes_archaea$LouvainLabelD))){
  if(cluster == 0){
    cluster_fix = 1
  }else cluster_fix = cluster
  #Chao1 - Richness
  arc_chao1 <- c(arc_chao1,chao1(nodes_archaea[which(nodes_archaea$LouvainLabelD == cluster),1],taxa.row=TRUE))
  #Shannon Entropy
  arc_shannon <- c(arc_shannon,diversity(nodes_archaea[which(nodes_archaea$LouvainLabelD == cluster),1],index="shannon"))
  #Simpson-Index
  arc_simpson <- c(arc_simpson,diversity(nodes_archaea[which(nodes_archaea$LouvainLabelD == cluster),1],index="simpson"))
  
  cat("Done with calculating the alpha diversities of Archaea Cluster ", cluster, "\n",sep="")
}


# Bacteria
bac_chao1 <- c()
bac_shannon <- c()
bac_simpson <- c()

for (cluster in sort(unique(nodes_bacteria$LouvainLabelD))){
  if(cluster == 0){
    cluster_fix = 1
  }else cluster_fix = cluster
  #Chao1 - Richness
  bac_chao1 <- c(bac_chao1,chao1(nodes_bacteria[which(nodes_bacteria$LouvainLabelD == cluster),1],taxa.row=TRUE))
  #Shannon Entropy
  bac_shannon <- c(bac_shannon,diversity(nodes_bacteria[which(nodes_bacteria$LouvainLabelD == cluster),1],index="shannon"))
  #Simpson-Index
  bac_simpson <- c(bac_simpson,diversity(nodes_bacteria[which(nodes_bacteria$LouvainLabelD == cluster),1],index="simpson"))
  
  cat("Done with calculating the alpha diversities of Bacteria Cluster ", cluster, "\n",sep="")
}


# Eukaryota
euk_chao1 <- c()
euk_shannon <- c()
euk_simpson <- c()

for (cluster in sort(unique(nodes_euk$LouvainLabelD))){
  if(cluster == 0){
    cluster_fix = 1
  }else cluster_fix = cluster
  #Chao1 - Richness
  euk_chao1 <- c(euk_chao1,chao1(nodes_euk[which(nodes_euk$LouvainLabelD == cluster),1],taxa.row=TRUE))
  #Shannon Entropy
  euk_shannon <- c(euk_shannon,diversity(nodes_euk[which(nodes_euk$LouvainLabelD == cluster),1],index="shannon"))
  #Simpson-Index
  euk_simpson <- c(euk_simpson,diversity(nodes_euk[which(nodes_euk$LouvainLabelD == cluster),1],index="simpson"))
  
  cat("Done with calculating the alpha diversities of Eukarytota Cluster ", cluster, "\n",sep="")
}



# Put into kingdom specific dataframes
# Archaea
arc_asv <- nodes_archaea %>% group_by(LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence)) %>% select(ASVs)
arc_df <- data.frame(
  cluster = cluster_numbers,
  kingdom = "arc",
  ASV_count = arc_asv,
  chao1 = arc_chao1,
  shannon = arc_shannon,
  simpson = arc_simpson
)

# Bacteria
bac_asv <- nodes_bacteria %>% group_by(LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence)) %>% select(ASVs)
bac_df <- data.frame(
  cluster = cluster_numbers,
  kingdom = "bac",
  ASV_count = bac_asv,
  chao1 = bac_chao1,
  shannon = bac_shannon,
  simpson = bac_simpson
)

# Eukaryota
euk_asv <- nodes_euk %>% group_by(LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence)) %>% select(ASVs)
euk_df <- data.frame(
  cluster = cluster_numbers,
  kingdom = "euk",
  ASV_count = euk_asv,
  chao1 = euk_chao1,
  shannon = euk_shannon,
  simpson = euk_simpson
)


# Put overall data into a single dataframe
df_diversity <- rbind(arc_df, bac_df, euk_df)




## Generate Barplots
# ASVs
gg_asvs <- ggplot(df_diversity, aes(x = cluster, y = ASVs, fill = kingdom)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_vline(xintercept = seq(1.5, 12.5, by = 1))+
  labs(
    title = "ASV Count by Cluster and Kingdom",
    x = "Cluster",
    y = "ASV Count",
    fill = "Kingdom") +
  scale_fill_manual(
    values = c("arc" = "#F8766D", "bac" = "#00BA38", "euk" = "#619CFF"),
    labels = c("Archaea", "Bacteria", "Eukaryota")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())
# Show it
gg_asvs

# Save it
ggsave(filename="ASV_Count.png", plot=gg_asvs,
       path=paste0(plot_path,"03_Diversities/Alpha/"))



# Chao1
gg_chao <- ggplot(df_diversity, aes(x = cluster, y = chao1, fill = kingdom)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_vline(xintercept = seq(1.5, 12.5, by = 1))+
  labs(
    title = "Chao1 Richness by Cluster and Kingdom",
    x = "Cluster",
    y = "Chao1 Richness",
    fill = "Kingdom") +
  scale_fill_manual(
    values = c("arc" = "#F8766D", "bac" = "#00BA38", "euk" = "#619CFF"),
    labels = c("Archaea", "Bacteria", "Eukaryota")) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank())
# Show it
gg_chao

# Save it
ggsave(filename="Chao.png", plot=gg_chao,
       path=paste0(plot_path,"03_Diversities/Alpha/"))



# Shannon
gg_shannon <- ggplot(df_diversity, aes(x = cluster, y = shannon, fill = kingdom)) +
                geom_col(position = position_dodge(width = 0.8), width = 0.7) +
                geom_vline(xintercept = seq(1.5, 12.5, by = 1))+
                labs(
                  title = "Shannon Entropy by Cluster and Kingdom",
                  x = "Cluster",
                  y = "Shannon Entropy",
                  fill = "Kingdom") +
                scale_fill_manual(
                  values = c("arc" = "#F8766D", "bac" = "#00BA38", "euk" = "#619CFF"),
                  labels = c("Archaea", "Bacteria", "Eukaryota")) +
                theme(
                  plot.title = element_text(hjust = 0.5, face = "bold"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank())
# Show it
gg_shannon

# Save it
ggsave(filename="Shannon.png", plot=gg_shannon,
       path=paste0(plot_path,"03_Diversities/Alpha/"))



# Simpson
gg_simpson <- ggplot(df_diversity, aes(x = cluster, y = simpson, fill = kingdom)) +
                geom_col(position = position_dodge(width = 0.8), width = 0.7) +
                geom_vline(xintercept = seq(1.5, 12.5, by = 1))+
                labs(
                  title = "Simpson Index by Cluster and Kingdom",
                  x = "Cluster",
                  y = "Simpson Index",
                  fill = "Kingdom") +
                scale_fill_manual(
                  values = c("arc" = "#F8766D", "bac" = "#00BA38", "euk" = "#619CFF"),
                  labels = c("Archaea", "Bacteria", "Eukaryota")) +
                theme(
                  plot.title = element_text(hjust = 0.5, face = "bold"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank())
# Show it
gg_simpson

# Save it
ggsave(filename="Simpson.png", plot=gg_simpson,
       path=paste0(plot_path,"03_Diversities/Alpha/"))