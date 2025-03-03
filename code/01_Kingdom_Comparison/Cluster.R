##Loading Libraries
library(ggplot2)
library(dplyr)

#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data"))


colorblind_gradient_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                                 "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                                 "#009e73","#f0e442","#0072b2","#d55e00",
                                 "#999999")


nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA


#Sum of Abundance from different Kingdoms
nodes_cluster <- nodes %>% group_by(Kingdom,LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence), Abundance = sum(Abundance4y))
nodes_cluster <- nodes_cluster %>% group_by(LouvainLabelD) %>% mutate(Freq = ASVs/sum(ASVs))

#ASVs per cluster
ASV_count <- nodes %>% group_by(LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence))
avg_asv_per_cluster <-  sum(ASV_count$ASVs)/length(ASV_count$ASVs)


#Sum of different Kingdoms in total
nodes_sum_total <- nodes %>% group_by(Kingdom) %>% summarise(ASVs = n_distinct(Sequence))
nodes_sum_total$LouvainLabelD <- c("Total")
nodes_sum_total <- nodes_sum_total %>% mutate(Freq = ASVs/sum(ASVs))

#% of Kingdoms from total
percent_kingdoms_total <- c(nodes_sum_total$Freq[nodes_sum_total$Kingdom == "Eukaryota"],
  nodes_sum_total$Freq[nodes_sum_total$Kingdom == "Bacteria"],
  nodes_sum_total$Freq[nodes_sum_total$Kingdom == "Archaea"])



## Generate Barplots
# ASV Count by Kingdom & Cluster
gg_distribution_count <- ggplot(nodes_cluster,
                                aes(fill=Kingdom,
                                    y=ASVs,
                                    x=factor(LouvainLabelD),
                                    label=ASVs)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Louvain Cluster",y="ASVs",title = "ASV count of Kingdoms by cluster ",
       linetype="Average ASVs\nper Cluster")+
  geom_text(check_overlap=F,size = 4,position = position_stack(0.5))+
  geom_hline(aes(yintercept = avg_asv_per_cluster, linetype = ""), color = "black") +
  scale_linetype_manual(values = "dashed") + 
  theme(legend.key.size = unit(0.65, units = "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Show it
gg_distribution_count

#Save to plot_path
ggsave(filename="Cluster_Count.png", plot=gg_distribution_count, path=paste0(plot_path, "01_Kingdom_Comparison"))



# Total ASV Count
gg_asv_cluster <- ggplot(ASV_count,
                         aes(fill=LouvainLabelD,
                             y=ASVs,
                             x=factor(LouvainLabelD),
                             label=ASVs)) +
  geom_bar(fill=colorblind_gradient_palette, stat="identity")+ # Cluster Kingdoms Barplot
  labs(x="Louvain Cluster",y="ASVs", title = "ASVs by Cluster")+ # Axis labels
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


gg_asv_cluster_number <- gg_asv_cluster +
  geom_text(inherit.aes = T,
            size = 4, vjust=-0.5)#Barplot text value

#Show it  
gg_asv_cluster
gg_asv_cluster_number

#Save to plot_path
ggsave(filename="Cluster_ASV_Count.png", plot=gg_asv_cluster, path=paste0(plot_path, "00_Appendix/01_Kingdom_Comparison"))
ggsave(filename="Cluster_ASV_Count_Numbers.png", plot=gg_asv_cluster_number, path=paste0(plot_path, "00_Appendix/01_Kingdom_Comparison"))



# Percentage of Abundance by cluster
gg_distribution_percent <- ggplot(nodes_cluster,
                                  aes(fill=Kingdom,
                                      y=Freq,
                                      x=factor(LouvainLabelD),
                                      label=round(Freq,3)*100)) + 
  geom_bar(position="dodge", stat="identity")+ # Cluster Kingdoms Barplot
  labs(x="Louvain Cluster",y="% of clusters ASV", title = "Ratio of Kingdoms by cluster",
       linetype="Kingdom (Total)", fill="Kingdom (Cluster)")+ # Axis labels
  geom_hline(aes(yintercept=percent_kingdoms_total[1], linetype = "Eukaryota"),
             color = "#619CFF",linewidth=0.75)+ #Eukaryota Total
  geom_hline(aes(yintercept=percent_kingdoms_total[2], linetype = "Bacteria"),
             color = "#00BA38",linewidth=0.75)+ # Bacteria Total
  geom_hline(aes(yintercept=percent_kingdoms_total[3], linetype = "Archaea"),
             color = "#F8766D",linewidth=0.75)+ # Archaea Total
  scale_linetype_manual(values=c("dashed","dashed","dashed")) + #Total linetypes
  scale_y_continuous(labels = scales::label_percent(suffix=""),breaks=seq(0,1,by=0.1))+#Y-Axis values
  theme(legend.key.size = unit(0.65, units = "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+ #Legend size adjustments
  geom_vline(xintercept = seq(1.5, 12.5, by = 1))

gg_distribution_percent_number <- gg_distribution_percent +
                                    geom_text(inherit.aes = T, size = 2.5,
                                              position = position_dodge(0.9), vjust=-0.5) #Barplot text value

# Show it
gg_distribution_percent
gg_distribution_percent_number

#Save to plot_path
ggsave(filename="Cluster_Percent.png", plot=gg_distribution_percent, path=paste0(plot_path, "00_Appendix/01_Kingdom_Comparison/"))
ggsave(filename="Cluster_Percent_Numbers.png", plot=gg_distribution_percent_number, path=paste0(plot_path, "01_Kingdom_Comparison"))




# Abundance
gg_abundance_asv <- ggplot(nodes_cluster,
                         aes(x=factor(LouvainLabelD),
                             y=Abundance,
                             fill=Kingdom)) + 
  geom_bar(position="dodge", stat="identity")+
  geom_vline(xintercept = seq(1.5, 12.5, by = 1))+
  labs(x="Louvain Cluster",
       y="Abundance",
       title = "Abundance by Kingdom & cluster",
       fill="Kingdom")+ # Axis labels
  theme(legend.key.size = unit(0.65, units = "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) #Legend size adjustments
# Show it
gg_abundance_asv
#Save to plot_path
ggsave(filename="Cluster_Abundance.png", plot=gg_abundance_asv, path=paste(plot_path,"/01_Kingdom_Comparison",sep=""))



# Abundance by Kingdoms
gg_abundance_asv_arc <- ggplot(subset(nodes_cluster,nodes_cluster$Kingdom == "Archaea"),
                           aes(x=factor(LouvainLabelD),
                               y=Abundance)) + 
  geom_bar(stat="identity", fill="#F8766D")+
  labs(x="Louvain Cluster",y="Abundance", title = "Archaea Abundance by cluster")+# Axis labels
  theme(legend.key.size = unit(0.65, units = "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+ #Legend size adjustments 
  geom_text(aes(label=paste0(round(Abundance,1),"\n",ASVs)),
            size = 4, position = position_dodge(0.9), vjust=0.5) #Barplot text value
# Show it
gg_abundance_asv_arc
#Save to plot_path
ggsave(filename="Cluster_Abundance_Arc.png", plot=gg_abundance_asv_arc, path=paste(plot_path,"00_Appendix/01_Kingdom_Comparison",sep=""))



gg_abundance_asv_bac <- ggplot(subset(nodes_cluster,nodes_cluster$Kingdom == "Bacteria"),
                               aes(x=factor(LouvainLabelD),
                                   y=Abundance)) + 
  geom_bar(stat="identity", fill="#00BA38")+
  labs(x="Louvain Cluster",y="Abundance", title = "Bacteria Abundance by cluster")+# Axis labels
  theme(legend.key.size = unit(0.65, units = "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+ #Legend size adjustments 
  geom_text(aes(label=paste0(round(Abundance,1),"\n",ASVs)),
            size = 4, position = position_dodge(0.9), vjust=0.5) #Barplot text value
# Show it
gg_abundance_asv_bac
#Save to plot_path
ggsave(filename="Cluster_Abundance_Bac.png", plot=gg_abundance_asv_bac, path=paste(plot_path,"00_Appendix/01_Kingdom_Comparison",sep=""))



gg_abundance_asv_euk <- ggplot(subset(nodes_cluster,nodes_cluster$Kingdom == "Eukaryota"),
                               aes(x=factor(LouvainLabelD),
                                   y=Abundance)) + 
  geom_bar(stat="identity", fill="#619CFF")+
  labs(x="Louvain Cluster",y="Abundance", title = "Eukaryota Abundance by cluster")+# Axis labels
  theme(legend.key.size = unit(0.65, units = "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+ #Legend size adjustments 
  geom_text(aes(label=paste0(round(Abundance,1),"\n",ASVs)),
            size = 4, position = position_dodge(0.9), vjust=0.5) #Barplot text value
# Show it
gg_abundance_asv_euk
#Save to plot_path
ggsave(filename="Cluster_Abundance_Euk.png", plot=gg_abundance_asv_euk, path=paste(plot_path,"00_Appendix/01_Kingdom_Comparison",sep=""))