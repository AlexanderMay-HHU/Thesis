##Loading Libraries
library(ggplot2)
library(dplyr)

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/"
colorblind_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                        "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                        "#009e73","#f0e442","#0072b2","#d55e00",
                        "#999999")


nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA


# By Kingdom
nodes_arc <- subset(nodes,nodes$Kingdom == "Archaea")
nodes_bac <- subset(nodes,nodes$Kingdom == "Bacteria")
nodes_euk <- subset(nodes,nodes$Kingdom == "Eukaryota")


#Sum of Abundance from different Kingdoms
arc_cluster <- nodes_arc %>% group_by(LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence), Abundance = sum(Abundance4y))


## Generate Barplots
gg_arc_cluster <- ggplot(arc_cluster,
                            aes(
                                y=Abundance,
                                x=LouvainLabelD,
                                label=paste0(round(Abundance,1),"\n",ASVs))) + 
  geom_bar(stat="identity")+
  geom_vline(aes(xintercept=c(0)+0.5))+
  geom_vline(aes(xintercept=c(1)+0.5))+
  geom_vline(aes(xintercept=c(2)+0.5))+
  geom_vline(aes(xintercept=c(3)+0.5))+
  geom_vline(aes(xintercept=c(4)+0.5))+
  geom_vline(aes(xintercept=c(5)+0.5))+
  geom_vline(aes(xintercept=c(6)+0.5))+
  geom_vline(aes(xintercept=c(7)+0.5))+
  geom_vline(aes(xintercept=c(8)+0.5))+
  geom_vline(aes(xintercept=c(9)+0.5))+
  geom_vline(aes(xintercept=c(10)+0.5))+
  geom_vline(aes(xintercept=c(11)+0.5))+
  geom_vline(aes(xintercept=c(12)+0.5))+
  labs(x="Cluster",y="Abundance",
       title = "Archaea Abundance by Cluster")+ # Axis labels
  scale_x_continuous(breaks = c(0,2:13))+
  scale_fill_discrete(type=colorblind_palette)+
  #scale_y_continuous(labels = scales::label_percent(suffix=""),breaks=seq(0,1,by=0.1))+#Y-Axis values
  theme(legend.key.size = unit(0.65, units = "cm") )+ #Legend size adjustments removing legend here ,legend.position="none"
  geom_text(inherit.aes = T,size = 4,position = position_dodge(0.9),vjust=0.5) #Barplot text value

gg_arc_cluster


ggsave(filename="Family_Overall.png", plot=gg_family_overall, path=paste(plot_path,"/02_Top/Eukaryote",sep=""))

























#Count numbers /w values
gg_distribution_count <- ggplot(nodes_sum_cluster,
                                aes(fill=Kingdom,
                                    y=ASVs,
                                    x=LouvainLabelD,
                                    label=ASVs)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Louvain Cluster",y="# of ASVs",title = "ASV count of Kingdoms per cluster ",
       linetype="Average ASVs\nper Cluster")+
  geom_text(check_overlap=F,size = 4,position = position_stack(0.5))+
  geom_hline(aes(yintercept = avg_asv_per_cluster, linetype = ""), color = "black") +
  scale_linetype_manual(values = "dashed") + 
  geom_text(aes(0,avg_asv_per_cluster,label = round(avg_asv_per_cluster,0), hjust=2.5,vjust=-1))+#avg ASV per cluster Text
  scale_x_continuous(breaks = c(0,2:13))+
  theme(legend.key.size = unit(0.65, units = "cm"))#+
#scale_fill_discrete(type=colorblind_palette)

# Show it
gg_distribution_count

#Save to plot_path
ggsave(filename="Cluster_Count.png", plot=gg_distribution_count, path=paste0(plot_path, "00_Appendix/01_Kingdom_Comparison/Values"))



#Percentage per cluster /w values
gg_distribution_percent <- ggplot(nodes_sum_cluster,
                                  aes(fill=Kingdom,
                                      y=Freq,
                                      x=LouvainLabelD,
                                      label=round(Freq,3)*100)) + 
  geom_bar(position="dodge", stat="identity")+ # Cluster Kingdoms Barplot
  labs(x="Louvain Cluster",y="% of clusters ASV", title = "Ratio of Kingdoms per cluster",
       linetype="Kingdom (Total)", fill="Kingdom (Cluster)")+ # Axis labels
  geom_hline(aes(yintercept=percent_kingdoms_total[1], linetype = "Eukaryota"),
             color = "#619CFF",linewidth=0.75)+ #Eukaryota Total
  geom_hline(aes(yintercept=percent_kingdoms_total[2], linetype = "Bacteria"),
             color = "#00BA38",linewidth=0.75)+ # Bacteria Total
  geom_hline(aes(yintercept=percent_kingdoms_total[3], linetype = "Archaea"),
             color = "#F8766D",linewidth=0.75)+ # Archaea Total
  scale_linetype_manual(values=c("dashed","dashed","dashed")) + #Total linetypes
  scale_x_continuous(breaks = c(0,2:13))+ #X Axis values
  scale_y_continuous(labels = scales::label_percent(suffix=""),breaks=seq(0,1,by=0.1))+#Y-Axis values
  theme(legend.key.size = unit(0.65, units = "cm"))+ #Legend size adjustments
  geom_text(inherit.aes = T,size = 2.5,position = position_dodge(0.9),vjust=-0.5)+#Barplot text value
  geom_text(aes(0,percent_kingdoms_total[1],label = round(percent_kingdoms_total[1],3)*100, hjust=2.25,vjust=-1))+#Eukaryota Total Text
  geom_text(aes(0,percent_kingdoms_total[2],label = round(percent_kingdoms_total[2],3)*100, hjust=2.25,vjust=-1))+#Bacteria Total Text
  geom_text(aes(0,percent_kingdoms_total[3],label = round(percent_kingdoms_total[3],3)*100, hjust=2.25,vjust=-1))+#Archaea Total Text
  geom_vline(aes(xintercept=c(0)+0.5))+
  geom_vline(aes(xintercept=c(1)+0.5))+
  geom_vline(aes(xintercept=c(2)+0.5))+
  geom_vline(aes(xintercept=c(3)+0.5))+
  geom_vline(aes(xintercept=c(4)+0.5))+
  geom_vline(aes(xintercept=c(5)+0.5))+
  geom_vline(aes(xintercept=c(6)+0.5))+
  geom_vline(aes(xintercept=c(7)+0.5))+
  geom_vline(aes(xintercept=c(8)+0.5))+
  geom_vline(aes(xintercept=c(9)+0.5))+
  geom_vline(aes(xintercept=c(10)+0.5))+
  geom_vline(aes(xintercept=c(11)+0.5))+
  geom_vline(aes(xintercept=c(12)+0.5))#+
  #scale_fill_discrete(type=colorblind_palette)

# Show it
gg_distribution_percent

#Save to plot_path
ggsave(filename="Cluster_Percent.png", plot=gg_distribution_percent, path=paste0(plot_path, "01_Kingdom_Comparison"))


#ASV Count 
gg_asv_cluster <- ggplot(ASV_count,
                       aes(fill=LouvainLabelD,
                           y=ASVs,
                           x=LouvainLabelD,
                           label=ASVs)) +
  geom_bar(fill=colorblind_gradient_palette, stat="identity")+ # Cluster Kingdoms Barplot
  labs(x="Louvain Cluster",y="ASVs", title = "ASV Count per Cluster")+ # Axis labels
  scale_x_continuous(breaks = c(0,2:13))+ #X Axis values
  geom_text(inherit.aes = T,size = 4,vjust=-0.5)#Barplot text value

#Show it  
gg_asv_cluster


#Save to plot_path
ggsave(filename="Cluster_ASV_Count.png", plot=gg_asv_cluster, path=paste0(plot_path, "00_Appendix/01_Kingdom_Comparison/Values"))