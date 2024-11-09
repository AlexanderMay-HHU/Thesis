##Loading Libraries
library(ggplot2)
library(dplyr)
library(patchwork)

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
colorblind_palette <- palette.colors("Okabe-Ito",n=4)[2:4]



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA


#Sum of Abundance from different Kingdoms
nodes_sum_cluster <- nodes %>% group_by(Kingdom,LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence))
nodes_sum_cluster <- nodes_sum_cluster %>% group_by(LouvainLabelD) %>% mutate(Freq = ASVs/sum(ASVs))

#Sum of different Kingdoms in total
nodes_sum_total <- nodes %>% group_by(Kingdom) %>% summarise(ASVs = n_distinct(Sequence))
nodes_sum_total$LouvainLabelD <- c("Total")
nodes_sum_total <- nodes_sum_total %>% mutate(Freq = ASVs/sum(ASVs))




### Totals
## Generate Barplots
#Count numbers & Percentage of Total
gg_distribution_count_of_total <- ggplot(nodes_sum_total,
                                         aes(fill=Kingdom,
                                             y=ASVs,
                                             x=LouvainLabelD,
                                             label=paste0(ASVs,"\n(",scales::label_percent(accuracy=0.1)(Freq),")"))) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Total",y="# of ASVs")+
  geom_text(inherit.aes = T,size = 5,position = position_stack(0.5))+
  scale_x_discrete(label = c(""))#+
  #scale_fill_discrete(type=colorblind_palette)

#Show it
gg_distribution_count_of_total

#Save to plot_path
ggsave(filename="Total_Count_Percentage.png", plot=gg_distribution_count_of_total,
       path=paste(plot_path,"/Overview/Kingdom_Distribution",sep=""))




###Clusters
#Calculation
#average cluster size
avg_per_cluster = sum(nodes_sum_cluster$ASVs)/n_distinct(nodes_sum_cluster$LouvainLabelD)

#% of Kingdoms from total
percent_kingdoms_total <- c(nodes_sum_total$Freq[nodes_sum_total$Kingdom == "Eukaryota"],
  nodes_sum_total$Freq[nodes_sum_total$Kingdom == "Bacteria"],
  nodes_sum_total$Freq[nodes_sum_total$Kingdom == "Archaea"])


## Generate Barplots
#Count numbers
gg_distribution_count <- ggplot(nodes_sum_cluster,
                                aes(fill=Kingdom,
                                    y=ASVs,
                                    x=LouvainLabelD,
                                    label=ASVs)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Louvain Cluster",y="# of ASVs",linetype="Average")+
  geom_text(check_overlap=F,size = 4,position = position_stack(0.5))+
  geom_hline(aes(yintercept = avg_per_cluster, linetype = ""), color = "black") +
  scale_linetype_manual(values = "dashed") + 
  scale_x_continuous(breaks = c(0,2:13))+
  theme(legend.key.size = unit(0.65, units = "cm"))#+
  #scale_fill_discrete(type=colorblind_palette)

#Percentage
gg_distribution_percent <- ggplot(nodes_sum_cluster,
                                  aes(fill=Kingdom,
                                      y=Freq,
                                      x=LouvainLabelD,
                                      label=round(Freq,1))) + 
  geom_bar(position="dodge", stat="identity")+
  labs(x="Louvain Cluster",y="% of clusters ASV",linetype="Total")+
  geom_hline(aes(yintercept=percent_kingdoms_total[1], linetype = "Eukaryota"),
             color = "#619CFF",size=0.75)+
  geom_hline(aes(yintercept=percent_kingdoms_total[2], linetype = "Bacteria"),
             color = "#00BA38",size=0.75)+
  geom_hline(aes(yintercept=percent_kingdoms_total[3], linetype = "Archaea"),
             color = "#F8766D",size=0.75)+
  scale_linetype_manual(values=c("dashed","dashed","dashed")) +
  scale_x_continuous(breaks = c(0,2:13))+
  scale_y_continuous(labels = scales::label_percent(suffix=""),breaks=seq(0,1,by=0.1))+
  theme(legend.key.size = unit(0.65, units = "cm"))#+
#scale_fill_discrete(type=colorblind_palette)

#Show generated barplots
gg_distribution_count
gg_distribution_percent


#Save barplots to plot_path
ggsave(filename="Cluster_Count.png", plot=gg_distribution_count, path=paste(plot_path,"/Appendix/Kingdom_Distribution",sep=""))
ggsave(filename="Cluster_Percent.png", plot=gg_distribution_percent, path=paste(plot_path,"/Overview/Kingdom_Distribution",sep=""))



## Combine Plots
gg_distribution_combined <- (gg_distribution_count) / (gg_distribution_percent) +
  plot_annotation(tag_levels = 'A')
gg_distribution_combined
ggsave(filename="Cluster_Combined.png", plot=gg_distribution_combined, path=paste(plot_path,"/Appendix/Kingdom_Distribution",sep="")) 