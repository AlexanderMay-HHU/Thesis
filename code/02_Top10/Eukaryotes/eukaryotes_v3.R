##Loading Libraries
library(ggplot2)
library(dplyr)
library(patchwork)


#Set Working Directory
setwd("R:/Studium/bachelor/Thesis/data")
plot_path <- "R:/Studium/bachelor/Thesis/generated_plots"
colorblind_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                        "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                        "#009e73","#f0e442","#0072b2","#d55e00",
                        "#999999")



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_euk <- subset(nodes,nodes$Kingdom == "Eukaryota")


# Remove Unknown ASVs
nodes_euk_noUnknown_family <- nodes_euk[which(!(is.na(nodes_euk$Family))),]
nodes_euk_noUnknown_family <- nodes_euk_noUnknown_family[nodes_euk_noUnknown_family$Family != "Unknown_Family",]

nodes_euk_unknown_family <- setdiff(nodes_euk,nodes_euk_noUnknown_family)



# Cluster Size comparison
euk_cluster <- nodes_euk %>% group_by(LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence), Abundance = sum(Abundance4y))


# How many Unique Families are there?
cat("There are # different\n","Family:",length(unique(nodes_euk$Family)),"\n")





## Family
# All Families per Cluster
head(nodes_euk_noUnknown_family)[,c("Abundance4y","Family")]
euk_family_overall <- nodes_euk_noUnknown_family %>% group_by(Family) %>% summarize(total_Abundance = sum(Abundance4y))
euk_family_counts <- nodes_euk_noUnknown_family %>% group_by(Family) %>% summarize(Count = n())
euk_family_overall <- euk_family_overall %>% left_join(euk_family_counts, by = "Family")
#euk_family_overall <- euk_family_overall %>% add_row(Family = "Other",
                                                     #total_Abundance = sum(nodes_euk_unknown_family$Abundance4y),
                                                     #Count = n_distinct(nodes_euk_unknown_family$Sequence))
euk_family_overall <- euk_family_overall %>% mutate(Freq = total_Abundance/sum(total_Abundance))
euk_family_overall <- euk_family_overall %>% arrange(total_Abundance)

# Top 10 Overall only
euk_family_top10_overall <- tail(euk_family_overall,11)
euk_family_top10_overall <- euk_family_top10_overall %>% mutate(Freq = total_Abundance/sum(total_Abundance))


# Top 10 by Cluster
euk_family_top10 <- subset(nodes_euk_noUnknown_family, nodes_euk_noUnknown_family$Family %in% euk_family_top10_overall$Family)
euk_family_top10 <- euk_family_top10 %>% group_by(Family,LouvainLabelD) %>% summarize(Abundance = sum(Abundance4y))
euk_family_top10 <- euk_family_top10 %>% group_by(LouvainLabelD) %>% mutate(FreqInCluster = Abundance/sum(Abundance))
euk_family_top10 <- euk_family_top10 %>% arrange(LouvainLabelD)
euk_family_top10


# Anteile
sum(nodes_euk$Abundance4y)
sum(nodes_euk_noUnknown_family$Abundance4y)
sum(euk_family_overall$total_Abundance)

sum(euk_family_overall$total_Abundance)/sum(nodes_euk$Abundance4y)



### Generate Stacked Barplot
## Family
# Size Comparison
gg_euk_clustersize <- ggplot(euk_cluster,
                             aes(x=LouvainLabelD,
                                 y=Abundance,
                                 fill=factor(LouvainLabelD),
                                 label=paste0(round(Abundance,1),"\n(",ASVs,")")))+
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
  labs(x="Louvain Cluster", y="Abundance",
       title= "Eukaryota Clustersize Comparison")+
  theme(legend.key.size = unit(0.65, units = "cm"), legend.position="none")+ #Legend size adjustments removing legend here
  scale_x_continuous(breaks = c(0,2:13))+
  scale_y_continuous(limits = c(0,max(euk_cluster$Abundance*1.2)))+
  scale_fill_manual(values=colorblind_palette)+
  geom_text(inherit.aes = T,size = 4,position = position_dodge(0.9),vjust=-0.25)
                

# Show it
gg_euk_clustersize
# Save to plot_path
ggsave(filename="ClusterSize.png", plot=gg_euk_clustersize, path=paste(plot_path,"/02_Top/Eukaryota",sep=""))



# Overall
gg_family_overall <- ggplot(euk_family_top10_overall,
                                  aes(fill=Family,
                                      y=Freq,
                                      x=Family,
                                      label=paste0(round(Freq,4)*100,"%\n(",Count,")"))) + 
  geom_bar(position="dodge", stat="identity")+ # Cluster Kingdoms Barplot
  scale_fill_manual(values= colorblind_palette[0:-1])+
  labs(x="Family",y="% of total identifies archaea ASVs",
       title = "Eukaryota families overall")+ # Axis labels
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(labels = scales::label_percent(suffix=""),
                     breaks=seq(0,1,by=0.1),
                     limits = c(0,max(euk_family_top10_overall$Freq*1.2)))+ #Y-Axis values
  theme(legend.key.size = unit(0.65, units = "cm"), legend.position="none")+ #Legend size adjustments removing legend here
  geom_text(inherit.aes = T,size = 4,position = position_dodge(0.9),vjust=-0.25) #Barplot text value

# Show it
gg_family_overall
# Save to plot_path
ggsave(filename="Family_Overall.png", plot=gg_family_overall, path=paste(plot_path,"/02_Top/Eukaryota",sep=""))



# Cluster (by Percentage)
gg_top_family <- ggplot(euk_family_top10,
                          aes(x=LouvainLabelD,
                              y=FreqInCluster,
                              fill=Family)) + 
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
                        labs(x="Louvain Cluster", y="% of total abundance of top identified families",
                             title= "Eukaryota Families")+
                        scale_x_continuous(breaks = c(0,2:13))+
                        scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
                        scale_fill_discrete(type=colorblind_palette[0:-1])
#Show it
gg_top_family
#Save to plot_path
ggsave(filename="Family_Cluster_Percent.png", plot=gg_top_family, path=paste(plot_path,"/02_Top/Eukaryota",sep=""))



# /w numbers
gg_top_family_numbers <- gg_top_family+
                          geom_text(label= ifelse(euk_family_top10$FreqInCluster>=0.02,round(euk_family_top10$FreqInCluster,4)*100,""),
                                    size = 4, position = position_stack(vjust = 0.5))
#Show it
gg_top_family_numbers
#Save to plot_path
ggsave(filename="Family_Cluster_Percent_Numbers.png", plot=gg_top_family_numbers, path=paste(plot_path,"/02_Top/Eukaryota",sep=""))



tail(euk_family_top10 %>% summarise(tot_Abdundance = sum(Abundance)) %>% arrange(tot_Abdundance),10)

# Cluster (by Abundance)
gg_top_family_abundance <- ggplot(euk_family_top10,
                        aes(x=LouvainLabelD,
                            y=Abundance,
                            fill=Family)) + 
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
  labs(x="Louvain Cluster", y="total abundance of top identified families",
       title= "Eukaryota Families")+
  scale_x_continuous(breaks = c(0,2:13))+
  scale_fill_discrete(type=colorblind_palette[0:-1])
#Show it
gg_top_family_abundance
#Save to plot_path
ggsave(filename="Family_Cluster_Abundance.png", plot=gg_top_family_abundance, path=paste(plot_path,"/02_Top/Eukaryota",sep=""))