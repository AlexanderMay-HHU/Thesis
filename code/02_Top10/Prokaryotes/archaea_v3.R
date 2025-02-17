##Loading Libraries
library(ggplot2)
library(dplyr)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
colorblind_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                        "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                        "#009e73","#f0e442","#0072b2","#d55e00",
                        "#999999")



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_arc <- subset(nodes,nodes$Kingdom == "Archaea")


# Remove Unknown ASVs
nodes_arc_noUnknown_family <- nodes_arc[which(!(is.na(nodes_arc$Family))),]
nodes_arc_noUnknown_family <- nodes_arc_noUnknown_family[nodes_arc_noUnknown_family$Family != "Unknown_Family",]



# Cluster Size comparison
arc_cluster <- nodes_arc %>% group_by(LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence), Abundance = sum(Abundance4y))


# How many Unique Families are there?
cat("There are # different\n","Family:",length(unique(nodes_arc$Family)),"\n")




## Family
# All Families per Cluster
head(nodes_arc_noUnknown_family)[,c("Abundance4y","Family")]
arc_family <- nodes_arc_noUnknown_family %>% group_by(Family,LouvainLabelD) %>% summarize(Abundance = sum(Abundance4y))
arc_family <- arc_family %>% group_by(LouvainLabelD) %>% mutate(FreqInCluster = Abundance/sum(Abundance))
arc_family <- arc_family %>% arrange(LouvainLabelD)
arc_family


# Families overall
arc_family_overall <- arc_family %>% group_by(Family) %>% summarize(total_Abundance = sum(Abundance))
arc_family_overall <- arc_family_overall %>% mutate(Freq = total_Abundance/sum(total_Abundance))
arc_family_overall$Count <- nodes_arc_noUnknown_family %>% group_by(Family) %>% summarise(Count = n()) %>% select(Count)
arc_family_overall


# Anteile
sum(nodes_arc$Abundance4y)
sum(nodes_arc_noUnknown_family$Abundance4y)
sum(arc_family_overall$total_Abundance)

sum(arc_family_overall$total_Abundance)/sum(nodes_arc$Abundance4y)



### Generate Stacked Barplot
## Family
# Size Comparison
gg_arc_clustersize <- ggplot(arc_cluster,
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
       title= "Archaea Clustersize Comparison")+
  theme(legend.key.size = unit(0.65, units = "cm"), legend.position="none")+ #Legend size adjustments removing legend here
  scale_x_continuous(breaks = c(0,2:13))+
  scale_y_continuous(limits = c(0,7000))+
  scale_fill_manual(values=colorblind_palette)+
  geom_text(inherit.aes = T,size = 4,position = position_dodge(0.9),vjust=-0.25)
                

# Show it
gg_arc_clustersize
# Save to plot_path
ggsave(filename="ClusterSize.png", plot=gg_arc_clustersize, path=paste(plot_path,"/02_Top/Archaea",sep=""))



# Overall
gg_family_overall <- ggplot(arc_family_overall,
                                  aes(fill=Family,
                                      y=Freq,
                                      x=Family,
                                      label=paste0(round(Freq,5)*100,"%\n(",Count$Count,")"))) + 
  geom_bar(position="dodge", stat="identity")+ # Cluster Kingdoms Barplot
  scale_fill_manual(values= colorblind_palette[0:-1])+
  labs(x="Family",y="% of total identifies archaea ASVs",
       title = "Archaea families overall")+ # Axis labels
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(labels = scales::label_percent(suffix=""),breaks=seq(0,1,by=0.1))+#Y-Axis values
  theme(legend.key.size = unit(0.65, units = "cm"), legend.position="none")+ #Legend size adjustments removing legend here
  geom_text(inherit.aes = T,size = 4,position = position_dodge(0.9),vjust=0.5) #Barplot text value

# Show it
gg_family_overall
# Save to plot_path
ggsave(filename="Family_Overall.png", plot=gg_family_overall, path=paste(plot_path,"/02_Top/Archaea",sep=""))



# Cluster
gg_top_family <- ggplot(arc_family,
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
                             title= "Archaea Families")+
                        scale_x_continuous(breaks = c(0,2:13))+
                        scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
                        scale_fill_discrete(type=colorblind_palette[0:-1])
#Show it
gg_top_family
#Save to plot_path
ggsave(filename="Family_Cluster_Percentage.png", plot=gg_top_family, path=paste(plot_path,"/02_Top/Archaea/",sep=""))



# /w numbers
gg_top_family_numbers <- gg_top_family+
                          geom_text(label= round(arc_family$FreqInCluster,4)*100,
                                    size = 4, position = position_stack(vjust = 0.5))
#Show it
gg_top_family_numbers
#Save to plot_path
ggsave(filename="Family_Cluster_Percentage_Numbers.png", plot=gg_top_family_numbers, path=paste(plot_path,"/02_Top/Archaea/",sep=""))



# Cluster (by Abundance)
gg_top_family_abundance <- ggplot(arc_family,
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
       title= "Archaea Families")+
  scale_x_continuous(breaks = c(0,2:13))+
  scale_fill_discrete(type=colorblind_palette[0:-1])
#Show it
gg_top_family_abundance
#Save to plot_path
ggsave(filename="Family_Cluster_Abundance.png", plot=gg_top_family_abundance, path=paste(plot_path,"/02_Top/Archaea",sep=""))