##Loading Libraries
library(ggplot2)
library(dplyr)
library(patchwork)


#Set Working Directory
project_folder <- dirname(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data"))


colorblind_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                        "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                        "#009e73","#f0e442","#0072b2","#d55e00",
                        "#999999")



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_bac <- subset(nodes,nodes$Kingdom == "Bacteria")


# Remove Unknown ASVs
nodes_bac_noUnknown_family <- nodes_bac[which(!(is.na(nodes_bac$Family))),]
nodes_bac_noUnknown_family <- nodes_bac_noUnknown_family[nodes_bac_noUnknown_family$Family != "Unknown_Family",]

nodes_bac_unknown_family <- setdiff(nodes_bac,nodes_bac_noUnknown_family)



# Cluster Size comparison
bac_cluster <- nodes_bac_noUnknown_family %>% group_by(LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence), Abundance = sum(Abundance4y))


# How many Unique Families are there?
cat("There are # different\n","Family:",length(unique(nodes_bac$Family)),"\n")





## Family
# All Families per Cluster
head(nodes_bac_noUnknown_family)[,c("Abundance4y","Family")]
bac_family_overall <- nodes_bac_noUnknown_family %>% group_by(Family) %>% summarize(total_Abundance = sum(Abundance4y))
bac_family_counts <- nodes_bac_noUnknown_family %>% group_by(Family) %>% summarize(Count = n())
bac_family_overall <- bac_family_overall %>% left_join(bac_family_counts, by = "Family")
#bac_family_overall <- bac_family_overall %>% add_row(Family = "Other",
                                                     #total_Abundance = sum(nodes_bac_unknown_family$Abundance4y),
                                                     #Count = n_distinct(nodes_bac_unknown_family$Sequence))
bac_family_overall <- bac_family_overall %>% mutate(Freq = total_Abundance/sum(total_Abundance))
bac_family_overall <- bac_family_overall %>% arrange(total_Abundance)

# Top 10 Overall only
bac_family_top10_overall <- tail(bac_family_overall,10)
bac_family_top10_overall <- bac_family_top10_overall %>% mutate(Freq = total_Abundance/sum(total_Abundance))


# Top 10 by Cluster
bac_family_top10 <- subset(nodes_bac_noUnknown_family, nodes_bac_noUnknown_family$Family %in% bac_family_top10_overall$Family)
bac_family_top10 <- bac_family_top10 %>% group_by(Family,LouvainLabelD) %>% summarize(Abundance = sum(Abundance4y))
bac_family_top10 <- bac_family_top10 %>% group_by(LouvainLabelD) %>% mutate(FreqInCluster = Abundance/sum(Abundance))
bac_family_top10 <- bac_family_top10 %>% arrange(LouvainLabelD,FreqInCluster)
bac_family_top10


# Anteile
sum(nodes_bac$Abundance4y)
sum(nodes_bac_noUnknown_family$Abundance4y)
sum(bac_family_top10_overall$total_Abundance)
clustersize_top10 <- bac_family_top10 %>% group_by(LouvainLabelD) %>% summarize(Abundance = sum(Abundance))
clustersize_top10
sum(bac_family_top10_overall$total_Abundance)/sum(nodes_bac$Abundance4y)



### Generate Stacked Barplot
## Family
# Size Comparison
gg_bac_clustersize <- ggplot(bac_cluster,
                             aes(x=as.factor(LouvainLabelD),
                                 y=Abundance,
                                 fill=factor(LouvainLabelD),
                                 label=paste0(round(Abundance,1),"\n(",ASVs,")")))+
  geom_bar(stat="identity")+
  labs(x="Louvain Cluster", y="Abundance")+
  theme(legend.key.size = unit(0.65, units = "cm"),
        legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+ #Legend size adjustments removing legend here
  scale_y_continuous(limits = c(0,max(bac_cluster$Abundance*1.2)))+
  scale_fill_manual(values=colorblind_palette)+
  geom_text(inherit.aes = T,size = 4,position = position_dodge(0.9),vjust=-0.25)
                

# Show it
gg_bac_clustersize+
  labs(title= "Bacteria Clustersize Comparison")
# Save to plot_path
ggsave(filename="ClusterSize.png", plot=gg_bac_clustersize +
                                            labs(title= "Bacteria Clustersize Comparison"),
       path=paste(plot_path,"/00_Appendix/02_Top/Bacteria",sep=""))



# Overall
gg_family_overall <- ggplot(bac_family_top10_overall,
                                  aes(fill=Family,
                                      y=Freq,
                                      x=Family,
                                      label=paste0(round(Freq,4)*100,"%\n(",Count,")"))) + 
  geom_bar(position="dodge", stat="identity")+ # Cluster Kingdoms Barplot
  scale_fill_manual(values= colorblind_palette[0:-1])+
  labs(x="Family",y="% of total identified bacteria ASVs")+ # Axis labels
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(labels = scales::label_percent(suffix=""),
                     breaks=seq(0,1,by=0.1),
                     limits = c(0,max(bac_family_top10_overall$Freq*1.2)))+ #Y-Axis values
  theme(legend.key.size = unit(0.65, units = "cm"),
        legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+ #Legend size adjustments removing legend here
  geom_text(inherit.aes = T,size = 4,position = position_dodge(0.9),vjust=-0.25) #Barplot text value

# Show it
gg_family_overall+
  labs(title = "Bacteria families overall")
# Save to plot_path
ggsave(filename="Family_Overall.png", plot=gg_family_overall +
                                              labs(title = "Bacteria families overall"),
       path=paste(plot_path,"/02_Top/Bacteria",sep=""))



# Cluster (by Percentage)
gg_top_family <- ggplot(bac_family_top10,
                          aes(x=as.factor(LouvainLabelD),
                              y=FreqInCluster,
                              fill=Family)) + 
                        geom_bar(stat="identity")+
                        labs(x="Louvain Cluster", y="% of total abundance of top identified families")+
                        scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
                        scale_fill_discrete(type=colorblind_palette[0:-1])+
                        theme(panel.grid.major.x = element_blank(),
                              panel.grid.minor.x = element_blank())
#Show it
gg_top_family+
  labs(title= "Bacteria Families")
#Save to plot_path
ggsave(filename="Family_Cluster_Percent.png", plot=gg_top_family +
                                                      labs(title= "Bacteria Families"),
       path=paste(plot_path,"/00_Appendix/02_Top/Bacteria",sep=""))



# /w numbers
gg_top_family_numbers <- gg_top_family+
                          geom_text(label= ifelse(bac_family_top10$FreqInCluster>=0.02,round(bac_family_top10$FreqInCluster,4)*100,""),
                                    size = 4, position = position_stack(vjust = 0.5))
#Show it
gg_top_family_numbers+
  labs(title = "Bacteria families overall")
#Save to plot_path
ggsave(filename="Family_Cluster_Percent_Numbers.png", plot=gg_top_family_numbers +
                                                              labs(title = "Bacteria families overall"),
       path=paste(plot_path,"/00_Appendix/02_Top/Bacteria",sep=""))



tail(bac_family_top10 %>% summarise(tot_Abdundance = sum(Abundance)) %>% arrange(tot_Abdundance),10)

# Cluster (by Abundance)
gg_top_family_abundance <- ggplot(bac_family_top10,
                        aes(x=as.factor(LouvainLabelD),
                            y=Abundance,
                            fill=Family)) + 
  geom_bar(stat="identity")+
  labs(x="Louvain Cluster", y="total abundance of top identified families")+
  theme(legend.key.size = unit(0.65, units = "cm"),
        legend.position="none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_fill_discrete(type=colorblind_palette[0:-1])
#Show it
gg_top_family_abundance+
  theme(legend.position="right")+
  labs(title= "Bacteria Families")
#Save to plot_path 
ggsave(filename="Family_Cluster_Abundance.png", plot=gg_top_family_abundance +
                                                          theme(legend.position="right")+
                                                          labs(title= "Bacteria Families"),
       path=paste(plot_path,"/00_Appendix/02_Top/Bacteria",sep=""))



# Combined Cluster Plots
combined_plots <- gg_top_family_abundance | gg_top_family
combined_plots <- combined_plots + plot_annotation(tag_levels = 'A')
combined_plots
ggsave(filename="Family_Cluster_Combined.png", plot=combined_plots, path=paste(plot_path,"/02_Top/Bacteria",sep=""))