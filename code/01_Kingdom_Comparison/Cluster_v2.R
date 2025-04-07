##Loading Libraries
library(ggplot2)
library(dplyr)

#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/generated"))

# Import data
abundance_louv <- read.csv("abundance_overview.csv", header=TRUE)

# Number of not clustered asvs 
print(paste("Number of not clustered ASVs:",sum(is.na(abundance_louv$LouvainLabelD))))
# highest abundance of non clustered ASV
max(subset(abundance_louv$Abundance_total,is.na(abundance_louv$LouvainLabelD)))

#Replace Environment_Condition with NA
abundance_louv[abundance_louv == "Environment_Condition"] <- NA



#Sum of Abundance from different Kingdoms
overall_cluster <- abundance_louv %>% group_by(Kingdom,LouvainLabelD) %>% summarise(ASVs = n_distinct(OTU.number), Abundance = sum(Abundance_total))
overall_cluster <- overall_cluster %>% group_by(LouvainLabelD) %>% mutate(Freq = ASVs/sum(ASVs))

#ASVs per cluster
ASV_count <- abundance_louv %>% group_by(LouvainLabelD) %>% summarise(ASVs = n_distinct(OTU.number))
# Remove 1 as its not a cluster (NA)
avg_asv_per_cluster <-  sum(ASV_count[1:15,"ASVs"])/length(ASV_count$ASVs)-1


#Sum of different Kingdoms in total
overall_sum_total <- abundance_louv %>% group_by(Kingdom) %>% summarise(ASVs = n_distinct(OTU.number))
overall_sum_total$LouvainLabelD <- c("Total")
overall_sum_total <- overall_sum_total %>% mutate(Freq = ASVs/sum(ASVs))

#% of Kingdoms from total
percent_kingdoms_total <- c(overall_sum_total$Freq[overall_sum_total$Kingdom == "Eukaryota"],
  overall_sum_total$Freq[overall_sum_total$Kingdom == "Bacteria"],
  overall_sum_total$Freq[overall_sum_total$Kingdom == "Archaea"])


mean(t(overall_cluster[overall_cluster$Kingdom == "Archaea", "Freq"]))

## Generate Barplots
# ASV Count by Kingdom & Cluster
gg_distribution_count <- ggplot(overall_cluster,
                                aes(x=factor(LouvainLabelD),
                                    y=ASVs,
                                    fill=Kingdom)) + 
  # Stacked Bars
  geom_bar(position="stack", stat="identity",color="black",linewidth=0.5)+
  # Color bars based on Kingdom
  scale_fill_manual(values = c("Eukaryota" = "#1f77b4",  
                               "Bacteria" = "#2ca02c", 
                               "Archaea" = "#d62728"),
                    labels = c("Archaea", "Bacteria", "Eukaryota")) +
  # Axis and other Labeling
  labs(x="Louvain Cluster",y="ASVs",title = "ASV count of Kingdoms by cluster")+
  # Label bars with values
  geom_text(aes(label = ASVs),  
            position = position_stack(vjust = 0.5),  #place in middle of stacked bar
            color = ifelse(overall_cluster$ASVs>2,"white","black"), size = 5, fontface = "bold")+ # White bold text for any but the small clusters
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),  
      axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5), 
      axis.text.y = element_text(size = 16),  
      legend.title = element_text(face = "bold"),  
      legend.position = "top",  
      panel.grid.major = element_line(color = "gray"),  #gray grid
      panel.grid.minor = element_blank()  
    )

# Show it
gg_distribution_count

#Save to plot_path
ggsave(filename="Cluster_Count.pdf", plot=gg_distribution_count, path=paste0(plot_path, "01_Kingdom_Comparison"),width=12,height=9)




# Percentage of Abundance by cluster
gg_distribution_percent <- ggplot(overall_cluster,
                                  aes(x=factor(LouvainLabelD),
                                      y=Freq,
                                      fill=Kingdom)) + 
  # Stacked Bars
  geom_bar(position="stack", stat="identity",color="black",linewidth=0.5)+
  # Color bars based on Kingdom
  scale_fill_manual(values = c("Eukaryota" = "#1f77b4",  
                               "Bacteria" = "#2ca02c", 
                               "Archaea" = "#d62728"),
                    labels = c("Archaea", "Bacteria", "Eukaryota")) +
  # Axis and other Labeling
  labs(x="Louvain Cluster",y="Percentage [%]",title = "Percentage of Kingdoms by cluster")+
  # Label bars with values
  geom_text(aes(label = round(Freq*100,1)),  
            position = position_stack(vjust = 0.5),  #place in middle of stacked bar
            color = "white", size = 5, fontface = "bold")+ # White bold text as the small clusters are made up by one group
  scale_y_continuous(labels = scales::label_percent(suffix=""),breaks=seq(0,1,by=0.1))+#Y-Axis values
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),  
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5), 
    axis.text.y = element_text(size = 16),  
    legend.title = element_text(face = "bold"),  
    legend.position = "top",  
    panel.grid.major = element_line(color = "gray"),  #gray grid
    panel.grid.minor = element_blank()  
  )


# Show it
gg_distribution_percent

#Save to plot_path
ggsave(filename="Cluster_Percent.pdf", plot=gg_distribution_percent, path=paste0(plot_path, "01_Kingdom_Comparison"))