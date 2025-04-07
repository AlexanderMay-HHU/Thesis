##Loading Libraries
library(ggplot2)
library(dplyr)


#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/generated"))


abundance_louv_orig <- read.csv("abundance_overview.csv", header=TRUE)
#Replace Environment_Condition with NA
abundance_louv_orig[abundance_louv_orig == "Environment_Condition"] <- NA
# Remove empty ASVs with missing Phyla or Cluster
abundance_louv <- abundance_louv_orig %>% filter(Phylum != "",!is.na(Phylum), !is.na(LouvainLabelD))


# Get total abundances of Phyla per Cluster
abundance_phyla <- abundance_louv %>% group_by(Phylum, LouvainLabelD) %>%
                    summarize(Abundance_total = sum(Abundance_total, na.rm = TRUE)) %>% ungroup()


# Top 5 Phyla + combined remaining Phyla
top_5_and_remaining <- abundance_phyla %>% group_by(LouvainLabelD) %>%
                        mutate(rank = rank(-Abundance_total, ties.method = "first")) %>% # Rank all Phyla of each cluster
                        mutate(Phylum = ifelse(rank > 5, "Other", Phylum)) %>% # Rename Phyla of non Top 5 to Other to sum them later
                        group_by(LouvainLabelD, Phylum) %>%
                        summarise(Abundance_total = sum(Abundance_total)) %>% ungroup() %>%
                        group_by(LouvainLabelD) %>% mutate(Freq = Abundance_total/sum(Abundance_total)) %>% ungroup() # Calculate Percentage


# Show all unique Phyla
unique(top_5_and_remaining$Phylum)

# Green = Autotroph | Orange = Mixotroph | Purple = Heterotroph
color_phyla <- c("Bacteroidetes" = "mediumpurple",
                 "Cyanobacteria" = "springgreen",
                 "Dinoflagellata" = "orange",
                 "Ochrophyta" = "chartreuse3",
                 "Proteobacteria" = "orange3",
                 "Chlorophyta" = "forestgreen",
                 "Haptophyta" = "lawngreen",
                 "Thaumarchaeota" = "darkgreen",
                 "Euryarchaeota" = "purple",
                 "Other" = "#999999"
)

# Sort Top 5 Phyla alphabetically & put Other at last position for display reasons
top_5_and_remaining$Phylum <- factor(top_5_and_remaining$Phylum,
                                     levels = c("Thaumarchaeota","Chlorophyta","Ochrophyta","Cyanobacteria","Haptophyta",
                                                "Proteobacteria","Dinoflagellata",
                                                "Bacteroidetes","Euryarchaeota",
                                                "Other"))

top5_nsc <- subset(top_5_and_remaining, top_5_and_remaining$LouvainLabelD != 1 & top_5_and_remaining$LouvainLabelD != 14)
 

summary_by_phlya <- top5_nsc %>%
  group_by(Phylum) %>%
  summarize(
    Min = min(Freq*100, na.rm = TRUE),
    Q1 = quantile(Freq*100, 0.25, na.rm = TRUE),
    Median = median(Freq*100, na.rm = TRUE),
    Mean = mean(Freq*100, na.rm = TRUE),
    Q3 = quantile(Freq*100, 0.75, na.rm = TRUE),
    Max = max(Freq*100, na.rm = TRUE),
    Cluster_count = sum(!is.na(LouvainLabelD))
  )


top5_nsc$Trophic_strat <- case_when(
  top5_nsc$Phylum == "Thaumarchaeota" ~ factor("Auto", levels= c("Auto","Mixo", "Hetero", "Not assigned")), 
  top5_nsc$Phylum == "Chlorophyta" ~ factor("Auto", levels= c("Auto","Mixo", "Hetero", "Not assigned")),
  top5_nsc$Phylum == "Ochrophyta" ~ factor("Auto", levels= c("Auto","Mixo", "Hetero", "Not assigned")), 
  top5_nsc$Phylum == "Cyanobacteria" ~ factor("Auto", levels= c("Auto","Mixo", "Hetero", "Not assigned")),  
  top5_nsc$Phylum == "Haptophyta" ~ factor("Auto", levels= c("Auto","Mixo", "Hetero", "Not assigned")),
  top5_nsc$Phylum == "Proteobacteria" ~ factor("Mixo", levels= c("Auto","Mixo", "Hetero", "Not assigned")),  
  top5_nsc$Phylum == "Dinoflagellata" ~ factor("Mixo", levels= c("Auto","Mixo", "Hetero", "Not assigned")),
  top5_nsc$Phylum == "Bacteroidetes" ~ factor("Hetero", levels= c("Auto","Mixo", "Hetero", "Not assigned")),
  top5_nsc$Phylum == "Euryarchaeota" ~ factor("Hetero", levels= c("Auto","Mixo", "Hetero", "Not assigned")),
  top5_nsc$Phylum == "Other" ~ factor("Not assigned", levels= c("Auto","Mixo", "Hetero", "Not assigned")))


summary_by_trophic <- top5_nsc %>%
  group_by(LouvainLabelD, Trophic_strat) %>%
  summarize(
    Sum = sum(Freq*100, na.rm = TRUE),
    Cluster_count = sum(!is.na(LouvainLabelD))) %>% ungroup %>% 
    group_by(LouvainLabelD) %>%
    mutate(Freq = Sum/sum(Sum)) %>% ungroup

for(clu in unique(summary_by_trophic$LouvainLabelD)){
  print(paste0("Cluster[",clu, "] - ",summary_by_trophic[summary_by_trophic$Sum == max(summary_by_trophic2[summary_by_trophic$LouvainLabelD == clu, "Sum"]), "Trophic_strat"],
               " with ", round(max(summary_by_trophic[summary_by_trophic$LouvainLabelD == clu, "Sum"]),1)))
} 



##Generate Stacked Barplot
#Family (This is going to be used)
# Overall
gg_phyla <- ggplot(top_5_and_remaining,
                            aes(x=as.factor(LouvainLabelD),
                                y=Freq,
                                fill=Phylum)) + 
  # Stacks Bar plot
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.5) +  
  # Coloring based on Trophic strategy
  scale_fill_manual(values = color_phyla) +  
  # Axis titles
  labs(x = "Louvain Cluster", y = "Percentage of Total Abundance [%]", 
       title = "Top 5 Phyla by Relative Abundance in their Cluster", 
       fill = "Phylum") +  
  scale_y_continuous(labels = scales::label_percent(suffix=""),breaks=seq(0,1,by=0.1))+#Y-Axis values
  guides(fill=guide_legend(nrow=3))+
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 20),  
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 18, face = "bold"),  
    axis.title.y = element_text(size = 18, face = "bold"),  
    panel.grid.major = element_line(color = "grey"),  
    panel.grid.minor = element_blank(),  
    legend.position = "bottom",  
    legend.direction = "horizontal",  
    legend.justification = "center",  
    legend.text = element_text(size = 15)
  )
  

gg_phyla


gg_phyla_numbers <- gg_phyla +
  # Label for each Bar
  geom_text(aes(label = paste0(round(Freq*100,1),"%")), 
            position = position_stack(vjust = 0.5), 
            color = ifelse(top_5_and_remaining$Phylum != "Cyanobacteria" & top_5_and_remaining$Phylum != "Haptophyta" &
                           top_5_and_remaining$Phylum != "Ochrophyta","white","black"),
            size = 4, fontface = "bold")

gg_phyla_numbers

ggsave(filename="Phyla_Overall.pdf", plot=gg_phyla, path=paste(plot_path,"02_Top/",sep=""))
ggsave(filename="Phyla_Overall_numbers.pdf", plot=gg_phyla_numbers, path=paste(plot_path,"00_Appendix/02_Top/",sep=""))



# Trophic strategy
gg_trophic <- ggplot(summary_by_trophic,
                     aes(x=as.factor(LouvainLabelD),
                         y=Freq,
                         fill=Trophic_strat)) + 
  # Stacks Bar plot
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.5)+
  # Coloring based on Trophic strategy
  scale_fill_manual(values = c("Auto" = "darkgreen","Mixo" = "orange","Hetero" = "purple", "Not assigned" = "#999999")) +  
    # Axis titles
    labs(x = "Louvain Cluster", y = "Percentage of Total Abundance [%]", 
         title = "Trophic strategies per cluster", 
         fill = "Trophic strategy") +  
    scale_y_continuous(labels = scales::label_percent(suffix=""),breaks=seq(0,1,by=0.1))+#Y-Axis values
    theme_minimal(base_size = 18) +  
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20),  
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
      axis.text.y = element_text(size = 16),  
      axis.title.x = element_text(size = 18, face = "bold"),  
      axis.title.y = element_text(size = 18, face = "bold"),  
      panel.grid.major = element_line(color = "grey"),  
      panel.grid.minor = element_blank(),  
      legend.position = "top",  
      legend.direction = "horizontal",  
      legend.justification = "center",  
      legend.text = element_text(size = 15)
    )

gg_trophic



gg_trophic_numbers <- gg_trophic +
  # Label for each Bar
  geom_text(aes(label = paste0(round(Freq*100,1),"%")), 
            position = position_stack(vjust = 0.5), 
            color = ifelse(summary_by_trophic$Trophic_strat =="Mixo", "black", "white"),
            size = 4, fontface = "bold")

gg_trophic_numbers



ggsave(filename="Trophic_Overall.pdf", plot=gg_trophic, path=paste(plot_path,"00_Appendix/02_Top/",sep=""),width =10, height =8)
ggsave(filename="Trophic_Overall_numbers.pdf", plot=gg_trophic_numbers, path=paste(plot_path,"00_Appendix/02_Top/",sep=""),width =10, height =8)