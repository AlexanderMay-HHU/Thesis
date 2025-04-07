## Loading Libraries
library(ggplot2)  #Used to plot
library(dplyr)
library(vegan)    #Used for Shannon & Simpson Diversities
library(fossil)   #used for Chao1 - Richness
library(grid)
library(gridExtra)

#Set Working Directory
project_folder <- dirname(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/generated"))



color_clusters <- c("#00ffff","#191970","#c71585","#2f4f4f",
                    "#ff0000","#0000ff","#bdb76b","#006400",
                    "#ffa500","#1e90ff","#ffb6c1","#00fa9a",
                    "#ffff00","#631919","#ff00ff","#00ff00")


## Get Data
abundance_louv_orig <- read.csv("abundance_overview.csv", header=TRUE)
abundance_table <- read.csv("abundance_asv_row_with_louvain_observations.csv", header=TRUE)
#Replace Environment_Condition with NA
abundance_louv_orig[abundance_louv_orig == "Environment_Condition"] <- NA


# Calculation of different alpha diversities per cluster
diversity_results <- abundance_louv_orig %>% group_by(LouvainLabelD) %>%
                        summarise(SpRichness = sum(Abundance_total > 0), #[Species-richness = S]
                                  Chao1 = chao1(Abundance_total), # [Chao = C]
                                  Simpson = diversity(Abundance_total, index = "simpson"), #Simpson [D]
                                  Shannon = diversity(Abundance_total, index = "shannon")) %>% #Shannon-Index [H]
                        mutate (Norm_Shannon = Shannon/log(SpRichness)) #Norm Shannon -> H/log-naturalis(S)

Norm_Shannon <- as.vector(subset(diversity_results,diversity_results$LouvainLabelD %in% c(0,2:13))$Norm_Shannon)
mean(Norm_Shannon)
Simpson <- as.vector(subset(diversity_results,diversity_results$LouvainLabelD %in% c(0,2:13))$Simpson)
mean(Simpson)

#t_test_result  <- t.test(Norm_Shannon[c(1:4,6:13)],mu =Norm_Shannon[5])
#print(t_test_result)

# Correlation excluding the two extreme clusters 1 & 14 which only have 2 ASVs to get a more general trend
print(paste0("Korrelation [Anzahl-Shannon]: ", round(cor(diversity_results[diversity_results$SpRichness > 2,"Chao1"],
                                                         diversity_results[diversity_results$SpRichness > 2,"Norm_Shannon"],method = "pearson"),3)))
print(paste0("Korrelation [Anzahl-Simpson]: ", round(cor(diversity_results[diversity_results$SpRichness > 2,"Chao1"],
                                                         diversity_results[diversity_results$SpRichness > 2,"Simpson"],method = "pearson"),3)))


## Generate Barplots
#Number of ASvs - Species Richness
gg_speciesrichness <- ggplot(diversity_results,
                             aes(x=as.factor(LouvainLabelD),
                                 y=SpRichness)) + 
  # Stacks Bar plot
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.5, fill = color_clusters) +  
  # Text labels of Bars
  geom_text(aes(label = SpRichness),  
            position = position_stack(vjust = 0.5),  
            color = ifelse(diversity_results$LouvainLabelD %in% c(1:0,2,4,6,8:12,14,NA),"black","white"),
            size = 6, fontface = "bold") +
  # Axis titles
  labs(x = "Louvain Cluster", y = "Number of ASVs") +  
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = -0.07, face = "bold", size = 40),  #Adjusted to make Title fit as Panel label
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 18, face = "bold"),  
    axis.title.y = element_text(size = 18, face = "bold"),  
    panel.grid.major = element_line(color = "grey"),  
    panel.grid.minor = element_blank(),  
    legend.position = "none")

# Show it
gg_speciesrichness

#Save it
ggsave(filename="Overall_SpRichness.pdf", plot=gg_speciesrichness,
       path=paste0(plot_path,"00_Appendix/03_Diversities/Alpha/"))



#Chao1 - Estimated Species Count per cluster by Chao1
gg_chao1 <- ggplot(diversity_results,
                             aes(x=as.factor(LouvainLabelD),
                                 y=Chao1)) + 
  # Stacks Bar plot
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.5, fill = color_clusters) + 
  # Text labels of Bars
  geom_text(aes(label = Chao1),  
            position = position_stack(vjust = 0.5),  
            color = ifelse(diversity_results$LouvainLabelD %in% c(1:0,2,4,6,8:12,14,NA),"black","white"),
            size = 6, fontface = "bold") +
  # Axis titles
  labs(x = "Louvain Cluster", y = "Chao1 Richness") +  
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = -0.07, face = "bold", size = 40),  #Adjusted to make Title fit as Panel label
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 18, face = "bold"),  
    axis.title.y = element_text(size = 18, face = "bold"),  
    panel.grid.major = element_line(color = "grey"),  
    panel.grid.minor = element_blank(),  
    legend.position = "none")

# Show it
gg_chao1

#Save it
ggsave(filename="Overall_Chao1.pdf", plot=gg_chao1,
       path=paste0(plot_path,"00_Appendix/03_Diversities/Alpha/"))



#Shannon - Diversity Index for sp-richness & evenness
gg_shannon <- ggplot(diversity_results,
                   aes(x=as.factor(LouvainLabelD),
                       y=Norm_Shannon)) + 
  # Stacks Bar plot
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.5, fill = color_clusters) + 
  # Text labels of Bars
  geom_text(aes(label = round(Norm_Shannon,2)),  
            position = position_stack(vjust = 0.5),  
            color = ifelse(diversity_results$LouvainLabelD %in% c(0,2,4,6,8:12,14,NA),"black","white"),
            size = 6, fontface = "bold") +
  # Axis titles
  labs(x = "Louvain Cluster", y = "Shannon Index") +  
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = -0.07, face = "bold", size = 40),  #Adjusted to make Title fit as Panel label
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 18, face = "bold"),  
    axis.title.y = element_text(size = 18, face = "bold"),  
    panel.grid.major = element_line(color = "grey"),  
    panel.grid.minor = element_blank(),  
    legend.position = "none")

# Show it
gg_shannon

#Save it
ggsave(filename="Overall_Shannon.pdf", plot=gg_shannon,
       path=paste0(plot_path,"00_Appendix/03_Diversities/Alpha/"))


#Simpson - Diversity Index for sp-richness & evenness with dominance of single species being accounted for
gg_simpson <- ggplot(diversity_results,
                   aes(x=as.factor(LouvainLabelD),
                       y=Simpson)) + 
  # Stacks Bar plot
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.5, fill = color_clusters) + 
  # Text labels of Bars
  geom_text(aes(label = round(Simpson,2)),  
            position = position_stack(vjust = 0.5),  
            color = ifelse(diversity_results$LouvainLabelD %in% c(0,2,4,6,8:12,14,NA),"black","white"),
            size = 6, fontface = "bold") +
  # Axis titles
  labs(x = "Louvain Cluster", y = "Simpson Index") +  
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = -0.07, face = "bold", size = 40),  #Adjusted to make Title fit as Panel label
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 18, face = "bold"),  
    axis.title.y = element_text(size = 18, face = "bold"),  
    panel.grid.major = element_line(color = "grey"),  
    panel.grid.minor = element_blank(),  
    legend.position = "none")

# Show it
gg_simpson

#Save it
ggsave(filename="Overall_Simpson.pdf", plot=gg_simpson,
       path=paste0(plot_path,"00_Appendix/03_Diversities/Alpha/"))



## Combined Graphs
# Combined ASV Count & Chao Plot
count_alpha_divs <- grid.arrange(gg_speciesrichness + labs(title = "A"), gg_chao1 + labs(title = "B"), 
                      ncol = 1, nrow = 2)
# Show it
count_alpha_divs
# Save to plot_path
ggsave(filename="Combined_Overall_SpRichness_Chao.pdf", plot=count_alpha_divs,
       path=paste0(plot_path,"03_Diversities/Alpha/"), width=10,height=10)



# Combined Shannon Entropy & Simpson Index
val_alpha_divs <- grid.arrange(gg_shannon + labs(title = "A"), gg_simpson + labs(title = "B"), 
                    ncol = 1, nrow = 2)
# Show it
val_alpha_divs
# Save to plot_path
ggsave(filename="Combined_Overall_Shannon_Simpson.pdf", plot=val_alpha_divs,
       path=paste0(plot_path,"03_Diversities/Alpha/"), width=10,height=10)
