##Loading Libraries
library(igraph)
library(dplyr)
library(tidyr)
library(ggplot2)

# Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/"))


# Import data
nodes <- read.csv("original/Convergent_Cross_Mapping/CCM_node_table.csv", header = TRUE)
edges <- read.csv("original/Convergent_Cross_Mapping/CCM_edge_table.csv", header=TRUE)
asv_overview <- read.csv("generated/abundance_overview.csv", header = T)


color_clusters <- c("#00ffff","#c71585","#2f4f4f","#ff0000",
                    "#0000ff","#bdb76b","#006400","#ffa500",
                    "#1e90ff","#ffb6c1","#00fa9a","#ffff00",
                    "#631919")


kingdom_colors <- c("#d62728","#2ca02c","#1f77b4")

## Declutter name of edges into from and to ASVs
edge_list <- edges  %>% mutate(from_ASV = substring(name,1,12),
                               to_ASV = substring(name,31,42)) %>%
                      select(from_ASV,to_ASV)


## Make igraph object from edgelist
edge_graph <- graph_from_data_frame(edge_list,directed=T)

## Calculate betweenness & closeness centrality
# Betweenness
betweenness <- betweenness(edge_graph, directed = T, normalized = F) %>% as.data.frame()
betweenness$ASV <- rownames(betweenness)
rownames(betweenness) <- NULL
colnames(betweenness) <- c("betweenness_value","ASV")
betweenness <- betweenness[,c("ASV","betweenness_value")]
# Normalized Betweenness
betweenness_norm <- betweenness(edge_graph, directed = T, normalized = T) %>% as.data.frame()
betweenness_norm$ASV <- rownames(betweenness_norm)
rownames(betweenness_norm) <- NULL
colnames(betweenness_norm) <- c("betweenness_value","ASV")
betweenness_norm <- betweenness_norm[,c("ASV","betweenness_value")]

# Closeness
closeness <- closeness(edge_graph, normalized = F) %>% as.data.frame()
closeness$ASV <- rownames(closeness)
rownames(closeness) <- NULL
colnames(closeness) <- c("closeness_value","ASV")
closeness <- closeness[,c("ASV","closeness_value")]
# Normalized Closeness
closeness_norm <- closeness(edge_graph,  normalized = T) %>% as.data.frame()
closeness_norm$ASV <- rownames(closeness_norm)
rownames(closeness_norm) <- NULL
colnames(closeness_norm) <- c("closeness_value","ASV")
closeness_norm <- closeness_norm[,c("ASV","closeness_value")]





## Merge centrality measures into single dataframe and add Louvainclusters + their color
centrality_measures <- betweenness %>% left_join(closeness,
                                               by = c("ASV" = "ASV")) %>%
                          left_join(nodes %>%
                                    select(name,LouvainLabelD),
                                    by = c("ASV" = "name"))
centrality_measures$LouvainLabelD <- as.factor(centrality_measures$LouvainLabelD)
centrality_measures$Cluster_color <- color_clusters[ifelse(centrality_measures$LouvainLabelD[1] == 0,1,centrality_measures$LouvainLabelD[1])]


centrality_measures_norm <- betweenness_norm %>% left_join(closeness_norm,
                                                 by = c("ASV" = "ASV"))%>%
                          left_join(nodes %>%
                                      select(name,LouvainLabelD),
                                    by = c("ASV" = "name"))
centrality_measures_norm$LouvainLabelD <- as.factor(centrality_measures_norm$LouvainLabelD)
centrality_measures_norm$Cluster_color <- color_clusters[ifelse(centrality_measures_norm$LouvainLabelD[1] == 0,1,centrality_measures_norm$LouvainLabelD[1])]




# export centrality measures
write.csv(centrality_measures, "generated/05_Network/Centrality_Measures.csv")
write.csv(centrality_measures_norm, "generated/05_Network/Centrality_Measures_norm.csv")





## Overview
# Betweenness
summary(betweenness$betweenness_value)
summary(betweenness_norm$betweenness_value)

# Closeness
summary(closeness$closeness_value)
summary(closeness_norm$closeness_value)



# Betweenness analysis
betweenness_norm_analysis <- betweenness_norm %>% 
                              left_join(asv_overview %>%
                                        select(OTU.number,max_abundance_year,max_abundance_season,max_abundance_month,LouvainLabelD),
                                        by = c("ASV" = "OTU.number"))

for (i in c(0,2:13)){
  print(paste0("C",i,": ",mean(betweenness_norm_analysis[betweenness_norm_analysis$LouvainLabelD==i,"betweenness_value"],na.rm = T)))
}

# Check betweenness from ASVs in highest 10% of dataset
threshold_bet <- quantile(betweenness_norm_analysis$betweenness_value, probs = 0.90, na.rm = TRUE)
top_10perc_bet <- betweenness_norm_analysis[betweenness_norm_analysis$betweenness_value >= threshold_bet, ]
top_10perc_bet<- top_10perc_bet %>% mutate(Kingdom = substr(ASV,1,3))

top_10perc_bet <- top_10perc_bet %>%
  group_by(max_abundance_season) %>%
  mutate(total = n()) %>%
  group_by(max_abundance_season, Kingdom) %>%
  summarise(proportion = n() / first(total), .groups = "drop", count = n())


ggplot(top_10perc_bet, aes(x = factor(max_abundance_season, levels= c("Spring","Summer","Autumn","Winter")),
                            y = count, fill = Kingdom)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Season",
    y = "Count",
    title = "Kingdom Distribution Across Seasons\n(Top 10% Betweenness)",
    fill = "Kingdom"
  ) +
  geom_text(label = paste0(round(top_10perc_bet$proportion*100,1),"%"),
            position = position_stack(vjust = 0.5),
            color = "black", size = 5, fontface = "bold")+
  theme_minimal() +
  # Customize colors (optional)
  scale_fill_manual(values = kingdom_colors) +
  # Improve x-axis labels
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  #Adjusted to make Title fit as Panel label
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 18, face = "bold"),  
    axis.title.y = element_text(size = 18, face = "bold"),  
    panel.grid.major = element_line(color = "grey"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none")


# Closeness analysis
closeness_norm_analysis <- closeness_norm %>% 
  left_join(asv_overview %>%
              select(OTU.number,max_abundance_year,max_abundance_season,max_abundance_month),
            by = c("ASV" = "OTU.number"))


threshold_close <- quantile(closeness_norm_analysis$closeness_value, probs = 0.90, na.rm = TRUE)
top_10perc_close <- closeness_norm_analysis[closeness_norm_analysis$closeness_value >= threshold_close, ]
top_10perc_close<- top_10perc_close %>% mutate(Kingdom = substr(ASV,1,3))

top_10perc_close <- top_10perc_close %>%
  group_by(max_abundance_season) %>%
  mutate(total = n()) %>%
  group_by(max_abundance_season, Kingdom) %>%
  summarise(proportion = n() / first(total), .groups = "drop", count = n())

top_10perc_close <- top_10perc_close[1:11,]

ggplot(top_10perc_close, aes(x = factor(max_abundance_season, levels= c("Spring","Summer","Autumn","Winter")), y = count, fill = Kingdom)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    x = "Season",
    y = "Count",
    title = "Kingdom Distribution Across Seasons\n(Top 10% Closeness)",
    fill = "Kingdom"
  ) +
  geom_text(label = paste0(round(top_10perc_close$proportion*100,1),"%"),  
            position = position_stack(vjust = 0.5),  #place in middle of stacked bar
            color = "black", size = 5, fontface = "bold")+ # White bold text for any but the small clusters
  theme_minimal() +
  # Customize colors (optional)
  scale_fill_manual(values = kingdom_colors) +
  # Improve x-axis labels
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  #Adjusted to make Title fit as Panel label
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 18, face = "bold"),  
    axis.title.y = element_text(size = 18, face = "bold"),  
    panel.grid.major = element_line(color = "grey"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none")



## Plots
# Betweenness Violin
gg_betweenness <- ggplot(centrality_measures,
                         aes(x=LouvainLabelD,
                             y=betweenness_value,
                             fill=LouvainLabelD))+
  geom_violin(trim=F)+
  ylim(0,100000) +
  labs(x="Louvain Cluster", y= "Betweenness Centrality",
       title = "Betweenness Centrality\nby Cluster",
       fill = "Louvain Cluster")+
  scale_fill_manual(values = color_clusters)+
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  #Adjusted to make Title fit as Panel label
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 18, face = "bold"),  
    axis.title.y = element_text(size = 18, face = "bold"),  
    panel.grid.major = element_line(color = "grey"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),  
    legend.position = "none")

# Show it
gg_betweenness
# Save to plot_path
ggsave(filename="Betweenness.pdf", plot=gg_betweenness,path=paste0(plot_path,"00_Appendix/05_Network/"),width = 10, height = 8)


# Normalized Betweenness Violin
gg_betweenness_norm <- ggplot(centrality_measures_norm,
                         aes(x=LouvainLabelD,
                             y=betweenness_value,
                             fill=LouvainLabelD))+
  geom_violin(trim=F)+
  ylim(0,0.02)+
  labs(x="Louvain Cluster", y= "Betweenness Centrality",
       title = "Normalized Betweenness Centrality\nby Cluster",
       fill = "Louvain Cluster")+
  scale_fill_manual(values = color_clusters)+
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  #Adjusted to make Title fit as Panel label
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 18, face = "bold"),  
    axis.title.y = element_text(size = 18, face = "bold"),  
    panel.grid.major = element_line(color = "grey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),  
    legend.position = "none")

# Show it
gg_betweenness_norm
# Save to plot_path
ggsave(filename="Betweenness_normalized.pdf", plot=gg_betweenness_norm,path=paste0(plot_path,"05_Network/"),width = 10, height = 8)



# Closeness Violin
gg_closeness <- ggplot(centrality_measures,
                         aes(x=LouvainLabelD,
                             y=closeness_value,
                             fill=LouvainLabelD))+
  geom_violin(trim=F)+
  ylim(0,0.00004)+
  labs(x="Louvain Cluster", y= "Closeness Centrality",
       title = "Closeness Centrality\nby Cluster",
       fill = "Louvain Cluster")+
  scale_fill_manual(values = color_clusters)+
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  #Adjusted to make Title fit as Panel label
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 18, face = "bold"),  
    axis.title.y = element_text(size = 18, face = "bold"),  
    panel.grid.major = element_line(color = "grey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),  
    legend.position = "none")

# Show it
gg_closeness
# Save to plot_path
ggsave(filename="Closeness.pdf", plot=gg_closeness,path=paste0(plot_path,"00_Appendix/05_Network/"),width = 10, height = 8)


# Normalized  Closeness Violin
gg_closeness_norm <- ggplot(centrality_measures_norm,
                              aes(x=LouvainLabelD,
                                  y=closeness_value,
                                  fill=LouvainLabelD))+
  geom_violin(trim=F)+
  ylim(0.03,0.23)+
  labs(x="Louvain Cluster", y= "Closeness Centrality",
       title = "Normalized Closeness Centrality\nby Cluster",
       fill = "Louvain Cluster")+
  scale_fill_manual(values = color_clusters)+
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  #Adjusted to make Title fit as Panel label
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 18, face = "bold"),  
    axis.title.y = element_text(size = 18, face = "bold"),  
    panel.grid.major = element_line(color = "grey"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),  
    legend.position = "none")

# Show it
gg_closeness_norm
# Save to plot_path
ggsave(filename="Closeness_Normalized.pdf", plot=gg_closeness_norm,path=paste0(plot_path,"05_Network/"),width = 10, height = 8)