##Loading Libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)


#Set Working Directory
project_folder <- dirname(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/original/Convergent_Cross_Mapping"))


# Import data
edges <- read.csv("CCM_edge_table.csv", header=TRUE)



# declutter name of edges into from and to ASVs
edges <- edges %>% mutate(from_ASV = substring(name,1,12), to_ASV = substring(name,31,42))

#Only get interactions between different clusters
edges_from_to_different <- edges[edges$from_clu != edges$to_clu,]



## (Not used currently)
# Number of interactions of each ASV
#out_interactions_per_asv <- data.frame(table(edges_from_to_different$from_ASV))
#in_interactions_per_asv <- data.frame(table(edges_from_to_different$to_ASV))



## Number of interactions from/to different clusters
# Make adjacency matrices
adjacencyData <- with(edges, table(from_clu, to_clu)) %>% as.data.frame() %>%
  pivot_wider(names_from = from_clu ,values_from = Freq) %>%
  select(-to_clu)

adjacencyData_diff <- with(edges_from_to_different, table(from_clu, to_clu)) %>% as.data.frame() %>%
  pivot_wider(names_from = from_clu ,values_from = Freq) %>%
  select(-to_clu)

# Get In and Out Interactions
in_deg <- apply(adjacencyData,1,sum)
out_deg <- unname(apply(adjacencyData,2,sum))

in_deg_diff <- apply(adjacencyData_diff,1,sum)
out_deg_diff <- unname(apply(adjacencyData_diff,2,sum))

#Get data into format
#Get data into format
clusters <- c(0,2:13)
in_out_deg <- data.frame(clusters,in_deg,out_deg) %>%
                          pivot_longer(cols = -clusters, 
                                       names_to = "degree_type",
                                       values_to = "count") %>%
                          group_by(clusters) %>%
                          mutate(percentage = count / sum(count)) %>% ungroup()
in_out_deg_diff <- data.frame(clusters,in_deg_diff,out_deg_diff) %>%
                              pivot_longer(cols = -clusters, 
                                           names_to = "degree_type",
                                           values_to = "count") %>%
                              group_by(clusters) %>%
                              mutate(percentage = count / sum(count)) %>% ungroup()



### Generate Plots
## All interactions
# Legend 
dummy_data <- data.frame(degree_type = factor(c("in_deg", "out_deg")), value = c(1, 1))

legend_plot <- ggplot(dummy_data, aes(x = 1, y = value, fill = degree_type)) +
  geom_col(width = 0) +  # Invisible bars
  scale_fill_manual(
    values = c("in_deg" = "#007A8B", "out_deg" = "#8B1100"),
    labels = c("Incoming", "Outgoing"),
    name = "Interaction Type"
  ) +
  labs(title = "Interactions between all Clusters by Cluster") +
  theme_void() +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    legend.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.text = element_text(size = 18)) +
  guides(fill = guide_legend(override.aes = list(color = NA))) # Remove legend key borders
# Show it
legend_plot


## All Interactions
count_plot <- ggplot(in_out_deg, aes(x = as.factor(clusters), y = count, fill = degree_type)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.5) + 
  labs(x = "Cluster",y = "Count",title = "A",
       fill = "Interaction Type") +
  geom_text(aes(label = paste0(count,"\n",round(percentage*100,1),"%")),  
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4, fontface = "bold") +
  scale_fill_manual(values = c("in_deg" = "#007A8B", "out_deg" = "#8B1100"),
                    labels = c("Incoming", "Outgoing")) +
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = -0.05, face = "bold", size = 40),  #Adjusted to make Title fit as Panel label  
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5), 
    axis.text.y = element_text(size = 16),  
    legend.title = element_text(face = "bold"),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray"),  #gray grid
    panel.grid.minor = element_blank()  
  )

# Show it
count_plot


## Different Interactions
count_plot_diff <- ggplot(in_out_deg_diff, aes(x = as.factor(clusters), y = count, fill = degree_type)) +
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.5) + 
  labs(x = "Cluster",y = "Count",title = "A",
       fill = "Interaction Type") +
  geom_text(aes(label = paste0(count,"\n",round(percentage*100,1),"%")),  
            position = position_stack(vjust = 0.5), 
            color = "white", size = 4, fontface = "bold") +
  scale_fill_manual(values = c("in_deg_diff" = "#007A8B", "out_deg_diff" = "#8B1100"),
                    labels = c("Incoming", "Outgoing")) +
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = -0.05, face = "bold", size = 40),  #Adjusted to make Title fit as Panel label  
    axis.text.x = element_text(size = 16, angle = 0, hjust = 0.5), 
    axis.text.y = element_text(size = 16),  
    legend.title = element_text(face = "bold"),  
    legend.position = "none",  
    panel.grid.major = element_line(color = "gray"),  #gray grid
    panel.grid.minor = element_blank()  
  )

# Show it
count_plot_diff





# combined
final_layout <- legend_plot / count_plot / count_plot_diff + 
  plot_layout(
    heights = c(0.25, 2, 2))&#,
  #guides = "collect") & 
  theme(plot.margin = margin(5, 15, 5, 5))

# Display the combined plot
final_layout



## Save all plots
ggsave(filename="Overall_all.pdf", plot=count_plot +
         labs(title = "Interactions between all Clusters")+
         theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
               legend.position = "top"),
       path=paste0(plot_path,"04_Interactions/Comparisons/"), width = 10, height = 9)

ggsave(filename="Overall_diff.pdf", plot=count_plot_diff +
         labs(title = "Interactions between different Clusters")+
         theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
               legend.position = "top"),
       path=paste0(plot_path,"04_Interactions/Comparisons/"), width = 10, height = 9)


ggsave(filename="Overall_combined.pdf", plot=final_layout,path=paste0(plot_path,"00_Appendix/04_Interactions/Comparisons/"), width = 10, height = 20)

