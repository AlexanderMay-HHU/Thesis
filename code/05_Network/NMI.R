##Loading Libraries
library("dplyr")
library("ggplot2")

# Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/"))


# Import data
nodes <- read.csv("original/Convergent_Cross_Mapping/CCM_node_table.csv", header = TRUE)
edges <- read.csv("original/Convergent_Cross_Mapping/CCM_edge_table.csv", header=TRUE)


color_clusters <- c("#00ffff","#c71585","#2f4f4f","#ff0000",
                    "#0000ff","#bdb76b","#006400","#ffa500",
                    "#1e90ff","#ffb6c1","#00fa9a","#ffff00",
                    "#631919")


## Declutter name of edges into from and to ASVs
edge_list_from <- edges  %>% mutate(from_ASV = substring(name,1,12),
                               to_ASV = substring(name,31,42)) %>%
                        select(from_ASV,from_clu,corr)


## Louvain clusters color
edge_list_from$Cluster_color <- color_clusters[ifelse(edge_list_from$from_clu == 0,1,edge_list_from$from_clu)]


# Summary of NMIs per cluster & overall
summary_by_clu <- edge_list_from %>%
  group_by(from_clu) %>%
  summarize(
    Count = sum(!is.na(corr)),
    Min = min(corr, na.rm = TRUE),
    Q1 = quantile(corr, 0.25, na.rm = TRUE),
    Median = median(corr, na.rm = TRUE),
    Mean = mean(corr, na.rm = TRUE),
    Q3 = quantile(corr, 0.75, na.rm = TRUE),
    Max = max(corr, na.rm = TRUE)
  )

summary_overall <- edge_list_from %>%
  summarize(
    Count = sum(!is.na(corr)),
    Min = min(corr, na.rm = TRUE),
    Q1 = quantile(corr, 0.25, na.rm = TRUE),
    Median = median(corr, na.rm = TRUE),
    Mean = mean(corr, na.rm = TRUE),
    Q3 = quantile(corr, 0.75, na.rm = TRUE),
    Max = max(corr, na.rm = TRUE)
  )


# Difference in C4 and others?
group4 <- edge_list_from$corr[edge_list_from$from_clu == 4]
other_groups <- edge_list_from$corr[edge_list_from$from_clu != 4]

shapiro.test(group4)
wilcox.test(group4, other_groups, alternative = "greater")

library(effectsize)

# Calculate rank-biserial correlation
rank_biserial <- rank_biserial(group4, other_groups)
print(rank_biserial)


boxplot(corr ~ from_clu == 4, data = edge_list_from,
        names = c("Other Groups", "Group 4"),
        xlab = "Group", ylab = "Correlation")



## Plots
# NMI - out (from clu)
gg_NMI_out <- ggplot(edge_list_from, aes(x = "", y = corr, fill = as.factor(from_clu))) +  
  geom_violin(trim = FALSE, scale = "width", adjust = 1.5, alpha = 0.7) + 
  geom_boxplot(aes(x = "", y = corr), 
               width = 0.1, 
               color = "black", 
               fill = "white", 
               alpha = 0.5, 
               outlier.shape = NA) +  
  labs(
    title = "Normalized Mutual Information - NMI (out)\n per cluster",
    x = "Louvain Cluster", y = "NMI (out)",
    fill = "Cluster"
  ) +
  scale_y_continuous(limits = c(0.35,0.9), breaks = seq(0.2, 0.9, 0.1))+
  scale_fill_manual(values = color_clusters) +
  facet_wrap(~ from_clu, nrow = 1, strip.position = "bottom") +  
  theme_minimal(base_size = 18) +  
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 22),  #Adjusted to make Title fit as Panel label
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
    axis.text.y = element_text(size = 16),  
    axis.title.x = element_text(size = 18, face = "bold"),  
    axis.title.y = element_text(size = 18, face = "bold"),  
    panel.grid.major.x = element_line(color = "grey"),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor = element_blank(),  
    legend.position = "none")


# Show it
gg_NMI_out
# Save to plot_path
ggsave(filename="NMI_out_Violin.pdf", plot=gg_NMI_out,path=paste0(plot_path,"05_Network/"),width = 10, height = 7.5)