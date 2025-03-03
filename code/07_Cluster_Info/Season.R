##Loading Libraries
library(ggplot2)
library(dplyr)
library("lubridate")

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/"


Season_Color <- c("#2BCE48","#990000","#F0A3FF","#5EF1F2")
colorblind_gradient_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                                 "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                                 "#009e73","#f0e442","#0072b2","#d55e00",
                                 "#999999")




nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA


# Number of ASVs per cluster by max abundance month
nodes_cluster_month <- nodes %>% group_by(max_abundance_month,LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence), Abundance = sum(Abundance4y))


#Sum of different max_abundance_months in total
nodes_month_overall <- nodes %>% group_by(max_abundance_month) %>% summarise(ASVs = n_distinct(Sequence), Abundance = sum(Abundance4y))
nodes_month_overall$max_abundance_month <- factor(nodes_month_overall$max_abundance_month)


### Seasons
## Set Season names & season colors for timeseries
# Assign Season function
assign_season <- function(df) {
  #print(df)
  if (is.null(df$Season)){
    df$Season <- NA
    ## Set Season Name & Color
    # Names
    df$Season[df$max_abundance_month %in% 3:5] <- "Spring"
    df$Season[df$max_abundance_month %in% 6:8] <- "Summer"
    df$Season[df$max_abundance_month %in% 9:11] <- "Autumn"
    df$Season[df$max_abundance_month %in% c(1:2,12)] <- "Winter"
  }
  # Colors & their Text color
  # Seasonal Color
  df[df$Season == "Spring","Season_Color"] <- Season_Color[1]
  df[df$Season == "Summer","Season_Color"] <- Season_Color[2]
  df[df$Season == "Autumn","Season_Color"] <- Season_Color[3]
  df[df$Season == "Winter","Season_Color"] <- Season_Color[4]
  df$Text_Color <- ifelse(df$Season_Color == "#990000",
                          "white", "black")
  
  return(df) # Return the modified data frame
}


# Set Seasons of dataframes
nodes_cluster_month <- assign_season(nodes_cluster_month)
nodes_month_overall <- assign_season(nodes_month_overall)

## Fit timeseries data into seasons instead of month
# Per Cluster
nodes_cluster_seasons <- nodes_cluster_month %>% 
  group_by(LouvainLabelD, Season) %>%
  summarize(ASVs = sum(ASVs), Abundance = sum(Abundance)) %>% # Calculate the sum of ASVs
  mutate(Freq = ASVs/sum(ASVs), Freq_Abundance = Abundance/sum(Abundance)) # Calculate the sum of Freq
nodes_cluster_seasons <- assign_season(nodes_cluster_seasons)
nodes_cluster_seasons$Season <- factor(nodes_cluster_seasons$Season,
                                      levels = c("Spring", "Summer", "Autumn", "Winter"))

# Overall
nodes_season_overall <- nodes_cluster_seasons %>% group_by(Season) %>%
  summarise(ASVs = sum(ASVs), Abundance = sum(Abundance)) %>%
  mutate(Freq = ASVs/sum(ASVs), Freq_Abundance = Abundance/sum(Abundance))
nodes_season_overall <- assign_season(nodes_season_overall)
nodes_season_overall$Season <- factor(nodes_season_overall$Season,
                                      levels = c("Spring", "Summer", "Autumn", "Winter"))



#### Generate Barplots
### Per Season
## Percentage overall (colored by Season)
gg_percent_asv_season <- ggplot(nodes_season_overall,
                                  aes(x = Season,
                                      y = Freq,
                                      fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 1/4, color = "red",
             linetype = "dashed")+
  #scale_x_discrete(breaks = c("Spring","Summer","Autumn","Winter"))+ #X Axis values (Season)
  scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
  scale_fill_manual(values = setNames(nodes_season_overall$Season_Color,
                                      nodes_season_overall$Season),
                    name = "Season")+ # Use scale_fill_manual for custom colors
  labs(x="Season",y="% of total ASV", title = "ASVs Overall by Season")+ # Axis labels
  geom_text(label= round(nodes_season_overall$Freq,3)*100,size = 4, vjust = -0.5)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Show it
gg_percent_asv_season


#Save to plot_path
ggsave(filename="Overall.png", plot=gg_percent_asv_season, path=paste0(plot_path, "07_Cluster_Info/Seasons/ASV"))



## Abundance overall (colored by Season)
gg_abundance_season <- ggplot(nodes_season_overall,
                             aes(x = Season,
                                 y = Abundance,
                                 fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = setNames(nodes_season_overall$Season_Color,
                                      nodes_season_overall$Season),
                    name = "Season",
                    labels = unique(nodes_season_overall$Season))+ # Use scale_fill_manual for custom colors & Legend
  labs(x="Month",y= "Abundance of seasons ASVs", title = "Abundance Overall by Season")+ # Axis labels
  geom_text(label= round(nodes_season_overall$Abundance,0),
            size = 4, vjust=-0.5)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Show it
gg_abundance_season


#Save to plot_path
ggsave(filename="Overall.png", plot=gg_abundance_season, path=paste0(plot_path, "07_Cluster_Info/Seasons/Abundance"))







## ASV Percentage per cluster (colored by Season) [All]
gg_percent_asv_season_all <- ggplot(nodes_cluster_seasons,
                                aes(x=as.factor(LouvainLabelD),
                                    y=Freq,
                                    fill = Season)) + 
  geom_bar(stat="identity", show.legend = T)+ # Season Barplot 
  labs(x="Cluster",y="% of clusters ASV", title = "Overview by Season for all Clusters by ASVs")+ # Axis labels
  scale_y_continuous(labels = scales::label_percent(suffix=""),
                     breaks=seq(0,1,by=0.1))+ #Y-Axis values (Percent)
  scale_fill_manual(values = setNames(nodes_cluster_seasons$Season_Color,
                                      nodes_cluster_seasons$Season),
                    name = "Season")+ # Use scale_fill_manual for custom colors
  geom_text(label= ifelse(nodes_cluster_seasons$Freq > 0.02,
                          round(nodes_cluster_seasons$Freq,3)*100, ""),
            color = nodes_cluster_seasons$Text_Color,
            size = 4, position = position_stack(vjust = 0.5))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Show it
gg_percent_asv_season_all


#Save to plot_path
ggsave(filename="Per_Cluster.png", plot=gg_percent_asv_season_all, path=paste0(plot_path, "07_Cluster_Info/Seasons/ASV"))



## Abundance Percentage per cluster (colored by Season) [All]
gg_percent_abundance_season_all <- ggplot(nodes_cluster_seasons,
                                aes(x=as.factor(LouvainLabelD),
                                    y=Freq_Abundance,
                                    fill = Season)) + 
  geom_bar(stat="identity", show.legend = T)+ # Season Barplot 
  labs(x="Cluster",y="% of clusters Abundance", title = "Overview by Season for all Clusters by Abundance")+ # Axis labels
  scale_y_continuous(labels = scales::label_percent(suffix=""),
                     breaks=seq(0,1,by=0.1))+ #Y-Axis values (Percent)
  scale_fill_manual(values = setNames(nodes_cluster_seasons$Season_Color,
                                      nodes_cluster_seasons$Season),
                    name = "Season")+ # Use scale_fill_manual for custom colors
  geom_text(label= ifelse(nodes_cluster_seasons$Freq_Abundance > 0.02,
                          round(nodes_cluster_seasons$Freq_Abundance,3)*100, ""),
            color = nodes_cluster_seasons$Text_Color,
            size = 4, position = position_stack(vjust = 0.5))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Show it
gg_percent_abundance_season_all


#Save to plot_path
ggsave(filename="Per_Cluster.png", plot=gg_percent_abundance_season_all, path=paste0(plot_path, "07_Cluster_Info/Seasons/Abundance"))





## Percentage per cluster (colored by Season)[Each alone]
all_gg_percent_season_plots <- list()
all_gg_abundance_season_plots <- list()

plot_season_of_cluster <- function (df, cluster){
  # Get specified cluster from dataframe
  df_cluster <- subset(df,df$LouvainLabelD == cluster)
  
  # Plot it
  plot_season_cluster_asv <- ggplot(df_cluster,
                                aes(x = Season,
                                    y = Freq,
                                    fill = Season)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
    scale_fill_manual(values = setNames(df_cluster$Season_Color,
                                        df_cluster$Season), name = "Season")+ # Use scale_fill_manual for custom colors
    labs(x="Season",y="% of total ASV", title = paste0("Cluster ", cluster, " by Season & ASVs"))+ # Axis labels
    geom_text(label= round(df_cluster$Freq,3)*100,
              size = 4, vjust = -0.5)+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  
  
  # Plot it
  plot_season_cluster_abundance <- ggplot(df_cluster,
                                aes(x = Season,
                                    y = Freq_Abundance,
                                    fill = Season)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
    scale_fill_manual(values = setNames(df_cluster$Season_Color,
                                        df_cluster$Season), name = "Season")+ # Use scale_fill_manual for custom colors
    labs(x="Season",y="% of total Abundance", title = paste0("Cluster ", cluster, " by Season & Abundance"))+ # Axis labels
    geom_text(label= round(df_cluster$Freq_Abundance,3)*100,
              size = 4, vjust = -0.5)+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
  
  # Add it to the all plot list & show it
  all_gg_percent_season_plots[[ifelse(cluster == 0,1,cluster)]] <- plot_season_cluster_asv
  all_gg_abundance_season_plots[[ifelse(cluster == 0,1,cluster)]] <- plot_season_cluster_abundance
  print(plot_season_cluster_asv)
  print(plot_season_cluster_abundance)
  
  #Save to plot_path
  ggsave(filename=paste0("Cluster_",cluster,".png"), plot=plot_season_cluster_asv, path=paste0(plot_path, "00_Appendix/07_Cluster_Info/Seasons/ASV"))
  ggsave(filename=paste0("Cluster_",cluster,".png"), plot=plot_season_cluster_abundance, path=paste0(plot_path, "00_Appendix/07_Cluster_Info/Seasons/Abundance"))
}

for (cluster in c(0,2:13)){
  plot_season_of_cluster(nodes_cluster_seasons, cluster)
}