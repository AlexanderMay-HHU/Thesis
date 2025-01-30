##Loading Libraries
library(ggplot2)
library(dplyr)
library("lubridate")
#library(patchwork) #currently not arranging plots in special patterns

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/"
colorblind_gradient_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                                 "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                                 "#009e73","#f0e442","#0072b2","#d55e00",
                                 "#999999")

nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA


# Number of ASVs per cluster by max abundance month
nodes_cluster_month <- nodes %>% group_by(max_abundance_month,LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence))
nodes_cluster_month <- nodes_cluster_month %>% group_by(LouvainLabelD) %>% mutate(Freq = ASVs/sum(ASVs))


#Sum of different max_abundance_months in total
nodes_month_overall <- nodes %>% group_by(max_abundance_month) %>% summarise(ASVs = n_distinct(Sequence))
nodes_month_overall <- nodes_month_overall %>% mutate(Freq = ASVs/sum(ASVs))
nodes_month_overall$max_abundance_month <- factor(nodes_month_overall$max_abundance_month)


### Seasons
## Set Season names & season colors for timeseries
# Assign Season function
assign_season <- function(df) {
  #print(df)
  if(!(is.null(df$Season))){
    # Seaons already defined
    # Colors & their Text color
    df[df$Season == "Spring","Season_Color"] <- "#2BCE48"
    df[df$Season == "Summer","Season_Color"] <- "#990000"
    df[df$Season == "Autumn","Season_Color"] <- "#F0A3FF"
    df[df$Season == "Winter","Season_Color"] <- "#5EF1F2"
    df$Text_Color <- ifelse(df$Season_Color == "#990000", "white", "black")
  }
  if(is.null(df$Season)){
    # No Seasons defined yet                                   
    df$Season <- NA  # Initialize the Season column (good practice)
    # Names
    df$Season[df$max_abundance_month %in% 3:5] <- "Spring"
    df$Season[df$max_abundance_month %in% 6:8] <- "Summer"
    df$Season[df$max_abundance_month %in% 9:11] <- "Autumn"
    df$Season[df$max_abundance_month %in% c(1:2,12)] <- "Winter"
    # Colors & their Text color
    df[df$Season == "Spring","Season_Color"] <- "#2BCE48"
    df[df$Season == "Summer","Season_Color"] <- "#990000"
    df[df$Season == "Autumn","Season_Color"] <- "#F0A3FF"
    df[df$Season == "Winter","Season_Color"] <- "#5EF1F2"
    df$Text_Color <- ifelse(df$Season_Color == "#990000", "white", "black")
  }
  
  
  return(df) # Return the modified data frame
}

# Set Seasons of dataframes
nodes_cluster_month <- assign_season(nodes_cluster_month)
nodes_month_overall <- assign_season(nodes_month_overall)
## Fit timeseries data into seasons instead of month
# Per Cluster
nodes_cluster_seasons <- nodes_cluster_month %>% 
  group_by(LouvainLabelD, Season) %>%
  summarize(ASVs = sum(ASVs)) %>% # Calculate the sum of ASVs
  mutate(Freq = ASVs/sum(ASVs)) # Calculate the sum of Freq
nodes_cluster_seasons <- assign_season(nodes_cluster_seasons)
# Overall
nodes_season_overall <- nodes_cluster_seasons %>% group_by(Season) %>% summarise(ASVs = sum(ASVs))
nodes_season_overall <- nodes_season_overall %>% mutate(Freq = ASVs/sum(ASVs))
nodes_season_overall <- assign_season(nodes_season_overall)
#nodes_season_overall$Season <- factor(nodes_season_overall$Season)



#### Generate Barplots
## This part is not used at the moment
# Arranges the plots from a list in the specified layout of rows & cols
'### Plots a list of graphs with specified no of rows/cols
plot_a_list <- function(master_list_with_plots, no_of_rows, no_of_cols) {
  
  patchwork::wrap_plots(master_list_with_plots, 
                        nrow = no_of_rows, ncol = no_of_cols)
}'

### Per Season
## Percentage overall (colored by Season)
gg_percent_total_season <- ggplot(nodes_season_overall,
                                  aes(x = Season,
                                      y = Freq,
                                      fill = Season)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 1/4, color = "red",
             linetype = "dashed")+
  scale_x_discrete(breaks = c("Spring","Summer","Autumn","Winter"))+ #X Axis values (Season)
  scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
  scale_fill_manual(values = setNames(nodes_season_overall$Season_Color,
                                      nodes_season_overall$Season),
                    name = "Season")+ # Use scale_fill_manual for custom colors
  labs(x="Season",y="% of total ASV", title = "Overall by Season")+ # Axis labels
  geom_text(label= round(nodes_season_overall$Freq,3)*100,
            size = 4, vjust = -0.5)

# Show it
gg_percent_total_season


#Save to plot_path
ggsave(filename="Overall.png", plot=gg_percent_total_season, path=paste0(plot_path, "07_Cluster_Info/Seasons/"))



## Percentage per cluster (colored by Season) [All]
gg_percent_season_all <- ggplot(nodes_cluster_seasons,
                                aes(x=LouvainLabelD,
                                    y=Freq,
                                    fill = Season)) + 
  geom_bar(stat="identity", show.legend = T)+ # Season Barplot 
  labs(x="Cluster",y="% of clusters ASV", title = "Overview by Season for all Clusters")+ # Axis labels
  scale_x_continuous(breaks = c(0,2:13))+ #X Axis values (Louvain Cluster)
  scale_y_continuous(labels = scales::label_percent(suffix=""),
                     breaks=seq(0,1,by=0.1))+ #Y-Axis values (Percent)
  scale_fill_manual(values = setNames(nodes_cluster_seasons$Season_Color,
                                      nodes_cluster_seasons$Season),
                    name = "Season")+ # Use scale_fill_manual for custom colors
  geom_text(label= ifelse(nodes_cluster_seasons$Freq > 0.02,
                          round(nodes_cluster_seasons$Freq,3)*100, ""),
            color = nodes_cluster_seasons$Text_Color,
            size = 4, position = position_stack(vjust = 0.5))

# Show it
gg_percent_season_all


#Save to plot_path
ggsave(filename="Per_cluster.png", plot=gg_percent_season_all, path=paste0(plot_path, "07_Cluster_Info/Seasons/"))



## Percentage per cluster (colored by Season)[Each alone]
all_gg_percent_season_plots <- list()

plot_season_of_cluster <- function (df, cluster){
  # Get specified cluster from dataframe
  df_cluster <- subset(df,df$LouvainLabelD == cluster)
  
  # Plot it
  plot_season_cluster <- ggplot(df_cluster,
                                aes(x = Season,
                                    y = Freq,
                                    fill = Season)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_discrete(breaks = c("Spring","Summer","Autumn","Winter"))+ #X Axis values (Season)
    scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
    scale_fill_manual(values = setNames(df_cluster$Season_Color,
                                        df_cluster$Season), name = "Season")+ # Use scale_fill_manual for custom colors
    labs(x="Season",y="% of total ASV", title = paste0("Cluster ", cluster, " by Season"))+ # Axis labels
    geom_text(label= round(df_cluster$Freq,3)*100,
              size = 4, vjust = -0.5)
  
  
  # Add it to the all plot list & show it
  all_gg_percent_month_plots[[ifelse(cluster == 0,1,cluster)]] <- plot_season_cluster
  print(plot_season_cluster)
  
  #Save to plot_path
  ggsave(filename=paste0("Cluster_",cluster,".png"), plot=plot_season_cluster, path=paste0(plot_path, "07_Cluster_Info/Seasons/"))
}

for (cluster in c(0,2:13)){
  plot_season_of_cluster(nodes_cluster_seasons, cluster)
}