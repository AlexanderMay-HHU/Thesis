##Loading Libraries
library(ggplot2)
library(dplyr)
library("lubridate")
#library(patchwork) #currently not arranging plots in special patterns

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/"


## Set Season colors
Season_Color <- c("#2BCE48","#990000","#F0A3FF","#5EF1F2")
# Monthly colors vary by 40% lighter/darker of their base season color
Season_Color_Monthly<- c("#5EF1F2","#0D8C8D","#5AED6D",
                         "#2BCE48","#087821","#EA6060",
                         "#990000","#640000","#F5CAFF",
                         "#F0A3FF","#BD04D1","#78FEFF")


## Get Data
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
  df$Season <- NA
  ## Set Season Name & Color
  # Names
    df$Season[df$max_abundance_month %in% 3:5] <- "Spring"
    df$Season[df$max_abundance_month %in% 6:8] <- "Summer"
    df$Season[df$max_abundance_month %in% 9:11] <- "Autumn"
    df$Season[df$max_abundance_month %in% c(1:2,12)] <- "Winter"
 # Colors & their Text color
    # Seasonal Color
    df[df$Season == "Spring","Season_Color"] <- Season_Color[1]
    df[df$Season == "Summer","Season_Color"] <- Season_Color[2]
    df[df$Season == "Autumn","Season_Color"] <- Season_Color[3]
    df[df$Season == "Winter","Season_Color"] <- Season_Color[4]
    
    # Monthly Color
    for (month in 1:12){
      df[df$max_abundance_month == month,"Month_Color"] <- Season_Color_Monthly[month]
      df$Text_Color <- ifelse(df$Month_Color %in% c("#087821","#990000","#640000"),
                           "white", "black")
    }
   
  return(df) # Return the modified data frame
}

# Set Seasons of dataframes
nodes_cluster_month <- assign_season(nodes_cluster_month)
nodes_month_overall <- assign_season(nodes_month_overall)




#### Generate Barplots
## This part is not used at the moment
# Arranges the plots from a list in the specified layout of rows & cols
'### Plots a list of graphs with specified no of rows/cols
plot_a_list <- function(master_list_with_plots, no_of_rows, no_of_cols) {
  
  patchwork::wrap_plots(master_list_with_plots, 
                        nrow = no_of_rows, ncol = no_of_cols)
}'


### Per Month
## Percentage overall (colored by Month)
gg_percent_total_month <- ggplot(nodes_month_overall,
                                 aes(x = max_abundance_month,
                                     y = Freq,
                                     fill = max_abundance_month)) +
                            geom_bar(stat = "identity", position = "dodge") +
                            geom_hline(yintercept = 1/12, color = "red",
                                       linetype = "dashed")+
                            scale_x_discrete(breaks = seq_along(month.name),
                                             labels=month.abb)+ #X Axis values (Louvain Cluster)
                            scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
                            scale_fill_manual(values = setNames(nodes_month_overall$Month_Color,
                                                                nodes_month_overall$max_abundance_month),
                                              name = "Month",
                                              labels = month.name)+ # Use scale_fill_manual for custom colors & Legend
                            labs(x="Month",y="% of total ASV", title = "Overall by Month")+ # Axis labels
                            geom_text(label= round(nodes_month_overall$Freq,3)*100,
                                      size = 4, vjust=-0.5)

# Show it
  gg_percent_total_month
  
#Save to plot_path
  ggsave(filename="Overall.png", plot=gg_percent_total_month, path=paste0(plot_path, "07_Cluster_Info/Max_Month/"))



  
## Percentage per cluster (colored by Month) [All]
gg_percent_month_all <- ggplot(nodes_cluster_month,
                                aes(x=LouvainLabelD,
                                    y=Freq,
                                    fill = factor(max_abundance_month))) + 
  geom_bar(stat="identity", show.legend = T)+ # Season Barplot 
  labs(x="Cluster",y="% of clusters ASV", title = "Overview by Season for all Clusters")+ # Axis labels
  scale_x_continuous(breaks = c(0,2:13))+ #X Axis values (Louvain Cluster)
  scale_y_continuous(labels = scales::label_percent(suffix=""),
                     breaks=seq(0,1,by=0.1))+ #Y-Axis values (Percent)
  scale_fill_manual(values = setNames(nodes_cluster_month$Month_Color,
                                      nodes_cluster_month$max_abundance_month),
                    name = "Month",
                    labels = month.name)+ # Use scale_fill_manual for custom colors & Legend
  geom_text(label= ifelse(nodes_cluster_month$Freq > 0.02,
                          round(nodes_cluster_month$Freq,3)*100, ""),
            color = nodes_cluster_month$Text_Color,
            size = 4, position = position_stack(vjust = 0.5))

# Show it
  gg_percent_month_all


#Save to plot_path
  ggsave(filename="Per_cluster.png", plot=gg_percent_month_all, path=paste0(plot_path, "07_Cluster_Info/Max_Month/"))
  

  
## Percentage per cluster (colored by Season)[Each alone]
all_gg_percent_month_plots <- list()

plot_month_of_cluster <- function (df, cluster){
  # Get specified cluster from dataframe
  df_cluster <- subset(df,df$LouvainLabelD == cluster)
  
  # Plot it
  plot_month_cluster <- ggplot(df_cluster,
                               aes(x = max_abundance_month,
                                   y = Freq,
                                   fill = factor(max_abundance_month))) +
                          geom_bar(stat = "identity") +
                          scale_x_continuous(breaks = seq_along(month.name),
                                           labels=month.abb)+ #X Axis values (Louvain Cluster)
                          scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
                          scale_fill_manual(values = setNames(df_cluster$Month_Color,
                                                              df_cluster$max_abundance_month),
                                            name = "Month",
                                            labels = month.name)+ # Use scale_fill_manual for custom colors & Legend
                          labs(x="Month",y="% of total ASV", title = paste0("Cluster ", cluster, " by Month"))+ # Axis labels
                          geom_text(label= round(df_cluster$Freq,3)*100,
                                    size = 4, vjust=-0.5)

  # Add it to the all plot list & show it
  all_gg_percent_month_plots[[ifelse(cluster == 0,1,cluster)]] <- plot_month_cluster
  print(plot_month_cluster)
  
  #Save to plot_path
  ggsave(filename=paste0("Cluster_",cluster,".png"), plot=plot_month_cluster, path=paste0(plot_path, "07_Cluster_Info/Max_Month"))
}

for (cluster in c(0,2:13)){
  plot_month_of_cluster(nodes_cluster_month, cluster)
}