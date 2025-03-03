##Loading Libraries
library(ggplot2)
library(dplyr)
library("lubridate")

#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data"))


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
nodes_cluster_month <- nodes %>% group_by(max_abundance_month,LouvainLabelD) %>% summarise(ASVs = n_distinct(Sequence), Abundance = sum(Abundance4y))
nodes_cluster_month <- nodes_cluster_month %>% group_by(LouvainLabelD) %>% mutate(Freq = ASVs/sum(ASVs), Freq_Abundance = Abundance/sum(Abundance))


#Sum of different max_abundance_months in total
nodes_month_overall <- nodes %>% group_by(max_abundance_month) %>% summarise(ASVs = n_distinct(Sequence), Abundance = sum(Abundance4y))
nodes_month_overall <- nodes_month_overall %>% mutate(Freq = ASVs/sum(ASVs), Freq_Abundance = Abundance/sum(Abundance))
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
### Per Month
### Overall
## Percentage of ASVs (colored by Month)
gg_percent_asv_month <- ggplot(nodes_month_overall,
                                 aes(x = max_abundance_month,
                                     y = Freq,
                                     fill = max_abundance_month)) +
                            geom_bar(stat = "identity", position = "dodge") +
                            geom_hline(yintercept = 1/12, color = "red",
                                       linetype = "dashed")+
                            scale_x_discrete(breaks = seq_along(month.name),
                                             labels=month.abb)+ #X Axis values (Month)
                            scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
                            scale_fill_manual(values = setNames(nodes_month_overall$Month_Color,
                                                                nodes_month_overall$max_abundance_month),
                                              name = "Month",
                                              labels = month.name)+ # Use scale_fill_manual for custom colors & Legend
                            labs(x="Month",y="% of total ASVs", title = "ASVs Overall by Month")+ # Axis labels
                            geom_text(label= round(nodes_month_overall$Freq,3)*100,
                                      size = 4, vjust=-0.5)+
                            theme(panel.grid.major.x = element_blank(),
                                  panel.grid.minor.x = element_blank())

# Show it
gg_percent_asv_month
  
#Save to plot_path
  ggsave(filename="Overall.png", plot=gg_percent_asv_month, path=paste0(plot_path, "07_Cluster_Info/Max_Month/ASV/"))
  
  

## Abundance overall (colored by Month)
gg_abundance_month <- ggplot(nodes_month_overall,
                                 aes(x = max_abundance_month,
                                     y = Abundance,
                                     fill = max_abundance_month)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(breaks = seq_along(month.name),
                   labels=month.abb)+ #X Axis values (Month)
  scale_fill_manual(values = setNames(nodes_month_overall$Month_Color,
                                      nodes_month_overall$max_abundance_month),
                    name = "Month",
                    labels = month.name)+ # Use scale_fill_manual for custom colors & Legend
  labs(x="Month",y= "Abundance of months ASVs", title = "Abundance Overall by Month")+ # Axis labels
  geom_text(label= round(nodes_month_overall$Abundance,0),
            size = 4, vjust=-0.5)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Show it
gg_abundance_month

#Save to plot_path
ggsave(filename="Overall.png", plot=gg_abundance_month, path=paste0(plot_path, "07_Cluster_Info/Max_Month/Abundance/"))
  
  



  
## ASV Percentage per cluster (colored by Month) [All - stacked barplot]
gg_percent_asv_month_all <- ggplot(nodes_cluster_month,
                                aes(x=as.factor(LouvainLabelD),
                                    y=Freq,
                                    fill = factor(max_abundance_month))) + 
  geom_bar(stat="identity", show.legend = T)+ # Season Barplot 
  labs(x="Cluster",y="% of clusters ASVs", title = "Overview by Month for all Clusters by ASVs")+ # Axis labels
  scale_y_continuous(labels = scales::label_percent(suffix=""),
                     breaks=seq(0,1,by=0.1))+ #Y-Axis values (Percent)
  scale_fill_manual(values = setNames(nodes_cluster_month$Month_Color,
                                      nodes_cluster_month$max_abundance_month),
                    name = "Month",
                    labels = month.name)+ # Use scale_fill_manual for custom colors & Legend
  geom_text(label= ifelse(nodes_cluster_month$Freq > 0.02,
                          round(nodes_cluster_month$Freq,3)*100, ""),
            color = nodes_cluster_month$Text_Color,
            size = 4, position = position_stack(vjust = 0.5))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Show it
gg_percent_asv_month_all


#Save to plot_path
  ggsave(filename="Per_Cluster.png", plot=gg_percent_asv_month_all, path=paste0(plot_path, "07_Cluster_Info/Max_Month/ASV/"))
  
  
  
  
## Abundance Percentage per cluster (colored by Month) [All - stacked barplot]
gg_abundance_month_all <- ggplot(nodes_cluster_month,
                               aes(x=as.factor(LouvainLabelD),
                                   y=Freq_Abundance,
                                   fill = factor(max_abundance_month))) + 
  geom_bar(stat="identity", show.legend = T)+ # Season Barplot 
  labs(x="Cluster",y="% of clusters Abundance", title = "Overview by Month for all Clusters by Abundance")+ # Axis labels
  scale_y_continuous(labels = scales::label_percent(suffix=""),
                     breaks=seq(0,1,by=0.1))+ #Y-Axis values (Percent)
  scale_fill_manual(values = setNames(nodes_cluster_month$Month_Color,
                                      nodes_cluster_month$max_abundance_month),
                    name = "Month",
                    labels = month.name)+ # Use scale_fill_manual for custom colors & Legend
  geom_text(label= ifelse(nodes_cluster_month$Freq_Abundance > 0.02,
                          round(nodes_cluster_month$Freq_Abundance,3)*100, ""),
            color = nodes_cluster_month$Text_Color,
            size = 4, position = position_stack(vjust = 0.5))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Show it
gg_abundance_month_all
  

#Save to plot_path
ggsave(filename="Per_Cluster.png", plot=gg_abundance_month_all, path=paste0(plot_path, "07_Cluster_Info/Max_Month/Abundance/"))

  
## Percentage per cluster (colored by Season)[Each alone]
all_gg_percent_month_plots <- list()
all_gg_abundance_month_plots <- list()


plot_month_of_cluster <- function (df, cluster){
  # Get specified cluster from dataframe
  df_cluster <- subset(df,df$LouvainLabelD == cluster)
  
  # Plot it (ASVs)
  plot_month_cluster_asv <- ggplot(df_cluster,
                               aes(x = max_abundance_month,
                                   y = Freq,
                                   fill = factor(max_abundance_month))) +
                          geom_bar(stat = "identity") +
                          scale_x_continuous(breaks = seq_along(month.name),
                                           labels=month.abb)+ #X Axis values (Month)
                          scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
                          scale_fill_manual(values = setNames(df_cluster$Month_Color,
                                                              df_cluster$max_abundance_month),
                                            name = "Month",
                                            labels = month.name)+ # Use scale_fill_manual for custom colors & Legend
                          labs(x="Month",y="% of Clusters ASVs", title = paste0("Cluster ", cluster, " by Month & ASVs"))+ # Axis labels
                          geom_text(label= round(df_cluster$Freq,3)*100,
                                    size = 4, vjust=-0.5)+
                          theme(panel.grid.major.x = element_blank(),
                                panel.grid.minor.x = element_blank())
  
  # Plot it (Abundance)
  plot_month_cluster_abundance <- ggplot(df_cluster,
                               aes(x = max_abundance_month,
                                   y = Freq_Abundance,
                                   fill = factor(max_abundance_month))) +
    geom_bar(stat = "identity") +
    scale_x_continuous(breaks = seq_along(month.name),
                       labels=month.abb)+ #X Axis values (Month)
    scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
    scale_fill_manual(values = setNames(df_cluster$Month_Color,
                                        df_cluster$max_abundance_month),
                      name = "Month",
                      labels = month.name)+ # Use scale_fill_manual for custom colors & Legend
    labs(x="Month",y="% of Clusters Abundance", title = paste0("Cluster ", cluster, " by Month & Abundance"))+ # Axis labels
    geom_text(label= round(df_cluster$Freq_Abundance,3)*100,
              size = 4, vjust=-0.5)+
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  

  # Add it to the all plot list & show it
  all_gg_percent_month_plots[[ifelse(cluster == 0,1,cluster)]] <- plot_month_cluster_asv
  all_gg_abundance_month_plots[[ifelse(cluster == 0,1,cluster)]] <- plot_month_cluster_abundance
  print(plot_month_cluster_asv)
  print(plot_month_cluster_abundance)
  
  #Save to plot_path
  ggsave(filename=paste0("Cluster_",cluster,".png"), plot=plot_month_cluster_asv, path=paste0(plot_path, "00_Appendix/07_Cluster_Info/Max_Month/ASV/"))
  ggsave(filename=paste0("Cluster_",cluster,".png"), plot=plot_month_cluster_abundance, path=paste0(plot_path, "00_Appendix/07_Cluster_Info/Max_Month/Abundance/"))
}



for (cluster in c(0,2:13)){
  plot_month_of_cluster(nodes_cluster_month, cluster)
}