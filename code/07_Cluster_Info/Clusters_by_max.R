##Loading Libraries
library(dplyr)
library(ggplot2)

#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/"))


# Import data
CCM_ASVs <- read.csv("generated/05_Network/Adjusted_Colors_CCM.csv", header=TRUE)



# Max abundance Year
color_year <- c("2007" = "#FF6F61",
                "2008" = "#6B5B93",
                "2009" = "#88B04B",
                "2010" = "#7986CB",
                "2011" = "#A0522D",
                "2012" = "#64B5F6",
                "2013" = "#FF8C00",
                "2014" = "#A1887F",
                "2015" = "#D32F2F")

# Max abundance Season (Season were defined by Spring: Mar-May | Summer: Jun-Aug | Autumn: Sep-Nov | Winter: Dec-Feb)
color_season <- c("Spring"="#238b45",
                  "Summer"="#c994c7",
                  "Autumn"="#cc4c02",
                  "Winter"="#2171b5")

# Monthly colors (vary by 40% lighter/darker of their base season color)
color_month <- c("January"="#2171b5",
                 "February"="#113b5f",
                 "March"="#44d072",
                 "April"="#238b45",
                 "May"="#0e3a1d",
                 "June"="#ebd8ea",
                 "July"="#c994c7",
                 "August"="#a453a1",
                 "September"="#fd8037",
                 "October"="#cc4c02",
                 "November"="#672601",
                 "December"="#5ba3e1")


## Summarize for Max of...
# Year
max_year <- CCM_ASVs %>% group_by(LouvainLabelD, max_abundance_year) %>%
              summarize(
                Total = sum(n())) %>% ungroup %>% 
              group_by(LouvainLabelD) %>%
              mutate(Freq = Total/sum(Total)) %>% ungroup

# Season
max_season <- CCM_ASVs %>% group_by(LouvainLabelD, max_abundance_season) %>%
  summarize(
    Total = sum(n())) %>% ungroup %>% 
  group_by(LouvainLabelD) %>%
  mutate(Freq = Total/sum(Total)) %>% ungroup

max_season$max_abundance_season <- factor(max_season$max_abundance_season, levels = c("Spring", "Summer", "Autumn", "Winter"))


# Month
max_month <- CCM_ASVs %>% group_by(LouvainLabelD, max_abundance_month) %>%
  summarize(
    Total = sum(n())) %>% ungroup %>% 
  group_by(LouvainLabelD) %>%
  mutate(Freq = Total/sum(Total)) %>% ungroup


max_month$max_abundance_month <- case_when(max_month$max_abundance_month == 1 ~ "January",
                                            max_month$max_abundance_month == 2 ~ "February",
                                            max_month$max_abundance_month == 3 ~ "March",
                                            max_month$max_abundance_month == 4 ~ "April",
                                            max_month$max_abundance_month == 5 ~ "May",
                                            max_month$max_abundance_month == 6 ~ "June",
                                            max_month$max_abundance_month == 7 ~ "July",
                                            max_month$max_abundance_month == 8 ~ "August",
                                            max_month$max_abundance_month == 9 ~ "September",
                                            max_month$max_abundance_month == 10 ~ "October",
                                            max_month$max_abundance_month == 11 ~ "November",
                                            max_month$max_abundance_month == 12 ~ "December")

max_month$max_abundance_month <- factor(max_month$max_abundance_month, levels = c("March","April","May",
                                                                                  "June","July","August",
                                                                                  "September","October","November",
                                                                                  "December","January","February"))


### Plot
## Max Year
gg_max_year <- ggplot(max_year,
                   aes(x=as.factor(LouvainLabelD),
                       y=Freq,
                       fill=as.factor(max_abundance_year))) + 
  # Stacks Bar plot
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.5) +  
  # Coloring based on Trophic strategy
  scale_fill_manual(values = color_year) + 
  #Labels
  geom_text(aes(label = ifelse(Freq>0.05,round(Freq*100,1),"")),  
            position = position_stack(vjust = 0.5),  
            color = ifelse(max_year$max_abundance_year %in% c(2012,2013),"black","white"),
            size = 6, fontface = "bold") +
  # Axis titles
  labs(x = "Louvain Cluster", y = "Amount of Clusters ASVs [%]", 
       title = "Distribution of Maximum Abundance Years\n across Clusters", 
       fill = "Year") +  
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
    legend.position = "right",  
    legend.direction = "vertical",  
    legend.justification = "center",  
    legend.text = element_text(size = 15)
  )

# Show it
gg_max_year


# Save to plot path
ggsave(filename="Max_Year.pdf", plot=gg_max_year, path=paste0(plot_path,"07_Cluster_Info/"), width = 10, height = 7.5)




## Max Season
gg_max_season <- ggplot(max_season,
                      aes(x=as.factor(LouvainLabelD),
                          y=Freq,
                          fill=as.factor(max_abundance_season))) + 
  # Stacks Bar plot
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.5) +  
  # Coloring based on Trophic strategy
  scale_fill_manual(values = color_season) + 
  #Labels
  geom_text(aes(label = ifelse(Freq>0.05,round(Freq*100,1),"")),  
            position = position_stack(vjust = 0.5),  
            color = ifelse(max_season$max_abundance_season %in% c("Summer"),"black","white"),
            size = 6, fontface = "bold") +
  # Axis titles
  labs(x = "Louvain Cluster", y = "Amount of Clusters ASVs [%]", 
       title = "Distribution of Maximum Abundance Season\n across Clusters", 
       fill = "Season") +  
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
    legend.position = "right",  
    legend.direction = "vertical",  
    legend.justification = "center",  
    legend.text = element_text(size = 15)
  )

# Show it
gg_max_season


# Save to plot path
ggsave(filename="Max_Season.pdf", plot=gg_max_season, path=paste0(plot_path,"07_Cluster_Info/"), width = 10, height = 7.5)




# Max Month
gg_max_month <- ggplot(max_month,
                        aes(x=as.factor(LouvainLabelD),
                            y=Freq,
                            fill=max_abundance_month)) + 
  # Stacks Bar plot
  geom_bar(stat = "identity", position = "stack", color = "black", linewidth = 0.5) +  
  # Coloring based on Trophic strategy
  scale_fill_manual(values = color_month) + 
  #Labels
  geom_text(aes(label = ifelse(Freq>0.05,round(Freq*100,1),"")),  
            position = position_stack(vjust = 0.5),  
            color = ifelse(max_month$max_abundance_month %in% c("January","March","June","July","August","December"),"black","white"),
            size = 6, fontface = "bold") +
  # Axis titles
  labs(x = "Louvain Cluster", y = "Amount of Clusters ASVs [%]", 
       title = "Distribution of Maximum Abundance Month\n across Clusters", 
       fill = "Month") +  
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
    legend.position = "right",  
    legend.direction = "vertical",  
    legend.justification = "center",  
    legend.text = element_text(size = 15)
  )

# Show it
gg_max_month


# Save to plot path
ggsave(filename="Max_Month.pdf", plot=gg_max_month, path=paste0(plot_path,"07_Cluster_Info/"), width = 10, height = 7.5)