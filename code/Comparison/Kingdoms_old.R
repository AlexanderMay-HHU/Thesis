##Loading Libraries
library(ggplot2)
library(dplyr)
library(patchwork)

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
colorblind_palette <- palette.colors("Okabe-Ito",n=4)[2:4]



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)


#Sum of Abundance from different Kingdoms
nodes_sum <- nodes %>% group_by(Kingdom,LouvainLabelD) %>% summarise(Abundance4y = round(sum(Abundance4y),0))
nodes_sum <- nodes_sum %>% group_by(LouvainLabelD) %>% mutate(Percent = Abundance4y/sum(Abundance4y))

#Sum of different Kingdoms in total
nodes_sum_total <- nodes %>% group_by(Kingdom) %>% summarise(Abundance4y = sum(Abundance4y))
nodes_sum_total$LouvainLabelD <- c("Total")
nodes_sum_total <- nodes_sum_total %>% mutate(Percent = Abundance4y/sum(Abundance4y))


###Clusters
## Generate Barplots
#Count numbers
gg_distribution_count <- ggplot(nodes_sum,
                                aes(fill=Kingdom,
                                    y=Abundance4y,
                                    x=LouvainLabelD,
                                    label=sprintf("%2.0f",Abundance4y))) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Louvain Cluster",y="Abundance")+
  #geom_text(check_overlap=T,size = 4,position = position_stack(0.5))+
  scale_x_continuous(breaks = c(0,2:13))+
  scale_y_continuous(breaks = seq(0,10000,by=1000))+
  scale_fill_discrete(type=colorblind_palette)

#Show it
gg_distribution_count

#Save to plot_path
ggsave(filename="Cluster_Count.png", plot=gg_distribution_count, path=paste(plot_path,"/Appendix/Kingdom_Distribution",sep=""))



#Percentage
gg_distribution_percent <- ggplot(nodes_sum,
                                  aes(fill=Kingdom,
                                      y=Percent,
                                      x=LouvainLabelD,
                                      label=scales::label_percent(accuracy = 1)(Percent))) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Louvain Cluster",y="% of total abundance")+
  geom_text(size = 4,position = position_stack(0.5))+
  scale_x_continuous(breaks = c(0,2:13))+
  scale_y_continuous(labels = scales::label_percent(),breaks=seq(0,1,by=0.1))+
  scale_fill_discrete(type=colorblind_palette)

#Show it
gg_distribution_percent

#Save to plot_path
ggsave(filename="Cluster_Percent.png", plot=gg_distribution_percent, path=paste(plot_path,"/Overview/Kingdom_Distribution",sep=""))


## Combined Plots
gg_distribution_combined <- (gg_distribution_count) / (gg_distribution_percent) +
                              plot_annotation(tag_levels = 'A')
gg_distribution_combined
ggsave(filename="Cluster_Combined.png", plot=gg_distribution_combined, path=paste(plot_path,"/Appendix/Kingdom_Distribution",sep="")) 



### Totals
## Generate Barplots
#Count numbers of Total
gg_distribution_count_of_total <- ggplot(nodes_sum_total,
                                         aes(fill=Kingdom,
                                             y=Abundance4y,
                                             x=LouvainLabelD,
                                             label=sprintf("%2.0f",Abundance4y))) + 
  geom_bar(position="dodge", stat="identity")+
  labs(x="Total",y="Abundance")+
  geom_text(inherit.aes = T,aes(y=Abundance4y*0.75),size = 5,position = position_dodge(width=0.9))+
  scale_x_discrete(label = c(""))+
  scale_y_continuous(breaks = seq(0,12500,by=2500))+
  scale_fill_discrete(type=colorblind_palette)

#Show it
gg_distribution_count_of_total

#Save to plot_path
ggsave(filename="Total_Count.png", plot=gg_distribution_count_of_total, path=paste(plot_path,"/Appendix/Kingdom_Distribution",sep=""))


#Percentage of Total
gg_distribution_percent_of_total <- ggplot(nodes_sum_total,
                                           aes(fill=Kingdom,
                                               y=Percent,
                                               x=LouvainLabelD,
                                               label=scales::label_percent()(Percent))) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Total",y="% of total abundance")+
  geom_text(size = 4,position = position_stack(0.5))+
  scale_x_discrete(label=c(""))+
  scale_y_continuous(labels = scales::label_percent(),breaks=seq(0,1,by=0.1))+
  scale_fill_discrete(type=colorblind_palette)

#Show it
gg_distribution_percent_of_total

#Save to plot_path
ggsave(filename="Total_Percent.png", plot=gg_distribution_percent_of_total, path=paste(plot_path,"/Overview/Kingdom_Distribution",sep=""))

## Combined Plots
gg_distribution_combined_total <- (gg_distribution_count_of_total) / (gg_distribution_percent_of_total) +
                                    plot_annotation(tag_levels = 'A')
gg_distribution_combined_total
ggsave(filename="Total_Combined.png", plot=gg_distribution_combined_total, path=paste(plot_path,"/Appendix/Kingdom_Distribution",sep="")) 
