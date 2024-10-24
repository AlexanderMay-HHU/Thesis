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

#Sum of different Kingdoms in total
nodes_sum_total <- nodes %>% group_by(Kingdom) %>% summarise(Abundance4y = sum(Abundance4y))
nodes_sum_total$LouvainLabelD <- c("Total")


###Clusters
## Generate Barplots
#Count numbers
gg_distribution_count <- ggplot(nodes_sum, aes(fill=Kingdom, y=Abundance4y, x=LouvainLabelD)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Louvain Cluster",y="Abundance")+
  scale_x_continuous(breaks = c(0,2:13))+
  scale_y_continuous(breaks = seq(0,10000,by=1000))+
  scale_fill_discrete(type=colorblind_palette)

#Show it
gg_distribution_count

#Save to plot_path
ggsave(filename="Kingdom_Distribution_count.png", plot=gg_distribution_count, path=paste(plot_path,"/Overview",sep=""))



#Percentage
gg_distribution_percent <- ggplot(nodes_sum, aes(fill=Kingdom, y=Abundance4y, x=LouvainLabelD)) + 
  geom_bar(position="fill", stat="identity")+
  labs(x="Louvain Cluster",y="% of total abundance")+
  scale_x_continuous(breaks = c(0,2:13))+
  scale_y_continuous(labels = scales::label_percent(),breaks=seq(0,1,by=0.1))+
  scale_fill_discrete(type=colorblind_palette)

#Show it
gg_distribution_percent

#Save to plot_path
ggsave(filename="Kingdom_Distribution_percent.png", plot=gg_distribution_percent, path=paste(plot_path,"/Overview",sep=""))


## Combined Plots
gg_distribution_combined <- gg_distribution_count+ theme(legend.position = "none") + gg_distribution_percent +
                              plot_annotation(tag_levels = 'A')
gg_distribution_combined
ggsave(filename="Combined.png", plot=gg_distribution_combined, path=paste(plot_path,"/Appendix/Kingdom_Distribution",sep="")) 



### Totals
## Generate Barplots
#Count numbers of Total
gg_distribution_count_of_total <- ggplot(nodes_sum_total, aes(fill=Kingdom, y=Abundance4y, x=LouvainLabelD)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Total",y="Abundance")+
  scale_x_discrete(label = c(""))+
  scale_y_continuous(breaks = seq(0,30000,by=2500))+
  scale_fill_discrete(type=colorblind_palette)

#Show it
gg_distribution_count_of_total

#Save to plot_path
ggsave(filename="Count_of_total.png", plot=gg_distribution_count_of_total, path=paste(plot_path,"/Appendix/Kingdom_Distribution",sep=""))


#Percentage of Total
gg_distribution_percent_of_total <- ggplot(nodes_sum_total, aes(fill=Kingdom, y=Abundance4y,x=LouvainLabelD)) + 
  geom_bar(position="fill", stat="identity")+
  labs(x="Total",y="% of total abundance")+
  scale_x_discrete(label=c(""))+
  scale_y_continuous(labels = scales::label_percent(),breaks=seq(0,1,by=0.1))+
  scale_fill_discrete(type=colorblind_palette)

#Show it
gg_distribution_percent_of_total

#Save to plot_path
ggsave(filename="Percent_of_total.png", plot=gg_distribution_percent_of_total, path=paste(plot_path,"/Appendix/Kingdom_Distribution",sep=""))

## Combined Plots
gg_distribution_combined_total <- gg_distribution_count_of_total+ theme(legend.position = "none") + gg_distribution_percent_of_total +
                                    plot_annotation(tag_levels = 'A')
gg_distribution_combined_total
ggsave(filename="Combined_total.png", plot=gg_distribution_combined_total, path=paste(plot_path,"/Appendix/Kingdom_Distribution",sep="")) 
