##Loading Libraries
library(ggplot2)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)



## Generate Barplots
#Total numbers
gg_distribution_total <- ggplot(nodes, aes(fill=Kingdom, y=Abundance4y, x=LouvainLabelD)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Louvain Cluster",y="Abundance")+#,title="Distribution of Kingdoms in each Cluster"
  scale_x_continuous(breaks = c(0,2:13))

#Show it
gg_distribution_total

#Save to plot_path
ggsave(filename="Kingdom_Distribution_total.png", plot=gg_distribution_total, path=paste(plot_path,"/Overview",sep=""))



#Percentage
gg_distribution_percent <- ggplot(nodes, aes(fill=Kingdom, y=Abundance4y, x=LouvainLabelD)) + 
  geom_bar(position="fill", stat="identity")+
  labs(x="Louvain Cluster",y="fraction of total abundance")+#,title="Distribution of Kingdoms in each Cluster"
  scale_x_continuous(breaks = c(0,2:13))

#Show it
gg_distribution_percent

#Save to plot_path
ggsave(filename="Kingdom_Distribution_percent.png", plot=gg_distribution_percent, path=paste(plot_path,"/Overview",sep=""))