##Loading Libraries
library(ggplot2)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)



#Generate Stacked Barplot
gg_distribution <- ggplot(nodes, aes(fill=Kingdom, y=Abundance4y, x=LouvainLabelD)) + 
  geom_bar(position="fill", stat="identity")+
  labs(x="Louvain Cluster",y="fraction of total abundance",title="Distribution of Kingdoms in each Cluster")+
  scale_x_continuous(breaks=seq(0,13,by=1))

#Show it
gg_distribution

#Save to plot_path
ggsave(filename="Kingdom_Distribution.png", plot=gg_distribution, path=paste(plot_path,"/Overview",sep=""))