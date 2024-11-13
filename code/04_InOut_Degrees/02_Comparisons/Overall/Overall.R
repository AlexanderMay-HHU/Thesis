##Loading Libraries
library(ggplot2)
library(dplyr)
library(tidyr)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/"
colorblind_palette <- palette.colors(palette = "Okabe-Ito")[c(4,7)]



edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)


# declutter name of edges into from and to ASVs
edges <- edges %>% mutate(from_ASV = substring(name,1,12), to_ASV = substring(name,31,42))

#Only get interactions between different clusters
edges_from_to_different <- edges[edges$from_clu != edges$to_clu,]



## (Not used currently)
# Number of interactions of each ASV
#out_interactions_per_asv <- data.frame(table(edges_from_to_different$from_ASV))
#in_interactions_per_asv <- data.frame(table(edges_from_to_different$to_ASV))



## Number of interactions from/to different clusters
# Make adjacency matrix
adjacencyData <- with(edges_from_to_different, table(from_clu, to_clu)) %>% as.data.frame()
adjacencyData <- adjacencyData %>% pivot_wider(names_from =from_clu ,values_from = Freq)
adjacencyData$to_clu <- NULL
# Get In and Out Interactions
in_deg <- apply(adjacencyData,1,sum)
out_deg <- unname(apply(adjacencyData,2,sum))

#Get data into format
cluster <- c(0,2:13)
type <- c(rep("to",13),rep("from",13))
count <- c(in_deg,out_deg)

in_out_deg <- data.frame(type,cluster,count)
colnames(in_out_deg) <- c("interaction_type", "cluster", "count")



## Generate Plots
dodge_number_no_values <- ggplot(in_out_deg, aes(fill=interaction_type,
                                         y=count,
                                         x=cluster))+
  geom_bar(position="dodge", stat="identity")+
  scale_x_continuous(breaks = c(0,2:13))+
  scale_y_continuous(breaks=seq(0,300,25))+
  scale_fill_manual(values=colorblind_palette)+
  labs(x="Cluster", y="Interactions", title = "Overall Interactions")+
  guides(fill=guide_legend(title="Interaction\n.... cluster"))+
  geom_vline(aes(xintercept=c(0)+0.5))+
  geom_vline(aes(xintercept=c(1)+0.5))+
  geom_vline(aes(xintercept=c(2)+0.5))+
  geom_vline(aes(xintercept=c(3)+0.5))+
  geom_vline(aes(xintercept=c(4)+0.5))+
  geom_vline(aes(xintercept=c(5)+0.5))+
  geom_vline(aes(xintercept=c(6)+0.5))+
  geom_vline(aes(xintercept=c(7)+0.5))+
  geom_vline(aes(xintercept=c(8)+0.5))+
  geom_vline(aes(xintercept=c(9)+0.5))+
  geom_vline(aes(xintercept=c(10)+0.5))+
  geom_vline(aes(xintercept=c(11)+0.5))+
  geom_vline(aes(xintercept=c(12)+0.5))

dodge_number <- dodge_number_no_values+
  geom_text(aes(label=count), position = position_dodge(width= 0.9), vjust= -0.5)

dodge_number_no_values
dodge_number

#Save to plot_path
ggsave(filename="Count.png", plot=dodge_number_no_values,
       path=paste0(plot_path,"00_Appendix/04_InOut_Degrees/02_Comparisons/No_Values/Overall/"))
ggsave(filename="Count.png", plot=dodge_number,
       path=paste0(plot_path,"00_Appendix/04_InOut_Degrees/02_Comparisons/Values/Overall/"))



### NEEDS LABELS
stacked_percent <- ggplot(in_out_deg, aes(fill=interaction_type,
                                          y=count,
                                          x=cluster))+
  geom_bar(position="fill", stat="identity")+
  scale_x_continuous(breaks = c(0,2:13))+
  scale_y_continuous(labels = scales::percent, breaks=seq(0,1,by=0.1))+
  scale_fill_manual(values=colorblind_palette)+
  labs(x="Cluster", y="Distribution of in and outgoing interactions",
       title= "Overall Interaction Ratio")+
  guides(fill=guide_legend(title="Interaction\n.... cluster"))

stacked_percent

#Save to plot_path
ggsave(filename="Percent.png", plot=stacked_percent, path=paste0(plot_path,"00_Appendix/04_InOut_Degrees/02_Comparisons/Values/Overall/"))