##Loading Libraries
library(ggplot2)
library(dplyr)

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
colorblind_palette <- palette.colors("Okabe-Ito",n=4)[2:4]



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA


#Sum of different Kingdoms in total
nodes_sum_total <- nodes %>% group_by(Kingdom) %>% summarise(ASVs = n_distinct(Sequence))
nodes_sum_total$LouvainLabelD <- c("Total")
nodes_sum_total <- nodes_sum_total %>% mutate(Freq = ASVs/sum(ASVs))



### Totals
## Generate Barplots
#Count numbers & Percentage of Total
gg_distribution_total_both <- ggplot(nodes_sum_total,
                                      aes(fill=Kingdom,
                                         y=ASVs,
                                         x=LouvainLabelD,
                                         label=paste0(ASVs,"\n(",scales::label_percent(accuracy=0.1)(Freq),")"))) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Total",y="# of ASVs")+
  geom_text(inherit.aes = T,size = 5,position = position_stack(0.5))+
  scale_x_discrete(label = c(""))#+
  #scale_fill_discrete(type=colorblind_palette)

#Show it
gg_distribution_total_both

#Save to plot_path
ggsave(filename="Total_Both.png", plot=gg_distribution_total_both,
       path=paste(plot_path,"/01_Kingdom_Comparison",sep=""))



#Count numbers of Total
gg_distribution_total_count <- ggplot(nodes_sum_total,
                                      aes(fill=Kingdom,
                                          y=ASVs,
                                          x=LouvainLabelD,
                                          label=ASVs)) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Total",y="# of ASVs")+
  geom_text(inherit.aes = T,size = 5,position = position_stack(0.5))+
  scale_x_discrete(label = c(""))#+
#scale_fill_discrete(type=colorblind_palette)

#Show it
gg_distribution_total_count

#Save to plot_path
ggsave(filename="Total_Count.png", plot=gg_distribution_total_count,
       path=paste(plot_path,"/00_Appendix/01_Kingdom_Comparison/Values",sep=""))



#Percentage of Total
gg_distribution_total_perc <- ggplot(nodes_sum_total,
                                     aes(fill=Kingdom,
                                         y=ASVs,
                                         x=LouvainLabelD,
                                         label=scales::label_percent(accuracy=0.1)(Freq))) + 
  geom_bar(position="stack", stat="identity")+
  labs(x="Total",y="# of ASVs")+
  geom_text(inherit.aes = T,size = 5,position = position_stack(0.5))+
  scale_x_discrete(label = c(""))#+
#scale_fill_discrete(type=colorblind_palette)

#Show it
gg_distribution_total_perc

#Save to plot_path
ggsave(filename="Total_Percent.png", plot=gg_distribution_total_perc,
       path=paste(plot_path,"/00_Appendix/01_Kingdom_Comparison/Values",sep=""))