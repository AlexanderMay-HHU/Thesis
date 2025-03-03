##Loading Libraries
library(ggplot2)
library(dplyr)

#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots")
setwd(paste0(project_folder,"/data"))


nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA


#Sum of different Kingdoms in total
nodes_sum_total <- nodes %>% group_by(Kingdom) %>% summarise(ASVs = n_distinct(Sequence))
nodes_sum_total$LouvainLabelD <- c("Total")
nodes_sum_total <- nodes_sum_total %>% mutate(Freq = ASVs/sum(ASVs))



### Totals
## Generate Barplots
# Count numbers & Percentage of Total
gg_total_none <- ggplot(nodes_sum_total,
                                      aes(x=LouvainLabelD,
                                         y=ASVs,
                                         fill=Kingdom)) + 
                  geom_bar(position="stack", stat="identity")+
                  labs(x="Total",y="# of ASVs")+
                  scale_x_discrete(label = c(""))+
                  theme(panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank())

gg_total_count <- gg_total_none +
                    geom_text(aes(label=ASVs),
                              size = 5, position = position_stack(0.5))
gg_total_perc <- gg_total_none +
                    geom_text(aes(label=scales::label_percent(accuracy=0.1)(Freq)),
                              size = 5, position = position_stack(0.5))
gg_total_both <- gg_total_none +
                    geom_text(aes(label=paste0(ASVs,"\n(",scales::label_percent(accuracy=0.1)(Freq),")")),
                              size = 5, position = position_stack(0.5))



#Show it
gg_total_none
gg_total_count
gg_total_perc
gg_total_both


#Save to plot_path
ggsave(filename="Total_None.png", plot=gg_total_none,
       path=paste(plot_path,"/00_Appendix/01_Kingdom_Comparison",sep=""))
ggsave(filename="Total_Count.png", plot=gg_total_count,
       path=paste(plot_path,"/00_Appendix/01_Kingdom_Comparison",sep=""))
ggsave(filename="Total_Percent.png", plot=gg_total_perc,
       path=paste(plot_path,"/00_Appendix/01_Kingdom_Comparison",sep=""))
ggsave(filename="Total_Both.png", plot=gg_total_both,
       path=paste(plot_path,"/01_Kingdom_Comparison",sep=""))