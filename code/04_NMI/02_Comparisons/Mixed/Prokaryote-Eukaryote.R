##Loading Libraries
library(ggplot2)
library(dplyr)
library(tidyr)


#Set Working Directory
project_folder <- dirname(dirname(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data"))


colorblind_palette <- palette.colors(palette = "Okabe-Ito")[c(4,7)]



edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)


# declutter name of edges into from and to ASVs
edges <- edges %>% mutate(from_ASV = substring(name,1,12), to_ASV = substring(name,31,42))

#Split edges into Bacteria-Eukaryote only
prokaryote_eukaryote_edges <- edges %>% filter(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "BacEuk" |
                                    paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "ArcEuk")

#Only get interactions between different clusters
prokaryote_eukaryote_edges_from_to_different <- prokaryote_eukaryote_edges[prokaryote_eukaryote_edges$from_clu != prokaryote_eukaryote_edges$to_clu,]



## (Not used currently)
# Number of interactions of each ASV
#bac_euk_out_interactions_per_asv <- data.frame(table(prokaryote_eukaryote_edges_from_to_different$from_ASV))
#bac_euk_in_interactions_per_asv <- data.frame(table(prokaryote_eukaryote_edges_from_to_different$to_ASV))



## Number of interactions from/to different clusters
# Make adjacency matrix
adjacencyData <- with(prokaryote_eukaryote_edges_from_to_different, table(from_clu, to_clu)) %>% as.data.frame()
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
                                                 x=as.factor(cluster)))+
  geom_bar(position="dodge", stat="identity")+
  scale_y_continuous(limits = c(0,160), breaks=seq(0,160,20))+
  scale_fill_manual(values=colorblind_palette)+
  labs(x="Cluster", y="Interactions", title = "Prokaryote-Eukaryote Interactions")+
  guides(fill=guide_legend(title="Interaction\n.... cluster"))+
  geom_vline(xintercept = seq(1.5, 12.5, by = 1))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

dodge_number <- dodge_number_no_values+
  geom_text(aes(label=count), position = position_dodge(width= 0.9), vjust= -0.5)

dodge_number_no_values
dodge_number

#Save to plot_path
ggsave(filename="Prokaryote-Eukaryote_Count.png", plot=dodge_number_no_values,
       path=paste0(plot_path,"04_NMI/02_Comparisons/Mixed/"))
ggsave(filename="Prokaryote-Eukaryote_Count_values.png", plot=dodge_number,
       path=paste0(plot_path,"00_Appendix/04_NMI/02_Comparisons/Mixed/"))



### NEEDS LABELS
stacked_percent <- ggplot(in_out_deg, aes(fill=interaction_type,
                                          y=count,
                                          x=as.factor(cluster)))+
  geom_bar(position="fill", stat="identity")+
  scale_y_continuous(labels = scales::percent, breaks=seq(0,1,by=0.1))+
  scale_fill_manual(values=colorblind_palette)+
  labs(x="Cluster", y="Distribution of in and outgoing interactions",
       title= "Prokaryote-Eukaryote Interaction Ratio")+
  guides(fill=guide_legend(title="Interaction\n.... cluster"))+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

stacked_percent

#Save to plot_path
ggsave(filename="Prokaryote-Eukaryote_Percent.png", plot=stacked_percent,
       path=paste(plot_path,"04_NMI/02_Comparisons/Mixed",sep=""))