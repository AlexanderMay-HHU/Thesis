##Loading Libraries
library(ggplot2)
library(dplyr)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
colorblind_gradient_palette <- c("#df536b","#61d04f","#2297e6","#9928e5",
                                 "#ee9ced","#e69f00","#8ee6ff","#009e73",
                                 "#f0e442","#d55e00")



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_euk <- subset(nodes,nodes$Kingdom == "Eukaryota" | nodes$Kingdom == "Archaea")



#How many Unique of XY are there?
cat("Genus:",length(unique(nodes_euk$Genus)))
cat("Family:",length(unique(nodes_euk$Family)))
cat("Order:",length(unique(nodes_euk$Order)))


#Family
sum_family <- c()
names_family <- c()
for (fam in unique(nodes_euk$Family)){
  sum_family <- c(sum_family, sum(nodes_euk[nodes_euk$Family == fam,1],na.rm = TRUE))
  names_family <- c(names_family, fam)
}
abundance_family <- data.frame(names_family,sum_family)
top10_family <- c(tail(abundance_family %>% arrange(sum_family),10)$names_family)
print(top10_family)


#Order
sum_order <- c()
names_order <- c()
for (ord in unique(nodes_euk$Order)){
  sum_order <- c(sum_order, sum(nodes_euk[nodes_euk$Order == ord,1],na.rm = TRUE))
  names_order <- c(names_order, ord)
}
abundance_order <- data.frame(names_order,sum_order)
top10_order <- c(tail(abundance_order %>% arrange(sum_order),10)$names_order)
print(top10_order)

#Genus
sum_genus <- c()
names_genus <- c()
for (gen in unique(nodes_euk$Genus)){
  sum_genus <- c(sum_genus, sum(nodes_euk[nodes_euk$Genus == gen,1],na.rm = TRUE))
  names_genus <- c(names_genus, gen)
}
abundance_genus <- data.frame(names_genus,sum_genus)
top10_genus <- c(tail(abundance_genus %>% arrange(sum_genus),10)$names_genus)
print(top10_genus)




##Generate Stacked Barplot
#Family (This is going to be used)
gg_top10_family <- ggplot(subset(nodes_euk,nodes_euk$Family %in% top10_family),
                          aes(x=LouvainLabelD, y=Abundance4y, fill=Family)) + 
                          geom_bar(position="fill", stat="identity")+
                          labs(x="Louvain Cluster", y="fraction of total abundance of top 10 families")+ #title="Distribution of family for eukaryota in each Cluster"
                          scale_x_continuous(breaks = c(0,2:13))+
                          scale_fill_discrete(type=colorblind_gradient_palette)

#Show it
gg_top10_family
#Save to plot_path
ggsave(filename="Top10_Euk_Family.png", plot=gg_top10_family, path=paste(plot_path,"/Top10/",sep=""))




#Order
gg_top10_order <- ggplot(subset(nodes_euk,nodes_euk$Order %in% top10_order),
                          aes(x=LouvainLabelD, y=Abundance4y, fill=Order)) + 
                          geom_bar(position="fill", stat="identity")+
                          labs(x="Louvain Cluster", y="fraction of total abundance of top 10 orders")+ #title="Distribution of order for eukaryota in each Cluster"
                          scale_x_continuous(breaks = c(0,2:13))+
                          scale_fill_discrete(type=colorblind_gradient_palette)

#Show it
gg_top10_order
#Save to plot_path
ggsave(filename="Top10_Euk_Order.png", plot=gg_top10_order, path=paste(plot_path,"/Appendix/",sep=""))




#Genus
gg_top10_genus <- ggplot(subset(nodes_euk,nodes_euk$Genus %in% top10_genus),
                          aes(x=LouvainLabelD, y=Abundance4y, fill=Genus)) + 
                          geom_bar(position="fill", stat="identity")+
                          labs(x="Louvain Cluster", y="fraction of total abundance of top 10 genera")+ #title="Distribution of genus for eukaryota in each Cluster"
                          scale_x_continuous(breaks = c(0,2:13))+
                          scale_fill_discrete(type=colorblind_gradient_palette)

#Show it
gg_top10_genus
#Save to plot_path
ggsave(filename="Top10_Euk_Genus.png", plot=gg_top10_genus, path=paste(plot_path,"/Appendix/",sep=""))