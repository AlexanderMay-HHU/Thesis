##Loading Libraries
library(ggplot2)
library(dplyr)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
colorblind_palette <- c("#df536b","#61d04f","#2297e6","#9928e5",
                        "#ee9ced","#e69f00","#8ee6ff","#009e73",
                        "#f0e442","#d55e00")



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_bac <- subset(nodes,nodes$Kingdom == "Bacteria")



#How many Unique of XY are there?
cat("Genus:",length(unique(nodes_bac$Genus)))
cat("Family:",length(unique(nodes_bac$Family)))
cat("Order:",length(unique(nodes_bac$Order)))



#Family
sum_family <- c()
names_family <- c()
for (fam in unique(nodes_bac$Family)){
  sum_family <- c(sum_family, sum(nodes_bac[nodes_bac$Family == fam,1],na.rm=TRUE))
  names_family <- c(names_family, fam)
  #cat(fam, ": ", sum(nodes_bac[nodes_bac$Family == fam,1],na.rm = TRUE),"\n",sep="")
}
abundance_family <- data.frame(names_family,sum_family)
top10_family <- c(tail(abundance_family %>% arrange(sum_family),10)$names_family)
print(top10_family)


#Order
sum_order <- c()
names_order <- c()
for (ord in unique(nodes_bac$Order)){
  sum_order <- c(sum_order, sum(nodes_bac[nodes_bac$Order == ord,1],na.rm=TRUE))
  names_order <- c(names_order, ord)
  #cat(ord, ": ", sum(nodes_bac[nodes_bac$Order == ord,1],na.rm = TRUE),"\n",sep="")
}
abundance_order <- data.frame(names_order,sum_order)
top10_order <- c(tail(abundance_order %>% arrange(sum_order),10)$names_order)
print(top10_order)

#Genus
sum_genus <- c()
names_genus <- c()
for (gen in unique(nodes_bac$Genus)){
  sum_genus <- c(sum_genus, sum(nodes_bac[nodes_bac$Genus == gen,1],na.rm=TRUE))
  names_genus <- c(names_genus, gen)
  #cat(gen, ": ", sum(nodes_bac[nodes_bac$Genus == gen,1],na.rm = TRUE),"\n",sep="")
}
abundance_genus <- data.frame(names_genus,sum_genus)
top10_genus <- c(tail(abundance_genus %>% arrange(sum_genus),10)$names_genus)
print(top10_genus)




##Generate Stacked Barplot
#Family (This is going to be used)
gg_top10_family <- ggplot(subset(nodes_bac,nodes_bac$Family %in% top10_family),
                          aes(x=LouvainLabelD, y=Abundance4y, fill=Family)) + 
                          geom_bar(position="fill", stat="identity")+
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
                          geom_vline(aes(xintercept=c(12)+0.5))+
                          labs(x="Louvain Cluster", y="% of total abundance of top 10 families",
                               title= "Bacteria Families")+
                          scale_x_continuous(breaks = c(0,2:13))+
                          scale_y_continuous(labels = scales::label_percent(),breaks=seq(0,1,by=0.1))+
                          scale_fill_discrete(type=colorblind_palette)

#Show it
gg_top10_family
#Save to plot_path
ggsave(filename="Bacteria_Family.png", plot=gg_top10_family, path=paste(plot_path,"/02_Top/",sep=""))






#Order
gg_top10_order <- ggplot(subset(nodes_bac,nodes_bac$Order %in% top10_order),
                          aes(x=LouvainLabelD, y=Abundance4y, fill=Order)) + 
                          geom_bar(position="fill", stat="identity")+
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
                          geom_vline(aes(xintercept=c(12)+0.5))+
                          labs(x="Louvain Cluster", y="% of total abundance of top 10 orders",
                               title= "Bacteria Order")+
                          scale_x_continuous(breaks = c(0,2:13))+
                          scale_y_continuous(labels = scales::label_percent(),breaks=seq(0,1,by=0.1))+
                          scale_fill_discrete(type=colorblind_palette)

#Show it
gg_top10_order
#Save to plot_path
ggsave(filename="Bacteria_Order.png", plot=gg_top10_order, path=paste(plot_path,"/00_Appendix/02_Top/Bacteria",sep=""))




#Genus
gg_top10_genus <- ggplot(subset(nodes_bac,nodes_bac$Genus %in% top10_genus),
                        aes(x=LouvainLabelD, y=Abundance4y, fill=Genus)) + 
                        geom_bar(position="fill", stat="identity")+
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
                        geom_vline(aes(xintercept=c(12)+0.5))+
                        labs(x="Louvain Cluster", y="% of total abundance of top 10 genera",
                             title= "Bacteria Genera")+
                        scale_x_continuous(breaks = c(0,2:13))+
                        scale_y_continuous(labels = scales::label_percent(),breaks=seq(0,1,by=0.1))+
                        scale_fill_discrete(type=colorblind_palette)

#Show it
gg_top10_genus
#Save to plot_path
ggsave(filename="Bacteria_Genus.png", plot=gg_top10_genus, path=paste(plot_path,"/00_Appendix/02_Top/Bacteria",sep=""))