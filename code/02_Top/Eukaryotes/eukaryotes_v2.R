##Loading Libraries
library(ggplot2)
library(dplyr)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
colorblind_palette <- c("#df536b","#61d04f","#2297e6","#9928e5",
                        "#ee9ced","#e69f00","#8ee6ff","#009e73",
                        "#f0e442","#d55e00")

top10_palette <- c("#1F77B4FF", "#FF7F0EFF", "#2CA02CFF",
                   "#D62728FF", "#9467BDFF", "#8C564BFF",
                   "#E377C2FF", "#7F7F7FFF", "#BCBD22FF",
                   "#17BECFFF")


nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_euk <- subset(nodes,nodes$Kingdom == "Eukaryota")


# Remove Unknown ASVs
nodes_euk_noUnknown_family <- nodes_euk[which(!(is.na(nodes_euk$Family))),]
nodes_euk_noUnknown_family <- nodes_euk_noUnknown_family[nodes_euk_noUnknown_family$Family != "Unknown_Family",]

nodes_euk_noUnknown_order <- nodes_euk[which(!(is.na(nodes_euk$Order))),]
nodes_euk_noUnknown_order <- nodes_euk_noUnknown_order[nodes_euk_noUnknown_order$Order != "Unknown_Order",]

nodes_euk_noUnknown_genus <- nodes_euk[which(!(is.na(nodes_euk$Genus))),]
nodes_euk_noUnknown_genus <- nodes_euk_noUnknown_genus[nodes_euk_noUnknown_genus$Genus != "Unknown_Genus",]



#How many Unique of XY are there?
cat("There are # different\n","Genera:",length(unique(nodes_euk$Genus)),"\n\t",
    "Families:",length(unique(nodes_euk$Family)),"\n\t\t",
    "Orders:",length(unique(nodes_euk$Order)),"\n")


#Family
head(nodes_euk_noUnknown_family)[,c("Abundance4y","Family")]
euk_family_overall <- nodes_euk_noUnknown_family %>% group_by(Family) %>% summarize(total_Abundance = sum(Abundance4y))
euk_family_overall <- euk_family_overall %>% mutate(Freq = total_Abundance/sum(total_Abundance))
euk_family_overall$Count <- nodes_euk_noUnknown_family %>% group_by(Family) %>% summarise(Count = n()) %>% select(Count)
euk_family_overall <- euk_family_overall %>% arrange(total_Abundance)

# Top 10 Overall only
euk_family_top10_overall <- tail(euk_family_overall,10)
euk_family_top10_overall <- euk_family_top10_overall %>% mutate(Freq = total_Abundance/sum(total_Abundance))


# Top 10 by Cluster
euk_family_top10 <- subset(nodes_euk_noUnknown_family, nodes_euk_noUnknown_family$Family %in% euk_family_top10_overall$Family)
euk_family_top10 <- euk_family_top10 %>% group_by(Family,LouvainLabelD) %>% summarize(Abundance = sum(Abundance4y))
euk_family_top10 <- euk_family_top10 %>% group_by(LouvainLabelD) %>% mutate(FreqInCluster = Abundance/sum(Abundance))
euk_family_top10 <- euk_family_top10 %>% arrange(LouvainLabelD)
euk_family_top10

# Anteile
sum(euk_family_top10_overall$total_Abundance)/sum(nodes_euk$Abundance4y)

sum(nodes_euk$Abundance4y)
sum(nodes_euk_noUnknown_family$Abundance4y)
sum(euk_family_top10_overall$total_Abundance)


#Order
head(nodes_euk_noUnknown_order)[,c("Abundance4y","Order")]
euk_order<- nodes_euk_noUnknown_order %>% group_by(Order,LouvainLabelD) %>% summarize(Abundance = sum(Abundance4y))
euk_order <- euk_order %>% group_by(LouvainLabelD) %>% mutate(FreqInCluster = Abundance/sum(Abundance))
euk_order <- euk_order %>% arrange(LouvainLabelD)
euk_order



#Genus
head(nodes_euk_noUnknown_genus)[,c("Abundance4y","Genus")]




##Generate Stacked Barplot
#Family (This is going to be used)
# Overall
gg_family_overall <- ggplot(euk_family_top10_overall,
                            aes(fill=Family,
                                y=Freq,
                                x=Family,
                                label=paste0(round(Freq,3)*100,"%\n",Count$Count))) + 
  geom_bar(position="dodge", stat="identity")+ # Cluster Kingdoms Barplot
  scale_fill_manual(values = top10_palette)+
  labs(x="Family",y="% of total identifies archaea ASVs",
       title = "Eukaryote families overall")+ # Axis labels
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(labels = scales::label_percent(suffix=""),breaks=seq(0,1,by=0.1))+#Y-Axis values
  theme(legend.key.size = unit(0.65, units = "cm"), legend.position="none")+ #Legend size adjustments removing legend here
  geom_text(inherit.aes = T,size = 4,position = position_dodge(0.9),vjust=0.5) #Barplot text value

gg_family_overall


ggsave(filename="Family_Overall.png", plot=gg_family_overall, path=paste(plot_path,"/02_Top/Eukaryote",sep=""))

# Cluster
gg_top_family <- ggplot(euk_family_top10,
                        aes(x=LouvainLabelD,
                            y=FreqInCluster,
                            fill=Family)) + 
  geom_bar(stat="identity")+
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
  labs(x="Louvain Cluster", y="% of total abundance of top identified families",
       title= "Eukaryote Families")+
  scale_x_continuous(breaks = c(0,2:13))+
  scale_y_continuous(labels = scales::label_percent(suffix=""))+ #Y-Axis values (Percent)
  scale_fill_discrete(type=colorblind_palette)

#Show it
gg_top_family
#Save to plot_path
ggsave(filename="Family_Cluster.png", plot=gg_top_family, path=paste(plot_path,"/02_Top/Eukaryote",sep=""))



gg_top_family_numbers <- gg_top_family +
                            geom_text(label= round(euk_family_top10$FreqInCluster,4)*100,
                                      size = 4, position = position_stack(vjust = 0.5))

#Show it
gg_top_family_numbers
#Save to plot_path
ggsave(filename="Family_cluster_numbers.png", plot=gg_top_family_numbers, path=paste(plot_path,"/02_Top/Eukaryote",sep=""))



#Order
gg_top10_order <- ggplot(subset(nodes_eukaryotes,nodes_eukaryotes$Order %in% top10_order),
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
                             title= "Eukaryote Order")+
                        scale_x_continuous(breaks = c(0,2:13))+
                        scale_y_continuous(labels = scales::label_percent(),breaks=seq(0,1,by=0.1))+
                        scale_fill_discrete(type=colorblind_gradient_palette)

#Show it
gg_top10_order
#Save to plot_path
ggsave(filename="Eukaryotes_Order.png", plot=gg_top10_order, path=paste(plot_path,"/00_Appendix/02_Top/Eukaryotes",sep=""))




#Genus
gg_top10_genus <- ggplot(subset(nodes_eukaryotes,nodes_eukaryotes$Genus %in% top10_genus),
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
                        labs(x="Louvain Cluster", y="% of total abundance of top 10 genera", title= "Eukaryote Genera")+
                        scale_x_continuous(breaks = c(0,2:13))+
                        scale_y_continuous(labels = scales::label_percent(),breaks=seq(0,1,by=0.1))+
                        scale_fill_discrete(type=colorblind_gradient_palette)

#Show it
gg_top10_genus
#Save to plot_path
ggsave(filename="Eukaryotes_Genus.png", plot=gg_top10_genus, path=paste(plot_path,"/00_Appendix/02_Top/Eukaryotes",sep=""))