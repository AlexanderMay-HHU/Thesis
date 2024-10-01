##Loading Libraries
library(ggplot2)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/InOutDegrees/Comparisons"
colorblind_palette <- palette.colors(palette = "Okabe-Ito")[c(4,7)]



nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)



#Get Interacting ASVs
for (interaction in 1:(length(edges$name))){
  #print(edges$name[interaction])
  edges$from_ASV[interaction] <- substring(edges$name[interaction],1,12)
  edges$to_ASV[interaction] <- substring(edges$name[interaction],31,42)
}


#Get different Interaction Types
for (interaction in 1:(length(edges$name))){
  switch(paste(substring(edges$from_ASV[interaction],1,3),"",substring(edges$to_ASV[interaction],1,3),sep=""),
         ArcArc={edges$interaction_type[interaction] <- "Euk"},
         ArcEuk={edges$interaction_type[interaction] <- "Euk"},
         ArcBac={edges$interaction_type[interaction] <- "BacEuk"},
         
         EukEuk={edges$interaction_type[interaction] <- "Euk"},
         EukArc={edges$interaction_type[interaction] <- "Euk"},
         EukBac={edges$interaction_type[interaction] <- "EukBac"},
         
         BacBac={edges$interaction_type[interaction] <- "Bac"},
         BacArc={edges$interaction_type[interaction] <- "BacEuk"},
         BacEuk={edges$interaction_type[interaction] <- "BacEuk"},
  )
}



#Split edges into different mixed interactions
euk_bac_edges <- edges[edges$interaction_type == "EukBac",]
bac_euk_edges <- edges[edges$interaction_type == "BacEuk",]


#Only get interactions between different clusters
euk_bac_edges_from_to_different <- euk_bac_edges[euk_bac_edges$from_clu != euk_bac_edges$to_clu,]


#Number of interactions of each ASV (Not used currently)
euk_bac_out_interactions_per_asv <- data.frame(table(euk_bac_edges_from_to_different$from_ASV))
euk_bac_in_interactions_per_asv <- data.frame(table(euk_bac_edges_from_to_different$to_ASV))


#Number of interactions to different clusters
out_deg <- c()
in_deg <- c()
for (clu in 0:(max(euk_bac_edges_from_to_different$from_clu))){
  if(clu != 1){
    out_deg <- c(out_deg, length(euk_bac_edges_from_to_different[euk_bac_edges_from_to_different$from_clu == clu,9]))
    in_deg <- c(in_deg, length(euk_bac_edges_from_to_different[euk_bac_edges_from_to_different$to_clu == clu,2]))
    #cat(in_deg[if(clu!=0){clu}else{1}], " -> Cluster", clu, " -> ",
    #    out_deg[if(clu!=0){clu}else{1}], "\n",sep="")
  }else{
    next
  }
}



#get Data into Format
cluster <- c(0,2:13)
type <- c(rep("to",13),rep("from",13))
count <- c(in_deg,out_deg)

in_out_deg <- data.frame(cluster,type,count)
colnames(in_out_deg) <- c("cluster", "interaction_type","count")





## generate Plots
stacked_number <- ggplot(in_out_deg, aes(fill=interaction_type, y=count, x=cluster))+
  geom_bar(position="stack", stat="identity")+
  scale_x_continuous(breaks = c(0,2:13))+
  scale_fill_manual(values=colorblind_palette)+
  labs(title="", y="Count",x="Cluster")+
  guides(fill=guide_legend(title="Interaction\n.... cluster"))+
  geom_text(aes(label=count), position = position_stack(vjust= 0.5), check_overlap = TRUE)

stacked_number

#Save to plot_path
ggsave(filename="Comparision_euk_bac_in_out_degrees_count.png", plot=stacked_number, path=paste(plot_path,"/Count/",sep=""))


stacked_percent <- ggplot(in_out_deg, aes(fill=interaction_type, y=count, x=cluster))+
  geom_bar(position="fill", stat="identity")+
  scale_x_continuous(breaks = c(0,2:13))+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values=colorblind_palette)+
  labs(title="",y="Distribution of in and outgoing interactions",
       x="Cluster")+
  guides(fill=guide_legend(title="Interaction\n.... cluster"))

stacked_percent

#Save to plot_path
ggsave(filename="Comparision_euk_bac_in_out_degrees_percent.png", plot=stacked_percent, path=paste(plot_path,"/Percent/",sep=""))

