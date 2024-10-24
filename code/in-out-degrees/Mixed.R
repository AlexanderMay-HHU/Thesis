##Loading Libraries
library(circlize)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"
plot_name_EukBac <- "Euk_Bac.png"
plot_name_BacEuk <- "Bac_Euk.png"
colorblind_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                        "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                        "#009e73","#f0e442","#0072b2","#d55e00",
                        "#999999")



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
bac_euk_edges_from_to_different <- bac_euk_edges[bac_euk_edges$from_clu != bac_euk_edges$to_clu,]



##Eukaryotes -> Bacteria
#Calc number of In&Out Degrees
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

adjacencyData_euk_bac <- with(euk_bac_edges_from_to_different, table(from_clu, to_clu))



## Generate Chord Plot
png(paste(plot_path,"/InOutDegrees/",plot_name_EukBac,sep=""), width=900, height=900, res=200)
chord_plot <- chordDiagram(adjacencyData_euk_bac,grid.col=colorblind_palette, transparency = 0.5)
dev.off()



##Bacteria -> Eukaryotes
#Calc number of In&Out Degrees
out_deg <- c()
in_deg <- c()
for (clu in 0:(max(bac_euk_edges_from_to_different$from_clu))){
  if(clu != 1){
    out_deg <- c(out_deg, length(bac_euk_edges_from_to_different[bac_euk_edges_from_to_different$from_clu == clu,9]))
    in_deg <- c(in_deg, length(bac_euk_edges_from_to_different[bac_euk_edges_from_to_different$to_clu == clu,2]))
    #cat(in_deg[if(clu!=0){clu}else{1}], " -> Cluster", clu, " -> ",
    #    out_deg[if(clu!=0){clu}else{1}], "\n",sep="")
  }else{
    next
  }
}

adjacencyData_bac_euk <- with(bac_euk_edges_from_to_different, table(from_clu, to_clu))



## Generate Chord Plot
png(paste(plot_path,"/InOutDegrees/",plot_name_BacEuk,sep=""), width=900, height=900, res=200)
chord_plot <- chordDiagram(adjacencyData_bac_euk,grid.col=colorblind_palette, transparency = 0.5)
dev.off()

