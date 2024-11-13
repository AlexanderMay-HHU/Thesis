##Loading Libraries
library("dplyr")

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")


colorblind_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                        "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                        "#009e73","#f0e442","#0072b2","#d55e00",
                        "#999999")


## Get Data
nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA


#switch to own color palette
for (cluster in unique(nodes$LouvainLabelD)){
  nodes[nodes$LouvainLabelD == cluster,"louvain_label_color"] <- colorblind_palette[ifelse(cluster==0,1,cluster)]
}

#get own color palette with key for export
adjusted_colors <- nodes %>% select(name,louvain_label_color)


# export color palette
write.csv(adjusted_colors, "generated/05_Network_Additions/Adjusted_Colors.csv")