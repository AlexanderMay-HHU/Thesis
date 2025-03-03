## Loading Libraries
library(readxl)


#Set Working Directory
project_folder <- dirname(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data"))


colorblind_gradient_palette <- c("#3300AA","#FFFFBF","#AA0033")


## Get Data
abundance_arc <- read_excel("Abundance_Taxa.xlsx", sheet = "Archaea")
abundance_arc <- abundance_arc[,7:138]

abundance_bac <- read_excel("Abundance_Taxa.xlsx", sheet = "Bacteria")
abundance_bac <- abundance_bac[,8:153]

abundance_euk <- read_excel("Abundance_Taxa.xlsx", sheet = "Eukaryotic phytoplankton")
abundance_euk <- abundance_euk[,8:147]

nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA
nodes_clusters_only <- nodes[,c("name","LouvainLabelD")]


# Function to format Colname to Date Format
data_conv <- function(data, i) {
    ifelse(i==1,"name",
    colnames(data)[i] |>
    as.integer() |>
    as.Date(origin = "1899-12-30") |>
    as.character())}


# Formatting Colnames
for (i in 1:132){
  colnames(abundance_arc)[i] <- data_conv(abundance_arc, i)
}
for (i in 1:146){
  colnames(abundance_bac)[i] <- data_conv(abundance_bac, i)
}
for (i in 1:140){
  colnames(abundance_euk)[i] <- data_conv(abundance_euk, i)
}


# Prefix OTUs
abundance_arc$name <- paste0("Arc_", abundance_arc$name)
abundance_arc$Cluster <- NA
abundance_bac$name <- paste0("Bac_", abundance_bac$name)
abundance_bac$Cluster <- NA
abundance_euk$name <- paste0("Euk_", abundance_euk$name)
abundance_euk$Cluster <- NA



## Make complete abundance dataframe
all_colnames <- unique(c(colnames(abundance_arc),colnames(abundance_bac),colnames(abundance_euk)))
all_colnames <- c("LouvainLabelD","name",all_colnames[2:147])
all_abundances <- data.frame(matrix(ncol=length(all_colnames),nrow=0))
colnames(all_abundances) <- all_colnames

# Set Column Types
for (col_types in 1:148){
  all_abundances[,col_types] <- as.numeric()  
}
all_abundances$name <- as.character()


add_clusters_to_abundance_table <- function(abundance_data){
  #head(abundance_data)
  for (name in abundance_data$name){
    abundance_data[which(abundance_arc[,1] == name),"Cluster"] <- nodes[nodes$name== name,"LouvainLabelD"]
    print(paste0("Name: ",name))
    print(paste0("Nodes: ",nodes[nodes$name== name,"LouvainLabelD"]))
    print(paste0("Abundance: ",abundance_data[which(abundance_arc[,1] == name),"Cluster"]))
  }
}



add_clusters_to_abundance_table(abundance_arc)


abundance_arc[,1]
abundance_arc[abundance_arc$name == "Arc_Otu00001","Cluster"] <- nodes[nodes$name== "Arc_Otu00001","LouvainLabelD"]



# NMDS
set.seed(123)
nmds_arc <- metaMDS(abundance_arc[,2:132] %>% as.matrix(), distance = "bray")
nmds_arc
plot(nmds_arc)


nmds_arc2 <- metaMDS(nodes_arc[], distance = "bray")
nmds_arc2
plot(nmds_arc2)