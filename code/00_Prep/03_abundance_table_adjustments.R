## Loading Libraries
library(dplyr)


## Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots")
setwd(paste0(project_folder,"/data/"))


# Import Data
overall_louv <- read.csv("original/overall.csv", header = T)

abundance_table <- read.csv("original/abundance.csv", header = T) 
abundance_maxima <- read.csv("generated/abundance_maxima.csv", header = T)


## Make new dataframe that contains ASVs in Rows
# Transpose to make each row a ASV with attributes in columns
abundance_asv_rows <- as.data.frame(t(abundance_table))
colnames(abundance_asv_rows) <- abundance_asv_rows[1,]
abundance_asv_rows <- abundance_asv_rows[2:dim(abundance_asv_rows)[1],]
abundance_asv_rows$OTU.number <- rownames(abundance_asv_rows)
rownames(abundance_asv_rows) <- NULL
abundance_asv_rows <- abundance_asv_rows[,c(dim(abundance_asv_rows)[2],1:(dim(abundance_asv_rows)[2]-1))]
for(i in 2:126){
  abundance_asv_rows[,i] <- as.numeric(abundance_asv_rows[,i])
}



# Get number of observations & total abundance
abundance_total_and_observations <- function(abundance_tbl){
  # Total abundance
  abundance_tbl$Abundance_total <- rowSums(abundance_tbl[sapply(abundance_tbl, is.numeric)], na.rm = TRUE)
  # Number of observations of ASV
  abundance_tbl$Number_of_observations <- rowSums(abundance_tbl[,2:126]>0, na.rm = T)
  return (abundance_tbl)
}



## Add taxa info, Louvain cluster and Abdundance Data to each ASV
# Abundance Data (Total, and observations)
abundance_asv_rows <- abundance_total_and_observations(abundance_asv_rows)
# (max-month, max-year, max-season)
abundance_asv_rows <- abundance_asv_rows %>% left_join(abundance_maxima %>%
                                                   select(ASV, max_abundance_year, max_abundance_month, max_abundance_season),
                                                 by = c("OTU.number" = "ASV"))

# Taxa info (Kingdom, Phylum, Class, Order, Family, Genus, Sequence)
abundance_asv_rows <- abundance_asv_rows %>% left_join(overall_louv %>% 
                                           select(Nodes, Kingdom, Phylum, Class, Order, Family, Genus, Sequence),
                                           by = c("OTU.number" = "Nodes"))

# Louvain cluster
abundance_asv_rows$LouvainLabelD <- overall_louv$LouvainLabelD[match(overall_louv$Nodes,abundance_asv_rows$OTU.number)]
# Add Kingdoms from missing clusters
abundance_asv_rows <- abundance_asv_rows %>%
  mutate(Kingdom = case_when(
    substr(OTU.number, 1, 3) == "Arc" ~ "Archaea",
    substr(OTU.number, 1, 3) == "Bac" ~ "Bacteria",
    substr(OTU.number, 1, 3) == "Euk" ~ "Eukaryota"
  ))



# Abundance Table with ASV as rows and added louvain clusters + oberservations
abundance_data_louvain <- abundance_asv_rows[,c(1:126,128,139)]


# Abundance table with ASV as rows containing  taxa info, Louvain clusters, Total Abundance, Number of Observations + maxima of asv for year,month and season
abundance_overview <- abundance_asv_rows[,c("OTU.number","Kingdom","Phylum","Class","Family","Genus","Sequence",
                                 "LouvainLabelD",
                                 "Abundance_total","Number_of_observations","max_abundance_year","max_abundance_month","max_abundance_season")]

# Save data
write.csv(abundance_data_louvain,"generated/abundance_asv_row_with_louvain_observations.csv",row.names = F)
write.csv(abundance_overview,"generated/abundance_overview.csv",row.names = F)
