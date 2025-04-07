##Loading Libraries
library(dplyr)

#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/"))


## Get Data
nodes_con <- read.csv("original/Co-occurence/CON_node_table.csv", header = T) 
edges_con <- read.csv("original/Co-occurence/CON_edge_table.csv", header = T, sep=";") 
asv_overview <- read.csv("generated/abundance_overview.csv", header = T)


# Add Max Month,year, season to node table info
nodes_con <- nodes_con %>%
  left_join(
    asv_overview %>% select(OTU.number, max_abundance_year, max_abundance_month, max_abundance_season),
    by = c("Nodes" = "OTU.number")
  )


# Range of P-value
summary(edges_con$p.value)


# Which nodes are missing?
missing_nodes <- setdiff(asv_overview$OTU.number,nodes_con$Nodes)
missing_nodes <- asv_overview[asv_overview$OTU.number %in% missing_nodes,] 



# Did Kingdom ratio change
CON_Kingdom_summary <- nodes_con %>% count(Kingdom, name = "Count") %>%
  mutate(Freq = Count/sum(Count))

Overall_Kingdom_summary <- asv_overview %>% count(Kingdom, name = "Count") %>%
  mutate(Freq = Count/sum(Count))

CON_Kingdom_summary$Freq
Overall_Kingdom_summary$Freq