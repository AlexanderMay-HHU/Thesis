##Loading Libraries
library(dplyr)

#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/"))


## Get Data
nodes_ccm <- read.csv("original/Convergent_Cross_Mapping/CCM_node_table.csv", header = T) 
edges_ccm <- read.csv("original/Convergent_Cross_Mapping/CCM_edge_table.csv", header = T) 
asv_overview <- read.csv("generated/abundance_overview.csv", header = T)


# Add Max Month,year, season to node table info
nodes_ccm <- nodes_ccm %>%
  left_join(
    asv_overview %>% select(OTU.number, max_abundance_year, max_abundance_month, max_abundance_season),
    by = c("name" = "OTU.number")
  )


# Range of P-value
summary(edges_ccm$p.value)


# Which nodes are missing?
missing_nodes <- setdiff(asv_overview$OTU.number,nodes_ccm$name)
missing_nodes <- asv_overview[asv_overview$OTU.number %in% missing_nodes,] 



# Did Kingdom ratio change
CCM_Kingdom_summary <- nodes_ccm %>% count(Kingdom, name = "Count") %>%
  mutate(Freq = Count/sum(Count))

Overall_Kingdom_summary <- asv_overview %>% count(Kingdom, name = "Count") %>%
  mutate(Freq = Count/sum(Count))

CCM_Kingdom_summary$Freq
Overall_Kingdom_summary$Freq