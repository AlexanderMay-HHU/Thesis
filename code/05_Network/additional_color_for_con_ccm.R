##Loading Libraries
library(dplyr)

#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/"))


## Get Data
nodes_con <- read.csv("original/Co-occurence/CON_node_table.csv", header = T) 
nodes_ccm <- read.csv("original/Convergent_Cross_Mapping/CCM_node_table.csv", header = T)
asv_overview <- read.csv("generated/abundance_overview.csv", header = T)
#Replace Environment_Condition with NA
nodes_con[nodes_con == "Environment_Condition"] <- NA
nodes_ccm[nodes_ccm == "Environment_Condition"] <- NA
# Remove old max abundance months
nodes_ccm <- nodes_ccm[,-c(11,12)]


## Colors for different Visualisations
# Kingdom
color_kingdoms <- c("Archaea"="#FD224D",
                    "Bacteria"="#2C7C2B",
                    "Eukaryota"="#1FB8FF")

# Max abundance Year
color_year <- c("2007" = "#FF6F61",
                "2008" = "#6B5B93",
                "2009" = "#88B04B",
                "2010" = "#7986CB",
                "2011" = "#A0522D",
                "2012" = "#64B5F6",
                "2013" = "#FF8C00",
                "2014" = "#A1887F",
                "2015" = "#D32F2F")

# Max abundance Season (Season were defined by Spring: Mar-May | Summer: Jun-Aug | Autumn: Sep-Nov | Winter: Dec-Feb)
color_season <- c("Spring"="#238b45",
                  "Summer"="#c994c7",
                  "Autumn"="#cc4c02",
                  "Winter"="#2171b5")

# Monthly colors (vary by 40% lighter/darker of their base season color)
color_month <- c("1"="#2171b5",
                 "2"="#113b5f",
                 "3"="#44d072",
                 "4"="#238b45",
                 "5"="#0e3a1d",
                 "6"="#ebd8ea",
                 "7"="#c994c7",
                 "8"="#a453a1",
                 "9"="#fd8037",
                 "10"="#cc4c02",
                 "11"="#672601",
                 "12"="#5ba3e1")

# Clusters
color_clusters <- c("#00ffff","#191970","#c71585","#2f4f4f",
                    "#ff0000","#0000ff","#bdb76b","#006400",
                    "#ffa500","#1e90ff","#ffb6c1","#00fa9a",
                    "#ffff00","#631919","#ff00ff")



# Add Max Month,year, season to node table info
nodes_con <- nodes_con %>%
  left_join(
    asv_overview %>% select(OTU.number, max_abundance_year, max_abundance_month, max_abundance_season),
    by = c("Nodes" = "OTU.number")
  )

nodes_ccm <- nodes_ccm %>%
  left_join(
    asv_overview %>% select(OTU.number, max_abundance_year, max_abundance_month, max_abundance_season),
    by = c("name" = "OTU.number")
  )

# Add colors
con_colors <- nodes_con %>%
  mutate(color_kingdom = color_kingdoms[Kingdom],
         color_cluster = color_clusters[LouvainLabelD+1],
         color_year = color_year[as.character(max_abundance_year)],
         color_month = color_month[as.character(max_abundance_month)],
         color_season = color_season[as.character(max_abundance_season)]) %>%
  select(Nodes,
         Kingdom,color_kingdom,
         LouvainLabelD,color_cluster,
         max_abundance_year,color_year,
         max_abundance_month,color_month,
         max_abundance_season,color_season)

ccm_colors <- nodes_ccm %>%
  mutate(color_kingdom = color_kingdoms[Kingdom],
         color_cluster = color_clusters[LouvainLabelD+1],
         color_year = color_year[as.character(max_abundance_year)],
         color_month = color_month[as.character(max_abundance_month)],
         color_season = color_season[as.character(max_abundance_season)]) %>%
  select(name,
         Kingdom,color_kingdom,
         LouvainLabelD,color_cluster,
         max_abundance_year,color_year,
         max_abundance_month,color_month,
         max_abundance_season,color_season)


# export color palette
write.csv(con_colors, paste0(project_folder,"/data/","generated/05_Network/Adjusted_Colors_CON.csv"))
write.csv(ccm_colors, paste0(project_folder,"/data/","generated/05_Network/Adjusted_Colors_CCM.csv"))