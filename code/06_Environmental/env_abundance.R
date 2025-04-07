##Loading Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(corrplot)

#### IMPORT DATA
#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/"))



abundance <- read.csv("generated/abundance_asv_row_with_louvain_observations.csv")
# remove no. observations
abundance <- abundance[,c(2:126,128)]


# Group abundances by cluster
abundance_dates_row_by_cluster <- abundance %>%
  pivot_longer(
    cols = -LouvainLabelD,               
    names_to = "date",                   
    names_prefix = "X",                  
    values_to = "abundance"              
  ) %>%
  group_by(date, LouvainLabelD) %>%      
  summarize(total_abundance = sum(abundance, na.rm = TRUE)) %>%  
  pivot_wider(
    names_from = LouvainLabelD,          
    values_from = total_abundance,       
    values_fill = 0                      
  )

abundance_dates_row_by_cluster$date <- as.Date(abundance_dates_row_by_cluster$date, "%Y.%m.%d")


# Import env data
env <- read.csv("env/environmental_filtered.csv")
colnames(env)[1] <- "date"
env$date <- as.Date(env$date)


# Filter dates to match abundance
env_added_dates <- bind_rows(env, anti_join(select(abundance_dates_row_by_cluster, date), env, by = "date"))
env_selected <- semi_join(env_added_dates, abundance_dates_row_by_cluster,  by = "date") %>% arrange (date)

# Interpolate missing dates
env_selected$T <- na.approx(env_selected$T)
env_selected$S <- na.approx(env_selected$S)
env_selected$O <- na.approx(env_selected$O)
env_selected$PH <- na.approx(env_selected$PH)
env_selected$NH4 <- na.approx(env_selected$NH4)
env_selected$NO3 <- na.approx(env_selected$NO3)
env_selected$NO2 <- na.approx(env_selected$NO2)
env_selected$CHLA <- na.approx(env_selected$CHLA)
env_selected$PO4 <- na.approx(env_selected$PO4)
env_selected$SIOH4<- na.approx(env_selected$SIOH4)
env_selected$POC <- na.approx(env_selected$POC)
env_selected$PON <- na.approx(env_selected$PON)
env_selected$minutes_of_daylight <- na.approx(env_selected$minutes_of_daylight)


# remove C:N ratio and date + Year
env_selected <- env_selected[,2:14]
abundance_dates_row_by_cluster <- abundance_dates_row_by_cluster[,2:17]

cor(abundance_dates_row_by_cluster[,2],env_selected, method = "spearman")

# Correlation matrix of abundance per clusters by env parameter
cor_df <- as.data.frame(sapply(abundance_dates_row_by_cluster, function(col) {
  cor(col, env_selected, method = "spearman")
}))
rownames(cor_df) <- colnames(env_selected)

# Only select ccm clusters
cor_df <- cor_df[,c(1,3:14)]
rownames(cor_df)[13] <- "PP"

cor_matrix <- as.matrix(cor_df)



# Draw plot
pdf(paste0(plot_path, "06_Environmental/","Heatmap_Env_Abundance.pdf"), width = 12, height = 9)
corrplot(cor_matrix, 
         method = "color", 
         addCoef.col = "black",
         col = rev(COL2('RdBu', 10)),
         tl.col = "black",
         tl.font = 2,
         tl.cex = 1.2,
         addgrid.col = "black",
         cl.lwd = 0.5)

dev.off()