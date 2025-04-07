## Loading Libraries
library(readr)  # for importing env data
library(zoo)    # for approximation of missing env data
library(dplyr)
library(tidyr)
library(vegan)    #Used for Bray-Curtis Distance & NMDS
library(ggplot2)
library(ggrepel)

#Set Working Directory
project_folder <- dirname(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/"))



heatmap_palette <- c("#3300AA","#FFFFBF","#AA0033")

color_clusters <- c("#00ffff","#191970","#c71585","#2f4f4f",
                    "#ff0000","#0000ff","#bdb76b","#006400",
                    "#ffa500","#1e90ff","#ffb6c1","#00fa9a",
                    "#ffff00","#631919","#ff00ff","#00ff00")





## Get Data
# Abundance
abundance <- read.csv("generated/abundance_asv_row_with_louvain_observations.csv", header=TRUE)
#abundance$date <- as.Date(abundance$date)

abundance_dates <- as.Date(substring(colnames(abundance[2:126]),2,11), format = "%Y.%m.%d")

long_abundance <- abundance %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Date",
    values_to = "Abundance"
  ) %>%
  mutate(Date = as.Date(substring(Date,2,11), format = "%Y.%m.%d"))

abundance_by_cluster <- long_abundance %>%
  group_by(LouvainLabelD, Date) %>%
  summarise(TotalAbundance = sum(Abundance, na.rm = TRUE)) %>%
  ungroup() %>% pivot_wider(names_from = LouvainLabelD, values_from = TotalAbundance)



# Environmental
env <- read_delim("env/Bio_Geochemical_Compounds.csv", 
                             delim = ";", escape_double = FALSE, comment = "//", 
                             trim_ws = TRUE, skip = 2,na = "999999")
env$DATE <- as.Date(env$DATE, format = "%m/%d/%Y")
# Filter to year timeframe of 2007-2015 and only contain surface data & select only rows with data needed
env <- with(env, env[(DATE >= as.Date('2007-01-01') & DATE <= as.Date('2015-12-31') & PROF_TEXT == "S"), ])
env <- env %>% select(DATE,T,S,O,PH,NH4,NO3,NO2,CHLA,PO4,SIOH4,COP,NOP)


# Get dates that abundance data was available for but not in environmental
missing_dates <- data.frame(DATE = as.Date(setdiff(abundance_dates,env$DATE)))
# and add those dates to be approximated later
env_complete <- dplyr::bind_rows(env, missing_dates) %>% dplyr::arrange(DATE)

# fill missing values by interpolation
env_complete$T <- na.approx(env_complete$T)
env_complete$S <- na.approx(env_complete$S)
env_complete$O <- na.approx(env_complete$O)
env_complete$PH <- na.approx(env_complete$PH)
env_complete$NH4 <- na.approx(env_complete$NH4)
env_complete$NO3 <- na.approx(env_complete$NO3)
env_complete$NO2 <- na.approx(env_complete$NO2)
env_complete$CHLA <- na.approx(env_complete$CHLA)
env_complete$PO4 <- na.approx(env_complete$PO4)
env_complete$SIOH4<- na.approx(env_complete$SIOH4)
env_complete$COP <- na.approx(env_complete$COP)
env_complete$NOP <- na.approx(env_complete$NOP)


env_matching <- env_complete[env_complete$DATE %in% abundance_dates, ]


## NMDS
# With all clusters
nmds <-  metaMDS(abundance_by_cluster[2:17], distance = "bray", k = 2, trymax = 100)
#nmds_points <- as.data.frame(nmds$points)
#nmds_points$Cluster <- factor(c(0:14,"NA"), levels=c(c(0:14),"NA"))
# except small clusters & NA
nmds_nsc <- metaMDS(abundance_by_cluster[,c(2,4:15)], distance = "bray", k = 2, trymax = 100)
#nmds_nsc_points <- as.data.frame(nmds_nsc$points)
#nmds_nsc_points$Cluster <- factor(c(0,2:13), levels=c(c(0,2:13)))


# Fit environmental variables to NMDS
env_fit <- envfit(nmds, env_matching, permutations = 999)
env_fit_nsc <- envfit(nmds_nsc, env_matching, permutations = 999)


plot(nmds_nsc, type = "n")
points(nmds_nsc, col = "gray")

plot(env_fit, p.max = 0.05, col = "red")  # Vectors for continuous variables
plot(env_fit_nsc, p.max = 0.05, col = "red")  # Vectors for continuous variables
#ordisurf(nmds, env_matching$T, add = TRUE)

ordihull(nmds_nsc, groups = LouvainLabelD, col = "blue")


## Generate NMDS plots
# With all clusters
gg_nmds_plot <- ggplot(nmds_points, aes(x = MDS1, y = MDS2,color = Cluster)) +
                      # Point data for clusters
                      geom_point(size = 8, shape = 20,
                                 fill = color_clusters) +  
                      # Labels for clusters
                      geom_text_repel(aes(label = Cluster), color = "black", size = 7, fontface = "bold") +
                      # Appling color scheme to Legend
                      scale_color_manual(values = color_clusters) + 
                      # Adding Plot, axis and Legend titles
                      labs(title = "NMDS Plot - Bray-Curtis Betadiversity",
                           subtitle = paste0("Stress: ", round(nmds$stress, 5)),
                           x = "NMDS Dimension 1",
                           y = "NMDS Dimension 2",
                           color = "Cluster") +
                      # Set Plot Theme
                      theme_minimal(base_size = 18) +  
                      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),  
                            plot.subtitle = element_text(hjust = 0.5, size = 18, face = "italic"),
                            axis.line = element_line(size = 0.5, color = "gray"),  
                            axis.ticks = element_line(color = "gray"),
                            axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
                            axis.text.y = element_text(size = 16),  
                            axis.title.x = element_text(size = 18, face = "bold"),  
                            axis.title.y = element_text(size = 18, face = "bold"),
                            legend.position = "right",  
                            legend.text = element_text(size = 16),  
                            legend.title = element_text(size = 18, face = "bold"))


# except small clusters
gg_nmds_nsc_plot <- ggplot(nmds_nsc_points, aes(x = MDS1, y = MDS2,color = Cluster)) +
                          # Point data for clusters
                          geom_point(size = 8, shape = 20,
                                     fill = color_clusters[c(1,3:14)]) +  #Need to specify colors because not all are used
                          # Labels for clusters
                          geom_text_repel(aes(label = Cluster), color = "black", size = 7, fontface = "bold") +
                          # Apply color scheme to Legend
                          scale_color_manual(values = color_clusters[c(1,3:14)]) +  #Need to specify colors because not all are used
                          # Adding Plot, axis and Legend titles
                          labs(title = "NMDS Plot - Bray-Curtis Betadiversity",
                                subtitle = paste0("Stress: ", round(nmds_nsc$stress, 3)),
                                x = "NMDS Dimension 1",
                                y = "NMDS Dimension 2",
                                color = "Cluster") +
                          # Set Plot Theme
                          theme_minimal(base_size = 18) +  
                          theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),  
                                plot.subtitle = element_text(hjust = 0.5, size = 18, face = "italic"),
                                axis.line = element_line(size = 0.5, color = "gray"),  
                                axis.ticks = element_line(color = "gray"),
                                axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),  
                                axis.text.y = element_text(size = 16),  
                                axis.title.x = element_text(size = 18, face = "bold"),  
                                axis.title.y = element_text(size = 18, face = "bold"),
                                legend.position = "right",  
                                legend.text = element_text(size = 16),  
                                legend.title = element_text(size = 18, face = "bold"))


# Show plots
gg_nmds_plot
gg_nmds_nsc_plot


# Save NMDS Plots
ggsave(filename="NMDS_all.pdf", plot=gg_nmds_plot, path=paste0(plot_path,"00_Appendix/03_Diversities/Beta/"),width=12,height=7.5)
ggsave(filename="NMDS_nsc.pdf", plot=gg_nmds_nsc_plot, path=paste0(plot_path,"03_Diversities/Beta/"),width=12,height=7.5)
