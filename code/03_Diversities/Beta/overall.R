## Loading Libraries
library(dplyr)
library(tidyr)
library(vegan)    #Used for Bray-Curtis Distance
library(pheatmap) # Heatmaps
library(ggplot2)
library(ggrepel) # Labeling NMDS

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
abundance_louv_orig <- read.csv("generated/abundance_overview.csv", header=TRUE)
#Replace Environment_Condition with NA
abundance_louv_orig[abundance_louv_orig == "Environment_Condition"] <- NA
abundance_louv_orig[abundance_louv_orig$Phylum=="" | is.na(abundance_louv_orig$Phylum),"Phylum"] <- "NA"
abundance_louv_orig[is.na(abundance_louv_orig$LouvainLabelD),"LouvainLabelD"] <- "NA"

excluding_small_clusters <- abundance_louv_orig %>% filter(LouvainLabelD != "1" & LouvainLabelD != "14" & LouvainLabelD != "NA" )


#Calculate the Abundance of Phylums per Cluster
abundance_phyla_louv  <- abundance_louv_orig %>% group_by(LouvainLabelD,Phylum) %>%
                                              summarise(Phyla_Abundance = sum(Abundance_total), .groups ="drop")
abundance_phyla_louv_wide <- abundance_phyla_louv  %>% pivot_wider(names_from = Phylum,
                                                                  values_from = Phyla_Abundance,
                                                                  values_fill = 0) %>% 
                                                    arrange(LouvainLabelD) %>% select(-LouvainLabelD) %>% 
                                                    as.data.frame()


abundance_phyla_louv_nsc  <- excluding_small_clusters %>% group_by(LouvainLabelD,Phylum) %>%
                                                      summarise(Phyla_Abundance = sum(Abundance_total), .groups ="drop")
abundance_phyla_louv_nsc_wide <- abundance_phyla_louv_nsc  %>% pivot_wider(names_from = Phylum,
                                                                   values_from = Phyla_Abundance,
                                                                   values_fill = 0) %>% 
                                                            arrange(LouvainLabelD) %>% select(-LouvainLabelD) %>% 
                                                            as.data.frame()


rownames(abundance_phyla_louv_wide) <- c(as.character(0:15)) # Last cluster (15) = not cluster assigned
cluster_order <- c(as.character(0:14),"NA")
rownames(abundance_phyla_louv_nsc_wide) <- c(as.character(c(0,2:13)))
cluster_order_nsc <- c(as.character(c(0,2:13)))


# Calculate Bray-Curtis dissimilarity
bray_curtis_dist <- vegdist(abundance_phyla_louv_wide, method = "bray",na.rm = TRUE) %>% as.matrix()
bray_curtis_dist_nsc <- vegdist(abundance_phyla_louv_nsc_wide, method = "bray",na.rm = TRUE) %>% as.matrix()
# Remove lower triangle of matrix (duplicate values)
bray_lower <- bray_curtis_dist
bray_lower[lower.tri(bray_lower)] <- NA


# Average of no small cluster Bray-Curtis across all Clusters
bray_nsc_avg <- round(rowSums(bray_curtis_dist_nsc)/(dim(bray_curtis_dist_nsc)[1]-1),3)
bray_nsc_avg

### Generate Heatmaps
## Without small clusters 1 & 14 + NA
heatmap_bray_nsc <- pheatmap(bray_curtis_dist_nsc, 
                     cluster_rows = FALSE, cluster_cols = FALSE,  
                     labels_row = cluster_order_nsc, labels_col = cluster_order_nsc,
                     display_numbers = TRUE, number_color = "black",  
                     fontsize = 20, fontsize_number = 17, fontsize_row = 20, fontsize_col = 20, 
                     main = "Bray-Curtis Betadiversity",
                     xlab = "Cluster", ylab = "Cluster",
                     cellwidth = 45, cellheight = 45)

# with dendrogram
heatmap_bray_nsc_dendro <- pheatmap(bray_curtis_dist_nsc,
                             display_numbers = TRUE, number_color = "black",  
                             fontsize = 20, fontsize_number = 17, fontsize_row = 20, fontsize_col = 20,
                             main = "Bray-Curtis Betadiversity",
                             xlab = "Cluster", ylab = "Cluster",
                             cellwidth = 45, cellheight = 45)


# Save Heatmeaps
ggsave(filename="Overall_Bray_Curtis_NSC.pdf", plot=heatmap_bray_nsc, path=paste0(plot_path,"03_Diversities/Beta/"),width=10.5,height=10.5)
ggsave(filename="Overall_Bray_Curtis_NSC_dendro.pdf", plot=heatmap_bray_nsc_dendro, path=paste0(plot_path,"00_Appendix/03_Diversities/Beta/"),width=10.5,height=10.5)


## With every cluster
heatmap_bray <- pheatmap(bray_curtis_dist, 
                        cluster_rows = FALSE, cluster_cols = FALSE,  
                        labels_row = cluster_order, labels_col = cluster_order,
                        display_numbers = TRUE, number_color = "black",  
                        fontsize = 20, fontsize_number = 17, fontsize_row = 20, fontsize_col = 20,
                        main = "Bray-Curtis Betadiversity",
                        xlab = "Cluster", ylab = "Cluster",
                        cellwidth = 45, cellheight = 45)

# with dendrogram
heatmap_bray_dendro <- pheatmap(bray_curtis_dist,
                               display_numbers = TRUE, number_color = "black",  
                               fontsize = 20, fontsize_number = 17, fontsize_row = 20, fontsize_col = 20,
                               main = "Bray-Curtis Betadiversity",
                               xlab = "Cluster", ylab = "Cluster",
                               cellwidth = 45, cellheight = 45)


# Save Heatmeaps
ggsave(filename="Overall_Bray_Curtis.pdf", plot=heatmap_bray, path=paste0(plot_path,"00_Appendix/03_Diversities/Beta/"),width=10.5,height=10.5)
ggsave(filename="Overall_Bray_Curtis_dendro.pdf", plot=heatmap_bray_dendro, path=paste0(plot_path,"00_Appendix/03_Diversities/Beta/"),width=10.5,height=10.5)





## NMDS
# With all clusters
nmds <-  metaMDS(bray_curtis_dist, distance = "bray", k = 2, trymax = 100)
nmds_points <- as.data.frame(nmds$points)
nmds_points$Cluster <- factor(cluster_order, levels=c(c(0:14),"NA"))
# except small clusters & NA
nmds_nsc <- metaMDS(bray_curtis_dist_nsc, distance = "bray", k = 2, trymax = 100)
nmds_nsc_points <- as.data.frame(nmds_nsc$points)
nmds_nsc_points$Cluster <- factor(cluster_order_nsc, levels=c(c(0,2:13)))



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
