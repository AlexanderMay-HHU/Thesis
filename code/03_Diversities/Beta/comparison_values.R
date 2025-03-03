## Loading Libraries
library(dplyr)
library(vegan)    #Used for Bray-Curtis Distance
library(ggplot2)


#Set Working Directory
project_folder <- dirname(dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path))))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data"))


colorblind_gradient_palette <- c("#000000","#df536b","#61d04f","#2297e6",
                                 "#9928e5","#ee9ced","#e69f00","#8ee6ff",
                                 "#009e73","#f0e442","#0072b2","#d55e00",
                                 "#999999")



## Get Data
nodes <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default node.csv", header=TRUE)
#Replace Environment_Condition with NA
nodes[nodes == "Environment_Condition"] <- NA

nodes_archaea <- subset(nodes,nodes$Kingdom == "Archaea")
nodes_bacteria <- subset(nodes,nodes$Kingdom == "Bacteria")
nodes_eukaryotes <- subset(nodes,nodes$Kingdom == "Eukaryota")



## Make Diversity dataframe
# Archaea
arc_abundance_of_phylum_in_cluster <- data.frame(matrix(ncol=length(unique(nodes_archaea$Phylum)),nrow=length(unique(nodes_archaea$LouvainLabelD))))
colnames(arc_abundance_of_phylum_in_cluster) <- unique(nodes_archaea$Phylum)
rownames(arc_abundance_of_phylum_in_cluster) <- sort(unique(nodes_archaea$LouvainLabelD))
# Bacteria
bac_abundance_of_phylum_in_cluster <- data.frame(matrix(ncol=length(unique(nodes_bacteria$Phylum)),nrow=length(unique(nodes_bacteria$LouvainLabelD))))
colnames(bac_abundance_of_phylum_in_cluster) <- unique(nodes_bacteria$Phylum)
rownames(bac_abundance_of_phylum_in_cluster) <- sort(unique(nodes_bacteria$LouvainLabelD))
# Eukaryotes
euk_abundance_of_phylum_in_cluster <- data.frame(matrix(ncol=length(unique(nodes_eukaryotes$Phylum)),nrow=length(unique(nodes_eukaryotes$LouvainLabelD))))
colnames(euk_abundance_of_phylum_in_cluster) <- unique(nodes_eukaryotes$Phylum)
rownames(euk_abundance_of_phylum_in_cluster) <- sort(unique(nodes_eukaryotes$LouvainLabelD))
# Overall
tot_abundance_of_phylum_in_cluster <- data.frame(matrix(ncol=length(unique(nodes$Phylum)),nrow=length(unique(nodes$LouvainLabelD))))
colnames(tot_abundance_of_phylum_in_cluster) <- unique(nodes$Phylum)
rownames(tot_abundance_of_phylum_in_cluster) <- sort(unique(nodes$LouvainLabelD))



## Calculate the Abundance of Phylums per Cluster
# Archaea
for (cluster in sort(unique(nodes_archaea$LouvainLabelD))){
  for (phylum in unique(nodes_archaea$Phylum)){
    #Abundance of Phylum (skip NA phylum)
    if(!is.na(phylum)){
      arc_abundance_of_phylum_in_cluster[ifelse(cluster==0,1,cluster),phylum] <- sum(nodes_archaea[nodes_archaea$LouvainLabelD == cluster &
                                                                            nodes_archaea$Phylum == phylum,1],na.rm = TRUE)
      cat("Done with calculating the sum of Phylum (", phylum,") from Cluster", cluster, "\n",sep="")
    }else{
      next
    }
  }
}
# Bacteria
for (cluster in sort(unique(nodes_bacteria$LouvainLabelD))){
  for (phylum in unique(nodes_bacteria$Phylum)){
    #Abundance of Phylum (skip NA phylum)
    if(!is.na(phylum)){
      bac_abundance_of_phylum_in_cluster[ifelse(cluster==0,1,cluster),phylum] <- sum(nodes_bacteria[nodes_bacteria$LouvainLabelD == cluster &
                                                                                                      nodes_bacteria$Phylum == phylum,1],na.rm = TRUE)
      cat("Done with calculating the sum of Phylum (", phylum,") from Cluster", cluster, "\n",sep="")
    }else{
      next
    }
  }
}
# Eukaryotes
for (cluster in sort(unique(nodes_eukaryotes$LouvainLabelD))){
  for (phylum in unique(nodes_eukaryotes$Phylum)){
    #Abundance of Phylum (skip NA phylum)
    if(!is.na(phylum)){
      euk_abundance_of_phylum_in_cluster[ifelse(cluster==0,1,cluster),phylum] <- sum(nodes_eukaryotes[nodes_eukaryotes$LouvainLabelD == cluster &
                                                                                                        nodes_eukaryotes$Phylum == phylum,1],na.rm = TRUE)
      cat("Done with calculating the sum of Phylum (", phylum,") from Cluster", cluster, "\n",sep="")
    }else{
      next
    }
  }
}
# Overall
for (cluster in sort(unique(nodes$LouvainLabelD))){
  for (phylum in unique(nodes$Phylum)){
    #Abundance of Phylum (skip NA phylum)
    if(!is.na(phylum)){
      tot_abundance_of_phylum_in_cluster[ifelse(cluster==0,1,cluster),phylum] <- sum(nodes[nodes$LouvainLabelD == cluster &
                                                                                             nodes$Phylum == phylum,1],na.rm = TRUE)
      cat("Done with calculating the sum of Phylum (", phylum,") from Cluster", cluster, "\n",sep="")
    }else{
      next
    }
  }
}



# Calculate Bray-Curtis dissimilarities
arc_bray_curtis_dist <- vegdist(arc_abundance_of_phylum_in_cluster, method = "bray",na.rm = TRUE) %>% as.matrix() %>% as.table() %>% as.data.frame()
colnames(arc_bray_curtis_dist) <- c("From_Clu","To_Clu","Freq")
bac_bray_curtis_dist <- vegdist(bac_abundance_of_phylum_in_cluster, method = "bray",na.rm = TRUE) %>% as.matrix() %>% as.table() %>% as.data.frame()
colnames(bac_bray_curtis_dist) <- c("From_Clu","To_Clu","Freq")
euk_bray_curtis_dist <- vegdist(euk_abundance_of_phylum_in_cluster, method = "bray",na.rm = TRUE) %>% as.matrix() %>% as.table() %>% as.data.frame()
colnames(euk_bray_curtis_dist) <- c("From_Clu","To_Clu","Freq")
tot_bray_curtis_dist <- vegdist(tot_abundance_of_phylum_in_cluster, method = "bray",na.rm = TRUE) %>% as.matrix() %>% as.table() %>% as.data.frame()
colnames(tot_bray_curtis_dist) <- c("From_Clu","To_Clu","Freq")



# Comparison of bray curtis dissimilarities
bray_Ø_arc <- arc_bray_curtis_dist %>% group_by(From_Clu) %>% summarise("Div_Arc" = round(sum(Freq)/12,4))
bray_Ø_bac <- bac_bray_curtis_dist %>% group_by(From_Clu) %>% summarise("Div_Bac" = round(sum(Freq)/12,4))
bray_Ø_euk <- euk_bray_curtis_dist %>% group_by(From_Clu) %>% summarise("Div_Euk" = round(sum(Freq)/12,4))
bray_Ø_tot <- tot_bray_curtis_dist %>% group_by(From_Clu) %>% summarise("Div_tot" = round(sum(Freq)/12,4))

bray_Ø <- bray_Ø_arc
bray_Ø <- bray_Ø %>% left_join(bray_Ø_bac, by = "From_Clu")
bray_Ø <- bray_Ø %>% left_join(bray_Ø_euk, by = "From_Clu")
bray_Ø
summary(bray_Ø$Div_Arc)
summary(bray_Ø$Div_Bac)
summary(bray_Ø$Div_Euk)


bray_Ø_plottable <- as.data.frame(matrix(nrow=39,ncol=0))
bray_Ø_plottable$Cluster <- rep(as.factor(c(0,2:13)),3)
bray_Ø_plottable$Kingdom <- c(rep("Archaea",13),rep("Bacteria",13),rep("Eukaryota",13))
bray_Ø_plottable$Bray_Curtis <- c(bray_Ø_arc$Div_Arc,bray_Ø_bac$Div_Bac,bray_Ø_euk$Div_Euk)


#summary(euk_bray_curtis_dist[euk_bray_curtis_dist$From_Clu == 9 & !(euk_bray_curtis_dist$To_Clu == 9),3])


## PLOTS
# Ø Bray-Curtis Beta Dissimilarity of Archaea per Cluster
gg_bray_arc_per_cluster <- ggplot(bray_Ø_arc, aes(x=From_Clu,
                                                   y=Div_Arc,
                                                   label=round(Div_Arc,3))) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Ø Bray-Curtis Beta Dissimilarity")+
  theme(legend.position="none")+
  scale_x_discrete(breaks = c(0,2:13))

#Show it
gg_bray_arc_per_cluster+
  labs(title="Archaea")+
  geom_text(vjust=-0.5,size=5)

#Save to plot_path
ggsave(filename="Comparison.png", plot=gg_bray_arc_per_cluster+
         labs(title="Archaea")+
         geom_text(vjust=-0.5,size=5),
       path=paste0(plot_path,"03_Diversities/Beta/Prokaryotes/Archaea"))



# Ø Bray-Curtis Beta Dissimilarity of Bacteria per Cluster
gg_bray_bac_per_cluster <- ggplot(bray_Ø_bac, aes(x=From_Clu,
                                                  y=Div_Bac,
                                                  label=round(Div_Bac,3))) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Ø Bray-Curtis Beta Dissimilarity")+
  theme(legend.position="none")+
  scale_x_discrete(breaks = c(0,2:13))

#Show it
gg_bray_bac_per_cluster+
  labs(title="Bacteria")+
  geom_text(vjust=-0.5,size=5)

#Save to plot_path
ggsave(filename="Comparison.png", plot=gg_bray_bac_per_cluster+
         labs(title="Bacteria")+
         geom_text(vjust=-0.5,size=5),
       path=paste0(plot_path,"03_Diversities/Beta/Prokaryotes/Bacteria"))



# Ø Bray-Curtis Beta Dissimilarity of Eukaryota per Cluster
gg_bray_euk_per_cluster <- ggplot(bray_Ø_euk, aes(x=From_Clu,
                                                  y=Div_Euk,
                                                  label=round(Div_Euk,3))) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Ø Bray-Curtis Beta Dissimilarity")+
  theme(legend.position="none")+
  scale_x_discrete(breaks = c(0,2:13))

#Show it
gg_bray_euk_per_cluster+
  labs(title="Eukaryota")+
  geom_text(vjust=-0.5,size=5)

#Save to plot_path
ggsave(filename="Comparison.png", plot=gg_bray_euk_per_cluster+
         labs(title="Eukaryota")+
         geom_text(vjust=-0.5,size=5),
       path=paste0(plot_path,"03_Diversities/Beta/Eukaryotes"))



# Ø Bray-Curtis Beta Dissimilarity Overall per Cluster
gg_bray_tot_per_cluster <- ggplot(bray_Ø_tot, aes(x=From_Clu,
                                                  y=Div_tot,
                                                  label=round(Div_tot,3))) + 
  geom_bar(stat="identity", fill=colorblind_gradient_palette)+
  labs(x="Louvain Cluster",y="Ø Bray-Curtis Beta Dissimilarity")+
  theme(legend.position="none")+
  scale_x_discrete(breaks = c(0,2:13))

#Show it
gg_bray_tot_per_cluster+
  labs(title="Cluster Comparison Overall")+
  geom_text(vjust=-0.5,size=5)

#Save to plot_path
ggsave(filename="Comparison_Cluster_Overall.png", plot=gg_bray_euk_per_cluster+
         labs(title="Cluster Comparison Overall")+
         geom_text(vjust=-0.5,size=5),
       path=paste0(plot_path,"00_Appendix/03_Diversities/Beta/"))



# Ø Bray-Curtis Beta Dissimilarity all kingdoms per Cluster
gg_bray_per_cluster <- ggplot(bray_Ø_plottable, aes(x=Cluster,
                                                    y=Bray_Curtis,
                                                    fill=Kingdom,
                                                    label=round(Bray_Curtis,2))) + 
  geom_bar(position="dodge", stat="identity")+
  labs(x="Louvain Cluster",y="Ø Bray-Curtis Beta Dissimilarity")+
  theme(legend.key.size = unit(0.65, units = "cm"))+
  scale_x_discrete(breaks = c(0,2:13))+
  scale_fill_discrete(type=c("#F8766D","#00BA38","#619CFF"))+
  geom_vline(xintercept = seq(1.5, 12.5, by = 1))

#Show it
gg_bray_per_cluster+
  labs(title="Comparison of Clusters by Kingdom")+
  geom_text(inherit.aes = T,size = 2.5,position = position_dodge(0.9),vjust=-0.5)

#Save to plot_path
ggsave(filename="Comparison_Cluster_Kingdom.png", plot=gg_bray_per_cluster+
         labs(title="Comparison of Clusters by Kingdom")+
         geom_text(inherit.aes = T,size = 2.5,position = position_dodge(0.9),vjust=-0.5),
       path=paste0(plot_path,"00_Appendix/03_Diversities/Beta"))





# Ø Bray-Curtis Beta Dissimilarity by kingdoms
gg_bray_violin <- ggplot(bray_Ø_plottable, aes(x=Kingdom,
                                                y=Bray_Curtis,
                                                fill=Kingdom)) + 
  geom_violin()+
  geom_boxplot(width=0.1, color="grey", alpha=0.2)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=2, color="grey", fill="grey") +
  labs(x="Kingdom",y= "Bray-Curtis Beta Dissimilarity")+
  theme(legend.key.size = unit(0.65, units = "cm"),legend.position="none")+
  scale_fill_discrete(type=c("#F8766D","#00BA38","#619CFF"))

#Show it
gg_bray_violin+
  labs(title="Bray Curtis Beta-Dissimilarity by Kingdom across Clusters")

#Save to plot_path
ggsave(filename="Kingdom_Comparison.png", plot=gg_bray_violin+
         labs(title="Bray Curtis Beta-Dissimilarity by Kingdom across Clusters"),
       path=paste0(plot_path,"00_Appendix/03_Diversities/Beta"))

