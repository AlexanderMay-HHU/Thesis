##Loading Libraries
library(dplyr)


#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data")



edges <- read.csv("ferret_tables_Pruned_CCMN.csv_1 default edge.csv", header=TRUE)


# declutter name of edges into from and to ASVs
edges <- edges %>% mutate(from_ASV = substring(name,1,12), to_ASV = substring(name,31,42))


#Split edges into different Interaction "types"
edges <- edges %>% mutate(Interaction_Type =ifelse(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "BacBac","BacBac",
                                            ifelse(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "BacEuk","BacEuk",
                                            ifelse(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "BacArc","BacEuk",
                                            ifelse(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "EukBac","EukBac",
                                            ifelse(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "ArcBac","EukBac",
                                            ifelse(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "EukEuk","EukEuk",
                                            ifelse(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "EukArc","EukEuk",
                                            ifelse(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "ArcArc","EukEuk",
                                            ifelse(paste0(substring(from_ASV,1,3),substring(to_ASV,1,3)) == "ArcEuk","EukEuk","_ERROR_"))))))))))


edges_short_info <- edges %>% select(name,from_ASV,to_ASV,Interaction_Type)


# export edges short info
write.csv(edges_short_info, "generated/05_Network_Additions/Edges_Short_Info.csv")