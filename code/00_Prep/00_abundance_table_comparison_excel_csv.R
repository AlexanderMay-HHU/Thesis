##Loading Libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyverse)

#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots")
setwd(paste0(project_folder,"/data"))



abundance_overall_orig <- read.csv("original/abundance.csv", header=TRUE)
abundance_overall_orig <- as.data.frame(t(abundance_overall_orig))
colnames(abundance_overall_orig) <- abundance_overall_orig[1,]
abundance_overall_orig <- abundance_overall_orig[2:dim(abundance_overall_orig)[1],]
abundance_overall_orig$OTU.number <- rownames(abundance_overall_orig)
rownames(abundance_overall_orig) <- NULL
abundance_overall_orig <- abundance_overall_orig[,c(dim(abundance_overall_orig)[2],1:(dim(abundance_overall_orig)[2]-1))]
for(i in 2:126){
  abundance_overall_orig[,i] <- as.numeric(abundance_overall_orig[,i])
}


test <- read.csv("original/overall.csv")
test <- test[,c("Nodes","Abundance4y","LouvainLabelD")]
all_NAs <- test[which(is.na(test$LouvainLabelD)),"Nodes"]

length(abundance_overall_orig)


abundance_values <- NULL
for (missing in all_NAs){
  #print(paste0("OTU:'",missing,"' - ",abundance_overall_orig[which(abundance_overall_orig$OTU.number == missing),"Abundance_total"]))
  abundance_values <- c(abundance_values,abundance_overall_orig[which(abundance_overall_orig$OTU.number == missing),"Abundance_total"])
}
missing_abundances <- as.data.frame(cbind(all_NAs, "Abundance4y" = abundance_values))
missing_abundances$Abundance4y <- as.numeric(missing_abundances$Abundance4y)

subset(test,test$Nodes == "Bac_Otu00079")
Bac_00079 <- subset(abundance_overall_orig, abundance_overall_orig$OTU.number == "Bac_Otu00079")

sum(missing_abundances$Abundance4y>0)


abundance_arc <- read_excel("Abundance_Taxa.xlsx", sheet = "Archaea")
abundance_bac <- read_excel("Abundance_Taxa.xlsx", sheet = "Bacteria")
abundance_euk <- read_excel("Abundance_Taxa.xlsx", sheet = "Eukaryotic phytoplankton")


# Refine OTU.number to match overall format of *KINGDOM*_OTU*NUMBER_OF_KINGDOM*
refine_OTU <- function(abundance_table){
  abundance_table$OTU.number <- paste(
    case_when(
      abundance_table$Kingdom == "Archaea" ~ "Arc",
      abundance_table$Kingdom == "Bacteria" ~ "Bac",
      abundance_table$Kingdom == "Eukaryota" ~ "Euk"
    ),
    abundance_table$OTU.number,
    sep = "_"
  )
  return(abundance_table)
}


abundance_arc <- refine_OTU(abundance_arc)
abundance_arc <- abundance_arc[,c(7:138)]
abundance_bac <- refine_OTU(abundance_bac)
abundance_bac <- abundance_bac[,c(8:153)]
abundance_euk <- refine_OTU(abundance_euk)
abundance_euk <- abundance_euk[,c(8:147)]



## Fix excels dates
# Function to fix date columns and names
fix_dates <- function(abundance_table) {
  # Identify date-like columns (all non-fixed columns)
  date_cols <- setdiff(names(abundance_table), "OTU.number")
  
  for (col in date_cols) {
    if (grepl("^\\d+$", col)) {
      # Convert column name to date
      excel_num <- as.numeric(col)
      date_name <- as.Date(excel_num, origin = "1899-12-30") # Windows Excel origin
      formatted_name <- format(date_name, "%Y-%m-%d")
      
      # Rename the column
      names(abundance_table)[names(abundance_table) == col] <- formatted_name
    } else {
      warning("Skipped column: ", col, " (not a numeric date)")
    }
  }
  return(abundance_table)
}

abundance_arc <- fix_dates(abundance_arc)
abundance_bac <- fix_dates(abundance_bac)
abundance_euk <- fix_dates(abundance_euk)


# Compare dates
original_dates <- as.Date(colnames(abundance_overall_orig)[2:dim(abundance_overall_orig)[2]])
arc_dates <- as.Date(colnames(abundance_arc)[2:dim(abundance_arc)[2]])
bac_dates <- as.Date(colnames(abundance_bac)[2:dim(abundance_bac)[2]])
euk_dates <- as.Date(colnames(abundance_euk)[2:dim(abundance_euk)[2]])


all_kingdom_dates <- as.Date(Reduce(
  f = intersect,
  x = list(arc_dates, bac_dates, euk_dates)
))


common_dates <- as.Date(intersect(all_kingdom_dates, original_dates))

all_kingdom_dates == original_dates

##
## Abundance data was only selected for dates where every kingdom was analyzed
##


# Get number of observations, total abundance and max month/year
abundance_total_and_max <- function(abundance_table){
  abundance_table$observations <- rowSums(abundance_table[,2:dim(abundance_table)[2]]>0, na.rm = T)
  
  
  max_abundance_date <- as.Date(colnames(abundance_table)[apply(abundance_table[,2:(dim(abundance_table)[2])-1], 1, which.max)])
  
  abundance_table$Abundance_total <- rowSums(abundance_table[sapply(abundance_table, is.numeric)], na.rm = TRUE)
  
  abundance_table$Max_month <- as.numeric(format(max_abundance_date,"%m"))
  abundance_table$Max_year <- as.numeric(format(max_abundance_date, "%Y"))
  return (abundance_table)
}



# Combine Kingdoms
combined_abundance_table <- list(abundance_arc, abundance_bac,abundance_euk) %>%  # Add all your dataframes to this list
  map(~ pivot_longer(.x, cols = -OTU.number, names_to = "date", values_to = "value")) %>%
  bind_rows() %>%
  pivot_wider(
    names_from = "date",
    values_from = "value",
    names_sort = TRUE  # Optional: sort columns chronologically
  )




# Compare the combined abundance table with the original one 
combined_abundance_table <- abundance_total_and_max(combined_abundance_table)
combined_abundance_table_limit <- combined_abundance_table[,c("OTU.number","observations","Abundance_total","Max_month","Max_year")]

abundance_overall_orig <- abundance_total_and_max(abundance_overall_orig)
abundance_overall_orig_limit <- abundance_overall_orig[,c("OTU.number","observations","Abundance_total","Max_month","Max_year")]