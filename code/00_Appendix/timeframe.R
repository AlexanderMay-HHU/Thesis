library(dplyr)
library(readxl)


abundance_euk <- read_excel("R:/Studium/Bachelor/Thesis/data/Abundance_Taxa.xlsx", 
                            sheet = "Eukaryotic phytoplankton")

abundance_arc <- read_excel("R:/Studium/Bachelor/Thesis/data/Abundance_Taxa.xlsx", 
                            sheet = "Archaea")

abundance_bac <- read_excel("R:/Studium/Bachelor/Thesis/data/Abundance_Taxa.xlsx", 
                            sheet = "Bacteria")


#Get Dates from Datasets
dates_euk <- colnames(abundance_euk[9:147])
dates_arc <- colnames(abundance_arc[9:138])
dates_bac <- colnames(abundance_bac[9:153])

dates_euk <- as.Date(as.integer(dates_euk), origin="1899-12-30")
dates_arc <- as.Date(as.integer(dates_arc), origin="1899-12-30")
dates_bac <- as.Date(as.integer(dates_bac), origin="1899-12-30")


#Get all Dates and only unique ones
all_dates <- sort(unique(c(dates_euk,dates_arc,dates_bac)))


#Get Beginning & End of Timeseries
min(all_dates)
max(all_dates)

#Save Dates
write.csv(all_dates, file="R:/Studium/Bachelor/Thesis/data/env/date_list.csv")