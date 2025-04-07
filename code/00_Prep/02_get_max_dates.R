## Loading Libraries
library(dplyr)
library(tidyr)


## Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots")
setwd(paste0(project_folder,"/data/"))


# Import Data
abundance_table <- read.csv("generated/abundance_samples_row_with_data_info.csv", header = T) 


# Max Year
unique(abundance_table$year)
yearly_abundance <- abundance_table %>% group_by(year) %>% summarise(across(where(is.numeric), sum)) %>% arrange(year)

yearly_reshaped <- yearly_abundance %>%
  pivot_longer(          
    cols = -year,        
    names_to = "ASV",
    values_to = "abundance"
  ) %>%
  pivot_wider(           
    names_from = year,
    values_from = abundance
  )

yearly_final <- yearly_reshaped %>%
  rowwise() %>%
  mutate(
    max_abundance_year = {
      years <- names(.)[-1]                
      values <- c_across(-ASV)
      years[which.max(values)]}) %>% ungroup()


# Max Month
unique(abundance_table$month)
monthly_abundance <- abundance_table %>% group_by(month) %>% summarise(across(where(is.numeric), sum)) %>% arrange(month)

monthly_reshaped <- monthly_abundance %>%
  pivot_longer(          
    cols = -month,        
    names_to = "ASV",
    values_to = "abundance"
  ) %>%
  pivot_wider(           
    names_from = month,
    values_from = abundance
  )

monthly_final <- monthly_reshaped %>%
  rowwise() %>%
  mutate(
    max_abundance_month = {
      month <- names(.)[-1]                
      values <- c_across(-ASV)
      month[which.max(values)]}) %>% ungroup()

# Max Season
unique(abundance_table$season)
seasonal_abundance <- abundance_table %>% group_by(season) %>% summarise(across(where(is.numeric), sum)) %>% arrange(season)

seasonal_reshaped <- seasonal_abundance %>%
  pivot_longer(          
    cols = -season,        
    names_to = "ASV",
    values_to = "abundance"
  ) %>%
  pivot_wider(           
    names_from = season,
    values_from = abundance
  )

seasonal_final <- seasonal_reshaped %>%
  rowwise() %>%
  mutate(
    max_abundance_season = {
      season <- names(.)[-1]                
      values <- c_across(-ASV)
      season[which.max(values)]}) %>% ungroup()



# Make complete (year, month, season) max_abundance dataframe
abundance_max <- yearly_final %>% select(ASV,max_abundance_year) %>%
  left_join(
    monthly_final %>% select(ASV, max_abundance_month),
    by = c("ASV" = "ASV")) %>%
  left_join(
    seasonal_final %>% select(ASV, max_abundance_season),
    by = c("ASV" = "ASV"))



# Save data
write.csv(abundance_max,"generated/abundance_maxima.csv",row.names = F)