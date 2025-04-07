## Loading Libraries
library(dplyr)


## Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots")
setwd(paste0(project_folder,"/data/"))


# Import Data
abundance_samples_rows <- read.csv("original/abundance.csv", header = T) 

## Fix dataframe that contains ASVs as columns
# Fix date column type
colnames(abundance_samples_rows)[1] <- "date"
abundance_samples_rows$date <- as.Date(abundance_samples_rows$date, format="%Y-%m-%d")

# Add Month/Year/Season columns
abundance_samples_rows$month <- as.character(format(abundance_samples_rows$date, "%m"))
abundance_samples_rows$year <- as.character(format(abundance_samples_rows$date, "%Y"))
abundance_samples_rows$season <- case_when(abundance_samples_rows$month == "01" ~ "Winter",
                                    abundance_samples_rows$month == "02" ~ "Winter",
                                    abundance_samples_rows$month == "03" ~ "Spring",
                                    abundance_samples_rows$month == "04" ~ "Spring",
                                    abundance_samples_rows$month == "05" ~ "Spring",
                                    abundance_samples_rows$month == "06" ~ "Summer",
                                    abundance_samples_rows$month == "07" ~ "Summer",
                                    abundance_samples_rows$month == "08" ~ "Summer",
                                    abundance_samples_rows$month == "09" ~ "Autumn",
                                    abundance_samples_rows$month == "10" ~ "Autumn",
                                    abundance_samples_rows$month == "11" ~ "Autumn",
                                    abundance_samples_rows$month == "12" ~ "Winter",
                                    TRUE ~ NA)


# Samples per Year + Difference in days between samples
abundance_samples_rows$days_diff <- c()
samples_per_year <- abundance_samples_rows %>% select(date,year) %>% group_by(year) %>%
                            summarise(samples_per_year = n(), .groups = "drop")


# 2007,2008, 2010 have less than 12 samples per year
# 2013/14 (17), 2009 (18), 2011 (19), 2012 (22) samples were taken per year

sample_day_difference <- abundance_samples_rows  %>%
                            mutate(days_to_next_sample = c(as.numeric(diff(date)),NA))  %>%
                            select(date,days_to_next_sample) %>%
                            mutate(Year = format(date, "%Y")) %>%
                            group_by(Year) %>%
                            mutate(avg_day_diff_per_year = round(sum(days_to_next_sample)/n(),0))

summary(sample_day_difference$days_to_next_sample, na.rm = T)

# Only 2012 (17), 2011 (19), 2009 (20), 2013/2014 (21) remained at or below an average difference of 21 days between samples
# 2007 (44), 2008 (38) and 2010 (33) had much more time between samples


ggplot(sample_day_difference, aes(x = as.factor(Year), y = days_to_next_sample)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +  # Boxplot styling
  labs(
    title = "Distribution of Days Between Samples by Year",
    x = "Year",
    y = "Days to Next Sample"
  ) +
  theme_minimal() 




# Save data
write.csv(abundance_samples_rows,"generated/abundance_samples_row_with_data_info.csv",row.names = F)