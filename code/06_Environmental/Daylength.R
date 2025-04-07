##Loading Libraries
library(readxl)
library(tidyverse)

#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/env/"))


# Replace "path/to/your/file.xlsx" with your actual file path
excel_file <- paste0(project_folder,"/data/env/Daylength.xlsx")

# Read all sheets and combine into one dataframe
daylength <- excel_sheets(excel_file) %>%
  set_names() %>%
  map_dfr(~{
    # Read each sheet with appropriate column types
    read_excel(
      excel_file,
      sheet = .x,
      col_names = TRUE,
      col_types = c("numeric", rep("text", 12))  # Day is numeric, months as text
    ) %>%
      # Reshape data from wide to long format
      pivot_longer(
        cols = -Day,
        names_to = "Month",
        values_to = "Time"
      ) %>%
      # Convert Month abbreviation to number (Jan=1, Feb=2, etc.)
      mutate(
        Year = as.integer(.x),  # Extract year from sheet name
        Month = match(substring(Month,1,3), month.abb),  # Convert "Jan" to 1, etc.
        Day = as.integer(Day)
      ) %>%
      # Remove rows where Time is NA (invalid days for a month)
      filter(!is.na(Time)) %>%
      # Reorder columns
      select(Year, Month, Day, Time)
  }, .id = NULL) %>%
  # Conversion from Excels HH:MM format to Minutes
  mutate(minutes_of_daylight = as.integer(as.double(Time)*24*60),
         Date = as.Date(paste(Year,Month,Day,sep="-"))) 


# Filter daylength dataframe 
daylength_short <- daylength[,c("Date","minutes_of_daylight")]


# Save to project folder
write.csv(daylength_short,"daylength_minutes.csv",row.names = F)




gg_daylight <- ggplot(daylength_short, aes(x = Date, y = minutes_of_daylight)) +
  geom_line(size = 1.1, color = "orange3", alpha = 0.7) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Format x-axis as years
  #geom_smooth(method = "loess", span = 0.1,color = "gray50", size = 1.2, se = FALSE, alpha = 0.2) +
  theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, color = "black", size = 18),  
    axis.text.y = element_text(color = "black", size = 20),  
    axis.title.x = element_text(size = 22, face = "bold", color = "black"),
    axis.title.y = element_text(size = 22, face = "bold", color = "black"),  
    panel.grid.major = element_line(color = "gray80", size = 0.5, linetype = "solid"), 
    panel.grid.minor = element_blank(), 
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5, color = "black"),
    legend.position = "none",
    strip.text = element_text(size = 20, color = "black", face = "bold")  
  ) +
  labs(
    title = "Daylight across Years",
    y = "Minutes of Daylight",
    x = "Year"
  )

# Show it
gg_daylight


ggsave(filename="Daylight.pdf", plot=gg_daylight, path=paste0(plot_path, "06_Environmental/Data/"),width=8,height=5)



