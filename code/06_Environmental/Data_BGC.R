##Loading Libraries
library(readr)
library(dplyr)
library(zoo)
library(ggplot2)
library(patchwork)

#### IMPORT DATA
#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
plot_path <- paste0(project_folder,"/generated_plots/")
setwd(paste0(project_folder,"/data/env"))


## q[Var] = Quality flag from SOMLIT Protocol
## Quality Flags of data taken from:
## https://doi.org/10.3389/fmars.2023.1135446 
## 0 = Below Detection (The value represented is the detection limit one)
## 1 = Sampled but not measured (Sample collected but measure not made because the sample was lost (e.g. broken vial, bad preservation, contamination,...). Missing value is represented by 999999 -> NA)
## 2 = Good Value, no replicate (Expert review. Measure made under optimal sampling, ambient, and analytical conditions. The value is in the range of those expected.)
## 3 = Doubtful value (Expert review (e.g. excessive or unexpected value, analytical problem, below quality requirements...). Value is reported but the end-user is advised to not use the value)
## 4 = Bad value (Expert review (e.g. excessive or unexpected value, analytical problem, below quality requirements...). Value is reported but the end-user is advised to not use the value)
## 5 = Measure made but value still not reported (Value is reported with delay. Missing value is represented by 999999 -> NA)
## 6 = Good value, mean of several replicates ( Mean of replicate measurements of the same sample. Measure made under optimal ambient and analytical conditions.)
## 7 = Good value but acquired with departure from the SOMLIT protocol (Sample collected but measure made with departure from the SOMLIT protocol. Despite the value is considered as good it is recommended to the end-user to contact the scientific manager of the sampling station for more information)
## 8 = No quality control (Value is given but the quality control flag is not available yet) 
## 9 = No Sampling (Sample not collected and/or measure impossible (e.g. bad weather at sea). Missing value is represented by 999999 -> NA)


### Bio-Geochemical Compounds
bgc <- read_delim("Bio_Geochemical_Compounds.csv", 
                                        delim = ";", escape_double = FALSE, comment = "//", 
                                        trim_ws = TRUE, skip = 2,na = "999999")
daylight <- read.csv("daylength_minutes.csv")
daylight$Date <- as.Date(daylight$Date, format = "%Y-%m-%d")

# Only select Date and Data Columns of surface
bgc_s <- bgc[bgc$PROF_TEXT == "S",] %>% select(DATE,T, S, O, PH, CHLA, NH4, NO3, NO2, PO4, SIOH4, COP, NOP)



# Rename "un-capslock" column variable name for Date and use english var names
names(bgc_s)[names(bgc_s) == "DATE"] <- "Date"
names(bgc_s)[names(bgc_s) == "COP"] <- "POC"  #particulate organic carbon
names(bgc_s)[names(bgc_s) == "NOP"] <- "PON"  #particulate organic nitrogen
# Fix date column and 
bgc_s$Date <- as.Date(bgc_s$Date, format = "%m/%d/%Y")


#### Dataset selection
### Only use specific portions of dataset (Timeframe)
start_date <- as.Date('2007-01-01')
end_date <- as.Date('2015-12-31')
bgc_filtered <- with(bgc_s, bgc_s[(Date >= start_date & Date <= end_date), ])
# Add daylight to env_data
bgc_filtered <- bgc_filtered %>% 
  inner_join(daylight %>% select(Date, minutes_of_daylight), by = "Date")
# Add C:P ratio
bgc_filtered$C_P_Ratio <- bgc_filtered$POC/bgc_filtered$PON

# Count missing values per parameter
colSums(is.na(bgc_filtered[,2:15]))


# Fill missing values by linear interpolation
bgc_filtered$T <- na.approx(bgc_filtered$T)
bgc_filtered$S <- na.approx(bgc_filtered$S)
bgc_filtered$O <- na.approx(bgc_filtered$O)
bgc_filtered$PH <- na.approx(bgc_filtered$PH)
bgc_filtered$NH4 <- na.approx(bgc_filtered$NH4)
bgc_filtered$NO3 <- na.approx(bgc_filtered$NO3)
bgc_filtered$NO2 <- na.approx(bgc_filtered$NO2)
bgc_filtered$CHLA <- na.approx(bgc_filtered$CHLA)
bgc_filtered$PO4 <- na.approx(bgc_filtered$PO4)
bgc_filtered$SIOH4<- na.approx(bgc_filtered$SIOH4)
bgc_filtered$POC <- na.approx(bgc_filtered$POC)
bgc_filtered$PON <- na.approx(bgc_filtered$PON)

# Save data
write.csv(bgc_filtered,"environmental_filtered.csv",row.names = F)


## Summary of changes over time (Beginning in 2007 and ending in 2015 [Just data for Jan ])
# Add Year as new col
bgc_filtered$Year <- format(bgc_filtered$Date, "%Y")
data_2007 <- subset(bgc_filtered, Year == 2007, select = -c(Year,Date))
data_2008 <- subset(bgc_filtered, Year == 2008, select = -c(Year,Date))
data_2009 <- subset(bgc_filtered, Year == 2009, select = -c(Year,Date))
data_2010 <- subset(bgc_filtered, Year == 2010, select = -c(Year,Date))
data_2011 <- subset(bgc_filtered, Year == 2011, select = -c(Year,Date))
data_2012 <- subset(bgc_filtered, Year == 2012, select = -c(Year,Date))
data_2013 <- subset(bgc_filtered, Year == 2013, select = -c(Year,Date))
data_2014 <- subset(bgc_filtered, Year == 2014, select = -c(Year,Date))
data_2015 <- subset(bgc_filtered, Year == 2015, select = -c(Year,Date))

mean_2007 <- colMeans(data_2007, na.rm = TRUE)
mean_2008 <- colMeans(data_2008, na.rm = TRUE)
mean_2009 <- colMeans(data_2009, na.rm = TRUE)
mean_2010 <- colMeans(data_2010, na.rm = TRUE)
mean_2011 <- colMeans(data_2011, na.rm = TRUE)
mean_2012 <- colMeans(data_2012, na.rm = TRUE)
mean_2013 <- colMeans(data_2013, na.rm = TRUE)
mean_2014 <- colMeans(data_2014, na.rm = TRUE)
mean_2015 <- colMeans(data_2015, na.rm = TRUE)

perc_change <- round((mean_2015 - mean_2007) / mean_2007 * 100,1)

result <- data.frame(
  variable = names(mean_2007),
  mean_2007 = mean_2007,
  mean_2008 = mean_2008,
  mean_2009 = mean_2009,
  mean_2010 = mean_2010,
  mean_2011 = mean_2011,
  mean_2012 = mean_2012,
  mean_2013 = mean_2013,
  mean_2014 = mean_2014,
  mean_2015 = mean_2015,
  perc_2007_to_2015 = perc_change
)


mean(bgc_filtered$S)
mean(bgc_filtered$PH)
mean(bgc_filtered$PO4)



#### PLOTTING
### Temperature Salinity Oxygen PH NH4 NO3 NO2 PO4 SiOH4 POC PON ChlA
## BGC
plot_env <- function(env_data, variable) {
  # Assign color based on variable
  var_color <- case_when(
    variable == 1 ~ "orange",
    variable == 2 ~ "aquamarine4",
    variable == 3 ~ "red3",
    variable == 4 ~ "sienna",
    variable == 5 ~ "green4",
    variable == 6 ~ "royalblue4",
    variable == 7 ~ "cyan",
    variable == 8 ~ "deepskyblue2",
    variable == 9 ~ "purple",
    variable == 10 ~ "hotpink",
    variable == 11 ~ "black",
    variable == 12 ~ "navy",
    variable == 13 ~ "yellow2"
  )
  
  # Assign y-axis label based on variable
  y_label <- case_when(
    variable == 1 ~ "T",
    variable == 2 ~ "S",
    variable == 3 ~ "O",
    variable == 4 ~ "pH",
    variable == 5 ~ "ChlA",
    variable == 6 ~ "NH4",
    variable == 7 ~ "NO3",
    variable == 8 ~ "NO2",
    variable == 9 ~ "PO4",
    variable == 10 ~ "SIOH4",
    variable == 11 ~ "POC",
    variable == 12 ~ "PON",
    variable == 13 ~ "PP"
  )
  
  # Get column names
  x_col <- names(env_data)[1]
  y_col <- names(env_data)[variable + 1]
  
  # Create plot
Plot <- ggplot(env_data, aes(x = .data[[x_col]], y = .data[[y_col]])) +
          geom_line(color = var_color, size = 1.2, alpha = 1) +
          geom_smooth(se = FALSE, color = "gray30", size = 1.2, alpha = 1) +
          scale_x_date(
            date_breaks = "1 year",
            #date_minor_breaks = "3 months",
            limits = c(as.Date("2007-01-01"), as.Date("2015-12-31")),
            expand = c(0.05, 0.05),
            date_labels = "%Y"
          ) +
          labs(x = "Year", y = y_label,
               title = "Environmental Variable\n Over the Study Period (2007–2015)") +
          theme_minimal(base_size = 20) +
          theme(
            axis.text.x = element_text(hjust = 0.5, color = "black", size = 16),  
            axis.text.y = element_text(color = "black", size = 16),  
            axis.title.x = element_text(size = 18, face = "bold", color = "black"),
            axis.title.y = element_text(size = 18, face = "bold", color = "black"),  
            panel.grid.major = element_line(color = "gray80", linewidth = 0.5, linetype = "solid"), 
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 24, face = "bold", hjust = 0.5, color = "black"),
            legend.position = "none",
            strip.text = element_text(size = 20, color = "black", face = "bold"),
            plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
          )
  print(Plot)
  return(Plot)
}


plot_env_multi <- function(env_data, variables){
  plot_list <- lapply(variables, function(var){
    p <- plot_env(env_data, var)
    ifelse(var == min(variables),
      p <- p + labs(title = "Environmental Variables\n Over the Study Period (2007–2015)"),
      p <- p + labs(title = ""))
    if (var != variables[length(variables)]) {
      p <- p + theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()
      )
    }
    return(p)
  })
  wrap_plots(plot_list, ncol = 1)
}



# Save single plots
for (env_var in 1:12){
  #Plot it
  cur_plot <- plot_env(bgc_filtered,env_var)
  # Get Current Plots name
  cur_plot_name <- case_when(
                      env_var == 1 ~ "Temperature",
                      env_var == 2 ~ "Salinity",
                      env_var == 3 ~ "Dissolved_Oxygen",
                      env_var == 4 ~ "pH",
                      env_var == 5 ~ "Chlorophyll_A",
                      env_var == 6 ~ "NH4",
                      env_var == 7 ~ "NO3",
                      env_var == 8 ~ "NO2",
                      env_var == 9 ~ "PO4",
                      env_var == 10 ~ "SIOH4",
                      env_var == 11 ~ "Particulate_Organic_Carbon",
                      env_var == 12 ~ "Particulate Organic Nitrogen",
                      env_var == 13 ~ "Photoperiod")
  
  #Save to plot_path
  ggsave(filename=paste0(cur_plot_name,".pdf"), plot=cur_plot, path=paste0(plot_path, "06_Environmental/Data/Single"))
}

# Save Multiples
# Temp, Photoperiod, Chlorophyll A and Oxygen
multi_plot1 <- plot_env_multi(bgc_filtered, variables = c(1, 13, 5, 3))
multi_plot1
ggsave(filename="Temp_Photoperiod_ChlA_Oxy.pdf", plot=multi_plot1, path=paste0(plot_path, "06_Environmental/Data"), width =12, height= 10)

# Ammonium, Nitrite, Nitrate, and Particulate Organic Nitrogen
multi_plot2 <- plot_env_multi(bgc_filtered, variables = c(6, 8, 7, 12))
multi_plot2
ggsave(filename="Ammonium_Nitrite_Nitrate_PON.pdf", plot=multi_plot2, path=paste0(plot_path, "06_Environmental/Data"), width =12, height= 10)

# Salinity, pH, Phosphate, Silicate and Particulate Organic Carbon
multi_plot3 <- plot_env_multi(bgc_filtered, variables = c(2, 4, 9, 10, 11))
multi_plot3
ggsave(filename="Sal_pH_PO4_SiOH4_POC.pdf", plot=multi_plot3, path=paste0(plot_path, "06_Environmental/Data"), width =12, height= 10)


#  Variables respective measurement unit
'
T [°C] - Temperature
S [ppt] - Salinity
O [mg/L] - Dissolved Oxygen
pH - pH
NH4 [µM] - Ammonium
NO3 [µM] - Nitrite
NO2 [µM] - Nitrate
PO4 [µM] - Phosphate
SiOH4 [µM] - Silicate
POC [µg/L] - Particulate Organic Carbon
PON [µg/L] - Particulate Organic Nitrogen
ChlA [µg/L] - Chlorophyll A
PP [min] - Photoperiod
'