##Loading Libraries
library("readr")
library("dplyr")
library("lubridate")
library("ggplot2")

#### IMPORT DATA
#Set Working Directory
project_folder <- dirname(dirname(dirname(rstudioapi::getActiveDocumentContext()$path)))
setwd(paste0(project_folder,"/data/env"))
#Sys.setlocale("LC_ALL", "English")


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



### "Base Variables" - Temperature, Flourescence, PAR & Salinity
Temperature_Fluorescence_PAR_Salinity <- read_delim("Temperature_Fluorescence_PAR_Salinity.csv", 
                                                   delim = ";", escape_double = FALSE, comment = "//", 
                                                   trim_ws = TRUE, skip = 2,na = "999999")
# Rename french to english and "un-capslock" column variable names
names(Temperature_Fluorescence_PAR_Salinity)[names(Temperature_Fluorescence_PAR_Salinity) == "DATE"] <- "Date"
names(Temperature_Fluorescence_PAR_Salinity)[names(Temperature_Fluorescence_PAR_Salinity) == "HEURE"] <- "Time"
names(Temperature_Fluorescence_PAR_Salinity)[names(Temperature_Fluorescence_PAR_Salinity) == "nomSite*"] <- "Site_Name"
names(Temperature_Fluorescence_PAR_Salinity)[names(Temperature_Fluorescence_PAR_Salinity) == "gpsLong*"] <- "gpsLong"
names(Temperature_Fluorescence_PAR_Salinity)[names(Temperature_Fluorescence_PAR_Salinity) == "gpsLat*"] <- "gpsLat"
names(Temperature_Fluorescence_PAR_Salinity)[names(Temperature_Fluorescence_PAR_Salinity) == "TEMPERATURE"] <- "Temperature"
names(Temperature_Fluorescence_PAR_Salinity)[names(Temperature_Fluorescence_PAR_Salinity) == "FLUORESCENCE"] <- "Fluorescence"
names(Temperature_Fluorescence_PAR_Salinity)[names(Temperature_Fluorescence_PAR_Salinity) == "SALINITE"] <- "Salinity"
names(Temperature_Fluorescence_PAR_Salinity)[names(Temperature_Fluorescence_PAR_Salinity) == "PROFONDEUR"] <- "Depth"

# Exclude Salinity from 2013-07-15 (2x 0 ppt, 10 ppt & 17 ppt at 0-1m Depth)
Temperature_Fluorescence_PAR_Salinity$Salinity[Temperature_Fluorescence_PAR_Salinity$Salinity < 18] <- NA



#### Dataset selection
### Highlight dataset specific definitions (Timeframe & Sampling Depth)
df_start_date <- as.Date('2007-10-01')
df_end_date <- as.Date('2015-01-31')
df_Depth <- 3
## Get filtered Dataset
# "Base Variables"
Filtered_Temperature_Fluorescence_PAR_Salinity <- Temperature_Fluorescence_PAR_Salinity
Filtered_Temperature_Fluorescence_PAR_Salinity <- Filtered_Temperature_Fluorescence_PAR_Salinity %>%
                                                    filter(Date >= df_start_date & Date < df_end_date & Depth == df_Depth)


## Define Seasons
Seasons <- data.frame(xstart=as.Date(NA),xend=as.Date(NA),
                      Season=NA,Season_Color="#000000")
Date <- as.Date("2007-09-01")
season_index <- 1
while(Date <= df_end_date){
  # Start & End Dates of season
  Seasons[season_index,"xstart"] <- Date
  Seasons[season_index,"xend"] <- floor_date(Date, 'month') + months(3) - 1
  # Set new Date
  Date = floor_date(Date, 'month') + months(3)
  # Update Season Index
  season_index <- season_index+1
}
# Set Season Name
Seasons[months(Seasons$xstart) %in% month.name[3:5],"Season"] <- "Spring"
Seasons[months(Seasons$xstart) %in% month.name[6:8],"Season"] <- "Summer"
Seasons[months(Seasons$xstart) %in% month.name[9:11],"Season"] <- "Autumn"
Seasons[months(Seasons$xstart) %in% month.name[c(1:2,12)],"Season"] <- "Winter"
# Set Colors
Seasons[Seasons$Season == "Spring","Season_Color"] <- "#2BCE48"
Seasons[Seasons$Season == "Summer","Season_Color"] <- "#990000"
Seasons[Seasons$Season == "Autumn","Season_Color"] <- "#F0A3FF"
Seasons[Seasons$Season == "Winter","Season_Color"] <- "#5EF1F2"




#### PLOTTING
## "Base Values"
# Temperature
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_point(data = Filtered_Temperature_Fluorescence_PAR_Salinity, aes(x=Date,y=Temperature),
             color = "black", size=2, alpha = 1)+
  geom_smooth(data = Filtered_Temperature_Fluorescence_PAR_Salinity, aes(x=Date,y=Temperature),
              color="black", size=0.5, span=0.1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "Temperature [°C]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))


# Fluorescence
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.5) +
  geom_point(data = Filtered_Temperature_Fluorescence_PAR_Salinity, aes(x=Date,y=Fluorescence),
             color = "black", size=2, alpha = 1)+
  geom_smooth(data = Filtered_Temperature_Fluorescence_PAR_Salinity, aes(x=Date,y=Fluorescence),
              color="black", size=1, span=0.1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "Fluorescence [?]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# PAR (Not useable as in data is not in specified timeframe)

# Salinity
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.5) +
  geom_point(data = Filtered_Temperature_Fluorescence_PAR_Salinity, aes(x=Date,y=Salinity),
             color = "black", size=2, alpha = 1)+
  geom_smooth(data = Filtered_Temperature_Fluorescence_PAR_Salinity, aes(x=Date,y=Salinity),
              color="black", size=1, span=0.1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "Salinity [ppt]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))