##Loading Libraries
library("readr")
library("dplyr")
library("lubridate")
library("ggplot2")

#### IMPORT DATA
#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data/env")


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
Bio_Geochemical_Compounds <- read_delim("Bio_Geochemical_Compounds.csv", 
                                        delim = ";", escape_double = FALSE, comment = "//", 
                                        trim_ws = TRUE, skip = 2,na = "999999")
# Rename french to english and "un-capslock" column variable names
names(Bio_Geochemical_Compounds)[names(Bio_Geochemical_Compounds) == "DATE"] <- "Date"
names(Bio_Geochemical_Compounds)[names(Bio_Geochemical_Compounds) == "HEURE"] <- "Time"
names(Bio_Geochemical_Compounds)[names(Bio_Geochemical_Compounds) == "COEF_MAREE"] <- "Coefficient_Tide"
names(Bio_Geochemical_Compounds)[names(Bio_Geochemical_Compounds) == "MAREE"] <- "Tide"
names(Bio_Geochemical_Compounds)[names(Bio_Geochemical_Compounds) == "nomSite*"] <- "Site_Name"
names(Bio_Geochemical_Compounds)[names(Bio_Geochemical_Compounds) == "gpsLong*"] <- "gpsLong"
names(Bio_Geochemical_Compounds)[names(Bio_Geochemical_Compounds) == "gpsLat*"] <- "gpsLat"



#### Dataset selection
### Highlight dataset specific definitions (Timeframe & Sampling Depth)
df_start_date <- as.Date('2007-10-01')
df_end_date <- as.Date('2015-01-31')
## Get filtered Dataset
# BGC
Filtered_Bio_Geochemical_Compounds <- Bio_Geochemical_Compounds
Filtered_Bio_Geochemical_Compounds <- Filtered_Bio_Geochemical_Compounds %>% filter(Date >= df_start_date & Date < df_end_date)
Filtered_Bio_Geochemical_Compounds <- Filtered_Bio_Geochemical_Compounds %>% filter(PROF_TEXT == "S")



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
### Temperature Salinity Oxygen PH NH4 NO3 NO2 PO4 SiOH4 COP NOP MES dn15 dc13 ChlA
## BGC
# Temperature
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=T),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=T),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "Temperature [°C]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# Salinity
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=S),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=S),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "Salinity [ppt]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# Oxygen
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=O),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=O),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "Dissolved Oxygen [mg/L]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# PH
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=PH),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=PH),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "pH", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# NH4 (Ammonium)
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=NH4),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=NH4),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "NH4 [µM]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# NO3 (Nitrate)
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=NO3),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=NO3),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "NO3 [µM]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# NO2 (Nitrite)
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=NO2),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=NO2),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "NO2 [µM]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# PO4 (Phosphate)
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=PO4),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=PO4),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "PO4 [µM]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# SiOH4 (Silicate)
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=SIOH4),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=SIOH4),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "SIOH4 [µM]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# COP (Particulate Organic Carbon)
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=COP),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=COP),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "COP [µg/L]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# NOP (Particulate Organic Nitrogen)
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=NOP),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=NOP),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "NOP [µg/L]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# MES (Suspended Particulate Matter)
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=MES),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=MES),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "Suspended Particulate Matter [mg/L]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))


# DN15 (δ-N15 - Nitrogen Isotope | Used for research in Nitrogen Cycle)
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=DN15),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=DN15),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "δ-N15 [‰]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))

# DC13 (δ-C13 - Carbon Isotope | Used for research in Nitrogen Cycle)
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=DC13),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=DC13),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "δ-C13 [‰]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))


# ChlA (Chlorophyll a)
ggplot()+
  geom_rect(data = Seasons, aes(xmin = xstart, xmax = xend, ymin = -Inf, ymax = Inf), fill = Seasons$Season_Color, alpha = 0.75) +
  geom_smooth(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=CHLA),
              color="black", size=1.2, span=0.1)+
  geom_point(data = Filtered_Bio_Geochemical_Compounds, aes(x=Date,y=CHLA),
             color = "black", size=1.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "3 months",
               limits = c(Seasons[1,"xstart"],Seasons[length(Seasons$xend),"xend"]), expand = c(0,0.1),
               date_labels = "%b / %Y")+
  labs(x = "Date", y = "Chlorophyll a [µg/L]", title = "")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))