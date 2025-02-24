##Loading Libraries
library("readr")
library("dplyr")
library("lubridate")
library("ggplot2")

#### IMPORT DATA
#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data/env")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/"


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
Seasons[month(Seasons$xstart) %in% 3:5,"Season"] <- "Spring"
Seasons[month(Seasons$xstart) %in% 6:8,"Season"] <- "Summer"
Seasons[month(Seasons$xstart) %in% 9:11,"Season"] <- "Autumn"
Seasons[month(Seasons$xstart) %in% c(1:2,12),"Season"] <- "Winter"
# Set Colors
Seasons[Seasons$Season == "Spring","Season_Color"] <- "#2BCE48"
Seasons[Seasons$Season == "Summer","Season_Color"] <- "#990000"
Seasons[Seasons$Season == "Autumn","Season_Color"] <- "#F0A3FF"
Seasons[Seasons$Season == "Winter","Season_Color"] <- "#5EF1F2"




#### PLOTTING
### Temperature Salinity Oxygen PH NH4 NO3 NO2 PO4 SiOH4 COP NOP MES dn15 dc13 ChlA
## BGC
# Temperature
gg_temp <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                                          aes(x=Date,y=T),
                                          color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=T),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "Temperature [°C]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
  gg_temp


# Add Season Background
gg_temp_seasons <-  gg_temp + 
                      geom_rect(data = Seasons, aes(xmin = xstart,
                                                    xmax = xend,
                                                    ymin = -Inf,
                                                    ymax = Inf),
                                fill = Seasons$Season_Color, alpha = 0.75)
# Show it
  gg_temp_seasons


#Save to plot_path
ggsave(filename="Temperature.png", plot=gg_temp, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="Temperature.png", plot=gg_temp_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))


  
# Salinity
gg_sal <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                        aes(x=Date,y=S),
                        color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=S),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "Salinity [ppt]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_sal


# Add Season Background
gg_sal_seasons <-  gg_sal + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_sal_seasons


#Save to plot_path
ggsave(filename="Salinity.png", plot=gg_sal, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="Salinity.png", plot=gg_sal_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# Oxygen
gg_oxy <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                        aes(x=Date,y=O),
                        color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=O),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "Dissolved Oxygen [mg/L]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_oxy


# Add Season Background
gg_oxy_seasons <-  gg_oxy + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_oxy_seasons


#Save to plot_path
ggsave(filename="Oxygen.png", plot=gg_oxy, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="Oxygen.png", plot=gg_oxy_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# PH
gg_pH <- ggplot()+
          geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                      aes(x=Date,y=PH),
                      color="black", size=1.2, span=0.1)+
          geom_point(data = Filtered_Bio_Geochemical_Compounds,
                     aes(x=Date,y=PH),
                     color = "black", size=1.5, alpha = 1)+
          scale_x_date(date_breaks = "1 year",
                       date_minor_breaks = "3 months",
                       limits = c(as.Date("2007-08-01"),
                                  as.Date("2015-03-01")),
                       expand = c(0,0.1),
                       date_labels = "%m.%y")+
          labs(x = "Date", y = "pH", title = "")+
          theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_pH


# Add Season Background
gg_pH_seasons <-  gg_pH + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_pH_seasons


#Save to plot_path
ggsave(filename="pH.png", plot=gg_pH, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="pH.png", plot=gg_pH_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# NH4 (Ammonium)
gg_NH4 <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                        aes(x=Date,y=NH4),
                        color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=NH4),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "NH4 [µM]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_NH4


# Add Season Background
gg_NH4_seasons <-  gg_oxy + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_NH4_seasons


#Save to plot_path
ggsave(filename="Ammonium.png", plot=gg_NH4, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="Ammonium.png", plot=gg_NH4_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# NO3 (Nitrate)
gg_NO3 <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                        aes(x=Date,y=NO3),
                        color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=NO3),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "NO3 [µM]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_NO3


# Add Season Background
gg_NO3_seasons <-  gg_NO3 + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_NO3_seasons


#Save to plot_path
ggsave(filename="Nitrate.png", plot=gg_NO3, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="Nitrate.png", plot=gg_NO3_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# NO2 (Nitrite)
gg_NO2 <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                        aes(x=Date,y=NO2),
                        color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=NO2),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "NO2 [µM]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_NO2


# Add Season Background
gg_NO2_seasons <-  gg_NO2 + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_NO2_seasons


#Save to plot_path
ggsave(filename="Nitrite.png", plot=gg_NO2, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="Nitrite.png", plot=gg_NO2_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# PO4 (Phosphate)
gg_PO4 <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                        aes(x=Date,y=PO4),
                        color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=PO4),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "PO4 [µM]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_PO4


# Add Season Background
gg_PO4_seasons <-  gg_PO4 + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_PO4_seasons


#Save to plot_path
ggsave(filename="Phosphate.png", plot=gg_PO4, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="Phosphate.png", plot=gg_PO4_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# SiOH4 (Silicate)
gg_SiOH4 <- ggplot()+
              geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                          aes(x=Date,y=SIOH4),
                          color="black", size=1.2, span=0.1)+
              geom_point(data = Filtered_Bio_Geochemical_Compounds,
                         aes(x=Date,y=SIOH4),
                         color = "black", size=1.5, alpha = 1)+
              scale_x_date(date_breaks = "1 year",
                           date_minor_breaks = "3 months",
                           limits = c(as.Date("2007-08-01"),
                                      as.Date("2015-03-01")), expand = c(0,0.1),
                           date_labels = "%m.%y")+
              labs(x = "Date", y = "SIOH4 [µM]", title = "")+
              theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_SiOH4


# Add Season Background
gg_SiOH4_seasons <-  gg_SiOH4 + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_SiOH4_seasons


#Save to plot_path
ggsave(filename="Silicate.png", plot=gg_SiOH4, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="Silicate.png", plot=gg_SiOH4_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# COP (Particulate Organic Carbon)
gg_POC <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                        aes(x=Date,y=COP),
                        color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=COP),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "Particulate Organic Carbon [µg/L]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_POC


# Add Season Background
gg_POC_seasons <-  gg_POC + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_POC_seasons


#Save to plot_path
ggsave(filename="Particulate_Organic_Carbon.png", plot=gg_POC, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="Particulate_Organic_Carbon.png", plot=gg_POC_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# NOP (Particulate Organic Nitrogen)
gg_PON <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                        aes(x=Date,y=NOP),
                        color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=NOP),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "Particulate Organic Nitrogen [µg/L]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_PON


# Add Season Background
gg_PON_seasons <-  gg_PON + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_PON_seasons


#Save to plot_path
ggsave(filename="Particulate_Organic_Nitrogen.png", plot=gg_PON, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="Particulate_Organic_Nitrogen.png", plot=gg_PON_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# MES (Suspended Particulate Matter)
gg_SPM <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                        aes(x=Date,y=MES),
                        color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=MES),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "Suspended Particulate Matter [mg/L]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_SPM


# Add Season Background
gg_SPM_seasons <-  gg_SPM + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_SPM_seasons


#Save to plot_path
ggsave(filename="Suspended_Particulate_Matter.png", plot=gg_SPM, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="Suspended_Particulate_Matter.png", plot=gg_SPM_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# DN15 (δ-N15 - Nitrogen Isotope | Used for research in Nitrogen Cycle)
gg_DN15 <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                        aes(x=Date,y=DN15),
                        color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=DN15),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "δ-N15 [‰]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_DN15


# Add Season Background
gg_DN15_seasons <-  gg_DN15 + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_DN15_seasons


#Save to plot_path
ggsave(filename="δ-N15.png", plot=gg_DN15, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="δ-N15.png", plot=gg_DN15_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# DC13 (δ-C13 - Carbon Isotope | Used for research in Nitrogen Cycle)
gg_DC13 <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                        aes(x=Date,y=DC13),
                        color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=DC13),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "δ-C13 [‰]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_DC13


# Add Season Background
gg_DC13_seasons <-  gg_DC13 + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_DC13_seasons


#Save to plot_path
ggsave(filename="δ-C13.png", plot=gg_DC13, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="δ-C13.png", plot=gg_DC13_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))



# ChlA (Chlorophyll a)
gg_ChlA <- ggplot()+
            geom_smooth(data = Filtered_Bio_Geochemical_Compounds,
                        aes(x=Date,y=CHLA),
                        color="black", size=1.2, span=0.1)+
            geom_point(data = Filtered_Bio_Geochemical_Compounds,
                       aes(x=Date,y=CHLA),
                       color = "black", size=1.5, alpha = 1)+
            scale_x_date(date_breaks = "1 year",
                         date_minor_breaks = "3 months",
                         limits = c(as.Date("2007-08-01"),
                                    as.Date("2015-03-01")),
                         expand = c(0,0.1),
                         date_labels = "%m.%y")+
            labs(x = "Date", y = "Chlorophyll a [µg/L]", title = "")+
            theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_ChlA


# Add Season Background
gg_ChlA_seasons <-  gg_ChlA + 
  geom_rect(data = Seasons, aes(xmin = xstart,
                                xmax = xend,
                                ymin = -Inf,
                                ymax = Inf),
            fill = Seasons$Season_Color, alpha = 0.75)
# Show it
gg_ChlA_seasons


#Save to plot_path
ggsave(filename="Chlorophyll_a.png", plot=gg_ChlA, path=paste0(plot_path, "06_Environmental/Data"))
ggsave(filename="Chlorophyll_a.png", plot=gg_ChlA_seasons, path=paste0(plot_path, "00_Appendix/06_Environmental/Seasons"))