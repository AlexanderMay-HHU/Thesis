##Loading Libraries
library("readr")
library("dplyr")
library("openair")
library("ggplot2")

#### IMPORT DATA
#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data/env")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/"



## Sources:
## Cap Bear: https://meteostat.net/en/station/07749?t=2007-10-01/2015-02-02
## Perpignan: https://meteostat.net/en/station/07747?t=2007-10-01/2015-02-02


### Weather at Cap Bear (~6km NW of SOLA; ~30km NW of SOLA)
weather_cap_bear <- read.csv("weather_cap_bear.csv")
weather_cap_bear <- weather_cap_bear[,c(1:4,7:8,10)]
weather_perpignan <- read.csv("weather_perpignan.csv")
weather_perpignan <- weather_perpignan[,c(1:5,7:8,10)]

# Rename column variable names & fix date
names(weather_cap_bear)[names(weather_cap_bear) == "tavg"] <- "temp_avg"
names(weather_cap_bear)[names(weather_cap_bear) == "tmin"] <- "temp_min"
names(weather_cap_bear)[names(weather_cap_bear) == "tmax"] <- "temp_max"
names(weather_cap_bear)[names(weather_cap_bear) == "wdir"] <- "wind_dir"
names(weather_cap_bear)[names(weather_cap_bear) == "wspd"] <- "wind_speed_kmh"
names(weather_cap_bear)[names(weather_cap_bear) == "pres"] <- "air_pressure"
weather_cap_bear[,"date"] <- as.Date(weather_cap_bear[,"date"]) 
weather_cap_bear[,"wind_speed_ms"] <- round(weather_cap_bear[,"wind_speed_kmh"]/3.6,2)

names(weather_perpignan)[names(weather_perpignan) == "tavg"] <- "temp_avg"
names(weather_perpignan)[names(weather_perpignan) == "tmin"] <- "temp_min"
names(weather_perpignan)[names(weather_perpignan) == "tmax"] <- "temp_max"
names(weather_perpignan)[names(weather_perpignan) == "prcp"] <- "precipitation"
names(weather_perpignan)[names(weather_perpignan) == "wdir"] <- "wind_dir"
names(weather_perpignan)[names(weather_perpignan) == "wspd"] <- "wind_speed_kmh"
names(weather_perpignan)[names(weather_perpignan) == "pres"] <- "air_pressure"
weather_perpignan[,"date"] <- as.Date(weather_perpignan[,"date"]) 
weather_perpignan[,"wind_speed_ms"] <- round(weather_perpignan[,"wind_speed_kmh"]/3.6,2)



#### PLOTTING
### Possible parameters:
### temp_avg temp_min temp_max precipitation wind_dir wind_speed air_pressure
## Temperature Perpignan (temp_min & temp_max)
gg_temp_perpignan <- ggplot()+
  geom_line(data = weather_perpignan,
            aes(x=date,y=temp_min),
            color = "blue", size=0.5, alpha = 1)+
  geom_line(data = weather_perpignan,
            aes(x=date,y=temp_max),
            color = "red", size=0.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year",
               date_minor_breaks = "3 months",
               limits = c(as.Date("2007-08-01"),
                          as.Date("2015-03-01")),
               expand = c(0,0.1),
               date_labels = "%m.%y")+
  labs(x = "Date", y = "Air Temperature [°C]", title = "Perpignan")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_temp_perpignan


#Save to plot_path
ggsave(filename="Temperature_min_max.png", plot=gg_temp_perpignan, path=paste0(plot_path, "00_Appendix/06_Environmental/Weather/Perpignan"))



## Temperature Perpignan (temp_avg)
gg_temp_perpignan_avg <- ggplot()+
  geom_line(data = weather_perpignan,
            aes(x=date,y=temp_min),
            size=0.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year",
               date_minor_breaks = "3 months",
               limits = c(as.Date("2007-08-01"),
                          as.Date("2015-03-01")),
               expand = c(0,0.1),
               date_labels = "%m.%y")+
  labs(x = "Date", y = "Air Temperature [°C]", title = "Perpignan")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_temp_perpignan_avg


#Save to plot_path
ggsave(filename="Temperature_avg.png", plot=gg_temp_perpignan_avg, path=paste0(plot_path, "06_Environmental/Weather/Perpignan"))



## Temperature Cap Bear (temp_min & temp_max)
gg_temp_cap_bear <- ggplot()+
  geom_line(data = weather_cap_bear,
            aes(x=date,y=temp_min),
            color = "blue", size=0.5, alpha = 1)+
  geom_line(data = weather_cap_bear,
            aes(x=date,y=temp_max),
            color = "red", size=0.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year",
               date_minor_breaks = "3 months",
               limits = c(as.Date("2007-08-01"),
                          as.Date("2015-03-01")),
               expand = c(0,0.1),
               date_labels = "%m.%y")+
  labs(x = "Date", y = "Air Temperature [°C]", title = "Cap Bear")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_temp_cap_bear


#Save to plot_path
ggsave(filename="Temperature_min_max.png", plot=gg_temp_cap_bear, path=paste0(plot_path, "00_Appendix/06_Environmental/Weather/Cap_Bear"))



## Temperature Perpignan (temp_avg)
gg_temp_cap_bear_avg <- ggplot()+
  geom_line(data = weather_perpignan,
            aes(x=date,y=temp_min),
            size=0.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year",
               date_minor_breaks = "3 months",
               limits = c(as.Date("2007-08-01"),
                          as.Date("2015-03-01")),
               expand = c(0,0.1),
               date_labels = "%m.%y")+
  labs(x = "Date", y = "Air Temperature [°C]", title = "Cap Bear")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_temp_cap_bear_avg


#Save to plot_path
ggsave(filename="Temperature_avg.png", plot=gg_temp_cap_bear_avg, path=paste0(plot_path, "06_Environmental/Weather/Cap_Bear"))




## Precipitation Perpignan
gg_prec_perpignan <- ggplot(data = weather_perpignan,  aes(x=date,y=precipitation))+
  geom_bar(stat="identity", color = "steelblue", size=0.5, alpha = 1)+
  scale_x_date(date_breaks = "1 year",
               date_minor_breaks = "3 months",
               limits = c(as.Date("2007-08-01"),
                          as.Date("2015-03-01")),
               expand = c(0,0),
               date_labels = "%m.%y")+
  labs(x = "Date", y = "Precipitation [mm]", title = "Perpignan")+
  theme(axis.text.x=element_text(angle=0, hjust=0.5))
# Show it
gg_prec_perpignan

#Save to plot_path
ggsave(filename="Precipitation.png", plot=gg_prec_perpignan, path=paste0(plot_path, "06_Environmental/Weather/Perpignan"))





### Wind
## Prepare data
wind_perpignan <- weather_perpignan[,c("date","wind_speed_ms","wind_dir")]
wind_cap_bear <- weather_cap_bear[,c("date","wind_speed_ms","wind_dir")]

## Plot it
windplot_perpignan <-  windRose(wind_perpignan,
                                  key.header = "Wind Data for Perpignan",
                                  ws= "wind_speed_ms",
                                  wd= "wind_dir",
                                  breaks=12,
                                  ws.int = 2.777,
                                  angle = 45,
                                  angle.scale = 30,
                                  width = 0.6,
                                  grid.line = 5,
                                  key.position="right")

# Show it
  print(windplot_perpignan$plot)
  
# Save it (Needs to be saved manually)


    
## Plot it
windplot_cap_bear <- windRose(wind_cap_bear,
                               key.header = "Wind Data for Cap Bear",
                               ws= "wind_speed_ms",
                               wd= "wind_dir",
                               breaks=12,
                               ws.int = 2.777,
                               angle = 45,
                               angle.scale = 30,
                               width = 0.6,
                               grid.line = 5,
                               key.position="right")
# Show it
print(windplot_cap_bear$plot)

# Save it (Needs to be saved manually)