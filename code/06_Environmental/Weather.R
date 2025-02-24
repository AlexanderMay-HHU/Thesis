##Loading Libraries
library("readr")
library("dplyr")
library("lubridate")
library("openair")
library("ggplot2")

#### IMPORT DATA
#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data/env")
plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots/"

season_colors <- c(
  "Winter" = "#5EF1F2",
  "Spring" = "#2BCE48",
  "Summer" = "#990000",
  "Autumn" = "#F0A3FF"
)



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

### Wind
## Prepare data
wind_perpignan <- weather_perpignan[,c("date","wind_speed_ms","wind_dir")]
wind_cap_bear <- weather_cap_bear[,c("date","wind_speed_ms","wind_dir")]


precipitation <- weather_perpignan %>% select(date,precipitation) %>%
  mutate(Month_Start = floor_date(date, "month")) %>%
  group_by(Month_Start) %>%
  summarise(Monthly_Precipitation = sum(precipitation, na.rm = TRUE)) %>%
  mutate(
    Month = month(Month_Start),
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Winter",
      Month %in% c(3, 4, 5) ~ "Spring",
      Month %in% c(6, 7, 8) ~ "Summer",
      Month %in% c(9, 10, 11) ~ "Autumn",
      TRUE ~ "Other"
    )
  )



# Generate the plot


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
gg_prec_perpignan <- ggplot(precipitation, aes(x = Month_Start, y = Monthly_Precipitation, fill = Season)) +
                              geom_col(width = 20) +
                              scale_fill_manual(values = season_colors) +
                              labs(
                                title = "Monthly Precipitation Colored by Season (2007-2015)",
                                x = "Year",
                                y = "Total Precipitation",
                                fill = "Season"
                              ) +
                              scale_x_date(
                                date_breaks = "1 year",
                                date_labels = "%Y",
                                limits = c(min(precipitation$Month_Start), max(precipitation$Month_Start))
                              ) +
                              theme_minimal() +
                              theme(
                                axis.text.x = element_text(angle = 45, hjust = 1),
                                legend.position = "bottom"
                              )
# Show it
gg_prec_perpignan

#Save to plot_path
ggsave(filename="Precipitation.png", plot=gg_prec_perpignan, path=paste0(plot_path, "06_Environmental/Weather/Perpignan"))





### Wind
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