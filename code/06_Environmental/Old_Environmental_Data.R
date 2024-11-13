##Loading Libraries
library("dplyr")
library("ggplot2")

#Set Working Directory
setwd("R:/Studium/Bachelor/Thesis/data/env")


ARGO <- read.csv("ARGO_BGC_Feb2024.csv", header=TRUE)
env_filt <- read.csv("environmental_data.csv", header = T) %>% as.data.frame()


sort(unique(substring(ARGO$JULD_LOCATION,1,10)))

## Declutter name of edges into from and to ASVs
Argo_surface <- ARGO %>% filter(lat > 42.04, lat < 42.54)
Argo_surface1 <- ARGO %>% filter(lon > 2.83, lon < 3.33)


env_filt$time <- as.Date(env_filt$time)


ggplot(env_filt, aes(x = time,
                             y = SurfaceSeaTemperature)) +
  geom_line()+
  geom_point()+
  labs(x = "Date", y = "Sea surface temperature [°C]", title = "")+# Labels
  scale_x_date(date_breaks = "3 months", date_labels = "%b-%y")+
  scale_y_continuous(breaks = seq(10,26,2))+
  theme(axis.text.x=element_text(angle=60, hjust=1))
