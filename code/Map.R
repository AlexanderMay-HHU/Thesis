library("ggplot2")
library("ggspatial")
library("sf")
#Install HighRes Country Outlines Package
#install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")
library("rnaturalearthhires")



plot_path <- "R:/Studium/Bachelor/Thesis/generated_plots"

theme_set(theme_minimal())

#Load Country Outlines
world <- countries10


gg_map <- ggplot(data = world) +
  geom_sf(fill="tan") +  #Adds Landmass outlines (Landmass in tan color)
  labs(x="Longitude", y="Latitude")+ #Axis Label
  coord_sf(xlim = c(2.5, 3.5), ylim = c(42, 43), expand = FALSE)+ #Sets Frame (42-43°N;2.5-3.5°E)
  annotate(geom = "point", x = 3.145, y = 42.4883333333, color = "black", size = 2) +  #Adds SOLA Station Point
  annotate(geom = "text", x = 3.275, y = 42.4883333333, label = "SOLA Station", color = "black", size = 5) +  #Adds SOLA Station Label
  annotate(geom = "text", x = 2.8, y = 42.2, label = "Spain", fontface = "bold", color = "darkblue", size = 6) + #Adds Spain Country Label
  annotate(geom = "text", x = 2.8, y = 42.8, label = "France", fontface = "bold", color = "darkblue", size = 6) + #Adds France Country Label
  annotate(geom = "text", x = 3.3, y = 42.8, label = "Mediterranean Sea", fontface = "italic", color = "white", size = 6) + #Adds Mediterranean Sea Label
  annotation_scale(location = "bl", width_hint = 0.5) + #Adds Scale
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm"),
                         style = north_arrow_nautical,width = unit(1.5, "cm"), 
                         height = unit(1.5, "cm")) +  #Adds Notrh Arrow
  theme(panel.grid.major = element_line(color = "steelblue"), panel.background = element_rect(fill = "steelblue"))  #Sets BG-Theme (Ocean in light blue color)


gg_map

#Save to plot_path
ggsave(filename="Simple_Overview_Map.png", plot=gg_map, path=paste(plot_path,"/Map/",sep=""))