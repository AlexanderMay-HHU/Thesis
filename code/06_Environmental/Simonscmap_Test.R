## Simons C Map
install.packages("devtools")
devtools::install_github("simonscmap/cmap4r/cmap4r")

## Package for data processing:
install.packages("dplyr")  

## Packages for visualization:
install.packages("ggplot2")
install.packages("plotly")


library(cmap4r)
library(dplyr)
library(ggplot2)
library(plotly)
library(knitr)

set_authorization(cmap_key = "181156fb-afd7-469a-8750-b1a628e9612a")
set_authorization(181156fb-afd7-469a-8750-b1a628e9612a)
set_authorization(reset=F,"181156fb-afd7-469a-8750-b1a628e9612a")

get_catalog() %>%
  select(Variable, Table_Name, Unit, Sensor, Unit) %>%
  head(20)


get_head("tblArgoMerge_REP") %>% kable() 