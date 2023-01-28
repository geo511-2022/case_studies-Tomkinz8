#Case6.
#Haoming

library(dplyr)
library(tidyverse)
library(gt)
library(ggplot2)
library(raster)
library(sp)
library(sf)
library(spData)

data(world) 
tmax_monthly = getData(name = "worldclim", var="tmax", res=10)

tmax_monthly

sp_world = world %>% 
  filter(continent != "Antarctica") %>% 
  as(.,"Spatial")

plot(tmax_monthly) 

gain(tmax_monthly) = 1/10
tmax_annual = max(tmax_monthly)
names(tmax_annual) = "tmax"

tmax_country = raster::extract(tmax_annual, sp_world, fun = max, na.rm = T, small = T, sp = T) %>%st_as_sf()

ggplot(tmax_country) +
  geom_sf(aes(fill = tmax)) + 
  scale_fill_viridis_c(name="Annual\nMaximum\nTemperature (C)") + 
  theme_minimal() + 
  theme(legend.position = 'bottom')

tmax_country %>%
  st_set_geometry(NULL) %>%
  group_by(continent) %>%
  dplyr::select(continent, name_long, tmax) %>%
  top_n(1) %>%
  ungroup() %>%
  gt() %>%
  cols_label(
    continent = "Continent",
    name_long = "Country",
    tmax = "Maximum Temperature (C)"
  ) %>%
  tab_header(
    title = "The hottest country in each continent",
  ) %>%
  tab_source_note(
    source_note = "Source: WorldClim"
  )