#case5.Haoming


library(spData)
library(sf)
library(tidyverse)
data(world)  
data(us_states)

albers = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

canada_buffer =  world %>% 
  st_transform(crs = albers) %>% 
  filter(name_long == "Canada") %>% 
  st_buffer(dist = 10000)

ny =  us_states %>% 
  st_transform(crs = albers) %>% 
  filter(NAME == "New York")

intersection = st_intersection(canada_buffer, ny)

ggplot() +
  geom_sf(data = ny) +
  geom_sf(data = intersection, fill = "red")