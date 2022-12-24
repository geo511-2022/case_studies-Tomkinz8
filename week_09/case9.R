
#case9
#Haoming

library(sf)
library(tidyverse)
library(ggmap)
library(rnoaa)

library(spData)
data(world)
data(us_states)

dataurl = "https://www.ncei.noaa.gov/data/international-best-track-archive-for-climate-stewardship-ibtracs/v04r00/access/shapefile/IBTrACS.NA.list.v04r00.points.zip"
tdir = tempdir()
download.file(dataurl, destfile = file.path(tdir, "temp.zip"))
unzip(file.path(tdir, "temp.zip"), exdir = tdir)
list.files(tdir)

storm_data <- read_sf(list.files(tdir, pattern=".shp", full.names = T))

storms <- storm_data %>% 
  filter(SEASON >= 1950) %>% 
  mutate_if(is.numeric, function(x) ifelse(x==-999.0, NA, x)) %>% 
  mutate(decade=(floor(year/10)*10))

region <- st_bbox(storm_data)

World <- map_data("world")
ggplot(storms) + 
  geom_map(
    data = World, map = World,
    aes(map_id = region),
    color = "black", fill = "lightgray", size  =0.1
  ) + 
  stat_bin2d(aes(y = st_coordinates(storms)[,2],
                 x = st_coordinates(storms)[,1]),
             bins = 100) +
  facet_wrap(~decade) + 
  scale_fill_distiller(palette = "YlOrRd", trans = "log", direction = -1, breaks = c(1, 10, 100, 1000)) + 
  coord_sf(ylim = region[c(2,4)], xlim = region[c(1,3)]) +   xlab("Longitude") + 
  ylab("Latitude")



states <- us_states %>% 
  st_transform(st_crs(storms)) %>% 
  rename(state = NAME)

storm_states <- st_join(storms, states, join = st_intersects, left = F)
storm_states %>% 
  group_by(state) %>% 
  summarize(storms = length(unique(NAME))) %>% 
  arrange(desc(storms)) %>% 
  slice(1:5)
