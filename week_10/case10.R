#case10

library(raster)
library(rasterVis)
library(rgdal)
library(ggmap)
library(tidyverse)
library(knitr)


library(ncdf4)


dir.create("data", showWarnings = F) #create a folder to hold the data

lulc_url = "https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MCD12Q1.051_aid0001.nc?raw=true"
lst_url = "https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MOD11A2.006_aid0001.nc?raw=true"


download.file(lulc_url,destfile = "data/MCD12Q1.051_aid0001.nc", mode = "wb")
download.file(lst_url,destfile = "data/MOD11A2.006_aid0001.nc", mode = "wb")



lulc = stack("data/MCD12Q1.051_aid0001.nc",varname="Land_Cover_Type_1")
lst = stack("data/MOD11A2.006_aid0001.nc",varname="LST_Day_1km")

plot(lulc)

lulc = lulc[[13]]
plot(lulc)

# Assign land cover clases from MODIS website
Land_Cover_Type_1 <-  c(
  Water = 0, 
  `Evergreen Needleleaf forest` = 1, 
  `Evergreen Broadleaf forest` = 2,
  `Deciduous Needleleaf forest` = 3, 
  `Deciduous Broadleaf forest` = 4,
  `Mixed forest` = 5, 
  `Closed shrublands` = 6,
  `Open shrublands` = 7,
  `Woody savannas` = 8, 
  Savannas = 9,
  Grasslands = 10,
  `Permanent wetlands` = 11, 
  Croplands = 12,
  `Urban & built-up` = 13,
  `Cropland/Natural vegetation mosaic` = 14, 
  `Snow & ice` = 15,
  `Barren/Sparsely vegetated` = 16, 
  Unclassified = 254,
  NoDataFill = 255)


lcd = data.frame(
  ID = Land_Cover_Type_1,
  landcover = names(Land_Cover_Type_1),
  col = c("#000080","#008000","#00FF00", "#99CC00","#99FF99", "#339966", "#993366", "#FFCC99", "#CCFFCC", "#FFCC00", "#FF9900", "#006699", "#FFFF00", "#FF0000", "#999966", "#FFFFFF", "#808080", "#000000", "#000000"),
  stringsAsFactors = F)

kable(head(lcd))


lulc = as.factor(lulc)


levels(lulc) = left_join(levels(lulc)[[1]], lcd, by = "ID")


gplot(lulc) +
  geom_raster(aes(fill = as.factor(value))) +
  scale_fill_manual(values = levels(lulc)[[1]]$col,
                    labels = levels(lulc)[[1]]$landcover,
                    name = "Landcover Type") +
  coord_equal() +
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(ncol = 5, nrow = 3, byrow = TRUE))


offs(lst) =  -273.15
plot(lst[[1:10]])


names(lst)[1:5]


tdates = names(lst) %>%
  gsub(pattern = "X", replacement = "", .) %>% 
  as.Date("%Y.%m.%d")

names(lst) = 1:nlayers(lst)
lst = setZ(lst, tdates) # Get or set z-values

lw = SpatialPoints(data.frame(x = -78.791547, y = 43.007211))
projection(lw) = "+proj=longlat"
lw = lw %>% spTransform(CRSobj = crs(lst))
lw_data = raster::extract(lst, lw, buffer = 1000, fun = mean, na.rm = T) %>%
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "id") %>% 
  as_tibble() %>% 
  rename(Temperature = V1) %>% 
  mutate(Date = getZ(lst))

lw_data %>% 
  ggplot(., aes(x = Date, y = Temperature)) + 
  geom_point() +
  geom_line(color = "blue") +
  geom_smooth(n = 300, span = 0.01) +
  theme_bw()

tmonth = as.numeric(format(getZ(lst),"%m"))
lst_month = stackApply(lst, tmonth, fun = mean)
names(lst_month) =  month.name


gplot(lst_month) + 
  geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = 'blue', high = 'orange') +
  coord_sf(datum = NA)

cellStats(lst_month, mean) %>% 
  as.data.frame() %>% 
  rename(Mean = ".") %>% 
  kable()