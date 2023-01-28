library(dplyr)
library(ggplot2)
library(ggmap)
library(htmlwidgets)
library(widgetframe)
library(dygraphs)
library(tidyverse)
library(rnoaa)
library(xts)


#get data from dataset
d=meteo_tidy_ghcnd("USW00014733",
                   date_min = "2016-01-01", 
                   var = c("TMAX"),
                   keep_flags=T) %>% 
  mutate(date=as.Date(date),
         tmax=as.numeric(tmax)/10) #Divide the tmax data by 10 to convert to degrees.

#Use dygraph to draw the plot

data=xts(d$tmax, order.by = d$date)
dygraph(data, main="Daily Maximum Temperature in Buffalo, NY") %>%
  dyRangeSelector(dateWindow = c("2020-01-01", "2020-10-31"))