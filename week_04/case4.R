#case4. Haoming

library(tidyverse)
library(nycflights13)

farthest_distance_df <- flights %>%
  group_by(origin) %>%
  arrange(desc(distance)) %>%
  slice(1)

farthest_airport_pair <- farthest_distance_df[c("origin","dest")]

farthest_airport_pair_join <- left_join(farthest_airport_pair,airports,by=c("dest"="faa"))

farthest_airport_pair_name <- farthest_airport_pair_join[c("origin","dest","name")]

farthest_airport_pair_name