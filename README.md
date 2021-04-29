# NPN-Data

USA National Phenology Network Extracted and Modified  Data

Initial exploratory analysis:

```r
library(tidyverse)

x <- read_csv('individual_phenometrics_data2008_2020trim.csv') %>% 
  filter(Species == 'florida-appalachianspring')

library(ggplot2)
library(ggmap)
usa <- map_data("usa")


y <- x %>% group_by(Latitude, Longitude) %>%  summarise(n = n())
ggplot() + 
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), fill = 'white') +
  geom_point(data = y, aes(x = Longitude, y = Latitude, color = n)) + 
#  coord_equal() + 
  xlab('Longitude') + 
  ylab('Latitude') +
  scale_color_viridis_c(direction = -1) 
  
y %>% filter(n == 1320)
# A tibble: 1 x 3
# Groups:   Latitude [1]
#  Latitude Longitude     n
#     <dbl>     <dbl> <int>
# 1     36.0     -84.3  1320
```

![image](https://user-images.githubusercontent.com/464871/116623935-de9cae80-a8fb-11eb-8fb8-bb89a270a204.png)

