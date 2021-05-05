Exploratory Plotting of Dogwood Clones
================

Read in necessary libraries.

``` r
library(dplyr)
library(tidyr)
library(maps)
library(ggplot2)
```

Read in dogwood data extracted from NPN API and state polygons for
plotting.

``` r
base_map <- map_data("state")
dogwood_obs <- read.csv("../data/raw_all_dogwood.csv")
```

Clean up date for plotting, restricting to only Cornus florida species.

``` r
plot_obs <- dogwood_obs %>% 
  separate(observation_date, c("year", "month", "day"), remove = FALSE) %>% 
  group_by(year, individual_id) %>%
  slice(1) %>% 
  ungroup() %>% 
  filter(longitude > -130, 
         species %in% c("florida", "florida-appalachianspring")) %>% 
  mutate(Clone = ifelse(species == "florida-appalachianspring", "Yes", "No"), 
         Clone = factor(Clone, levels = c("Yes", "No")))
```

Plot locations of dogwoods and clones by year.

``` r
ggplot() +
  geom_polygon(data = base_map, aes(x = long, y = lat, group = group), 
               fill = "white", color = "grey") +
  geom_point(data = plot_obs, aes(x = longitude, y = latitude, color = Clone)) +
  scale_color_manual(values = c("red", "black")) +
  coord_quickmap() +
  facet_wrap(~year) +
  labs(x = "Longitude", y = "Latitude") +
  theme_classic() + 
  theme(legend.position = "top")
```

![](plot_npn_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->