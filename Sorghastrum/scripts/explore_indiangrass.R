# Explore Indiangrass data from 1957 - 2020
library(dplyr)
library(ggplot2)

# Read in dataset
df <- read.csv("Sorghastrum/data/individual_phenometrics_data.csv")

# How many individuals are represented
length(unique(df$Individual_ID)) # total of 50 individuals
inds <- unique(df$Individual_ID)

# Map of where individuals are from
conus_states<- map_data("state")

fig_map <- ggplot() +
  geom_polygon(data = conus_states, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +
  geom_point(data = df, aes(x = Longitude, y = Latitude)) +
  scale_fill_gradient2(low = "yellow", mid = "yellowgreen", high = "darkgreen", 
                       midpoint = 1, 
                       breaks = seq(-10, 10, 0.4), 
                       limits = c(0, 2)) +
  coord_cartesian(xlim = c(-113, -80), ylim = c(36, 40.5)) +
  facet_wrap(~genotype_formal, ncol = 1) +
  theme_classic(base_size = 20) +
  theme(panel.background = element_rect(fill = "grey", colour = "grey"), 
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey")) +
  labs(x = "", y = "", fill = "")

# Subset to understand a single individuals
sub <- subset(df, Individual_ID == inds[1])

table(df$Phenophase_Description) 
