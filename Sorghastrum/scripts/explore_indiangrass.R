# Explore Indiangrass data from 1957 - 2020
library(dplyr)
library(ggplot2)

# Read in dataset
df <- read.csv("Sorghastrum/data/individual_phenometrics_data.csv") %>%
  mutate(days_to_last_no = ifelse(NumDays_Since_Prior_No == -9999, NA, NumDays_Since_Prior_No))

# How many individuals are represented
length(unique(df$Individual_ID)) # total of 50 individuals
inds <- unique(df$Individual_ID)

# Summarize by individual - how many unique observations
sum.df <- df %>%
  mutate(geo = paste(Latitude, Longitude)) %>%
  group_by(geo) %>%
  summarize(lat = unique(Latitude),
            long = unique(Longitude),
            nobs = length(Phenophase_Description))
str(sum.df)

# Map of where individuals are from
conus_states<- map_data("state")

fig_map <- ggplot() +
  geom_polygon(data = conus_states, aes(x = long, y = lat, group = group), 
               fill = "white", color = "black") +
  geom_point(data = sum.df, aes(x = long, y = lat, color = as.factor(nobs)), size = 2) +
  scale_color_brewer(type = "div", palette = "PiYG") +
  coord_cartesian(xlim = c(-100, -70), ylim = c(35, 48)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank())
fig_map


# Subset to understand a single individuals
sub <- subset(df, Individual_ID == inds[1])

table(df$Phenophase_Description) 

# Reconfigure to be a single row for each individual_ID / year combination
match.df <- df %>%
  group_by(Phenophase_ID) %>%
  summarize(description = unique(Phenophase_Description))

sum(ifelse(c(table(df$Individual_ID, df$First_Yes_Year)) > 0, 1, 0)) # Expect 159 ind/year combos

wide.df <- df %>%
  select(1:12, 14, 17) %>%
  tidyr::pivot_wider(names_from = Phenophase_ID, 
                     values_from = First_Yes_DOY) %>%
  rename(leaves = "489",
         init_growth = "492",
         flower_heads = "493",
         open_flowers = "494",
         pollen_release = "502") %>%
  relocate(init_growth, .before = leaves) %>%
  relocate(open_flowers, .after = flower_heads)

# How many have duplicates/triplicates
which(unlist(lapply(wide.df$init_growth, length)) > 1)
which(unlist(lapply(wide.df$leaves, length)) > 1)
which(unlist(lapply(wide.df$flower_heads, length)) > 1)
which(unlist(lapply(wide.df$open_flowers, length)) > 1)
which(unlist(lapply(wide.df$pollen_release, length)) > 1)

# index of ind/year combos that have a duplicate of at least one phenophase
dup_ind <- unique(c(which(unlist(lapply(wide.df$init_growth, length)) > 1),
                    which(unlist(lapply(wide.df$leaves, length)) > 1),
                    which(unlist(lapply(wide.df$flower_heads, length)) > 1),
                    which(unlist(lapply(wide.df$open_flowers, length)) > 1)))
length(dup_ind)


