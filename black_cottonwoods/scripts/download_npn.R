library(rnpn)
library(dplyr)

cottonwood_npn_codes <- npn_species() %>% filter(genus == "Populus", species == "trichocarpa")

cottonwood_phenophases <- npn_download_status_data(request_source = "Kristina Riemer", 
                                                   years = c(2008:2020), 
                                                   species_ids = c(cottonwood_npn_codes$species_id), 
                                                   climate_data = TRUE)

write.csv(cottonwood_phenophases, "black_cottonwoods/data/raw_all_cottonwood.csv", row.names = FALSE)
