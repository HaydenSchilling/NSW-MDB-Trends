# Script to go from full catch download to only electrofishing and Murray-darling

library(tidyverse)
library(sf)
library(spatialEco)

catch_data <- read_csv("../database exploration/All catch_30_11_2022.csv") %>% drop_na(SampleLatitude)

WRAs <-st_read("../database exploration/WRA Shapefile/Surface Water Water Resource Plan Areas.shp") %>%
  dplyr::select(SWWRPANAME) %>% st_make_valid() # select variable wanted and make it valid

# make to spatial data and set projection
catch_data2 <- sf::st_as_sf(catch_data, coords = c("SampleLongitude", "SampleLatitude"), crs = 4283)

# match data to shapefile
xx <- point.in.poly(catch_data2, WRAs, sp = TRUE, duplicate = TRUE)
# back to dataframe and save
catch_data3 <- as.data.frame(xx) %>% filter(Method == "BTE" | Method == "BPE") 
write_csv(catch_data3, "../database exploration/All e-catch_30_11_2022_WRPA.csv")

table(catch_data3$Method)

## Now fix most sites
site_dets <- read_csv("../Site selection/Full Site List with number of sampling events_updated for modelling.csv") %>% select(SiteName, SiteID, SWWRPANAME_NEW)

full_dat2 <- catch_data3 %>% left_join(site_dets) %>%
  mutate(SWWRPANAME_NEW = case_when(!is.na(SWWRPANAME_NEW) ~ SWWRPANAME_NEW,
                                    T ~ SWWRPANAME))




table(full_dat2$SWWRPANAME_NEW, full_dat2$SWWRPANAME)

plot_dat <- full_dat2 %>% distinct(coords.x1, coords.x2, SWWRPANAME_NEW, SWWRPANAME) %>% filter(coords.x1 < 160) %>%
  filter(coords.x1 > 139) %>% filter(coords.x2 < -20)

write_csv(plot_dat, "e-catch sites WRPAs.csv")

WRAs2 <-st_read("../database exploration/WRA Shapefile/Surface Water Water Resource Plan Areas.shp") %>%
  dplyr::select(SWWRPANAME)


ggplot(plot_dat, aes(x=coords.x1, y = coords.x2, col=SWWRPANAME_NEW)) + geom_point() +
  coord_quickmap() + ggspatial::layer_spatial(WRAs2, aes(fill=SWWRPANAME), inherit.aes = F, alpha = 0.1)

ggsave("test.png", height=21, width=28, units = "cm", dpi=600)

### In this gap Camilia checked all the assignments manually in GIS program

catch_data3 <- read_csv("../database exploration/All e-catch_30_11_2022_WRPA.csv")
wrpa_camila <- read_csv("e-catch sites WRPAs_fixed_1_12_22.csv")

catch_data4 <- catch_data3 %>% left_join(wrpa_camila)
write_csv(catch_data4, "../database exploration/All e-catch_30_11_2022_WRPA_fixed.csv")
