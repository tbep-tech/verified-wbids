library(tidyverse)
library(sf)
library(tbeptools)
library(units)
library(here)

tbsegshed <- st_make_valid(tbsegshed)

sf::sf_use_s2(FALSE)

# # station data, run 62
# stas <- st_read('https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/IMPAIRED_WATERS/MapServer/1/query?outFields=*&where=1%3D1&f=geojson', quiet = T)

# # wbid polygons, run 62
# wbid <- st_read('https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/WBIDS/MapServer/0/query?outFields=*&where=1%3D1&f=geojson', quiet = T)

# verified wbid polygons, run 58
# https://geodata.dep.state.fl.us/datasets/verified-list-waterbody-ids-wbids
vwbid <- st_read('https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/IMPAIRED_WATERS/MapServer/5/query?outFields=*&where=1%3D1&f=geojson', quiet = T)

# tbstas <- stas %>%
#   .[tbsegshed,]
# tbwbid <- wbid %>%
#   .[tbsegshed, ]
tbvwbid <- vwbid %>%
  .[tbsegshed, ] %>%
  mutate(
    PARAMETER_ASSESSED = case_when(
      PARAMETER_ASSESSED == 'Bacteria (Beach Advisories)' ~ 'Bacteria (Beach)',
      PARAMETER_ASSESSED == 'Dissolved Oxygen (Percent Saturation)' ~ 'Dissolved Oxygen (% Sat)',
      PARAMETER_ASSESSED == 'Fecal Coliform (SEAS Classification)' ~ 'Fecal Coliform (SEAS)',
      PARAMETER_ASSESSED == 'Nutrients (Total Phosphorus)' ~ 'Nutrients (Total Phosph.)',
      T ~ PARAMETER_ASSESSED
    ),
    areami2 = st_area(.),
    areami2 = set_units(areami2, 'mi2'),
    areami2 = as.numeric(areami2)
  ) %>%
  filter(!PARAMETER_ASSESSED %in% c('Fecal Coliform (3)')) %>%
  filter(!WBID %in% c('1497', '1549A', '1924', '1937', '1896', '1968B', '1885A', '1643', '1633B', '1614', '1567', '1440A', '1508A', '1527B')) %>%
  select(
    wbid = WBID,
    group_number = GROUP_NUMBER,
    parameter = PARAMETER_ASSESSED,
    parameter_group = PARAMETER_GROUP,
    waterbody_type = WATERBODY_TYPE,
    areami2
    )

save(tbvwbid, file = here('data/tbvwbid.RData'))

# tmp <- fread('~/Desktop/iwr2002_pv.txt', sep = '\t')
# tmp2 <- tmp[wbid %in% tbwbid$WATERBODY_ID, ]
