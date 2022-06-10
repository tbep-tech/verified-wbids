library(tidyverse)
library(sf)
library(tbeptools)
library(ggspatial)

tbsegshed <- st_make_valid(tbsegshed)

sf::sf_use_s2(FALSE)

# # station data, run 62
# stas <- st_read('https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/IMPAIRED_WATERS/MapServer/1/query?outFields=*&where=1%3D1&f=geojson', quiet = T)

# # wbid polygons, run 62
# wbid <- st_read('https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/WBIDS/MapServer/0/query?outFields=*&where=1%3D1&f=geojson', quiet = T)

# verified wbid polygons, run 62
vwbid <- st_read('https://ca.dep.state.fl.us/arcgis/rest/services/OpenData/IMPAIRED_WATERS/MapServer/5/query?outFields=*&where=1%3D1&f=geojson', quiet = T)

# tbstas <- stas %>%
#   .[tbsegshed,]
# tbwbid <- wbid %>%
#   .[tbsegshed, ]
tbvwbid <- vwbid %>%
  .[tbsegshed, ]

# tmp <- fread('~/Desktop/iwr2002_pv.txt', sep = '\t')
# tmp2 <- tmp[wbid %in% tbwbid$WATERBODY_ID, ]

ggplot() +
  annotation_map_tile(zoom = 10, type = 'cartolight') +
  geom_sf(data = tbvwbid, fill = 'lightgreen') +
  facet_wrap(~PARAMETER_ASSESSED)
