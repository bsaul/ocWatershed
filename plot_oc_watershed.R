#------------------------------------------------------------------------------#
#  TITLE: Plot OC watershed for 2019 SOE
#   DATE: 20190101
#   PROG: B.Saul
#   DESC: Create a plot of OC watersheds for the 2019 State of the Environment
#         reprot
#------------------------------------------------------------------------------#

library(spatial)
library(rgdal)
library(dplyr)
library(sp)
library(rgeos)
library(sf)
library(ggplot2)

# EPA data downloaded from:
# https://www.epa.gov/sites/production/files/2017-06/nc_phwa_package_170518.zip

## Load spatial data ####
projproj <- function(x) spTransform(x, CRS("+init=epsg:4326"))

read_sp_data <- function(.dsn, .layer){
  sf::st_transform(sf::st_read(dsn = .dsn, layer = .layer), 2264)
}

fgdb <- file.path("data", "nc_phwa_package_170518",
                  "NC_PHWA_Geodatabase_170518.gdb")

county_boundary <- read_sp_data("data/county", "CountyLine")
meta <- sf::st_read(dsn = fgdb, layer = "PHWA_Metadata")
indices <- sf::st_read(dsn = fgdb, layer = "PHWA_Indices")
hucs <- read_sp_data(.dsn = fgdb, 
                     .layer = "NC_HUC12")

ochucs <- hucs[sf::st_intersects(hucs, county_boundary, sparse = FALSE), ] %>%
  left_join(indices, by = "NAME_HUC12")

ocintersect <- sf::st_intersection(ochucs, county_boundary) %>%
  mutate(
    geometry = st_cast(Shape, "POLYGON")
  )



ochucs %>%
  mutate(
    geometry = st_cast(Shape, "POLYGON")
  ) %>%
  ggplot() +
  geom_sf(size = .1, fill = "white") +
  geom_sf(
    data = ocintersect,
    aes(fill = PHWA_VULN_NDX_ER_2016),
    size = .1
  ) + 
  geom_sf_text(aes(label = HUC_12), size = 2) +
  scale_fill_gradient(low = "grey90", high = "grey10") +
  labs(
    title = meta %>%
      filter(Field_Name == "PHWA_VULN_NDX_ER_2016") %>%
      pull(Indicator_Name)
  ) +
  theme_void() +
  theme(
    legend.title     = element_blank(), 
    panel.grid.major = element_line(colour = NA),
  )

  

# ochucs %>%
#   as_tibble %>%
#   dplyr::select(-Shape) %>%
#   write.csv(file = "ochucs.csv", row.names = FALSE)
# write.csv(meta, file = "epameta.csv",row.names = FALSE)
