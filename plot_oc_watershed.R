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
streams <- read_sp_data("data/streams", "Streams")
city <- read_sp_data("data/city", "city")
meta <- sf::st_read(dsn = fgdb, layer = "PHWA_Metadata")
indices <- sf::st_read(dsn = fgdb, layer = "PHWA_Indices")
hucs <- read_sp_data(.dsn = fgdb,  .layer = "NC_HUC12") %>%
  select(HUC12 = HUC_12) %>%
  mutate(
    HUC10 = substr(HUC12, 1, 10),
    HUC4 = substr(HUC12, 1, 4)
  )

ochucs <- 
  hucs[sf::st_intersects(hucs, county_boundary, sparse = FALSE), ] %>%
  # Add back indices information
  left_join(indices, by = "HUC12")

ocintersect <- ochucs %>%
  select(NAME_HUC12, HUC12, Shape) %>%
  sf::st_intersection(select(county_boundary, geometry)) %>%
  mutate(
    geometry = st_cast(Shape, "POLYGON")
  ) %>%
  # Add indices information
  left_join(indices, by = "HUC12")

# Subregion (HUC4) boundaries
subregions <-
  ochucs %>%
  select(HUC4, Shape) %>%
  group_by(HUC4) %>%
  summarise(
    geometry = st_union(Shape)
  ) %>%
  mutate(
    geometry = st_cast(geometry, "LINESTRING"),
  ) %>%
  st_intersection(st_union(ocintersect))

## Create map of health index ####

PLOT_VAR <- "PHWA_HEALTH_NDX_ST_2016"

hold1 <- 
  indices %>%
  as_tibble() %>%
  select(NAME_HUC12, HUC12, !! rlang::sym(PLOT_VAR) )



GRADIENT_BREAKS  <- 
 hold1 %>%
  mutate(
    in_orange = HUC12 %in% ochucs$HUC12
  ) %>%
  group_by(in_orange) %>%
  summarise_at(
    .vars = vars(one_of(PLOT_VAR)),
    .funs = list(val = ~ list(quantile(., probs = c(0, .5, 1), type = 1)))
  ) %>%
  tidyr::unnest(cols = val) %>%
  left_join(hold1, by = c("val" = PLOT_VAR)) %>%
  filter(
    in_orange | val == min(val) | val == max(val)
  ) %>%
  group_by(in_orange) %>%
  tidyr::nest() %>%
  mutate(
    data = purrr::map2(
      .x = in_orange,
      .y = data,
      .f = ~ {
        pattern <- if (in_orange) {
          "%.2f - %s healthy in county (%s)" 
        } else {
          "%.2f - %s healthy in NC"    
        }
        .y %>% 
        mutate(
          label = case_when(
            val == min(val) ~ sprintf(pattern, val, "Least", NAME_HUC12),
            val == max(val) ~ sprintf(pattern, val, "Most", NAME_HUC12),
            TRUE ~ sprintf("%.2f - Median index in county", val)
          )
        )
      }
        
    )
  ) %>%
  tidyr::unnest(cols = data)

p <-
ochucs %>%
  mutate(
    geometry = st_cast(Shape, "POLYGON"),
    highlight = case_when(
      !! rlang::sym(PLOT_VAR) == min(!! rlang::sym(PLOT_VAR)) ~ "least",
      !! rlang::sym(PLOT_VAR) == max(!! rlang::sym(PLOT_VAR)) ~ "most",
      TRUE ~ "etc"
    )
  ) %>%
  ggplot() +
  geom_sf(size = .1, fill = "white") +
  geom_sf(
    aes(fill = !! rlang::sym(PLOT_VAR),
        color = highlight, 
        size  = highlight),
    alpha = 1
  ) +
  geom_sf(
    data = county_boundary,
    color = "black",
    fill = NA,
    size = .25
  ) + 
  # geom_sf(
  #   data = ocintersect,
  #   aes(fill = !! rlang::sym(PLOT_VAR)),
  #   color = NA,
  #   size = .1
  # ) + 
  geom_sf(
    data = city,
    color = NA,
    # color = "#fdae6b",
    size  = 0.05,
    fill = "grey50",
    alpha = 0.35
  ) + 
  geom_sf(
    data = filter(streams, NAMED == "yes"),
    size = 0.15,
    color = "#3182bd"
  ) + 
  geom_sf(
    data  = subregions,
    size  = .25,
    color = "grey10",
    fill  = NA
  ) + 
  # geom_sf_text(aes(label = HUC_12), size = 2) +
  # scale_fill_brewer(type = "seq", palette = "YlGnBu") +
  scale_fill_gradient2(
    breaks = GRADIENT_BREAKS$val,
    limits = c(0, 1),
    labels = GRADIENT_BREAKS$label,
    low = "#edf8b1", mid = "#7fcdbb", high = "#2c7fb8",
    midpoint = .5) +
  scale_color_manual( 
    values = c("least" = "#7b3294", "etc" = "grey50", "most" = "#008837")
  ) +
  scale_size_manual(
    values = c("least" = 0.45, "etc" = 0.1, "most" = 0.45)
  ) + 
  guides(size = FALSE, colour = FALSE) + 
  labs(
    title = "Watershed Health in Orange County",
    subtitle = meta %>%
      filter(Field_Name == PLOT_VAR) %>%
      pull(Indicator_Name),
    caption = paste("Data from the EPA's 2017 Preliminary Healthy Watersheds Assessments and Orange County GIS.",
                    "Grey shading indicates incorporated areas.", sep = "\n")
  ) +
  theme_void() +
  theme(
    legend.title     = element_blank(), 
    legend.position  = "right", 
    panel.grid.major = element_line(colour = NA),
    plot.caption     = element_text(size = 8, hjust = 0)
  )

ggsave(p, file = "ocWatershedsMap.pdf", width = 11, height = 8.5)  


## Scatterplot ####

scatter_data <- 
  indices %>%
  as_tibble() %>%
  select(NAME_HUC12, HUC12, y = PHWA_HEALTH_NDX_ST_2016, x = PHWA_VULN_NDX_ST_2016) %>%
  mutate(
    in_orange = HUC12 %in% ochucs$HUC12
  )
  
p2 <-
ggplot(
  data = scatter_data,
  aes(x = x, y = y, color = in_orange, size = in_orange,
      shape = in_orange)) +
  geom_point(
    data = filter(scatter_data, !in_orange)
  ) +
  geom_point(
    data = filter(scatter_data, in_orange)
  ) +
  geom_text(
    data = filter(scatter_data, in_orange) %>%
      arrange(desc(x)) %>%
      mutate(most_vulerable = rank(rev(x)) <= 5) %>%
      filter(most_vulerable),
    aes(label = NAME_HUC12),
    size = 2,
    hjust = 1,
    nudge_x = -0.01,
    color = "grey10"
  ) +
  geom_segment(
    data = filter(scatter_data, in_orange) %>%
      arrange(desc(x)) %>%
      mutate(most_vulerable = rank(rev(x)) <= 5) %>%
      filter(most_vulerable),
    aes(x = x - 0.01, xend = 0.2, yend = 0.5),
    size = 0.4,
    color = "grey50"
  ) +
  annotate(
    "text", label = "Most vulnerable\nin Orange County",
    x = 0.2, y = 0.5, hjust = 0,
    size  = 2, 
    color = "grey50"
  ) +
  scale_color_manual(
    guide = FALSE,
    values = c("grey85", "orange")
  ) +
  scale_shape_manual(
    guide = FALSE,
    values = c(1, 20)
  ) +
  scale_size_manual(
    guide = FALSE,
    values = c(0.1, 1.5)
  ) +
  annotate(
    "text", 
    x = 0.9, y = 0.5, label = "More healthy ->",
    angle = 90) + 
  annotate("text", 
     x = 0.5, y = 0.1, label = "Less vulnerable ->") +
  scale_x_reverse(limits = c(1, 0), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  labs(
    title    = "Watershed Health vs. Vulnerability for all NC",
    subtitle = "Highlighting Orange County Watersheds",
    x = "Vulnerability Index",
    y = "Health Index"
  ) +
  theme_classic() +
  theme(
    axis.ticks = element_line(color = "grey50"),
    axis.text  = element_text(color = "grey50"),
    axis.line  = element_line(color = "grey50"),
    axis.title = element_text(color = "grey50")
  )

ggsave(p2, file = "ocWatershedsScatter.pdf", width = 5, height = 5)  
# ochucs %>%
#   as_tibble %>%
#   dplyr::select(-Shape) %>%
#   write.csv(file = "ochucs.csv", row.names = FALSE)
# write.csv(meta, file = "epameta.csv",row.names = FALSE)
