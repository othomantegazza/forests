library(tidyverse)
library(readxl)
library(sf)
library(leaflet)
library(units)
library(here)

sf_use_s2(FALSE)

forest_type <- 
  read_csv('data/forest-type.csv') %>% 
  arrange(code)

dat <- read_excel(
  'data/fede-2023-10-12/AnyConv.com__forest-by-name2.xlsx'
) %>% 
  janitor::clean_names()

spec_to_fam <- 
  tribble(
    ~species, ~family,
    "Pinus sylvestris", "Pinaceae",
    "Betula pubescens", "Betulaceae",
    "Picea abies", "Pinaceae",
    "Alnus glutinosa", "Betulaceae",
    "Fagus sylvatica", "Fagaceae",
    "Quercus robur", "Fagaceae",
    "Abies alba", "Pinaceae",
    "Betula spp", "Betulaceae",
    "Laurus novocanarien", "Lauraceae",
    "Apollonias barbujana", "Lauraceae",
    "Ocotea foetens", "Lauraceae",
    "Pinus heldreichii", "Pinaceae",
    "Carpinus betulus", "Betulaceae",
    "Pinus mugo", "Pinaceae",
    "Pinus nigra", "Pinaceae",
  ) %>% 
  mutate(species = species %>% str_to_lower())

out <- 
  dat %>% 
  transmute(
    dmnnt_t_c_119 = dmnnt_t_c_119 %>% str_to_lower()
  ) %>% 
  separate(dmnnt_t_c_119, into = 'dmnnt_t_c_119', sep = ' - ') %>% 
  left_join(spec_to_fam, 
            by = c('dmnnt_t_c_119' = 'species')) %>% 
  mutate(
    family = family %>% {
      case_when(
        ! is.na(.) ~ .,
        str_detect(dmnnt_t_c_119, '^abies') ~ 'Pinaceae',
        str_detect(dmnnt_t_c_119, '^alnus|^alna') ~ 'Betulaceae',
        str_detect(dmnnt_t_c_119, '^apollonias') ~ 'Lauraceae',
        str_detect(dmnnt_t_c_119, '^betula') ~ 'Betulaceae',
        str_detect(dmnnt_t_c_119, '^carpinus') ~ 'Betulaceae',
        str_detect(dmnnt_t_c_119, '^castan') ~ 'Fagaceae',
        str_detect(dmnnt_t_c_119, '^fagus') ~ 'Fagaceae',
        str_detect(dmnnt_t_c_119, '^larix') ~ 'Pinaceae',
        str_detect(dmnnt_t_c_119, '^laurus') ~ 'Lauraceae',
        str_detect(dmnnt_t_c_119, '^oak') ~ 'Fagaceae',
        str_detect(dmnnt_t_c_119, '^ocotea') ~ 'Lauraceae',
        str_detect(dmnnt_t_c_119, '^picea') ~ 'Pinaceae',
        str_detect(dmnnt_t_c_119, '^pinus') ~ 'Pinaceae',
        str_detect(dmnnt_t_c_119, '^quercus') ~ 'Fagaceae',
        dmnnt_t_c_119 == 'na' ~ NA_character_,
        TRUE ~ 'Other'
      )
    }
  )


out %>% 
  count(dmnnt_t_c_119, family) %>%
  write_csv(
    'data/spec-fam.csv'
  )

  

# from fede's qgis ----------------------------------------------

# |- Polygons ---------------------------------------------------

d_sf <- 
  read_sf('data/fede-2023-10-12/PrimaryForest_EU_OA_WGS84(2)/polygons/')
  # read_sf('data/PrimaryForest_EU_OA_WGS84_polygons/')
  
d_sf <- 
  d_sf %>% 
    mutate(
    family = DOMINANT_T %>% 
      str_to_lower %>% {
        case_when(
          str_detect(., '^abies') ~ 'Pinaceae',
          str_detect(., '^alnus|^alna') ~ 'Betulaceae',
          str_detect(., '^apollonias') ~ 'Lauraceae',
          str_detect(., '^betula') ~ 'Betulaceae',
          str_detect(., '^carpinus') ~ 'Betulaceae',
          str_detect(., '^castan') ~ 'Fagaceae',
          str_detect(., '^fagus') ~ 'Fagaceae',
          str_detect(., '^larix') ~ 'Pinaceae',
          str_detect(., '^laurus') ~ 'Lauraceae',
          str_detect(., '^oak') ~ 'Fagaceae',
          str_detect(., '^ocotea') ~ 'Lauraceae',
          str_detect(., '^picea') ~ 'Pinaceae',
          str_detect(., '^pinus') ~ 'Pinaceae',
          str_detect(., '^quercus') ~ 'Fagaceae',
          is.na(.) ~ NA_character_,
          TRUE ~ 'Other'
        )
      }
    )
  
d_sf %>% 
  as_tibble() %>%
  count(BIOGEOGRAP, sort = T) # %>% view()

d_sf_centr <- 
  d_sf %>% 
  mutate(area_m2 = d_sf %>% st_area() %>% round()) %>% 
  mutate(geometry = geometry %>% st_centroid())

d_sf_centr %>% 
  as_tibble() %>% 
  ggplot() +
  aes(x = Area_ha,
      y = area_m2) +
  geom_point()

# |- Points ---------------------------------------------------

d_points <- 
  read_sf('data/fede-2023-10-12/PrimaryForest_EU_OA_WGS84(2)/points/')

d_points <- 
  d_points %>% 
  mutate(
    family = DOMINANT_T %>% 
      str_to_lower %>% {
        case_when(
          str_detect(., '^abies') ~ 'Pinaceae',
          str_detect(., '^alnus|^alna') ~ 'Betulaceae',
          str_detect(., '^apollonias') ~ 'Lauraceae',
          str_detect(., '^betula') ~ 'Betulaceae',
          str_detect(., '^carpinus') ~ 'Betulaceae',
          str_detect(., '^castan') ~ 'Fagaceae',
          str_detect(., '^fagus') ~ 'Fagaceae',
          str_detect(., '^larix') ~ 'Pinaceae',
          str_detect(., '^laurus') ~ 'Lauraceae',
          str_detect(., '^oak') ~ 'Fagaceae',
          str_detect(., '^ocotea') ~ 'Lauraceae',
          str_detect(., '^picea') ~ 'Pinaceae',
          str_detect(., '^pinus') ~ 'Pinaceae',
          str_detect(., '^quercus') ~ 'Fagaceae',
          is.na(.) ~ NA_character_,
          TRUE ~ 'Other'
        )
      }
  )

d_points %>% 
  as_tibble() %>%
  count(DOMINANT_T, family, sort = T)

d_points %>% 
  write_sf('data/fede-2023-10-12/PrimaryForest_EU_OA_WGS84(2)/out-points.shp')


# put them together ---------------------------------------------

d_all <- 
  d_sf_centr %>% 
  bind_rows(d_points)

units(d_all$area_m2) <- make_units(km^2)

d_all <- 
  d_all %>% 
  rename(area_km2 = area_m2)

d_all %>% 
  write_sf(
    'data/fede-2023-10-12/PrimaryForest_EU_OA_WGS84(2)/out-all-to-points.shp'
  )


# forest type by area -------------------------------------------

d_all %>% 
  as_tibble() %>% 
  group_by(BIOGEOGRAP) %>% 
  summarise(area_km2 = area_km2 %>% sum(na.rm = T)) %>% 
  arrange(desc(area_km2)) %>% 
  write_csv(
    here("data/fede-2023-10-12/PrimaryForest_EU_OA_WGS84(2)",
    "biogeography-by-area.csv"
    )
  )

d_all %>% 
  as_tibble() %>% 
  group_by(FOREST_TYP) %>% 
  summarise(area_km2 = area_km2 %>% sum(na.rm = T)) %>% 
  arrange(desc(area_km2)) %>% 
  left_join(forest_type,
            by = c("FOREST_TYP" = "code")) %>% 
  select(-FOREST_TYP) %>% 
  write_csv(
    here("data/fede-2023-10-12/PrimaryForest_EU_OA_WGS84(2)",
         "forest-type-by-area.csv"
    )
  )



# leaflet(
#   d_all
# ) %>%
#   addProviderTiles(providers$CartoDB.Positron) %>%
#   addCircles()
# 