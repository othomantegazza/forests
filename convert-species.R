library(tidyverse)
library(readxl)

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
        str_detect(dmnnt_t_c_119, '^betula') ~ 'Betulaceae',
        str_detect(dmnnt_t_c_119, '^carpinus') ~ 'Betulaceae',
        str_detect(dmnnt_t_c_119, '^castan') ~ 'Fagaceae',
        str_detect(dmnnt_t_c_119, '^fagus') ~ 'Fagaceae',
        str_detect(dmnnt_t_c_119, '^larix') ~ 'Pinaceae',
        str_detect(dmnnt_t_c_119, '^laurus') ~ 'Lauraceae',
        str_detect(dmnnt_t_c_119, '^oak') ~ 'Fagaceae',
        str_detect(dmnnt_t_c_119, '^picea') ~ 'Pinaceae',
        str_detect(dmnnt_t_c_119, '^pinus') ~ 'Pinaceae',
        str_detect(dmnnt_t_c_119, '^quercus') ~ 'Fagaceae',
        dmnnt_t_c_119 == 'na' ~ NA_character_,
        TRUE ~ 'Other'
      )
    }
  )


out %>% 
  count(dmnnt_t_c_119, family) %>% view()
  write_csv(
    'data/spec-fam.csv'
  )
