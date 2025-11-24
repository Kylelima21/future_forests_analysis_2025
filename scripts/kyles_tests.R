library(readxl)
library(tidyverse)
library(stringr)



longdat <- read_excel("data/data_long.xlsx") %>% 
  rename(unique.id = UniqueID, planting.date = PlantingDate) %>% 
  rename_with(tolower)


full_clean <- longdat %>% 
  mutate(measure = str_extract(visit, "(\\w*)\\_", group = 1),
         measure = tolower(measure),
         sample.period = str_extract(visit, "\\_(\\w*)", group = 1),
         year = str_extract(sample.period, "\\w*(\\w\\w\\w\\w)", group = 1),
         sample.period = str_extract(sample.period, "(\\w*)\\w\\w\\w\\w", group = 1)) %>% 
  select(-visit) %>% 
  pivot_wider(names_from = measure, values_from = data) %>% 
  mutate(region = case_when(
      species == "tulip" | species == "s.gum" ~ "southern",
      species == "r.oak" | species == "w.spruce" | species == "w.pine" ~ "local",
      species == "ch.oak" | species == "r.cedar" | species == "w.oak" ~ "maine"), .before = tube)


t <- full_clean %>% 
  select(-c(year, browse, livedead)) %>% 
  pivot_wider(names_from = sample.period, values_from = length) %>% 
  mutate(growth = fall2024 - summer2019)
  



         