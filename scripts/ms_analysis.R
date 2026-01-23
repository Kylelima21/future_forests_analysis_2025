#### Packages ####
install.packages('DHARMa')
library(tidyverse)
library(readxl)
library(glmmTMB)
library(DHARMa)
library(emmeans)


## Read in old form of data
## Remove individuals that died early (can't rule out transplant shock)
## Adding a site column and make these factors ordered from Belfast to Schoodic
## Calculate growth rate (using initial to final alive height difference)
## Replace this with the final clean data you've created
seedlings <- read_excel("data/finalformat.xlsx") %>%
  as_tibble() %>% 
  filter(years.grown > 1) %>% 
  mutate(site = str_extract(site.plot, "\\w\\w\\w"),
         site = factor(site, levels = c("Bel", "Sur", "MDI", "Sch"), ordered = T),
         growth.rate = total.growth/years.grown) %>% 
  filter(!is.na(growth.rate))

## Check distribution of this growth rate data
## This is good, relatively normally distributed
hist(seedlings$growth.rate)

## Create candidate models starting with the global model (all variables and their 2-way interactions)
## After the global, now create three models that remove one interaction each from the global model
global <- glmmTMB(growth.rate ~ species + tube + site + species:tube + species:site + tube:site + 
                    (1 | site.plot), data = seedlings)
no.tubesite <- glmmTMB(growth.rate ~ species + tube + site + species:tube + species:site +
                         (1 | site.plot), data = seedlings)
no.speciestube <- glmmTMB(growth.rate ~ species + tube + site + species:site + tube:site + 
                            (1 | site.plot), data = seedlings)
no.speciessite <- glmmTMB(growth.rate ~ species + tube + site + species:tube + tube:site + 
                            (1 | site.plot), data = seedlings)

## Before we go further we need to check the global model and see if we need to fix anything
## We will explain this more in person
sim_res <- simulateResiduals(global, plot = T)
testDispersion(sim_res)
testOutliers(sim_res)
testCategorical(sim_res, seedlings$species)
testCategorical(sim_res, seedlings$site)
testCategorical(sim_res, seedlings$tube)

