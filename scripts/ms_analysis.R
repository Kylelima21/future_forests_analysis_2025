#### Packages ####
#install.packages('emmeans')
#install.packages('DHARMa')
library(tidyverse)
library(readxl)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(writexl)

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

# down to 911 seedlings
write_xlsx(seedlings, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\seedlings.xlsx')

## Check distribution of this growth rate data
## This is good, relatively normally distributed
hist(seedlings$growth.rate)

## Create candidate models starting with the global model (all variables and their 2-way interactions)
## After the global, now create three models that remove one interaction each from the global model
global <- glmmTMB(growth.rate ~ species + tube + site + species:tube + species:site + tube:site + 
                    (1 | site.plot), data = seedlings, dispformula = ~ species)
no.tubesite <- glmmTMB(growth.rate ~ species + tube + site + species:tube + species:site +
                         (1 | site.plot), data = seedlings, dispformula = ~ species)
no.speciestube <- glmmTMB(growth.rate ~ species + tube + site + species:site + tube:site + 
                            (1 | site.plot), data = seedlings, dispformula = ~ species)
no.speciessite <- glmmTMB(growth.rate ~ species + tube + site + species:tube + tube:site + 
                            (1 | site.plot), data = seedlings, dispformula = ~ species)

## Before we go further we need to check the global model and see if we need to fix anything
## We will explain this more in person
sim_res <- simulateResiduals(global, plot = T)
testDispersion(sim_res)
testOutliers(sim_res)
testCategorical(sim_res, seedlings$species)
testCategorical(sim_res, seedlings$site)
testCategorical(sim_res, seedlings$tube)
testUniformity(sim_res)
## This is where we are doing the Likelihood Ratio Test
## We compare each model that removed one of the interactions to the global model
## If any of the results of the model comparisons are not significant, than we take that model as the top model
anova(no.tubesite, global)
anova(no.speciestube, global) ## p-value not significant = better model than global model
# when I updated the final data frame, I believe this value did become significant. so not better than the global model
anova(no.speciessite, global)


## So, the model that removed the species:tube interaction was better than the global model
## The others were not which means we need to keep the species:site and tube:site interactions
## Lets now rerun our top model ("no.species.tube") with REML = T now
topmod <- glmmTMB(growth.rate ~ species + tube + site + species:tube + species:site + tube:site + 
                    (1 | site.plot), data = seedlings, dispformula = ~ species, REML = T)


## Look at a very confusing summary of the model
## Next steps are to get estimates from the model so we can present results and make figures
summary(topmod)

s <- emmeans(topmod, trt.vs.ctrl ~ tube|species+ site)
s.df <- as.data.frame(s$emmeans)

ggplot(s.df, aes(fill = tube, y = emmean, x = species)) +
  geom_bar(position='dodge', stat='identity') + facet_wrap(~ site) +
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), 
                position = position_dodge(0.9), width = 0.2) +
  scale_x_discrete(guide = guide_axis(angle = 90), 
                   labels = c("Chestnut oak", "Red cedar", "Red oak", "Sweet gum",
                              "Tulip tree", "White oak", "White pine", "White spruce")) +
  labs(x = "Species", y = "Growth Rate (cm/yr)") +
  theme_classic() +
  theme(strip.background =element_rect(fill="lightgray"),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

ggplot(seedlings, aes(fill = tube, y = growth.rate, x = species)) +
  geom_bar(position='dodge', stat='identity') + facet_wrap(~ site) +
  scale_x_discrete(guide = guide_axis(angle = 90), 
                   labels = c("Chestnut oak", "Red cedar", "Red oak", "Sweet gum",
                              "Tulip tree", "White oak", "White pine", "White spruce")) +
  labs(x = "Species", y = "Growth Rate (cm/yr)") +
  theme_classic() +
  theme(strip.background =element_rect(fill="lightgray"),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) #+
#  scale_y_continuous(limits =  c(-45,75))

seedling.draft <- seedlings %>%
  group_by(site, tube, species) %>%
  summarise(avg.growth.rate = mean(growth.rate),
            sd = sd(growth.rate))


ggplot(seedling.draft, aes(fill = tube, y = avg.growth.rate, x = species)) +
  geom_bar(position='dodge', stat='identity') + facet_wrap(~ site) +
  scale_x_discrete(guide = guide_axis(angle = 90), 
                   labels = c("Chestnut oak", "Red cedar", "Red oak", "Sweet gum",
                              "Tulip tree", "White oak", "White pine", "White spruce")) +
  labs(x = "Species", y = "Growth Rate (cm/yr)") +
  theme_classic() +
  theme(strip.background =element_rect(fill="lightgray"),
        panel.border = element_rect(color = "black", fill = NA, size = 1))

seedling.count <- seedlings %>%
  group_by(site, species) %>%
  summarise(n = n())
