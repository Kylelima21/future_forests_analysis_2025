install.packages('lmtest')
install.packages('glmmTMB')
library('glmmTMB')
library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
library(ggplot2)
library(forcats)
library(lmtest)

# cleanest version of the data in long form
longdat <- read_excel("data/longdat.xlsx")
longdat2 <- read_excel("data/longdat2.xlsx")
# pulled out variables that we are interested in analyzing
clean24 <- read_excel("data/clean24.xlsx")

clean24 <- clean24 %>%
  mutate(years_growth = runif(nrow(clean24), 0, 5))

## needs site and plot combined
clean <- longdat2 %>%
  select(sapling.id, site, plot, species, region, tube, sample.period, year, livedead, growth) %>%
  filter(sample.period == "fall" & year == "2024") %>%
  select(-c(sample.period, year)) %>%
  mutate(site.plot = paste(site, plot, sep = "_")) %>%
  mutate(years_growth = runif(nrow(clean24), 0, 5)) %>%
  filter(!is.na(growth))

############
m1 = glmmTMB(growth ~ site*species*tube*years_growth + (1|site.plot), data = clean, na.action = "na.fail")
m2 = glmmTMB(growth ~ site+species+tube + site+species*years_growth + species*tube*years_growth + site*tube*years_growth +(1|site.plot), data = clean)
anova(m2, m1)
## if significatn choose the more complex model, not significant taking away the 4 way interaction and using the 3 way doesnt make the model worst
# 
install.packages('MuMIn')
library('MuMIn')

dredge(m1)
########################################
#double checking some bad seedlings
seed_1382 <- master_wide %>%
  filter(UniqueID == 1382)

a <- longdat %>%
  filter(!grepl("Browse", visit)) %>%
  select(!c(planting.date, cell)) %>%
  mutate(
    measure = case_when(
      str_detect(visit, "Length") ~ "length",
      str_detect(visit, "LiveDead") ~ "livedead",
    )
  ) %>%
  mutate(
    sample.period = case_when(
      str_detect(visit, "fall") ~ "fall",
      str_detect(visit, "summer") ~ "summer"
    )
  ) %>%
  mutate(
    year = case_when (
      str_detect(visit, "2019") ~ "2019",
      str_detect(visit, "2020") ~ "2020",
      str_detect(visit, "2021") ~ "2021",
      str_detect(visit, "2022") ~ "2022",
      str_detect(visit, "2023") ~ "2023",
      str_detect(visit, "2024") ~ "2024",
    )
  ) %>%
  select(-visit) %>%
  filter(!is.na(data)) %>%
  filter(data != 0.0) %>%
  pivot_wider(names_from = measure, values_from = data) %>%
  mutate(visit = paste(sample.period, year, sep = ".")) %>%
  select(!c(sample.period, year)) %>%
  pivot_wider(names_from = visit, values_from )
  
#############tube nightmare
tube.info <- master_wide %>%
  select(UniqueID, Site, Species, Tube) %>%
  filter(Tube == "Y") %>%
  group_by(Site, Species) %>%
  summarize(n = n())

plant.info <- master_wide %>%
  select(UniqueID, Site, Species) %>%
  group_by(Site, Species) %>%
  summarize(n = n())

###########################zombies again

# so I have the total zombie (which is sapling.id) and master wide (Unique ID)
# also have to get rid of duplicates maybe from total zombie?

z <- master_wide %>%
  rename(sapling.id = UniqueID) %>%
  semi_join(duplicate,master_wide,  by = "sapling.id")

# I think it worked, but some are missing? duplicate has 63 rows and z has 55? close enough for my purposes

ztotal <- totalzombie %>%
  count(sapling.id) 

z2 <- master_wide %>%
  rename(sapling.id = UniqueID) %>%
  semi_join(ztotal,master_wide,  by = "sapling.id")

write_xlsx(z2, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\z2_zombiehistory.xlsx')

ggplot(z2, aes(x = Species)) +
  geom_bar()

##### looking at notes really fast
notes <- master_wide %>%
  select(UniqueID, Site, Species, starts_with("Notes"))

#### final year there is data
# wide version
final_live_wide <- longdat %>%
  pivot_wider(names_from = visit, values_from = data) %>%
  select(!starts_with("Browse"))
final_live_wide[final_live_wide == 0] <- NA


#long version

final_live_long <- longdat %>%
  mutate(
    measure = case_when(
      str_detect(visit, "Length") ~ "length",
      str_detect(visit, "Browse") ~ "browse",
      str_detect(visit, "LiveDead") ~ "livedead",
      str_detect(visit, "Total") ~ "growth"
    )
  ) %>%
  mutate(
    sample.period = case_when(
      str_detect(visit, "summer2019") ~ 0,
      str_detect(visit, "fall2019") ~ 0.5,
      str_detect(visit, "summer2020") ~ 1,
      str_detect(visit, "fall2020") ~ 1.5,
      str_detect(visit, "summer2021") ~ 2,
      str_detect(visit, "fall2021") ~ 2.5,
      str_detect(visit, "summer2022") ~ 3,
      str_detect(visit, "fall2022") ~ 3.5,
      str_detect(visit, "summer2023") ~ 4,
      str_detect(visit, "fall2023") ~ 4.5,
      str_detect(visit, "summer2024") ~ 5,
      str_detect(visit, "fall2024") ~ 5.5
    )
  ) %>%
  filter(measure != "browse")

#getting rid of NAs for length and ) for livedead. Should be left with the living
alive_final_long <- final_live_long %>%
  filter(complete.cases(.) & data >0) 

a_summary <- alive_final_long %>%
  group_by(sapling.id) %>%
  summarize(sample.period = max(sample.period))
a <- left_join(a_summary, alive_final_long, by = c("sapling.id", "sample.period")) 







mtcars %>%
  group_by(cyl) %>%
  summarize(mean_mpg = mean(mpg, na.rm = TRUE))

## WHich ones were mowed?

mower <- master_wide %>%
  filter(Site == "MDI", Plot == 4) %>%
  select(UniqueID, Site, Plot, Cell, Species, starts_with("Notes"))


