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

# attempt analysis ----

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

m1 = glmmTMB(growth ~ site*species*tube*years_growth + (1|site.plot), data = clean, na.action = "na.fail")
m2 = glmmTMB(growth ~ site+species+tube + site+species*years_growth + species*tube*years_growth + site*tube*years_growth +(1|site.plot), data = clean)
anova(m2, m1)
## if significatn choose the more complex model, not significant taking away the 4 way interaction and using the 3 way doesnt make the model worst
# 
install.packages('MuMIn')
library('MuMIn')

dredge(m1)

# bad seedlings ----
#double checking some bad seedlings
seed_1382 <- master_wide %>%
  filter(UniqueID == 1382)

## WHich ones were mowed?

mower <- master_wide %>%
  filter(Site == "MDI", Plot == 4) %>%
  select(UniqueID, Site, Plot, Cell, Species, starts_with("Notes"))

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
  
# tube ----
tube.info <- master_wide %>%
  select(UniqueID, Site, Species, Tube) %>%
  filter(Tube == "Y") %>%
  group_by(Site, Species) %>%
  summarize(n = n())

plant.info <- master_wide %>%
  select(UniqueID, Site, Species) %>%
  group_by(Site, Species) %>%
  summarize(n = n())

# need to code out the tubes, instead of manually doing it
# key phrases: "no tube" "tube fell" "tube broke" "tube tip"
# "uprooted by tube" "uprooted tube" "tube came off" "tube gone"
# "tube missing" "removed tube" "took tube away" "tube down"
# "tube removed" "outside tube" "tube was down" "no tube found"
# "tube fell over" "tube fallen over" "not tube"
# "tube fell off" "tube remove" 

# what do I do about tube previously removed?

# finding bad tubes through notes
bad.tubes <- master_wide %>%
  select(!starts_with(c("Length", "Browse", "Live"))) %>%
  pivot_longer(
    cols = starts_with("Notes"),
    names_to = "Visit",
    values_to = "Notes") %>%
  filter(Notes == "no tube" | Notes == "tube fell" | Notes == "tube broke" | 
         Notes == "tube tip" | Notes == "uprooted by tube" | Notes == "uprooted tube" | 
         Notes == "tube came off" | Notes == "tube gone" | Notes == "tube missing" |
         Notes == "removed tube" | Notes == "took tube away" | Notes == "tube down" |
         Notes == "tube removed" | Notes == "outside tube" | Notes == "tube was down" |
         Notes == "no tube found" | Notes == "tube fell over" | Notes == "tube fallen over" |
         Notes == "not tube" | Notes == "tube fell off" | Notes == "tube remove")
  
  
# filter(is.na(Notes_summer2019) | Notes_summer2019 == "not planted") %>%  
# filter(grepl("dead total length", Notes_fall2019) | 






# zombies again ----

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

# notes ----
notes <- master_wide %>%
  select(UniqueID, Site, Species, starts_with("Notes"))

# final growth year ----

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
# so I am making the visits their own number, so the beginning summer2019 is 0 and end fall 2024 5.5
# this was I can pull out each seedlings max sampling 
# that should have their final height and last sampling time
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

# getting rid of NAs for length and ) for livedead. Should be left with the living
alive_final_long <- final_live_long %>%
  filter(complete.cases(.) & data > 0) 

# this should be each seedlings final sampling
# NOTE I AM IGNORING THE ZOMBIES (FOR NOW)
a_summary <- alive_final_long %>%
  group_by(sapling.id) %>%
  summarize(sample.period = max(sample.period))

# This should pull back the rest of the data, so the final sampling and final height
a <- left_join(a_summary, alive_final_long, by = c("sapling.id", "sample.period")) 

# trying to get the initial lengths from the seedlings

inital_live_wide <- final_live_wide %>%
  select(sapling.id, Length_summer2019)

a2 <- a %>%
  select(!visit) %>%
  pivot_wider(names_from = measure, values_from = data) %>%
# with this join I am grabbing the initial length for each seedling
  left_join(inital_live_wide, by = "sapling.id") %>%
# here I am finding how much it has grown until it dies
  mutate(growth = length - Length_summer2019) %>%
# cleaning up the columns, grabbing what we need
  mutate(site.plot = paste(site, plot, sep = "_")) %>%
  select(!c(site, plot, planting.date, cell)) %>%
  rename(years.grown = sample.period) %>%
  relocate(years.grown, .after = growth) %>%
  relocate(site.plot, .after = sapling.id) %>%
# putting the plant regions in
  mutate(region = case_when(
    species == "tulip" | species == "s.gum" ~ "southern",
    species == "r.oak" | species == "w.spruce" | species == "w.pine" ~ "local",
    species == "ch.oak" | species == "r.cedar" | species == "w.oak" ~ "maine"), .before = tube)
# next is deleting the initial and the final growth

# jk next step is to find the maximum height for each seedling

max_height <- final_live_long %>%
  filter(measure == "length") %>%
  filter(complete.cases(.) & data > 0) %>%
  group_by(sapling.id) %>%
  summarize(max = max(data))

# adding that max height to the other dataframe

a_clean <- a2 %>%
  left_join(max_height, by = "sapling.id") %>%
  mutate(max.growth = max - Length_summer2019) %>%
  select(!c(length, Length_summer2019, max))
  
write_xlsx(a_clean, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\a_clean.xlsx')










