#install.packages('lmtest')
#install.packages('glmmTMB')
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
master_wide <- read_excel("data/master_wide.xlsx")

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
  filter(grepl("no tube", Notes) | grepl("tube fell", Notes) | grepl("tube broke", Notes) |
           grepl("tube tip", Notes) | grepl("uprooted by tube", Notes) |
           grepl("uprooted tube", Notes) | grepl("tube came off", Notes) |
           grepl("tube gone", Notes) | grepl("tube missing", Notes) |
           grepl("removed tube", Notes) | grepl("took tube away", Notes) |
           grepl("tube down", Notes) | grepl("tube removed", Notes) |
           grepl("outside tube", Notes) | grepl("tube was down", Notes) |
           grepl("no tube found", Notes) | grepl("tube fell over", Notes) |
           grepl("tube fallen over", Notes) | grepl("not tube", Notes) |
           grepl("tube fell off", Notes) | grepl("tube remove", Notes)) %>%
# Making the visits numerical so I can grab the first one easier
  mutate(
    sample.period = case_when(
      str_detect(Visit, "summer2019") ~ 0,
      str_detect(Visit, "fall2019") ~ 0.5,
      str_detect(Visit, "summer2020") ~ 1,
      str_detect(Visit, "fall2020") ~ 1.5,
      str_detect(Visit, "summer2021") ~ 2,
      str_detect(Visit, "fall2021") ~ 2.5,
      str_detect(Visit, "summer2022") ~ 3,
      str_detect(Visit, "fall2022") ~ 3.5,
      str_detect(Visit, "summer2023") ~ 4,
      str_detect(Visit, "fall2023") ~ 4.5,
      str_detect(Visit, "summer2024") ~ 5,
      str_detect(Visit, "fall2024") ~ 5.5
    )
  ) %>%
  group_by(UniqueID) %>%
  summarize(first.down.tube = min(sample.period)) %>%
# I renamed this to sample.period so it would match the other data frame
# However, as a reminder, it is the first visit the tube went down
  rename(sample.period = first.down.tube) %>%
  rename(sapling.id = UniqueID)

# grabbing the needed info from the long version of the data so I can join

tube.long <- longdat %>%
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
  filter(grepl("LiveDead", visit))

tubes.dead.down <- left_join(bad.tubes, tube.long, by = c("sapling.id", "sample.period"))
# OK so I found the firs time the tube went bad for each seedling and whether it was alive or dead
# there are 163 tubes with notes
tubes.down.seedling.alive <- tubes.dead.down %>%
  filter(data == 1)
# SO these are the true bad tubes? These are the seedlings we need to crop their growth
# So only 79 bad tubes? 
# 13 of them were originally marked N for tubes
# need to grab info about them 
ggplot(tubes.down.seedling.alive, aes(x = species)) +
  geom_bar()

updated.tube.table <- tubes.down.seedling.alive %>%
  group_by(species, site) %>%
  summarize(n = n())


# zombies again ----

# so I have the total zombie (which is sapling.id) and master wide (Unique ID)
# also have to get rid of duplicates maybe from total zombie?
zombie <- longdat %>%
  pivot_wider(names_from = visit, values_from = data) %>%
  select(sapling.id, site, species, starts_with("LiveDead"))

# Trying out seeing if dead multiple visits in a row
zombie1 <- zombie %>%
  filter(LiveDead_fall2019 == 0 & LiveDead_summer2020 == 1) %>%
  select(sapling.id)


write_xlsx(zombie1, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie1.xlsx')

zombie2 <- zombie %>%
  filter(LiveDead_summer2020 == 0 & LiveDead_fall2020 == 1) %>%
  filter(site != "Belfast") %>%
  select(sapling.id)

write_xlsx(zombie2, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie2.xlsx')

zombie3 <- zombie %>%
  filter(LiveDead_fall2020 == 0 & LiveDead_summer2021 == 1) %>%
  filter(site != "Belfast") %>%
  select(sapling.id)

write_xlsx(zombie3, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie3.xlsx')

zombie4 <- zombie %>%
  filter(LiveDead_summer2021 == 0 & LiveDead_fall2021 == 1) %>%
  select(sapling.id)

write_xlsx(zombie4, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie4.xlsx')

zombie5 <- zombie %>%
  filter(LiveDead_fall2021 == 0 & LiveDead_summer2022 == 1) %>%
  select(sapling.id)

write_xlsx(zombie5, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie5.xlsx')

zombie6 <- zombie %>%
  filter(LiveDead_summer2022 == 0 & LiveDead_fall2022 == 1) %>%
  select(sapling.id)

write_xlsx(zombie6, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie6.xlsx')

zombie7 <- zombie %>%
  filter(LiveDead_fall2022 == 0 & LiveDead_summer2023 == 1) %>%
  select(sapling.id)

write_xlsx(zombie7, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie7.xlsx')

zombie8 <- zombie %>%
  filter(LiveDead_summer2023 == 0 & LiveDead_fall2023 == 1) %>%
  select(sapling.id)

write_xlsx(zombie8, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie8.xlsx')

zombie9 <- zombie %>%
  filter(LiveDead_fall2023 == 0 & LiveDead_summer2024 == 1) %>%
  select(sapling.id)

write_xlsx(zombie9, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie9.xlsx')

zombie10 <- zombie %>%
  filter(LiveDead_summer2024 == 0 & LiveDead_fall2024 == 1) %>%
  select(sapling.id)

write_xlsx(zombie10, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie10.xlsx')
# 462 zombie seedlings. We may want to exclude these going forward
# Combining all the zombie files into one long file
totalzombie <-rbind(zombie1, zombie2, zombie3, zombie4, zombie5, zombie6, zombie7, zombie8, zombie9, zombie10)

duplicate <- totalzombie %>%
  count(sapling.id) %>%
  filter(n > 1)
# I manually checked this in excel, and it seems to be correct
write_xlsx(duplicate, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\duplicate_seedlings.xlsx')

duplicate2 <- duplicate %>%
  select(sapling.id)

master_wide_altered <- master_wide %>%
  rename(sapling.id = UniqueID) %>%
  rename(region = Region) %>%
  rename(site = Site) %>%
  rename(plot = Plot) %>%
  rename(cell = Cell) %>%
  rename(species = Species) %>%
  rename(planting.date = PlantingDate) %>%
  rename(tube = Tube)
# got rid of the zombies that are 2 or more resurrections
master_wide_altered <- anti_join(master_wide_altered, duplicate2, by = "sapling.id")

# example?
# Example data frame
df <- data.frame(
  a = c(0, 1, 0, 0),
  b = c(0, 0, 2, 0),
  c = c(1, 0, 0, 0)
)

# Function to check consecutive zeros in a row
has_consec_zeros <- function(row) {
  r <- rle(row == 0)
  any(r$values & r$lengths > 1)
}

# Apply to each row
df$multiple_zeros <- apply(df, 1, has_consec_zeros)

print(df)

# do the zombie has multiple 0s in a row. Specifically >2

has_consec_zeros <- function(row) {
  r <- rle(row == 0)
  any(r$values & r$lengths >2)
}

# Apply to each row
zombie$multiple_zeros <- apply(zombie, 1, has_consec_zeros)


# this still includes the ones that died...
# example
# Example data frame

# Example data
df <- data.frame(
  A = c(1, 0, 3, 5, 0, 2),
  B = c(0, 0, 0, 0, 0, 0),
  C = c(0, 0, 4, 0, 0, 3),
  D = c(0, 0, 0, 0, 0, 0),
  E = c(1, 0, 0, 0, 1, 1)
)

# Function to check each row
check_consecutive_zeros <- function(row) {
  # Convert to logical: TRUE where zero
  zero_flags <- row == 0
  
  # Find run lengths of consecutive zeros
  r <- rle(zero_flags)
  
  # Check if the last run is zeros and length >= 1
  last_is_zero <- tail(r$values, 1) && tail(r$lengths, 1) >= 1
  
  # Check if last run length >= 2 (multiple consecutive zeros)
  multiple_consecutive <- tail(r$values, 1) && tail(r$lengths, 1) >= 2
  
  # Return TRUE if either condition is met
  return(last_is_zero || multiple_consecutive)
}

# Apply to all rows
df$ends_with_zeros <- apply(df, 1, check_consecutive_zeros)

print(df)

# Example dataframe
df <- data.frame(
  A = c(1, 0, 0, 0, 2),
  B = c(0, 0, 0, 0, 0),
  C = c(3, 0, 0, 0, 0),
  D = c(4, 1, 0, 0, 0)
)

# Function to check each row
check_consecutive_zeros <- function(row) {
  # Find positions of consecutive zeros
  zero_runs <- rle(row == 0)
  
  # Check if there is a run of >= 2 zeros
  has_consec <- any(zero_runs$values & zero_runs$lengths >= 2)
  
  # Check if the last run is zeros and goes to the end
  ends_with_zeros <- tail(zero_runs$values, 1) && tail(zero_runs$lengths, 1) >= 2
  
  # Return TRUE only if consecutive zeros exist but don't go to the end
  has_consec && !ends_with_zeros
}

# Apply to all rows
result <- apply(df, 1, check_consecutive_zeros)

# Show results
df$result <- result
df




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

longdat <- read_excel("data/longdat.xlsx")

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

# growth and tube
# tube df is tubes.down.seedling.alive
# final_live_long is good df for finding growth (alive_final_long has the dead ones exlcuded)
# so, take the bad tubes and only grab lengths when the tubes were good

a.bad.tube <- tubes.down.seedling.alive %>%
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
  )
# so anything after that sample period should be ignored
  
  
  # Example data frames
df1 <- data.frame(id = 1:10, value = c(5, 8, 3, 12, 7, 15, 2, 9, 4, 6))
df2 <- data.frame(threshold = 7)  # This holds the cutoff number

# Extract the threshold value safely
cutoff <- df2$threshold[1]

# Filter df1 to keep only rows with value <= cutoff
df1_cropped <- df1 %>%
  filter(value <= cutoff)

# Show result
print(df1_cropped)

# I guess try it...
# grab the bad tubed seedlings from final live
bad.seedling.id <- 

final_live_tube <- left_join(a.bad.tube, final_live_wide, by =c("sapling.id", "plot", "site", "cell",
                                                                "species", "planting.date", "tube")) %>%
  pivot_wider(names_from = Visit, values_from = LiveDead)
  

cutoff2 <- a.bad.tube$sample.period

tube.final <- final_live_long %>%
  filter(sample.period <= cutoff2)




