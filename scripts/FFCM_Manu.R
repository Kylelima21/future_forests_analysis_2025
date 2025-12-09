# Loading in packages
#install.packages("readxl")
#install.packages('tidyverse')
#install.packages('dplyr')
#install.packages('writexl')
#install.packages('ggplots')
#install.packages('forcats')
library(readxl)
library(tidyverse)
library(dplyr)
library(writexl)
library(ggplot2)
library(forcats)

# STEP 1: GRAB THE DATA
belfast <- read_excel("C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM_Data_Belfast_2024.xlsx")
mdi <- read_excel("C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM_Data_MDI_2024.xlsx")
schoodic <- read_excel("C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM_Data_SchoodicNorth_2024.xlsx")
surry <- read_excel("C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM_Data_SurryForest_2024.xlsx")

# STEP 2: MAKE ALL COLUMN NAMES THE SAME
# STEP 3: MAKE ALL SITES HAVE SAME NUMBER OF COLUMNS
# STEP 4: MISC CLEANING OF COLUMNS

belfast <- belfast %>%
  rename(
    PlantingDate = `Planting Date`,
    Length_summer2019 = `Initial length (cm)_05/24/19`,
    Notes_summer2019 = notes,
    Length_fall2019 = `length_cm 10/19`,
    Notes_fall2019 = `notes 7Oct19`,
    LiveDead_fall2019 = `LiveDead Oct2019`,
    
    LiveDead_summer2020 = `L/D 6/17/20`,
    Browse_summer2020 = `Browse 6/17/20`,
    Length_summer2020 = `Length 6.17.20(cm)`,
    Notes_summer2020 = `Notes 6/17/20`,
    
    LiveDead_summer2021 = `L/D 6/07/21`,
    Browse_summer2021 = `Browse 6/07/21`,
    Length_summer2021 = `Length 6/07/21(cm)`,
    Notes_summer2021 = `Notes 6/07/21`,
    LiveDead_fall2021 = `L/D 10/25/21`,
    Browse_fall2021 = `Browse 10/25/21`,
    Length_fall2021 = `Length 10/25/21 (cm)`,
    Notes_fall2021 = `Notes 10/25/21`,
    
    LiveDead_summer2022 = `L/D 7/7/22`,
    Browse_summer2022 = `Browse Y/N 7/7/22`,
    Length_summer2022 = `Length (cm) 7/7/22`,
    LiveDead_fall2022 = `L/D 10/20/22`,
    Browse_fall2022 = `Browse Y/N 10/20/22`,
    Length_fall2022 = `Length 10/20/22`,
    Notes_fall2022 = `Notes 10/20/22`,
    
    LiveDead_summer2023 = `L/D July 2023`,
    Browse_summer2023 = `Browse Y/N July 2023`,
    Length_summer2023 = `Length July 2023`,
    Notes_summer2023 = `Notes July 2023`,
    LiveDead_fall2023 = `L/D 10/23/23`,
    Length_fall2023 = `Length 10/23/23`,
    Notes_fall2023 = `Notes 10/23/23`,
    
    LiveDead_summer2024 = `L/D 6/13/24`,
    Browse_summer2024 = `Browse Y/N 6/13/24`,
    Length_summer2024 = `Length 6/13/24`,
    Notes_summer2024 = `Notes 6/13/24`,
    LiveDead_fall2024 = `L/D 10/29/24`,
    Browse_fall2024 = `Browse 10/29/24`,
    Length_fall2024 = `Length 10/29/24`,
    Notes_fall2024 = Notes
  ) %>%
#Note that Belfast was not visited in fall 2020, so I added those columns
  mutate(
    LiveDead_fall2020 = NA,
    Browse_fall2020 = NA, 
    Length_fall2020 = NA,
    Notes_fall2020 = NA, .after = Notes_summer2020
  ) %>%
#Belfast is also missing some note and browse columns
  mutate( 
    Notes_summer2022 = NA, .after = Length_summer2022
  ) %>%
  mutate(
    Browse_fall2023 = NA, .after = LiveDead_fall2023
  )

mdi <- mdi %>%
  rename(
    PlantingDate = `Planting Date`,
    Tube = `Tube (Y/N)`,
    
    Length_summer2019 = `Initial length (cm)_05/30/19`,
    Notes_summer2019 = notes,
    Length_fall2019 = `length_cm 10/19`,
    Notes_fall2019 = `notes 14Oct19`,
    LiveDead_fall2019 = `LiveDead Oct2019`,
    
    LiveDead_summer2020 = `L/D 6/29/20`,
    Browse_summer2020 = `Browse Y/N 6/29/20`,
    Length_summer2020 = `Length 6/29/20`,
    Notes_summer2020 = `Notes 6/29/20`,
    LiveDead_fall2020 = `L/D 10/01/20`,
    Length_fall2020 = `Length 10/01/20`,
    Notes_fall2020 = `Notes 10/01/20`,
    
    LiveDead_summer2021 = `L/D 6/4/2021`,
    Browse_summer2021 = `Browse 6/4/2021`,
    Length_summer2021 = `Length 6/4/2021`,
    Notes_summer2021 = `Notes 6/4/2021`,
    LiveDead_fall2021 = `L/D 10/15/21`,
    Browse_fall2021 = `Browse Y/N 10/15/21`,
    Length_fall2021 = `Length 10/15/21`,
    Notes_fall2021 = `Notes 10/14/21`,
    
    LiveDead_summer2022 = `L/D 06/21/22`,
    Browse_summer2022 = `Browse Y/N 06/21/22`,
    Length_summer2022 = `Length 06/21/22`,
    Notes_summer2022 = `Notes 06/21/22`,
    LiveDead_fall2022 = `L/D 11/22`,
    Browse_fall2022 = `Browse Y/N 11/22`,
    Length_fall2022 = `Length 11/22`,
    Notes_fall2022 = `Notes 11/22`,
    
    LiveDead_summer2023 = `L/D July 2023`,
    Browse_summer2023 = `Browse Y/N July 2023`,
    Length_summer2023 = `Length July 2023`,
    Notes_summer2023 = `Notes July 2023`,
    LiveDead_fall2023 = `L/D October 12 2023`,
    Browse_fall2023 = `Browse Y/N October 12 2023`,
    Length_fall2023 = `Length October 12 2023`,
    Notes_fall2023 = `Notes October 12 2023`,
    
    LiveDead_summer2024 = `L/D 6/10/24`,
    Browse_summer2024 = `Browse Y/N 6/10/24`,
    Length_summer2024 = `Length 6/10/24`,
    Notes_summer2024 = `Notes 6/10/24`,
    LiveDead_fall2024 = `L/D 10/28/24`,
    Browse_fall2024 = `Browse Y/N 10/28/24`,
    Length_fall2024 = `Length 10/28/24`,
    Notes_fall2024 = `Notes 10/28/24`
  ) %>%
  mutate(
    Browse_fall2020 = NA, .after = LiveDead_fall2020
  )
#Note: Schoodic Planting Data column doesn't read as a date, fixed.
schoodic$`Planting Date` <- as.Date(43617.0, origin = "1899-12-30")

schoodic <- schoodic %>%
  rename(
    PlantingDate = `Planting Date`,
    Tube = `Tube (Y/N)`,
    
    Length_summer2019 = `Initial length (cm)`,
    Notes_summer2019 = notes,
    Length_fall2019 = `length_cm 10/19`,
    Notes_fall2019 = `notes 11Oct19`,
    LiveDead_fall2019 = `LiveDead Oct2019`,
    
    LiveDead_summer2020 = `L/D 6/23/20`,
    Browse_summer2020 = `Browse Y/N 6/23/20`,
    Length_summer2020 = `Length 6/23/20`,
    Notes_summer2020 = `Notes 6/23/20`,
    LiveDead_fall2020 = `LiveDead9/11/20`,
    Length_fall2020 = `Length9/11/20`,
    Notes_fall2020 = `Notes9/11/20`,
    
    LiveDead_summer2021 = `L/D 6/11/21`,
    Browse_summer2021 = `Browse Y/N 6/11/21`,
    Length_summer2021 = `Length 6/11/21`,
    Notes_summer2021 = `Notes 6/11/21`,
    LiveDead_fall2021 = `L/D 10/25/21`,
    Browse_fall2021 = `Browse Y/N 10/25/21`,
    Length_fall2021 = `Length 10/25/21`,
    Notes_fall2021 = `Notes 10/25/21`,
    
    LiveDead_summer2022 = `L/D 06/21/22`,
    Browse_summer2022 = `Browse Y/N 06/21/22`,
    Length_summer2022 = `Length 06/21/22`,
    Notes_summer2022 = `Notes 06/21/22`,
    LiveDead_fall2022 = `L/D 10/20/22`,
    Browse_fall2022 = `Browse Y/N 10/20/22`,
    Length_fall2022 = `Length 10/20/22`,
    Notes_fall2022 = `Notes 10/20/22`,
    
    LiveDead_summer2023 = `LD 6/19/23`,
    Browse_summer2023 = `Browse 6/19/23`,
    Length_summer2023 = `Length 6/19/23`,
    Notes_summer2023 = `Notes 6/19/23`,
    LiveDead_fall2023 = `LD 10/13/23`,
    Length_fall2023 = `Length 10/13/23`,
    Notes_fall2023 = `Notes 10/13/23`,
    
    LiveDead_summer2024 = `L/D 6/6/24`,
    Browse_summer2024 = `Browse 6/6/24`,
    Length_summer2024 = `Length 6/6/24`,
    Notes_summer2024 = `Notes 6/6/24`,
    LiveDead_fall2024 = `L/D 10/28/24`,
    Browse_fall2024 = `Browse 10/28/24`,
    Length_fall2024 = `Length 10/28/24`,
    Notes_fall2024 = `Notes 10/28/24`
  ) %>%
  mutate(
    Browse_fall2020 = NA, .after = LiveDead_fall2020
  ) %>%
  mutate(
    Browse_fall2023 = NA, .after = LiveDead_fall2023
  )

#Note: Surry had two corrected columns; used the corrected data
surry <- surry %>%
  mutate(`L/D July 2023` = NULL,
         `Length July 2023` = NULL
  ) %>%
  rename(
    PlantingDate = `Planting Date`,
    Tube = `Tube (Y/N)`,
    
    Length_summer2019 = `Initial length (cm)_05/24/19`,
    Notes_summer2019 = `notes`,
    Length_fall2019 = `length_cm 10/19`,
    Notes_fall2019 = `notes 8Oct19`,
    LiveDead_fall2019 = `LiveDead Oct2019`,
    
    LiveDead_summer2020 = `L/D 6/23/20`,
    Browse_summer2020 = `Browse Y/N 6/23/20`,
    Length_summer2020 = `Length 6/23/20`,
    Notes_summer2020 = `Notes 6/23/20`,
    LiveDead_fall2020 = `L/D 9/30/20`,
    Length_fall2020 = `Length 9/30/20`,
    Notes_fall2020 = `Notes 9/30/20`,
    
    LiveDead_summer2021 = `L/D 6/11/21`,
    Browse_summer2021 = `Browse Y/N 6/11/21`,
    Length_summer2021 = `Length 6/11/21`,
    Notes_summer2021 = `Notes 6/11/21`,
    LiveDead_fall2021 = `L/D 10/21`,
    Browse_fall2021 = `Browse Y/N 10/21`,
    Length_fall2021 = `Length 10/21`,
    Notes_fall2021 = `Notes 10/21`,
    
    LiveDead_summer2022 = `L/D 06/22`,
    Browse_summer2022 = `Browse Y/N 06/22`,
    Length_summer2022 = `Length 06/22`,
    Notes_summer2022 = `Notes 06/22`,
    LiveDead_fall2022 = `L/D 11/22`,
    Browse_fall2022 = `Browse Y/N 11/22`,
    Length_fall2022 = `Length 11/22`,
    Notes_fall2022 = `Notes 11/22`,
    
    LiveDead_summer2023 = `corrected L/D July`,
    Length_summer2023 = `corrected Length July`,
    Notes_summer2023 = `Notes July 2023`,
    LiveDead_fall2023 = `L/D October 2023`,
    Browse_fall2023 = `Browse Y/N 10/31/2023 (not collected this season)`,
    Length_fall2023 = `Length Oct 2023`,
    Notes_fall2023 = `Notes Oct 2023`,
    
    LiveDead_summer2024 = `L/D 6/11/24`,
    Browse_summer2024 = `Browse Y/N 6/11/24`,
    Length_summer2024 = `Length 6/11/24`,
    Notes_summer2024 = `Notes 6/11/24`,
    LiveDead_fall2024 = `L/D 10/31/24`,
    Browse_fall2024 = `Browse Y/N 10/31/24`,
    Length_fall2024 = `Length 10/31/24`,
    Notes_fall2024 = `Notes 10/31/24`
  ) %>%
  mutate(
    Browse_fall2020 = NA, .after = LiveDead_fall2020
  ) %>%
  mutate(
    Browse_summer2023 = NA, .after = LiveDead_summer2023
  )

# STEP 5: COMBINE SITES
# STEP 6: ADD A UNIQUE ID COLUMN

#Note: Noticed there is r oak, r. oak, and r.oak. Fixed

master <- rbind(mdi, schoodic, belfast, surry)
master <- master %>% mutate(UniqueID = 1:n(), .before = Site)
#table(master$Species)
master <- master %>%
  mutate(Species = recode(Species, "r oak" = "r.oak")) %>%
  mutate(Species = recode(Species, "r. oak" = "r.oak"))

#Note: Noticed that the Tube column is messy. Clean.
master <- master %>%
  mutate(Tube = recode(Tube, "y" = "Y")) %>%
  mutate(Tube = recode(Tube, "Y." = "Y")) %>%
  mutate(Tube = replace_na(Tube, "N"))

#Note: Numeric woes. Make sure the columns are compatible. Length should be numeric, 
#browse should be character, at least for now

#str(master)
master_wide <- master %>%
  mutate_at(c('Length_summer2019', 'Length_fall2019', 'Length_summer2020', 
              'Length_fall2020', 'Length_summer2021', 'Length_fall2021',
              'Length_summer2022', 'Length_fall2022', 'Length_summer2023', 
              'Length_fall2023', 'Length_summer2024', 'Length_fall2024'), as.numeric) %>%
  mutate_at(c('Browse_summer2020', 'Browse_fall2020', 'Browse_summer2021', 'Browse_fall2021',
              'Browse_summer2022', 'Browse_fall2022', 'Browse_summer2023', 
              'Browse_fall2023', 'Browse_summer2024', 'Browse_fall2024'), as.character)

#str(master_wide)

#Note: Deleting the not planted seedlings
#Found that they were not planted in the notes, and they never have any data
#List of unique IDs documented in tidy markdown

not_planted <- master_wide %>%
  filter(is.na(Notes_summer2019) | Notes_summer2019 == "not planted") %>%
  filter(is.na(Notes_fall2019) | Notes_fall2019 == "not planted") %>%
  filter(is.na(LiveDead_fall2019) | LiveDead_fall2019 == "not planted")

#exported the not planted, probably not necessary
#write_xlsx(not_planted, 'C:\\Users\\jattanasio\\Desktop\\R_related\\not_planted.xlsx')

master_wide <- master_wide %>%
  filter(is.na(Notes_summer2019) | Notes_summer2019 != "not planted") %>%
  filter(is.na(Notes_fall2019) | Notes_fall2019 != "not planted") %>%
  filter(is.na(LiveDead_fall2019) | LiveDead_fall2019 != "not planted") %>%
# Added a LiveDead column for summer2019 so I can graph the different starting numbers of spp
  mutate(LiveDead_summer2019 = "L", .before = Length_fall2019)
# remember that the filter will delete the NAs, so manually kept them in

#This is really where I start to differ from tidy markdown
#Cleaning everything up now instead of later, so far tube is clean

# STEP 7: CLEAN LIVE DEAD
#made it long to clean the columns better
cleanLiveDead <- master_wide %>%
  select(UniqueID, starts_with("Live")) %>%
  pivot_longer(
    cols = starts_with("Live"),
    names_to = "Visit",
    values_to = "LiveDead") %>%
  mutate(LiveDead = recode(LiveDead, "d" = "D", "D?" = "D", "l" = "L", "L*" = "L",
                         "L." = "L", ".L" = "L", "dnf" = "D", "Dnf" = "D", 
                         "DNF" = "D", "NF" = "D", "not found" = "D", "Y" = "L",
                         "R" = "D", "F" = "D", "c" = "D", "-" = "D", "ad" = "D")) %>%
  mutate(LiveDead = replace_na(LiveDead, "D"))

#I can't just recode the rows with the N value because one is live and one is dead
#determined Live or Dead based on length data, documented in tidy markdown
cleanLiveDead$LiveDead[cleanLiveDead$UniqueID == 246] <- "D"  #MDI
cleanLiveDead$LiveDead[cleanLiveDead$UniqueID == 1016] <- "L" #Belfast
# made it wide again to reattach it to master wide
cleanLiveDead <- cleanLiveDead %>%
  pivot_wider(names_from = Visit, values_from = LiveDead)
# got rid of the not cleaned LiveDead columns and then reattached the clean LiveDead based on UniqueID
master_wide <- master_wide %>%
  select(!starts_with("Live")) %>%
  left_join(cleanLiveDead, by = "UniqueID")

# STEP 8: CLEAN BROWSE
# actually might be fine, the NAs seem to be when the plant is dead. Can double check this

# STEP 9: CLEAN LENGTH
#There are 200 seedlings where in the fall of 2019 they were dead but still had a length measured. 
#I would like to make it so the length of them is NA in that visit.
#Therefore, I filtered them out the seedlings with the dead lengths, replaced their length with NA, 
#and then combined them back with the original.

#grabbing the seedlings with bad fall 2019 lengths and making that column NA
dead19 <- master_wide %>%
  filter(grepl("dead total length", Notes_fall2019) | 
           grepl("dead. total length", Notes_fall2019) | 
           grepl("dead? total length", Notes_fall2019) |
           grepl("deal total length", Notes_fall2019) |
           # these two seedling had a note saying dead and were listed as D but still had a length
           UniqueID == 940 | UniqueID == 945) %>%
  mutate(Length_fall2019 = NA)

#grabbing the good seedlings and separating them out
alive19 <- master_wide %>%
  filter(!grepl("dead total length", Notes_fall2019), 
         !grepl("dead. total length", Notes_fall2019), 
         !grepl("dead? total length", Notes_fall2019),
         !grepl("deal total length", Notes_fall2019),
         UniqueID != 940, UniqueID != 945) 
#then combined
master_wide <- rbind(alive19,dead19)
#then took out the misc seedlings with bad lengths
master_wide <- master_wide %>%
  filter(UniqueID != 1382 & UniqueID != 808 & UniqueID != 11 & UniqueID != 1227)

# PLEASE NOTE I DID NOT DELETE ANY ZOMBIE SEEDLINGS

#making the data long didnt work because characters and numbers
# Y = 1, N = 0  for browse and L = 1, D = 0 for LiveDead, getting rid of notes column

live_numeric <- master_wide %>%
  #pulling out LiveDead and making 1,0
  select(UniqueID, starts_with("Live")) %>%
  pivot_longer(
    cols = starts_with("Live"),
    names_to = "Visit",
    values_to = "LiveDead") %>%
  mutate(LiveDead = recode(LiveDead, "L" = 1, "D" = 0)) %>%
  #going back wide
  pivot_wider(names_from = Visit, values_from = LiveDead)

#combining again
master_wide <- master_wide %>%
  select(!starts_with("Live")) %>%
  left_join(live_numeric, by = "UniqueID")

browse_numeric <- master_wide %>%
  #pulling out browse and making 1,0
  select(UniqueID, starts_with("Browse")) %>%
  pivot_longer(
    cols = starts_with("Browse"),
    names_to = "Visit",
    values_to = "Browse") %>%
  mutate(Browse = recode(Browse, "Y" = 1, "N" = 0)) %>%
  #going back wide
  pivot_wider(names_from = Visit, values_from = Browse)

#combining again
master_wide <- master_wide %>%
  select(!starts_with("Browse")) %>%
  left_join(browse_numeric, by = "UniqueID")

data_long <- master_wide %>%
  select(!starts_with("Notes")) %>%
  pivot_longer(
   cols = starts_with(c("Length", "Browse", "Live")),
   names_to = "Visit",
   values_to = "Data")

write_xlsx(data_long, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\data_long.xlsx')

#here is where I am pulling out the data needed for the manuscript
data <- data_long %>%
  pivot_wider(names_from = Visit, values_from = Data) %>%
  select(UniqueID, Site, Plot, Species, Tube, LiveDead_fall2024, Length_summer2019,
         Length_fall2024) %>%
  mutate(Growth = Length_fall2024 - Length_summer2019) %>%
  select(-c(Length_summer2019, Length_fall2024)) %>%
  mutate(Region = case_when(
    Species == "tulip" | Species == "s.gum" ~ "southern",
    Species == "r.oak" | Species == "w.spruce" | Species == "w.pine" ~ "local",
    Species == "ch.oak" | Species == "r.cedar" | Species == "w.oak" ~ "Maine")
    , .before = Tube)

#plotting just to make sure it looks reasonable
ggplot(data, aes(x = Species, y = Growth)) +
  geom_boxplot() +
  facet_wrap(~Site) +
  geom_hline(yintercept = 0) +
  labs(title = "Growth in 2024", x = "Species", y = "Growth (cm)") +
  theme(axis.text.x = element_text(angle = 90))

#I think I need to go through the previous markdown files from winter 2025 to...
#make sure I got all the seedlings that should be altered. Can do this when I deal with zombie seedlings?
write_xlsx(data, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\data.xlsx')

# Continuing to alter the data files into what I need
# added measure (what type of data, sample.period (season), year)

longdat <- read_excel("data/data_long.xlsx") %>% 
  rename(sapling.id = UniqueID, planting.date = PlantingDate) %>% 
  rename_with(tolower)

longdat2 <- longdat %>%
  pivot_wider(names_from = visit, values_from = data) %>%
# getting the growth in the fall since the summer of 2019. So total growth from the beginning at the end of the growing season.
# Unsure if I should also be doing this for the spring
  mutate(TotalGrowth_fall2019 = Length_fall2019 - Length_summer2019) %>%
  mutate(TotalGrowth_fall2020 = Length_fall2020 - Length_summer2019) %>%
  mutate(TotalGrowth_fall2021 = Length_fall2021 - Length_summer2019) %>%
  mutate(TotalGrowth_fall2022 = Length_fall2022 - Length_summer2019) %>%
  mutate(TotalGrowth_fall2023 = Length_fall2023 - Length_summer2019) %>%
  mutate(TotalGrowth_fall2024 = Length_fall2024 - Length_summer2019) %>%
  pivot_longer(
    cols = starts_with(c("Length", "Browse", "Live", "Total")),
    names_to = "visit",
    values_to = "data") %>%
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
  pivot_wider(names_from = measure, values_from = data) %>% 
  mutate(region = case_when(
    species == "tulip" | species == "s.gum" ~ "southern",
    species == "r.oak" | species == "w.spruce" | species == "w.pine" ~ "local",
    species == "ch.oak" | species == "r.cedar" | species == "w.oak" ~ "maine"), .before = tube)

# Now I am looking for zombie seedlings aka the seedlings that were at one point marked dead and then later alive
# Important to remember that Belfast was not visited in the fall of 2020, so excluding Belfast 

zombie <- longdat %>%
  pivot_wider(names_from = visit, values_from = data) %>%
  select(sapling.id, site, species, starts_with("LiveDead"))

zombie1 <- zombie %>%
  filter(LiveDead_fall2019 == 0 & LiveDead_summer2020 == 1)

write_xlsx(zombie1, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie1.xlsx')

zombie2 <- zombie %>%
  filter(LiveDead_summer2020 == 0 & LiveDead_fall2020 == 1) %>%
  filter(site != "Belfast")

write_xlsx(zombie2, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie2.xlsx')

zombie3 <- zombie %>%
  filter(LiveDead_fall2020 == 0 & LiveDead_summer2021 == 1) %>%
  filter(site != "Belfast")

write_xlsx(zombie3, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie3.xlsx')

zombie4 <- zombie %>%
  filter(LiveDead_summer2021 == 0 & LiveDead_fall2021 == 1) 

write_xlsx(zombie4, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie4.xlsx')

zombie5 <- zombie %>%
  filter(LiveDead_fall2021 == 0 & LiveDead_summer2022 == 1) 

write_xlsx(zombie5, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie5.xlsx')

zombie6 <- zombie %>%
  filter(LiveDead_summer2022 == 0 & LiveDead_fall2022 == 1)
write_xlsx(zombie6, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie6.xlsx')

zombie7 <- zombie %>%
  filter(LiveDead_fall2022 == 0 & LiveDead_summer2023 == 1)

write_xlsx(zombie7, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie7.xlsx')

zombie8 <- zombie %>%
  filter(LiveDead_summer2023 == 0 & LiveDead_fall2023 == 1)

write_xlsx(zombie8, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie8.xlsx')

zombie9 <- zombie %>%
  filter(LiveDead_fall2023 == 0 & LiveDead_summer2024 == 1)

write_xlsx(zombie9, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie9.xlsx')

zombie10 <- zombie %>%
  filter(LiveDead_summer2024 == 0 & LiveDead_fall2024 == 1)

write_xlsx(zombie10, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie10.xlsx')
# 462 zombie seedlings. We may want to exclude these going forward

# Making a new column called unreliable
# Also I see there are a lot of NAs in the browse column, I think I should make those 0? 

clean <- longdat2 %>%
  mutate(unreliable = 0)

# There are 462 seedlings that are unreliable, I couple copy paste in their sapling.id but I will double check

#t <- clean %>% 
#  select(-c(year, browse, livedead)) %>% 
# pivot_wider(names_from = sample.period, values_from = length) %>% 
# mutate(growth = fall2024 - summer2019)