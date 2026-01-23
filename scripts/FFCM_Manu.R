# Loading in packages ----
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

# GRAB DATA, CLEAN SITES, COMBINE SITES ----
# STEP 1: GRAB THE DATA
belfast <- read_excel("C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\FFCM_Data_Belfast_2024.xlsx")
mdi <- read_excel("C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\FFCM_Data_MDI_2024.xlsx")
schoodic <- read_excel("C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\FFCM_Data_SchoodicNorth_2024.xlsx")
surry <- read_excel("C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\FFCM_Data_SurryForest_2024.xlsx")

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
master <- rbind(mdi, schoodic, belfast, surry)



# ADD UNIQUEID, CLEAN SPECIES, TUBE, BROWSE ----

# STEP 6: ADD A UNIQUE ID COLUMN

master <- master %>% mutate(UniqueID = 1:n(), .before = Site)

# are there any duplicates?
doubles <- master %>%
  count(UniqueID) %>%
  filter(n > 1)
#table(master$Species)
#Note: Noticed there is r oak, r. oak, and r.oak. Fixed
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

doubles <- master_wide %>%
  count(UniqueID) %>%
  filter(n > 1)

# DELETE THE NOT PLANTED SEEDLINGS ----
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

doubles <- master_wide %>%
  count(UniqueID) %>%
  filter(n > 1)

#This is really where I start to differ from tidy markdown
#Cleaning everything up now instead of later, so far tube is clean

# CLEAN LIVEDEAD ----

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

doubles <- master_wide %>%
  count(UniqueID) %>%
  filter(n > 1)

# CLEAN LENGTH ----

# STEP 8: CLEAN LENGTH
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

doubles <- master_wide %>%
  count(UniqueID) %>%
  filter(n > 1)

# PLEASE NOTE I DID NOT DELETE ANY ZOMBIE SEEDLINGS

# MAKING DATA LONG, COLUMNS NUMERIC ----
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

doubles <- master_wide %>%
  count(UniqueID) %>%
  filter(n > 1)

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
doubles <- master_wide %>%
  count(UniqueID) %>%
  filter(n > 1)

# FINALIZE MASTER_WIDE; MISC SEEDLINGS, BAD TUBES, ZOMBIES ----

# renaming columns in master_wide so it matches later data
master_wide <- master_wide %>%
  rename(sapling.id = UniqueID) %>%
  rename(site = Site) %>%
  rename(plot = Plot) %>%
  rename(cell = Cell) %>%
  rename(species = Species) %>%
  rename(planting.date = PlantingDate) %>%
  rename(tube = Tube)
# altering seedlings - already done: deleted not planted, fixed LD, deleted bad lengths
# deleting questionable seedlings
master_wide <- master_wide %>%
  filter(sapling.id != 69 &  sapling.id != 165 & sapling.id != 934 & sapling.id != 575 &
         sapling.id !=900 & sapling.id != 933 & sapling.id != 1067) 
# the seedlings were marked as not having tubes, when in reality they do have tubes
# looking at the seedlings with tube notes
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
           grepl("tube fell off", Notes) | grepl("tube remove", Notes))
# below are the tubes incorrectly coded as not having tubes, need to make that a Y tube
N.bad.tubes <- bad.tubes %>%
  filter(tube == "N")
# grabbing these sapling.id
N.bad.tubes2 <- N.bad.tubes %>%
  select(sapling.id) %>%
  distinct(sapling.id, .keep_all = TRUE)

N.bad.tubes3 <- left_join(N.bad.tubes2, master_wide, by = "sapling.id") %>%
  mutate(tube = "Y")
# deleting these from master_wide, so I can combine them back with corrected tube column
master_wide <- anti_join(master_wide, N.bad.tubes2, by = "sapling.id")

doubles <- master_wide %>%
  count(sapling.id) %>%
  filter(n > 1)

#combining them back together again
master_wide <- rbind(master_wide, N.bad.tubes3)

doubles <- master_wide %>%
  count(sapling.id) %>%
  filter(n > 1)

# completed fixing the tube Y or N
# So tubes are not finalized by this point. All we did was fix the incorrect columns
# STILL NEED TO ALTER THE TUBES THAT WENT DOWN WHILE THE SEEDLING WAS STILL ALIVE
# WILL DO THAT DOWN IN GROWTH BECAUSE THAT IS WHAT IS MOSTLY IMPACTED

# Zombie pt 1: getting rid of the zombies with 2 or more resurrections
# first we must make master_wide long and pull out only the LiveDead columns

zombie <- master_wide %>%
select(sapling.id, site, species, starts_with("LiveDead"))

# next step is to see when they died and came back to life
zombie1 <- zombie %>%
  filter(LiveDead_fall2019 == 0 & LiveDead_summer2020 == 1) %>%
  select(sapling.id)


write_xlsx(zombie1, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie1.xlsx')

# please note fall2020 Belfast was not sampled, so I am excluding it from these files
zombie2 <- zombie %>%
  filter(LiveDead_summer2020 == 0 & LiveDead_fall2020 == 1) %>%
  filter(site != "Belfast") %>%
  select(sapling.id)

write_xlsx(zombie2, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\zombie2.xlsx')

# please note fall2020 Belfast was not sampled, so I am excluding it from these files
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

# Combining all the zombie files into one long file
totalzombie <-rbind(zombie1, zombie2, zombie3, zombie4, zombie5, zombie6, zombie7, zombie8, zombie9, zombie10)

# so totalzombie tells us the seedlings that died and came back to life aka resurrections
# seedlings can come up multiple times if there are multiple resurrections
duplicate <- totalzombie %>%
  count(sapling.id) %>%
  filter(n > 1) %>%
# we decided to get rid of the seedlings that resurrected 2 or more times
  select(sapling.id)
# 63 seedlings that we are getting rid of 
# LOOK INTO THESE SEEDLINGS, SEE IF IT INTRODUCES BIAS IN THE DATA
master_wide <- anti_join(master_wide, duplicate, by = "sapling.id")

doubles <- master_wide %>%
  count(sapling.id) %>%
  filter(n > 1)

# Zombie pt 2: get rid of the zombies that were dead 3 or more visits

# need to differentiate between consecutive zeros vs true death
# Function to check each row
check_row <- function(row_vals) {
  # Convert to logical: TRUE if zero
  is_zero <- row_vals == 0
  
  # Find lengths of consecutive runs
  rle_zero <- rle(is_zero)
  
  # Any run of 3+ zeros?
  has_long_run <- any(rle_zero$values & rle_zero$lengths >= 3)
  if (!has_long_run) return(FALSE)
  
  # Check if the ONLY long run is at the end
  if (rle_zero$values[length(rle_zero$values)] && 
      rle_zero$lengths[length(rle_zero$lengths)] >= 3) {
    # If there is another long run earlier, still TRUE
    earlier_long <- any(
      head(rle_zero$values, -1) & head(rle_zero$lengths, -1) >= 3
    )
    return(earlier_long)
  }
  
  return(TRUE)
}

# Need to have Belfast separate because wasn't sampled in fall 2020
consec_zombie <- master_wide %>%
  select(sapling.id, site, species, starts_with("LiveDead")) %>%
  filter(site != "Belfast")

result <- apply(consec_zombie, 1, check_row)
consec_zombie$result <- result

#pulling out the sapling.id of zombies with 3 or more visits dead
consec_zombie <- consec_zombie %>%
  filter(result == "TRUE") %>%
  select(sapling.id)
# now looking at Belfast

Belfast_consec_zombie <- master_wide %>%
  select(sapling.id, site, species, starts_with("LiveDead")) %>%
  filter(site == "Belfast") %>%
  select(!LiveDead_fall2020)

result2 <- apply(Belfast_consec_zombie, 1, check_row)
Belfast_consec_zombie$result <- result2

#pulling out the sapling.id of zombies with 3 or more visits dead
Belfast_consec_zombie <- Belfast_consec_zombie %>%
  filter(result == "TRUE") %>%
  select(sapling.id)
  
  
# deleting these from master_wide
master_wide <- anti_join(master_wide, consec_zombie, by = "sapling.id")

doubles <- master_wide %>%
  count(sapling.id) %>%
  filter(n > 1)

master_wide <- anti_join(master_wide, Belfast_consec_zombie, by = "sapling.id")

doubles <- master_wide %>%
  count(sapling.id) %>%
  filter(n > 1)
# finalized master wide maybe?
# looking at the deleted zombies and what spp/site they are 
deleted.zombies <- rbind(duplicate, consec_zombie, Belfast_consec_zombie) 
# pulled from zombie and not master_wide because it had already been deleted from master_wide
deleted.zombies <- left_join(deleted.zombies, zombie, by = "sapling.id") %>%
  group_by(species, site) %>%
  summarise(n = n())
ggplot(deleted.zombies, aes(x = species)) +
  geom_bar()

# master_wide is complete!
write_xlsx(master_wide, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\master_wide.xlsx')


# FINALIZED LONG DATA HERE, HAVE MASTER_WIDE CLEAN BY THIS POINT ----
master_long <- master_wide %>%
  select(!starts_with("Notes")) %>%
  pivot_longer(
   cols = starts_with(c("Length", "Browse", "Live")),
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
  ) 

write_xlsx(master_long, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\master_long.xlsx')
  
# EDITING TUBE AGAIN ----

master_wide <- read_excel("data/master_wide.xlsx")
master_long <- read_excel("data/master_long.xlsx")

tube.long <- master_long %>%
  filter(grepl("LiveDead", visit))

# bad.tubes again, but this time edited for the growth step later
# also note I am using master_wide because I deleted Notes from master_long so the would all be numeric
growth.bad.tubes <- master_wide %>%
  select(!starts_with(c("Length", "Browse", "Live"))) %>%
  pivot_longer(
    cols = starts_with("Notes"),
    names_to = "visit",
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
  group_by(sapling.id) %>%
  summarize(first.down.tube = min(sample.period)) %>%
  # I renamed this to sample.period so it would match the other data frame
  # However, as a reminder, it is the first visit the tube went down
  rename(sample.period = first.down.tube) 
# AT THIS POINT growth.bad.tubes IS JUST WHEN THE TUBE WAS REMOVED; MIGHT HAVE BEEN WHEN SEEDLING WAS DEAD
# need to pull out when the seedling was alive and the tube went down

tubes.dead.down <- left_join(growth.bad.tubes, tube.long, by = c("sapling.id", "sample.period"))
doubles.tube <- tubes.dead.down %>%
  count(sapling.id) %>%
  filter(n > 1)
# OK so I found the firs time the tube went bad for each seedling and whether it was alive or dead
# there are 163 tubes with notes
tubes.down.seedling.alive <- tubes.dead.down %>%
  filter(data == 1) %>%
  select("sapling.id", "sample.period")

doubles.tube <- tubes.down.seedling.alive %>%
  count(sapling.id) %>%
  filter(n > 1)
# SO these are the true bad tubes! These are the seedlings we need to crop their growth
# So only 79 bad tubes? 
# format so it can be used to crop tube.long

test <- tube.long %>%
  left_join(tubes.down.seedling.alive, by = "sapling.id") %>%
  filter(is.na(sample.period.y) | sample.period.x <= sample.period.y) %>%
  select(-sample.period.y) %>%
  rename(sample.period = sample.period.x)

# need to add back in the rest of the data (esp length) so we can use it for growth
# struggling a bit... going to export test to see what I can do.

write_xlsx(test, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\test.xlsx')

# test: a df that has livedead info for all tubed and non tubed seedlings;
# still contains zombie death and true death; only cropped alive seedlings where the tube fell
# useful because it fixes bad tubes. 

# so literally all I need to do is add length back to test?


# OK I need to picture what the final dataset is going to look like in my mind
# needs livedead2024, total.growth, max.growth, years.grown

# I can create a df with what i do have, then refocus on growth (need a win)

finalformat <- master_wide %>%
  select(sapling.id, site, plot, species, tube, LiveDead_fall2024) %>%
  mutate(site.plot = paste(site, plot, sep = "_")) %>%
  relocate(site.plot, .after = sapling.id) %>%
  select(!c(site, plot)) %>%
  mutate(region = case_when(
    species == "tulip" | species == "s.gum" ~ "southern",
    species == "r.oak" | species == "w.spruce" | species == "w.pine" ~ "local",
    species == "ch.oak" | species == "r.cedar" | species == "w.oak" ~ "maine"), .before = tube)

doubles <- finalformat %>%
  count(sapling.id) %>%
  filter(n > 1)
# ok now I need total.growth, max.growth, years.grown
# the most important part of test is the sample period ans sapling id; at this point I just need length

test2 <- test %>%
  select(sapling.id, sample.period)

cropped.length <- master_long %>%
  filter(measure == "length")

draft <- left_join(test2, cropped.length, by = c("sapling.id", "sample.period"))

# so that cropped the length data it seems, now get rid of the NAs because we care if it is a living lengt

alive.draft <- draft %>%
  filter(complete.cases(.))

final.length <- alive.draft %>%
  group_by(sapling.id) %>%
  filter(sample.period == max(sample.period, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(final_length = data) %>%
  select(!c(visit, measure))

doubles <- final.length %>%
  count(sapling.id) %>%
  filter(n > 1)

initial.length <- alive.draft %>%
  filter(sample.period == 0) %>%
  rename(initial_length = data) %>%
  select(c(sapling.id, initial_length))

doubles <- initial.length %>%
  count(sapling.id) %>%
  filter(n > 1)

growth <- left_join(final.length, initial.length, by = "sapling.id")  

doubles.growth <- growth %>%
  count(sapling.id) %>%
  filter(n > 1)

# growth2 has one less observation than growth. makes me nervous...

total.growth <- growth %>%
  mutate(total_growth = final_length - initial_length) %>%
  select(sapling.id, sample.period, total_growth)

# have total growth! until the last time it was alive!
finalformat <- left_join(finalformat, total.growth, by = "sapling.id")

doubles <- finalformat %>%
  count(sapling.id) %>%
  filter(n > 1)
# trying this another way

# I have realized the final.format livedead is wrong? or not? because we cropped the ones with tubes but they are alive after that

max.height <- alive.draft %>%
  group_by(sapling.id) %>%
  filter(data == max(data, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(max_length = data) %>%
  select(!c(visit, measure)) %>%
  group_by(sapling.id) %>%
  filter(sample.period == min(sample.period, na.rm = TRUE))

# OK I have duplicates here because sometimes the max height was the same multiple years. Just need to grab it

max.growth <- left_join(max.height, initial.length, by = "sapling.id")  

# I got that warning about many-to-many relationships
doubles <- max.growth %>%
  count(sapling.id) %>%
  filter(n > 1)
# uh oh



total.max.growth <- max.growth %>%
  mutate(max_growth = max_length - initial_length) %>%
  select(sapling.id, max_growth)

doubles <- total.max.growth %>%
  count(sapling.id) %>%
  filter(n > 1)


# adding the growth from the intitial length to the maximum length
finalformat <- left_join(finalformat, total.max.growth, by = "sapling.id") 

doubles <- finalformat %>%
  count(sapling.id) %>%
  filter(n > 1)

# cleaning columns

finalformat <- finalformat %>%
  rename(alive2024 = LiveDead_fall2024, total.growth = total_growth, 
         max.growth = max_growth, years.grown = sample.period)

# are there duplicates, is that what is happening?
# test has duplicates because it is long

# maybe tubes.down.seedlings.alive has duplicates?

write_xlsx(finalformat, 'C:\\Users\\jattanasio\\OneDrive - DOI\\Desktop\\R_related\\FFCM\\future_forests_analysis_2025\\data\\finalformat.xlsx')


















