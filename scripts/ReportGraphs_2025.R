# This is to make the graphs for the report

# needed packages
install.packages("RColorBrewer")
library(RColorBrewer)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
# load data needed
master_wide <- read_excel("C:\\Users\\jattanasio\\Desktop\\R_related\\master_wide.xlsx")
master_length <- read_excel("C:\\Users\\jattanasio\\Desktop\\R_related\\master_length.xlsx")

### NOTES ABOUT GRAPHS

## You can do the gradient in mort by making the labels column a factor and telling it the order

# You'll notice that the top and right lines of the boxes at least in "survive" and 
## "tube" are thinner than the other two lines of the box. You can adjust the 
### linewidth in theme() so that it's the same all the way around.


# In general let's keep the legend labels and axis labels  written out, 

# SURVIVE
# linewidth in theme() so that it's the same

# MORT
# Order the legend by the color; gradient - NOT GOING WELL...

# GROW
# zero line dotted or dashes

# TUBE
# Y should go first
# zero line dotted or dashes
# Make a graph with outliers and without outliers
# linewidth in theme() so that it's the same

# BROWSE
# Y should go first
# Full box around each facet

## mort
LiveDead_long <- master_wide %>%
  # Adding a LiveDead column for summer2019 so I can graph the different starting numbers of spp
  mutate(LiveDead_summer2019 = "L", .before = Length_fall2019) %>%
  select(starts_with("Live"), Region, UniqueID, Site, Species) %>%
  filter(Site != "Belfast") %>%
  pivot_longer(
    cols = starts_with("Live"),
    names_to = "Visit",
    values_to = "LiveDead")

LiveDead_long <- LiveDead_long %>%
  mutate(LiveDead = recode(LiveDead, "d" = "D", "D?" = "D", "l" = "L", "L*" = "L",
                           "L." = "L", ".L" = "L", "dnf" = "D", "Dnf" = "D", 
                           "DNF" = "D", "NF" = "D", "not found" = "D", "Y" = "L",
                           "R" = "D", "F" = "D", "c" = "D", "-" = "D", "ad" = "D")) %>%
  mutate(LiveDead = replace_na(LiveDead, "D"))
# I can't just recode the rows with the N value because on is live and one is dead
LiveDead_long$LiveDead[LiveDead_long$UniqueID == 246] <- "D"  #MDI

# repeating this but for Belfast
LiveDead_long_Belfast <- master_wide %>%
  mutate(LiveDead_summer2019 = "L", .before = Length_fall2019) %>%
  select(starts_with("Live"), Region, UniqueID, Site, Species) %>%
  filter(Site == "Belfast") %>%
  mutate_at(vars(-c(LiveDead_summer2020, LiveDead_fall2020)), ~replace(., is.na(.), "D")) %>%
  pivot_longer(
    cols = starts_with("Live"),
    names_to = "Visit",
    values_to = "LiveDead") 

LiveDead_long_Belfast <- LiveDead_long_Belfast %>%
  mutate(LiveDead = recode(LiveDead, "d" = "D", "D?" = "D", "l" = "L", "L*" = "L",
                           "L." = "L", ".L" = "L", "dnf" = "D", "Dnf" = "D", 
                           "DNF" = "D", "NF" = "D", "not found" = "D", "Y" = "L",
                           "R" = "D", "F" = "D", "c" = "D", "-" = "D", "ad" = "D")) 
# I can't just recode the rows with the N value because on is live and one is dead
LiveDead_long_Belfast$LiveDead[LiveDead_long_Belfast$UniqueID == 1016] <- "L" #Belfast

mortality <- rbind(LiveDead_long, LiveDead_long_Belfast)
# x-axis wasn't graphing in correct order, so I fixed it

mortality <- mortality %>%
  mutate(LiveDead = recode(LiveDead, "L" = 1, "D" = 0))

mortality_site <- mortality %>%
  mutate_at('LiveDead', as.numeric) %>%
  group_by(Species, Visit, Site) %>%
  summarize(sum = sum(LiveDead))

brewer.pal(n = 11, name = 'RdYlBu')
"#A50026" "#D73027" "#F46D43" "#FDAE61" "#FEE090" "#FFFFBF" "#E0F3F8" "#ABD9E9" "#74ADD1" "#4575B4" "#313695"
brewer.pal(n = 11, name = 'RdBu')
"#67001F" "#B2182B" "#D6604D" "#F4A582" "#FDDBC7" "#F7F7F7" "#D1E5F0" "#92C5DE" "#4393C3" "#2166AC" "#053061"


mortality_site$Species <- factor(mortality_site$Species, levels = c('s.gum', 'tulip','r.cedar', 'ch.oak', 'w.oak', 'r.oak',
                                                                    'w.pine', 'w.spruce'))

mort <- ggplot(mortality_site, aes(x = factor(Visit, level = c('LiveDead_summer2019', 'LiveDead_fall2019',
                                                         'LiveDead_summer2020', 'LiveDead_fall2020',
                                                         'LiveDead_summer2021', 'LiveDead_fall2021',
                                                         'LiveDead_summer2022', 'LiveDead_fall2022',
                                                         'LiveDead_summer2023', 'LiveDead_fall2023',
                                                         'LiveDead_summer2024', 'LiveDead_fall2024')), 
                                   y = sum, color = Species, group = Species)) +
  geom_point() + 
  geom_line(size = 0.65) +
  scale_x_discrete(guide = guide_axis(angle = 90), labels = c('2019', '', 
                                                              '2020', '', 
                                                              '2021', '', 
                                                              '2022', '', 
                                                              '2023','', 
                                                              '2024', '')) +

  scale_color_manual(values=c("#FDAE61","#F46D43","#D73027","#A50026","#67001F",
                              "#74ADD1","#4575B4","#313695"),
                     labels = c('Sweet gum', 'Tulip tree','Red cedar', 
                                'Chestnut oak', 'White oak', 'Red oak',
                                'White pine', 'White spruce')) +
  facet_wrap(~Site) +
  labs(x = "Visit", y = "Number of Living Seedlings") +
  theme_classic() +
  theme(strip.background =element_rect(fill="lightgray"),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) 

mort

ggsave("mort.png", width = 7, height = 7, dpi = 1200)
# Mortality through time of 8 species of trees at 4 sites.
# Southern species are warm colors, and northern species are cool colors.

#___________________________________________________________________________________

## survive

# I did this calculation not in r, should probably figure it out in r
Species <- c("Sweet gum", "Tulip tree", "Red cedar", "Chestnut oak", "White oak", 
             "Red oak", "White pine", "White spruce")
Survival <- c(30.6, 25.5, 26.1, 61.5, 46.8, 28.6, 57.6, 51.0)
# Creating a dataframe
perc_survival <- data.frame(Species, Survival)
str(perc_survival)

survive <- ggplot(perc_survival,aes(x = reorder(Species, Survival), y = Survival)) +
  geom_col(fill = "#347C98") +
  labs(y = "Survival (%)", x = "Species") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
        aspect.ratio = 1) +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 101))
survive

ggsave("survive.png", width = 7, height = 7, dpi = 1200)

#_______________________________________________________________________________
## growth
# I remember we wanted to do the growth of sweet gum, red oak, and chestnut oak
length_wide <- master_length %>%
  dplyr::select(starts_with("Length"), Region, UniqueID, Site, Species) %>%
  # The seedlings below have bad lengths, so I am just getting rid of them
  filter(UniqueID != 1382 & UniqueID != 808 & UniqueID != 11 &
           UniqueID != 1227) %>%
  mutate(growth2019 = Length_fall2019 - Length_summer2019,
         growth2020 = Length_fall2020 - Length_summer2020,
         growth2021 = Length_fall2021 - Length_summer2021,
         growth2022 = Length_fall2022 - Length_summer2022,
         growth2023 = Length_fall2023 - Length_summer2023, 
         growth2024 = Length_fall2024 - Length_summer2024)


box_species <- length_wide %>%
  filter(Species == "s.gum" | Species == "r.oak" | Species == "ch.oak") 

box_species_long <- box_species %>%
  dplyr::select(starts_with("growth"), Region, UniqueID, Site, Species) %>%
  pivot_longer(
    cols = starts_with("grow"),
    names_to = "Year",
    values_to = "Growth") %>%
  mutate(Year = recode(Year, "growth2019" = "2019",
                       "growth2020" = "2020",
                       "growth2021" = "2021",
                       "growth2022" = "2022",
                       "growth2023" = "2023",
                       "growth2024" = "2024")) 
# Not what we wanted, but interesting  
ggplot(box_species_long, aes(x = factor(Species, level = c("s.gum", "ch.oak", "r.oak")), y = Growth)) +
  geom_boxplot() +
  facet_grid(Site~Year)

grow <- ggplot(box_species, aes(x = factor(Species, level = c("s.gum", "ch.oak", "r.oak")), y = growth2024)) +
  geom_boxplot() +
  facet_wrap(~Site) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  scale_y_continuous(limits =  c(-15,30)) +
  labs(x = "Species", y = "Growth (cm)") +
  theme_classic() +
  theme(strip.background =element_rect(fill="lightgray"),
        panel.border = element_rect(color = "black", fill = NA, size = 1))  +
  scale_x_discrete(guide = guide_axis(angle = 90), labels = c("Sweet gum", 
                                                              "Chestnut oak",
                                                              "Red oak"))

grow

ggsave("grow.png", width = 7, height = 7, dpi = 1200)

# grow w/ outliers
growOut <- ggplot(box_species, aes(x = factor(Species, level = c("s.gum", "ch.oak", "r.oak")), y = growth2024)) +
  geom_boxplot() +
  facet_wrap(~Site) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
#  scale_y_continuous(limits =  c(-15,30)) +
  labs(x = "Species", y = "Growth (cm)") +
  theme_classic() +
  theme(strip.background =element_rect(fill="lightgray"),
        panel.border = element_rect(color = "black", fill = NA, size = 1))  +
  scale_x_discrete(guide = guide_axis(angle = 90), labels = c("Sweet gum", 
                                                              "Chestnut oak",
                                                              "Red oak"))
ggsave("growOut.pdf", width = 7, height = 7, dpi = 1200)

growOut
# Browse graph
browse24 <- master_wide %>%
  select(Browse_fall2024, LiveDead_fall2024, Region, UniqueID, Site, Plot, Species)  %>%
  filter(LiveDead_fall2024 == "L") %>%
  mutate(Browse_fall2024 = replace_na(Browse_fall2024, "N"))

# MAking Yes first, I don't know how to do it in ggplot itself
browse24$Browse_fall2024 <- factor(browse24$Browse_fall2024, levels = c('Y','N'))

browse <- ggplot(browse24, aes(x = Species, fill = Browse_fall2024)) +
  geom_bar() +
  scale_fill_manual(values = c("#66B032", "#B25600")) +
  facet_wrap(~Site) +
  labs(x = "Species", y = "Number of Seedlings", fill = "Browse") +
  theme_classic() +
  theme(strip.background =element_rect(fill="lightgray"),
        panel.border = element_rect(color = "black", fill = NA, size = 1)) +
  scale_x_discrete(guide = guide_axis(angle = 90),
                   labels = c('Chestnut oak', 'Red cedar', 'Red oak', 'Sweet gum', 
                              'Tulip tree', 'White oak', 'White pine', 'White spruce')) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 42))
browse

ggsave("browse.png", width = 7, height = 7, dpi = 1200)
# Tubes growth 2019
growth19 <- master_length %>%
  dplyr::select(Length_summer2019, Length_fall2019, 
                UniqueID, Site, Plot, Species, Tube) %>%
  mutate(growth2019 = Length_fall2019 - Length_summer2019) %>%
  # Taking out the seedlings with faulty tubes 
  filter(UniqueID != 250 & UniqueID != 288 & UniqueID != 682 & UniqueID != 455 &
           UniqueID != 1017 & UniqueID != 1050 & UniqueID != 1055 & UniqueID != 500 &
           UniqueID != 597) 

growth19$Tube <- factor(growth19$Tube, levels = c('Y','N'))
# This is excluding outliers
tube <- ggplot(growth19, aes(x = Tube, y = growth2019)) +
  geom_boxplot(outlier.shape = NA) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  scale_y_continuous(limits = c(-20, 30)) +
  labs(x = "Tube Y/N", y = "Growth (cm)") +
  theme_classic() +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),legend.box.background = element_rect(colour = "black")) 
tube
ggsave("tube.png", width = 7, height = 7, dpi = 1200)

