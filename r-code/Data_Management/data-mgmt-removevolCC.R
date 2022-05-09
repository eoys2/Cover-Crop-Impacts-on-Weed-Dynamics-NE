
# Created by: Emily Robinson
# Cleaned, combined, and exported data for analyses (9/27/2021)
# SHOULD NOT NEED TO RUN AGAIN

# ------------------------------------------------------------------------------
# LOAD LIBRARIES ---------------------------------------------------------------
# ------------------------------------------------------------------------------

library(readxl)    # read in .xlsx files
library(readr)     # read in and output .csv files
library(tidyverse) # data cleaning, manipulation, joining, plotting, etc. (basically a powerhouse package!)
library(here)      # makes file paths for input and output cleaner

# ------------------------------------------------------------------------------
# FIELD DATA -------------------------------------------------------------------
# ------------------------------------------------------------------------------

# IMPORT FIELD DATA
field.early.data <- read_excel(path = here("data", "Field_data", "EarlySeasonWeeds_Master.xlsx"), sheet = "Sheet1") %>%
  mutate(Season = "Early")
field.late.data <- read_excel(path = here("data", "Field_data", "LateSeasonWeeds_Master.xlsx"), sheet = "Sheet1") %>%
  mutate(Season = "Late")

# JOIN FIELD DATA
field.data <- rbind(field.early.data, field.late.data) %>%
  separate(Sample_Location, into = c(NA, "Sample"), sep = "_") %>%
  select(Site, Treatment, Rep, Sample, Sample_Date, Season, Current_crop, Previous_crop, Crop_Stage, Biomass_Pigweed, Biomass_Grass, Biomass_Broadleaf, Biomass_Total, 
         Weed_Density, Pigweed_Density, Grass_Density, Broadleaf_Density, Moisture, Temperature) %>%
  arrange(Site, Treatment, Rep, Season, Sample)

# MAKE SURE COLUMNS ARE FORMATTED CORRECTLY (e.g. Factor, Numeric, etc.)
factorCols <- c("Site", "Treatment", "Rep", "Sample", "Sample_Date", "Season", "Current_crop", "Previous_crop", "Crop_Stage")
field.data[,factorCols] <- lapply(field.data[,factorCols], factor)
summary(field.data)

# EXPORT COMBINED FIELD DATA
write.csv(field.data, file = here("data", "field-data-originalEO.csv"), row.names = F, na = "")

# AVERAGE OVER SAMPLES
avgfield.data <- field.data %>%
  group_by(Site, Treatment, Rep, Sample_Date, Season, Current_crop, Previous_crop, Crop_Stage) %>%
  summarise(avgBiomass_Pigweed     = mean(Biomass_Pigweed),
            avgBiomass_Grass       = mean(Biomass_Grass),
            avgBiomass_Broadleaf   = mean(Biomass_Broadleaf),
            avgBiomass_Total       = mean(Biomass_Total),
            sumWeed_Density        = sum(Weed_Density),
            sumPigweed_Density     = sum(Pigweed_Density),
            sumGrass_Density       = sum(Grass_Density),
            sumBroadleaf_Density   = sum(Broadleaf_Density),
            avgMoisture            = mean(Moisture),
            avgTemperature         = mean(Temperature)) %>%
  ungroup()

# MAKE SURE COLUMNS ARE FORMATTED CORRECTLY (e.g. Factor, Numeric, etc.)
factorCols <- c("Site", "Treatment", "Rep", "Sample_Date", "Season", "Current_crop", "Previous_crop", "Crop_Stage")
avgfield.data[,factorCols] <- lapply(avgfield.data[,factorCols], factor)
summary(avgfield.data)

# EXPORT COMBINED FIELD DATA
write.csv(avgfield.data, file = here("data", "field-data-aggregateEO.csv"), row.names = F, na = "")

# ------------------------------------------------------------------------------
# SEEDBANK DATA ----------------------------------------------------------------
# ------------------------------------------------------------------------------

# IMPORT SEEDBANK DATA
seedbank.colfax.data <- read_excel(path = here("data", "GH_Seedbank_Data", "Colfax_GH_Results_MASTER.xlsx"), sheet = "Sheet1") %>%
  mutate(Site = "Colfax")
seedbank.enrec.data <- read_excel(path = here("data", "GH_Seedbank_Data", "ENREC_GH_Results_MASTER.xlsx"), sheet = "RESULTS") %>%
  mutate(Site = "ENREC")
seedbank.greeley.data <- read_excel(path = here("data", "GH_Seedbank_Data", "Greeley_GH_Results_MASTER.xlsx"), sheet = "Results") %>%
  mutate(Site = "Greeley")
seedbank.howard.data <- read_excel(path = here("data", "GH_Seedbank_Data", "Howard_GH_Results_MASTER.xlsx"), sheet = "RESULTS") %>%
  mutate(Site = "Howard")
seedbank.merrick.data <- read_excel(path = here("data", "GH_Seedbank_Data", "Merrick_GH_Results_MASTER.xlsx"), sheet = "RESULTS") %>%
  mutate(Site = "Merrick")
seedbank.scal.data <- read_excel(path = here("data", "GH_Seedbank_Data", "SCAL_GH_Results_MASTER.xlsx"), sheet = "RESULTS") %>%
  mutate(Site = "SCAL")

# COMBINE SEEDBANK DATA
seedbank.data <- seedbank.colfax.data
seedbank.data <- full_join(seedbank.data, seedbank.enrec.data,   by = intersect(names(seedbank.data), names(seedbank.enrec.data)))
seedbank.data <- full_join(seedbank.data, seedbank.greeley.data, by = intersect(names(seedbank.data), names(seedbank.greeley.data)))
seedbank.data <- full_join(seedbank.data, seedbank.howard.data,  by = intersect(names(seedbank.data), names(seedbank.howard.data)))
seedbank.data <- full_join(seedbank.data, seedbank.merrick.data, by = intersect(names(seedbank.data), names(seedbank.merrick.data)))
seedbank.data <- full_join(seedbank.data, seedbank.scal.data,    by = intersect(names(seedbank.data), names(seedbank.scal.data)))

speciesList <- names(seedbank.data)[c(4:9, 17:63)]
seedbank.data <- seedbank.data %>% 
  rename(Rep = REP,
         Treatment = TREATMENT) %>%
  mutate(Treatment = ifelse(Treatment %in% c("RYE"), "Rye",
                            ifelse(Treatment %in% c("VETCH"), "Vetch",
                                   ifelse(Treatment %in% c("COVER", "Cover"), "Cover", 
                                          ifelse(Treatment %in% c("CHECK", "Check"), "Check", Treatment))))) %>%
  select(c(Site, Treatment, Rep, Sample_ID, PLOT_ID, TOTAL_WEEDS, TOTAL_SPECIES, TOTAL_PIGWEEDS, TOTAL_GRASSES, TOTAL_OTHERBROAD, speciesList))

# MAKE SURE COLUMNS ARE FORMATTED CORRECTLY (e.g. Factor, Numeric, etc.)
factorCols <- c("Site", "Treatment", "Rep", "Sample_ID", "PLOT_ID")
seedbank.data[,factorCols] <- lapply(seedbank.data[,factorCols], factor)
numericCols <- names(seedbank.data)[c(6:63)]
seedbank.data[,numericCols] <- lapply(seedbank.data[,numericCols], as.numeric)
summary(seedbank.data)

# EXPORT COMBINED SEEDBANK DATA
#write.csv(seedbank.data, file = here("data", "seedbank-data-original.csv"), row.names = F, na = "")

# SUM OVER THE 2 BUCKETS
sumseedbank.data <- seedbank.data %>%
  select(-Sample_ID, -PLOT_ID, -TOTAL_SPECIES) %>%
  pivot_longer(cols = names(seedbank.data)[c(6, 8:63)],
               names_to = "Species",
               values_to = "Seed_Count") %>%
  arrange(Site, Treatment, Rep, Species) %>%
  group_by(Site, Treatment, Rep, Species) %>%
  summarise(sumSeed_Count = sum(Seed_Count, na.rm = T)) %>%
  ungroup() %>%
  pivot_wider(id_cols = c("Site", "Treatment", "Rep"),
              names_from = "Species",
              values_from = "sumSeed_Count") %>%
  select(Site, Treatment, Rep, names(seedbank.data)[c(6, 8:63)]) %>%
  mutate(TOTAL_SPECIES = rowSums(.[,c(8:60)] > 0)) %>%
  select(Site, Treatment, Rep, TOTAL_SPECIES, names(seedbank.data)[c(6, 8:63)])

# EXPORT COMBINED SEEDBANK DATA
# write.csv(sumseedbank.data, file = here("data", "seedbank-data-sum.csv"), row.names = F, na = "")

# ------------------------------------------------------------------------------
# COMBIND FIELD AND SEEDBANK DATA ----------------------------------------------
# ------------------------------------------------------------------------------

joinCols <- c("Site", "Treatment", "Rep")
combined.data <- full_join(avgfield.data, sumseedbank.data, by = joinCols) %>%
  rename(totSeeds_Pigweed = TOTAL_PIGWEEDS,
         totSeeds_Grasses = TOTAL_GRASSES,
         totSeeds_Otherbroad = TOTAL_OTHERBROAD,
         totSeeds_Weeds = TOTAL_WEEDS,
         Total_Species = TOTAL_SPECIES
  ) %>%
  select(Site, Treatment, Rep, Sample_Date, Season, Current_crop, Previous_crop, Crop_Stage, avgBiomass_Total, sumWeed_Density, totSeeds_Weeds, Total_Species,
         avgBiomass_Pigweed, sumPigweed_Density, totSeeds_Pigweed, 
         avgBiomass_Grass, sumGrass_Density, totSeeds_Grasses, 
         avgBiomass_Broadleaf, sumBroadleaf_Density, 
         totSeeds_Otherbroad,
         avgMoisture, avgTemperature) %>%
  mutate(FieldType = ifelse(Site %in% c("Colfax", "Greeley", "Howard", "Merrick"), "On Farm", "Research Station")) %>%
  mutate(FieldType = factor(FieldType))

with(avgfield.data[avgfield.data$Season == "Early",], table(Site, Treatment))
with(avgfield.data[avgfield.data$Season == "Late",], table(Site, Treatment))
with(sumseedbank.data, table(Site, Treatment))

write.csv(combined.data, file = here("data", "combined-data-EO.csv"), row.names = F, na = "")

