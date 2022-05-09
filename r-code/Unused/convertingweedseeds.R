#changing weed seedbank dataset to units of weeds/m2

library(tidyverse)
library(here)

weeds <- read.csv(file = here("data", "updatedseedbank-data-sum.csv"))

farmweeds <- weeds %>% 
  filter(Site %in% c("Colfax", "Howard", "Merrick", "Greeley")) %>% 
  select(-c(1:3))

justfourcols <- weeds %>% 
  filter(Site %in% c("Colfax", "Howard", "Merrick", "Greeley")) %>% 
  select(c(1:3))

farmweedsconv <- farmweeds %>% 
  mutate(farmweeds *1 / (((pi * 3.625^2) * 20 ) / 10000 ))
  
#now write it as a csv and copy paste cells??

library("writexl")
write_xlsx(farmweedsconv,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\OnFarm_ConvertedSeeds.xlsx")

write_xlsx(justfourcols,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\ToBeMerged.xlsx")
  
  
  
farmweeds

farmweedsperunit <- function(totseeds){
  
  tom2conv <- 1 / (((pi * 3.625^2) * 20 ) / 10000 )
  
  totseedsm2 <- totseeds * tom2conv
  
  return(totseedsm2)
}


#on farm
258* (1 / (((pi * 3.625^2) * 20 ) / 10000 ))

#RS
36.5* (1 / (((pi * 1.5875^2) * 20 ) / 10000 ))





# Research Stations -------------------------------------------------------

library(tidyverse)
library(here)

weeds2 <- read.csv(file = here("data", "updatedseedbank-data-sum.csv"))


rsweeds <- weeds2 %>% 
  filter(Site %in% c("ENREC", "SCAL")) %>% 
  select(-c(1:3))


justfourcols2 <- weeds2 %>% 
  filter(Site %in% c("ENREC", "SCAL")) %>% 
  select(c(1:3))


rsweedsconv <- rsweeds %>% 
  mutate(rsweeds *1 / (((pi * 1.5875^2) * 20 ) / 10000 ))

#now write it as a csv and copy paste cells??

library("writexl")
write_xlsx(rsweedsconv,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\ResearchStation_ConvertedSeeds.xlsx")

write_xlsx(justfourcols2,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\ResearchStation_Columns.xlsx")






# try again again, on farm only. USE THESE ONLY -------------------------------------------

weeds <- read.csv(file = here("data", "updated-combined-dataEO.csv"))

farmweeds <- weeds %>% 
  filter(Site %in% c("Colfax", "Howard", "Merrick", "Greeley")) %>% 
  mutate(convtotSeeds_Weeds = (totSeeds_Weeds*1 / (((pi * 3.625^2) * 20 ) / 10000 ))) %>% 
  mutate(convtotSeeds_Pigweed = (totSeeds_Pigweed*1 / (((pi * 3.625^2) * 20 ) / 10000 ))) %>% 
  mutate(convtotSeeds_Grasses = (totSeeds_Grasses*1 / (((pi * 3.625^2) * 20 ) / 10000 ))) %>% 
  mutate(convtotSeeds_Otherbroad = (totSeeds_Otherbroad*1 / (((pi * 3.625^2) * 20 ) / 10000 )))


rsweeds <- weeds %>% 
  filter(Site %in% c("ENREC","SCAL")) %>% 
  mutate(convtotSeeds_Weeds = (totSeeds_Weeds*1 / (((pi * 1.5875^2) * 20 ) / 10000 ))) %>% 
  mutate(convtotSeeds_Pigweed = (totSeeds_Pigweed*1 / (((pi * 1.5875^2) * 20 ) / 10000 ))) %>% 
  mutate(convtotSeeds_Grasses = (totSeeds_Grasses*1 / (((pi * 1.5875^2) * 20 ) / 10000 ))) %>% 
  mutate(convtotSeeds_Otherbroad = (totSeeds_Otherbroad*1 / (((pi * 1.5875^2) * 20 ) / 10000 )))


library("writexl")
write_xlsx(farmweeds,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\convupdated-combined-dataEO.xlsx")

write_xlsx(rsweeds,"C:\\Users\\Owner\\OneDrive - University of Nebraska-Lincoln\\Graduate School\\Weed Seedbank Project\\Stat Consulting\\Fall2021_Consulting\\data\\convupdated-rs-temp.xlsx")






justfourcols <- weeds %>% 
  filter(Site %in% c("Colfax", "Howard", "Merrick", "Greeley")) %>% 
  select(c(1:3))

farmweedsconv <- farmweeds %>% 
  mutate(farmweeds *1 / (((pi * 3.625^2) * 20 ) / 10000 ))

