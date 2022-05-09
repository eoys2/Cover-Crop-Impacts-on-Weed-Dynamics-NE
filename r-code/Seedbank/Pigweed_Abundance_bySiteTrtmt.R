#Pigweed Abundance by Site and Treatment
## Manipulation of the seedbank dataset 


# Load Libraries ----------------------------------------------------------
library(dplyr)
library(tidyverse)


# Load Data ---------------------------------------------------------------
seedbank.data <- read.csv(file = here("data","FINAL_DATA", "updatedseedbank-data-sum.csv"))

seedbank.data %>% 
  group_by(Site, Treatment) %>% 
  summarize(across(AMATU:SIDSP, sum)) %>% 
  pivot_longer(cols = c("AMATU":"SIDSP"),
               names_to = "Species",
               values_to = "Count") %>%
  group_by(Site, Treatment) %>% 
  mutate(Total = sum(Count)) %>% 
  filter(Count > 0) %>% 
  mutate(Pct = (Count/Total)*100) %>% 
  arrange(desc(Pct), .by_group = TRUE) %>% 
  filter(str_detect(Species,"AMA")) %>% 
  filter(Treatment != "VetchDNI")-> seedz2 #Filter out enrec vetch data (we are not using)

seedz2

