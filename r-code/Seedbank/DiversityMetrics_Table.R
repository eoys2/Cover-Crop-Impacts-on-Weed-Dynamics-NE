# install.packages('rmarkdown')
# install.packages('tinytex')
# install.packages('gt')
# install.packages('kableExtra')
# install.packages('formattable')
#install.packages('glue')

# Libraries ---------------------------------------------------------------

library(rmarkdown)
library(tinytex)
library(gt)
library(kableExtra)
library(formattable)
library(here)
library(tidyverse)
library(glue)



# Seedbank Community Analyses ---------------------------------------------

metrics <- read_csv(file = here("data", "Diversity_Metrics_forTable.csv"))


metric_table <-
  metrics %>% 
  mutate(across(3:5, round, 2)) %>% 
  mutate(est_se = paste0((Change), "(", (SE), ")")) %>% 
  select(-Change, -SE) %>% 
  pivot_wider(names_from = Metric, values_from = c(est_se, P_value))
metric_table  


metric_gt <-
  metric_table %>% 
  gt() %>% 
  tab_spanner(
    label = md("**Shannon Hill Diversity**"),
    columns= c(est_se_ShanHill, P_value_ShanHill)) %>% 
  tab_spanner(
    label = md("**Richness**"),
    columns = c(est_se_Richness, P_value_Richness)
  ) %>% 
  tab_spanner(
    label = md("**Evenness**"),
    columns = c(est_se_Evenness, P_value_Evenness)
  ) %>% 
  cols_label(
    est_se_ShanHill = md("Estimated Change *(SE)*"),
    P_value_ShanHill = md("*P-value*"),
    est_se_Richness = md("Estimated Change *(SE)*"),
    P_value_Richness = md("*P-value*"),
    est_se_Evenness = md("Estimated Change *(SE)*"),
    P_value_Evenness = md("*P-value*"),
    Site = " ") %>% 
  cols_align(align = "center") %>% 
  tab_header(
    title = md("**Seedbank Community Analyses**")) %>% 
  tab_options(
    table.background.color = "white"
  )

metric_gt
  

  