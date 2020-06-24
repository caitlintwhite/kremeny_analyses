library(tidyverse)
library(UpSetR)



dat = read.csv("round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv") %>%
  filter(version=='final')




upset_ecosystem_prep = dat %>% 
  filter(abbr == 'Ecosystem') %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_answer', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>% 
  dplyr::select(-`Agricultural/Agroforestry/Rural`, -`Wetland/Riparian`, -`Experimental Ecosystem`) %>%
  data.frame()

upset(upset_ecosystem_prep)
