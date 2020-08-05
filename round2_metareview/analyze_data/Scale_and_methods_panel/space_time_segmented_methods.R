library(tidyverse)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

num_papers = dat %>% 
  pull(Title) %>%
  unique() %>%
  length()




dat %>% 
  filter(abbr=='Nested') %>%
  dplyr::select(Title, 'Nested' = clean_answer) %>%
  #join with time trends
  full_join(
    dat %>% 
      filter(abbr=='TimeTrends') %>%
      mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
      mutate(clean_answer = ifelse(clean_answer =='Space for time', 'Yes', clean_answer)) %>%
      dplyr::select(Title, 'TimeTrends' = clean_answer),
    by = 'Title'
  ) %>%
  # join with connectivity
  full_join(
    dat %>%
      filter(abbr=='Connect') %>%
      dplyr::select(Title, Connectivity = clean_answer)
  ) %>%
  # join with methods
  full_join(
    dat %>% 
      filter(abbr == 'Methods') %>%
      mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
      dplyr::select(Title, clean_answer) %>%
      separate_rows(clean_answer, sep = ',')
  )





dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(Title, 'Methods' = clean_answer) %>%
  mutate(methods_groups = case_when(
    Methods == 'Model/Data Simulation' ~ 'Modeling only',
    Methods == 'Social survey/Interview' ~ 'Survey only',
    Methods == 'Experimental' ~ 'Experimental only',
    Methods == 'Observational' ~ 'Observational only',
    TRUE ~ 'Mixed non-observational'
  ))





