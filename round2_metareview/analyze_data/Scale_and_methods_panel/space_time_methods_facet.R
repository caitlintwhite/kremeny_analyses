library(tidyverse)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

num_papers = dat %>% 
  pull(Title) %>%
  unique() %>%
  length()

methods_by_title = dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(Title, clean_answer)


dat %>% 
  filter(abbr=='Nested') %>%
  dplyr::select(Title, 'Nested' = clean_answer) %>%
  full_join(
    dat %>% 
      filter(abbr=='TimeTrends') %>%
      mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
      mutate(clean_answer = ifelse(clean_answer =='Space for time', 'Yes', clean_answer)) %>%
      dplyr::select(Title, 'TimeTrends' = clean_answer),
    by = 'Title'
  ) %>% 
  # removing NAs, not sure why some (11) studies don't have TimeTrends?
  filter(!is.na(TimeTrends)) %>%
  left_join(methods_by_title, by = 'Title') %>% 
  group_by(Nested, TimeTrends, clean_answer) %>%
  summarise(count = n()) %>%
  mutate(space_time = case_when(
    Nested=='No' & TimeTrends =='No' ~ 'Neither',
    Nested=='Yes' & TimeTrends =='No' ~ 'Space, not time',
    Nested=='No' & TimeTrends =='Yes' ~ 'Time, not space',
    Nested=='Yes' & TimeTrends =='Yes' ~ 'Space and time'
  )) %>%
  ggplot(aes(x = clean_answer, y = count)) +
  geom_col() +
  facet_wrap(~space_time) +
  theme_bw()





dat %>% 
  filter(abbr=='Nested') %>%
  dplyr::select(Title, 'Nested' = clean_answer) %>%
  full_join(
    dat %>% 
      filter(abbr=='TimeTrends') %>%
      mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
      mutate(clean_answer = ifelse(clean_answer =='Space for time', 'Yes', clean_answer)) %>%
      dplyr::select(Title, 'TimeTrends' = clean_answer),
    by = 'Title'
  ) %>% 
  # removing NAs, not sure why some (11) studies don't have TimeTrends?
  filter(!is.na(TimeTrends)) %>%
  left_join(methods_by_title, by = 'Title') %>%
  separate_rows(clean_answer, sep = ',') %>%
  group_by(Nested, TimeTrends, clean_answer) %>%
  summarise(count = n()) %>%
  mutate(space_time = case_when(
    Nested=='No' & TimeTrends =='No' ~ 'Neither',
    Nested=='Yes' & TimeTrends =='No' ~ 'Space, not time',
    Nested=='No' & TimeTrends =='Yes' ~ 'Time, not space',
    Nested=='Yes' & TimeTrends =='Yes' ~ 'Space and time'
  )) %>%
  ggplot(aes(x = clean_answer, y = count)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~space_time) +
  theme_bw()
