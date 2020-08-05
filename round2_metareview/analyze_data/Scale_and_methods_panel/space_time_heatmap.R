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
  group_by(Nested, TimeTrends) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count)) %>%
  ggplot(aes(x = Nested, y = TimeTrends, fill = -proportion, label = paste0(100*round(proportion, digits = 2), '%'))) +
  geom_tile() +
  geom_label() +
  xlab('Multiple spatial scales?') +
  ylab('Temporal trends?') +
  ggtitle('Space-time tradeoffs') +
  theme_bw()


# To-do's
  # general plot aesthetics edits
  # should connectivity be included here, maybe as part of the space one? Where
  # a study gets a yes for either multiple spatial scales or connectivity being
  # considered?
  
  
  
  
  
  
  
  
  



