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
  ggplot(aes(x = Nested, y = TimeTrends, fill = proportion)) +
  geom_tile() +
  geom_label(aes(label = paste0(100*round(proportion, digits = 2), '%')), fill = 'white') +
  xlab('Multiple spatial scales?') +
  ylab('Time considered?') +
  ggtitle('Space-time tradeoffs') +
  scale_fill_continuous(trans = 'reverse') +
  labs(fill = 'Proportion of studies') +
  theme_bw()

ggsave('round2_metareview/analyze_data/Scale_and_methods_panel/fig_files/space_time_heatmap.pdf', width = 6, height = 5)


# To-do's
  # general plot aesthetics edits
  # should connectivity be included here, maybe as part of the space one? Where
  # a study gets a yes for either multiple spatial scales or connectivity being
  # considered?




## Looking at methods trends within the heatmap...
# Takeaway: Model/data sim more frequently used in 'Time considered'
# studies...slightly more than experimental, but still not as much as
# observational. Something worth mentioning but not striking.
nested_time_counts = dat %>% 
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
  group_by(Nested, TimeTrends) %>%
  summarise(nested_time_count = n()) %>%
  mutate(nested_time_prop = nested_time_count / num_papers)
  
multiscale_labs = c(Yes = 'Multiple spatial scales', No = 'Not mult. spatial scales')
time_labs = c(Yes = 'Time considered', No = 'Time not considered')
  
nested_time_methods = dat %>% 
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
      dplyr::select(Title, Connectivity = clean_answer),
    by = 'Title'
  ) %>%
  # join with methods
  full_join(
    dat %>% 
      filter(abbr == 'Methods') %>%
      mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
      dplyr::select(Title, Methods = clean_answer) %>%
      separate_rows(Methods, sep = ','),
    by = 'Title'
  ) %>% 
  group_by(Nested, TimeTrends, Methods) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportion = count / num_papers) %>% 
  left_join(nested_time_counts, by = c('Nested','TimeTrends')) %>%
  mutate(proportion_within_nestedtime = count / nested_time_count) 


ggplot(nested_time_methods, aes(x = Methods, y = proportion)) +
  geom_col() +
  #geom_label(aes(label = round(proportion, digits = 2))) +
  facet_grid(Nested ~ TimeTrends, labeller = labeller(Nested = multiscale_labs, TimeTrends = time_labs)) +
  xlab('Methods used') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw()
ggsave('round2_metareview/analyze_data/Scale_and_methods_panel/fig_files/nested_time_methods.pdf', width = 5, height = 5)


#doesn't make a real difference to look at proportions within the group, so don't do this
ggplot(nested_time_methods, aes(x = Methods, y = proportion_within_nestedtime)) +
  geom_col() +
  facet_grid(Nested ~ TimeTrends, labeller = labeller(Nested = multiscale_labs, TimeTrends = time_labs)) +
  xlab('Methods used') +
  ylab('Proportion of studies (within group)') +
  coord_flip() +
  theme_bw()


# this is a figure that would be useful to have in the supplement but one that we can describe in the main text - takeaway: 

  

  
  
  



