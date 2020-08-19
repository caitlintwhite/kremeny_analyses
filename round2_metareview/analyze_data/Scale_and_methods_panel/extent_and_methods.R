library(tidyverse)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

num_papers = dat %>% 
  pull(Title) %>%
  unique() %>%
  length()


extent_title = dat %>% 
  filter(abbr=='Extent') %>% 
  dplyr::select(Title, Extent = clean_answer)

extent_props = extent_title %>%
  group_by(Extent) %>%
  summarise(count = n()) %>%
  mutate(ext_proportion = count / num_papers) %>%
  rename(ext_count = count)



methods_title_seprow = dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(Title, Methods = clean_answer) %>%
  separate_rows(Methods, sep = ',') 


ext_methods = methods_title_seprow %>%
  left_join(extent_title, by = 'Title') %>%
  group_by(Extent, Methods) %>%
  summarise(count = n()) %>%
  ungroup() %>% 
  mutate(Extent = as.character(Extent)) %>%
  complete(Extent, Methods, fill = list(count = 0)) %>%
  mutate(methods_ext_prop = count / num_papers) %>%
  left_join(extent_props, by = 'Extent') %>%
  group_by(Extent) %>%
  mutate(methods_ext_prop_within = count / ext_count) 
  


ggplot() +
  geom_col(data = extent_props %>% mutate(Extent = factor(Extent, levels = c('Local','Macro-scale','Global','Undefined/no scale'))),
           aes(x = fct_rev(Extent), y = ext_proportion), 
           fill = 'gray') +
  geom_col(data = ext_methods, 
           aes(x = Extent, y = methods_ext_prop, group = Methods, fill = Methods), 
           position = 'dodge') +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme_bw() 

# methods on reverse axes with proportions of each method within each scale group
ggplot() +
  geom_col(data = extent_props %>% mutate(Extent = factor(Extent, levels = c('Local','Macro-scale','Global','Undefined/no scale'))),
           aes(x = fct_rev(Extent), y = ext_proportion), 
           fill = 'gray') +
  geom_col(data = ext_methods, 
           aes(x = Extent, y = -methods_ext_prop_within/4, group = Methods, fill = Methods), 
           position = 'dodge') +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme_bw() +
  ggtitle('Methods proportions calculated within scale groups')




