library(tidyverse)
library(UpSetR)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')


dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'ES', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>%
  mutate(num_ES_types = rowSums(.[-1])) %>% # gets the number of ES types for each paper (as long as Title is first column)
  filter(num_ES_types > 1) %>% 
  data.frame() %>% 
  upset(nsets = ncol(.)-1, nintersects = NA)

# this is far too messy to include, but it may go in the supplement to show that, if a study is looking at multifuncitonality, climate reg is most often considered
# however, this also reflects largely, though not perfectly, the overall distribution of service types, so not super important
# below is a cleaner version of the figure for the main text

num_1plus_papers = 12 # this is the number of intersections that contain more than one paper, counted from above plot

dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'ES', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>%
  mutate(num_ES_types = rowSums(.[-1])) %>% # gets the number of ES types for each paper (as long as Title is first column)
  filter(num_ES_types > 1) %>% 
  data.frame() %>% 
  upset(nsets = ncol(.)-1, 
        nintersects = num_1plus_papers, 
        order.by = 'freq',
        mainbar.y.label = 'Number of papers'
        )



dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>%
  group_by(Title) %>%
  summarise(count = n()) %>%
  mutate(multifun = case_when(
    count > 1 ~ 'Yes',
    count <= 1 ~ 'No'
  )) %>%
  group_by(count, multifun) %>%
  summarise(num_papers = n()) %>%
  ungroup() %>%
  mutate(proportion = num_papers/sum(num_papers)) %>% 
  ggplot(aes(x = multifun, y = proportion, group = count, fill = count)) +
  geom_col(position = 'dodge') +
  xlab('Multifunctional?') +
  ylab('Proportion of papers') +
  coord_flip() +
  labs(fill = 'Number of ES types considered') +
  theme_bw()


dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>%
  group_by(Title) %>%
  summarise(count = n()) %>%
  mutate(multifun = case_when(
    count > 1 ~ 'Yes',
    count <= 1 ~ 'No'
  )) %>%
  group_by(count, multifun) %>%
  summarise(num_papers = n()) %>%
  ungroup() %>%
  mutate(proportion = num_papers/sum(num_papers)) %>% 
  ggplot(aes(x = fct_rev(multifun), y = proportion, fill = count)) +
  geom_col() +
  xlab('Multifunctional?') +
  ylab('Proportion of papers') +
  coord_flip() +
  labs(fill = 'Number of ES types considered') +
  theme_bw()




