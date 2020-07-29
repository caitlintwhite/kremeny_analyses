library(tidyverse)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

num_papers = dat %>% 
  pull(Title) %>%
  unique() %>%
  length()

# proportion of studies that looked at each service
services_overall = dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(ES) %>%
  group_by(ES) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers)

connect_df = dat %>%
  filter(abbr=='Connect') %>%
  dplyr::select(Title, Connectivity = clean_answer)

overall_yes_prop = dat %>%
  filter(abbr=='Connect') %>%
  dplyr::select(Title, Connectivity = clean_answer) %>%
  dplyr::select(Connectivity) %>%
  group_by(Connectivity) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count)) %>%
  filter(Connectivity=='Yes') %>%
  pull(proportion)


dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>%
  dplyr::select(Title, ES) %>%
  left_join(connect_df, by = 'Title') %>% 
  group_by(ES, Connectivity) %>%
  summarise(count_yesno = n()) %>%
  left_join(services_overall, by = 'ES') %>%
  filter(Connectivity == 'Yes') %>%
  rename(prop_overall = proportion, count_yes = count_yesno) %>%
  mutate(prop_yes = count_yes/num_papers) %>%
  mutate(prop_expected_yes = overall_yes_prop * prop_overall) %>%
  ggplot(aes(x = fct_reorder(ES, prop_overall))) +
  geom_col(aes(y = prop_overall), fill = 'gray') +
  geom_col(aes(y = prop_yes), fill = 'black') +
  geom_point(aes(y = prop_expected_yes), colour = 'yellow', shape = '|', size = 6) +
  xlab('Ecosystem service type') +
  ylab('Proportion of studies that looked at connectivity \n (with overall proportion in light gray)') +
  ggtitle("Spatial connectivity?") +
  coord_flip() +
  theme_bw()

ggsave('round2_metareview/analyze_data/ES_type_panel/fig_files/es_type_connectivity.pdf', width = 5, height = 5, dpi = 'retina')

# here the yellow bar indicates the proportion yes expected if the group was mirroring the number of overall studies that looked at temporal trends
# would need a good caption to explain this, but I think it's really informative
# yellow bar = # of studies that looked at connectivity / total # of studies * proportion of studies that studied that ES type

# To-do's for plot aesthetics:
  # figure out the best option for the yellow bar - dashed line? etc.
  # figure out best size for the yellow bar indicator







