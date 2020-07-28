library(tidyverse)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

num_papers = dat %>% 
  pull(Title) %>%
  unique() %>%
  length()

# proportion of studies that looked at each service
dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(ES) %>%
  group_by(ES) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers) %>%
  ggplot(aes(x = fct_reorder(ES, proportion), y = proportion, label = round(proportion, digits = 2))) +
  geom_col() +
  geom_label(hjust = 1) +
  xlab('Ecosystem service type') +
  ylab('Proportion of studies that studied this service type') +
  coord_flip() +
  theme_bw()

ggsave('round2_metareview/analyze_data/ES_type_panel/which_services.pdf', width = 5, height = 5, dpi = 'retina')

# Note: studies that studied multiple service types are counted in each bin
# proportions represent the number of papers that studied a given service type divided by the total number of unique papers




# to-do aesthetically - 
  # full ES type names
  # percent instead of proportion?
  # color by regulating, provisioning, etc? maybe even group by regulating, provisioning, etc and rank in that order?
  # label unnecessary?
  # height/width/font sizing




