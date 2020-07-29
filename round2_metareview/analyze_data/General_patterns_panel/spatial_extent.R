library(tidyverse)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

num_papers = dat %>% 
  pull(Title) %>%
  unique() %>%
  length()

dat %>% 
  filter(abbr=='Extent') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / num_papers) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('Local', 'Macro-scale','Global','Undefined/no scale'))) %>%
  ggplot(aes(x = fct_rev(clean_answer), y = proportion)) +
  geom_col() +
  xlab('Spatial extent') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw()

ggsave('round2_metareview/analyze_data/General_patterns_panel/fig_files/spatial_extent.pdf', width = 5, height = 5, dpi = 'retina')
  





