library(tidyverse)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

num_papers = dat %>% 
  pull(Title) %>%
  unique() %>%
  length()

dat %>% 
  filter(abbr == 'Ecosystem') %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers) %>%
  ggplot(aes(x = fct_reorder(clean_answer, proportion), y = proportion)) +
  geom_col() +
  xlab('Study system') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw()

ggsave('round2_metareview/analyze_data/General_patterns_panel/fig_files/study_system.pdf', width = 5, height = 5, dpi = 'retina')



# To-do's:
  # QA/QC terrestrial and overlapping systems (agricultural, urban, etc)
  # how to show the intersections, if we want to (e.g., how many studies worked in freshwater and terrestrial?)








