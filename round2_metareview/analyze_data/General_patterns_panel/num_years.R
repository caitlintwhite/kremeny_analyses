library(tidyverse)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

num_papers = dat %>% 
  pull(Title) %>%
  unique() %>%
  length()

dat %>%
  filter(abbr=='YrsData') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(clean_answer = as.character(clean_answer)) %>%
  mutate(clean_answer = ifelse(is.na(clean_answer), 'Time not considered', clean_answer)) %>%
  mutate(proportion = count / num_papers) %>%
  mutate(clean_answer = gsub('–', '-', clean_answer)) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('Time not considered', '1 year or less','2-5 years','6-10 years', '10+ years'))) %>%
  ggplot(aes(x = fct_rev(clean_answer), y = proportion)) +
  geom_col() +
  xlab('Number of years') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw()

ggsave('round2_metareview/analyze_data/General_patterns_panel/fig_files/num_years.pdf', width = 5, height = 5, dpi = 'retina')

# To-do's:
  # fix dashes turning into ...
  # see what it looks like if we stack those that do consider time and then segment the bar?





