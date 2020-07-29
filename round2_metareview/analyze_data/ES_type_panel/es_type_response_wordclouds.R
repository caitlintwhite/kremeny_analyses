library(tidyverse)
library(ggwordcloud)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

dat %>%
  filter(abbr=='Response') %>% 
  filter(!is.na(clean_answer_finer)) %>% 
  dplyr::select(Title, clean_answer, clean_answer_finer, ES) %>% 
  # TEMPORARY - replace TEST_VALUE with the value in clean_answer
  mutate(clean_answer = as.character(clean_answer), clean_answer_finer = as.character(clean_answer_finer)) %>%
  mutate(clean_answer_finer = ifelse(clean_answer_finer=="TEST_VALUE", clean_answer, clean_answer_finer)) %>%
  # END TEMPORARY
  group_by(ES, clean_answer_finer) %>% 
  summarise(count = n()) %>%
  ggplot(aes(label = clean_answer_finer, size = count)) +
  geom_text_wordcloud() +
  facet_wrap(~ES) +
  theme_bw()

ggsave('round2_metareview/analyze_data/ES_type_panel/fig_files/response_wordclouds_TEMP.png', width = 7, height = 7, dpi = 'retina')
  
  
# what is TEST_VALUE and will it be changed?
# should I be using clean_answer instead to see what we actually said before binning?
  # depends on the purpose of the word clouds

# THESE WORDCLOUDS ARE TEMPORARY, data need to be updated before these can be finalized












