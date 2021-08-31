## Script for binning and examining specific types of biotic drivers.

library(tidyverse)
library(ggVennDiagram) #devtools::install_github("gaospecial/ggVennDiagram")

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

kremen_topic_q13 = dat %>%
  filter(qnum=='Q13') %>% 
  select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep=',') 

krementopics_1_2 = kremen_topic_q13 %>%
  filter(clean_answer %in% c('Kremen Topic 1 : ESPs', 'Kremen Topic 2 : Community structure influencing function')) %>%
  mutate(clean_answer = str_trim(gsub(".*:",'', clean_answer), side = 'left')) %>%
  group_by(Title) %>%
  summarise(kremen_topics1_2 = paste0(unique(clean_answer), collapse =';'))

answers_binned = dat %>% 
  filter(qnum=='Q14', abbr=='ESP_type') %>%
  select(Title, clean_answer) %>%
  # clean up and separate answers
  mutate(clean_answer = gsub("\\[.*?\\]", '', clean_answer)) %>% 
  separate_rows(clean_answer, sep=',') %>%
  mutate(clean_answer = str_trim(clean_answer, side='right')) %>%
  # bin driver categories
  mutate(binned = case_when(
    clean_answer %in% c('Within species') ~'Within species',
    clean_answer %in% c('Single species') ~'Single species',
    clean_answer %in% c('Among species','Across species', 'Multiple ESP species') ~'Multiple species',
    clean_answer %in% c('Only land cover or habitat type as proxy') ~'Land cover proxy'
  ))
  

binned_biotic_driv = answers_binned %>%
  left_join(krementopics_1_2, by='Title')

binned_biotic_driv %>%
  group_by(binned) %>%
  summarise(count = n())
  # I checked, all 74 NAs are unanswered at the earliest stage, not a coding mishap

binned_biotic_driv %>%
  group_by(binned) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count)) %>%
  mutate(binned = factor(binned, levels = c('Within species','Single species','Multiple species','Land cover proxy', NA))) %>%
  filter(!is.na(binned)) %>%
  ggplot() +
  geom_col(aes(x=fct_rev(binned), y=perc)) +
  xlab('Type of biotic driver') +
  ylab('Proportion of papers') +
  coord_flip() +
  theme_bw()


  # Not sure I fully trust that this many studies actually looked at multiple
  # species in a truly combined way. But I can check with the kremen topics
  # question to see if it makes sense or not too (kind of already did this but
  # not rigorously). I expected to see more single species studies, but maybe
  # that was a faulty expectation. Either way this will be worthwhile to show on
  # the google doc and discuss.










