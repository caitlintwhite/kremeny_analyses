# space and time alone basic summary bar plots
# author: Grant

library(tidyverse)


dat = read.csv("round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv")


### first the basic summary plots for each separately ###

# spatial extent summary
dat %>% 
  filter(abbr=='Extent') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>% 
  mutate(clean_answer = factor(clean_answer, levels = c('Local', 'Macro-scale','Global','Undefined/no scale'))) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  xlab('Scale') +
  ylab('Number of papers')


# nested/multiple scales
dat %>%
  filter(abbr=='Nested') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  xlab('Nested/multiple scales?') +
  ylab('Number of papers')


# connectivity
dat %>%
  filter(abbr=='Connect') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  xlab('Connectivity?') +
  ylab('Number of papers')

dat %>%
  filter(abbr=='ConnectDist') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  filter(!is.na(clean_answer)) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  xlab('Connectivity distance') +
  ylab('Number of papers') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# I don't think the distance for connectivity is useful


# temporal trends
dat %>%
  filter(abbr=='TimeTrends') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  xlab('Time considered?') +
  ylab('Number of papers')


dat %>%
  filter(abbr=='YrsData') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  filter(!is.na(clean_answer)) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label()

 


# now getting to the time-space interactions somehow

# Is there a bias for local scale in the long-term studies?
# Is there a bias for not considering time in the macro and global scale studies?
# 


#TimeTrends vs Nested
dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, abbr, clean_answer, Init) %>%
  left_join(
    dat %>% 
      filter(abbr %in% c('Nested')) %>% 
      dplyr::select(Title, abbr, clean_answer),
    by = 'Title'
  ) %>%
  rename(TimeTrends = clean_answer.x, Nested = clean_answer.y) %>%
  dplyr::select(-abbr.x, -abbr.y) %>%
  #getting rid of NAs, seems like they are only in some of the double reviewed papers
  filter(!is.na(TimeTrends)) %>%
  group_by(Title) %>%
  # need to get rid of duplicate titles, this will preserve all unique answers just in case
  summarise(Nested = paste0(unique(Nested), collapse = '_'),
            TimeTrends = paste0(unique(TimeTrends), collapse = '_')) %>%
  group_by(Nested, TimeTrends) %>%
  summarise(count = n()) %>%
  #TEMPORARILY get rid of the double reviewed papers that disagreed
  filter(!grepl("_", TimeTrends)) %>%
  mutate(combo_col = paste0(Nested, ', ', TimeTrends)) %>%
  mutate(combo_col = gsub(" \\(.*\\)", '', combo_col)) %>%
  ggplot(aes(x = combo_col, y = count, label = count)) +
  geom_col() +
  geom_label() +
  xlab("Nested, TimeTrends") +
  ylab("Number of papers") +
  theme_bw()

# TimeTrends across extents

dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, clean_answer, Init) %>%
  left_join(
    dat %>% 
      filter(abbr %in% c('Extent')) %>% 
      dplyr::select(Title, clean_answer),
    by = 'Title'
  ) %>%
  rename(TimeTrends = clean_answer.x, Extent = clean_answer.y) %>%
  group_by(Title) %>%
  summarise(TimeTrends = paste0(unique(TimeTrends), collapse = "_"),
            Extent = paste0(unique(Extent), collapse = "_")) %>%
  #TEMPORARILY get rid of the double reviewed papers that disagreed
  filter(!grepl("_", TimeTrends)) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>% #shorten naming
  group_by(TimeTrends, Extent) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(TimeTrends = factor(TimeTrends, levels = c('Yes', 'Space for time', 'No')),
         Extent = factor(Extent, levels = c('Local', 'Macro-scale', 'Global','Undefined/no scale'))) %>%
  ggplot(aes(x = TimeTrends, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Extent) +
  theme_bw() +
  xlab('Does the study consider temporal trends?')
# might want to do proportions instead since macroscale has so many more papers than the other categories





# Extent across number of years studied
dat %>% 
  filter(abbr %in% c('YrsData')) %>% 
  dplyr::select(Title, clean_answer, Init) %>%
  filter(!is.na(clean_answer)) %>% #only available for studies that answered yes to time trends
  #TEMPORARY - this is the only paper that had disagreement between double reviewers
  filter(Title != 'Global evidence of positive impacts of freshwater biodiversity on fishery yields') %>% 
  left_join(
    dat %>% 
      filter(abbr %in% c('Extent')) %>% 
      dplyr::select(Title, clean_answer),
    by = 'Title'
  ) %>%
  rename(YrsData = clean_answer.x, Extent = clean_answer.y) %>%
  group_by(Title) %>%
  summarise(Extent = paste0(unique(Extent), collapse = "_"),
            YrsData = paste0(unique(YrsData), collapse = "_")) %>%
  group_by(Extent, YrsData) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(Extent = factor(Extent, levels = c('Local', 'Macro-scale', 'Global','Undefined/no scale')),
         YrsData = factor(YrsData, levels = c('1 year or less','2–5 years','6–10 years','10+ years'))) %>%
  ggplot(aes(x = YrsData, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Extent) +
  theme_bw() +
  xlab('How many years? (Given the study was Yes to TimeTrends)')

# will be interesting to see how this looks when study type is considered








# TIME TRENDS HAVE SOME DOUBLE REVIEWS WITH DIFFERENT ANSWERS
# let's see how many
doub_titles = dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, abbr, clean_answer, Init) %>%
  group_by(Title) %>%
  summarise(rev_count = n()) %>%
  filter(rev_count!=1) %>%
  pull(Title)

doub_rev_disagree_timetrends = dat %>% 
  filter(abbr %in% c('TimeTrends')) %>%
  filter(Title %in% doub_titles) %>%
  dplyr::select(Title, clean_answer, Init) %>% 
  filter(!is.na(clean_answer)) %>%
  group_by(Title) %>%
  summarise(answer = paste0(unique(clean_answer), collapse = ';'),
            Init = paste0(unique(Init), collapse = ';')) %>%
  filter(!answer %in% c('No', 'Yes (e.g., a site sampled multiple points in time, a model with dynamics)')) %>%
  separate_rows(answer, Init, sep = ";") 

# ONE disagreement for number of years
doub_rev_disagree_yrsdata = dat %>%
  filter(abbr == 'YrsData') %>%
  filter(Title %in% doub_titles) %>%
  dplyr::select(Title, clean_answer, Init) %>% 
  filter(!is.na(clean_answer)) %>% 
  group_by(Title) %>%
  summarise(answer = paste0(unique(clean_answer), collapse = ';'),
            Init = paste0(unique(Init), collapse = ';')) %>%
  filter(grepl(';', answer)) %>%
  separate_rows(answer, Init, sep = ";") 







