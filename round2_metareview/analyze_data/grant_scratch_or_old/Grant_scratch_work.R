### Scratch spot to work in, and deposit old figures that I can't force myself to delete ###
# Author (if you can call it that): Grant



#+
#' ### Time considered and spatial extent
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

#' Notes:
#' Local studies more likely than macro-scale to consider temporal trends.


#+
nested_df = dat %>%
  filter(abbr=='Nested') %>%
  dplyr::select(Title, Nested = clean_answer)

extent_df = dat %>%
  filter(abbr=='Extent') %>%
  dplyr::select(Title, Extent = clean_answer)

time_trends_df = dat %>%
  filter(abbr=='TimeTrends') %>%
  dplyr::select(Title, TimeTrends = clean_answer) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) # get rid of the 'e.g' text

yrs_df = dat %>%
  filter(abbr=='YrsData') %>%
  dplyr::select(Title, YrsData = clean_answer) %>%
  filter(!is.na(YrsData))



#+
#' ## ES type scale biases
#' Notes: ...
service_bins = dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES, clean_group) %>% 
  group_by(Title) %>%
  summarise(ES = paste0(unique(ES), collapse = ', ')) 

ES_groups_to_plot = service_bins %>%
  group_by(ES) %>%
  summarise(count = n()) %>%
  filter(count > 3) %>% # excludes groups of ES bins that have 3 or fewer papers associated
  pull(ES)


#' ### Number of years?
service_bins %>%
  filter(ES %in% ES_groups_to_plot) %>%
  left_join(yrs_df, by = 'Title') %>%
  group_by(ES, YrsData) %>%
  summarise(count = n()) %>%
  filter(!is.na(YrsData)) %>%
  mutate(YrsData = factor(YrsData, levels = c('1 year or less','2–5 years','6–10 years', '10+ years'))) %>%
  ggplot(aes(x = YrsData, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~ES) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#' ### Spatial extent?
service_bins %>%
  filter(ES %in% ES_groups_to_plot) %>%
  left_join(extent_df, by = 'Title') %>%
  group_by(ES, Extent) %>%
  summarise(count = n()) %>%
  mutate(Extent = factor(Extent, levels = c('Local','Macro-scale','Global', 'Undefined/no scale'))) %>%
  ggplot(aes(x = Extent, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~ES) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#+
#' ## Methods scale biases
#' Notes: This will contain all of the notes for the methods scale biases, potentially using all 4 plots below.
study_type = dat %>%
  filter(abbr=='Methods') %>%
  dplyr::select(Title, Methods = clean_answer) %>%
  mutate(Methods = gsub(" \\(.*\\)", '', Methods))


#' ### Number of years?
study_type %>%
  left_join(yrs_df, by = 'Title') %>% # this one is a right join since not all studies have the yrsdata
  group_by(Methods, YrsData) %>%
  summarise(count = n()) %>%
  filter(!is.na(YrsData)) %>%
  mutate(YrsData = factor(YrsData, levels = c('1 year or less','2–5 years','6–10 years', '10+ years'))) %>%
  ggplot(aes(x = YrsData, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Methods) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# I think this could be better addressed (potentially) via the previous plot
# with number of years, but with segments in the bars showing the methodology.
# However, it's tough to deal with the different groups then...


#' ### Spatial extent?
study_type %>%
  left_join(extent_df, by = 'Title') %>%
  group_by(Methods, Extent) %>%
  summarise(count = n()) %>%
  mutate(Extent = factor(Extent, levels = c('Local','Macro-scale','Global', 'Undefined/no scale'))) %>%
  ggplot(aes(x = Extent, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Methods) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#+
#' ## Drivers scale biases
driver_groups =  dat %>%
  filter(abbr=='Driver') %>%
  filter(!is.na(clean_answer)) %>%
  dplyr::select(Title, Group, ES) %>% #may want to consider ES as well
  group_by(Title) %>%
  summarise(Group = paste0(unique(Group), collapse = ', '),
            ES = paste0(unique(ES), collapse = ', ')) 

#' ### Number of years?
driver_groups %>%
  left_join(yrs_df, by = 'Title') %>%
  group_by(Group, YrsData) %>%
  summarise(count = n()) %>%
  filter(!is.na(YrsData)) %>%
  mutate(YrsData = factor(YrsData, levels = c('1 year or less','2–5 years','6–10 years', '10+ years'))) %>%
  ggplot(aes(x = YrsData, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Group) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#' ### Spatial extent?
driver_groups %>%
  left_join(extent_df, by = 'Title') %>%
  group_by(Group, Extent) %>%
  summarise(count = n()) %>%
  mutate(Extent = factor(Extent, levels = c('Local','Macro-scale','Global', 'Undefined/no scale'))) %>%
  ggplot(aes(x = Extent, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Group) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))






#' ---
#' title: "Preliminary graphs for results exploration"
#' output: html_document
#' ---


library(tidyverse)
knitr::opts_chunk$set(echo = FALSE)

dat = read.csv("../data/cleaned/ESqualtrics_r2keep_cleaned.csv") %>%
  filter(version=='final')


#+
#' ## Space-time tradeoffs

#+
#' ### Time trends and space
nested_timetrends = dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  left_join(
    dat %>% 
      filter(abbr %in% c('Nested')) %>% 
      dplyr::select(Title, abbr, clean_answer),
    by = 'Title'
  ) %>%
  rename(TimeTrends = clean_answer.x, Nested = clean_answer.y) %>%
  dplyr::select(-abbr.x, -abbr.y) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>%
  mutate(TimeTrends = case_when(
    TimeTrends=='Space for time' ~ 'Yes',
    TRUE ~ TimeTrends
  )) %>%
  count(Nested, TimeTrends) %>%
  spread(TimeTrends, n) %>% 
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  mutate(Nested = case_when(
    Nested == 'Yes' ~ 'Multi-scale',
    Nested == 'No' ~ 'Non-multi-scale'
  )) %>%
  rename(Group = Nested)

ggplot(nested_timetrends, aes(x = Group, y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_abline(slope = 0, intercept = sum(nested_timetrends$Yes)/sum(nested_timetrends$count_tot),
              linetype = 'dashed') +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  coord_flip() +
  ylab('Proportion of studies considering temporal trends') +
  xlab('') +
  ylim(c(0,1)) +
  theme_bw()



extent_timetrends = dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, clean_answer) %>%
  rename(TimeTrends = clean_answer) %>%
  #dplyr::select(-abbr.x, -abbr.y) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>%
  mutate(TimeTrends = case_when(
    TimeTrends=='Space for time' ~ 'Yes',
    TRUE ~ TimeTrends
  )) %>%
  left_join(
    dat %>% 
      filter(abbr %in% c('Extent')) %>% 
      dplyr::select(Title, clean_answer),
    by = 'Title'
  ) %>%
  rename(Extent = clean_answer) %>%
  count(Extent, TimeTrends) %>%
  spread(TimeTrends, n) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  rename(Group = Extent) %>%
  mutate(Group = factor(Group, levels = c('Local','Macro-scale','Global','Undefined/no scale')))

ggplot(extent_timetrends, aes(x = fct_rev(Group), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_abline(slope = 0, intercept = sum(extent_timetrends$Yes)/sum(extent_timetrends$count_tot),
              linetype = 'dashed') +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  coord_flip() +
  ylab('Proportion of studies considering temporal trends') +
  xlab('') +
  ylim(c(0,1)) +
  theme_bw()


#+
#' ### Multi-scale and time

timetrends_nested = dat %>% 
  filter(abbr %in% c('Nested')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  left_join(
    dat %>% 
      filter(abbr %in% c('TimeTrends')) %>% 
      dplyr::select(Title, abbr, clean_answer),
    by = 'Title'
  ) %>%
  rename(TimeTrends = clean_answer.y, Nested = clean_answer.x) %>%
  dplyr::select(-abbr.x, -abbr.y) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>%
  mutate(TimeTrends = case_when(
    TimeTrends=='Space for time' ~ 'Yes',
    TRUE ~ TimeTrends
  )) %>%
  count(TimeTrends, Nested) %>%
  spread(Nested, n) %>% 
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  mutate(TimeTrends = case_when(
    TimeTrends == 'Yes' ~ 'Time considered',
    TimeTrends == 'No' ~ 'Time not considered'
  )) %>%
  rename(Group = TimeTrends) %>% 
  filter(!is.na(Group)) # this line filters out the NAs, which results in slightly different overall proportions

ggplot(timetrends_nested, aes(x = Group, y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_abline(slope = 0, intercept = sum(timetrends_nested$Yes)/sum(timetrends_nested$count_tot),
              linetype = 'dashed') +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  coord_flip() +
  ylab('Proportion of studies that are multi-scale') +
  xlab('') +
  ylim(c(0,1)) +
  theme_bw()

extent_nested = dat %>% 
  filter(abbr %in% c('Nested')) %>% 
  dplyr::select(Title, clean_answer) %>%
  rename(Nested = clean_answer) %>%
  #dplyr::select(-abbr.x, -abbr.y) %>%
  left_join(
    dat %>% 
      filter(abbr %in% c('Extent')) %>% 
      dplyr::select(Title, clean_answer),
    by = 'Title'
  ) %>%
  rename(Extent = clean_answer) %>%
  count(Extent, Nested) %>%
  spread(Nested, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  rename(Group = Extent) %>%
  filter(Group != 'Undefined/no scale') %>%
  mutate(Group = factor(Group, levels = c('Local', 'Macro-scale','Global')))

ggplot(extent_nested, aes(x = fct_rev(Group), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_abline(slope = 0, intercept = sum(extent_nested$Yes)/sum(extent_nested$count_tot),
              linetype = 'dashed') +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  coord_flip() +
  ylab('Proportion of studies that are multi-scale') +
  xlab('') +
  ylim(c(0,1)) +
  theme_bw()

yrs_nested = dat %>% 
  filter(abbr %in% c('Nested')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  left_join(
    dat %>% 
      filter(abbr %in% c('YrsData')) %>% 
      dplyr::select(Title, abbr, clean_answer),
    by = 'Title'
  ) %>%
  rename(YrsData = clean_answer.y, Nested = clean_answer.x) %>%
  dplyr::select(-abbr.x, -abbr.y) %>%
  count(YrsData, Nested) %>%
  spread(Nested, n) %>% 
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  rename(Group = YrsData) %>% 
  filter(!is.na(Group)) %>%
  mutate(Group = factor(Group, levels = c('1 year or less','2–5 years','6–10 years','10+ years')))

ggplot(yrs_nested, aes(x = fct_rev(Group), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_abline(slope = 0, intercept = sum(yrs_nested$Yes)/sum(yrs_nested$count_tot),
              linetype = 'dashed') +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  coord_flip() +
  ylab('Proportion of studies that are multi-scale') +
  xlab('') +
  ylim(c(0,1)) +
  theme_bw()


#+
#' ## Methods scale biases

#' ### TimeTrends
methods_timetrends = dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  rename(TimeTrends = clean_answer) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>%
  mutate(TimeTrends = case_when(
    TimeTrends=='Space for time' ~ 'Yes',
    TRUE ~ TimeTrends
  )) %>%
  left_join(
    dat %>%
      filter(abbr=='Methods') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, clean_answer) %>%
      mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
      separate_rows(clean_answer, sep = ','),
    by = 'Title'
  ) %>% 
  rename(Methods = clean_answer) %>%
  pivot_longer(cols = c('Methods')) %>%
  filter(!is.na(value), value != 'NA') %>% # character NAs removed
  count(value, TimeTrends) %>%
  spread(TimeTrends, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) 

ggplot(methods_timetrends, aes(x = fct_reorder(value, prop_yes), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  geom_abline(slope = 0, 
              intercept = sum(methods_timetrends$Yes)/sum(methods_timetrends$count_tot),
              linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Proportion of studies that consider temporal trends') +
  xlab('Methods') +
  ylim(c(0,1))



#' ### Multi-scale

methods_nested = dat %>% 
  filter(abbr %in% c('Nested')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  rename(Nested = clean_answer) %>%
  left_join(
    dat %>%
      filter(abbr=='Methods') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, clean_answer) %>%
      mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
      separate_rows(clean_answer, sep = ','),
    by = 'Title'
  ) %>% 
  rename(Methods = clean_answer) %>%
  pivot_longer(cols = c('Methods')) %>%
  filter(!is.na(value), value != 'NA') %>% # character NAs removed
  count(value, Nested) %>%
  spread(Nested, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) 

ggplot(methods_nested, aes(x = fct_reorder(value, prop_yes), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  geom_abline(slope = 0, 
              intercept = sum(methods_nested$Yes)/sum(methods_nested$count_tot),
              linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Proportion of studies that are multi-scale') +
  xlab('Methods') +
  ylim(c(0,1))


#+
#' ## ES type scale biases

#' ### TimeTrends
es_timetrends = dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  rename(TimeTrends = clean_answer) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>%
  mutate(TimeTrends = case_when(
    TimeTrends=='Space for time' ~ 'Yes',
    TRUE ~ TimeTrends
  )) %>% 
  left_join(
    dat %>%
      filter(abbr=='Yclass') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, ES, clean_group) %>% 
      group_by(Title) %>%
      summarise(ES = paste0(unique(ES), collapse = ', ')),
    by = 'Title'
  ) %>% 
  separate_rows(ES, sep = ', ') %>% 
  pivot_longer(cols = c('ES')) %>%
  filter(!is.na(value)) %>%
  count(value, TimeTrends) %>%
  spread(TimeTrends, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) 

ggplot(es_timetrends, aes(x = fct_reorder(value, prop_yes), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  geom_abline(slope = 0, intercept = sum(es_timetrends$Yes)/sum(es_timetrends$count_tot),
              linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Proportion of studies that consider temporal trends') +
  xlab('ES type') +
  ylim(c(0,1))


#' ### Multi-scale
es_nested = dat %>% 
  filter(abbr %in% c('Nested')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  rename(Nested = clean_answer) %>%
  left_join(
    dat %>%
      filter(abbr=='Yclass') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, ES, clean_group) %>% 
      group_by(Title) %>%
      summarise(ES = paste0(unique(ES), collapse = ', ')),
    by = 'Title'
  ) %>% 
  separate_rows(ES, sep = ', ') %>% 
  pivot_longer(cols = c('ES')) %>%
  filter(!is.na(value)) %>%
  count(value, Nested) %>%
  spread(Nested, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) 

ggplot(es_nested, aes(x = fct_reorder(value, prop_yes), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  geom_abline(slope = 0, intercept = sum(es_nested$Yes)/sum(es_nested$count_tot),
              linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Proportion of studies that are multi-scale') +
  xlab('ES type') +
  ylim(c(0,1))



#+
#' ## Driver scale biases
#' ### TimeTrends
drivers_timetrends = dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  rename(TimeTrends = clean_answer) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>%
  mutate(TimeTrends = case_when(
    TimeTrends=='Space for time' ~ 'Yes',
    TRUE ~ TimeTrends
  )) %>%
  left_join(
    dat %>%
      filter(abbr=='Driver') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, clean_group) %>%
      group_by(Title) %>%
      summarise(clean_group = paste0(unique(clean_group), collapse = ',')) %>%
      separate_rows(clean_group, sep = ','),
    by = 'Title'
  ) %>% 
  rename(Driver = clean_group) %>%
  pivot_longer(cols = c('Driver')) %>%
  filter(!is.na(value), value != 'NA') %>% # character NAs removed
  count(value, TimeTrends) %>%
  spread(TimeTrends, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) 

ggplot(drivers_timetrends, aes(x = fct_reorder(value, prop_yes), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  geom_abline(slope = 0, 
              intercept = sum(drivers_timetrends$Yes)/sum(drivers_timetrends$count_tot),
              linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Proportion of studies that consider temporal trends') +
  xlab('Driver group') +
  ylim(c(0,1))



#' ### Multi-scale
drivers_nested = dat %>% 
  filter(abbr %in% c('Nested')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  rename(Nested = clean_answer) %>%
  left_join(
    dat %>%
      filter(abbr=='Driver') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, clean_group) %>%
      group_by(Title) %>%
      summarise(clean_group = paste0(unique(clean_group), collapse = ',')) %>%
      separate_rows(clean_group, sep = ','),
    by = 'Title'
  ) %>% 
  rename(Driver = clean_group) %>%
  pivot_longer(cols = c('Driver')) %>%
  filter(!is.na(value), value != 'NA') %>% # character NAs removed
  count(value, Nested) %>%
  spread(Nested, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) 

ggplot(drivers_nested, aes(x = fct_reorder(value, prop_yes), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  geom_abline(slope = 0, 
              intercept = sum(drivers_nested$Yes)/sum(drivers_nested$count_tot),
              linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Proportion of studies that are multi-scale') +
  xlab('Driver group') +
  ylim(c(0,1))


library(tidyverse)

dat  = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version == 'final')


### Time trends ###

overall_cts_props = dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(prop = count/sum(count))

nested_timetrends = dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  left_join(
    dat %>% 
      filter(abbr %in% c('Nested')) %>% 
      dplyr::select(Title, abbr, clean_answer),
    by = 'Title'
  ) %>%
  rename(TimeTrends = clean_answer.x, Nested = clean_answer.y) %>%
  dplyr::select(-abbr.x, -abbr.y) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>%
  mutate(TimeTrends = case_when(
    TimeTrends=='Space for time' ~ 'Yes',
    TRUE ~ TimeTrends
  )) %>%
  count(Nested, TimeTrends) %>%
  spread(TimeTrends, n) %>% 
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  mutate(Nested = case_when(
    Nested == 'Yes' ~ 'Multi-scale',
    Nested == 'No' ~ 'Non-multi-scale'
  )) %>%
  rename(Group = Nested)

extent_timetrends = dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, clean_answer) %>%
  rename(TimeTrends = clean_answer) %>%
  #dplyr::select(-abbr.x, -abbr.y) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>%
  mutate(TimeTrends = case_when(
    TimeTrends=='Space for time' ~ 'Yes',
    TRUE ~ TimeTrends
  )) %>%
  left_join(
    dat %>% 
      filter(abbr %in% c('Extent')) %>% 
      dplyr::select(Title, clean_answer),
    by = 'Title'
  ) %>%
  rename(Extent = clean_answer) %>%
  count(Extent, TimeTrends) %>%
  spread(TimeTrends, n) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  rename(Group = Extent)


# multi-scale and extent
rbind(extent_timetrends, nested_timetrends) %>%
  mutate(Group = factor(Group, levels = c('Multi-scale','Non-multi-scale','Local','Macro-scale','Global','Undefined/no scale'))) %>%
  mutate(Group = fct_rev(Group)) %>%
  filter(Group != 'Undefined/no scale') %>% 
  ggplot(aes(x = Group, y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_abline(slope = 0, intercept = 1 - overall_cts_props %>% 
                filter(clean_answer=='No') %>% 
                pull(prop),
              linetype = 'dashed') +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  coord_flip() +
  ylab('Proportion of studies considering temporal trends') +
  xlab('ES type') +
  ylim(c(0,1)) +
  theme_bw()



# ES type proportion yes, with labels of total counts
dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  rename(TimeTrends = clean_answer) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>%
  mutate(TimeTrends = case_when(
    TimeTrends=='Space for time' ~ 'Yes',
    TRUE ~ TimeTrends
  )) %>% 
  left_join(
    dat %>%
      filter(abbr=='Yclass') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, ES, clean_group) %>% 
      group_by(Title) %>%
      summarise(ES = paste0(unique(ES), collapse = ', ')),
    by = 'Title'
  ) %>% 
  separate_rows(ES, sep = ', ') %>% 
  pivot_longer(cols = c('ES')) %>%
  filter(!is.na(value)) %>%
  count(value, TimeTrends) %>%
  spread(TimeTrends, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  ggplot(aes(x = fct_reorder(value, prop_yes), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  geom_abline(slope = 0, intercept = 1 - overall_cts_props %>% 
                filter(clean_answer=='No') %>% 
                pull(prop),
              linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Proportion of studies that consider temporal trends') +
  xlab('ES type') +
  ylim(c(0,1))

# These are the proportions of yes's for studies that contain the ES type,
# thus studies that have multiple ES types are included in all of their
# categories.

drivers_timetrends = dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  rename(TimeTrends = clean_answer) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>%
  mutate(TimeTrends = case_when(
    TimeTrends=='Space for time' ~ 'Yes',
    TRUE ~ TimeTrends
  )) %>%
  left_join(
    dat %>%
      filter(abbr=='Driver') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, clean_group) %>%
      group_by(Title) %>%
      summarise(clean_group = paste0(unique(clean_group), collapse = ',')) %>%
      separate_rows(clean_group, sep = ','),
    by = 'Title'
  ) %>% 
  rename(Driver = clean_group) %>%
  pivot_longer(cols = c('Driver')) %>%
  filter(!is.na(value), value != 'NA') %>% # character NAs removed
  count(value, TimeTrends) %>%
  spread(TimeTrends, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) 


ggplot(drivers_timetrends, aes(x = fct_reorder(value, prop_yes), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  geom_abline(slope = 0, 
              intercept = sum(drivers_timetrends$Yes)/sum(drivers_timetrends$count_tot),
              linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Proportion of studies that consider temporal trends') +
  xlab('Driver group') +
  ylim(c(0,1))

# I think that these are all above average because the studies that have
# multiple driver groups are also more likely to consider time, thus they are
# overrepresented in these proportions. Thus, it seems like I need to find
# another benchmark to compare these values to.

methods_timetrends = dat %>% 
  filter(abbr %in% c('TimeTrends')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  rename(TimeTrends = clean_answer) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>%
  mutate(TimeTrends = case_when(
    TimeTrends=='Space for time' ~ 'Yes',
    TRUE ~ TimeTrends
  )) %>%
  left_join(
    dat %>%
      filter(abbr=='Methods') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, clean_answer) %>%
      mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
      separate_rows(clean_answer, sep = ','),
    by = 'Title'
  ) %>% 
  rename(Methods = clean_answer) %>%
  pivot_longer(cols = c('Methods')) %>%
  filter(!is.na(value), value != 'NA') %>% # character NAs removed
  count(value, TimeTrends) %>%
  spread(TimeTrends, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) 

ggplot(methods_timetrends, aes(x = fct_reorder(value, prop_yes), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  geom_abline(slope = 0, 
              intercept = sum(methods_timetrends$Yes)/sum(methods_timetrends$count_tot),
              linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Proportion of studies that consider temporal trends') +
  xlab('Methods') +
  ylim(c(0,1))







### Nested/multi-scale ###
nested_overall_cts_props = dat %>% 
  filter(abbr %in% c('Nested')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(prop = count/sum(count))

timetrends_nested = dat %>% 
  filter(abbr %in% c('Nested')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  left_join(
    dat %>% 
      filter(abbr %in% c('TimeTrends')) %>% 
      dplyr::select(Title, abbr, clean_answer),
    by = 'Title'
  ) %>%
  rename(TimeTrends = clean_answer.y, Nested = clean_answer.x) %>%
  dplyr::select(-abbr.x, -abbr.y) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>%
  mutate(TimeTrends = case_when(
    TimeTrends=='Space for time' ~ 'Yes',
    TRUE ~ TimeTrends
  )) %>%
  count(TimeTrends, Nested) %>%
  spread(Nested, n) %>% 
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  mutate(TimeTrends = case_when(
    TimeTrends == 'Yes' ~ 'Time considered',
    TimeTrends == 'No' ~ 'Time not considered'
  )) %>%
  rename(Group = TimeTrends) %>% 
  filter(!is.na(Group))

extent_nested = dat %>% 
  filter(abbr %in% c('Nested')) %>% 
  dplyr::select(Title, clean_answer) %>%
  rename(Nested = clean_answer) %>%
  #dplyr::select(-abbr.x, -abbr.y) %>%
  left_join(
    dat %>% 
      filter(abbr %in% c('Extent')) %>% 
      dplyr::select(Title, clean_answer),
    by = 'Title'
  ) %>%
  rename(Extent = clean_answer) %>%
  count(Extent, Nested) %>%
  spread(Nested, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  rename(Group = Extent)

yrs_nested = dat %>% 
  filter(abbr %in% c('Nested')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  left_join(
    dat %>% 
      filter(abbr %in% c('YrsData')) %>% 
      dplyr::select(Title, abbr, clean_answer),
    by = 'Title'
  ) %>%
  rename(YrsData = clean_answer.y, Nested = clean_answer.x) %>%
  dplyr::select(-abbr.x, -abbr.y) %>%
  count(YrsData, Nested) %>%
  spread(Nested, n) %>% 
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  rename(Group = YrsData) %>% 
  filter(!is.na(Group))


rbind(extent_nested, timetrends_nested, yrs_nested) %>%
  mutate(Group = factor(Group, levels = c('Time considered','Time not considered',
                                          '1 year or less','2–5 years','6–10 years','10+ years',
                                          'Local','Macro-scale','Global','Undefined/no scale'))) %>%
  mutate(Group = fct_rev(Group)) %>%
  filter(Group != 'Undefined/no scale') %>% 
  ggplot(aes(x = Group, y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_abline(slope = 0, intercept = 1 - overall_cts_props %>% 
                filter(clean_answer=='No') %>% 
                pull(prop),
              linetype = 'dashed') +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  coord_flip() +
  ylab('Proportion of studies that are multi-scale') +
  xlab('') +
  ylim(c(0,1)) +
  theme_bw()



dat %>% 
  filter(abbr %in% c('Nested')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  rename(Nested = clean_answer) %>%
  left_join(
    dat %>%
      filter(abbr=='Yclass') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, ES, clean_group) %>% 
      group_by(Title) %>%
      summarise(ES = paste0(unique(ES), collapse = ', ')),
    by = 'Title'
  ) %>% 
  separate_rows(ES, sep = ', ') %>% 
  pivot_longer(cols = c('ES')) %>%
  filter(!is.na(value)) %>%
  count(value, Nested) %>%
  spread(Nested, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  ggplot(aes(x = fct_reorder(value, prop_yes), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  geom_abline(slope = 0, intercept = 1 - overall_cts_props %>% 
                filter(clean_answer=='No') %>% 
                pull(prop),
              linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Proportion of studies that are multi-scale') +
  xlab('ES type') +
  ylim(c(0,1))

# These are the proportions of yes's for studies that contain the ES type,
# thus studies that have multiple ES types are included in all of their
# categories.


dat %>% 
  filter(abbr %in% c('Nested')) %>% 
  dplyr::select(Title, abbr, clean_answer) %>%
  rename(Nested = clean_answer) %>%
  left_join(
    dat %>%
      filter(abbr=='Driver') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, clean_group) %>%
      group_by(Title) %>%
      summarise(clean_group = paste0(unique(clean_group), collapse = ',')) %>%
      separate_rows(clean_group, sep = ','),
    by = 'Title'
  ) %>% 
  rename(Driver = clean_group) %>%
  pivot_longer(cols = c('Driver')) %>%
  filter(!is.na(value), value != 'NA') %>% # character NAs removed
  count(value, Nested) %>%
  spread(Nested, n) %>%
  replace(is.na(.), 0) %>%
  mutate(count_tot = No + Yes) %>%
  mutate(prop_yes = Yes/count_tot) %>%
  ggplot(aes(x = fct_reorder(value, prop_yes), y = prop_yes, label = paste0('n=',count_tot))) +
  geom_col() +
  geom_label(aes(y = 0 + 0.01), hjust = 0) +
  geom_abline(slope = 0, intercept = 1 - overall_cts_props %>% 
                filter(clean_answer=='No') %>% 
                pull(prop),
              linetype = 'dashed') +
  coord_flip() +
  theme_bw() +
  ylab('Proportion of studies that are multi-scale') +
  xlab('Driver group') +
  ylim(c(0,1))



















