# Scale in conjunction with other variables
# Author: Grant

library(tidyverse)


dat = read.csv("round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv")

# scale data frames
nested_df = dat %>%
  filter(abbr=='Nested') %>%
  dplyr::select(Title, Nested = clean_answer)

extent_df = dat %>%
  filter(abbr=='Extent') %>%
  dplyr::select(Title, Extent = clean_answer)

time_trends_df = dat %>%
  filter(abbr=='TimeTrends') %>%
  dplyr::select(Title, TimeTrends = clean_answer) %>%
  group_by(Title) %>%
  summarise(TimeTrends = paste0(unique(TimeTrends), collapse = '_')) %>% # deal with double reviews
  filter(!grepl('_', TimeTrends)) %>% # TEMPORARY - remove studies with double review disagreements
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) # get rid of the 'e.g' text

yrs_df = dat %>%
  filter(abbr=='YrsData') %>%
  dplyr::select(Title, YrsData = clean_answer) %>%
  filter(!is.na(YrsData)) %>% # excludes the No's in timetrends
  group_by(Title) %>%
  summarise(YrsData = paste0(unique(YrsData), collapse = '_')) %>% # deal with double reviews
  filter(!grepl('_', YrsData)) # TEMPORARY - remove studies with double review disagreements


### Scales broken down by study type (experimental, sim, observational, etc)
study_type = dat %>%
  filter(abbr=='Methods') %>%
  dplyr::select(Title, Methods = clean_answer) %>%
  mutate(Methods = gsub(" \\(.*\\)", '', Methods)) %>% #clean up the stuff in parentheses
  group_by(Title) %>%
  summarise(Methods = paste0(unique(Methods), collapse = '_')) %>% # deal with double reviews
  filter(!grepl('_', Methods)) # TEMPORARY - remove studies with double review disagreements
  
  
# multi-scale?
study_type %>%
  left_join(nested_df, by = 'Title') %>%
  group_by(Methods, Nested) %>%
  summarise(count = n()) %>%
  # need to deal with order of the different options - not sure why they can go in different orders
  ggplot(aes(x = Nested, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Methods) +
  theme_bw()


# time considered?
study_type %>%
  left_join(time_trends_df, by = 'Title') %>%
  group_by(Methods, TimeTrends) %>%
  summarise(count = n()) %>%
  filter(TimeTrends != 'Space for time', !is.na(TimeTrends)) %>% #exclude space for time and NAs for cleanliness (total 11 studies excluded)
  ggplot(aes(x = TimeTrends, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Methods) +
  theme_bw()

  # model/data simulation studies are more likely than other methods to consider time (expected and reasonable)
  # but more than 1/3 (roughly) of exp and observational studies consider time still


# spatial extent?
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

  # experimental studies biased towards local scale when compared with model/sim and observational

# num years?
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



### Scales broken down by service bins
# multi-scale?
# time considered?
# spatial extent?
# num years?


### Scales broken down by type of driver
# multi-scale?
# time considered?
# spatial extent?
# num years?










