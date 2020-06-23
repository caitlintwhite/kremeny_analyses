# Scale in conjunction with other variables
# Author: Grant

library(tidyverse)


dat = read.csv("round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv") %>%
  filter(version=='final') # avoids the double reviewed papers

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
  #summarise(TimeTrends = paste0(unique(TimeTrends), collapse = '_')) %>% # deal with double reviews
  #filter(!grepl('_', TimeTrends)) %>% # TEMPORARY - remove studies with double review disagreements
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) # get rid of the 'e.g' text

yrs_df = dat %>%
  filter(abbr=='YrsData') %>%
  dplyr::select(Title, YrsData = clean_answer) %>%
  filter(!is.na(YrsData)) %>% # excludes the No's in timetrends
  group_by(Title) #%>%
  #summarise(YrsData = paste0(unique(YrsData), collapse = '_')) %>% # deal with double reviews
  #filter(!grepl('_', YrsData)) # TEMPORARY - remove studies with double review disagreements


### Scales broken down by study type (experimental, sim, observational, etc)
study_type = dat %>%
  filter(abbr=='Methods') %>%
  dplyr::select(Title, Methods = clean_answer) %>%
  mutate(Methods = gsub(" \\(.*\\)", '', Methods)) #%>% #clean up the stuff in parentheses
  #group_by(Title) %>%
  #summarise(Methods = paste0(unique(Methods), collapse = '_')) %>% # deal with double reviews
  #filter(!grepl('_', Methods)) # TEMPORARY - remove studies with double review disagreements
  
  
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
  
  # mostly the same trend across all of the study types


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

  # experimental studies mostly short term (1-5 years), model/data simulation
  # often long term 10+, quite a few long term Observational studies as well



### Scales broken down by service bins
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


# multi-scale?
service_bins %>%
  filter(ES %in% ES_groups_to_plot) %>%
  left_join(nested_df, by = 'Title') %>%
  group_by(ES, Nested) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Nested, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~ES) +
  theme_bw()

  # pest path and pollination have more yes than no, others even or more no's
  

# time considered?
service_bins %>%
  filter(ES %in% ES_groups_to_plot) %>%
  left_join(time_trends_df, by = 'Title') %>%
  group_by(ES, TimeTrends) %>%
  summarise(count = n()) %>%
  filter(TimeTrends != 'Space for time') %>%
  ggplot(aes(x = TimeTrends, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~ES) +
  theme_bw()

  # habitat creation more likely to have time considered, most even or more no's

# spatial extent?
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

  # no obvious trends here, might need to look at all of the groups differently

# num years?
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

  #climate regulation, food, soil protection quite a few long-term studies


### Scales broken down by type of driver
driver_groups =  dat %>%
  filter(abbr=='Driver') %>%
  filter(!is.na(clean_answer)) %>%
  dplyr::select(Title, Group, ES) %>% #may want to consider ES as well
  group_by(Title) %>%
  summarise(Group = paste0(unique(Group), collapse = ', '),
            ES = paste0(unique(ES), collapse = ', ')) 
  # maybe also do one with specific drivers included

# these ones might need to be done differently, since a lot of studies have more
# than one type of driver, maybe flip it?


# multi-scale?
driver_groups %>%
  left_join(nested_df, by = 'Title') %>%
  group_by(Group, Nested) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Nested, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Group) +
  theme_bw()

  # mostly same trends across groups

# time considered?
driver_groups %>%
  left_join(time_trends_df, by = 'Title') %>%
  group_by(Group, TimeTrends) %>%
  summarise(count = n()) %>%
  filter(TimeTrends != 'Space for time') %>%
  ggplot(aes(x = TimeTrends, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Group) +
  theme_bw()

  # tough to tell with the groups the way they are

# spatial extent?
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

  # tough to tell with the groups the way they are

# num years?
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

  # tough to tell with the groups the way they are







### Summary ###

# Study type
# Model/Data simulation studies are more likely to consider time. 1/3 (roughly) of Experimental and Observational studies consider time.
# Experimental studies biased toward local scale.
# Experimental studies mostly short term (1-5 years). Model/Data simulation often long term (10+). Quite a few long-term Observational studies.

# ES type
# Pest/pathogens and Pollination more often consider scale. Others even or more no's.
# Habitat creation more likely to have time considered. Others even or more no's.
# Climate regulation, Food, Soil protection have quite a few long-term studies.

# Drivers
# need to figure out how to best explore this

