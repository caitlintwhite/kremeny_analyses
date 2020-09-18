# This script contains values to be placed in the text of the results section.
# It also includes some tables that will be useful to include in the SI.


library(tidyverse)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

num_papers = dat %>% 
  pull(Title) %>%
  unique() %>%
  length()


###### Paragraph 1 ######
# Our literature review shows a heavy bias toward studies conducted in North
# America and Europe (XX% combined; Figure X.A) 
loc_counts = dat %>% 
  filter(abbr == 'Place') %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / num_papers)
loc_counts

loc_counts %>% 
  filter(clean_answer %in% c('North America','Europe')) %>%
  pull(proportion) %>%
  sum() %>%
  round(digits = 2)


# and towards terrestrial study
# systems (XX%; Figure X.B). 
study_systems = dat %>% 
  filter(abbr == 'Ecosystem') %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers)
study_systems

study_systems %>% 
  filter(clean_answer == 'Terrestrial') %>% 
  pull(proportion) %>%
  round(digits = 2)

# Very few studies we reviewed were conducted in
# marine or coastal systems (XX%, Figure X.B). 
study_systems %>% 
  filter(clean_answer %in% c('Marine/Off-shore', 'Coastal')) %>% 
  pull(proportion) %>%
  sum() %>%
  round(digits = 2)

# Ecology of ecosystem services studies most
# often use observational methods (XX%), 
methods_used = dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / num_papers)
methods_used

methods_used %>% 
  filter(clean_answer=='Observational') %>% 
  pull(proportion) %>% 
  round(digits=2)

# though experimental and model/data
# simulation methods are used frequently as well (XX % and XX% respectively;
# Figure X.C). 
methods_used %>% 
  filter(clean_answer %in% c('Experimental', 'Model/Data Simulation')) %>% 
  dplyr::select(clean_answer, proportion) 

# Social surveys/interviews are used rarely, with only XX% of
# studies using those methods.
methods_used %>% 
  filter(clean_answer %in% c('Social survey/Interview')) %>%
  pull(proportion) %>%
  round(digits=2)

# XX% of studies use multiple methods (SI.XX).
dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(Title, clean_answer) %>%
  filter(grepl(',', clean_answer)) %>%
  nrow(.) / num_papers
  

###### Paragraph 2 ######
# Our review showed that ecology of ecosystem services studies examine many
# types of drivers. Most commonly studied were anthropogenic drivers (XX% of studies),
driver_groups = dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_group) %>% 
  unique() %>%
  group_by(clean_group) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / num_papers)
driver_groups

driver_groups %>% 
  filter(clean_group == 'Human') %>% 
  pull(proportion) %>% 
  round(digits=2)

# followed by biotic (studied in XX% of studies), 
driver_groups %>% 
  filter(clean_group == 'Biotic') %>% 
  pull(proportion) %>% 
  round(digits=2)

# and environmental (present in XX% of studies) drivers. 
driver_groups %>% 
  filter(clean_group == 'Environmental') %>% 
  pull(proportion) %>% 
  round(digits=2)

# There were some
# differences in the representation of driver types among different ecosystem
# service types (Figure X- Sankey). [For example, ____sankey example, tough to
# explain concisely____.]


###### Paragraph 3 ######
# Many studies considered multiple types of drivers (XX %; Figure X.A).
dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_group) %>% 
  unique() %>%
  filter(!is.na(clean_group)) %>% # two NAs snuck through somehow, I checked and there were always other drivers
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE) %>%
  mutate(multiple_drivers = ifelse(Biotic + Human + Environmental > 1, TRUE, FALSE)) %>%
  group_by(multiple_drivers) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count))


# The most
# common combinations of driver types were anthropogenic-biotic (XX %)
dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_group) %>% 
  unique() %>%
  filter(!is.na(clean_group)) %>% # two NAs snuck through somehow, I checked and there were always other drivers
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE) %>%
  mutate(humbio = ifelse(Biotic & Human, TRUE, FALSE)) %>%
  group_by(humbio) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count))

# and
# anthropogenic-environmental (XX %), likely reflecting the prevalence of
# anthropogenic drivers in studies. 
dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_group) %>% 
  unique() %>%
  filter(!is.na(clean_group)) %>% # two NAs snuck through somehow, I checked and there were always other drivers
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE) %>%
  mutate(humenv = ifelse(Environmental & Human, TRUE, FALSE)) %>%
  group_by(humenv) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count))

# XX% of studies considered only anthropogenic
# drivers (Figure X.A). 
dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_group) %>% 
  unique() %>%
  filter(!is.na(clean_group)) %>% # two NAs snuck through somehow, I checked and there were always other drivers
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE) %>%
  mutate(humonly = ifelse(Human & !Biotic & !Environmental, TRUE, FALSE)) %>%
  group_by(humonly) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count))

# Environmental drivers were rarely the only driver type
# included in a study (XX %; Figure X.A). 
dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_group) %>% 
  unique() %>%
  filter(!is.na(clean_group)) %>% # two NAs snuck through somehow, I checked and there were always other drivers
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE) %>%
  mutate(envonly = ifelse(!Human & !Biotic & Environmental, TRUE, FALSE)) %>%
  group_by(envonly) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count))

# Additionally, XX % of studies studied
# drivers from all three types. 
dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_group) %>% 
  unique() %>%
  filter(!is.na(clean_group)) %>% # two NAs snuck through somehow, I checked and there were always other drivers
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE) %>%
  mutate(alldriv = ifelse(Human & Biotic & Environmental, TRUE, FALSE)) %>%
  group_by(alldriv) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count))

# Notably, service provider identity (see SI.X for
# definition) was the most commonly studied biotic driver making up XX% of
# studies that looked at biotic drivers (Figure X.B). 
drivers_binned = dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_answer_binned, Group) %>%
  unique() %>% 
  filter(clean_answer_binned != 'Other') %>%
  group_by(Group, clean_answer_binned) %>%
  summarise(count = n())

drivers_binned %>%
  filter(Group == 'Biotic') %>%
  mutate(proportion = count / sum(count)) %>%
  arrange(-proportion)

# Management/restoration and
# land use and land cover change were the most commonly studied anthropogenic
# drivers (Figure X.C). 
drivers_binned %>%
  filter(Group == 'Human') %>%
  mutate(proportion = count / sum(count)) %>%
  arrange(-proportion)

# The most common environmental driver was climate (Figure
# X.D).
drivers_binned %>%
  filter(Group == 'Environmental') %>%
  mutate(proportion = count / sum(count)) %>%
  arrange(-proportion)




###### Paragraph 4 ######
# Our review showed a wide range of representation of ecosystem service types in
# the literature. The most prevalent ecosystem service type was climate
# regulation, which was studied in XX% of studies (Figure X.A). By comparison,
# the least studied ecosystem service type was ocean regulation, present in just
# XX% of studies (Figure X.A). 
dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(ES) %>%
  group_by(ES) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers) %>%
  arrange(-proportion)

# XX % of studies in our review considered more than one service type (Figure
# SI.X). Most of these studies considered just two ecosystem service types,
# though we reviewed papers that included up to 11 ecosystem service types
# (Figure SI.X).
numtypes_prop_tab = dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'ES', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>%
  mutate(num_ES_types = rowSums(.[-1])) %>%
  dplyr::select(num_ES_types) %>%
  group_by(num_ES_types) %>%
  summarise(count = n()) %>% 
  ungroup() %>%
  mutate(proportion = count / sum(count))
numtypes_prop_tab

# prop of more than 1 ES type
numtypes_prop_tab %>% 
  filter(num_ES_types != 1) %>% 
  pull(proportion) %>% 
  sum() %>%
  round(digits=2)

# There were some common pairings of
# ecosystem service types, particularly __ES__ with __ES__ and __ES__ with
# __ES__ (Figure X.B). However, the common pairings do not dominate the pairings
# for any ecosystem service type; most ecosystem service types have been paired
# with many other ecosystem service types (Figure X.B).




###### Paragraph 5 ######
# Most studies in our review were conducted at the macro-scale (XX%; brief def.)
# level, though there were several local scale (XX%; brief def.) studies as
# well. We found very few global scale (spanning multiple continents) studies
# (Figure X.D). 
dat %>% 
  filter(abbr=='Extent') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / num_papers) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('Local', 'Macro-scale','Global','Undefined/no scale'))) %>%
  arrange(clean_answer)

# Additionally, (almost?) all global scale studies used a
# combination of observational and model/data simulation methods (SI.X). 
# THIS IS NOT CORRECT, this statement will be removed
glob_titles = dat %>% 
  filter(abbr=='Extent') %>%
  dplyr::select(Title, clean_answer) %>%
  filter(clean_answer=='Global') %>%
  pull(Title) %>%
  as.character()

dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  filter(Title %in% glob_titles) %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportion_ofglobal = count / sum(count))


# About
# XX % of studies in our review considered temporal dynamics (Figure X.E). 
temp_tab = dat %>%
  filter(abbr=='YrsData') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(clean_answer = as.character(clean_answer)) %>%
  mutate(clean_answer = ifelse(is.na(clean_answer), 'Time not considered', clean_answer)) %>%
  mutate(proportion = count / num_papers) %>%
  mutate(clean_answer = gsub('–', '-', clean_answer)) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('Time not considered', '1 year or less','2-5 years','6-10 years', '10+ years'))) %>%
  arrange(clean_answer)
temp_tab

# did consider time
1 - temp_tab %>% filter(clean_answer == 'Time not considered') %>% pull(proportion)

# Of
# those studies that considered time, a surprising XX% included 10 or more years
# (Figure X.E). 
temp_tab %>% 
  dplyr::select(-proportion) %>%
  filter(clean_answer != 'Time not considered') %>%
  mutate(proportion_tempyes = count / sum(count))


# [Methods considerations for 10+ years studies].
tenyear_studies = dat %>%
  filter(abbr=='YrsData') %>%
  dplyr::select(Title, clean_answer) %>%
  filter(clean_answer=='10+ years') %>%
  pull(Title) %>%
  as.character()

tenyear_prop = dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  filter(Title %in% tenyear_studies) %>%
  group_by(Title) %>%
  summarise(methods = paste0(clean_answer, collapse = ',')) %>%
  group_by(methods) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportion_oftenyear = count / sum(count)) 
tenyear_prop

# tenyears used model/data sim
tenyear_prop %>%
  filter(grepl('Model/Data Simulation',methods)) %>%
  pull(proportion_oftenyear) %>%
  sum()


###### Paragraph 6 ######
# There was a clear tradeoff between considering space and considering time.
# Only XX% of studies considered both space and time, with XX% considering
# neither and XX% considering either space or time but not both (Figure SI.X).
time_space = dat %>% 
  filter(abbr=='Nested') %>%
  dplyr::select(Title, 'Nested' = clean_answer) %>%
  full_join(
    dat %>% 
      filter(abbr=='TimeTrends') %>%
      mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
      mutate(clean_answer = ifelse(clean_answer =='Space for time', 'Yes', clean_answer)) %>%
      dplyr::select(Title, 'TimeTrends' = clean_answer),
    by = 'Title'
  ) %>% 
  # removing NAs, not sure why some (11) studies don't have TimeTrends?
  filter(!is.na(TimeTrends)) %>%
  group_by(Nested, TimeTrends) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(proportion = count / sum(count))
time_space

# percent considering both space and time
time_space %>% 
  filter(Nested == 'Yes' & TimeTrends =='Yes') %>% 
  pull(proportion) %>% 
  round(digits=2)

#percent considering neither
time_space %>% 
  filter(Nested == 'No' & TimeTrends =='No') %>% 
  pull(proportion) %>% 
  round(digits=2)

# percent considering one or the other
time_space %>%
  filter(Nested != TimeTrends) %>%
  pull(proportion) %>%
  sum() %>%
  round(digits=2)



