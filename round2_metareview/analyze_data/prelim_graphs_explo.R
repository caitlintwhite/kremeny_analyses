#' ---
#' title: "Preliminary graphs for results exploration"
#' output: html_document
#' ---


knitr::opts_chunk$set(echo = FALSE)

#+ messsage=FALSE, warnings=FALSE
### Preliminary graphs for exploration of results ###
suppressMessages(library(tidyverse))
library(UpSetR) # checkout https://cran.r-project.org/web/packages/UpSetR/vignettes/basic.usage.html


# read in the data
dat = read.csv("../data/cleaned/ESqualtrics_r2keep_cleaned.csv") %>%
  filter(version=='final')



#+
#' ## Study system
dat %>% 
  filter(abbr == 'Ecosystem') %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_answer', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>% 
  dplyr::select(-`Agricultural/Agroforestry/Rural`, -`Wetland/Riparian`, -`Experimental Ecosystem`) %>%
  data.frame() %>%
  upset(nsets = ncol(.)-1) # lots of parameters can be added here to make the plot look better

#+
#' ## Places
dat %>% 
  filter(abbr == 'Place') %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_answer', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>% 
  data.frame() %>%
  upset(nsets = ncol(.)-1)
# for this one, intersections might not matter quite as much, but could still be interesting to see
# intersections probably not useful, since most of the intersections have just 1 record

#+
#' ## Study type
dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_answer', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>% 
  data.frame() %>%
  upset(nsets = ncol(.)-1)

#+
#' ## ES type (grouped) & Multifunctionality
dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'ES', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>% 
  data.frame() %>%
  upset(nsets = ncol(.)-1, nintersects = NA) # nintersects changes total number of columns shown

#' Notes:
#' Most studies fall in single ES type bin, some 2 or 3 ES types. Max 11 ES bins in single study (see below)

#+
#' ##### Looking at number of studies (bottom row) with number of service types considered (top row)
dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>%
  group_by(Title) %>%
  summarise(count = n()) %>% select(count) %>% table()

#+
#' ## Multifunctionality
#' Can be observed in above plot


#+
#' ## Response variables by ES type
#' Probably a word cloud or another sankey (sankey would require binning similar
#' response var's which is probably too much to bother with)

#+
#' ## ESPs - single, multiple, interacting
dat %>%
  filter(abbr=='ESP_type') %>%
  dplyr::select(Title, clean_answer) %>%
  filter(!is.na(clean_answer)) %>%
  mutate(clean_answer = gsub(" \\[.*\\]", "", clean_answer)) %>%
  mutate(clean_answer = gsub('Across species ', 'Across species', clean_answer)) %>%
  mutate(clean_answer = gsub('Single species ', 'Single species', clean_answer)) %>%
  mutate(clean_answer = gsub('Within species  ', 'Within species', clean_answer)) %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#' Notes:
#' Not sure exactly how this will be used, or if the quality is usable?

# don't need this upset diagram
# dat %>%
#   filter(abbr=='ESP_type') %>% 
#   dplyr::select(Title, clean_answer) %>%
#   filter(!is.na(clean_answer)) %>%
#   mutate(clean_answer = gsub(" \\[.*\\]", "", clean_answer)) %>% 
#   mutate(clean_answer = gsub('Across species ', 'Across species', clean_answer)) %>%
#   mutate(clean_answer = gsub('Single species ', 'Single species', clean_answer)) %>%
#   mutate(clean_answer = gsub('Within species  ', 'Within species', clean_answer)) %>%
#   separate_rows(clean_answer, sep = ',') %>%
#   mutate(pres_holder = 1) %>%
#   pivot_wider(id_cols = 'Title', names_from = 'clean_answer', values_from = 'pres_holder') %>% 
#   replace(is.na(.), 0) %>% 
#   data.frame() %>%
#   upset(nsets = ncol(.)-1)

#+
#' ## ESPs and ES type
dat %>%
  filter(abbr=='ESP_type') %>%
  dplyr::select(Title, clean_answer) %>%
  filter(!is.na(clean_answer)) %>%
  mutate(clean_answer = gsub(" \\[.*\\]", "", clean_answer)) %>% 
  mutate(clean_answer = gsub('Across species ', 'Across species', clean_answer)) %>%
  mutate(clean_answer = gsub('Single species ', 'Single species', clean_answer)) %>%
  mutate(clean_answer = gsub('Within species  ', 'Within species', clean_answer)) %>%
  full_join(
    dat %>%
      filter(abbr=='Yclass') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, ES) %>%
      group_by(Title) %>%
      summarise(ES = paste0(unique(ES), collapse = ',')),
    by = 'Title'
  ) %>%
  group_by(clean_answer, ES) %>%
  summarise(count = n()) %>%
  # filter out all with multiple ES for better plotting
  filter(!grepl(',', ES)) %>%
  # also filter out the single species, only land cover study for ease of viz
  filter(!grepl(',', clean_answer)) %>%
  ungroup() %>%
  mutate(clean_answer = case_when(
    clean_answer=='Only land cover or habitat type as proxy' ~ 'Land cover/habitat type',
    TRUE ~ clean_answer
  )) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~ES) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#' Notes: 
#' pest pathogens more 'across species', climate regulation more NA thus less
#' often ESPs considered, Pollination more 'multiple ESP species'



#+
#' ## Community structure - how many?

dat %>%
  filter(abbr=='KremenTopics') %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  mutate(comm_struct = case_when(
    clean_answer == "Kremen Topic 2 : Community structure influencing function" ~ 'Yes',
    TRUE ~ 'No'
  )) %>% 
  group_by(Title) %>%
  summarise(comm_struct = paste0(unique(comm_struct), collapse = ',')) %>%
  mutate(comm_struct = case_when(
    comm_struct %in% c('Yes,No', 'No,Yes') ~ 'Yes',
    TRUE ~ comm_struct
  )) %>%
  group_by(comm_struct) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = comm_struct, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw()

#' Notes:
#' 79 studies look at comm structure influencing function and got included

#+
#' ### Community structure - how many stopped at biodiversity?
#' will have to ask Caitlin about this one (or she might be doing it already?)

#+
#' ### Community structure and ES type
dat %>%
  filter(abbr=='KremenTopics') %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  mutate(comm_struct = case_when(
    clean_answer == "Kremen Topic 2 : Community structure influencing function" ~ 'Yes',
    TRUE ~ 'No'
  )) %>% 
  group_by(Title) %>%
  summarise(comm_struct = paste0(unique(comm_struct), collapse = ',')) %>%
  mutate(comm_struct = case_when(
    comm_struct %in% c('Yes,No', 'No,Yes') ~ 'Yes',
    TRUE ~ comm_struct
  )) %>% 
  full_join(
    dat %>%
      filter(abbr=='Yclass') %>%
      filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
      dplyr::select(Title, ES) %>%
      group_by(Title) %>%
      summarise(ES = paste0(unique(ES), collapse = ',')),
    by = 'Title'
  ) %>% 
  group_by(comm_struct, ES) %>%
  summarise(count = n()) %>%
  # filter out all with multiple ES for better plotting
  filter(!grepl(',', ES)) %>%
  ggplot(aes(x = comm_struct, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~ES) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#' Notes: 
#' Pest pathogens, Habitat creation, Climate regulation pretty even (which
#' means more Yes than expected), Pollination may have more No's than expected


#+
#' ## Abiotic driver through biotic driver? 
#' not sure exactly what this means or if/how we could test it?

#+ 
#' ## Feedbacks
dat %>% 
  filter(abbr=='Feedbacks') %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  mutate(clean_answer = case_when(
    clean_answer=='No feedbacks measured directly' ~ 'None',
    TRUE ~ clean_answer
  )) %>%
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_answer', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>% 
  data.frame() %>%
  upset(nsets = ncol(.)-1)

dat %>% 
  filter(abbr=='Feedbacks') %>%
  dplyr::select(Title, clean_answer) %>%
  group_by(clean_answer) %>%
  summarise(count = n())

#+
#' ## Non-linearities/thresholds
dat %>% 
  filter(abbr=='Thresholds') %>%
  dplyr::select(Title, clean_answer) %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('No', 'Mentioned or discussed but not measured','Yes'))) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  xlab('Thresholds considered?') 


#' # SPATIO-TEMPORAL SCALE - LARGE SECTION
#' This section contains three subsections. First, there are some basic plots of
#' the overall trends of spatio-temporal scale across all papers. Second, there
#' is a section looking at space-time tradeoffs. Third, there is a section on
#' scale biases for Methods used, ES type, and Driver group.
#' 
#' In sections two and three, the dashed line represents what we would expect if
#' subgroups were selected at random from the overall distribution from Yes's
#' and No's. Thus, bars larger or smaller than the dashed line show that either
#' time or space is more/less likely to be considered in that group. (I'm still
#' working on how to write this up more specifically in a non-confusing way...so
#' let me know if you have questions!). It's also useful to keep in mind that
#' these proportions can also be considered in absolute terms, not just with
#' respect to the dashed line. So, for example, even if something is more likely
#' than expected to be multi-scale, it still may be the case that less than half
#' of the studies in that group are multi-scale (which would still point to some
#' sort of absolute gap).

#+
#' ### Time considered
dat %>%
  filter(abbr=='TimeTrends') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  xlab('Time considered?') +
  ylab('Number of papers') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#+
#' #### Number of years
dat %>%
  filter(abbr=='YrsData') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  filter(!is.na(clean_answer)) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('1 year or less','2–5 years','6–10 years', '10+ years'))) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab('Number of years')

#+
#' ### Multi scale
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

#+
#' ### Connectivity considered
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

#+ 
#' ### Spatial extent
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


#+
#' ## Space-time tradeoffs
#' Reminder that the dashed line represents the overall proportion before
#' grouping into the different categories.

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

#' In the plot below, the dashed line represents the overall proportion of
#' multi-scale studies given that the study considered temporal trends (since
#' that was the only way to get the number of years).

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
#' For these biases plots, a single paper can fit into multiple groups (e.g. a
#' paper that used Observational and Model/data simulation methods). The dashed
#' line accounts for this, so that it still serves as a 'random expectation',
#' but it is a little complicated to explain (I'm working on putting together a
#' little explanation with an example though!).

study_type = dat %>%
  filter(abbr=='Methods') %>%
  dplyr::select(Title, Methods = clean_answer) %>%
  mutate(Methods = gsub(" \\(.*\\)", '', Methods))

#+
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

#+
#' ### Number of years
#' Unlike the above plots which double count papers that are in multiple groups,
#' these plots keep papers single counted because it shows the counts and not
#' proportions. These also keep all intersecting groups separate instead of only
#' looking at the groups individually. These plots are a little messier, but I'm
#' working on a way to better show these patterns (if it seems worth it).

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

#+
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
#' ### Spatial extent
#' Unlike the above plots which double count papers that are in multiple groups,
#' these plots keep papers single counted because it shows the counts and not
#' proportions. These also keep all intersecting groups separate instead of only
#' looking at the groups individually. These plots are a little messier, but I'm
#' working on a way to better show these patterns (if it seems worth it).

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
#' ## ES type scale biases
#' For these biases plots, a single paper can fit into multiple groups (e.g. a
#' paper that used Observational and Model/data simulation methods). The dashed
#' line accounts for this, so that it still serves as a 'random expectation',
#' but it is a little complicated to explain (I'm working on putting together a
#' little explanation with an example though!).

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


#+
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

#+
#' ### Number of years
#' Unlike the above plots which double count papers that are in multiple groups,
#' these plots keep papers single counted because it shows the counts and not
#' proportions. These also keep all intersecting groups separate instead of only
#' looking at the groups individually. These plots are a little messier, but I'm
#' working on a way to better show these patterns (if it seems worth it).

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

#+
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
#' ### Spatial extent
#' Unlike the above plots which double count papers that are in multiple groups,
#' these plots keep papers single counted because it shows the counts and not
#' proportions. These also keep all intersecting groups separate instead of only
#' looking at the groups individually. These plots are a little messier, but I'm
#' working on a way to better show these patterns (if it seems worth it).

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
#' ## Driver scale biases
#' For these biases plots, a single paper can fit into multiple groups (e.g. a
#' paper that used Observational and Model/data simulation methods). The dashed
#' line accounts for this, so that it still serves as a 'random expectation',
#' but it is a little complicated to explain (I'm working on putting together a
#' little explanation with an example though!).

driver_groups =  dat %>%
  filter(abbr=='Driver') %>%
  filter(!is.na(clean_answer)) %>%
  dplyr::select(Title, Group, ES) %>% #may want to consider ES as well
  group_by(Title) %>%
  summarise(Group = paste0(unique(Group), collapse = ', '),
            ES = paste0(unique(ES), collapse = ', '))


#+
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

#+
#' ### Number of years
#' Unlike the above plots which double count papers that are in multiple groups,
#' these plots keep papers single counted because it shows the counts and not
#' proportions. These also keep all intersecting groups separate instead of only
#' looking at the groups individually. These plots are a little messier, but I'm
#' working on a way to better show these patterns (if it seems worth it).
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


#+
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


#+
#' ### Spatial extent
#' Unlike the above plots which double count papers that are in multiple groups,
#' these plots keep papers single counted because it shows the counts and not
#' proportions. These also keep all intersecting groups separate instead of only
#' looking at the groups individually. These plots are a little messier, but I'm
#' working on a way to better show these patterns (if it seems worth it).

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

