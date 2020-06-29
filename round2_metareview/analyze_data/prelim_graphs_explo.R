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
#' ## Time considered
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
#' ### Number of years
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
#' ## Multi scale
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
#' ## Connectivity considered
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
#' ## Spatial extent
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

#+
#' ### Time considered vs nested/multiple scales
#' Working on best way to visualize, bar graphs look confusing
dat %>% 
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
  group_by(TimeTrends, Nested) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = TimeTrends, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Nested) +
  xlab("Does the study consider time? (facetted by whether it is multi-scale)") +
  ylab("Number of papers") +
  theme_bw()

#' Notes: 
#' Multi-scale studies are less likely to consider time (though not drastically so)


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

#' ### Temporal trends?
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

#' ### Nested/multi-scale?
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

#' ### Temporal trends?
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

#' ### Nested/multi-scale?
study_type %>%
  left_join(nested_df, by = 'Title') %>%
  separate_rows(Methods, sep = ',') %>% 
  arrange(Methods) %>%
  group_by(Title) %>%
  summarise(Methods = paste0(unique(Methods), collapse = ','),
            Nested = paste0(unique(Nested), collapse = ',')) %>%
  group_by(Methods, Nested) %>%
  summarise(count = n()) %>%
  # need to deal with order of the different options - not sure why they can go in different orders
  ggplot(aes(x = Nested, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Methods) +
  theme_bw()

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

#' ### Temporal trends?
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

#' ### Nested/multi-scale?
driver_groups %>%
  left_join(nested_df, by = 'Title') %>%
  group_by(Group, Nested) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = Nested, y = count, label = count)) +
  geom_col() +
  geom_label() +
  facet_wrap(~Group) +
  theme_bw()

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

