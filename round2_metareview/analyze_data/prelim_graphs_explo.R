### Preliminary graphs for exploration of results ###
library(tidyverse)
library(UpSetR) # checkout https://cran.r-project.org/web/packages/UpSetR/vignettes/basic.usage.html


# read in the data
dat = read.csv("round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv") %>%
  filter(version=='final')




# Study system
  # upset diagram
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


# Places
  # I think this could be a good upset diagram too, 18 unique values
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

# Study type
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


# ES type (grouped) & Multifunctionality
dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'ES', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>% 
  data.frame() %>%
  upset(nsets = ncol(.)-1)

# Multifunctionality
  # can be observed in above plot



# Response variables by ES type
  # going to wait on this to see if there's a better way - maybe another sankey?


# ESPs - single, multiple, interacting

# only one intersection, so a simple bar graph will work for this one
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


# ESPs and ES type
# Community structure - how many?
#   Community structure - how many stopped at biodiversity?
#   Community structure and ES type
# Abiotic driver through biotic driver? - not sure exactly
# Time considered
# Multi scale
# Connectivity considered
# Spatial extent
# Space-time tradeoffs
# ES scale biases
# Methods scale biases
# Drivers scale biases
# Feedbacks
# Non-linearities/thresholds
