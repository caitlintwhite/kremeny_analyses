## Script to exclude Land use / land cover from other types of drivers. 
## Goal: take papers that looked at land use/land cover within biotic, human,
## and environmental drivers, separate them into their own 'lu/lc' category,
## then re-calculate how many papers looked at biotic, human, environmental
## drivers.

library(tidyverse)
library(ggVennDiagram)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')



# find the land use land cover studies
dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_answer_binned, Group) %>% 
  pull(clean_answer_binned) %>% 
  unique() 

  # 'Land cover' and 'Land use and land cover change' are the two bins that should get their own categories


# Biotic drivers papers that only looked at land cover or habitat type as a proxy
biot_lulc_titles = dat %>%
  filter(qnum=='Q14', abbr=='ESP_type', clean_answer == 'Only land cover or habitat type as proxy') %>%
  pull(Title)


# separate out land use and land cover studies
driv_types_title = dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_answer_binned, clean_group) %>%
  mutate(old_group = clean_group) %>%
  # reassign clean_answer_binned land cover answers to have Group 'LU LC'
  mutate(clean_group = ifelse(clean_answer_binned %in% c('Land cover', 'Land use and land cover change'), 'LU_LC',clean_group)) %>%
  # reassign studies with only land cover biotic drivers (based on Q14) to 'LU LC' Group
  mutate(clean_group = ifelse(Title %in% biot_lulc_titles & clean_group == 'Biotic', 'LU_LC', clean_group)) %>%
  # take only unique rows 
  unique() %>%
  select(-old_group, -clean_answer_binned) %>%
  filter(!is.na(clean_group)) %>% # two NAs snuck through somehow, I checked and there were always other drivers
  mutate(pres_holder = TRUE) %>%
  unique() %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE)

# exclude lulc studies
excl_lulc = driv_types_title %>%
  select(-LU_LC) %>%
  filter(!(Biotic=='FALSE' & Human =='FALSE' & Environmental =='FALSE'))
    # 110 studies used an lulc drivers, 26 studies only used lulc drivers
  
# make overall venn diagram
venn_list = list(
    biotic = excl_lulc %>% filter(Biotic == TRUE) %>% pull(Title) %>% as.character(), 
    human = excl_lulc %>% filter(Human == TRUE) %>% pull(Title) %>% as.character(),
    environmental = excl_lulc %>% filter(Environmental == TRUE) %>% pull(Title) %>% as.character()
  )

ggVennDiagram(venn_list, category.names = c('Biotic drivers','Human drivers','Environmental drivers'), label = 'percent', color = 'darkgray', show.legend = FALSE) +
  scale_fill_distiller(direction = 1) + #change fill colour palette
  scale_color_manual(values = c('black','black','black')) + #outlines of circles
  theme(legend.position = 'None', plot.title = element_text(hjust =-0.1, face='bold')) + #remove legend
  coord_sf(clip="off")  #don't cutoff labels 
  # excludes 26 studies that only used land use or land cover drivers


# make venn diagrams by ES type


