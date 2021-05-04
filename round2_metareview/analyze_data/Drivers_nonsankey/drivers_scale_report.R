#' ---
#' title: Drivers and scale intersections
#' author: 
#' date:
#' output: 
#'    github_document:
#'       pandoc_args: --webtex
#' ---
#' 


#+ echo=F, results=F, message=F, warnings=F
library(tidyverse, quietly = TRUE, warn.conflicts = FALSE)

dat = read.csv('../../../round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

# drivers true false categories by title
driv_tf_bytitle = dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_group) %>% 
  unique() %>%
  filter(!is.na(clean_group)) %>% # two NAs snuck through somehow, I checked and there were always other drivers
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE) 

# specific drivers by title
driv_specific_bytitle = dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_answer_binned, Group) %>%
  unique() %>% 
  filter(clean_answer_binned != 'Other')

# spatial extent
extent_title = dat %>% 
  filter(abbr=='Extent') %>% 
  dplyr::select(Title, Extent = clean_answer)



#' ### Proportion of studies that studied each type of driver broken down by scale
#' These first three plots show the proportion of studies at each scale that
#' consider biotic, environmental, and human drivers. These proportions were
#' calculated out of the totals for each scale, so if every local study
#' considered a biotic variable, that bar would be at 1. I could also do this
#' from the opposite perspective, asking how many studies that considered biotic
#' drivers were in each scale, if that is more applicable. Either way, I think
#' the plots in the next section will be more useful in general.
#' 
#+ echo=F
prop_driv_scale = extent_title %>%
  left_join(driv_tf_bytitle, by = 'Title') %>% 
  filter(!is.na(Biotic)) %>% #remove NAs for studies that weren't present in driv_tf_bytitle
  group_by(Extent) %>%
  summarise(count_ext = n(), 
            n_biotic = sum(Biotic), 
            n_env = sum(Environmental),
            n_human = sum(Human)) %>%
  mutate(prop_biotic = n_biotic/count_ext,
         prop_env = n_env/count_ext,
         prop_human = n_human/count_ext) # these proportions are taken within each scale

#+ echo=F
prop_driv_scale %>%
  mutate(Extent = factor(Extent, levels = c('Local','Macro-scale','Global','Undefined/no scale'))) %>%
  ggplot() +
  geom_col(aes(x = Extent, y = prop_biotic)) +
  ylab('Proportion of studies at each scale with a biotic driver') +
  ggtitle('Biotic drivers')

#' This figure shows the proportion of studies (out of the total number within
#' each scale bin) that studied a biotic driver.


#+ echo=F
prop_driv_scale %>%
  mutate(Extent = factor(Extent, levels = c('Local','Macro-scale','Global','Undefined/no scale'))) %>%
  ggplot() +
  geom_col(aes(x = Extent, y = prop_env)) +
  ylab('Proportion of studies at each scale with an environmental driver') +
  ggtitle('Environmental drivers')

#' This figure shows the proportion of studies (out of the total number within
#' each scale bin) that studied an environmental driver.

#+ echo=F
prop_driv_scale %>%
  mutate(Extent = factor(Extent, levels = c('Local','Macro-scale','Global','Undefined/no scale'))) %>%
  ggplot() +
  geom_col(aes(x = Extent, y = prop_human)) +
  ylab('Proportion of studies at each scale with a human driver') +
  ggtitle('Human drivers')

#' This figure shows the proportion of studies (out of the total number within
#' each scale bin) that studied a human driver.
#' 



#' ## Specific drivers at different scales
#' Here, I've made three figures (one for each scale bin) that shows the
#' proportion of studies within that scale bin that considered specific types of
#' drivers. The drivers are also broken out among their groups (biotic,
#' environmental, human).

#+ echo=F
# Which specific drivers at each scale (maybe with broad categories)
driv_specific_scale = driv_specific_bytitle %>%
  left_join(extent_title, by='Title') 


# numbers at each scale - taken from extent_props
n_global = 7
n_macro = 197
n_local = 58



driv_specific_scale %>%
  group_by(Extent, Group, clean_answer_binned) %>%
  summarise(count = n()) %>%
  filter(Extent=='Global') %>%
  mutate(proportion = count / n_global) %>% #props of global studies for each driver
  filter(proportion > 0.0) %>% 
  ggplot(aes(x = fct_reorder(clean_answer_binned, proportion), y = proportion)) +
  geom_col() +
  facet_wrap(~Group) +
  xlab('') +
  ylab('Proportion of global studies') +
  ggtitle('Global studies (n=7) - specific drivers') +
  coord_flip() +
  theme_bw()

driv_specific_scale %>%
  group_by(Extent, Group, clean_answer_binned) %>%
  summarise(count = n()) %>%
  filter(Extent=='Macro-scale') %>%
  mutate(proportion = count / n_macro) %>% #props of global studies for each driver
  filter(proportion > 0.0) %>% 
  ggplot(aes(x = fct_reorder(clean_answer_binned, proportion), y = proportion)) +
  geom_col() +
  facet_wrap(~Group) +
  xlab('') +
  ylab('Proportion of macro-scale studies') +
  ggtitle('Macro-scale studies (n=197) - specific drivers') +
  coord_flip() +
  theme_bw()


driv_specific_scale %>%
  group_by(Extent, Group, clean_answer_binned) %>%
  summarise(count = n()) %>%
  filter(Extent=='Local') %>%
  mutate(proportion = count / n_local) %>% #props of global studies for each driver
  filter(proportion > 0.0) %>% 
  ggplot(aes(x = fct_reorder(clean_answer_binned, proportion), y = proportion)) +
  geom_col() +
  facet_wrap(~Group) +
  xlab('') +
  ylab('Proportion of local studies') +
  ggtitle('Local studies (n=58) - specific drivers') +
  coord_flip() +
  theme_bw()

