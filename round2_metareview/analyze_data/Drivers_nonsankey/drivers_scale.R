library(tidyverse)

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
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






### Proportion of studies at each scale that looked at each type of driver
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
  
prop_driv_scale %>%
  mutate(Extent = factor(Extent, levels = c('Local','Macro-scale','Global','Undefined/no scale'))) %>%
  ggplot() +
  geom_col(aes(x = Extent, y = prop_biotic)) +
  ylab('Proportion of studies at each scale with a biotic driver')

# just under half of macro-scale studies considered a biotic driver




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
ggsave('round2_metareview/analyze_data/Drivers_nonsankey/fig_files/global_drivers.pdf',width=10, height=7)

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
ggsave('round2_metareview/analyze_data/Drivers_nonsankey/fig_files/macro_drivers.pdf',width=10, height=7)


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
ggsave('round2_metareview/analyze_data/Drivers_nonsankey/fig_files/local_drivers.pdf',width=10, height=7)

# These three figures show the proportion of studies within each spatial extent bin that studied each driver, with drivers split by their biotic/env/human bin.

