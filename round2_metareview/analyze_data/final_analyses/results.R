### Script to produce all final figures and all final results (percents, summaries, etc.)

library(tidyverse)
library(ggVennDiagram)
library(PBSmapping)


###### Read and prep data
dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

num_papers = dat %>% 
  pull(Title) %>%
  unique() %>%
  length()


### Re-classify land use and land cover drivers studies

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



##### General Patterns
# STILL NEED TO ADD INDIAN OCEAN WITH 0% LABEL ON PLOT

### geographic location
loc_counts = dat %>% 
  filter(abbr == 'Place') %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / num_papers)

wm <- map_data("world") %>%
  setNames(c("X","Y","PID","POS","region","subregion")) %>% 
  clipPolys(xlim=c(-180,180), ylim=c(-180,180), keepExtra=T) %>% #clips at boundaries so it can work with map projections
  rename(long = X, lat = Y, group = PID)

# country continents dataframe
cc <- raster::ccodes()

# rename country codes RENAME 
# some country names don't match between the two databases, I only corrected
# those that were visible on the map
wm$region %>% unique %>% setdiff(cc$NAME)
  
mappings <- c("UK"="United Kingdom", "USA"="United States", "Ivory Coast" = "Côte d'Ivoire") # You add the others here
cc$NAME[match(mappings, cc$NAME)] <- names(mappings)

# change Russia to Asia, not Europe (looks weird with all of it considered Europe)
cc[cc$NAME=='Russia',]$continent <- 'Asia'

# set up labels
loc_counts = loc_counts %>%
  mutate(label_coords_lon = case_when(
    clean_answer == 'Africa' ~ 20,
    clean_answer == 'Asia' ~ 90,
    clean_answer == 'Europe' ~ 10,
    clean_answer == 'North America' ~ -100,
    clean_answer == 'Oceania' ~ 150,
    clean_answer == 'South America' ~ -60,
    clean_answer == 'Global Marine' ~ -150,
    clean_answer == 'Global Terrestrial' ~ -120,
    clean_answer == 'Atlantic Ocean' ~ -45,
    clean_answer == 'Pacific Ocean' ~ -120
  ),
  label_coords_lat = case_when(
    clean_answer == 'Africa' ~ 10,
    clean_answer == 'Asia' ~ 40,
    clean_answer == 'Europe' ~ 50,
    clean_answer == 'North America' ~ 40,
    clean_answer == 'Oceania' ~ -15,
    clean_answer == 'South America' ~ -15,
    clean_answer == 'Global Marine' ~ -80,
    clean_answer == 'Global Terrestrial' ~ -80,
    clean_answer == 'Atlantic Ocean' ~ 25,
    clean_answer == 'Pacific Ocean' ~ 0
  )) 

# join cc to the counts of papers and PLOT
cc %>%
  dplyr::select(NAME, continent) %>%
  left_join(loc_counts, by = c('continent' = 'clean_answer')) %>%
  left_join(wm, by = c('NAME' = 'region')) %>%
  ggplot() +
  geom_vline(xintercept = 180, colour = 'lightblue') +
  geom_vline(xintercept = -180, colour = 'lightblue') +
  geom_polygon(aes(x = long, y = lat, group = group, fill = -proportion), show.legend = F) +
  geom_label(data = loc_counts %>% filter(!grepl('Global',clean_answer)), 
             aes(x = label_coords_lon, 
                 y = label_coords_lat, 
                 label = paste0(clean_answer, ':', 100*round(proportion, digits = 2), '%') )) +
  #geom_tile(aes(x = 0, y = 0, width = 360, height = 360), colour = 'lightblue') +
  #geom_abline(slope = Inf, intercept = 180) +
  coord_map("mollweide") + # to change projections
  theme_void()

ggsave('round2_metareview/analyze_data/final_analyses/fig_files/map_study_locations.pdf', width = 10, height = 5)

# study system
# STILL NEED TO WRITE SI METHODS FOR THIS FIGURE, SOME CATEGORIES EXCLUDED BECAUSE THEY ARE SUBSETS
dat %>% 
  filter(abbr == 'Ecosystem') %>%
  dplyr::select(Title, clean_answer) %>% 
  separate_rows(clean_answer, sep = ',') %>%
  filter(clean_answer %in% c('Terrestrial','Marine/Off-shore','Coastal','Freshwater')) %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / num_papers) %>%
  ggplot(aes(x = fct_reorder(clean_answer, proportion), y = proportion, label = round(proportion, digits = 2))) +
  geom_col() +
  xlab('Study system') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw() 

ggsave('round2_metareview/analyze_data/final_analyses/fig_files/study_system_simple.pdf', width = 5, height = 5, dpi = 'retina')


# methods
# STILL might need to do something to show that several studies used multiple methods
dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / num_papers) %>%
  ggplot(aes(x = fct_reorder(clean_answer, proportion), y = proportion)) +
  geom_col() +
  xlab('Methods used') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw()

ggsave('round2_metareview/analyze_data/final_analyses/fig_files/methods_used.pdf', width = 5, height = 5, dpi = 'retina')


# ES type
dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(ES) %>%
  group_by(ES) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers) %>%
  ggplot(aes(x = fct_reorder(ES, proportion), y = proportion, label = round(proportion, digits = 2))) +
  geom_col() +
  #geom_label(hjust = 1) +
  xlab('Ecosystem service type') +
  ylab('Proportion of studies that studied this service type') +
  coord_flip() +
  theme_bw()

ggsave('round2_metareview/analyze_data/final_analyses/fig_files/which_services.pdf', width = 5, height = 5, dpi = 'retina')


##### Biotic drivers


##### Abiotic drivers (human & env)



##### Spatiotemporal scale



##### Multifunctionality



##### 'Analytical approaches' (new name eventually)



















