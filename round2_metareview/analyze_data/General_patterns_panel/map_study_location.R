library(tidyverse)
library(PBSmapping)
# relied on accepted answer on: 
# https://stackoverflow.com/questions/43801030/filling-countries-and-continents-with-maps-library-according-to-variable-value

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

num_papers = dat %>% 
  pull(Title) %>%
  unique() %>%
  length()

loc_counts = dat %>% 
  filter(abbr == 'Place') %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / num_papers)



## MAPPING
wm <- map_data("world") %>%
  setNames(c("X","Y","PID","POS","region","subregion")) %>% 
  clipPolys(xlim=c(-180,180), ylim=c(-180,180), keepExtra=T) %>% #clips at boundaries so it can work with map projections
  rename(long = X, lat = Y, group = PID)



# country continents dataframe
cc <- raster::ccodes()

## RENAME - some country names don't match between the two databases, 
# I only corrected those that were visible on the map 
wm$region %>% unique %>% setdiff(cc$NAME)

mappings <- c("UK"="United Kingdom", "USA"="United States", "Ivory Coast" = "Côte d'Ivoire") # You add the others here
cc$NAME[match(mappings, cc$NAME)] <- names(mappings)

# change Russia to Asia, not Europe (looks weird with all of it considered Europe)
cc[cc$NAME=='Russia',]$continent <- 'Asia'



## Join
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



#join cc to the counts of papers and PLOT
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

ggsave('round2_metareview/analyze_data/General_patterns_panel/fig_files/map_study_locations.pdf', width = 10, height = 5)

