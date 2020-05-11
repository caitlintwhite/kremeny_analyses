library(tidyverse)

df = read.csv("round2_metareview/data/cleaned/prelim_singlereview.csv")


### Binning ###
spat = df %>% 
  filter(qnum %in% c("Q8","Q9","Q10","Q11"))

smalls = c('25m','50m','100m','500m', '1km')
mediums = c('10km', '100km', '1000km')
larges = c('100Mgm','100Gm','101Gm')

plot_bins = spat %>% 
  filter(qnum == 'Q9', !abbr=='ScaleNotes', !is.na(answer), abbr=='Plots') %>% 
  mutate(plots_binned = case_when(
    Group %in% smalls ~ 'Small',
    Group %in% mediums ~ 'Medium',
    Group %in% larges ~ 'Large',
    Group == 'unk' ~ 'unk'
  )) %>% 
  select(Title, abbr, Group, answer, plots_binned) %>%
  #run above this to see specific answers for each group
  filter(answer!=0) %>%
  group_by(Title) %>%
  summarise(plots_binned = paste0(unique(plots_binned), collapse = ','))
  
  
site_bins = spat %>% 
  filter(qnum == 'Q9', !abbr=='ScaleNotes', !is.na(answer), abbr=='Sites') %>% 
  mutate(sites_binned = case_when(
    Group %in% smalls ~ 'Small',
    Group %in% mediums ~ 'Medium',
    Group %in% larges ~ 'Large',
    Group == 'unk' ~ 'unk'
  )) %>% 
  select(Title, abbr, Group, answer, sites_binned) %>%  
  #run above this to see specific answers for each group
  filter(answer!=0) %>%
  group_by(Title) %>%
  summarise(sites_binned = paste0(unique(sites_binned), collapse = ','))
  
bins_unks = plot_bins %>%
  left_join(site_bins, by = 'Title') %>%
  mutate(contains_unk_plots = ifelse(grepl('unk', plots_binned),'Plots','')) %>%
  mutate(contains_unk_sites = ifelse(grepl('unk', sites_binned),'Sites','')) %>%
  mutate(contains_unk = paste0(contains_unk_plots, contains_unk_sites)) %>%
  mutate(contains_unk = ifelse(contains_unk == 'PlotsSites', 'Both', contains_unk)) %>%
  mutate(contains_unk = na_if(contains_unk, '')) %>%
  mutate(only_unk = ifelse(plots_binned == 'unk' & sites_binned == 'unk', 'Only unk', NA)) %>%
  select(-contains_unk_sites, -contains_unk_plots)




### Zeros ###
plots_zeros = spat %>%
  filter(qnum=='Q9', !is.na(answer), !abbr=='ScaleNotes', abbr=='Plots') %>%
  group_by(Title) %>%
  summarise(unique_nums = paste0(unique(answer), collapse = ',')) %>%
  mutate(only_0_entered = ifelse(unique_nums == 0, 'Plots',NA))

sites_zeros = spat %>%
  filter(qnum=='Q9', !is.na(answer), !abbr=='ScaleNotes', abbr=='Sites') %>%
  group_by(Title) %>%
  summarise(unique_nums = paste0(unique(answer), collapse = ',')) %>%
  mutate(only_0_entered = ifelse(unique_nums == 0, 'Sites',NA))

all_zeros = plots_zeros %>%
  left_join(sites_zeros, by = 'Title') %>%
  mutate(only_zeros = paste0(only_0_entered.x, only_0_entered.y)) %>% 
  mutate(only_zeros = gsub('NA', '', only_zeros)) %>% 
  mutate(only_zeros = ifelse(only_zeros=='PlotsSites', 'Both', only_zeros)) %>%
  mutate(only_zeros = na_if(only_zeros, '')) %>%
  select(Title, only_zeros) 


### Missings ###
missings_plots_sites = spat %>%
  filter(qnum=='Q9', !is.na(answer), !abbr=='ScaleNotes') %>%
  group_by(Title) %>%
  summarise(abbrs_non_na = paste0(unique(abbr), collapse = ',')) %>%
  mutate(missing_entry = ifelse(abbrs_non_na=='Plots', 'Sites', ifelse(abbrs_non_na=='Sites','Plots',NA))) %>%
  mutate(missing_entry = ifelse(is.na(missing_entry), NA, missing_entry)) %>%
  select(Title, missing_entry)



### Multiscale question ###

multiscale_quest = spat %>% 
  filter(qnum == "Q8") %>%
  select(Title, answer) %>% 
  rename('multiscale_quest' = answer)

# bins multiscale

crosses = c('Small,Medium','Small,Large','Small,Medium,Large', 'Medium,Large')

bins_multiscale = bins_unks %>%
  select(-contains_unk, -only_unk) %>%
  #get rid of unk as long as there's something else there too, just for this step
  mutate(plots_binned = gsub(',unk','', plots_binned),
         sites_binned = gsub(',unk','', sites_binned)) %>%
  mutate(plots_binned = gsub('unk,','', plots_binned),
         sites_binned = gsub('unk,','', sites_binned)) %>%
  mutate(bins_multi = case_when(
    plots_binned == 'Small' & (sites_binned == 'Medium' | sites_binned == 'Large') ~ 'Yes',
    plots_binned == 'Medium' & (sites_binned == 'Small' | sites_binned == 'Large') ~ 'Yes',
    plots_binned == 'Large' & (sites_binned == 'Small' | sites_binned == 'Medium') ~ 'Yes', #this shouldn't happen but just in case
    plots_binned %in% crosses ~ 'Yes',
    sites_binned %in% crosses ~ 'Yes'
  )) %>% 
  mutate(bins_multi = replace_na(bins_multi, 'No')) %>%
  select(Title, bins_multi)

#mismatches
mismatches = bins_unks %>%
  left_join(bins_multiscale, by = 'Title') %>%
  left_join(multiscale_quest, by = 'Title') %>% 
  select(Title, bins_multi, multiscale_quest) %>% 
  mutate(multiscale_mismatch = ifelse(bins_multi==multiscale_quest, 'No','Yes')) %>% 
  select(Title, multiscale_mismatch) 





### Merging ###

df %>% select(Title) %>% unique() %>% nrow() # number of unique titles included at start


flagged_df_all = df %>% select(Title) %>% unique() %>%
  left_join(bins_multiscale, by = 'Title') %>%
  left_join(bins_unks, by = 'Title') %>%
  left_join(multiscale_quest, by = 'Title') %>%
  left_join(all_zeros, by = 'Title') %>%
  left_join(missings_plots_sites, by = 'Title') %>%
  left_join(mismatches, by = 'Title')
  


non_flagged_titles = flagged_df_all %>%
  filter(is.na(contains_unk), is.na(only_unk), is.na(only_zeros), is.na(missing_entry), is.na(multiscale_mismatch) | multiscale_mismatch == 'No') %>% 
  pull(Title) %>% as.character()

need_look = flagged_df_all %>%
  filter(!Title %in% non_flagged_titles) 

need_look %>% nrow() #number that need to be looked through

#as of 11 May 2020: 252/390 = 65% of papers need to be looked through again for at least one of the criteria


