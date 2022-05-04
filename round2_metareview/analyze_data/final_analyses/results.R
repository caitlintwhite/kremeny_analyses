### Script to produce all final figures and all final results (percents, summaries, etc.)

library(tidyverse) # used throughout
library(ggVennDiagram) # only for drivers venn diagrams
library(PBSmapping) # used for map plot
library(chorddiag) # used for multifunctionality
  #devtools::install_github("mattflor/chorddiag") #github install required for R 4.0.5 on 11 Oct 2021
library(htmlwidgets) # used to make interactive chord diagram
library(webshot) # used to save html chord diagram as pdf
library(cowplot) # used for all panel figures
library(ungeviz) # used for time, connectivity, multiscale plots
library(grid) # used for spatiotemporal panel


##### Read and prep data #####
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
  dplyr::select(-old_group, -clean_answer_binned) %>%
  filter(!is.na(clean_group)) %>% # two NAs snuck through somehow, I checked and there were always other drivers
  mutate(pres_holder = TRUE) %>%
  unique() %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE)

# exclude lulc studies
excl_lulc = driv_types_title %>%
  dplyr::select(-LU_LC) %>%
  filter(!(Biotic=='FALSE' & Human =='FALSE' & Environmental =='FALSE'))
  # 110 studies used an lulc drivers, 26 studies only used lulc drivers

# save updated driver types
dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_answer_binned, clean_group) %>%
  mutate(old_group = clean_group) %>%
  # reassign clean_answer_binned land cover answers to have Group 'LU LC'
  mutate(clean_group = ifelse(clean_answer_binned %in% c('Land cover', 'Land use and land cover change'), 'LU_LC',clean_group)) %>%
  # reassign studies with only land cover biotic drivers (based on Q14) to 'LU LC' Group
  mutate(clean_group = ifelse(Title %in% biot_lulc_titles & clean_group == 'Biotic', 'LU_LC', clean_group)) %>%
  # take only unique rows 
  unique() %>%
  write.csv(file='round2_metareview/analyze_data/final_analyses/lulc_reclass_bytitle.csv', row.names=F)

# save driver types and es types for sankey
dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_answer_binned, clean_group) %>%
  mutate(old_group = clean_group) %>%
  # reassign clean_answer_binned land cover answers to have Group 'LU LC'
  mutate(clean_group = ifelse(clean_answer_binned %in% c('Land cover', 'Land use and land cover change'), 'LU_LC',clean_group)) %>%
  # reassign studies with only land cover biotic drivers (based on Q14) to 'LU LC' Group
  mutate(clean_group = ifelse(Title %in% biot_lulc_titles & clean_group == 'Biotic', 'LU_LC', clean_group)) %>%
  dplyr::select(-clean_answer_binned, -old_group) %>%
  # take only unique rows 
  unique() %>%
  rename(driv_type = clean_group) %>%
  # join with ES types
  full_join(
    dat %>%
      filter(abbr=='Response') %>% 
      filter(!is.na(clean_answer)) %>% 
      dplyr::select(Title, ES) %>%
      unique(),
    by='Title'
  ) %>%
  write.csv(file='round2_metareview/analyze_data/final_analyses/lulcreclass_driv_ES_bytitle.csv', row.names=F)







##### General Patterns #####


### geographic location
loc_counts = dat %>% 
  filter(abbr == 'Place') %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / num_papers) %>%
  add_row(clean_answer='Indian Ocean', count=0, proportion=0)

wm <- map_data("world") %>%
  setNames(c("X","Y","PID","POS","region","subregion")) %>% 
  clipPolys(xlim=c(-180,180), ylim=c(-180,180), keepExtra=T) %>% #clips at boundaries so it can work with map projections
  rename(long = X, lat = Y, group = PID)

# country continents dataframe
cc <- raster::ccodes()

# rename country codes 
wm$region %>% unique %>% setdiff(cc$NAME)
  
mappings <- c("UK"="United Kingdom", "USA"="United States", "Ivory Coast" = "Côte d'Ivoire") # You add the others here
cc$NAME[match(mappings, cc$NAME)] <- names(mappings)

# change Russia to Asia, not Europe
cc[cc$NAME=='Russia',]$continent <- 'Asia'

# set up labels
loc_counts_lab = loc_counts %>%
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
    clean_answer == 'Pacific Ocean' ~ -120,
    clean_answer == 'Indian Ocean' ~ 70
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
    clean_answer == 'Pacific Ocean' ~ 0,
    clean_answer == 'Indian Ocean' ~ -5
  )) 

# join cc to the counts of papers and PLOT
map_plot = cc %>%
  dplyr::select(NAME, continent) %>%
  left_join(loc_counts, by = c('continent' = 'clean_answer')) %>%
  left_join(wm, by = c('NAME' = 'region')) %>%
  ggplot() +
  geom_vline(xintercept = 180, colour = 'lightblue') +
  geom_vline(xintercept = -180, colour = 'lightblue') +
  geom_polygon(aes(x = long, y = lat, group = group, fill = -proportion), show.legend = F) +
  geom_label(data = loc_counts_lab %>% filter(!grepl('Global',clean_answer)), 
             aes(x = label_coords_lon, 
                 y = label_coords_lat, 
                 label = paste0(clean_answer, ':', 100*round(proportion, digits = 2), '%') ),
             size=3) +
  #geom_tile(aes(x = 0, y = 0, width = 360, height = 360), colour = 'lightblue') +
  #geom_abline(slope = Inf, intercept = 180) +
  coord_map("mollweide") + # to change projections
  theme_void()

#ggsave('round2_metareview/analyze_data/final_analyses/fig_files/map_study_locations.pdf', width = 10, height = 5)

# study system
system_plot = dat %>% 
  filter(abbr == 'Ecosystem') %>%
  dplyr::select(Title, clean_answer) %>% 
  separate_rows(clean_answer, sep = ',') %>%
  filter(clean_answer %in% c('Terrestrial','Marine/Off-shore','Coastal','Freshwater')) %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / num_papers) %>%
  ggplot(aes(x = fct_reorder(clean_answer, proportion), y = proportion, label = round(proportion, digits = 2))) +
  geom_col() +
  ggtitle('Study system') +
  xlab('') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw() 

#ggsave('round2_metareview/analyze_data/final_analyses/fig_files/study_system_simple.pdf', width = 5, height = 5, dpi = 'retina')


# methods
mthds_plot = dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep = ',') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / num_papers) %>%
  ggplot(aes(x = fct_reorder(clean_answer, proportion), y = proportion)) +
  geom_col() +
  ggtitle('Methods used') +
  xlab('') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw()

#ggsave('round2_metareview/analyze_data/final_analyses/fig_files/methods_used.pdf', width = 5, height = 5, dpi = 'retina')

# methods venn
methods_tf = dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(Title, clean_answer) %>%
  separate_rows(clean_answer, sep=',') %>%
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = Title, names_from = 'clean_answer', values_from = pres_holder) %>%
  replace(is.na(.), FALSE) %>% 
  dplyr::select(-Title) %>%
  as.matrix()

# methods_list = list(
#   modsim = methods_tf %>% filter(`Model/Data Simulation`==TRUE) %>% pull(Title) %>% as.character(),
#   obs = methods_tf %>% filter(`Observational`==TRUE) %>% pull(Title) %>% as.character(),
#   survey = methods_tf %>% filter(`Social survey/Interview`==TRUE) %>% pull(Title) %>% as.character(),
#   experiment = methods_tf %>% filter(`Experimental`==TRUE) %>% pull(Title) %>% as.character()
# )

#ggVennDiagram(methods_list)
rowSums(methods_tf)
  # no paper used three methods, so the pairwise matrix will tell the whole story
sum(rowSums(methods_tf) == 2)/length(rowSums(methods_tf))
  # 19 % of papers used two methods

methods_adjmat = t(methods_tf) %*% methods_tf
methods_adjmat



# ES type
estype_plot = dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(ES) %>%
  group_by(ES) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers) %>%
  ggplot(aes(x = fct_reorder(ES, proportion), y = proportion, label = round(proportion, digits = 2))) +
  geom_col() +
  #geom_label(hjust = 1) +
  ggtitle('Ecosystem service type') +
  xlab('') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw()

#ggsave('round2_metareview/analyze_data/final_analyses/fig_files/which_services.pdf', width = 5, height = 5, dpi = 'retina')


# Make general patterns panel

ggdraw() +
  draw_plot(map_plot, x = 0, y = 0.5, width = 0.6, height = 0.5) +
  draw_plot(system_plot, x = 0.6, y = 0.5, width = .4, height = 0.5) +
  draw_plot(estype_plot, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(mthds_plot, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0.6, 0, 0.5), y = c(1, 1, 0.5, 0.5))

ggsave(filename='round2_metareview/analyze_data/final_analyses/fig_files/panel_gen_patterns.pdf', width=7.5, height=5, dpi='retina')






##### Biotic drivers #####

# make overall venn diagram
venn_list = list(
  biotic = excl_lulc %>% filter(Biotic == TRUE) %>% pull(Title) %>% as.character(), 
  human = excl_lulc %>% filter(Human == TRUE) %>% pull(Title) %>% as.character(),
  environmental = excl_lulc %>% filter(Environmental == TRUE) %>% pull(Title) %>% as.character()
)

venn_dat = process_data(Venn(venn_list))
region_label <- venn_dat@region %>%
  dplyr::filter(.data$component == "region") %>%
  dplyr::mutate(percent = paste(round(.data$count*100/sum(.data$count),
                                      digits = 0),"%", sep="")) %>%
  dplyr::mutate(both = paste(.data$count,paste0("(",.data$percent,")"),sep = "\n"))


venn_plot = ggplot() +
  geom_sf(aes(fill=count), data = venn_region(venn_dat)) +
  geom_sf(data = venn_setedge(venn_dat), show.legend = F) +
  geom_sf_text(aes(label = c('Biotic drivers','Human drivers','Environmental drivers')), 
               data = venn_setlabel(venn_dat),
               nudge_y = 0.1) +
  geom_sf_label(aes(label=percent),
               data = region_label,
               alpha=0.5,
               color = 'black',
               size = NA,
               lineheight = 0.85) +
  theme_void() +
  scale_fill_distiller(direction = 1) + #change fill colour palette
  scale_color_manual(values = c('black','black','black')) + #outlines of circles
  theme(legend.position = 'None') + #remove legend
  coord_sf(clip="off") + #don't cutoff labels 
  scale_x_continuous(expand = expansion(mult = .3)) 
  # excludes 26 studies that only used land use or land cover drivers



# venn diagrams by service
driv_byserv = dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, ES) %>% 
  unique() %>%
  left_join(excl_lulc, by='Title') %>%
  unique() %>%
  filter(! (is.na(Biotic) & is.na(Human) & is.na(Environmental)))

es_types = unique(driv_byserv$ES)

for (i in 1:length(es_types)){
  es = es_types[i]
  npap = driv_byserv %>% filter(ES==es) %>% pull(Title) %>% unique() %>% length()
  
  venn_list = list(
    biotic = driv_byserv %>% filter(ES == es) %>% filter(Biotic == TRUE) %>% pull(Title) %>% as.character(), 
    human = driv_byserv %>% filter(ES == es) %>% filter(Human == TRUE) %>% pull(Title) %>% as.character(),
    environmental = driv_byserv %>% filter(ES == es) %>% filter(Environmental == TRUE) %>% pull(Title) %>% as.character()
  )
  
  ggVennDiagram(venn_list, category.names = c('Biotic drivers','Human drivers','Environmental drivers'), label = 'percent') +
    scale_fill_distiller(direction = 1) + #change fill colour palette
    scale_color_manual(values = c('black','black','black')) + #outlines of circles
    ggtitle(paste0('ES type: ',es), subtitle = paste0('n papers = ',npap)) +
    theme(legend.position = 'None', plot.title = element_text(hjust =-0.1, face='bold')) + #remove legend
    coord_sf(clip="off")  #don't cutoff labels
  
  ggsave(filename = paste0('round2_metareview/analyze_data/final_analyses/fig_files/drivvenn_byservice/drivvenn_byserv_',es,'.pdf'), width=6, height=5, dpi='retina')
}


# Ecological scale of biotic drivers
ecolscalebiotic_plot = dat %>% 
  filter(qnum=='Q14', abbr=='ESP_type') %>%
  dplyr::select(Title, clean_answer) %>%
  # clean up and separate answers
  mutate(clean_answer = gsub("\\[.*?\\]", '', clean_answer)) %>% 
  separate_rows(clean_answer, sep=',') %>%
  mutate(clean_answer = str_trim(clean_answer, side='right')) %>%
  # bin driver categories
  mutate(binned = case_when(
    clean_answer %in% c('Within species') ~'Within species',
    clean_answer %in% c('Single species') ~'Single species',
    clean_answer %in% c('Multiple ESP species') ~'Multiple ESP species',
    clean_answer %in% c('Among species','Across species') ~'Community level',
    clean_answer %in% c('Only land cover or habitat type as proxy') ~'Land cover proxy'
  )) %>%
  filter(!is.na(binned)) %>%
  dplyr::select(-clean_answer) %>%
  unique() %>%
  group_by(binned) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count)) %>%
  mutate(binned = factor(binned, levels = c('Within species','Single species','Multiple ESP species','Community level','Land cover proxy', NA))) %>%
  filter(!is.na(binned)) %>%
  ggplot() +
  geom_col(aes(x=fct_rev(binned), y=perc)) +
  ggtitle('Levels of biotic drivers') +
  xlab('') +
  ylab('Proportion of papers') +
  coord_flip() +
  theme_bw()
  # checked that the same number of papers in multiple ESP species and community level was just a coincidence - it was

#ggsave(filename='round2_metareview/analyze_data/final_analyses/fig_files/biotic_driv_specific.pdf', width=5, height=5, dpi='retina')

# Biotic drivers panel
ggdraw()+
  draw_plot(venn_plot, x=0, y=0, height=1, width=0.5) +
  draw_plot(ecolscalebiotic_plot, x=0.5, y=0, height=1, width=0.5) +
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0, 0.5), y = c(1, 1))

ggsave(filename = 'round2_metareview/analyze_data/final_analyses/fig_files/panel_bioticdrivers.pdf', width=7.5, height=4, dpi='retina')





### In-text values
# % of studies that considered a biotic driver
excl_lulc %>%
  filter(Biotic==TRUE) %>%
  nrow() / num_papers
  # 42.8% of papers



##### Abiotic drivers (human & env) #####

# see above venn diagrams for human and environmental drivers, including those by es type
excl_lulc %>%
  filter(Human==TRUE) %>%
  nrow() / nrow(excl_lulc)

excl_lulc %>%
  filter(Environmental==TRUE) %>%
  nrow() / nrow(excl_lulc)


##### Spatiotemporal scales #####

### num years
numyears_plot = dat %>%
  filter(abbr=='YrsData') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(clean_answer = as.character(clean_answer)) %>%
  mutate(clean_answer = ifelse(is.na(clean_answer), 'Time not considered', clean_answer)) %>%
  mutate(proportion = count / num_papers) %>%
  mutate(clean_answer = gsub('–', '-', clean_answer)) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('Time not considered', '1 year or less','2-5 years','6-10 years', '10+ years'))) %>%
  ggplot(aes(x = fct_rev(clean_answer), y = proportion)) +
  geom_col() +
  ggtitle('Temporal duration') +
  xlab('') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw()

#ggsave('round2_metareview/analyze_data/final_analyses/fig_files/num_years.pdf', width = 5, height = 5, dpi = 'retina')


### spatial extent
spatextent_plot = dat %>% 
  filter(abbr=='Extent') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>% 
  mutate(proportion = count / num_papers) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('Local', 'Macro-scale','Global','Undefined/no scale'))) %>%
  ggplot(aes(x = fct_rev(clean_answer), y = proportion)) +
  geom_col() +
  ggtitle('Spatial extent') +
  xlab('') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw()

#ggsave('round2_metareview/analyze_data/final_analyses/fig_files/spatial_extent.pdf', width = 5, height = 5, dpi = 'retina')


### duration
# overall props
services_overall = dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(ES) %>%
  group_by(ES) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers)

# duration props
timetrends_df = dat %>%
  filter(abbr=='TimeTrends') %>%
  dplyr::select(Title, TimeTrends = clean_answer) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>% # get rid of the 'e.g' text
  mutate(TimeTrends = ifelse(TimeTrends=='Space for time', 'Yes', TimeTrends)) # make all space for time 'yes'

# expected props
overall_yes_prop = dat %>%
  filter(abbr=='TimeTrends') %>%
  dplyr::select(Title, TimeTrends = clean_answer) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>% # get rid of the 'e.g' text
  mutate(TimeTrends = ifelse(TimeTrends=='Space for time', 'Yes', TimeTrends)) %>%
  dplyr::select(TimeTrends) %>%
  group_by(TimeTrends) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count)) %>%
  filter(TimeTrends=='Yes') %>%
  pull(proportion)


timeestype_plot = dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>%
  dplyr::select(Title, ES) %>%
  left_join(timetrends_df, by = 'Title') %>% 
  group_by(ES, TimeTrends) %>%
  summarise(count_yesno = n()) %>%
  left_join(services_overall, by = 'ES') %>%
  filter(TimeTrends == 'Yes') %>%
  rename(prop_overall = proportion, count_yes = count_yesno) %>%
  mutate(prop_yes = count_yes/num_papers) %>%
  mutate(prop_expected_yes = overall_yes_prop * prop_overall) %>%
  ggplot(aes(x = fct_reorder(ES, prop_overall))) +
  geom_col(aes(y = prop_overall), fill = 'gray') +
  geom_col(aes(y = prop_yes), fill = 'black') +
  geom_hpline(aes(y = prop_expected_yes), colour = 'yellow', width=0.9, size=0.8) +
  xlab('Ecosystem service type') +
  ylab('Proportion of studies') +
  ggtitle('Considered time?') +
  coord_flip() +
  theme_bw()

#ggsave('round2_metareview/analyze_data/final_analyses/fig_files/es_type_temporal.pdf', width = 5, height = 5, dpi = 'retina')
rm(list=c('services_overall','overall_yes_prop'))


### connectivity

# overall props
services_overall = dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(ES) %>%
  group_by(ES) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers)

# specific props
connect_df = dat %>%
  filter(abbr=='Connect') %>%
  dplyr::select(Title, Connectivity = clean_answer)

# expected props
overall_yes_prop = dat %>%
  filter(abbr=='Connect') %>%
  dplyr::select(Title, Connectivity = clean_answer) %>%
  dplyr::select(Connectivity) %>%
  group_by(Connectivity) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count)) %>%
  filter(Connectivity=='Yes') %>%
  pull(proportion)


spatconn_plot = dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>%
  dplyr::select(Title, ES) %>%
  left_join(connect_df, by = 'Title') %>% 
  group_by(ES, Connectivity) %>%
  summarise(count_yesno = n()) %>%
  left_join(services_overall, by = 'ES') %>%
  filter(Connectivity == 'Yes') %>%
  rename(prop_overall = proportion, count_yes = count_yesno) %>%
  mutate(prop_yes = count_yes/num_papers) %>%
  mutate(prop_expected_yes = overall_yes_prop * prop_overall) %>%
  ggplot(aes(x = fct_reorder(ES, prop_overall))) +
  geom_col(aes(y = prop_overall), fill = 'gray') +
  geom_col(aes(y = prop_yes), fill = 'black') +
  geom_hpline(aes(y = prop_expected_yes), colour = 'yellow', width=0.9, size=0.8) +
  xlab('Ecosystem service type') +
  ylab('Proportion of studies') +
  ggtitle("Spatial connectivity?") +
  coord_flip() +
  theme_bw()

#ggsave('round2_metareview/analyze_data/final_analyses/fig_files/es_type_connectivity.pdf', width = 5, height = 5, dpi = 'retina')
rm(list=c('services_overall','overall_yes_prop'))

### multiple spatial scales

# overall props
services_overall = dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(ES) %>%
  group_by(ES) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers)

# specific props
nested_df = dat %>%
  filter(abbr=='Nested') %>%
  dplyr::select(Title, Nested = clean_answer)

# expected props
overall_yes_prop = dat %>%
  filter(abbr=='Nested') %>%
  dplyr::select(Title, Nested = clean_answer) %>%
  dplyr::select(Nested) %>%
  group_by(Nested) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count)) %>%
  filter(Nested=='Yes') %>%
  pull(proportion)


multispat_plot = dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>%
  dplyr::select(Title, ES) %>%
  left_join(nested_df, by = 'Title') %>% 
  group_by(ES, Nested) %>%
  summarise(count_yesno = n()) %>%
  left_join(services_overall, by = 'ES') %>%
  filter(Nested == 'Yes') %>%
  rename(prop_overall = proportion, count_yes = count_yesno) %>%
  mutate(prop_yes = count_yes/num_papers) %>%
  mutate(prop_expected_yes = overall_yes_prop * prop_overall) %>%
  ggplot(aes(x = fct_reorder(ES, prop_overall))) +
  geom_col(aes(y = prop_overall), fill = 'gray') +
  geom_col(aes(y = prop_yes), fill = 'black') +
  geom_hpline(aes(y = prop_expected_yes), colour = 'yellow', width=0.9, size=0.8) +
  xlab('Ecosystem service type') +
  ylab('Proportion of studies') +
  ggtitle('Multiple spatial scales?') +
  coord_flip() +
  theme_bw()

#ggsave('round2_metareview/analyze_data/final_analyses/fig_files/es_type_multiscale.pdf', width = 5, height = 5, dpi = 'retina')
rm(list=c('services_overall','overall_yes_prop'))



# Make spatiotemporal panel

# without description of C,D,E
ggdraw() +
  draw_plot(numyears_plot, x=0, y=0.5, width=0.5, height=0.5) +
  draw_plot(spatextent_plot, x=0.5, y=0.5, width=0.5, height=0.5) +
  draw_plot(timeestype_plot, x=0, y=0, width=0.333, height=0.5) +
  draw_plot(spatconn_plot, x=0.333, y=0, width=0.333, height=0.5) +
  draw_plot(multispat_plot, x=0.666, y=0, width=0.333, height=0.5) +
  draw_plot_label(label = c("A", "B", "C", "D", "E"), size = 15,
                  x = c(0, 0.5, 0, 0.333, 0.666), y = c(1, 1, 0.5, 0.5, 0.5)) 
  
ggsave(filename = 'round2_metareview/analyze_data/final_analyses/fig_files/panel_spatiotemp_simp.pdf', width=9, height=6, dpi='retina')

# with description of C,D,E
ggdraw() +
  draw_plot(numyears_plot, x=0, y=0.5, width=0.333, height=0.5) +
  draw_plot(spatextent_plot, x=0.333, y=0.5, width=0.333, height=0.5) +
  draw_plot(timeestype_plot, x=0, y=0, width=0.333, height=0.5) +
  draw_plot(spatconn_plot, x=0.333, y=0, width=0.333, height=0.5) +
  draw_plot(multispat_plot, x=0.666, y=0, width=0.333, height=0.5) +
  draw_plot_label(label = c("A", "B", "C*", "D*", "E*"), size = 15,
                  x = c(0, 0.333, 0, 0.333, 0.666), y = c(1, 1, 0.5, 0.5, 0.5)) +
  draw_grob(grob = rectGrob(width=0.3, height=0.37), 
            x =  .34, 
            y =  .23, hjust=0, vjust=0) +
  draw_text('*C, D, E:', x=0.7, y=0.89, size=14, hjust=0, fontface='bold') +
  draw_text(text=str_wrap('Black bars: proportion that considered time (C), spatial connectivity (D), and multiple spatial scales (E)', width=42),
            x=0.7, y=0.85, size=10, hjust=0, vjust=1) +
  draw_text(text=str_wrap('Gray bars: overall proportion of studies by ecosystem service type', width=42),
            x=0.7, y=0.73, size=10, hjust=0, vjust=1) +
  draw_text(text=str_wrap('Yellow lines: expected proportion by ecosystem service type based on overall average', width=42),
            x=0.7, y=0.65, size=10, hjust=0, vjust=1) 
  
ggsave(filename='round2_metareview/analyze_data/final_analyses/fig_files/panel_spatiotemp.pdf', width=9, height=6, dpi='retina')



### intersections with methods
# see `extent_and_methods.R`, to be included in SI

### space & time intersections
# Note to make heatmap figure, but to make sure connectivity gets included with multiple spatial scales




##### Multifunctionality #####

### number of ES types per paper
dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'ES', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>%
  mutate(num_ES_types = rowSums(.[-1])) %>%
  dplyr::select(Title, num_ES_types) %>% 
  group_by(num_ES_types) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/ sum(count)) %>%
  ggplot() +
  geom_col(aes(x=fct_rev(factor(num_ES_types)), y=proportion)) +
  xlab('Number of ES types') +
  ylab('Proportion of papers') +
  theme_bw() +
  coord_flip()

# view the table
dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'ES', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>%
  mutate(num_ES_types = rowSums(.[-1])) %>%
  dplyr::select(Title, num_ES_types) %>% 
  group_by(num_ES_types) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/ sum(count))

#ggsave(filename='round2_metareview/analyze_data/final_analyses/fig_files/numestype_perpaper.pdf', width=5, height=5, dpi='retina')

# pull titles that had 8+ service types
dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'ES', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>%
  mutate(num_ES_types = rowSums(.[-1])) %>%
  filter(num_ES_types > 7) %>%
  dplyr::select(Title, num_ES_types)



### chord diagram of multifunctionality (two service types)
site_by_es = dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>% 
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = 'Title', names_from = 'ES', values_from = 'pres_holder') %>% 
  replace(is.na(.), FALSE) %>%
  dplyr::select(-Title) %>%
  as.matrix()

adj_mat = t(site_by_es) %*% site_by_es   # cross product translates from site by ES to cooccurrences of ES's
diag(adj_mat) = 0 # make diagonal zero
adj_mat = replace(adj_mat, adj_mat < 0, 0) # can be used to exclude rare connections
p_int = chorddiag(adj_mat, 
                  groupnameFontsize = 14, 
                  showTicks = F, # show ticks with counts on circle edge
                  groupPadding = 5, # distance of names/segments from each other
                  groupnamePadding = 5 # distance of names from outside of circles
                  )
  # this plot is interactive and can be saved as an interactive html

# save the interactive version
withr::with_dir('round2_metareview/analyze_data/final_analyses/fig_files/', saveWidget(p_int, file="chord_diag_interactive.html"))

# static version (excludes rare connections for better viewing)
adj_mat_stat = replace(adj_mat, adj_mat < 5, 0) # can be used to exclude rare connections
p_static = chorddiag(adj_mat_stat, 
                     groupnameFontsize = 14, 
                     showTicks = F, # show ticks with counts on circle edge
                     groupPadding = 5, # distance of names/segments from each other
                     groupnamePadding = 5 # distance of names from outside of circles
)
# save static
withr::with_dir('round2_metareview/analyze_data/final_analyses/fig_files/', saveWidget(p_static, file="chord_diag_static.html"))

# webshot to pdf
webshot("round2_metareview/analyze_data/final_analyses/fig_files/chord_diag_static.html" , "round2_metareview/analyze_data/final_analyses/fig_files/chord_diag_static.pdf", delay = 0.2)


### Multifunctionality and time scales

# proportions within each single ES type
dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>%
  left_join(
    dat %>%
      filter(abbr=='YrsData') %>%
      mutate(clean_answer = ifelse(is.na(clean_answer), 'Time not considered', clean_answer)) %>%
      dplyr::select(Title, clean_answer),
    by='Title'
  ) %>% 
  group_by(ES, clean_answer) %>%
  summarise(count=n()) %>%
  mutate(proportion_within_es = count / sum(count)) %>%
  mutate(clean_answer = gsub('–', '-', clean_answer)) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('Time not considered', '1 year or less','2-5 years','6-10 years', '10+ years'))) %>%
  ggplot(aes(x = fct_rev(clean_answer), y = proportion_within_es)) +
  geom_col() +
  facet_wrap(.~ES) +
  ylab('Proportion of studies \n(within ecosystem service type)') +
  xlab('') +
  ggtitle('Study duration by ecosystem service type') +
  coord_flip() +
  theme_bw()

# counts instead, within each single ES type
dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>%
  left_join(
    dat %>%
      filter(abbr=='YrsData') %>%
      mutate(clean_answer = ifelse(is.na(clean_answer), 'Time not considered', clean_answer)) %>%
      dplyr::select(Title, clean_answer),
    by='Title'
  ) %>% 
  group_by(ES, clean_answer) %>%
  summarise(count=n()) %>%
  mutate(proportion_within_es = count / sum(count)) %>%
  mutate(clean_answer = gsub('–', '-', clean_answer)) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('Time not considered', '1 year or less','2-5 years','6-10 years', '10+ years'))) %>%
  ggplot(aes(x = fct_rev(clean_answer), y = count)) +
  geom_col() +
  facet_wrap(.~ES) +
  ylab('Number of studies') +
  xlab('') +
  ggtitle('Study duration by ecosystem service type') +
  coord_flip() +
  theme_bw()


dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>%
  left_join(
    dat %>%
      filter(abbr=='YrsData') %>%
      mutate(clean_answer = ifelse(is.na(clean_answer), 'Time not considered', clean_answer)) %>%
      dplyr::select(Title, clean_answer),
    by='Title'
  ) %>% 
  ungroup() %>%
  group_by(Title) %>%
  summarise(num_ES = length(unique(ES)), duration = clean_answer) %>%
  unique() %>%
  group_by(num_ES, duration) %>%
  summarise(count=n()) %>% 
  mutate(proportion_within_esnum = count / sum(count)) %>%
  mutate(duration = gsub('–', '-', duration)) %>%
  mutate(duration = factor(duration, levels = c('Time not considered', '1 year or less','2-5 years','6-10 years', '10+ years'))) %>%
  ggplot(aes(x=duration, y=count)) +
  geom_col() +
  facet_wrap(.~num_ES) +
  coord_flip() +
  theme_bw()

    # Not sure what we want from this...will need to ask what Laura had in mind



##### Dynamics, non-linearities, uncertainty, and thresholds #####



### Feedbacks
dat %>% 
  filter(qnum=='Q15') %>%
  dplyr::select(Title, clean_answer) %>% 
  # one conflicting answer, set as no feedbacks measured
  mutate(clean_answer = ifelse(clean_answer=='Ecosystem function -> service providers,No feedbacks measured directly', 'No feedbacks measured directly', clean_answer)) %>%
  separate_rows(clean_answer, sep = ',') %>%
  group_by(clean_answer) %>%
  summarise(count = n())

# feedbacks or not
dat %>% 
  filter(qnum=='Q15') %>%
  dplyr::select(Title, clean_answer) %>% 
  # one conflicting answer, set as no feedbacks measured
  mutate(clean_answer = ifelse(clean_answer=='Ecosystem function -> service providers,No feedbacks measured directly', 'No feedbacks measured directly', clean_answer)) %>%
  mutate(clean_answer = ifelse(clean_answer=='No feedbacks measured directly',clean_answer,'Feedback measured')) %>% 
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count))

# mention 2 studies considered multiple feedbacks
dat %>% 
  filter(qnum=='Q15') %>%
  dplyr::select(Title, clean_answer) %>% 
  # one conflicting answer, set as no feedbacks measured
  mutate(clean_answer = ifelse(clean_answer=='Ecosystem function -> service providers,No feedbacks measured directly', 'No feedbacks measured directly', clean_answer)) %>%
  group_by(clean_answer) %>%
  summarise(count = n())


# feedbacks plot
dat %>% 
  filter(qnum=='Q15') %>%
  dplyr::select(Title, clean_answer) %>% 
  # one conflicting answer, set as no feedbacks measured
  mutate(clean_answer = ifelse(clean_answer=='Ecosystem function -> service providers,No feedbacks measured directly', 'No feedbacks measured directly', clean_answer)) %>%
  mutate(clean_answer = ifelse(clean_answer=='No feedbacks measured directly',clean_answer,'Feedbacks measured')) %>% 
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count)) %>%
  ggplot() +
  geom_col(aes(x=clean_answer, y=proportion)) +
  xlab('') +
  ylab('Proportion of studies') +
  ylim(c(0,1)) +
  theme_bw()

ggsave(filename='round2_metareview/analyze_data/final_analyses/fig_files/feedbacks.pdf', width=5, height=5, dpi='retina')


# pulling list of titles that considered feedbacks
dat %>% 
  filter(qnum=='Q15') %>%
  dplyr::select(Title, clean_answer) %>% 
  # one conflicting answer, set as no feedbacks measured
  mutate(clean_answer = ifelse(clean_answer=='Ecosystem function -> service providers,No feedbacks measured directly', 'No feedbacks measured directly', clean_answer)) %>%
  mutate(clean_answer = ifelse(clean_answer=='No feedbacks measured directly',clean_answer,'Feedbacks measured')) %>%
  filter(clean_answer != 'No feedbacks measured directly')
  

### Thresholds
dat %>%
  filter(qnum=='Q16') %>%
  dplyr::select(Title, clean_answer) %>% 
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count))


# thresholds plot
dat %>%
  filter(qnum=='Q16') %>%
  dplyr::select(Title, clean_answer) %>% 
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count)) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('Yes','Mentioned or discussed but not measured','No'))) %>%
  ggplot() +
  geom_col(aes(x=clean_answer, y=proportion)) +
  xlab('Thresholds considered?') +
  ylab('Proportion of studies') +
  ylim(c(0,1)) +
  theme_bw()

ggsave(filename='round2_metareview/analyze_data/final_analyses/fig_files/thresholds.pdf', width=5, height=5, dpi='retina')


### Uncertainty
# Only present in survey as an open ended question with short answer format.
# Here are the notes people left (22 papers had something noted, could go
# through in more detail).
dat %>%
  filter(qnum=='Q17') %>%
  dplyr::select(Title, clean_answer) %>%
  filter(!is.na(clean_answer)) %>%
  View()














