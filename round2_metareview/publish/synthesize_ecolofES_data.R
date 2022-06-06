# Synthesize Ecology of Ecosystem Services quasi-systematic map extracted data and excluded papers
## Last updated: 2022-May-09


# SCRIPT PURPOSE:
## Produce all figures and summary results (percents, summaries, etc.) for narrative synthesis
## Summarize journal distribution at each review step (search, abstract screen, full text review) and reviewer consistency


# NOTES:
# >> Code credit:
# * Grant Vagle (grant.vagle@colorado.edu) wrote all code to synthesize general patterns through dynamics, non-linearities and thresholds
# * Aislyn Keyes (aislyn.keyes@colordao.edu) wrote code to produce sankey diagram
# * Caitlin T. White (caitlin.t.white@colorado.edu) prepared and quality assured the data, wrote EML metadata, and wrote code to summarize metadata and reviewer consistency
## contact Laura Dee (laura.dee@colorado.edu) or code authors with any questions

# >> Session info:
# R version 4.1.3 (2022-03-10)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Big Sur 11.6.4
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
# [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] rstudioapi_0.13      ggfittext_0.9.1     networkD3_0.4       magick_2.7.3        ungeviz_0.1.0      
# [6] cowplot_1.1.1        webshot_0.5.2       htmlwidgets_1.5.4   chorddiag_0.1.3     PBSmapping_2.73.0  
# [11] ggVennDiagram_1.2.0 forcats_0.5.1       stringr_1.4.0       dplyr_1.0.9         purrr_0.3.4        
# [16] readr_2.1.2         tidyr_1.2.0         tibble_3.1.7        ggplot2_3.3.6       tidyverse_1.3.1 




#### SCRIPT SETUP ===============================================

# libraries needed for analyses
library(tidyverse) # used throughout
library(ggVennDiagram) # only for drivers venn diagrams
library(PBSmapping) # used for map plot
library(chorddiag) # used for multifunctionality
#devtools::install_github("mattflor/chorddiag") #github install required for R 4.0.5 on 11 Oct 2021
library(htmlwidgets) # used to make interactive chord diagram
library(webshot) # used to save html chord diagram as pdf
library(cowplot) # used for all panel figures
library(ungeviz) # used for time, connectivity, multiscale plots
#devtools::install_github("wilkelab/ungeviz") #github install for development version
library(grid) # used for spatiotemporal panel
library(magick) # used for images in panel figure (temp dyn, thresholds, feedbacks)
library(networkD3) # used to make sankey alluvial diagram
library(ggfittext) # used for sankey diagram labels
library(rstudioapi) # used to set path directory for reading in data


##### Read and prep data #####

# specify current path
home <- rstudioapi::getSourceEditorContext()$path # path to current script
home <- gsub("synthesize.*R$", "", home) # up one level (download folder where data live)

# read in full text extracted data
dat <- read.csv(paste0(home, "ecolofES_extracted_data.csv"))
# subset individual answers to double reviewed papers for metadata summaries
doublerev <- filter(dat, version == 'individual')
# subset dat object to final answers used for analyses
dat <- filter(dat, version == 'final')

# read in excluded papers 
# > this includes papers not selected for full text review by random chance (passed abstract sceen)
excluded <- read.csv(paste0(home, "ecolofES_excludedpapers.csv"))
distinct(excluded, exclusion_id, exclusion_reason) # exclusion_id == 9 are papers not random selected for full text review

# specify number of papers retained
num_papers = dat %>% 
  pull(title) %>%
  unique() %>%
  length()


### Re-classify land use and land cover drivers studies

# find the land use land cover studies
dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(title, clean_answer_binned) %>% 
  pull(clean_answer_binned) %>% 
  unique() 

# 'Land cover' and 'Land use and land cover change' are the two bins that should get their own categories


# Biotic drivers papers that only looked at land cover or habitat type as a proxy
biot_lulc_titles = dat %>%
  filter(qnum=='Q14', abbr=='ESP_type', clean_answer == 'Only land cover or habitat type as proxy') %>%
  pull(title)


# separate out land use and land cover studies
driv_types_title = dat %>%
  # subset to all non-NA driver answers except those == "Other" (signals Other Driver answers present, not meaningful answer)
  subset(grepl("Driver", abbr) & !clean_answer_binned %in% c(NA, "Other"), 
         # select only study title, and LU_LC adjusted driver groups present in study
         select = c(title,  lulc_group)) %>% #clean_answer_binned,
  # take only unique rows 
  unique() %>%
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = 'title', names_from = 'lulc_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE)


# create driver analysis dataset that excludes lulc-only studies
excl_lulc = driv_types_title %>%
  dplyr::select(-LU_LC) %>% # this drops any variables recoded as lulc for papers retained (in venn diagrams below we're ignoring any variables recoded as LU_LC) 
  filter(!(Biotic=='FALSE' & Human =='FALSE' & Environmental =='FALSE'))
summary(driv_types_title)
nrow(excl_lulc)
# 113 studies used an lulc driver, 29 studies only used lulc drivers

# ctw note: another way to get excl_lulc: use T/F column 'only_lulc', which indicates studies that only involved LULC drivers based on:
# 1) land use or land cover binned driver variables in Q12
# 2) or indicated study ESP or other biotic variables (when only other biotic driver variables provided) were land cover proxies in Q14
excl_lulc_titles <- with(dat, unique(title[only_lulc]))

# create folder for storing figures
figpath <- paste0(home, "fig_files/")
if(!dir.exists(figpath)){
  dir.create(figpath)
}
# uncomment any ggsave(...) code below to save figures



#### NARRATIVE SYNTHESIS ===============================================


##### General Patterns #####

### geographic location
loc_counts = dat %>% 
  filter(abbr == 'Place') %>%
  dplyr::select(title, clean_answer) %>%
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

#ggsave(paste0(figpath,'map_study_locations.pdf'), width = 10, height = 5)

# study system
system_plot = dat %>% 
  filter(abbr == 'Ecosystem') %>%
  dplyr::select(title, clean_answer) %>% 
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

#ggsave(paste0(figpath, 'study_system_simple.pdf'), width = 5, height = 5, dpi = 'retina')


# methods
mthds_plot = dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(title, clean_answer) %>%
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

#ggsave(paste0(figpath, 'methods_used.pdf'), width = 5, height = 5, dpi = 'retina')

# methods venn
methods_tf = dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(title, clean_answer) %>%
  separate_rows(clean_answer, sep=',') %>%
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = title, names_from = 'clean_answer', values_from = pres_holder) %>%
  replace(is.na(.), FALSE) %>% 
  dplyr::select(-title) %>%
  as.matrix()

# methods_list = list(
#   modsim = methods_tf %>% filter(`Model/Data Simulation`==TRUE) %>% pull(title) %>% as.character(),
#   obs = methods_tf %>% filter(`Observational`==TRUE) %>% pull(title) %>% as.character(),
#   survey = methods_tf %>% filter(`Social survey/Interview`==TRUE) %>% pull(title) %>% as.character(),
#   experiment = methods_tf %>% filter(`Experimental`==TRUE) %>% pull(title) %>% as.character()
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
  filter(abbr == "Response") %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(title, es) %>%
  # take unique ESes per study
  unique() %>%
  group_by(es) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers) %>%
  ggplot(aes(x = fct_reorder(es, proportion), y = proportion, label = round(proportion, digits = 2))) +
  geom_col() +
  #geom_label(hjust = 1) +
  ggtitle('Ecosystem service type') +
  xlab('') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw()


# Make general patterns panel

ggdraw() +
  draw_plot(map_plot, x = 0, y = 0.5, width = 0.6, height = 0.5) +
  draw_plot(system_plot, x = 0.6, y = 0.5, width = .4, height = 0.5) +
  draw_plot(estype_plot, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(mthds_plot, x = 0.5, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("A", "B", "C", "D"), size = 15,
                  x = c(0, 0.6, 0, 0.5), y = c(1, 1, 0.5, 0.5))

#ggsave(filename=paste0(figpath, 'panel_gen_patterns.pdf'), width=7.5, height=5, dpi='retina')






##### Biotic drivers #####

# make overall venn diagram
venn_list = list(
  biotic = excl_lulc %>% filter(Biotic == TRUE) %>% pull(title) %>% as.character(), 
  human = excl_lulc %>% filter(Human == TRUE) %>% pull(title) %>% as.character(),
  environmental = excl_lulc %>% filter(Environmental == TRUE) %>% pull(title) %>% as.character()
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
# excludes 29 studies that only used land use or land cover drivers



# venn diagrams by service
driv_byserv = dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !clean_answer %in% c(NA, "Other")) %>% # ignore non-answers or "Other" answers that signal Other Drivers present
  dplyr::select(title, es) %>% 
  unique() %>%
  left_join(excl_lulc, by='title') %>%
  unique() %>%
  filter(! (is.na(Biotic) & is.na(Human) & is.na(Environmental)))

es_types = unique(driv_byserv$es)

for (i in 1:length(es_types)){
  es = es_types[i]
  npap = driv_byserv %>% filter(es==es) %>% pull(title) %>% unique() %>% length()
  
  venn_list = list(
    biotic = driv_byserv %>% filter(es == es) %>% filter(Biotic == TRUE) %>% pull(title) %>% as.character(), 
    human = driv_byserv %>% filter(es == es) %>% filter(Human == TRUE) %>% pull(title) %>% as.character(),
    environmental = driv_byserv %>% filter(es == es) %>% filter(Environmental == TRUE) %>% pull(title) %>% as.character()
  )
  
  ggVennDiagram(venn_list, category.names = c('Biotic drivers','Human drivers','Environmental drivers'), label = 'percent') +
    scale_fill_distiller(direction = 1) + #change fill colour palette
    scale_color_manual(values = c('black','black','black')) + #outlines of circles
    ggtitle(paste0('ES type: ',es), subtitle = paste0('n papers = ',npap)) +
    theme(legend.position = 'None', plot.title = element_text(hjust =-0.1, face='bold')) + #remove legend
    coord_sf(clip="off")  #don't cutoff labels
  
  # create venn diagram folder for storing venn figures
  if(!dir.exists(paste0(figpath,"drivvenn_byservice/"))){
    dir.create(paste0(figpath,"drivvenn_byservice/"))
  }
  
  #ggsave(filename = paste0(figpath, 'drivvenn_byservice/drivvenn_byserv_',es,'.pdf'), width=6, height=5, dpi='retina')
}


# > ID papers that have biotic drivers based on original clean group, but excluding papers that were lulc-drivers only 
# >> (e.g., Service Provider or other biotic drivers recoded to LULC based on Q14 proxy answer are still Biotic this way)
biot_driver_titles <- with(dat, unique(title[grepl("Biotic", clean_group) & !only_lulc])) # 133 papers

# ecological scale plot for studies that reported a biotic driver (rather than ESP as response variable)
ecolscalebiotic_plot = dat %>% 
  # select only papers that have a biotic driver indicated
  filter(title %in% biot_driver_titles) %>%
  filter(qnum=='Q14', abbr=='ESP_type') %>%
  dplyr::select(title, clean_answer) %>%
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
  # ctw adds paper count
  mutate(n_studies = length(unique(title))) %>%
  # preserve total # studies that had biotic driver to calculate proportions
  group_by(binned, n_studies) %>%
  #group_by(binned) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(perc = count/sum(count), # proportion of total answers checked (i.e., reviewer could check multiple answers per paper)
         prop_studies = count/n_studies) %>%  # ctw adds: calculate proportion of papers with a given ESP type
  mutate(binned = factor(binned, levels = c('Within species','Single species','Multiple ESP species','Community level','Land cover proxy', NA))) %>%
  filter(!is.na(binned)) %>%
  ggplot() +
  geom_col(aes(x=fct_rev(binned), y=prop_studies)) + # ctw: use proportion of studies rather than proportion of checked answers instead
  ggtitle('Levels of biotic drivers') +
  xlab('') +
  ylab('Proportion of papers') +
  coord_flip() +
  theme_bw()

#ggsave(filename=paste0(figpath,'biotic_driv_specific.pdf'), width=5, height=5, dpi='retina')

# Biotic drivers panel
ggdraw()+
  draw_plot(venn_plot, x=0, y=0, height=1, width=0.5) +
  draw_plot(ecolscalebiotic_plot, x=0.5, y=0, height=1, width=0.5) +
  draw_plot_label(label = c("A", "B"), size = 15,
                  x = c(0, 0.5), y = c(1, 1))

#ggsave(filename = paste0(figpath,'panel_bioticdrivers.pdf'), width=7.5, height=4, dpi='retina')





### In-text values
# % of studies that considered a biotic driver
excl_lulc %>%
  filter(Biotic==TRUE) %>%
  nrow() / num_papers
# 44.7% of papers



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

#ggsave(paste0(figpath,'num_years.pdf'), width = 5, height = 5, dpi = 'retina')


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

#ggsave(paste0(figpath,'spatial_extent.pdf'), width = 5, height = 5, dpi = 'retina')


### time trends yes/no
# overall props
services_overall = dat %>%
  filter(abbr=='Response') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(title, es) %>%
  # unique ESes studies per paper
  unique() %>%
  group_by(es) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers)

# timetrends props
timetrends_df = dat %>%
  filter(abbr=='TimeTrends') %>%
  dplyr::select(title, TimeTrends = clean_answer) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>% # get rid of the 'e.g' text
  mutate(TimeTrends = ifelse(TimeTrends=='Space for time', 'Yes', TimeTrends)) # make all space for time 'yes'

# expected props
overall_yes_prop = dat %>%
  filter(abbr=='TimeTrends') %>%
  dplyr::select(title, TimeTrends = clean_answer) %>%
  mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>% # get rid of the 'e.g' text
  mutate(TimeTrends = ifelse(TimeTrends=='Space for time', 'Yes', TimeTrends)) %>%
  dplyr::select(TimeTrends) %>%
  group_by(TimeTrends) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count)) %>%
  filter(TimeTrends=='Yes') %>%
  pull(proportion)


timeestype_plot = dat %>%
  filter(abbr=='Response') %>%
  filter(!is.na(clean_answer)) %>%
  dplyr::select(title, es) %>%
  # because multiple Responses can be recorded per ES (tidy data), take unique title-ES rows
  unique() %>%
  left_join(timetrends_df, by = 'title') %>% 
  group_by(es, TimeTrends) %>%
  summarise(count_yesno = n()) %>%
  left_join(services_overall, by = 'es') %>%
  filter(TimeTrends == 'Yes') %>%
  rename(prop_overall = proportion, count_yes = count_yesno) %>%
  mutate(prop_yes = count_yes/num_papers) %>%
  mutate(prop_expected_yes = overall_yes_prop * prop_overall) %>%
  ggplot(aes(x = fct_reorder(es, prop_overall))) +
  geom_col(aes(y = prop_overall), fill = 'gray') +
  geom_col(aes(y = prop_yes), fill = 'black') +
  geom_hpline(aes(y = prop_expected_yes), colour = 'yellow', width=0.9, size=0.8) +
  xlab('Ecosystem service type') +
  ylab('Proportion of studies') +
  ggtitle('Considered time?') +
  coord_flip() +
  theme_bw()

#ggsave(paste0(figpath,'es_type_temporal.pdf'), width = 5, height = 5, dpi = 'retina')
rm(list=c('services_overall','overall_yes_prop'))


### connectivity

# overall props
services_overall = dat %>%
  filter(abbr=='Response') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(title,es) %>%
  # unique ESes studied per paper
  unique() %>%
  group_by(es) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers)

# specific props
connect_df = dat %>%
  filter(abbr=='Connect') %>%
  dplyr::select(title, Connectivity = clean_answer)

# expected props
overall_yes_prop = dat %>%
  filter(abbr=='Connect') %>%
  dplyr::select(title, Connectivity = clean_answer) %>%
  dplyr::select(Connectivity) %>%
  group_by(Connectivity) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count)) %>%
  filter(Connectivity=='Yes') %>%
  pull(proportion)


spatconn_plot = dat %>%
  filter(abbr=='Response') %>%
  filter(!is.na(clean_answer)) %>%
  dplyr::select(title, es) %>%
  # take unique title-ESes studied (reviewer could enter multiple response vars per es)
  unique() %>%
  left_join(connect_df, by = 'title') %>% 
  group_by(es, Connectivity) %>%
  summarise(count_yesno = n()) %>%
  left_join(services_overall, by = 'es') %>%
  filter(Connectivity == 'Yes') %>%
  rename(prop_overall = proportion, count_yes = count_yesno) %>%
  mutate(prop_yes = count_yes/num_papers) %>%
  mutate(prop_expected_yes = overall_yes_prop * prop_overall) %>%
  ggplot(aes(x = fct_reorder(es, prop_overall))) +
  geom_col(aes(y = prop_overall), fill = 'gray') +
  geom_col(aes(y = prop_yes), fill = 'black') +
  geom_hpline(aes(y = prop_expected_yes), colour = 'yellow', width=0.9, size=0.8) +
  xlab('Ecosystem service type') +
  ylab('Proportion of studies') +
  ggtitle("Spatial connectivity?") +
  coord_flip() +
  theme_bw()

#ggsave(paste0(figpath, 'es_type_connectivity.pdf'), width = 5, height = 5, dpi = 'retina')
rm(list=c('services_overall','overall_yes_prop'))

### multiple spatial scales

# overall props
services_overall = dat %>%
  filter(abbr=='Response') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(title, es) %>%
  # take unique ESes studied per paper (could enter multiple response variables per ES per study)
  unique() %>%
  group_by(es) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/num_papers)

# specific props
nested_df = dat %>%
  filter(abbr=='Nested') %>%
  dplyr::select(title, Nested = clean_answer)

# expected props
overall_yes_prop = dat %>%
  filter(abbr=='Nested') %>%
  dplyr::select(title, Nested = clean_answer) %>%
  dplyr::select(Nested) %>%
  group_by(Nested) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count)) %>%
  filter(Nested=='Yes') %>%
  pull(proportion)


multispat_plot = dat %>%
  filter(abbr == 'Response') %>%
  filter(!is.na(clean_answer)) %>%
  dplyr::select(title, es) %>%
  # take unique ESes studied per paper (could enter multiple response variables per ES per study)
  unique() %>%
  left_join(nested_df, by = 'title') %>% 
  group_by(es, Nested) %>%
  summarise(count_yesno = n()) %>%
  left_join(services_overall, by = 'es') %>%
  filter(Nested == 'Yes') %>%
  rename(prop_overall = proportion, count_yes = count_yesno) %>%
  mutate(prop_yes = count_yes/num_papers) %>%
  mutate(prop_expected_yes = overall_yes_prop * prop_overall) %>%
  ggplot(aes(x = fct_reorder(es, prop_overall))) +
  geom_col(aes(y = prop_overall), fill = 'gray') +
  geom_col(aes(y = prop_yes), fill = 'black') +
  geom_hpline(aes(y = prop_expected_yes), colour = 'yellow', width=0.9, size=0.8) +
  xlab('Ecosystem service type') +
  ylab('Proportion of studies') +
  ggtitle('Multiple spatial scales?') +
  coord_flip() +
  theme_bw()

#ggsave(paste0(figpath,'es_type_multiscale.pdf'), width = 5, height = 5, dpi = 'retina')
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

#ggsave(filename = paste0(figpath,'panel_spatiotemp_simp.pdf'), width=9, height=6, dpi='retina')

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

#ggsave(filename=paste0(figpath,'panel_spatiotemp.pdf'), width=9, height=6, dpi='retina')



### intersections with methods
# see `extent_and_methods.R`, to be included in SI
extent_title = dat %>% 
  filter(abbr=='Extent') %>% 
  dplyr::select(title, Extent = clean_answer)

extent_props = extent_title %>%
  group_by(Extent) %>%
  summarise(count = n()) %>%
  mutate(ext_proportion = count / num_papers) %>%
  rename(ext_count = count)



methods_title_seprow = dat %>% 
  filter(abbr == 'Methods') %>%
  mutate(clean_answer = gsub(" \\(.*\\)", '', clean_answer)) %>%
  dplyr::select(title, Methods = clean_answer) %>%
  separate_rows(Methods, sep = ',') 


ext_methods = methods_title_seprow %>%
  left_join(extent_title, by = 'title') %>%
  group_by(Extent, Methods) %>%
  summarise(count = n()) %>%
  ungroup() %>% 
  mutate(Extent = as.character(Extent)) %>%
  complete(Extent, Methods, fill = list(count = 0)) %>%
  mutate(methods_ext_prop = count / num_papers) %>%
  left_join(extent_props, by = 'Extent') %>%
  group_by(Extent) %>%
  mutate(methods_ext_prop_within = count / ext_count) 


ggplot() +
  geom_col(data = extent_props %>% mutate(Extent = factor(Extent, levels = c('Local','Macro-scale','Global','Undefined/no scale'))),
           aes(x = fct_rev(Extent), y = ext_proportion), 
           fill = 'lightgray') +
  geom_col(data = ext_methods, 
           aes(x = Extent, y = methods_ext_prop, group = Methods, fill = Methods), 
           position = 'dodge') +
  geom_hline(yintercept = 0) +
  xlab('Spatial extent') +
  ylab('Proportion of studies') +
  coord_flip() +
  theme_bw() 


ext_methods

### space & time intersections

# take nested
dat %>%
  filter(abbr=='Nested') %>%
  dplyr::select(title, Nested = clean_answer) %>%
  # join with connectivity
  left_join(
    dat %>%
      filter(abbr=='Connect') %>%
      dplyr::select(title, Connectivity = clean_answer),
    by='title'
  ) %>%
  # new col for yes/no space if connectivity or Nested is true
  mutate(Space_yesno = ifelse(Connectivity=='Yes' | Nested =='Yes', 'Yes', 'No')) %>%
  dplyr::select(title, Space_yesno) %>%
  # join with time trends yes/no
  left_join(
    dat %>%
      filter(abbr=='TimeTrends') %>%
      dplyr::select(title, TimeTrends = clean_answer) %>%
      mutate(TimeTrends = gsub(" \\(.*\\)", '', TimeTrends)) %>% # get rid of the 'e.g' text
      mutate(TimeTrends = ifelse(TimeTrends=='Space for time', 'Yes', TimeTrends)),
    by='title'
  ) %>%
  group_by(Space_yesno, TimeTrends) %>%
  summarise(count = n(), proportion = count/nrow(.))



##### Multifunctionality #####

### number of ES types per paper
dat %>%
  filter(abbr=='Response') %>% 
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(title, es) %>%
  # take unique ESes studied per paper (could enter multiple response variables per ES per study)
  unique() %>%
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'title', names_from = 'es', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>%
  mutate(num_ES_types = rowSums(.[-1])) %>%
  dplyr::select(title, num_ES_types) %>% 
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
  filter(abbr=='Response') %>% 
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(title, es) %>% 
  # take unique ESes studied per paper (could enter multiple response variables per ES per study)
  unique() %>%
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'title', names_from = 'es', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>%
  mutate(num_ES_types = rowSums(.[-1])) %>%
  dplyr::select(title, num_ES_types) %>% 
  group_by(num_ES_types) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/ sum(count))

#ggsave(filename=paste0(figpath, 'numestype_perpaper.pdf'), width=5, height=5, dpi='retina')

# pull titles that had 8+ service types
dat %>%
  filter(abbr=='Response') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(title, es) %>% 
  # take unique ESes studied per paper (could enter multiple response variables per ES per study)
  unique() %>%
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'title', names_from = 'es', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>%
  mutate(num_ES_types = rowSums(.[-1])) %>%
  filter(num_ES_types > 7) %>%
  dplyr::select(title, num_ES_types)



### chord diagram of multifunctionality (two service types)
site_by_es = dat %>%
  filter(abbr=='Response') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(title, es) %>% 
  # take unique ESes studied per paper (could enter multiple response variables per ES per study)
  unique() %>%
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = 'title', names_from = 'es', values_from = 'pres_holder') %>% 
  replace(is.na(.), FALSE) %>%
  dplyr::select(-title) %>%
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
withr::with_dir('figpath', saveWidget(p_int, file="chord_diag_interactive.html"))

# static version (excludes rare connections for better viewing)
adj_mat_stat = replace(adj_mat, adj_mat < 5, 0) # can be used to exclude rare connections
p_static = chorddiag(adj_mat_stat, 
                     groupnameFontsize = 14, 
                     showTicks = F, # show ticks with counts on circle edge
                     groupPadding = 5, # distance of names/segments from each other
                     groupnamePadding = 5 # distance of names from outside of circles
)
# save static
withr::with_dir('figpath', saveWidget(p_static, file="chord_diag_static.html"))

# webshot to pdf
webshot(paste0(figpath, "chord_diag_static.html"), paste0(figpath, "chord_diag_static.pdf"), delay = 0.2)


### Multifunctionality and time scales

# proportions within each single ES type
dat %>%
  filter(abbr=='Response') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(title, es) %>%
  # take unique ESes studied per paper 
  unique() %>%
  left_join(
    dat %>%
      filter(abbr=='YrsData') %>%
      mutate(clean_answer = ifelse(is.na(clean_answer), 'Time not considered', clean_answer)) %>%
      dplyr::select(title, clean_answer),
    by='title'
  ) %>% 
  group_by(es, clean_answer) %>%
  summarise(count=n()) %>%
  mutate(proportion_within_es = count / sum(count)) %>%
  mutate(clean_answer = gsub('–', '-', clean_answer)) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('Time not considered', '1 year or less','2-5 years','6-10 years', '10+ years'))) %>%
  ggplot(aes(x = fct_rev(clean_answer), y = proportion_within_es)) +
  geom_col() +
  facet_wrap(.~es) +
  ylab('Proportion of studies \n(within ecosystem service type)') +
  xlab('') +
  ggtitle('Study duration by ecosystem service type') +
  coord_flip() +
  theme_bw()

# counts instead, within each single ES type
dat %>%
  filter(abbr=='Response') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(title, es) %>%
  # take unique ESes studied per paper 
  unique() %>%
  left_join(
    dat %>%
      filter(abbr=='YrsData') %>%
      mutate(clean_answer = ifelse(is.na(clean_answer), 'Time not considered', clean_answer)) %>%
      dplyr::select(title, clean_answer),
    by='title'
  ) %>% 
  group_by(es, clean_answer) %>%
  summarise(count=n()) %>%
  mutate(proportion_within_es = count / sum(count)) %>%
  mutate(clean_answer = gsub('–', '-', clean_answer)) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('Time not considered', '1 year or less','2-5 years','6-10 years', '10+ years'))) %>%
  ggplot(aes(x = fct_rev(clean_answer), y = count)) +
  geom_col() +
  facet_wrap(.~es) +
  ylab('Number of studies') +
  xlab('') +
  ggtitle('Study duration by ecosystem service type') +
  coord_flip() +
  theme_bw()


dat %>%
  filter(abbr=='Response') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(title, es) %>%
  # take unique ESes studied per paper 
  unique() %>%
  left_join(
    dat %>%
      filter(abbr=='YrsData') %>%
      mutate(clean_answer = ifelse(is.na(clean_answer), 'Time not considered', clean_answer)) %>%
      dplyr::select(title, clean_answer),
    by='title'
  ) %>% 
  ungroup() %>%
  group_by(title) %>%
  summarise(num_ES = length(unique(es)), duration = clean_answer) %>%
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




##### Dynamics, non-linearities, uncertainty, and thresholds #####



### Feedbacks
dat %>% 
  filter(qnum=='Q15') %>%
  dplyr::select(title, clean_answer) %>% 
  # one conflicting answer, set as no feedbacks measured
  mutate(clean_answer = ifelse(clean_answer=='Ecosystem function -> service providers,No feedbacks measured directly', 'No feedbacks measured directly', clean_answer)) %>%
  separate_rows(clean_answer, sep = ',') %>%
  group_by(clean_answer) %>%
  summarise(count = n())

# feedbacks or not
dat %>% 
  filter(qnum=='Q15') %>%
  dplyr::select(title, clean_answer) %>% 
  # one conflicting answer, set as no feedbacks measured
  mutate(clean_answer = ifelse(clean_answer=='Ecosystem function -> service providers,No feedbacks measured directly', 'No feedbacks measured directly', clean_answer)) %>%
  mutate(clean_answer = ifelse(clean_answer=='No feedbacks measured directly',clean_answer,'Feedback measured')) %>% 
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count))

# mention 2 studies considered multiple feedbacks
dat %>% 
  filter(qnum=='Q15') %>%
  dplyr::select(title, clean_answer) %>% 
  # one conflicting answer, set as no feedbacks measured
  mutate(clean_answer = ifelse(clean_answer=='Ecosystem function -> service providers,No feedbacks measured directly', 'No feedbacks measured directly', clean_answer)) %>%
  group_by(clean_answer) %>%
  summarise(count = n())


# feedbacks plot
fdbck_plot = dat %>% 
  filter(qnum=='Q15') %>%
  dplyr::select(title, clean_answer) %>% 
  # one conflicting answer, set as no feedbacks measured
  mutate(clean_answer = ifelse(clean_answer=='Ecosystem function -> service providers,No feedbacks measured directly', 'No feedbacks measured directly', clean_answer)) %>%
  mutate(clean_answer = ifelse(clean_answer=='No feedbacks measured directly',clean_answer,'Feedbacks measured')) %>% 
  mutate(clean_answer = case_when(
    clean_answer=='No feedbacks measured directly' ~ 'No',
    clean_answer=='Feedbacks measured' ~ 'Yes'
  )) %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count)) %>%
  mutate(clean_answer = factor(clean_answer, levels=c('Yes','No'))) %>%
  ggplot() +
  geom_col(aes(x=clean_answer, y=proportion)) +
  ggtitle('Feedbacks measured?') +
  xlab('') +
  ylab('Proportion of studies') +
  ylim(c(0,1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=7), 
        title = element_text(size=7),
        axis.text.y=element_text(size=5))

#ggsave(filename=paste0(figpath,'feedbacks.pdf'), width=5, height=5, dpi='retina')


# pulling list of titles that considered feedbacks
dat %>% 
  filter(qnum=='Q15') %>%
  dplyr::select(title, clean_answer) %>% 
  # one conflicting answer, set as no feedbacks measured
  mutate(clean_answer = ifelse(clean_answer=='Ecosystem function -> service providers,No feedbacks measured directly', 'No feedbacks measured directly', clean_answer)) %>%
  mutate(clean_answer = ifelse(clean_answer=='No feedbacks measured directly',clean_answer,'Feedbacks measured')) %>%
  filter(clean_answer != 'No feedbacks measured directly')


### Thresholds
dat %>%
  filter(qnum=='Q16') %>%
  dplyr::select(title, clean_answer) %>% 
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count))


# thresholds plot
thresh_plot = dat %>%
  filter(qnum=='Q16') %>%
  dplyr::select(title, clean_answer) %>% 
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  mutate(proportion = count/sum(count)) %>%
  mutate(clean_answer = gsub('Mentioned or discussed but not measured', 'Mentioned', clean_answer)) %>%
  mutate(clean_answer = factor(clean_answer, levels = c('Yes','Mentioned','No'))) %>%
  ggplot() +
  geom_col(aes(x=clean_answer, y=proportion)) +
  ggtitle('Thresholds considered?') +
  xlab('') +
  ylab('Proportion of studies') +
  ylim(c(0,1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=7), 
        title = element_text(size=7),
        axis.text.y=element_text(size=5))

#ggsave(filename=paste0(figpath,'thresholds.pdf'), width=5, height=5, dpi='retina')


# Add feedbacks and thresholds to conceptual figure
# conceptual figure: round2_metareview/analyze_data/final_analyses/fig_files/draft_tempdyn_feedbacks_thresholds.pdf

ggdraw() +
  draw_image(magick::image_read(paste0(home,"tempdyn_feedbacks_thresh_fig.pdf"), density = 600),
             x=0, y=0, height=1, width=0.8) +
  # draw_image(magick::image_read("round2_metareview/analyze_data/final_analyses/fig_files/draft_tempdyn_feedbacks_thresh_fig.pdf", density = 100),
  #            x=0, y=0, height=1, width=0.8) + # low res for tweaking
  draw_plot(fdbck_plot, x=0.58, y=0.5, height=0.35, width=0.2) +
  draw_plot(thresh_plot, x=0.58, y=0.1, height=0.4, width=0.2)

#ggsave(filename=paste0(figpath,'panel_dynfdbckthresh.pdf'), width=8.5, height=5, dpi='retina')


### Uncertainty
# Only present in survey as an open ended question with short answer format.
# Here are the notes people left (22 papers had something noted, could go
# through in more detail).
dat %>%
  filter(qnum=='Q17') %>%
  dplyr::select(title, clean_answer) %>%
  filter(!is.na(clean_answer)) %>%
  View()




#### Sankey Alluvial ES drivers diagram ####

## prep data
# isolate updated driver types
new.bins <- # > using adjusted LULC driver types column to make table of driver bins with their lulc adjusted driver types
  # > this includes papers excluded for being lulc drivers only (to exclude those from sankey)
  dat %>%
  # pull driver answers that are not NA or 'Other' (signals other drivers present, but is not a meaningful driver)
  # to exclude lulc driver-only studies, add filter: "!only_lulc" <-- will drop any paper where this is true
  filter(abbr %in% c('Driver', 'OtherDriver'), !clean_answer %in% c(NA, "Other")) %>%
  # retain all studies (even lulc_only), keep just title, binned variable, and lulc adjusted driver type
  dplyr::select(title, clean_answer_binned, lulc_group) %>%
  # rename to clean_group so sankey code runs
  rename(clean_group = lulc_group) %>%
  # take only unique rows
  unique()

# isolate driver types and es types for sankey
# > using adjusted LULC driver types column to make table of driver bins with their lulc adjusted driver types
# > this included papers excluded for being lulc drivers only
es.bins <- dat  %>%
  # pull driver answers that are not NA or 'Other' (signals other drivers present, but is not a meaningful driver)
  # to exclude lulc driver-only studies, add filter: "!only_lulc" <-- will drop any paper where this is true
  filter(abbr %in% c('Driver', 'OtherDriver'), !clean_answer %in% c(NA, "Other")) %>%
  # select title, lulc adjusted driver group, and ESes studied per paper
  dplyr::select(title, lulc_group, es) %>%
  # take unique rows
  unique() %>%
  # rename driver type as above column so sankey code runs
  rename(driv_type = lulc_group)

# subset extracted data
df <- dat[c("title", "abbr", "lulc_group", "es", "qnum")] # pull relevant columns
df <- df[df$qnum == "Q12",] # subset to only Q12
df <- df[,-5]

response.df <- df[df$abbr=="Response",] # pull just response rows
#response.df <- response.df[!is.na(response.df$clean_answer_finer),] # get rid of NA response bin for now
response.df <- response.df[!duplicated(response.df),]


driver.df <- es.bins
driver.df <- driver.df[!is.na(driver.df$driv_type),] # get rid of NA values
driver.df[driver.df == "LU_LC"] <- "Land Use Land Change"


##### Small Sankey: Drive type-ES type #####
## Node date frame for sankey
nodes.driver.bins <- data.frame(name = unique(driver.df$driv_type), NodeType="driver.type")
nodes.ES <- data.frame(name = unique(response.df$es), NodeType = "eco.serv")

nodes <- rbind(nodes.driver.bins, nodes.ES)
nodes <- nodes[!is.na(nodes$name),] # get rid of NA values

attach(nodes)
nodes <- nodes[order(NodeType,name),]
detach(nodes)

nodes <- nodes[c(3,1,2,4,5:20),]

nodes$NodeType <- c("Human","Biotic","Environmental","Land Use Land Change", "Regulating", "Regulating","Regulating",
                    "Cultural","Provisioning","Provisioning","Regulating","Supporting",
                    "Regulating","Cultural","Provisioning","Provisioning","Regulating",
                    "Regulating","Supporting","Supporting")


nodes$Type <- c(rep("Driver", times = 4), rep("eco.serv", times=16))

attach(nodes)
nodes <- nodes[order(Type, NodeType),]
detach(nodes)

nodes$FullName <- c("Biotic", "Environmental", "Human","Land Use Land Change",
                    "Physical and Psychological exp.","Maintenance of Options",
                    "Energy","Food and Feed","Materials", "Medical",
                    "Air Quality","Climate Regulation","Coastal Water Quality",
                    "Fresh Water Quality","Hazard Regulation","Regulation of Ocean Acidification",
                    "Regulation of pests/pathogens","Habitat Creation","Pollination",
                    "Soil Formation/Protection")


nodes <- nodes[c(3,4,2,1,12,17,13,14,15,11,16,20,18,19,8,9,7,10,5,6),] # reorder based on number of papers...this is just manual at this point

x <- nrow(nodes) - 1
nodes$ID <- 0:x

colnames(nodes)[colnames(nodes)=="name"] <- "TargetName"

nodes[1,c(1,2)] <- "Human"
nodes[4,c(1,2)] <- "Biotic"
nodes[3,c(1,2)] <- "Environmental"
nodes[2,c(1,2)] <- "Land Use Land Change"

nodes$FullName[1:4] <- ""

## Link data frame for sankey
# link VALUES
values <- driver.df[,c(2,3)]
values$num.papers <- 1 # add column to sum
values <- aggregate(formula = num.papers ~ es + driv_type , data = values, FUN = sum)
col <- c("TargetName","SourceName","value")
colnames(values) <- col


# create data frame to merge later
links <- data.frame(SourceName = rep(c("Human","Land Use Land Change","Environmental", "Biotic"), each=nrow(nodes[nodes$Type=="eco.serv",])),
                    Source = rep(c(0,1,2,3), each=nrow(nodes[nodes$Type=="eco.serv",])),
                    TargetName = rep(nodes$TargetName[c(5:20)], times = 4),
                    Target = rep(nodes$ID[5:nrow(nodes)], times=4)
) 

s.links <- merge(x=links, y=values, by = c("SourceName","TargetName"), all.x = T)
s.links <- s.links[!is.na(s.links$value),] # get rid of NA values



names(s.links)[names(s.links)=="x"] <- "value"

s.links <- merge(s.links,nodes,by="TargetName", all.x=T)

attach(s.links)
s.links <- s.links[order(NodeType, value),]
detach(s.links)

# Add group labels for coloring in sankey plot
s.links$LinkType <- s.links$SourceName
nodes$Group <- c("A","B","C","D",rep(c("E","F","G","H"), times=c(7,3,4,2)))
s.links["LinkType"][s.links["LinkType"] == "Human"] <- "A"
s.links["LinkType"][s.links["LinkType"] == "Land Use Land Change"] <- "B"
s.links["LinkType"][s.links["LinkType"] == "Environmental"] <- "C"
s.links["LinkType"][s.links["LinkType"] == "Biotic"] <- "D"


my_colors <-  'd3.scaleOrdinal() .domain(["A","B","C","D","E","F","G","H"]) 
.range(["#a6cee3", "#00008B","#b2df8a", "#33a02c", "#000000","#808080","#D3D3D3","#FFFFFF"])'



##### Base Sankey Code #####


# build sankey
sn <- sankeyNetwork(Links = s.links, Nodes = nodes, Source = "Source",
                    Target = "Target", Value = "value", NodeID = "FullName",
                    units = "papers", fontSize = 17, nodeWidth = 30,
                    fontFamily = "Arial",
                    LinkGroup = "LinkType",
                    NodeGroup = "Group",
                    colourScale = my_colors,
                    margin = list(left=300, right=50),
                    iterations=0)

# ecosystem service labels
right <- c("Climate Regulation","Regulation of pests/pathogens","Coastal Water Quality",
           "Fresh Water Quality","Hazard Regulation","Air Quality","Regulation of Ocean Acidification",
           "Soil Formation/Protection","Habitat Creation","Pollination","Food and Feed",
           "Materials","Energy","Medical","Physical and Psychological exp.","Maintenance of Options")

# Plot sankey  
onRender(
  sn,
  paste0('
        function(el,x){
        d3.select(el)
        .selectAll(".node text")
        .filter(function(d) { return (["',paste0(right,collapse = '","'),'"].indexOf(d.name) > -1);})
        .attr("x", 6 + x.options.nodeWidth)
        .attr("text-anchor", "begin");
        }
        '))



#### METADATA SUMMARIES ===============================================

##### Journal distribution #####
# how many journals considered, what potential bias is there?
allpapers <- subset(excluded, select = c(title:review_round, exclusion_id)) %>%
  rbind(
    # row-bind unique titles and citation info from full-text dataset
    cbind(distinct(dat, title, authors, pubyear, sourcepub),
          # indicate from round 2 and NA for exclusion_id (not excluded)
          review_round = 2, exclusion_id = NA)
  )

# how many unique journals to start?
length(unique(allpapers$sourcepub)) #128 journals for round 1
# how many unique journals retained after abstract review (made it to round 2 pre random selection)?
length(unique(allpapers$sourcepub[allpapers$review_round == 2])) #98 journals
# how many unique journals retained after random selection subsetting for full-text review assignment?
with(allpapers, length(unique(sourcepub[review_round == 2 & exclusion_id %in% c(NA, 1:8)]))) #80 journals
# how many unique journals ultimately retained after full-text review?
length(unique(allpapers$sourcepub[is.na(allpapers$exclusion_id)])) # 69 journals

# group titles by journal
r1jsum <- group_by(allpapers, sourcepub) %>%
  summarise(papers = length(title),
            pct = papers/nrow(allpapers),
            pct = 100*pct) %>%
  ungroup() %>%
  arrange(-pct) %>%
  mutate(rank = 1:nrow(.)) %>%
  rename_if(!grepl("^source", names(.)), function(x) paste0(x,"_r1"))

# summarize journals that were assigned for full text review (drops papers that passed abstract screen but were not random selected)
r2jsum <- subset(allpapers, review_round == 2 & exclusion_id %in% c(NA, 1:8)) %>%
  group_by(sourcepub) %>%
  summarise(papers = length(title),
            pct = papers/length((allpapers$title[allpapers$review_round == 2 & allpapers$exclusion_id %in% c(NA, 1:8)])),
            pct = 100*pct) %>%
  ungroup() %>%
  arrange(-pct) %>%
  mutate(rank = 1:nrow(.)) %>%
  rename_if(!grepl("^source", names(.)), function(x) paste0(x,"_r2"))

# summarize journals retained after full-text review
finaljsum <- distinct(dat, title, sourcepub) %>%
  group_by(sourcepub) %>%
  summarise(papers = length(title),
            pct = papers/length(unique(dat$title)),
            pct = 100*pct) %>%
  ungroup() %>%
  arrange(-pct) %>%
  mutate(rank = 1:nrow(.)) %>%
  rename_if(!grepl("^source", names(.)), function(x) paste0(x,"_final"))

jsum <- left_join(r1jsum, r2jsum) %>%
  left_join(finaljsum) %>%
  subset(rank_r1 %in% 1:10 | rank_r2 %in% 1:10 | rank_final %in% 1:10)



##### Reasons for exclusion #####
# number of distinct title-abstracts to start
length(unique(allpapers$title)) #1932 (WOS returned 1933, but one record same paper with pub date of late 2018 and early 2019)
# number of papers exclude in round 1
sum(excluded$review_round == 1) #1149
# number of papers kept after round 1 abstract screen
sum(allpapers$review_round != 1) #783
# number of papers excluded by random selection subsetting for full-text review
nrow(subset(excluded, review_round == 2 & grepl("random", exclusion_reason))) #391
# number of papers kept after random selection for full-text review
nrow(subset(allpapers, review_round == 2 & !grepl("9", exclusion_id))) #392
# number of papers retained after full text review (generally)
length(unique(dat$title)) #273
# number of papers excluded in round 2
with(excluded, sum(review_round == 2 & exclusion_id %in% 1:8)) #119
# number of papers that were LULC drivers only (excluded from certain narrative syntheses)
length(unique(dat$title[dat$only_lulc]))
# number of full text retained for all narrative synthesis (excluded LULc only)
length(unique(dat$title[!dat$only_lulc])) #244

# count reasons for exclusion across all rounds
table(excluded$exclusion_reason)
# split by review round
sapply(split(excluded$exclusion_reason, excluded$review_round), table)

# ignore papers not random selected for full text review (i.e. true excluded papers)
# reason excluded?
subset(excluded, exclusion_id != 9) %>%
  group_by(review_round, exclusion_reason) %>%
  summarise(nobs = length(title),
            grand_pct = (nobs/nrow(.))*100) %>%
  ungroup() %>%
  mutate(round_pct = ifelse(review_round == 1, 
                            (nobs/nrow(subset(excluded, review_round == 1)))*100, 
                            (nobs/nrow(subset(excluded, review_round == 2 &exclusion_id != 9)))*100)) %>%
  arrange(review_round, -nobs)

# lump reasons per round 2 and re-summarize
subset(excluded, exclusion_id != 9) %>%
  mutate(grp_reason = ifelse(grepl("^Revi|^Meta|tool", exclusion_reason), "Meta-analysis, review, or methods/tool paper only",
                             ifelse(grepl("^Val|^Soc", exclusion_reason), "Social dimensions, valuation or risk paper", 
                                    exclusion_reason))) %>%
  # calculate grand tot
  group_by(grp_reason) %>%
  mutate(grand_nobs = length(title),
         grand_pct = (grand_nobs/nrow(.))*100) %>%
  ungroup() %>%
  # summarize per round
  group_by(grp_reason, review_round, grand_nobs, grand_pct) %>%
  summarise(nobs = length(title),
            pct = (nobs/nrow(.))*100) %>%
  ungroup() %>%
  # gather stats then spread
  gather(met, val, nobs:pct) %>%
  # add "r" to round vals for spread colnames
  mutate(review_round = paste0("r", review_round)) %>%
  unite(met, review_round, met) %>%
  spread(met, val) %>%
  arrange(-grand_nobs)



##### Reviewer consistency #####
# review consistency of double review answers for methods
# > runting et al. 2017 assigned 1 if agreed, 0.5 if partially agreed, and 0 if completely disagreed
# > gauge q12 based on 1) ES row filled in, and types of coarse original drivers select (Human, Biotic, or Env in raw_group)

# how many papers in total double reviewed?
length(unique(doublerev$title)) #34
# of excluded papers, how many required a second opinion?
with(excluded, sapply(split(outsidereviewer, review_round), function(x) sum(!is.na(x))))
# 35 papers in excluded dataset, but only for round 2 (not for round 1 -- may have been done more informally, e.g., in class discussion)

# drop 3 papers where paper kept but reviewers didn't agree on exclusion (because all answers to those will conflict)
dbltidy <- subset(doublerev, !title %in% with(dat, title[doublerev & grepl("keep paper", qa_note, ignore.case = T)]))

# summarize ES-response-driver question
dblq12 <- subset(dbltidy, qnum == "Q12") %>%
  # need to note which cols are response 2
  mutate(response2 = grepl("Response 2", fullquestion) & abbr == "Response") %>%
  # drop response 2.. if something is in there, it means they filled out response 1
  subset(!response2) %>%
  #distill to ES and coarse group (ignore specifics of variable)
  group_by(title, survey_order) %>%
  mutate(hasanswer = length(clean_answer[!is.na(clean_answer)])) %>%
  subset(hasanswer > 0) %>%
  arrange(title, survey_order, reviewer) %>%
  ungroup() %>%
  mutate(markanswer = ifelse(is.na(clean_answer), 0, 1)) %>%
  dplyr::select(title, reviewer, markanswer, abbr, es, survey_order, raw_group, clean_group) %>%
  distinct() %>%
  # if both agree (answers provided), will have sum of 2 for given question, if not will only sum to 1
  group_by(title, survey_order) %>%
  mutate(sumcheck = sum(markanswer)) %>%
  # can drop effect direct and Yclass bc we're not using those
  subset(!abbr %in% c("Yclass", "EffectDirect")) %>%
  ungroup() %>%
  # I think can drop individual reviewer and just look at the sumchecks
  dplyr::select(title, survey_order, abbr, es, clean_group, sumcheck) %>%
  distinct() %>%
  # drop clean_group if NA (means only 1 person answered answer in that row)
  subset(!(grepl("Driver", abbr) & is.na(clean_group)))


summary(as.factor(dblq12$sumcheck)) #a little more than 1/3rd of answers agree in q12 matrix
# we want to know where they are differing. is it on ES? on driver type?
# look separately by ES, response and driver?
dplyr::select(dblq12, title, es, sumcheck) %>%
  distinct() %>%
  group_by(title, es) %>%
  mutate(ignore = ifelse(length(sumcheck)==2 & sumcheck == 1, TRUE, FALSE)) %>%
  subset(!ignore) %>%
  ungroup() %>%
  group_by(title) %>%
  mutate(score = ifelse(all(sumcheck == 1), 0, ifelse(length(unique(sumcheck)) > 1, 0.5, 1))) %>%
  dplyr::select(title, score) %>%
  distinct() %>%
  ggplot(aes(as.factor(score))) +
  geom_bar() # note: the ES barchart IS the same as congruence in response cateogry (if ES filled out, response is filled out)

ESagreement <- dblq12 %>%
  subset(abbr == "Response") %>%
  group_by(title) %>%
  mutate(score = ifelse(all(sumcheck == 1), 0, ifelse(length(unique(sumcheck)) > 1, 0.5, 1))) %>%
  ungroup() %>%
  dplyr::select(title, score) %>%
  distinct() %>%
  group_by(score) %>%
  summarize(pct = round((length(score)/31)*100, 2)) %>%
  ungroup() %>%
  mutate(q = "ES agreement") %>%
  spread(score, pct)


# look at driver congruence--do double reviewers choose the same driver types? (i guess looking with ES's where they agree? or maybe just outright drivers in any ES)
# method 1: just look at congruence where reviewers agree on the same ES
driversimilarity <- group_by(dblq12, title, es) %>%
  mutate(dropES = es %in% es[abbr == "Response" & sumcheck == 1]) %>%
  subset(!dropES) %>%
  # can drop response too
  subset(abbr != "Response")
# the way it should work (i think) is. if all same driver cats filled out (sumcheck == 2), then in agreement, if mix of 1s and 2s then partial, if all 1s, no agreement
driversimilarity %>%
  group_by(title, es, abbr) %>%
  mutate(score = str_flatten(sort(unique(sumcheck))),
         score = ifelse(score == "12", 0.5, ifelse(score == "1", 0, 1))) %>%
  ungroup() %>%
  dplyr::select(title, es, abbr, score) %>%
  distinct() %>%
  ggplot(aes(as.factor(score))) +
  geom_bar() +
  facet_wrap(~abbr)
# what about by paper (ignore ES, since we're selecting for ES's where reviewers agreed)
driversimilarity %>%
  group_by(title, abbr) %>%
  mutate(score = str_flatten(sort(unique(sumcheck))),
         score = ifelse(score == "12", 0.5, ifelse(score == "1", 0, 1))) %>%
  ungroup() %>%
  dplyr::select(title, abbr, score) %>%
  distinct() %>%
  ggplot(aes(as.factor(score))) +
  geom_bar() +
  facet_wrap(~abbr)

# pct agreement for drivers vs other driver answered (not considering ES)
driversim <- subset(dbltidy, qnum == "Q12") %>%
  subset(grepl("Driver", abbr)) %>%
  # drop ES -- we're just comparing drivers answered
  select(-c(es, survey_order, qid, qa_note, raw_answer, varnum)) %>%
  # drop "Other" in abbr == Driver bc other driver is there
  subset(!(abbr == "Driver" & grepl("Other", clean_answer)) & !is.na(clean_answer)) %>%
  distinct() %>%
  group_by(title, clean_answer, abbr, clean_group) %>%
  # can check if ppl wrote same answer for drivers (since standardized drivers..)
  mutate(count_inits = length(unique(reviewer))) %>%
  ungroup() %>%
  # if other driver has any answer count (don't compare answers verbatim)
  group_by(title, abbr, clean_group) %>%
  mutate(count_inits = ifelse(abbr == "OtherDriver", length(unique(reviewer)), count_inits)) %>%
  ungroup()

# .. maybe check congruency within driver category since also assess general congruency across drivers
driversim2 <- select(driversim, title, clean_group, abbr, count_inits) %>%
  group_by(title, clean_group) %>%
  mutate(score = ifelse(all(count_inits == 2), 1, ifelse(all(count_inits == 1), 0, 0.5))) %>%
  ungroup() %>%
  select(title, clean_group, score) %>%
  distinct() %>%
  # if no clean_group present, it means reviewers agreed that category does not have a driver
  spread(clean_group, score, fill = 1) %>%
  gather(clean_group, score, Biotic:Human) %>%
  # count titles per category
  group_by(clean_group) %>%
  mutate(paper_count = length(unique(title))) %>% #31 papers considered
  ungroup() %>%
  group_by(clean_group, score) %>%
  summarize(pct = round(100*(length(title)/31),2)) %>%
  ungroup() %>%
  mutate(q = "drivers") %>%
  spread(score, pct)


# do they agree on driver category? ignoring standardized driver answer vs. "other driver"
dblq12_catsimilarity <- subset(dbltidy, qnum == "Q12") %>%
  # need to note which cols are response 2
  mutate(response2 = grepl("Response 2", fullquestion) & abbr == "Response") %>%
  # drop response 2.. if something is in there, it means they filled out response 1
  subset(!response2) %>%
  # need to re-code otherdriver as driver to compare coarse driver cats
  mutate(abbr = recode(abbr, "OtherDriver" = "Driver")) %>%
  #distill to ES and coarse group (ignore specifics of variable)
  group_by(title, abbr) %>%
  mutate(hasanswer = length(clean_answer[!is.na(clean_answer)])) %>%
  subset(hasanswer > 0) %>%
  arrange(title, survey_order, reviewer) %>%
  ungroup() %>%
  mutate(markanswer = ifelse(is.na(clean_answer), 0, 1)) %>%
  dplyr::select(title, reviewer, markanswer, abbr, es, abbr, raw_group, clean_group) %>%
  distinct() %>%
  # if both agree (answers provided), will have sum of 2 for given question, if not will only sum to 1
  group_by(title, es, abbr, clean_group) %>%
  mutate(sumcheck = length(unique(reviewer[markanswer == 1]))) %>%
  ungroup() %>%
  group_by(title, abbr, clean_group) %>%
  mutate(sumcheck_simple = length(unique(reviewer[markanswer == 1]))) %>%
  # can drop effect direct and Yclass bc we're not using those
  subset(!abbr %in% c("Yclass", "EffectDirect")) %>%
  ungroup() %>%
  # I think can drop individual reviewer and just look at the sumchecks
  dplyr::select(title, abbr, es, clean_group, sumcheck, sumcheck_simple) %>%
  distinct() %>%
  # drop clean_group if NA (means only 1 person answered answer in that row)
  subset(!(grepl("Driver", abbr) & is.na(clean_group)))
# > include ES .. within ES (where they agreed), did they pick the same coarse drivers
group_by(dblq12_catsimilarity, title, es) %>%
  mutate(dropES = es %in% es[abbr == "Response" & sumcheck != 2]) %>%
  subset(!dropES) %>%
  ungroup() %>%
  group_by(title, abbr, es) %>%
  mutate(score = str_flatten(sort(unique(sumcheck))),
         score = ifelse(score == "12", 0.5, ifelse(score == "1", 0, 1))) %>%
  ungroup() %>%
  subset(abbr == "Driver") %>%
  dplyr::select(title, es, score) %>%
  distinct() %>%
  ggplot(aes(as.factor(score))) +
  geom_bar() +
  facet_wrap(~es)

# drop ES -- this is looking at, per paper, where same coarse driver categories selected (ignoring which ES row filled out)
dblq12_catsimilarity %>%
  subset(sumcheck >0) %>%
  group_by(title, abbr) %>%
  mutate(score = str_flatten(sort(unique(sumcheck_simple))),
         score = ifelse(score == "12", 0.5, ifelse(score == "1", 0, 1))) %>%
  ungroup() %>%
  subset(abbr == "Driver") %>%
  dplyr::select(title, abbr, score) %>%
  distinct() %>%
  ggplot(aes(as.factor(score))) +
  geom_bar()

drivercats <- dblq12_catsimilarity %>%
  subset(sumcheck >0) %>%
  group_by(title, abbr) %>%
  mutate(score = str_flatten(sort(unique(sumcheck_simple))),
         score = ifelse(score == "12", 0.5, ifelse(score == "1", 0, 1))) %>%
  ungroup() %>%
  group_by(score) %>%
  summarise(pct = (length(score)/nrow(.))*100) %>%
  mutate(q = "driver category") %>%
  spread(score, pct)


# assess fixed responses in questions other than q12, no exclusion question [if it's in this dataset, it made it through], and no notes fields (only standardized answers [multi-choice options])
dblother <- subset(dbltidy, !qnum %in% c("Q12", "Q3") & !grepl("Notes|GenInfo", abbr)) %>%
  # here we go
  group_by(title, survey_order) %>%
  mutate(sameanswer = length(unique(clean_answer))==1) %>%
  arrange(title, survey_order, reviewer) %>%
  ungroup() %>%
  mutate(rowid = rownames(.))

# plot with no double counts
dplyr::select(dblother, title, qnum, abbr, survey_order, sameanswer) %>%
  distinct() %>%
  ggplot(aes(sameanswer)) +
  geom_bar() +
  facet_wrap(~abbr)
# think need to split answers on commas to compare partial matches..
# > also if answers for time trends and connectivity (yes/no) don't agree, don't consider discrepancies for subsequent related question
# > and don't match uncertainty verbatim, since it's write in. if there's a non-NA answer at all, it means reviewers agreed paper included some aspect of uncertainty
dblother$score <- ifelse(dblother$sameanswer, 1, 0)
for(t in unique(dblother$title)){
  tempdat <- subset(dblother, title == t & !sameanswer)
  r1 <- unique(tempdat$reviewer)[1]
  r2 <- unique(tempdat$reviewer)[2]
  for(s in unique(tempdat$survey_order)){
    ans1 <- unlist(with(tempdat, strsplit(clean_answer[survey_order == s & reviewer == r1], split = ","))) %>% trimws()
    ans2 <- unlist(with(tempdat, strsplit(clean_answer[survey_order == s & reviewer == r2], split = ","))) %>% trimws()
    compare <- any(ans1 %in% ans2) | any(ans2 %in% ans1)
    if(compare){
      # replace 0 with 0.5 for partial match
      dblother$score[dblother$rowid %in% with(tempdat, rowid[title == t & survey_order == s])] <- 0.5
    }
    # address dependent questions
    if(s %in% c(30, 58)){
      prevanswer <- unique(tempdat$sameanswer[tempdat$survey_order == s-1])
      # if length is 0, it means previous answer agreed
      prevanswer <- length(prevanswer)==0
      if(!prevanswer){
        # if it's false, it means disagreed on first-part question
        dblother$score[dblother$rowid %in% with(tempdat, rowid[title == t & survey_order == s])] <- 5 #5 = ignore
      }
      # if prevanswer is TRUE and one of the dependent answers is NA (forgot to answer, ignore)
      if(prevanswer & any(is.na(tempdat$clean_answer[tempdat$survey_order == s]))){
        dblother$score[dblother$rowid %in% with(tempdat, rowid[title == t & survey_order == s])] <- 3 # 3 = one answer missing
      }
    }
    # address uncertainty write-in question (only gets score of 0 if one answer is NA)
    if(s == 211 & !any(is.na(with(tempdat, clean_answer[title == t & survey_order == s])))){
      dblother$score[dblother$rowid %in% with(tempdat, rowid[title == t & survey_order == s])] <- 1
    }
  }
}

# what does it look like now?
dplyr::select(dblother, title, qnum, abbr, survey_order, score) %>%
  distinct() %>%
  ggplot(aes(as.factor(score))) +
  geom_bar() +
  facet_wrap(~survey_order + abbr)

dplyr::select(dblother, title, qnum, abbr, survey_order, score) %>%
  distinct() %>%
  group_by(survey_order, abbr, score) %>%
  summarise(pct = round(length(score)/length(unique(dblq12$title)),2)) %>%
  data.frame() %>%
  mutate(pct = round(100*pct)) %>%
  spread(score, pct, fill = "-")

# re-crunch dropping q's reviewers forgot to answer and ones that shouldn't be considered 
otherqs <- dplyr::select(dblother, title, qnum, abbr, survey_order, score) %>%
  distinct() %>%
  group_by(qsurvey_order, abbr, score) %>%
  summarise(nobs = length(score)) %>%
  data.frame() %>%
  subset(score <= 1) %>%
  group_by(abbr) %>%
  mutate(totsum = sum(nobs)) %>%
  ungroup() %>%
  mutate(pct = round((nobs/totsum)*100,2)) %>%
  select(qnum, survey_order, abbr, score, pct) %>%
  spread(score, pct, fill = 0)

# how many papers in each question for other questions (interested in answer-dependent questions?
count_papers <- subset(dblother, score <= 1, select = c(title, qnum, abbr)) %>%
  distinct() %>%
  group_by(qnum, abbr) %>%
  summarise(nobs = length(title))

# compile all pcts for reference
congruence <- rename(otherqs, q = abbr) %>%
  select(-survey_order) %>%
  rbind(cbind(qnum = "Q12", ESagreement)) %>%
  rbind(cbind(qnum = "Q12", drivercats)) %>%
  rbind(cbind(qnum = "Q12", unite(driversim2, q, q, clean_group, sep =" "))) %>%
  mutate(qnum = parse_number(qnum)) %>%
  arrange(qnum)

# what is the average congruence score across all questions assessed?
# take average by question, and then average those composite scores
mean_congruence <- congruence %>%
  mutate(rowid = 1:nrow(.)) %>%
  gather(score, pct, "0":"1") %>%
  mutate(score2 = as.numeric(score) * pct) %>%
  group_by(rowid, q) %>%
  summarise(score = (sum(score2)/100)) %>%
  ungroup() %>%
  arrange(rowid)


# check congruency in excluded papers (or kept but one person excluded). 
# how many had reviewers agree (or one checked "yes" and another expressed doubt in comments), vs. ones where reviewers disagreed and 3rd party reviewed?
nrow(subset(excluded, review_round == 2 & doublerev & exclusion_id != 9)) #29 papers double reviewed in round 2 and excluded (not counting those randomly unselected)
# > any paper where exclusion opinion disagreed will have a third party reviewer
with(excluded, length(title[!is.na(outsidereviewer) & review_round ==2 & doublerev & exclusion_id != 9])) # 15 papers required third party review
# review notes
View(subset(excluded, !is.na(outsidereviewer) & doublerev & exclusion_id != 9, select = c(title, reviewer_notes, outsidereviewer_notes)))
# 6 have partial agreement:
## 2 papers agree on exclusion, but for different reasons
## 2 papers both have reviewers expressing doubts in notes (but neither excluded)
## 2 papers have 1 reviewer excluded by Q3, and other expressed doubts in survey notes (but still filled out survey)
# additionally, if look at full-text dataset, there are 3 double-reviewed papers that were kept ultimately, but initially reviewers disagreed on exclusion (outside reviewer reviewed the papers for final opinion)

# add score for Exclude? agreement (do reviewers agree that papers should be excluded/kept?)
# and score for Exclusion reason
# > prep excluded and kept data so by title has a score for each
doublerev_excludeQ <- subset(excluded, review_round == 2 & doublerev & exclusion_id != 9) %>%
  group_by(title) %>%
  summarise(exclude = TRUE,
         # if there was no outside reviewer needed, then reviewers agreed, else they didn't
         agree_exclude = ifelse(is.na(outsidereviewer), 1, 
                                # if they agreed to exclude but differed on reason why, assign partial
                                ifelse(grepl("disagree on exclusion", outsidereviewer_notes), 0.5, 
                                       # otherwise, no agreement
                                       0)),
         # if outside reviewer note says reviewers disagreed on exclusion reason, then 0
         agree_reason = ifelse(grepl("disagree on exclusion", outsidereviewer_notes), 0,
                               # if both reviewers commented, say partial agreement
                               ifelse(grepl("R[0-9]+.*R[0-9]+", reviewer_notes), 0.5, 
                                      # if there was no outside review needed then reviewers agreed on reason
                                      ifelse(is.na(outsidereviewer), 1, 
                                             # otherwise, no agreement about reason to exclude (third party required)
                                             0))))

doublerev_keep <- subset(dat, doublerev & qnum == "Q3") %>%
  group_by(title) %>%
  summarise(exclude = FALSE,
            agree_exclude = ifelse(any(grepl("Yes", raw_answer)), 0, 1),
            agree_reason = ifelse(any(grepl("inconsistent", qa_note)), 0, 
                                  # otherwise assign NA to ignore because exclusion reason not applicable
                                  NA))

# rbind both then crunch percents for summary table
doublerev_exclude_tally <- rbind(doublerev_excludeQ, doublerev_keep) %>%
  gather(q, val, agree_exclude:agree_reason) %>%
  # if reason agreement is NA, drop because it means the paper was kept
  subset(!is.na(val)) %>%
  group_by(q, val) %>%
  summarise(tally = length(title)) %>%
  spread(val, tally)
# calculate mean score
doublerev_exclude_mean <- doublerev_exclude_tally %>%
  gather(score, tally, '0':'1') %>%
  group_by(q) %>%
  mutate(tot = sum(tally)) %>%
  ungroup() %>%
  mutate(prop = tally/tot,
         weighted_score = prop*as.numeric(score)) %>%
  group_by(q) %>%
  summarise(mean_score = sum(weighted_score))

# convert to percentage
doublerev_exclude_pcts <- doublerev_exclude_tally 
doublerev_exclude_pcts[,2:4] <- doublerev_exclude_tally[,2:4]/apply(doublerev_exclude_tally[,2:4], 1, sum)
doublerev_exclude_pcts[,2:4] <- apply(doublerev_exclude_pcts[,2:4], 2, function(x) round((x*100),2))
# append mean score to pcts and add qnum
doublerev_exclude_pcts <- left_join(doublerev_exclude_pcts, doublerev_exclude_mean) %>%
  mutate(qnum = 3) %>%
  # rearrange columns
  dplyr::select(qnum, q:ncol(.))


# what is the overall average reviewer convergence score (average all question averages)
mean(c(mean_congruence$score,doublerev_exclude_pcts$mean_score)) # 0.7548786




