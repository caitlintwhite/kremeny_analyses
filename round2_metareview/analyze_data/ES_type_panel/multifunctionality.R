library(tidyverse)
library(UpSetR)
library(chorddiag)
library(htmlwidgets)
library(webshot)
webshot::install_phantomjs()

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')


dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'ES', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>%
  mutate(num_ES_types = rowSums(.[-1])) %>% # gets the number of ES types for each paper (as long as Title is first column)
  filter(num_ES_types > 1) %>% 
  data.frame() %>% 
  upset(nsets = ncol(.)-1, nintersects = NA)

# this is far too messy to include, but it may go in the supplement to show that, if a study is looking at multifuncitonality, climate reg is most often considered
# however, this also reflects largely, though not perfectly, the overall distribution of service types, so not super important
# below is a cleaner version of the figure for the main text

num_1plus_papers = 12 # this is the number of intersections that contain more than one paper, counted from above plot

pl2 = dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>% 
  mutate(pres_holder = 1) %>%
  pivot_wider(id_cols = 'Title', names_from = 'ES', values_from = 'pres_holder') %>% 
  replace(is.na(.), 0) %>%
  mutate(num_ES_types = rowSums(.[-1])) %>% # gets the number of ES types for each paper (as long as Title is first column)
  filter(num_ES_types > 1) %>% 
  data.frame() %>% 
  upset(nsets = ncol(.)-1, 
        nintersects = num_1plus_papers, 
        order.by = 'freq',
        sets.x.label = '# of multifunctional papers \n with each ES type',
        mainbar.y.label = 'Number of papers'
        )

pdf(file = 'round2_metareview/analyze_data/ES_type_panel/fig_files/multifun_upset.pdf')
pl2
dev.off()



dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>%
  group_by(Title) %>%
  summarise(count = n()) %>%
  mutate(multifun = case_when(
    count > 1 ~ 'Yes',
    count <= 1 ~ 'No'
  )) %>%
  group_by(count, multifun) %>%
  summarise(num_papers = n()) %>%
  ungroup() %>%
  mutate(proportion = num_papers/sum(num_papers)) %>% 
  ggplot(aes(x = multifun, y = proportion, group = count, fill = count)) +
  geom_col(position = 'dodge') +
  xlab('Multifunctional?') +
  ylab('Proportion of papers') +
  coord_flip() +
  labs(fill = 'Number of ES types considered') +
  theme_bw()


dat %>%
  filter(abbr=='Yclass') %>%
  filter(!is.na(clean_answer)) %>% #removes the non-checked service bins
  dplyr::select(Title, ES) %>%
  group_by(Title) %>%
  summarise(count = n()) %>%
  mutate(multifun = case_when(
    count > 1 ~ 'Yes',
    count <= 1 ~ 'No'
  )) %>%
  group_by(count, multifun) %>%
  summarise(num_papers = n()) %>%
  ungroup() %>%
  mutate(proportion = num_papers/sum(num_papers)) %>% 
  ggplot(aes(x = fct_rev(multifun), y = proportion, fill = count)) +
  geom_col() +
  xlab('Multiple ecosystem service types?') +
  ylab('Proportion of papers') +
  coord_flip() +
  labs(fill = 'Number of ES types considered') +
  theme_bw()

ggsave('round2_metareview/analyze_data/ES_type_panel/fig_files/multifun_yesno.pdf', width = 5, height = 5, dpi = 'retina')




# Notes: might need to rethink how to show the upset plots as proportions (not
# possible in UpSetR), might be able to do manually, or put together a bar chart
# manually that shows exactly what we want it to but that would take some manual
# construction




# chord diagram - https://www.r-graph-gallery.com/chord-diagram heavily referenced
# packages needed: (code loads these at top of script)
# library(chorddiag)
# library(htmlwidgets)
# library(webshot)
# webshot::install_phantomjs()

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
withr::with_dir('round2_metareview/analyze_data/ES_type_panel/fig_files/', saveWidget(p_int, file="chord_diag_interactive.html"))




# save the static version (somewhat simplified, excluding rare connections)
adj_mat = t(site_by_es) %*% site_by_es   # cross product translates from site by ES to cooccurrences of ES's
diag(adj_mat) = 0 # make diagonal zero
adj_mat = replace(adj_mat, adj_mat < 5, 0) # can be used to exclude rare connections
p_static = chorddiag(adj_mat, 
              groupnameFontsize = 14, 
              showTicks = F, # show ticks with counts on circle edge
              groupPadding = 5, # distance of names/segments from each other
              groupnamePadding = 5 # distance of names from outside of circles
)
#save static
withr::with_dir('round2_metareview/analyze_data/ES_type_panel/fig_files/', saveWidget(p_static, file="chord_diag_static.html"))



# Make a webshot in pdf : high quality but can not choose printed zone
webshot("round2_metareview/analyze_data/ES_type_panel/fig_files/chord_diag_static.html" , "round2_metareview/analyze_data/ES_type_panel/fig_files/chord_diag_static.pdf", delay = 0.2)



