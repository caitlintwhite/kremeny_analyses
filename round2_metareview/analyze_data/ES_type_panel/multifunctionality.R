library(tidyverse)
library(UpSetR)
library(chorddiag)
library(htmlwidgets)
library(webshot)
webshot::install_phantomjs()
library(VennDiagram)

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




# Looking for highly represented triples (in addition to the doubles)
choose(16,3) #560 potential combinations of services

# the plan: make a dataframe of all possible 3-ES combos, fill in a column with the number of times they are all three studied inthe same paper (from site_by_es), order the column and examine the table

es_types = colnames(site_by_es)

es_3_combs = t(combn(es_types, 3)) # all combos of 3 es_types

trip_df = data.frame('ES1' = es_3_combs[,1], 'ES2' = es_3_combs[,2], 'ES3' = es_3_combs[,3], 'NumPapers' = NA)

for (i in 1:nrow(trip_df)){
  es1 = as.character(trip_df$ES1[i])
  es2 = as.character(trip_df$ES2[i])
  es3 = as.character(trip_df$ES3[i])
  
  npap = site_by_es %>%
    as.data.frame() %>%
    filter(!!as.symbol(es1)==TRUE & !!as.symbol(es2)==TRUE & !!as.symbol(es3)==TRUE) %>%
    nrow()
  trip_df$NumPapers[i] = npap
}

trip_df %>%
  arrange(-NumPapers) %>%
  View()



# Looking for highly represented quadruples (in addition to the doubles)
choose(16,4) #1820 potential combinations of services

# the plan: make a dataframe of all possible 3-ES combos, fill in a column with the number of times they are all three studied inthe same paper (from site_by_es), order the column and examine the table

es_types = colnames(site_by_es)

es_4_combs = t(combn(es_types, 4)) # all combos of 3 es_types

quad_df = data.frame('ES1' = es_4_combs[,1], 'ES2' = es_4_combs[,2], 'ES3' = es_4_combs[,3], 'ES4' = es_4_combs[,4], 'NumPapers' = NA)

for (i in 1:nrow(trip_df)){
  es1 = as.character(quad_df$ES1[i])
  es2 = as.character(quad_df$ES2[i])
  es3 = as.character(quad_df$ES3[i])
  es4 = as.character(quad_df$ES4[i])
  
  npap = site_by_es %>%
    as.data.frame() %>%
    filter(!!as.symbol(es1)==TRUE & !!as.symbol(es2)==TRUE & !!as.symbol(es3)==TRUE & !!as.symbol(es4)==TRUE) %>%
    nrow()
  quad_df$NumPapers[i] = npap
}

quad_df %>%
  arrange(-NumPapers) %>%
  View()



# Make a fancy venn diagram for each ES with all of its associated ES types (trial run)
# Not sure these will ever look good, leaving here for now
# uses the VennDiagram package

i = 1
curr_col = colnames(site_by_es)[i]

# translate to list format for VennDiagram
curr_col_list = site_by_es %>%
  as.data.frame() %>%
  filter(!!as.symbol(curr_col)==TRUE) %>%
  dplyr::select(-!!as.symbol(curr_col)) %>%
  as.list() %>%
  lapply(which) 
  


  lengths(curr_col_list)[order(lengths(curr_col_list), decreasing = TRUE)[1:3]] %>% # this takes the top 5, order returns the indices
    names() %>%
    curr_col_list[.]  %>% # grab just the list elements that are the top 5 in length
    venn.diagram(filename='round2_metareview/analyze_data/ES_type_panel/fig_files/pollination_multifun.tiff')



