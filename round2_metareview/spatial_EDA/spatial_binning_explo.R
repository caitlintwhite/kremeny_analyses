library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(igraph)
library(plotly)



df = read.csv("round2_metareview/data/cleaned/prelim_singlereview.csv")

# get the titles that made it through the first few exclusion questions
non_excl_titles = df %>% 
  filter(qnum =='Q3', !answer=='Yes') %>%
  select(Title)

spat = df %>% 
  filter(Title %in% non_excl_titles$Title) %>%
  filter(qnum %in% c("Q8","Q9","Q10","Q11"))


# Disregarding plot vs site differences, what are the spatial scales at which
# the paper worked
spat %>% 
  filter(qnum == 'Q9', !is.na(answer), !answer==0, !is.na(Group)) %>% 
  group_by(Title) %>%
  summarise(scales = paste0(unique(Group), collapse = ','))

x[grep("100m", x$scales),]

match("100m", x$scales)

spat %>% 
  filter(qnum == 'Q9', !is.na(answer), !answer==0, !is.na(Group)) %>% 
  group_by(Title) %>%
  summarise(scales = paste0(unique(Group), collapse = ',')) %>%
  separate_rows(scales, sep = ",") %>%
  group_by(scales) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(scales = factor(scales, levels = c('25m','50m','100m','500m','1km','10km', '100km','1000km','100Mgm','100Gm','101Gm','unk'))) 
  

#this one double counts papers with multiple scales listed  
spat %>% 
  filter(qnum == 'Q9', !is.na(answer), !answer==0, !is.na(Group)) %>% 
  group_by(Title) %>%
  summarise(scales = paste0(unique(Group), collapse = ',')) %>%
  separate_rows(scales, sep = ",") %>% 
  mutate(scales_binned = case_when(
    scales %in% c('25m','50m','100m','500m', '1km') ~ 'Small',
    scales %in% c('10km', '100km', '1000km') ~ 'Medium',
    scales %in% c('100Mgm','100Gm','101Gm') ~ 'Large',
    scales == 'unk' ~ 'unk'
  )) %>%
  group_by(scales_binned) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(scales_wordy = case_when(
    scales_binned == 'Small' ~ 'Within communities',
    scales_binned == 'Medium' ~ 'Across communities',
    scales_binned == 'Large' ~ 'Regional to Global',
    scales_binned == 'unk' ~ 'unk'
  )) %>%
  mutate(scales_binned = factor(scales_binned, levels = c('Small', 'Medium', 'Large', 'unk'))) %>%
  mutate(scales_wordy = factor(scales_wordy, levels = c('Within communities', 'Across communities', 'Regional to Global', 'unk'))) %>%
  ggplot(aes(x = scales_wordy, y = count, label = count)) +
  geom_col() +
  geom_label() +
  xlab('Scale of study') +
  ylab('Number of papers')


# this plot only shows one record for each paper, and includes info on how many papers had multiple scales listed
smalls = c('25m','50m','100m','500m', '1km')
mediums = c('10km', '100km', '1000km')
larges = c('100Mgm','100Gm','101Gm')

scale_fig_df = spat %>% 
  filter(qnum == 'Q9', !is.na(answer), !answer==0, !is.na(Group)) %>% 
  group_by(Title) %>%
  summarise(scales = paste0(unique(Group), collapse = ',')) %>%
  separate_rows(scales, sep = ",") %>% 
  mutate(scales_binned = case_when(
    scales %in% smalls ~ 'Small',
    scales %in% mediums ~ 'Medium',
    scales %in% larges ~ 'Large',
    scales == 'unk' ~ 'unk'
  )) %>%
  group_by(Title) %>%
  summarise(cross_grps = paste0(unique(scales_binned), collapse = ',')) %>% 
  ungroup() %>%
  group_by(cross_grps) %>%
  summarise(count = n()) %>%
  mutate(Scale_groups = gsub("unk,","",cross_grps)) %>%
  mutate(Scale_groups = gsub(",unk","",Scale_groups)) %>%
  group_by(Scale_groups) %>%
  summarise(count = sum(count)) %>%
  mutate(Scale_groups = factor(Scale_groups, levels = c("Small", "Small,Medium","Medium","Medium,Large","Large","Small,Large","Small,Medium,Large","unk")))


key_df = data.frame("Scale_binned" = c('Small','Medium', 'Large'), "scales" = c(paste0(smalls,collapse = ","), paste0(mediums,collapse = ","), paste0(larges,collapse = ",")))
print(key_df)
print(arrange(scale_fig_df, Scale_groups))
  

cross_sc_tab = spat %>% 
  filter(qnum == 'Q9', !is.na(answer), !answer==0, !is.na(Group)) %>% 
  group_by(Title) %>%
  summarise(scales = paste0(unique(Group), collapse = ',')) %>%
  separate_rows(scales, sep = ",") %>% 
  mutate(scales_binned = case_when(
    scales %in% c('25m','50m','100m','500m', '1km') ~ 'Small',
    scales %in% c('10km', '100km', '1000km') ~ 'Medium',
    scales %in% c('100Mgm','100Gm','101Gm') ~ 'Large',
    scales == 'unk' ~ 'unk'
  )) %>%
  group_by(Title) %>%
  summarise(cross_grps = paste0(unique(scales_binned), collapse = ',')) %>% 
  ungroup() %>%
  group_by(cross_grps) %>%
  summarise(count = n()) %>%
  mutate(cross_grps_nounk = gsub("unk,","",cross_grps)) %>%
  mutate(cross_grps_nounk = gsub(",unk","",cross_grps_nounk)) %>%
  group_by(cross_grps_nounk) %>%
  summarise(count = sum(count)) %>%
  mutate(cross_grps_nounk = factor(cross_grps_nounk, levels = c("Small", "Small,Medium","Medium","Medium,Large","Large","Small,Large","Small,Medium,Large","unk"))) %>%
  filter(!cross_grps_nounk %in% c("Small,Medium,Large","unk")) %>%
  separate(cross_grps_nounk, into = c('scale1','scale2'), sep = ",") %>%
  mutate(scale2 = ifelse(is.na(scale2),scale1,scale2))
  

# matrix for heatmap
mat = matrix(nrow = length(unique(cross_sc_tab$scale1)), ncol = length(unique(cross_sc_tab$scale1)), dimnames = list(unique(cross_sc_tab$scale1),unique(cross_sc_tab$scale2)))

for (i in 1:nrow(mat)) {
  r_ = unique(cross_sc_tab$scale1)[i]
  for(j in 1:i){
    c_ = unique(cross_sc_tab$scale2)[j]
    mat[r_,c_] = cross_sc_tab %>% filter(scale1 == r_, scale2 == c_) %>% pull(count)
  }
  
}

#p1 = 
  t(mat) %>% 
  melt() %>%
  mutate(Var1 = factor(Var1, levels = c("Small", "Medium","Large")),
         Var2 = factor(Var2, levels = c("Small", "Medium","Large")),
         label_ = ifelse(Var1==Var2, paste0('only ', Var1), NA)) %>%
  ggplot(aes(x = Var1, y = Var2, fill = value, label = label_)) +
  geom_raster() +
  geom_label() + # doesn't work with plotly
  theme(axis.text.x = element_text(angle = 45)) +
  labs(fill = "Number of papers", x = "", y = "", title = "Scales of study") +
  theme_bw() 
# the numbers from this plot should be able to directly fill in the numbers from the scale plot
# as long as we remember to add the unknown scale and s-m-l numbers, since they don't fit on this plot


#ggplotly(p1)


scale_fig_table = spat %>% 
    filter(qnum == 'Q9', !is.na(answer), !answer==0, !is.na(Group)) %>% 
    group_by(Title) %>%
    summarise(scales = paste0(unique(Group), collapse = ',')) %>%
    separate_rows(scales, sep = ",") %>% 
    mutate(scales_binned = case_when(
      scales %in% c('25m','50m','100m','500m', '1km') ~ 'Small',
      scales %in% c('10km', '100km', '1000km') ~ 'Medium',
      scales %in% c('100Mgm','100Gm','101Gm') ~ 'Large',
      scales == 'unk' ~ 'unk'
    )) %>%
    group_by(Title) %>%
    summarise(cross_grps = paste0(unique(scales_binned), collapse = ',')) %>% 
    ungroup() %>%
    group_by(cross_grps) %>%
    summarise(count = n()) %>%
    mutate(cross_grps_nounk = gsub("unk,","",cross_grps)) %>%
    mutate(cross_grps_nounk = gsub(",unk","",cross_grps_nounk)) %>%
    group_by(cross_grps_nounk) %>%
    summarise(count = sum(count))




### including type of study ###
# gets especially complicated since lots of studies have multiple methods_used, so it might be necessary to keep them in multiple categories
# need a more specific goal to make plots and explore questions, otherwise it gets confusing to interpret - can discuss

methods_used_df = df %>%
  filter(qnum=="Q6", !abbr %in% c('GenInfo','MethodsNotes')) %>%
  separate_rows(answer, sep = ",") %>%
  mutate(answer = ifelse(answer=="Observational (Includes data observed in situ OR via remote sensing", "Observational", as.character(answer))) %>%
  mutate(answer = ifelse(answer==" if used directly)", NA, as.character(answer))) %>%
  group_by(Title) %>%
  summarise(methods_used = paste0(unique(answer), collapse = ","))


smalls = c('25m','50m','100m','500m', '1km')
mediums = c('10km', '100km', '1000km')
larges = c('100Mgm','100Gm','101Gm')

scale_fig_df_methods = spat %>% 
  filter(qnum == 'Q9', !is.na(answer), !answer==0, !is.na(Group)) %>% 
  group_by(Title) %>%
  summarise(scales = paste0(unique(Group), collapse = ',')) %>%
  left_join(methods_used_df, by = "Title") %>%
  separate_rows(scales, sep = ",") %>%
  mutate(methods_used = gsub(",NA","",methods_used)) %>%
  mutate(scales_binned = case_when(
    scales %in% smalls ~ 'Small',
    scales %in% mediums ~ 'Medium',
    scales %in% larges ~ 'Large',
    scales == 'unk' ~ 'unk'
  )) %>%
  group_by(Title) %>%
  summarise(cross_grps = paste0(unique(scales_binned), collapse = ','), 
            methods_used = paste0(unique(methods_used), collapse = ";")) %>% 
  ungroup() %>%
  group_by(cross_grps, methods_used) %>%
  summarise(count = n()) %>%
  mutate(Scale_groups = gsub("unk,","",cross_grps)) %>%
  mutate(Scale_groups = gsub(",unk","",Scale_groups)) %>%
  group_by(Scale_groups, methods_used) %>%
  summarise(count = sum(count)) %>%
  ungroup() %>%
  mutate(Scale_groups = factor(Scale_groups, levels = c("Small", "Small,Medium","Medium","Medium,Large","Large","Small,Large","Small,Medium,Large","unk")))


print(arrange(scale_fig_df_methods, Scale_groups))

scale_fig_df_methods %>%
  mutate(methods_used = ifelse(methods_used %in% c('Experimental', 'Observational','Model/Data Simulation', 'Social survey/Interview'), methods_used, 'Other/Multiple')) %>%
  ggplot(aes(x = Scale_groups, y = count, fill = methods_used)) +
  geom_col() +
  xlab('Scale of study') +
  ylab('Number of papers') +
  theme_bw()


#for the unk studies
spat %>% 
  filter(qnum == 'Q9', !is.na(answer), !answer==0, !is.na(Group)) %>% 
  group_by(Title) %>%
  summarise(scales = paste0(unique(Group), collapse = ',')) %>%
  left_join(methods_used_df, by = "Title") %>%
  separate_rows(scales, sep = ",") %>%
  mutate(methods_used = gsub(",NA","",methods_used)) %>%
  mutate(scales_binned = case_when(
    scales %in% smalls ~ 'Small',
    scales %in% mediums ~ 'Medium',
    scales %in% larges ~ 'Large',
    scales == 'unk' ~ 'unk'
  )) %>% 
  group_by(Title) %>%
  mutate(scales_binned = paste0(scales_binned, collapse = ",")) %>%
  filter(scales_binned == 'unk') %>%
  group_by(methods_used) %>%
  summarise(unk_count = n()) %>%
  mutate(methods_used = fct_reorder(methods_used, -unk_count)) %>%
  ggplot() +
  geom_col(aes(x = methods_used, y = unk_count)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  ylab("Number of papers") +
  ggtitle("For papers with unknown spatial scale, what type?")
  
  


  mutate(blank = 'X') %>%
  ggplot() +
  geom_col(aes(x = blank, y = unk_count, fill = methods_used))

  
  
  
  
  
  
  
  
  
