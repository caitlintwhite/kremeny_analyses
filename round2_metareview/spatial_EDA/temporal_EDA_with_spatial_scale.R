library(tidyverse)
library(reshape2)
library(plotly)



data_path = "round2_metareview/data/cleaned/prelim_singlereview.csv"
df = read.csv(data_path)
non_excl_titles = df %>% 
  filter(qnum =='Q3', !answer=='Yes') %>%
  select(Title)
spat = df %>% 
  filter(Title %in% non_excl_titles$Title) %>%
  filter(qnum %in% c("Q8","Q9","Q10","Q11"))
smalls = c('25m','50m','100m','500m', '1km')
mediums = c('10km', '100km', '1000km')
larges = c('100Mgm','100Gm','101Gm')



time_trends_titles = df %>%
  filter(qnum == "Q7", abbr=='TimeTrends', answer=="Yes (e.g., a site sampled multiple points in time, a model with dynamics)") %>%
  pull(Title)

numyears_df = df %>% 
  filter(qnum == "Q7", abbr == 'YrsData', Title %in% time_trends_titles) %>%
  select(Title, answer) %>%
  rename(num_years = answer)

methods_used_df = df %>%
  filter(qnum=="Q6", !abbr %in% c('GenInfo','MethodsNotes')) %>%
  separate_rows(answer, sep = ",") %>%
  mutate(answer = ifelse(answer=="Observational (Includes data observed in situ OR via remote sensing", "Observational", as.character(answer))) %>%
  mutate(answer = ifelse(answer==" if used directly)", NA, as.character(answer))) %>%
  group_by(Title) %>%
  summarise(methods_used = paste0(unique(answer), collapse = ","))


spat_temp_methods_df = spat %>% 
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
  mutate(cross_grps = gsub("unk,","",cross_grps)) %>%
  mutate(cross_grps = gsub(",unk","",cross_grps)) %>%
  left_join(numyears_df, by = 'Title') %>%
  left_join(methods_used_df, by = 'Title') %>%
  rename(spat_scale = cross_grps) %>%
  mutate(methods_used = gsub(",NA","",methods_used)) %>%
  mutate(spat_scale = gsub("unk", NA, spat_scale)) %>%
  mutate(num_years = factor(num_years, levels = c('1 year or less','2–5 years','6–10 years','10+ years'))) %>%
  mutate(spat_scale = factor(spat_scale, levels = c("Small", "Small,Medium","Medium","Medium,Large","Large","Small,Large","Small,Medium,Large","unk")))




# for each temp scale, what is spatial scale?
spat_temp_methods_df %>% 
  group_by(methods_used,num_years,spat_scale) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = num_years, y = count)) +
  geom_bar(position = 'fill', stat='identity', aes(fill = spat_scale)) +
  geom_label(data = spat_temp_methods_df %>% group_by(num_years) %>% summarise(count = n()), 
             aes(label = count, y = 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  ylab('Proportion of studies')

  #long term studies have higher proporiton of larger scale studies, fewer small scale studies, more unk

# for each spatial scale, what is temp scale?
spat_temp_methods_df %>% 
  group_by(methods_used,num_years,spat_scale) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = spat_scale, y = count)) +
  geom_bar(position = 'fill', stat='identity', aes(fill = num_years)) +
  geom_label(data = spat_temp_methods_df %>% group_by(spat_scale) %>% summarise(count = n()), 
             aes(label = count, y = 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  ylab('Proportion of studies')

#long term studies have higher proporiton of larger scale studies, fewer small scale studies, more unk


# for each type of study (methods), what is temp scale?
spat_temp_methods_df %>%
  group_by(methods_used,num_years,spat_scale) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = methods_used, y = count)) +
  geom_bar(position = 'fill', stat='identity', aes(fill = num_years)) +
  geom_label(data = spat_temp_methods_df %>% group_by(methods_used) %>% summarise(count = n()), 
             aes(label = count, y = 0)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  ylab('Proportion of studies')



#position="fill", stat="identity"

# might be nice to look at relative counts between scales, types


  
  