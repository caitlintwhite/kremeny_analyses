library(tidyverse)
library(ggVennDiagram) #devtools::install_github("gaospecial/ggVennDiagram")

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')

num_papers = dat %>% 
  pull(Title) %>%
  unique() %>%
  length()

driv_tf_bytitle = dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_group) %>% 
  unique() %>%
  filter(!is.na(clean_group)) %>% # two NAs snuck through somehow, I checked and there were always other drivers
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE) 

venn_list = list(
  biotic = driv_tf_bytitle %>% filter(Biotic == TRUE) %>% pull(Title) %>% as.character(), 
  human = driv_tf_bytitle %>% filter(Human == TRUE) %>% pull(Title) %>% as.character(),
  environmental = driv_tf_bytitle %>% filter(Environmental == TRUE) %>% pull(Title) %>% as.character()
)
  
ggVennDiagram(venn_list, category.names = c('Biotic drivers','Human drivers','Environmental drivers'), label = 'percent', color = 'darkgray', show.legend = FALSE) 

ggsave('round2_metareview/analyze_data/Drivers_nonsankey/fig_files/drivers_venn.pdf', width = 5, height = 5, dpi = 'retina')




