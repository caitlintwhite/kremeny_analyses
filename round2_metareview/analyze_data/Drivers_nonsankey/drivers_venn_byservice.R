# Driver types intersected with service types, goal is to have a venn diagram
# for each service type broken down by driver types within that service type.


library(tidyverse)
library(ggVennDiagram) #devtools::install_github("gaospecial/ggVennDiagram")

dat = read.csv('round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv') %>%
  filter(version=='final')


driv_byserv = dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_group, ES) %>% 
  unique() %>%
  filter(!is.na(clean_group)) %>% 
  mutate(pres_holder = TRUE) %>%
  pivot_wider(id_cols = c('Title','ES'), names_from = 'clean_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE)


# Make venn diagram for each ES type
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
    
  ggsave(filename = paste0('round2_metareview/analyze_data/Drivers_nonsankey/fig_files/drivvenn_byservice/drivvenn_byserv_',es,'.pdf'), width=6, height=5, dpi='retina')
}

  
  
  
