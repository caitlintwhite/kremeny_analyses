### check new No's to old Yes's ###
library(dplyr)
library(caret)


excl_titles = read.csv('round2_metareview/data/intermediate/round2_excluded.csv')


old_multi = 
  read.csv('round2_metareview/data/intermediate/round2_prelim_singlereview.csv') %>% 
  filter(qnum=='Q8') %>%
  group_by(Title) %>%
  summarise(old_multiscale = answer) %>%
  filter(!Title %in% excl_titles$Title) %>%
  mutate(old_multiscale = as.character(old_multiscale))


new_dat = read.csv('round2_metareview/clean_qa_data/scale_qa/comp_rereview.csv')

full_multi = new_dat %>%
  dplyr::select(Title, nested_answer) %>%
  left_join(old_multi, by = 'Title') 

confusionMatrix(as.factor(full_multi$old_multiscale), as.factor(full_multi$nested_answer))
#reference is new, prediction is old
#only 8 old yes's are new no's

full_multi %>%
  filter(nested_answer=='No', old_multiscale=='Yes') %>%
  pull(Title)






