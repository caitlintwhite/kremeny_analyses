library(dplyr)

# list of excluded titles from other questions
excl_titles = read.csv('round2_metareview/data/intermediate/round2_excluded.csv')

# list of titles we've already re-reviewed
already_reviewed = read.csv('round2_metareview/clean_qa_data/scale_qa/already_reviewed_27May.csv')


# need to check that all of the titles are included in the prelim_singlereview.csv
r2_double = read.csv('round2_metareview/data/intermediate/round2_doublereviewed_tidy.csv')

doub_titles = r2_double %>% 
  pull(Title) %>%
  unique()

r2_single = read.csv('round2_metareview/data/intermediate/round2_prelim_singlereview.csv')

sing_titles = r2_single %>% 
  pull(Title) %>%
  unique()

data.frame(doub_titles) %>%
  filter(!doub_titles %in% sing_titles)
# 0 titles in double review csv that are not in the single review, so it is a complete list (almost certainly)
rm(list = c('r2_double', 'r2_single'))


# now make a list of the titles that we have not already re-reviewed
titles_to_be = read.csv('round2_metareview/data/intermediate/round2_prelim_singlereview.csv') %>%
  dplyr::select(Title) %>%
  unique() %>%
  filter(!Title %in% excl_titles$Title) %>% # not excluded
  filter(!Title %in% already_reviewed$Title) %>% # not already reviewed
  pull(Title)


split_list = split(titles_to_be, f = c('Grant','Julie'))  
split_list$Grant %>% write.csv(file = 'round2_metareview/clean_qa_data/scale_qa/grant_todo_list.csv', row.names = F)
split_list$Julie %>% write.csv(file = 'round2_metareview/clean_qa_data/scale_qa/julie_todo_list.csv', row.names = F)

  
