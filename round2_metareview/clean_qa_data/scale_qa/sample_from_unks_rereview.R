library(dplyr)

anyunk = read.csv('round2_metareview/clean_qa_data/scale_qa/grants_review_anyunk.csv')

excl = read.csv('round2_metareview/data/intermediate/round2_excluded.csv')

anyunk %>% filter(!Title %in% excl$Title) %>% pull(Title) %>% sample(size = 30) %>% write.csv(file = 'round2_metareview/clean_qa_data/scale_qa/random_30_rereview.csv', row.names = F)
