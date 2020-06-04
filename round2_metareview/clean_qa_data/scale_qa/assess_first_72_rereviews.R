library(dplyr)

anyunk = read.csv('round2_metareview/clean_qa_data/scale_qa/grants_review_anyunk.csv')

random30 = read.csv('round2_metareview/clean_qa_data/scale_qa/random_30_rereview.csv')


anyunk %>% 
  filter(Title %in% random30$x) %>%
  select(Title, MultiScale) %>%
  write.csv(file = 'round2_metareview/clean_qa_data/scale_qa/random30_multiscale_orig.csv', row.names = F)





agree = read.csv('round2_metareview/clean_qa_data/scale_qa/prelim_agreement_multiscale.csv')

cm = confusionMatrix(agree$Multiscale_orig, agree$Multiscale_new)
cm$table #reference is new, prediction is old
# new yes's when old was no: 10
# new no's when old was yes: 4
# accuracy was 80%
# new: 25/72 = 34% yes
# old: 19/72 = 26% yes






