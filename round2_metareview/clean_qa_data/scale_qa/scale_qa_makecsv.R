### consolidates notes and other variables to do the first round of qa/qc on the
### scale questions - looking at the notes and seeing if they make sense ####


library(tidyverse)

sc_notes = read.csv("round2_metareview/clean_qa_data/needs_classreview/scalenotes_review.csv")

# consolidate scalenotes_review
scale_notes = sc_notes %>%
  group_by(Title, Init) %>%
  summarise(ScaleNotes = paste0(unique(ScaleNotes),collapse = ";"),
            GenInfo = paste0(unique(GenInfo),collapse = ";"),
            MultiScale = paste0(unique(MultiScale), collapse = ";"),
            nothing_entered = paste0(unique(nothing_entered), collapse = ";"),
            Connect = paste0(unique(Connect), collapse = ";"),
            KT4_scale = paste0(unique(KT4_scale), collapse = ";"),
            scales = paste0(unique(interval_abbr), collapse = ";")
            ) %>%
  select(Title, MultiScale, scales, KT4_scale, nothing_entered, ScaleNotes, GenInfo, Init) 


### add in methods variables to (esp for unk) ###
df = read.csv("round2_metareview/data/cleaned/prelim_singlereview.csv")

methods_used = df %>%
  filter(qnum=="Q6", !abbr %in% c('GenInfo','MethodsNotes')) %>%
  separate_rows(answer, sep = ",") %>%
  mutate(answer = ifelse(answer=="Observational (Includes data observed in situ OR via remote sensing", "Observational", as.character(answer))) %>%
  mutate(answer = ifelse(answer==" if used directly)", NA, as.character(answer))) %>%
  group_by(Title, Init) %>%
  summarise(methods_used = paste0(unique(answer), collapse = ",")) %>%
  mutate(methods_used = gsub(",NA","",methods_used)) 

#methods notes
methods_other = df %>%
  filter(qnum=="Q6", abbr == 'MethodsNotes') %>%
  select(Title, Init, answer) %>%
  filter(!is.na(answer))

gen_methods_notes = df %>%
  filter(qnum=="Q6", abbr == 'GenInfo') %>%
  select(Title, Init, answer) %>%
  filter(!is.na(answer))

# all methods
full_methods = methods_used %>%
  left_join(methods_other, by = c('Title','Init')) %>%
  rename(if_other_notes = answer) %>%
  left_join(gen_methods_notes, by = c('Title','Init')) %>%
  rename(gen_methods_notes = answer)


# 192 total rows
#58 with only unk scale
#104 with any scale unk

#### For GRANT ####
#104 rows
any_unk_scale_methods = scale_notes %>%
  filter(grepl("unk", scales)) %>%
  left_join(full_methods, by = c('Title','Init'))

write.csv(any_unk_scale_methods, file = "round2_metareview/clean_qa_data/scale_qa/grants_review_anyunk.csv")


#### For JULIE ####
#88 rows
julies = scale_notes %>%
  filter(!grepl("unk", scales)) 

julies_withmethods = julies %>%
  left_join(full_methods, by = c('Title','Init'))

write.csv(julies, file = "round2_metareview/clean_qa_data/scale_qa/julies_review_nounk.csv")
write.csv(julies_withmethods, file = "round2_metareview/clean_qa_data/scale_qa/julies_review_nounk_withmethods.csv")



