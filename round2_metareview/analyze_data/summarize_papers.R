# short script to read in and lookup summary stats on abstracts/papers reviewed for ES
# > and other summary stats needed for methods section
# author(s): CTW

# notes:
# > feel free to add on as needed (e.g. more summary stats, figs, whatever needed)

# add-ons:
# jan 2022: +code section to pull references for discussion examples:
# > papers that mention uncertainty
# > papers that studied interactions or feedbacks
# > papers that used multiple methods
# > suss out consistency in indicators



# -- SETUP --
rm(list= ls())
library(tidyverse)
options(stringsAsFactors = F)
na_vals <- c("NA", "NaN", ".", "", " ", NA, NaN)


# -- REVIEW ROUND SUMMARY -----
# initial starting pool of ES abstracts
round1 <- read.csv("round1_exclusion/EcosystemServicesPapersNov2019.csv", na.strings = na_vals)
# note 1 abstract title appears twice
summary(duplicated(round1$Title)); length(unique(round1$Title)) # actually 1932 abstracts, not 1933
# was the duplicated title assigned to same person?
with(round1, EBIOReviewer[Title == Title[duplicated(Title)]]) # yes (Aislyn)
# what is the paper?
subset(round1, Title %in% Title[duplicated(Title)])
# > difference = one record has pubdate of Dec 2018, the other Apr 2019; otherwise title, authors, abstract, etc. identical
# how many assigned per person?
with(subset(round1, !duplicated(Title)), sapply(split(Title, EBIOReviewer), length)) # Aislyn reviewed 148 unique titles


# abstracts excluded in round 1
r1exclude <- read.csv("round1_exclusion/output/exclude_round1.csv", na.strings = na_vals)
# percent excluded?
length(unique(r1exclude$Title))/length(unique(round1$Title))

# abstracts kept after round1
r1kept <- read.csv("round1_exclusion/output/keep_round1.csv", na.strings = na_vals)
# percent kept
length(unique(r1kept$Title))/nrow(round1)
# of abstracts kept, papers assigned for round 2 (half of round 1 kept with the intent for each paper to have 2 reviewers)
r2assigned <- read.csv("round1_exclusion/output/review_assignments_round2_grpdsubset.csv", na.strings = na_vals)

# papers excluded in round 2
# note: this has multiple rows per paper for double reviewed papers (use version = "final" if only want 1 record)
r2exclude <- read.csv("round2_metareview/data/cleaned/ESqualtrics_r2exclude_cleaned.csv", na.strings = na_vals) 
# percent excluded
length(unique(r2exclude$Title))/length(unique(r2assigned$Title))


# compiled round1 and round 2 exclusion dataset to summarize reasons for exclusion in each round
# > note: this dataset only allows 1 reason for exclusion (exclusion datasets above give responses to all exclusion questions that reviewer answers)
allexclude <- read.csv("round2_metareview/data/cleaned/ESreview_allexcluded_allrounds.csv")
# how many papers excluded per round?
sapply(split(allexclude$title, allexclude$review_round), length)
# reason excluded?
group_by(allexclude, review_round, exclusion_reason) %>%
  summarise(nobs = length(title),
            grand_pct = (nobs/nrow(.))*100) %>%
  ungroup() %>%
  mutate(round_pct = ifelse(review_round == 1, (nobs/nrow(r1exclude))*100, (nobs/length(unique(r2exclude$Title)))*100)) %>%
  arrange(review_round, -nobs)

# lump reasons per round 2 and re-summarize
mutate(allexclude, grp_reason = ifelse(grepl("revi|meta|tool", exclusion_reason, ignore.case = T), "ReviewOnly",
                                       ifelse(grepl("^no", exclusion_reason, ignore.case = T), "no_efes",
                                              ifelse(grepl("Biod", exclusion_reason, ignore.case = T), "biodiv", "SocialOnly")))) %>%
  group_by(grp_reason) %>%
  summarise(nobs = length(title),
            grand_pct = (nobs/nrow(.))*100) %>%
  arrange(-nobs)

# cleaned full text retained dataset (before lulc adjustment)
qdat <- read.csv("round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv", na.strings = na_vals) 
# stack and write out all varnames for all datasets for metadata
varnames <- rbind(data.frame(source = "allexcluded", varnames = names(allexclude), type = sapply(allexclude, class)),
                  data.frame(source = "r1exclude", varnames = names(r1exclude), type = sapply(r1exclude, class))) %>%
  rbind(data.frame(source = "r2exclude", varnames = names(r2exclude),  type = sapply(r2exclude, class))) %>%
  rbind(data.frame(source = "r1keep", varnames = names(r1kept),  type = sapply(r1kept, class))) %>%
  rbind(data.frame(source = "r2keep", varnames = names(qdat),  type = sapply(qdat, class))) %>%
  rbind(data.frame(source = "r2_assigned", varnames = names(r2assigned),  type = sapply(r2assigned, class))) %>%
  rbind(data.frame(source = "r1_assigned", varnames = names(round1),  type = sapply(round1, class)))
#write.csv(varnames, "round2_metareview/data/intermediate/varnames4metadata.csv", row.names = F)



# -- DOUBLE REVIEW CONSISTENCY -----
# review consistency of double review answers for methods
# > runting et al. 2017 assigned 1 if agreed, 0.5 if partially agreed, and 0 if completely disagreed
# > because we're not really analyzing variables to specifics, gauge q12 based on 1) ES row filled in, and types of coarse drivers (Human, Biotic, or Env)
dbltidy <- subset(qdat, doublerev) %>%
  # want to look at clean answer in version == "orig" (qa'd independent answer)
  subset(version == "original") %>%
  # also want to drop 3 papers where paper kept but reviewers didn't agree on exclusion (because all answers to those will conflict)
  subset(!Title %in% with(qdat, Title[doublerev & version == "final" & grepl("keep paper", qa_note, ignore.case = T)]))

dblq12 <- subset(dbltidy, qnum == "Q12") %>%
  # need to note which cols are response 2
  mutate(response2 = grepl("Response 2", fullquestion) & abbr == "Response") %>%
  # drop response 2.. if something is in there, it means they filled out response 1
  subset(!response2) %>%
  #distill to ES and coarse group (ignore specifics of variable)
  group_by(Title, survey_order) %>%
  mutate(hasanswer = length(clean_answer[!is.na(clean_answer)])) %>%
  subset(hasanswer > 0) %>%
  arrange(Title, survey_order, Init) %>%
  ungroup() %>%
  mutate(markanswer = ifelse(is.na(clean_answer), 0, 1)) %>%
  dplyr::select(Title, Init, markanswer, abbr, ES, survey_order, Group, clean_group) %>%
  distinct() %>%
  # if both agree (answers provided), will have sum of 2 for given question, if not will only sum to 1
  group_by(Title, survey_order) %>%
  mutate(sumcheck = sum(markanswer)) %>%
  # can drop effect direct and Yclass bc we're not using those
  subset(!abbr %in% c("Yclass", "EffectDirect")) %>%
  ungroup() %>%
  # I think can drop individual reviewer and just look at the sumchecks
  dplyr::select(Title, survey_order, abbr, ES, clean_group, sumcheck) %>%
  distinct() %>%
  # drop clean_group if NA (means only 1 person answered answer in that row)
  subset(!(grepl("Driver", abbr) & is.na(clean_group)))
  

summary(as.factor(dblq12$sumcheck)) #yikes.. only a little more than 1/3rd of answers agree in q12 matrix
# we want to know where they are differing. is it on ES? on driver type?
# look separately by ES, response and driver?
dplyr::select(dblq12, Title, ES, sumcheck) %>%
  distinct() %>%
  group_by(Title, ES) %>%
  mutate(ignore = ifelse(length(sumcheck)==2 & sumcheck == 1, TRUE, FALSE)) %>%
  subset(!ignore) %>%
  ungroup() %>%
  group_by(Title) %>%
  mutate(score = ifelse(all(sumcheck == 1), 0, ifelse(length(unique(sumcheck)) > 1, 0.5, 1))) %>%
  dplyr::select(Title, score) %>%
  distinct() %>%
  ggplot(aes(as.factor(score))) +
  geom_bar() # note.. the ES barchart IS the same as congruence in response cateogry (if ES filled out, response is filled out)

ESagreement <- dblq12 %>%
  subset(abbr == "Response") %>%
  group_by(Title) %>%
  mutate(score = ifelse(all(sumcheck == 1), 0, ifelse(length(unique(sumcheck)) > 1, 0.5, 1))) %>%
  ungroup() %>%
  dplyr::select(Title, score) %>%
  distinct() %>%
  group_by(score) %>%
  summarize(pct = round((length(score)/34)*100, 2)) %>%
  ungroup() %>%
  mutate(q = "ES agreement") %>%
  spread(score, pct)


# look at driver congruence--do double reviewers choose the same driver types? (i guess looking with ES's where they agree? or maybe just outright drivers in any ES)
# method 1: just look at congruence where reviewers agree on the same ES
driversimilarity <- group_by(dblq12, Title, ES) %>%
  mutate(dropES = ES %in% ES[abbr == "Response" & sumcheck == 1]) %>%
  subset(!dropES) %>%
  # can drop response too
  subset(abbr != "Response")
# the way it should work (i think) is. if all same driver cats filled out (sumcheck == 2), then in agreement, if mix of 1s and 2s then partial, if all 1s, no agreement
driversimilarity %>%
  group_by(Title, ES, abbr) %>%
  mutate(score = str_flatten(sort(unique(sumcheck))),
         score = ifelse(score == "12", 0.5, ifelse(score == "1", 0, 1))) %>%
  ungroup() %>%
  dplyr::select(Title, ES, abbr, score) %>%
  distinct() %>%
  ggplot(aes(as.factor(score))) +
  geom_bar() +
  facet_wrap(~abbr)
# what about by paper (ignore ES, since we're selecting for ES's where reviewers agreed)
driversimilarity %>%
  group_by(Title, abbr) %>%
  mutate(score = str_flatten(sort(unique(sumcheck))),
         score = ifelse(score == "12", 0.5, ifelse(score == "1", 0, 1))) %>%
  ungroup() %>%
  dplyr::select(Title, abbr, score) %>%
  distinct() %>%
  ggplot(aes(as.factor(score))) +
  geom_bar() +
  facet_wrap(~abbr)

# pct agreement for drivers vs other driver answered (not considering ES)
driversim <- subset(dbltidy, qnum == "Q12") %>%
  subset(grepl("Driver", abbr)) %>%
  # drop ES -- we're just comparing drivers answered
  select(-c(ES, survey_order, id, qa_note, answer, varnum)) %>%
  # drop "Other" in abbr == Driver bc other driver is there
  subset(!(abbr == "Driver" & grepl("Other", clean_answer)) & !is.na(clean_answer)) %>%
  distinct() %>%
  group_by(Title, clean_answer, abbr, clean_group) %>%
  # can check if ppl wrote same answer for drivers (since standardized drivers..)
  mutate(count_inits = length(unique(Init))) %>%
  ungroup() %>%
  # if other driver has any answer count (don't compare answers verbatim)
  group_by(Title, abbr, clean_group) %>%
  mutate(count_inits = ifelse(abbr == "OtherDriver", length(unique(Init)), count_inits)) %>%
  ungroup()

# .. maybe check congruency within driver category since also assess general congruency across drivers
driversim2 <- select(driversim, Title, clean_group, abbr, count_inits) %>%
  group_by(Title, clean_group) %>%
  mutate(score = ifelse(all(count_inits == 2), 1, ifelse(all(count_inits == 1), 0, 0.5))) %>%
  ungroup() %>%
  select(Title, clean_group, score) %>%
  distinct() %>%
  # if no clean_group present, it means reviewers agreed that category does not have a driver
  spread(clean_group, score, fill = 1) %>%
  gather(clean_group, score, Biotic:Human) %>%
  group_by(clean_group, score) %>%
  summarize(pct = round(100*(length(Title)/34),2)) %>%
  ungroup() %>%
  mutate(q = "drivers") %>%
  spread(score, pct)

  
# do they agree on driver category? ignoring standardized driver answer vs. "other driver"
dblq12_catsimilarity <- subset(dbltidy, qnum == "Q12") %>%
  # need to note which cols are response 2
  mutate(response2 = grepl("Response 2", fullquestion) & abbr == "Response") %>%
  # drop response 2.. if something is in there, it means they filled out response 1
  subset(!response2) %>%
  # need to re-code otherdriver as driver to compare coarse driver cats
  mutate(abbr = recode(abbr, "OtherDriver" = "Driver")) %>%
  #distill to ES and coarse group (ignore specifics of variable)
  group_by(Title, abbr) %>%
  mutate(hasanswer = length(clean_answer[!is.na(clean_answer)])) %>%
  subset(hasanswer > 0) %>%
  arrange(Title, survey_order, Init) %>%
  ungroup() %>%
  mutate(markanswer = ifelse(is.na(clean_answer), 0, 1)) %>%
  dplyr::select(Title, Init, markanswer, abbr, ES, abbr, Group, clean_group) %>%
  distinct() %>%
  # if both agree (answers provided), will have sum of 2 for given question, if not will only sum to 1
  group_by(Title, ES, abbr, clean_group) %>%
  mutate(sumcheck = length(unique(Init[markanswer == 1]))) %>%
  ungroup() %>%
  group_by(Title, abbr, clean_group) %>%
  mutate(sumcheck_simple = length(unique(Init[markanswer == 1]))) %>%
  # can drop effect direct and Yclass bc we're not using those
  subset(!abbr %in% c("Yclass", "EffectDirect")) %>%
  ungroup() %>%
  # I think can drop individual reviewer and just look at the sumchecks
  dplyr::select(Title, abbr, ES, clean_group, sumcheck, sumcheck_simple) %>%
  distinct() %>%
  # drop clean_group if NA (means only 1 person answered answer in that row)
  subset(!(grepl("Driver", abbr) & is.na(clean_group)))
# > include ES .. within ES (where they agreed), did they pick the same coarse drivers
group_by(dblq12_catsimilarity, Title, ES) %>%
  mutate(dropES = ES %in% ES[abbr == "Response" & sumcheck != 2]) %>%
  subset(!dropES) %>%
  ungroup() %>%
  group_by(Title, abbr, ES) %>%
  mutate(score = str_flatten(sort(unique(sumcheck))),
         score = ifelse(score == "12", 0.5, ifelse(score == "1", 0, 1))) %>%
  ungroup() %>%
  subset(abbr == "Driver") %>%
  dplyr::select(Title, ES, score) %>%
  distinct() %>%
  ggplot(aes(as.factor(score))) +
  geom_bar() +
  facet_wrap(~ES)

# drop ES -- this is looking at, per paper, where same coarse driver categories selected (ignoring which ES row filled out)
dblq12_catsimilarity %>%
  subset(sumcheck >0) %>%
  group_by(Title, abbr) %>%
  mutate(score = str_flatten(sort(unique(sumcheck_simple))),
         score = ifelse(score == "12", 0.5, ifelse(score == "1", 0, 1))) %>%
  ungroup() %>%
  subset(abbr == "Driver") %>%
  dplyr::select(Title, abbr, score) %>%
  distinct() %>%
  ggplot(aes(as.factor(score))) +
  geom_bar()

drivercats <- dblq12_catsimilarity %>%
  subset(sumcheck >0) %>%
  group_by(Title, abbr) %>%
  mutate(score = str_flatten(sort(unique(sumcheck_simple))),
         score = ifelse(score == "12", 0.5, ifelse(score == "1", 0, 1))) %>%
  ungroup() %>%
  group_by(score) %>%
  summarise(pct = (length(score)/nrow(.))*100) %>%
  mutate(q = "driver category") %>%
  spread(score, pct)


# assess fixed responses in questions other than q12, no exclusion question [if it's in this dataset, it made it through], and no notes fields (only standardized answers [multi-choice options])
dblother <- subset(dbltidy, !qnum %in% c("Q12", "Q3") & !grepl("Notes|GenInfo", abbr)) %>%
  # here we go
  group_by(Title, survey_order) %>%
  mutate(sameanswer = length(unique(clean_answer))==1) %>%
  arrange(Title, survey_order, Init) %>%
  ungroup() %>%
  mutate(rowid = rownames(.))

# plot with no double counts
dplyr::select(dblother, Title, qnum, abbr, survey_order, sameanswer) %>%
  distinct() %>%
ggplot(aes(sameanswer)) +
  geom_bar() +
  facet_wrap(~abbr)
# think need to split answers on commas to compare partial matches..
# > also if answers for time trends and connectivity (yes/no) don't agree, don't consider discrepancies for subsequent related question
# > and don't match uncertainty verbatim, since it's write in. if there's a non-NA answer at all, it means reviewers agreed paper included some aspect of uncertainty
dblother$score <- ifelse(dblother$sameanswer, 1, 0)
for(t in unique(dblother$Title)){
  tempdat <- subset(dblother, Title == t & !sameanswer)
  r1 <- unique(tempdat$Init)[1]
  r2 <- unique(tempdat$Init)[2]
  for(s in unique(tempdat$survey_order)){
    ans1 <- unlist(with(tempdat, strsplit(clean_answer[survey_order == s & Init == r1], split = ","))) %>% trimws()
    ans2 <- unlist(with(tempdat, strsplit(clean_answer[survey_order == s & Init == r2], split = ","))) %>% trimws()
    compare <- any(ans1 %in% ans2) | any(ans2 %in% ans1)
    if(compare){
      # replace 0 with 0.5 for partial match
      dblother$score[dblother$rowid %in% with(tempdat, rowid[Title == t & survey_order == s])] <- 0.5
    }
    # address dependent questions
    if(s %in% c(30, 58)){
      prevanswer <- unique(tempdat$sameanswer[tempdat$survey_order == s-1])
      # if length is 0, it means previous answer agreed
      prevanswer <- length(prevanswer)==0
      if(!prevanswer){
        # if it's false, it means disagreed on first-part question
        dblother$score[dblother$rowid %in% with(tempdat, rowid[Title == t & survey_order == s])] <- 5 #5 = ignore
      }
     # if prevanswer is TRUE and one of the dependent answers is NA (forgot to answer, ignore)
      if(prevanswer & any(is.na(tempdat$clean_answer[tempdat$survey_order == s]))){
        dblother$score[dblother$rowid %in% with(tempdat, rowid[Title == t & survey_order == s])] <- 3 # 3 = one answer missing
      }
    }
    # address uncertainty write-in question (only gets score of 0 if one answer is NA)
    if(s == 211 & !any(is.na(with(tempdat, clean_answer[Title == t & survey_order == s])))){
      dblother$score[dblother$rowid %in% with(tempdat, rowid[Title == t & survey_order == s])] <- 1
    }
  }
}

# what does it look like now?
dplyr::select(dblother, Title, qnum, abbr, survey_order, score) %>%
  distinct() %>%
  ggplot(aes(as.factor(score))) +
  geom_bar() +
  facet_wrap(~survey_order + abbr)

dplyr::select(dblother, Title, qnum, abbr, survey_order, score) %>%
  distinct() %>%
  group_by(survey_order, abbr, score) %>%
  summarise(pct = round(length(score)/length(unique(dblq12$Title)),2)) %>%
  data.frame() %>%
  mutate(pct = round(100*pct)) %>%
  spread(score, pct, fill = "-")

# re-crunch dropping q's ppl forgot to answer and ones that shouldn't be considered 
otherqs <- dplyr::select(dblother, Title, qnum, abbr, survey_order, score) %>%
  distinct() %>%
  group_by(survey_order, abbr, score) %>%
  summarise(nobs = length(score)) %>%
  data.frame() %>%
  subset(score <= 1) %>%
  group_by(abbr) %>%
  mutate(totsum = sum(nobs)) %>%
  ungroup() %>%
  mutate(pct = round((nobs/totsum)*100,2)) %>%
  select(survey_order, abbr, score, pct) %>%
  spread(score, pct, fill = 0)
  
# compile all pcts and write out for reference
congruence <- rename(otherqs, q = abbr) %>%
  select(-survey_order) %>%
  rbind(ESagreement) %>%
  rbind(drivercats) %>%
  rbind(unite(driversim2, q, q, clean_group, sep =" "))

# what is the average congruence score across all questions assessed?
# take average by question, and then average those composite scores
congruence2 <- congruence %>%
  mutate(rowid = 1:nrow(.)) %>%
  gather(score, pct, "0":"1") %>%
  mutate(score2 = as.numeric(score) * pct) %>%
  group_by(rowid, q) %>%
  summarise(score = (sum(score2)/100)) %>%
  ungroup() %>%
  arrange(rowid)

mean(congruence2$score)


  
# check congruency in excluded papers (or kept but one person excluded). how many had reviewers agree (or one checked "yes" and another expressed doubt in comments), vs. ones where reviewers disagreed and LD/NBD reviewed
unique(with(r2exclude, Title[doublerev & flagged4review])) #15 titles checked by LD
unique(with(r2exclude, reviewer_exclusion_status[doublerev & flagged4review & version == "final"]))
# i'm saying 8 don't agree (1 reviewer says keep, the other exclude or doubts in survey notes)
# 6 have partial agreement:
## 2 papers agree on exclusion, but for different reasons
## 2 papers both have reviewers expressing doubts in notes (but neither excluded)
## 2 papers have 1 reviewer excluded by Q3, and other expressed doubts in survey notes (but still filled out survey)
# 15 agreed on reason for exclusion
# additionally, if look at kept qualtrics dataset, there are 3 double-reviewed papers that were kept ultimately, but initially reviewers disagreed on exclusion (LD reviewed the papers for final opinion)


# -- SUMMARIZE JOURNALS ----
# how many journals considered, what potential bias is there?
length(unique(round1$SourcePublication)) #128 journals for round 1
length(unique(round1$SourcePublication[round1$Number %in% r1kept$Number])) #98 journals made it through
length(unique(r2assigned$SourcePublication)) # 80 journals selected for round 2 review
length(unique(round1$SourcePublication[round1$Title %in% unique(qdat$Title)]))

# group titles by journal
r1jsum <- subset(round1, !duplicated(Title)) %>%
  group_by(SourcePublication) %>%
  summarise(papers = length(Title),
            pct = papers/nrow(round1),
            pct = 100*pct) %>%
  ungroup() %>%
  arrange(-pct) %>%
  mutate(rank = 1:nrow(.)) %>%
  rename_if(!grepl("^Source", names(.)), function(x) paste0(x,"_r1"))

r2jsum <- subset(r2assigned, !duplicated(Title)) %>%
  group_by(SourcePublication) %>%
  summarise(papers = length(Title),
            pct = papers/length((r2assigned$Title[!duplicated(r2assigned$Title)])),
            pct = 100*pct) %>%
  ungroup() %>%
  arrange(-pct) %>%
  mutate(rank = 1:nrow(.)) %>%
  rename_if(!grepl("^Source", names(.)), function(x) paste0(x,"_r2"))

finaljsum <- select(qdat, Title) %>%
  distinct() %>%
  left_join(select(original, Title, SourcePublication)) %>%
  group_by(SourcePublication) %>%
  summarise(papers = length(Title),
            pct = papers/length((r2assigned$Title[!duplicated(r2assigned$Title)])),
            pct = 100*pct) %>%
  ungroup() %>%
  arrange(-pct) %>%
  mutate(rank = 1:nrow(.)) %>%
  rename_if(!grepl("^Source", names(.)), function(x) paste0(x,"_final"))

jsum <- left_join(r1jsum, r2jsum) %>%
  left_join(finaljsum) %>%
  subset(rank_r1 %in% 1:10 | rank_r2 %in% 1:10 | rank_final %in% 1:10)


# -- THIRD PARTY REVIEW ----
third <- read.csv("round2_metareview/data/reviewer_revisions/excludenotes_review-COMPLETE.csv")
with(third, sapply(split(Title, exclude_LD), function(x) length(unique(x))))
length(unique(third$Title))



# -- SUMMARIZE PERCENT MODIFIED ANSWERS FOR FINAL LULC DATASET ------
# read in clean dataset that has lulc adjustments so can quantify how many answers/rows that mod impacted (in addition to other mods made)
qdat_lulc <- read.csv("round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned_lulc.csv", na.strings = na_vals)

# filter data to rows with answers or qa/lulc notes present
review_qdat <- subset(qdat_lulc, select = c(Title, Init, doublerev, version, answer, clean_answer, qnum, abbr, Group, clean_group, lulc_group, ES, qa_note, lulc_note, only_lulc)) %>%
  # remove rows that are empty in answer and clean_answer, keep anything with a qa_note or lulc_note
  filter(!(is.na(answer) & is.na(clean_answer)) | !is.na(qa_note) | !is.na(lulc_note)) %>%
  # remove empty notes for Q9
  filter(!(is.na(answer) & is.na(clean_answer) & qnum == "Q9"))

# -- 1. tally third party review -----
# count questions that had a third party review 
# > ak + nbd looked at methods
# > is + ld looked at ecosystem type
# > ld + nbd considered exclusion
# > ctw did third party review when original reviewers not available or responsive
third_party <- subset(review_qdat, !is.na(qa_note)) %>% # will have a note if modified
  # check if raw answer is same as clean answer, and if clean group changed
  mutate(same_answer = (answer == clean_answer) | is.na(answer) & is.na(clean_answer),
         same_group = (Group == clean_group) | (is.na(Group) & is.na(clean_group))) %>%
  replace_na(list(same_answer = FALSE, same_group = FALSE))


# what questions have notes?
distinct(third_party, qnum, abbr) %>%
  mutate(qnum = parse_number(qnum)) %>%
  arrange(qnum) # apparently every question has a note somewhere..

# look at example notes
unique(third_party$qa_note[third_party$qnum == "Q15"])
unique(third_party$qa_note[third_party$qnum == "Q9"])
unique(third_party$qa_note[grepl("reviewed|third party|agree|[+]|look[ed]? at", third_party$qa_note)])
unique(third_party$qa_note[!grepl("CTW", third_party$qa_note)])

# what are the unique notes when answer and groups are the same?
with(third_party, unique(qa_note[same_answer & same_group]))
# can remove rows that are "Infilled ES from Driver where 'Other' specified" and any notes questions (we didn't review those)
# also ignore Q8? or include as reviewed by third party.. bc question was completely redone (keep as reviewed)
ignore_third_party <- subset(third_party, grepl("Note|GenInfo", abbr) | 
                               (same_group & qa_note %in% "Infilled ES from Driver where 'Other' specified"))
third_party2 <- anti_join(third_party, ignore_third_party)
# summarize these reviews
tally_reviews <- dplyr::select(third_party2, Title, Init, version, qnum, abbr, ES, Group, answer, qa_note) %>%
  distinct() %>%
  group_by(qnum, abbr) %>%
  summarise(revnobs = length(qa_note))
refcounts <- dplyr::select(review_qdat,  Title, Init, version, qnum, abbr, ES, Group,answer, qa_note) %>%
  distinct() %>%
  group_by(qnum, abbr) %>%
  summarise(refnobs = length(qa_note))
# full join all to calculate % modified
summarize_revs <- full_join(tally_reviews, refcounts) %>%
  replace_na(list(revnobs = 0)) %>%
  mutate(pctmod = round((revnobs/refnobs)*100,2),
         # pretty table
         qnum = parse_number(qnum)) %>%
  arrange(qnum) %>%
  # join full question
  left_join(distinct(subset(qdat_lulc, qnum != "Q12", select = c(abbr, fullquestion)))) %>%
  # drop extent and nested notes because implicit in new questions
  subset(!(qnum == 8 & grepl("Notes", abbr)))
# bigger table, write out to put in data methods supplement
# > put in clean_qa_data/qafigs since it's a summary of cleaning actions
write.csv(summarize_revs, "round2_metareview/clean_qa_data/qafigs/summarize_thirdreview.csv", row.names = F)



# -- 2. pull records that were modified to tally: -----
# > notes were never modified, Q8 was never modified once GV and JL redid
modified_questions <- subset(review_qdat, !grepl("Notes|GenIn", abbr) & qnum != "Q8", 
                             # ignore lulc for counting mods before lulc adjustment
                             select = -c(lulc_group, lulc_note, only_lulc)) %>%
  distinct() %>%
  # track things things are different
  mutate(noted = !is.na(qa_note) & ((answer != clean_answer) | is.na(answer) & !is.na(clean_answer)), # does it have a QA note
         # does Group differ from clean_group?
         modified_group = ifelse(is.na(Group), FALSE, Group != clean_group),
         # does clean_answer differ from raw_answer (note: will be true for all Q12 variables split out and for double review bc of initials prefix)
         modified_answer = (answer != clean_answer) | (is.na(answer) & !is.na(clean_answer)) | (is.na(clean_answer) & !is.na(answer)),
         # does qa note indicate it was reviewed in any way?
         reviewed = grepl("reviewed|third party|agree|[+]", qa_note)) # note answers could be different because I removed a comma

# ignore answers that are same as clean_answer and clean_group did not change
ignore_questions <- subset(modified_questions, !modified_group | is.na(modified_group)) %>%
  # ignore (keep) what has same answer and doesn't have a qa note
  subset(!modified_answer & !noted)

# pull whatever still needs attention (isn't in ignore dataset)
modified_questions2 <- anti_join(modified_questions, ignore_questions)

# subset out more to ignore
ignore_questions <- rbind(ignore_questions, 
                          subset(modified_questions2, abbr %in% c("EffectDirect", "Yclass") & grepl("missing|No effect dire", qa_note) & modified_answer %in% c(NA, FALSE))) %>%
  rbind(subset(modified_questions2, (!noted & !reviewed & !modified_group))) %>%
  rbind(subset(modified_questions2, qa_note %in% c("Infilled ES from Driver where 'Other' specified", "Reviewer correction") & !modified_group)) %>%
  rbind(subset(modified_questions2, grepl("Reviewer correc",qa_note) & !grepl("inconsistent", qa_note))) %>%
  rbind(subset(modified_questions2, grepl("removed duplicate", qa_note, ignore.case = T))) %>%
  distinct()

# update filtered modified 
modified_questions2 <- anti_join(modified_questions, ignore_questions) # I think this is enough sorting

# distill to unique title-responseid-qnum-abbr-ES and tally that
tally_mods <- dplyr::select(modified_questions2, Title, Init, version, qnum, abbr, ES, Group, answer, qa_note) %>%
  distinct() %>%
  group_by(qnum, abbr) %>%
  summarise(modnobs = length(qa_note))

# full join all to calculate % modified
summarize_mods <- full_join(tally_mods, refcounts) %>%
  replace_na(list(modnobs = 0)) %>%
  mutate(pctmod = round((modnobs/refnobs)*100,2),
  # pretty table
  qnum = parse_number(qnum)) %>%
  arrange(qnum) %>%
  # join full question
  left_join(distinct(subset(qdat_lulc, qnum != "Q12", select = c(abbr, fullquestion))))
# bigger table, write out to put in data methods supplement
# > put in clean_qa_data/qafigs since it's a summary of cleaning actions
write.csv(summarize_mods, "round2_metareview/clean_qa_data/qafigs/summarize_modifications.csv", row.names = F)


# -- 3. summarize lulc recodes -----
# > just count rows for drivers and effect directs that got recoded
q12lulc <- subset(review_qdat, qnum == "Q12" & grepl("Driver|Effect", abbr))

totdrivers <- sum(grepl("Driver", q12lulc$abbr))
toteffects <- sum(grepl("Effect", q12lulc$abbr))

# how many rows added in dataset because multiple drivers present in clean_group partially recoded to lulc?
summarize_lulc <- q12lulc %>%
  #subset(q12lulc, grepl("Effect", abbr)) %>%
  # remove empty groups due to NA clean_answers (corrections to NA)
  subset(!is.na(clean_group)) %>%
  # count number of added rows due to lulc
  mutate(driver = ifelse(grepl("Driver", abbr), "driver", "effect"), 
    added = grepl("multiple", lulc_note, ignore.case = T),
         recoded = lulc_group == "LU_LC") %>%
  group_by(driver, added, recoded) %>%
  summarise(nobs = length(Title)) %>%
  ungroup() %>%
  group_by(driver) %>%
  mutate(totnobs = sum(nobs),
         prop = nobs/sum(nobs))
 
# 22% of driver answers and %15 effect direction answers recoded (includes rows duplicated because of presence of other drivers partially recoded)

# how many papers would lose Kremen Topic: Env because of lulc recode
nrow(subset(qdat_lulc, grepl("does not include env", lulc_note))) # 10 reviews
length(unique(qdat_lulc$ResponseId)) #341 unique reviews present
10/341 #3% of answers

# how many ESP types consolidated by GV's aggregation?
# he combines across and among species answers for ESP
summarise_esp <- subset(qdat_lulc, abbr == "ESP_type" & version == "final", select = c(Title, clean_answer)) %>%
  # split answers
  separate(clean_answer, into = paste0("X", 1:4), sep = "],") %>%
  gather(num, clean_answer, X1:X4) %>%
  subset(!is.na(clean_answer)) %>%
  mutate(adjust_answer = ifelse(grepl("Among|Across",clean_answer), "Community", clean_answer)) %>%
  arrange(Title, num)
summary(summarise_esp$adjust_answer == "Community") # 80 of 154 non-NA answers impacted
# how many titles did not have ESP
summary(unique(qdat_lulc$Title) %in% unique(summarise_esp$Title)) # 73 papers did not have an ESP
80/(73+154) #35% of answers impacted (35% of responses, including no ESP, were across or among species)
# how many unique titles have community? maybe counting boxes checked is double counting
length(unique(summarise_esp$Title[summarise_esp$adjust_answer == "Community"])) # 77 titles out of 200 that gave an answer (or 273 total)
77/200 # 38% of responses
77/273 # or 28% of responses if include NA as "no ESP" response



# -- PULL REFS FOR IN-TEXT EXAMPLES -----
# cols to keep
keepcols <- c("Title","Init", "version", "doublerev", "clean_answer", "fullquestion", "abbr", "qnum", "survey_order", "qa_note")
# make simple refdat (kept for round 2) for joining
simpleref <- distinct(dplyr::select(r2assigned, Comments:Title, SourcePublication, PublicationYear)) %>%
  rename(Round1_Comments = Comments)
# pull general notes for all
notes <- subset(qdat, version == "final" & abbr == "SurveyNotes" & !is.na(clean_answer), select = keepcols) %>%
  unite(abbr, qnum, abbr)
names(notes)[grep("clean_answer", names(notes))] <- unique(notes$abbr)

# 1. mentioned uncertainty ----
# Qualtrics Q23: "Note if there is anything interesting this paper does to assess uncertainty"

q23 <- subset(qdat, version== "final" & abbr == "Uncertainty" & !is.na(clean_answer), select = keepcols) %>%
  # remove those papers that did not discuss uncertainty
  subset(!grepl("No uncertainty|Did not consider", clean_answer)) %>%
  unite(abbr, qnum, abbr)
names(q23)[grep("clean_a", names(q23))] <- unique(q23$abbr)
q23 <- left_join(q23, notes[c("Title", "Q18_SurveyNotes")]) %>%
  # join refdat
  left_join(simpleref) %>%
  # clean up columns
  dplyr::select(Title, FirstAuthor:PublicationYear, Init, Q17_Uncertainty, Q18_SurveyNotes, Round1_Comments, qa_note) %>%
  # arrange by author for easier lookup
  arrange(FirstAuthor)
# write out
write_csv(q23, "round2_metareview/analyze_data/examples/ESuncertainty_examples.csv", na = "")
data_frame(q23[c("FirstAuthor", "PublicationYear", "Title")])


# 2. studied interactions or feedbacks ----
# Qualtrics Q21: "Does the paper measure feedbacks between levels?" 
# Q. Q22: "Are there explicit discussions of thresholds required for the provisioning of ES?"
q21 <- subset(qdat, version== "final" & abbr == "Feedbacks" & !is.na(clean_answer), select = keepcols) %>%
  # remove those papers that did not discuss uncertainty
  #subset(!grepl("No", clean_answer)) %>%
  unite(abbr, qnum, abbr, remove = F)
names(q21)[grep("clean_a", names(q21))] <- unique(q21$abbr)
names(q21)[grep("qa_", names(q21))] <- paste(unique(q21$qnum), "qa_note", sep = "_")
q22 <- subset(qdat, version== "final" & abbr == "Thresholds" & !is.na(clean_answer), select = keepcols) %>%
  # keep only the papers that considered thresholds
  #subset(clean_answer == "Yes") %>%
  unite(abbr, qnum, abbr, remove = F)
names(q22)[grep("clean_a", names(q22))] <- unique(q22$abbr)
names(q22)[grep("qa_", names(q22))] <- paste(unique(q22$qnum), "qa_note", sep = "_")

# distill ES study reports addressed
ES_table <- distinct(subset(qdat, abbr == "Driver" & Group == "Environmental", select = c(fullquestion, ES))) %>%
  # grab full ES description
  mutate(fullES = gsub(".*Driver - ", "", fullquestion))
services <- subset(qdat, version == "final" & abbr %in% c("Response","Driver") & !is.na(clean_answer), select = c(Init:Title, qnum, ES)) %>%
  distinct() %>%
  # join ES full descrip
  left_join(ES_table[c("ES", "fullES")]) %>%
  # sort alphabetically
  arrange(Title, ES) %>%
  # number ES's considered
  group_by(Title) %>%
  mutate(ESnum = 1:length(ES)) %>%
  ungroup() %>%
  #unite(ES, ESnum, ES,sep = ") ", remove = F) %>%
  #unite(ES, ES, fullES, sep = ": ") %>%
  group_by(Title) %>%
  mutate(ES = str_flatten(ES, collapse = "; "),
         EScount= max(ESnum)) %>%
  ungroup() %>%
  dplyr::select(-c(fullES,ESnum, qnum)) %>%
  distinct()
# there are three papers whose Q12 double-reviewer answers didn't combine correctly -- pull those services separately
badmerge_titles <- unique(qdat$Title[!qdat$Title %in% services$Title])
badmerge_services <- subset(qdat, version == "original" & Title %in% badmerge_titles & abbr %in% c("Response","Driver") & !is.na(clean_answer), select = c(Init:Title, qnum, ES)) %>%
  distinct() # these ES's don't all agree.. doesn't look like inconsistent answers got reviewed (looking at reviewer corrections files -- add to-do)
# for Travis' purposes, keep all
badmerge_services <- subset(badmerge_services, select = -c(Init:version)) %>%
  distinct() %>%
  arrange(Title, ES) %>%
  group_by(Title) %>%
  mutate(ESnum = 1:length(ES),
         ES = str_flatten(ES, collapse = "; "),
         EScount = max(ESnum)) %>%
  ungroup() %>%
  # add init, version as final and doublerev cols back
  left_join(distinct(subset(qdat, version == "final", select = c(Init, doublerev, version, qnum, Title))))
# add to services
services <- rbind(services, badmerge_services[names(services)]) %>%
  distinct()

# combine all to write out
interactions <- subset(q21, select = c(Title:Q15_Feedbacks, Q15_qa_note)) %>%
  full_join(subset(q22, select = c(Title:Q16_Thresholds, Q16_qa_note))) %>%
  subset(grepl("Ecosystem", Q15_Feedbacks) | grepl("Yes|Mentioned", Q16_Thresholds)) %>%
  # drop thresholds only mentioned and no feedbacks studied
  subset(!(grepl("Mentioned", Q16_Thresholds) & grepl("^No", Q15_Feedbacks))) %>%
  # combine qa_notes
  mutate(qa_note = ifelse(!is.na(Q15_qa_note) & Q15_qa_note == Q16_qa_note, Q15_qa_note, 
                          ifelse(is.na(Q15_qa_note), Q16_qa_note, 
                                 paste(Q15_qa_note, Q16_qa_note, sep = "; ")))) %>%
  #join ESs measured
  left_join(services) %>%
  # join notes
  left_join(notes[c("Title", "Q18_SurveyNotes")]) %>%
  # join refdat
  left_join(simpleref) %>%
  # clean up df
  dplyr::select(Title, FirstAuthor:PublicationYear, Init, ES, EScount, Q15_Feedbacks, Q16_Thresholds, Q18_SurveyNotes, Round1_Comments, qa_note) %>%
  rename(ES_assessed = ES) %>%
  arrange(FirstAuthor, PublicationYear)
  
# write out
write_csv(interactions, "round2_metareview/analyze_data/examples/ESfeedback_threshold_examples.csv", na = "")
with(interactions, data_frame(interactions[grepl("-", Q15_Feedbacks) & !is.na(Q18_SurveyNotes), c("FirstAuthor", "PublicationYear", "Title")]))



# 3. used multiple methods ----
# subset methods responses to multiple methods only
q6 <- subset(qdat, version== "final" & abbr == "Methods" & grepl(",[a-z]", clean_answer,ignore.case = T), select = keepcols) %>%
  # drop redundant Observation.. Observation
  subset(!grepl("Observation.*,Observation", clean_answer)) %>%
  # get rid of comma in ", if used directly" to count number methods
  mutate(clean_answer = gsub(", if used", " if used", clean_answer)) %>%
  # split methods to alphabetize if writer wants to review paper in each cat
  separate_rows(clean_answer, sep = ",") %>%
  arrange(Title, clean_answer) %>%
  # number and recollapse
  group_by(Title) %>%
  mutate(methodcount = length(clean_answer),
         clean_answer = str_flatten(clean_answer, collapse = "; ")) %>%
  ungroup() %>%
  distinct() %>%
  unite(abbr, qnum, abbr, remove = F)
names(q6)[grep("clean_a", names(q6))] <- unique(q6$abbr)
q6 <- q6 %>%
  # join notes
  left_join(notes[c("Title", "Q18_SurveyNotes")]) %>%
  # join refdat
  left_join(simpleref) %>%
  # clean up df
  dplyr::select(Title, FirstAuthor:PublicationYear, Init, Q6_Methods, methodcount, Q18_SurveyNotes, Round1_Comments, qa_note) %>%
  arrange(FirstAuthor, PublicationYear)

# write out
write_csv(q6, "round2_metareview/analyze_data/examples/ESmultimethods_examples.csv", na = "")


# 4. consistency in indicators by service type -----
# want list of how all the different ESs are measured (driver or response) "what ES was measured and how it was measured"
# also give how different ESPs are presented in studies
# also give confusing examples
q12dat_drivers <- subset(qdat, version == "final" & qnum == "Q12" & abbr %in% c("Driver", "OtherDriver") & !is.na(clean_answer)) %>%
  arrange(Title, ES, varnum) %>%
  # drop "Other" since captured in OtherDriver
  subset(!clean_answer_binned == "Other") %>%
  # clean up
  dplyr::select(Init:id, clean_answer:clean_answer_binned, abbr:ES, qa_note) # might give tehm this tidyy? most flexible to work w even tho a lot
# how many unique titles?
length(unique(q12dat_drivers$Title)) # 269
unique(qdat$Title[!qdat$Title %in% q12dat_drivers$Title]) # three missing are AK+GV bad merges + one of Sierra's papers where she notes they did not discuss drivers (NA okay)

q12dat_drivers_badmerge <- subset(qdat, Title %in% badmerge_titles & qnum == "Q12" & abbr %in% c("Driver", "OtherDriver") & !is.na(clean_answer)) %>%
  dplyr::select(Init:qa_note) %>%
  # if combine Init how many distinct rows are there?
  mutate(Init = "AK/GV", # in manual check, looks okay
         # make version final
         version = "final") %>% 
  distinct() %>%
  # drop "Other" since captured in OtherDriver
  subset(!clean_answer_binned == "Other") %>%
  # clean up
  dplyr::select(Init:id, clean_answer:clean_answer_binned, abbr:ES, qa_note)

# stack with others
q12dat_drivers_all <- rbind(q12dat_drivers, q12dat_drivers_badmerge) %>%
  # join general notes
  left_join(notes[c("Title", "Q18_SurveyNotes")]) %>%
  # join refdat
  left_join(simpleref) %>%
  # join full ES descript
  left_join(ES_table[c("ES", "fullES")]) %>%
  # specify qa_note pertains to drivers
  rename(Q12drivers_qa_note = qa_note) %>%
  # clean up df
  dplyr::select(Title, FirstAuthor:PublicationYear, Init:version, qnum, id, clean_answer:ES, fullES, Q18_SurveyNotes, Round1_Comments, Q12drivers_qa_note) %>%
  arrange(FirstAuthor, PublicationYear, Title, ES, varnum)
str(q12dat_drivers_all)  

q12dat_responses <- subset(qdat, version == "final" & qnum == "Q12" & abbr %in% c("Response", "Yclass") & !is.na(clean_answer)) %>%
  dplyr::select(Init:Title, clean_answer, varnum, abbr, qnum, ES, qa_note)
q12_yclass <- subset(q12dat_responses, abbr == "Yclass", select = -varnum) %>%
  unite(abbr, qnum, abbr)
names(q12_yclass)[names(q12_yclass) == "clean_answer"] <- unique(q12_yclass$abbr)
names(q12_yclass)[grepl("^qa", names(q12_yclass))] <- paste(unique(q12_yclass$abbr), names(q12_yclass)[grepl("^qa", names(q12_yclass))], sep = "_")
# join in 
q12dat_responses <- subset(q12dat_responses, abbr=="Response") %>%
  unite(abbr, qnum, abbr)
names(q12dat_responses)[names(q12dat_responses) == "clean_answer"] <- unique(q12dat_responses$abbr)
names(q12dat_responses)[grepl("^qa", names(q12dat_responses))] <- paste(unique(q12dat_responses$abbr), names(q12dat_responses)[grepl("^qa", names(q12dat_responses))], sep = "_")
# put all together
q12dat_responses2 <- subset(q12dat_responses, select = -abbr) %>%
  left_join(subset(q12_yclass,select = -abbr)) %>%
  # combine notes
  mutate(notecheck = Q12_Response_qa_note == Q12_Yclass_qa_note,
       qa_note = ifelse(is.na(notecheck), NA, ifelse(notecheck, Q12_Response_qa_note,
                                                     paste0("Response: ", Q12_Response_qa_note, "; Yclass: ", Q12_Yclass_qa_note)))) %>%
  dplyr::select(Init:varnum, Q12_Yclass, ES, qa_note)

# ak + gv 3 papers that got orphaned in final comp
q12dat_responses_badmerge <- subset(qdat, Title %in% badmerge_titles & qnum == "Q12" & abbr %in% c("Response", "Yclass") & !is.na(clean_answer)) %>%
# note: it won't work on this one to combine all -- answers are pretty much the same (manual review), go with Grant's responses
  subset(Init == "GV") %>%
  dplyr::select(Init:qa_note) %>%
  # if combine Init how many distinct rows are there?
  mutate(Init = "AK/GV", # in manual check, looks okay
         # make version final
         version = "final") %>% 
  dplyr::select(Init:Title, clean_answer, varnum, abbr, qnum, ES, qa_note)
# split YClass and its qa_notes to merge
q12_yclass_badmerge <- subset(q12dat_responses_badmerge, abbr == "Yclass", select = -varnum) %>%
  unite(abbr, qnum, abbr)
names(q12_yclass_badmerge)[names(q12_yclass_badmerge) == "clean_answer"] <- unique(q12_yclass_badmerge$abbr)
names(q12_yclass_badmerge)[grepl("^qa", names(q12_yclass_badmerge))] <- paste(unique(q12_yclass_badmerge$abbr), names(q12_yclass_badmerge)[grepl("^qa", names(q12_yclass_badmerge))], sep = "_")
# join in 
q12dat_responses_badmerge <- subset(q12dat_responses_badmerge, abbr=="Response") %>%
  unite(abbr, qnum, abbr)
names(q12dat_responses_badmerge)[names(q12dat_responses_badmerge) == "clean_answer"] <- unique(q12dat_responses_badmerge$abbr)
names(q12dat_responses_badmerge)[grepl("^qa", names(q12dat_responses_badmerge))] <- paste(unique(q12dat_responses_badmerge$abbr), names(q12dat_responses_badmerge)[grepl("^qa", names(q12dat_responses_badmerge))], sep = "_")
# put all together
q12dat_responses_badmerge2 <- subset(q12dat_responses_badmerge, select = -abbr) %>%
  left_join(subset(q12_yclass_badmerge,select = -abbr)) %>%
  # since these are not yet finalized, there won't be a qa_note
  mutate(qa_note = "ctw needs to finalize answer, placeholder")
#stack
q12dat_responses_all <- q12dat_responses2 %>%
  rbind(q12dat_responses_badmerge2[names(.)]) %>%
  # join general notes
  left_join(notes[c("Title", "Q18_SurveyNotes")]) %>%
  # join refdat
  left_join(simpleref) %>%
  # join full ES descript
  left_join(ES_table[c("ES", "fullES")]) %>%
  # clean up df
  dplyr::select(Title, FirstAuthor:PublicationYear, Init, doublerev, version, Q12_Response:ES, fullES, Q18_SurveyNotes, Round1_Comments, qa_note) %>%
  arrange(FirstAuthor, PublicationYear, Title, ES, varnum) %>%
 # clarify note is for responses as w drivers dataset
  rename(Q12responses_qa_note = qa_note)
str(q12dat_responses_all)
  

# pull esp dat
q14 <- subset(qdat, version == "final" & qnum == "Q14", select = keepcols[!grepl("fullq|survey_order", keepcols)]) %>%
  unite(abbr, qnum, abbr) %>%
  rename(Q14_qa_note = qa_note) %>%
  group_by(Title) %>%
  # collapse qa_note (if it's present should only be there for the answer anyway)
  mutate(Q14_qa_note = str_flatten(unique(Q14_qa_note[!is.na(Q14_qa_note)]), "; ")) %>%
  ungroup() %>%
  spread(abbr, clean_answer) %>%
  mutate(Q14_qa_note = ifelse(Q14_qa_note %in% c("", " "), NA, Q14_qa_note),
         #count ESP boxes checked
         ESPcount = ifelse(is.na(Q14_ESP_type), 0, str_count(Q14_ESP_type, ",(?=[A-Z])")+1))
# could add col to note if study indicated a biotic driver
bioticdrivers <- group_by(q12dat_drivers_all, Title)%>%
  mutate(biotic_driver_present = any(clean_group == "Biotic" & !is.na(clean_answer)),
         biotic_drivers = ifelse(biotic_driver_present, str_flatten(unique(clean_answer[!is.na(clean_answer) & clean_group == "Biotic"]), collapse = "; "), NA)) %>%
  ungroup() %>%
  dplyr::select(Title, Init, biotic_driver_present, biotic_drivers)
q14 <- left_join(q14, bioticdrivers) %>%
  # join general notes
  left_join(notes[c("Title", "Q18_SurveyNotes")]) %>%
  # join refdat
  left_join(simpleref) %>%
  # clean up df
  dplyr::select(Title, FirstAuthor:PublicationYear, Init, doublerev, version, Q14_ESP_type, ESPcount, Q14_KremenNotes, biotic_driver_present, biotic_drivers, Q18_SurveyNotes, Round1_Comments, Q14_qa_note) %>%
  arrange(FirstAuthor, PublicationYear) %>%
  distinct()
str(q14) # ok

# write out data for consistency reviewers/writers
write_csv(q12dat_drivers_all, "round2_metareview/analyze_data/examples/ES_drivers_consistency.csv", na = "")
write_csv(q12dat_responses_all, "round2_metareview/analyze_data/examples/ES_response_consistency.csv", na = "")
write_csv(q14, "round2_metareview/analyze_data/examples/ES_ESPconsistency.csv", na = "")


# 5. generally recommended papers for examples (good or bad)-----
# search notes fields for recommendations
goodex <- subset(qdat, version == "final" & qnum == "Q18", select = c(Title, Init, doublerev, version, clean_answer)) %>%
  subset(grepl("cool |good|example|interesting|flag ", clean_answer, ignore.case = T)) %>%
  data_frame() %>%
  left_join(simpleref) %>%
  arrange(Title, Init) %>%
  rename(Q18_SurveyNotes = clean_answer) %>%
  dplyr::select(Title, FirstAuthor:PublicationYear, Init:Round1_Comments)
goodex$Q18_SurveyNotes # drop fungal abundances paper
goodex <- subset(goodex, !grepl("fungal abundances but nowhere", Q18_SurveyNotes)) %>%
  arrange(FirstAuthor)


# 6. drivers, ESes, and title/citations -----
# write out for AIS and SDJ
# > want all drivers, ESes, Titles with lookup citation info (raw data)
driverdat <- subset(qdat, grepl("Driver", abbr) & !is.na(clean_answer) & !grepl("^Other$", clean_answer_binned) & version == "final",
                    select = c(Title, clean_answer:clean_group, ES)) %>%
  #join citation info
  left_join(r2assigned[c("Title", "FirstAuthor", "SourcePublication", "PublicationYear")]) 

# make ES descrip table
ESdescrip <- subset(driverdat, abbr == "Driver", select = c(fullquestion, ES)) %>%
  mutate(ESdescription = str_extract(fullquestion, "(?<=Driver - )[A-Z].*$")) %>%
  distinct(ES, ESdescription)

# join ES desrip to driver table
driverdat <- left_join(driverdat, ESdescrip, by = "ES") %>%
  data.frame() %>%
  dplyr::select(clean_answer:clean_answer_binned, abbr:ES, ESdescription, Title, FirstAuthor:PublicationYear) %>%
  # arrange by Title, ES, varnum
  arrange(FirstAuthor, Title, ES, varnum)

# write out 
write_csv(driverdat, "round2_metareview/analyze_data/examples/ES_alldrivers_withESes_citations.csv", na = "")

