# short script to read in and lookup summary stats on abstracts/papers reviewed for ES
# > and other summary stats needed for methods section
# author(s): CTW

# notes:
# > feel free to add on as needed (e.g. more summary stats, figs, whatever needed)

# -- SETUP --
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
original <- read.csv("round1_exclusion/EcosystemServicesPapersNov2019.csv") %>%
  # drop title that's duplicated (only 1 record)
  subset(!duplicated(Title))
length(unique(original$SourcePublication)) #128 journals for round 1
length(unique(original$SourcePublication[original$Number %in% r1kept$Number])) #98 journals made it through
length(unique(r2assigned$SourcePublication)) # 80 journals selected for round 2 review
length(unique(original$SourcePublication[original$Title %in% unique(qdat$Title)]))

# group titles by journal
r1jsum <- group_by(original, SourcePublication) %>%
  summarise(papers = length(Title),
            pct = papers/nrow(original),
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
