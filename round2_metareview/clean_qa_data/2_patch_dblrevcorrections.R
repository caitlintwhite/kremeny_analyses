# script dependent on clean_round2.R
# run once get to section "APPLY CORRECTIONS TO DOUBLE REVIEW"

# dbl corrections for review sent out before CTW had corrected some additional mistakes/incorporated received corrections
# script purpose is to pair new inconsistent answers for review with old, figure out what still needs corrections so can read that into main clean_round2 script
# write out what still needs corrections (new stuff) so can read that into main clean_round2 script


# notes:
# > specifically, file dependent on double_inconsistent_all and headerLUT
# -- SETUP -----
library(readxl)
library(tidyverse)
options(stringsAsFactors = F)
na_vals <- c("NA", NA, "NaN", NaN, " ", "", ".")

# list intermediate dbl review corrections
interdat <- list.files("round2_metareview/data/reviewer_revisions/intermediate", full.names = T)
# > double rev corrections
dblcorAK <- read.csv(interdat[grep("inconsistent_AK", interdat)], na.strings = na_vals, strip.white = T)
dblcorGV <- read.csv(interdat[grep("inconsistent_GV", interdat)], na.strings = na_vals, strip.white = T) %>%
  # remove extra empty cols on left
  select(ResponseId:revorder) %>%
  # add keep and notes col
  mutate(keep = ifelse(!is.na(final_answer) | !is.na(review_notes), 1, NA), 
         ctw_notes = ifelse(keep == 1, "GV reviewed and corrected"))
dblcorJL <- read.csv(interdat[grep("inconsistent_JL", interdat)], na.strings = na_vals, strip.white = T)
dblcorKCG <- read.csv(interdat[grep("inconsistent_KCG", interdat)], na.strings = na_vals, strip.white = T)
dblcorLD <- read.csv(interdat[grep("inconsistent_LD", interdat)], na.strings = na_vals, strip.white = T)
dblcorSDJ <- read_excel(interdat[grep("inconsistent_SDJ", interdat)], na = na_vals, trim_ws = T) %>%
  # add keep and notes col
  mutate(keep = 1, ctw_notes = "SDJ reviewed and corrected")


# -- PREP FILES -----
# 1) julie + tim/claire
newjldbl <- subset(double_inconsistent_all, Title %in% Title[Init == "JL"]) %>%
  select(-c(final_answer, review_notes)) %>%
  left_join(distinct(headerLUT, ES, ESnum)) %>%
  left_join(select(dblcorJL, -c(survey_order))) %>%
  # reappend varnum
  left_join(select(prelimlong1f, ResponseId, Init, id, answer, clean_answer, varnum, clean_answer_finer, survey_order, Group)) %>%
  select(ResponseId:clean_answer, varnum, clean_answer_finer, final_answer, review_notes, fullquestion:clean_group, ESnum, qnum:ctw_notes)
  

# 2) ld + tim
newlddbl <- subset(double_inconsistent_all, Title %in% Title[Init == "LD"]) %>%
  select(-c(final_answer, review_notes)) %>%
  left_join(distinct(headerLUT, ES, ESnum)) %>%
  left_join(select(dblcorLD, -c(survey_order))) %>%
  # reappend varnum
  left_join(select(prelimlong1f, ResponseId, Init, id, answer, clean_answer, varnum, clean_answer_finer, survey_order, Group)) %>%
  select(ResponseId:clean_answer, varnum, clean_answer_finer, final_answer, review_notes, fullquestion:clean_group, ESnum, qnum:ctw_notes)


# 3) kcg + tim
newkcgdbl <- subset(double_inconsistent_all, Title %in% Title[Init == "KCG"]) %>%
  select(-c(final_answer, review_notes)) %>%
  left_join(distinct(headerLUT, ES, ESnum)) %>%
  left_join(select(dblcorKCG, -c(survey_order))) %>%
  # reappend varnum
  left_join(select(prelimlong1f, ResponseId, Init, id, answer, clean_answer, varnum, clean_answer_finer, survey_order, Group)) %>%
  select(ResponseId:clean_answer, varnum, clean_answer_finer, final_answer, review_notes, fullquestion:clean_group, ESnum, qnum:ctw_notes)


# 4) ak + gv
newakdbl <- subset(double_inconsistent_all, Title %in% unique(dblcorAK$Title)) %>%
  select(-c(final_answer, review_notes)) %>%
  left_join(distinct(headerLUT, ES, ESnum)) %>%
  left_join(select(dblcorAK, -c(survey_order))) %>%
  # reappend varnum
  left_join(select(prelimlong1f, ResponseId, Init, id, answer, clean_answer, varnum, clean_answer_finer, survey_order, Group)) %>%
  select(ResponseId:clean_answer, varnum, clean_answer_finer, final_answer, review_notes, fullquestion:clean_group, ESnum, qnum:ctw_notes)


# 5) gv
newgvdbl <- subset(double_inconsistent_all, Title %in% unique(dblcorGV$Title)) %>%
  select(-c(final_answer, review_notes)) %>%
  left_join(distinct(headerLUT, ES, ESnum)) %>%
  left_join(select(dblcorGV, -c(survey_order))) %>%
  # reappend varnum
  left_join(select(prelimlong1f, ResponseId, Init, id, answer, clean_answer, varnum, clean_answer_finer, survey_order, Group)) %>%
  select(ResponseId:clean_answer, varnum, clean_answer_finer, final_answer, review_notes, fullquestion:clean_group, ESnum, qnum:ctw_notes)
# 6) sdj + gv
newsdjdbl <- subset(double_inconsistent_all, Title %in% unique(dblcorSDJ$Title)) %>%
  select(-c(final_answer, review_notes)) %>%
  left_join(distinct(headerLUT, ES, ESnum)) %>%
  left_join(select(dblcorSDJ, -c(qa_note, survey_order))) %>%
  # reappend varnum
  left_join(select(prelimlong1f, ResponseId, Init, id, answer, clean_answer, varnum, clean_answer_finer, survey_order, Group)) %>%
  select(ResponseId:clean_answer, varnum, clean_answer_finer, final_answer, review_notes, fullquestion:clean_group, ESnum, qnum:ctw_notes)


# -- WRITE OUT ----
# stack all and write out for final doublereview corrections
dbl2correct <- rbind(newjldbl, newlddbl) %>%
  rbind(newkcgdbl) %>%
  rbind(newakdbl) %>%
  rbind(newgvdbl) %>%
  rbind(newsdjdbl)

write_csv(dbl2correct, "round2_metareview/clean_qa_data/needs_classreview/doublerev_inconsistent/augcheck/alldbl2review_aug2020.csv", na = "")

# which papers still need records reviewed?
subset(dbl2correct, is.na(keep) & !Init %in% c("CK", "TK", "AIS")) %>%
 distinct(Init, Title) %>%
  print(n= nrow(.))

# A tibble: 22 x 2
# Init  Title                                                                   
# <chr> <chr>                                                                   
# 1 JL    Application of a rangeland soil erosion model using National Resources …
# 2 JL    Assessing and mapping of multiple ecosystem services in Guizhou Provinc…
# 3 JL    Ecosystem Properties of Urban Land Covers at the Aboveground-Belowgroun…
# 4 JL    Effects of coastal development on nearshore estuarine nekton communities
# 5 JL    Exploring the complex relations between water resources and social indi…
# 6 JL    Woody shrubs increase soil microbial functions and multifunctionality i…
# 7 LD    Abundance of urban male mosquitoes by green infrastructure types: impli…
# 8 LD    Detecting pest control services across spatial and temporal scales      
# 9 LD    Global evidence of positive impacts of freshwater biodiversity on fishe…
# 10 LD    Using citizen scientists to measure an ecosystem service nationwide     
# 11 KCG   Climate and plant controls on soil organic matter in coastal wetlands   
# 12 KCG   Pollination benefits are maximized at intermediate nutrient levels      
# 13 AK    An invasive ant reduces diversity but does not disrupt a key ecosystem …
# 14 AK    Evaluating the effectiveness of urban trees to mitigate storm water run…
# 15 AK    Quantifying resilience of multiple ecosystem services and biodiversity …
# 16 AK    Seeing the light: urban stream restoration affects stream metabolism an…
# 17 AK    Simulation of ecosystem service responses to multiple disturbances from…
# 18 GV    Simulation of ecosystem service responses to multiple disturbances from…
# 19 AK    Spatial variability in ecosystem services: simple rules for predator-me…
# 20 GV    Spatial variability in ecosystem services: simple rules for predator-me…
# 21 SDJ   Managing ecosystems without prior knowledge: pathological outcomes of l…
# 22 GV    Managing ecosystems without prior knowledge: pathological outcomes of l…
