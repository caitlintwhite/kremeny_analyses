# compile all excluded papers from round 1 and round 2
# author(s): CTW
# questions?: caitlin.t.white@colorado.edu

# script purpose:
# 1) read in excluded papers from round 1 and excluded papers from round 2
# 2) append google form questions ("full question") to round 1 data, tidy dataset
# 3) stack round 1 excluded and final version of round 2 excluded papers, with reason for exclusion, along with journal citation info


# -- SETUP -----
rm(list = ls()) # clean enviro
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
na_vals <- c("NA", NA, "NaN", NaN, " ", "", ".")
theme_set(theme_bw())

# read in dats
r1exclude <- read.csv("round1_exclusion/output/exclude_round1.csv", na.strings = na_vals)
r2exclude <- read.csv("round2_metareview/data/cleaned/ESqualtrics_r2exclude_cleaned.csv", na.strings = na_vals)
ESpapers <- read.csv("round1_exclusion/EcosystemServicesPapersNov2019.csv", na.strings = na_vals)
r2papers <- read.csv("round1_exclusion/output/keep_round1.csv", na.strings = na_vals)

# review data structure
glimpse(r1exclude)
glimpse(r2exclude)
glimpse(ESpapers)


# -- PREP LOOKUP TABLES ----
# need to make lookup table for round 1 exclusion questions
exclusionLUT <- data.frame(round = 1, abbr = names(r1exclude)[(grep("Title", names(r1exclude))+1):grep("biodiv", names(r1exclude))],
                           fullquestion = NA,
                           survey_order = NA)
# copy/paste questions from google survey
exclusionLUT$fullquestion[grepl("meta", exclusionLUT$abbr)] <- "Is this a meta-analysis?"
exclusionLUT$fullquestion[grepl("^rev", exclusionLUT$abbr)] <- "Is this a review?"
exclusionLUT$fullquestion[grepl("efes", exclusionLUT$abbr)] <- "This paper does NOT directly measure/model an EF and/or ES"
exclusionLUT$fullquestion[grepl("^val", exclusionLUT$abbr)] <- "This paper focuses ONLY on valuation or risk assessment"
exclusionLUT$fullquestion[grepl("tool", exclusionLUT$abbr)] <- "This paper describes ONLY a tool, but not does report implications for EF/ES on said tool"
exclusionLUT$fullquestion[grepl("^bio", exclusionLUT$abbr)] <- "This paper only measures biodiversity/abundance but NOT as an explicit proxy for ES/EF"
exclusionLUT$survey_order <- 1:nrow(exclusionLUT)
# append round 2 exclusion reasons (3)
exclusionLUT <- rbind(exclusionLUT, data.frame(round = 2, distinct(r2exclude, abbr, fullquestion, survey_order))) %>%
  # remove Q# from round 2
  mutate(fullquestion = gsub("^Q.*: ", "", fullquestion),
         fullquestion = trimws(fullquestion))

# also LUT for reviewer inits (full name in round 1, inits in round 2)
reviewerLUT <- data.frame(Init = sort(unique(r2exclude$Init[!r2exclude$doublerev])),
                          Name = sort(unique(r1exclude$EBIOReviewer[!grepl("/", r1exclude$EBIOReviewer)]))) %>%
  # almost worked -- clean up names
  mutate(Name = ifelse(Init == "AIS", "Anna", ifelse(Init == "AK", "Aislyn", ifelse(Init == "CK", "Claire", ifelse(Init == "CW", "Caitlin", ifelse(Init == "LD", "Laura", ifelse(Init == "LB", "Laurel", Name))))))) %>%
  # rbind ref for LD/CW
  rbind(data.frame(Init = "LD/CW", Name = "Laura/Caitlin"))

# add first author to citation info
ESpapers <- ESpapers %>%
  #add first author
  mutate(FirstAuthor = str_extract(AuthorsFull, "^[:alpha:]+( |-|')*[:alpha:]+(?=,)"),
         # three word names, or names that are followed by semicolon
         FirstAuthor = ifelse(is.na(FirstAuthor),str_extract(AuthorsFull,"^[:alpha:]+( |-|')*[:alpha:]+( |-|')*[:alpha:]+(?=,)"),FirstAuthor),
         FirstAuthor = ifelse(is.na(FirstAuthor),str_extract(AuthorsFull,"^[:alpha:]+( |[:alpha:])*(?=;)"),FirstAuthor))



# -- TIDY R1 EXCLUDED ----
# some people stopped answering questions after marked 1 yes,
# other people answered all questions (biodiv was created later, so sometimes that is NA)
# follow same methods as in R2, where as soon as mark "Yes", questions stop, so go in order of how questions asked


# need to think about order submitted (i think?) for round1, since some people were submitting multiple surveys for one paper
# how many entries per paper?
nobscheck <- group_by(r1exclude, final_name) %>%
  summarise(nobs = length(EBIOReviewer),
            # how many unique reviewers?
            revs = length(unique(EBIOReviewer))) %>% 
  ungroup()
summary(nobscheck) # great, no dups
rm(nobscheck)

tidyr1 <- select(r1exclude,EBIOReviewer, final_name, meta:comments) %>%
  gather(abbr, answer, meta:biodiv) %>%
  # join LUT to select reason excluded (whichever question answered Yes first)
  left_join(exclusionLUT) %>%
  left_join(reviewerLUT, by = c("EBIOReviewer" = "Name")) %>%
  # select questions on which paper excluded
  subset(answer == "Yes") %>%
  group_by(final_name) %>%
  mutate(firstexclude = min(survey_order),
         # append inits to comment so will be similar to R2
         comments = ifelse(!is.na(comments), paste(Init, comments, sep = ": "), comments)) %>%
  subset(survey_order == firstexclude) %>%
  ungroup() %>%
  # add doublerev col
  # > if LD/CW yes, otherwise no
  mutate(doublerev = ifelse(Init == "LD/CW", TRUE, FALSE))

# what the breakdown by paper?
sort(summary(as.factor(tidyr1$abbr))) # most ruled out for not directly measuring EF or ES


# -- TIDY ROUND 2 ----
# every paper already only has 1 reason for exclusion, so just need to winnow dows cols and subset to final
tidyr2 <- subset(r2exclude, version == "final") %>%
  distinct(Init, doublerev, Title, exclusion_reason, reviewer_surveynotes, outsidereviewer, outsidereviewer_notes) %>%
  # add full question back in 
  left_join(exclusionLUT, by = c("exclusion_reason" = "abbr")) %>%
  # make round all 2 (no number for papers that should have been excluded in round 1 but made it to 2)
  # > but first infill fullquestion and survey order with that from round 1 for no efes
  mutate(survey_order = ifelse(is.na(round), with(exclusionLUT, survey_order[abbr == "no_efes"]), survey_order),
         fullquestion = ifelse(is.na(round), with(exclusionLUT, fullquestion[abbr == "no_efes"]), fullquestion),
         round = 2)


# -- COMBINE ROUNDS, ADD JOURNAL INFO ----
allexcluded <- select(tidyr1, -c(EBIOReviewer, firstexclude, answer)) %>%
  rename(Title = final_name,
         reviewer_surveynotes = comments,
         exclusion_reason = abbr) %>%
  cbind(as.data.frame(matrix(nrow = nrow(.), ncol = length(names(tidyr2)[!names(tidyr2) %in% names(.)]),
                             dimnames = list(NULL, names(tidyr2)[!names(tidyr2) %in% names(.)])))) %>%
  rbind(tidyr2[names(.)]) %>%
  # reorder cols, then append citation info
  select(round, Init, doublerev, Title, reviewer_surveynotes, outsidereviewer, outsidereviewer_notes, exclusion_reason, fullquestion, survey_order) %>%
  # make all colnames lowcase
  rename_all(casefold) %>%
  left_join(distinct(ESpapers,Title, SourcePublication, PublicationYear, FirstAuthor), by = c("title" = "Title")) %>%
  #replace_na(list(doublerev = FALSE)) %>%
  rename(review_round = round, reviewer_init = init, reviewer_comments = reviewer_surveynotes, exclusion_id = survey_order)

# need to infill missing citation info for Claire's round 1 paper (had to lookup in kept papers for r2)
allexcluded[grepl("^Effects of dams on", allexcluded$title),c("SourcePublication", "PublicationYear", "FirstAuthor")] <- ESpapers[ESpapers$Number == r2papers$Number[grepl("^Effects of dams on", r2papers$Title)], c("SourcePublication", "PublicationYear", "FirstAuthor")]

# check out to see if all looks good for writing out
str(allexcluded)
summary(is.na(allexcluded)) # looks okay! GV can deal with how he wants to combine reasons for exclusion

write_csv(allexcluded, "round2_metareview/data/cleaned/ESreview_allexcluded_allrounds.csv")


