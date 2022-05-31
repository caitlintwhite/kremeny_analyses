# internal use R script to prep data and EML metadata for publication
# author(s): CTW
# questions?: caitlin.t.white@colorado.edu
# date init: 2022-05-15

# script purpose:
# we should publish 2 datasets:
# 1. all abstracts and full text excluded with reasons and citation info
# 2. full text studies retained, with qualtrics data used for analyses, and citation info
# > #2 is our "map database" (anyone interested in particular aspects our our survey could filter studies based on coding responses)


# once datasets prepped with citation info and unecessary columns removed (e.g., anything internal use not critical to publish), write metadata with EML package
# write out all to main level of "publish" subfolder



#### SETUP -----
rm(list = ls()) # start w clean environment (ctw does not care this is not best practice code)
# load needed libraries
library(tidyverse)
library(EML)
options(stringsAsFactors = F)
na_vals = c(NA, "NA", " ", "", ".")

# read in relevant datasets
datpath <- "round2_metareview/data/cleaned/"
r2keep_lulc <- read.csv(paste0(datpath, "ESqualtrics_r2keep_cleaned_lulc.csv"), na.strings = na_vals)
excludedpapers <- read.csv(paste0(datpath, "ESreview_allexcluded_allrounds.csv"), na.strings = na_vals)
citations <- read.csv("round1_exclusion/EcosystemServicesPapersNov2019.csv", na.strings = na_vals)

# review how all read in
str(r2keep_lulc)
str(excludedpapers)
str(citations)

# make lut to anonymize reviewers
reviewers <- unique(c(r2keep_lulc$Init, excludedpapers$reviewer_init))
# remove double review inits
single_reviewers <- data.frame(Init = sort(trimws(reviewers[!grepl("[/]", reviewers)])))
single_reviewers$ID <- paste0("R", 1:nrow(single_reviewers))
double_reviewers <- data.frame(Init = sort(reviewers[grepl("[/]", reviewers)])) %>%
  separate(col = Init, into = c("Init1", "Init2"), "[/]", remove = F) %>%
  mutate_at(.vars = c("Init1", "Init2"), .funs =  function(x) trimws(x))
for(i in 1:nrow(double_reviewers)){
  double_reviewers$alpha_Init[i] <- str_flatten(sort(c(double_reviewers$Init1[i], double_reviewers$Init2[i])), collapse = "/")
}
double_reviewers <- dplyr::select(double_reviewers, -c(Init1, Init2)) %>%
  separate(col = alpha_Init, into = c("Init1", "Init2"), "[/]", remove = F) %>%
  mutate_at(.vars = c("Init1", "Init2"), .funs =  function(x) trimws(x)) %>%
  left_join(single_reviewers, by = c("Init1" = "Init")) %>%
  rename(ID1 = ID) %>%
  left_join(single_reviewers, by = c("Init2" = "Init")) %>%
  rename(ID2 = ID) %>%
  mutate(ID = paste(ID1, ID2, sep = "/"))

reviewers_lut <- rbind(single_reviewers, double_reviewers[c("Init", "ID")])
# add ND row
reviewers_lut <- rbind(reviewers_lut, data.frame(Init = "ND", ID = reviewers_lut$ID[reviewers_lut$Init == "NBD"]))



#### MAKE FULL TEXT DATA OUT ----
# anonymize dataset:
# > Init column, answers for double reviews, any inits in QA note

r2keep_out <- dplyr::select(r2keep_lulc, ResponseId:only_lulc) %>% # don't need start, end, recorded date
  # join lut col to anonymize reviewers
  left_join(reviewers_lut) %>%
  # replace reviewer
  mutate(Init = ID) %>%
  # drop ID col
  dplyr::select(-ID) %>%
  # prefix "raw" to answer and Group to distinguish more intuitively from clean_group
  rename(raw_answer = answer, raw_group = Group, qid = id) %>%
  # lowcase colnames
  rename_all(casefold)

# anonymize reviewer answers
for(i in 1:nrow(reviewers_lut)){
  r2keep_out$raw_answer <- gsub(paste0(reviewers_lut$Init[i],":"), paste0(reviewers_lut$ID[i],":"), r2keep_out$raw_answer)
  r2keep_out$clean_answer <- gsub(paste0(reviewers_lut$Init[i],":"), paste0(reviewers_lut$ID[i],":"), r2keep_out$clean_answer)
}

# anonymize qa_notes
for(i in 1:nrow(reviewers_lut)){
  r2keep_out$qa_note <- gsub(reviewers_lut$Init[i], reviewers_lut$ID[i], r2keep_out$qa_note)
}

# spot review anonymous subbing
View(subset(r2keep_out, grepl("R[0-9]{1,2}", raw_answer)))
sort(unique(r2keep_out$qa_note[grepl("R[0-9]{1,2}", r2keep_out$qa_note)]))
sort(unique(r2keep_out$raw_answer[grepl("R[0-9]{1,2}", r2keep_out$raw_answer)])) # seems okay

# rename init col to reviewer
names(r2keep_out)[names(r2keep_out)== "init"] <- "reviewer"

# add citation info
summary(is.na(citations))
View(subset(citations, is.na(PublicationYear))) # early access date 'ea' has at least the year when pubyear not available
simple_citations <- mutate(citations, pubyear = ifelse(is.na(PublicationYear), 
                                                               as.numeric(paste0(20, str_extract(EA,"[0-9]{2}$"))),
                                                               PublicationYear)) %>% #NA warnings thrown but it looks fine on manual review
  subset(select = names(.)[grepl("Tit|Auth|Sour|pub", names(.))]) %>%
  rename(sourcepub = SourcePublication, title = Title, authors = AuthorsFull) %>%
  # rearrange columns
  subset(select = c(title, authors, pubyear, sourcepub))

r2keep_out <- left_join(r2keep_out, simple_citations, by = "title")
# rearrange cols for writing out
r2keep_out <- dplyr::select(r2keep_out, title, authors:sourcepub, responseid:only_lulc)


# write out final dataset
write.csv(r2keep_out, "round2_metareview/publish/ecolofES_extracted_data.csv", row.names = F)


#### MAKE EXCLUDED PAPERS DATA OUT----

#### MAKE EML METADATA -----