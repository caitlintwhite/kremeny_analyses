# clean round 2 results


# -- SETUP -----
library(tidyverse)
library(lubridate)
options(stringsAsFactors = F)
na_vals <- c("NA", NA, "NaN", NaN, " ", "", ".")

# find where the prelim results live
qualtrix <- read.csv(file.choose(), na.strings = na_vals, skip  = 2) 
headers <- read.csv(file.choose(), na.strings = na_vals)


# -- IDENTIFY DOUBLE-REVIEWED PAPERS -----
# remove top two header rows
names(qualtrix) <- names(headers)

# remove anything not finished
prelim <- subset(qualtrix, Finished == "True" & Status != "Survey Preview" & !is.na(Q1) & Q1 != "dasdas") %>%
  # make true date time column
  mutate_at(.vars = c("StartDate", "EndDate", "RecordedDate"), function(x) ymd_hms(x)) %>%
  #sort by most recent date first
  arrange(desc(RecordedDate)) %>%
  group_by(Q27, Q1) %>%
  mutate(doubleentry = duplicated(Q1)) %>%
  ungroup() %>%
  #remove any doubleentries
  filter(!doubleentry) %>%
  # remove doubleentryrow
  select(-doubleentry)
# tally papers that have been reviewed more than once (i.e. 2 people reviewed it)
records <- dplyr::select(prelim, Q27, Q1) %>%
  group_by(Q1) %>%
  mutate(nobs = length(Q1)) %>%
  ungroup()
# write out double-reviewed for Aislyn to compare results 2020-03-04
forAK <- subset(prelim, Q1 %in% unique(records$Q1[records$nobs == 2])) %>%
  arrange(Q1, StartDate) %>%
  mutate_all(function(x) ifelse(is.na(x), "", x))
#write.csv(forAK, "round2_doublereviewed.csv", row.names = F)

# write out headers to manually create abbreviations for headers
headerdf <- data.frame(headers[1,]) %>%
  gather(id, fullquestion, StartDate:ncol(.))


# -- DIVIDE RESULTS BY QUESTION ---
names(prelim)
# important ID-key fields to keep across question datasets are:
## ResponseID
## Q27 (Reviewer ID)
## Q1 (Title)

keydf <- distinct(dplyr::select(prelim, RecordedDate, ResponseId, Q27, Q1))
# who has reviewed?
sort(unique(keydf$Q27))
# how many papers per reviewer?
sort(sapply(split(keydf$Q1, keydf$Q27), length))

# filtering (exclusion) questions are Q2, Q29, (and Q30? not sure what question that is)
# pull unique question names
questions <- names(prelim)[grep("^Q", names(prelim))]
question_numbers <- unique(str_extract(questions, "^Q[:number:]+"))

# question 9 (scale)
q9df <- dplyr::select(prelim, c(names(keydf), names(prelim)[grep("Q25", names(prelim))]))
# question 25 (response variables)


