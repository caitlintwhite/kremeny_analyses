# clean round 2 results


# -- SETUP -----
library(tidyverse)
library(lubridate)
options(stringsAsFactors = F)
na_vals <- c("NA", NA, "NaN", NaN, " ", "", ".")

# find where the prelim results live
qualtrix <- read.csv(file.choose(), na.strings = na_vals, skip  = 2) 
headers <- read.csv(file.choose(), na.strings = na_vals)


# -- DATA PREP -----
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

records <- dplyr::select(prelim, Q27, Q1) %>%
  group_by(Q1) %>%
  mutate(nobs = length(Q1)) %>%
  ungroup()

forAK <- subset(prelim, Q1 %in% unique(records$Q1[records$nobs == 2])) %>%
  arrange(Q1, StartDate) %>%
  mutate_all(function(x) ifelse(is.na(x), "", x))
write.csv(forAK, "round2_doublereviewed.csv", row.names = F)
