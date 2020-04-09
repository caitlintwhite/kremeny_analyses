# make study summary figs

# script purpose:
# read in current prepped dataset [will change as incorp more answers and clean]
# subset by question and widen for LD
# make following study summary figs:
# 1) Ecosystem type studied (q4)
# 2) Study location summary
# 3) Study type
# 4) Temporal component
# 5) Feedbacks studied? counts (q21)
# 6) Threshold/non-linearity discussed? summary (q22)
# write out to round2_metareview/figs
## > use consistent, intuitive file nomenclature

# notes:




# -- SETUP -----
library(tidyverse)
options(stringsAsFactors = F)
na_vals <- c("NA", "NaN", "", " ", ".", NA, NaN)

# read in latest metareview data
dat <- read.csv("round2_metareview/data/cleaned/prelim_singlereview.csv", na.strings = na_vals)
# check read in as expected
glimpse(dat)

# for data susbetting reference, here are the Q#s and their abbreviations:
distinct(dat[c("abbr", "qnum")])



# -- SUMMARIZE REVIEW DATA BY QUESTION -----
# 1) Plot Ecosystem Type -----
#table(krem$Q4)
ecotype <- subset(dat, qnum == "Q4")


# 2) Plot Location Type -----
loc <- subset(dat, qnum == "Q5")


# 2) Plot Study Type -----
studytype <- subset(dat, qnum == "Q6")



# 3) Consideration of multiple years & # of years -----
temptype <- subset(dat, qnum == "Q7")



# 4) Plot Feedbacks counts -----
#table(krem$Q21)
feedbacks <- subset(dat, abbr == "Feedbacks")



# 5) Plot Threshold/Non-linearity counts -----
#table(krem$Q22)
tholds <- subset(dat, abbr == "Thresholds")
