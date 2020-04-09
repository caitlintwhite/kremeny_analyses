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
rawdat <- read.csv("round2_metareview/data/cleaned/prelim_singlereview.csv", na.strings = na_vals)
# check read in as expected
glimpse(rawdat)


# function for breaking out comma separated answers
splitcom <- function(df){
  df <- df %>% # break out answers by comma
    dplyr::select(Title, answer) %>%
    separate(answer, paste0("v",1:20), ",") %>% # <- this will throw warning, fine. pad with extra cols for up to 20 comma-separated answers just in case
    # remove cols that are all na
    dplyr::select(names(.)[sapply(., function(x) !(all(is.na(x))))]) %>%
    # gather to tidy
    gather(num, answer, v1:ncol(.)) %>%
    # make number of answers numeric
    mutate(num = parse_number(num)) %>%
    # remove any NAs in answer
    filter(!is.na(answer)) # dat still tidy at this point
  return(df)
}



# -- PREP DATA -----
# remove any titles that were excluded (yes in q3)
excludetitles <- with(rawdat, Title[(qnum == "Q3" & grepl("yes", answer, ignore.case = T))]) 
checkexcluded <- subset(rawdat, Title %in% excludetitles) # looks good
rm(checkexcluded)

# remove excluded papers
dat <- subset(rawdat, !Title %in% excludetitles)

# for data susbetting reference, here are the Q#s and their abbreviations:
distinct(dat[c("abbr", "qnum")])



# -- SUMMARIZE REVIEW DATA BY QUESTION -----
# 1) Plot Ecosystem Type -----
#table(krem$Q4)
ecotype <- subset(dat, qnum == "Q4")
View(ecotype)
unique(ecotype$answer[ecotype$abbr == "Ecosytem"]) #ctw will fix typo later
# for simplicity, keep only ecosytem checked (ignore notes for now)
ecotype <- subset(ecotype, abbr == "Ecosytem") %>%
  # apply splitcom
  splitcom() # end result is still tidy data

# simple bar plot
ggplot(ecotype, aes(answer)) +
  geom_bar()
table(ecotype$answer)
table(ecotype$num) # most papers study only one type of system, but multiple systems entered for +100 papers (about 30% of papers reviewed)



# 2) Plot Location Type -----
loc <- subset(dat, qnum == "Q5")
unique(loc$answer) # deal with commas
loc <- splitcom(loc)  

# simple bar plot
ggplot(loc, aes(answer)) +
  geom_bar()
table(loc$answer)
table(loc$num)



# 2) Plot Study Type -----
studytype <- subset(dat, qnum == "Q6")
head(studytype) # for simplicity, keep only Methods for now (deal with other and notes later)
unique(studytype$answer[studytype$abbr == "Methods"]) #remove "(Includes...)", then un-comma
studytype <- subset(studytype, abbr == "Methods") %>%
  # remove "(Includes...)" text
  mutate(answer = gsub(" [(]Includes.*[a-z][)]", "", answer)) %>%
  # split com-sep answers
  splitcom()

# simple bar plot
ggplot(studytype, aes(answer)) +
  geom_bar()
table(studytype$answer)
table(studytype$num)


# 3) Consideration of multiple years & # of years -----
temptype <- subset(dat, qnum == "Q7")
unique(temptype$answer) # no comma splitting needed, but there are two questions to Q7 (time? y/n, and if yes, what?)
# can separate by abbr if wish
unique(temptype$abbr)



# 4) Plot Feedbacks counts -----
#table(krem$Q21)
feedbacks <- subset(dat, abbr == "Feedbacks")
unique(feedbacks2$answer) # needs comma splitting
feedbacks <- splitcom(feedbacks)

# simple bar plot
ggplot(feedbacks, aes(answer)) +
  geom_bar() +
  # hard to see when on answers on x
  coord_flip()
table(feedbacks$answer)
table(feedbacks$num)



# 5) Plot Threshold/Non-linearity counts -----
#table(krem$Q22)
tholds <- subset(dat, abbr == "Thresholds")
unique(tholds$answer) # no un-comma needed
unique(tholds$abbr) # no subquestions or optional text comments to this question

# simple bar plot
ggplot(tholds, aes(answer)) +
  geom_bar()
table(tholds$answer)
  


