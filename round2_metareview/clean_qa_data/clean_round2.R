# clean round 2 results
# author(s): ctw
# initiated: mar 2020


# script purpose:
# 1) read in raw qualtrics data (lives in data/raw/) [.. and eventually reviewer corrections]
## > must download manually bc CU qualtrics settings doesn't allow user to authenticate with 'qualtRics' R package 
# 3) tidy
# 4) apply basic cleaning and logic check flags
# 5) separate double reviews from single reviews
# 6) triage double reviews
## > write out conflicting answers for reviewer QA
## > incorporate reviewer QA responses
# 7) write out modular by-question cleaned datasets to data/cleaned
# > file nomenclature consistent to include qnum
# > make wide-form for LD and non-tidy coders


# notes:
## cached code for qualtRics## -----
# how to read in qualtrix dynamically
# https://www.rdocumentation.org/packages/qualtRics/versions/3.0
## > unfortunately, this package will not work with CU qualtrics as set up. users cannot authenticate, will throw error.
## code for had it worked:
#library(qualtRics)
# cache qualtrics credentials (one time)
# qualtRics::qualtrics_api_credentials(api_key = "YHLHyYFX7PPSN1XCRnzL7BoBpc7V6600TSmTnenb", 
#                           base_url = "cuboulder.ca1.qualtrics.com",
#                           install = TRUE)
# Your Qualtrics key and base URL have been stored in your .Renviron.  
# To use now, restart R or run `readRenviron("~/.Renviron")`

# find where the prelim results live
# read in survey dynamically
#surveys <- all_surveys()
#qualtrix <- fetch_survey(surveyID = "SV_8A1uHzmFP7yQTWt")





# -- SETUP -----
rm(list = ls()) # clean enviro
library(tidyverse)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
na_vals <- c("NA", NA, "NaN", NaN, " ", "", ".")
theme_set(theme_bw())

# read in raw data
## skip first two lines to read data col types correctly
qualtrix <- read.csv(list.files("round2_metareview/data/raw", full.names = T), na.strings = na_vals, skip  = 2) 
## re-read, no skipping, for colnames
headers <- read.csv(list.files("round2_metareview/data/raw", full.names = T), na.strings = na_vals)
## read in header lookup table (exported headers and typed in abbreviations and group assignments bc easier doing in Excel than R)
headerLUT <- read.csv("round2_metareview/data/headersLUT.csv", na.strings = na_vals)

# original round 2 assignment
original <- read.csv("round1_exclusion/output/review_assignments_round2_grpdsubset.csv")

# function for breaking out comma separated answers
splitcom <- function(df, keepcols = c("Title", "answer"), splitcol = "answer"){
  df <- df[keepcols] %>%
    # rename column to split as answer
    rename_at(splitcol, function(x) x <- "answer") %>%
    #dplyr::select_vars(keepcols) %>%
    distinct() %>%
    # break out answers by comma
    separate(answer, paste0("v",1:20), ",") %>% # <- this will throw warning, fine. pad with extra cols for up to 20 comma-separated answers just in case
    # remove cols that are all na
    dplyr::select(names(.)[sapply(., function(x) !(all(is.na(x))))]) %>%
    # gather to tidy
    gather(num, answer, names(.)[grep("^v[0-9]$", names(.))]) %>% #v1:ncol(.)
    mutate(answer = trimws(answer)) %>%
    # make number of answers numeric
    mutate(num = parse_number(num)) %>%
    # remove any NAs in answer
    filter(!(is.na(answer)|answer %in% c("", " "))) # dat still tidy at this point
  return(df)
}


# read in reviewer revisions/comments/corrections
corrections <- list.files("round2_metareview/data/reviewer_revisions/", full.names = T)
IS <- read.csv(corrections[grep("IS", corrections)], na.strings = na_vals)



# -- RM JUNK ROWS + ID DOUBLE-REVIEWED PAPERS -----
# assign colnames
names(qualtrix) <- names(headers)
names(headers)
# assess values for survey parameters to look for screening values
sapply(qualtrix[c("Status", "Progress", "Finished", "DistributionChannel", "Q27", "Q1")], unique)
qualtrix[grep("[a-z]", qualtrix$Q27), c("Q1", "Status", "DistributionChannel")] # remove these

# remove anything not finished or was survey preview
prelim <- subset(qualtrix, Finished == "True" & Status != "Survey Preview" & !is.na(Q1) & !grepl("[a-z]|TEST_REMOVE", Q27))
# check pulled correct records
unwanted <- anti_join(qualtrix, prelim) # looks good. these records are unfinished or test runs
# continue with prelim column class clean up, title clean up, and ordering

# check for titles that don't match
needs_match <- unique(prelim$Q1)[!unique(prelim$Q1) %in% original$Title]
needs_match
# https:// value = "Assessment of the relationship between ecosystem services and human wellbeing in the social-ecological landscapes of Lefke Region in North Cyprus"
https <- "Assessment of the relationship between ecosystem services and human wellbeing in the social-ecological landscapes of Lefke Region in North Cyprus"
# see if we can match on all lowcase, no punctuation, and subset of starting words. https is gonna have to be manual matched
needs_match <- data.frame(Q1 = needs_match, nmlow = str_remove_all(casefold(needs_match),"[:punct:]")) %>%
  # manual fix https
  mutate(nmlow = ifelse(grepl("https://link.springer.com/content/pdf/10.1007/s1", Q1), str_remove_all(casefold(https),"[:punct:]"), nmlow),
         nm_words = trimws(word(nmlow, 1, 5)),
         nm_words = ifelse(is.na(nm_words), nmlow, nm_words))
# repeat for titles
title_df <- dplyr::select(original, Title) %>%
  mutate(title_low = str_remove_all(casefold(Title), "[:punct:]"),
         title_words = trimws(word(title_low, 1, 5))) #[:blank:]|
# partial match
needs_match <- mutate(needs_match, match_num = pmatch(nm_words, title_df$title_words)) %>%
  # for anything that didn't partial match, match on first 25 characters 
  group_by(Q1) %>%
  mutate(match_num = ifelse(is.na(match_num), grep(paste0("^", substr(nm_words, 1, 25)), title_df$title_words), match_num)) %>%
  ungroup() %>%
  mutate(clean_title = title_df$Title[match_num])
# does everything have a match?
needs_match$Q1[is.na(needs_match$clean_title)] # this is a comment to paper, can pull title from original
# Service measured: bird predation on insect pest in agroforestry system
needs_match$clean_title[grep("Service measured: ", needs_match$Q1)] <- original$Title[grep("^Service measured: bird", original$Comments)]


prelim <- prelim %>%
  # make true date time column
  mutate_at(.vars = c("StartDate", "EndDate", "RecordedDate"), function(x) ymd_hms(x)) %>%
  #sort by most recent date first
  arrange(desc(RecordedDate)) %>%
  # add clean title
  left_join(needs_match[c("Q1", "clean_title")]) %>%
  mutate(clean_title = ifelse(is.na(clean_title), Q1, clean_title)) %>%
  # identify duplicate records
  group_by(Q27, clean_title) %>%
  mutate(doubleentry = duplicated(Q1)) %>%
  ungroup() %>%
  # move clean title to behind Q1
  dplyr::select(names(.)[1]:Q1, clean_title, Q2:ncol(.))
# screen double entries
doubles <- subset(prelim, clean_title %in% clean_title[doubleentry]) %>%
  arrange(clean_title, Q27, desc(RecordedDate))
# 3 of 4 double entries have more of the Q3 exclusion questions answered in the recent, paper #4 identitical (excluded on first exclusion question)
# verdict: keep most recent entry for same reviewer-double reviews
prelim <- prelim %>%
  #remove any doubleentries
  filter(!doubleentry) %>%
  # remove doubleentryrow
  select(-doubleentry)
# tally papers that have been reviewed more than once (i.e. 2 people reviewed it)
records <- dplyr::select(prelim, Q27, clean_title) %>%
  group_by(clean_title) %>%
  mutate(nobs = length(clean_title)) %>%
  ungroup() %>%
  arrange(desc(nobs), clean_title, Q27)
# write out double-reviewed for Aislyn to compare results 2020-03-04
forAK <- subset(prelim, clean_title %in% unique(records$clean_title[records$nobs > 1])) %>%
  arrange(clean_title, StartDate) %>%
  mutate_all(function(x) ifelse(is.na(x), "", x)) %>%
  distinct()
write.csv(forAK, "round2_metareview/data/intermediate/round2_doublereviewed.csv", row.names = F)

# pull titles for screening further down
doubletitles <- unique(forAK$clean_title)
# how many papers double-reviewed?
length(doubletitles)

# clean up environment
rm(doubles, qualtrix, unwanted, title_df, needs_match, https)



# -- PREP HEADER LOOKUP TABLE -----
# write out headers to manually create abbreviations for headers
# headerdf <- data.frame(headers[1,]) %>%
#   gather(id, fullquestion, StartDate:ncol(.))
# write_csv(headerdf, "headers.csv")
## made header LUT from this basic data table, so only needed to run code once

str(headerLUT)
# append out question number to frame
headerLUT$qnum <- str_extract(headerLUT$fullquestion, "Q[:number:]+")
# what needs aq#?
headerLUT$abbr[is.na(headerLUT$qnum)]
# fill in/clean up any question that didn't have a Q#
headerLUT$qnum[headerLUT$abbr %in% c("ReviewOnly", "GenInfo", "Uncertainty", "SurveyNotes")]  <- c("Q3", "Q6", "Q17", "Q18")
# extract ES number for Q25 (for eventually joining ES abbreviation)
## will be at position .x_x
headerLUT$ESnum <- str_extract(headerLUT$id, "(?<=Q25.[:number:]_)[:number:]+(_)?") %>% parse_number()
# extract ES number + abbreviation
ESshort <- distinct(dplyr::select(headerLUT, ESnum, ES)) %>% na.omit()
# redo group
headerLUT <- headerLUT %>%
  mutate(Group = ifelse(grepl("plot|site", abbr, ignore.case = T), gsub("[P|Sa-z]+_", "", headerLUT$abbr), 
                        ifelse(grepl("Human", fullquestion) & qnum == "Q12", "Anthro",
                               ifelse(grepl("Biotic", fullquestion) & qnum == "Q12", "Bio",
                                      ifelse(grepl("Envir", fullquestion) & qnum == "Q12", "Env", NA))))) %>%
  #drop ES column and join ESshort
  dplyr::select(-ES) %>%
  left_join(ESshort) %>%
  # clean up abbr for scale question
  mutate(abbr = gsub("_[1-9].*|_unk", "", abbr),
         survey_order = parse_number(row.names(.)))

# specify ES type for all ES's considered
provisioning <- c("Energy", "Food", "Materials")
regulating <- c("AQReg","ClimReg", "Hazards", "PestPath","SoilProtect", "freshWQReg", "coastWQReg", "OceanReg")
supporting <- c("HabCreate", "MaintainOpts", "MedGen", "Pollination")
cultural <- c("CulturePsych")
# specify ES field as factor for plotting
ESlevels <- c(provisioning, regulating, supporting, cultural)
headerLUT$ES <- factor(headerLUT$ES, levels = rev(ESlevels))






# -- ASSESS REVIEW STATUS + TIDY DATA ----
names(prelim)
# important ID-key fields to keep across question datasets are:
## ResponseID
## Q27 (Reviewer ID)
## clean_title

keydf <- distinct(dplyr::select(prelim, RecordedDate, ResponseId, Q27, clean_title))
# who has reviewed?
sort(unique(keydf$Q27))
# how many papers per reviewer?
sort(sapply(split(keydf$clean_title, keydf$Q27), length))
# how many papers have we gotten through out of initial start?
summary(unique(prelim$clean_title) %in% unique(original$Title)) # 9 either misspelled or have weird punctuation
summary(unique(original$Title) %in% unique(prelim$clean_title)) #61 still to go (4/9)..
# assign names to initals to link with original assigments
initials <- c("Aislyn" = "AK", "Anna" = "AIS", "Caitlin" = "CW", "Claire" = "CK",
              "Grant" = "GV", "Isabel" = "IS", "Julie" = "JL", "Kathryn" = "KCG",
              "Laura" = "LD", "Laurel" = "LB", "Nick" = "NBD", "Sierra" = "SDJ",
              "Tim" = "TK", "Travis" = "TM")
keydf <- left_join(keydf, data.frame(Q27 = unname(initials), Name = names(initials))) %>%
  # append review assignment info to check status on all rev1 papers accounted for
  left_join(original[c("Round2_reviewer1","Round2_reviewer2", "Title")], by = c("clean_title" ="Title")) %>%
  # clean up and re-order cols
  rename(Init = Q27, Title = clean_title) %>%
  dplyr::select(RecordedDate, ResponseId, Init, Name, Title:ncol(.))
# ggplot for status check

stat_byname <- ggplot(keydf, aes(Name, fill = Round2_reviewer1)) +
  geom_bar() +
  # add line to indicate 28 papers
  geom_hline(aes(yintercept = 28)) +
  guides(fill = guide_legend(title = "R2 rev1")) +
  labs(title = paste("Round 2 progress:", length(unique(keydf$Title)), "of", length(original$Title), "unique papers reviewed on", Sys.Date()),
       subtitle = "# papers reviewed by person (answered Qualtrics), colored by R2 reviewer 1 assignment")
# ggplot for status check
stat_byrev <- ggplot(keydf, aes(Round2_reviewer1, fill = Name)) +
  geom_bar() +
  # add line to indicate 28 papers
  geom_hline(aes(yintercept = 28)) +
  guides(fill = guide_legend(title = "Name")) +
  labs(subtitle = "# papers reviewed by R2 reviewer 1 assigned, colored by person who answered Qualtrics")
# write to repo
plot_grid(stat_byname, stat_byrev, nrow = 2)
ggsave("round2_metareview/clean_qa_data/review_status/reviewstatus_r2papers.pdf", 
       height = 6, width = 6, units = "in", scale = 1.5)

# what remains?
outstanding <- cbind(assess_date = Sys.Date(), subset(original, !Title %in% unique(keydf$Title)) %>%
                       dplyr::select(Round2_reviewer1, Round2_reviewer2, Round1_reviewer, FirstAuthor, Title, SourcePublication, PublicationYear)) %>%
  # re-assign KG to any remaining Tim and Nick papers since they're done as of 4/15
  mutate(Round2_reviewer1 = gsub("Nick|Tim", "Kathryn*", Round2_reviewer1)) %>%
  arrange(Round2_reviewer1, Title)


# who remains to reach 28 papers
effort <- data.frame(assess_date = Sys.Date(), Init = unname(initials), Name = names(initials)) %>%
  left_join(data.frame(table(keydf$Name)), by = c("Name" = "Var1")) %>%
  rename(R2_reviewed = Freq) %>%
  # if count is NA, assign 0
  replace_na(list(R2_reviewed = 0)) %>%
  # add number of rev1 outstanding
  left_join(data.frame(table(gsub("[*]", "", outstanding$Round2_reviewer1))), by = c("Name" = "Var1")) %>%
  replace_na(list(Freq = 0 )) %>%
  rename(Rev1_remain = Freq) %>%
  # add rev 2
  left_join(distinct(original[,1:2]), by = c("Name" = "Round2_reviewer1")) %>%
  group_by(Name) %>%
  mutate(pair = paste0("pair",1:2)) %>%
  ungroup() %>%
  spread(pair, Round2_reviewer2) %>%
  arrange(desc(R2_reviewed))

# write out both for LD to deal with
write_csv(outstanding, "round2_metareview/clean_qa_data/review_status/outstanding_r2papers.csv")
write_csv(effort, "round2_metareview/clean_qa_data/review_status/revieweffort_r2papers.csv")

# make tidy dataset
prelimlong <- prelim %>%
  # select cols to keep (drop Q1 -- old/incorrect title)
  dplyr::select(StartDate, EndDate, RecordedDate, ResponseId, Q27, clean_title:ncol(.)) %>%
  gather(id, answer, Q2:ncol(.)) %>%
  left_join(headerLUT) %>%
  arrange(RecordedDate) %>%
  rename(Init = Q27, Title = clean_title)

# for prelim results, just look at single/first reviewed
firstreview <- prelimlong %>%
  group_by(Title, abbr, id) %>%
  filter(!duplicated(Title)) %>%
  ungroup()
# write to cleaned data folder for now..
write_csv(firstreview, "round2_metareview/data/cleaned/prelim_singlereview.csv")


## -- 2020/03/18: LOOP TO SUSBET PAIRED REVIEWERS [only run this one time (pre spring bring)] -----
pairs <- distinct(original[,1:2])

unique(prelimlong$Init)

reviewlist <- data.frame()
for(r in 1:nrow(pairs)){
  temp <- subset(original, (Round2_reviewer1 == pairs$Round2_reviewer1[r] & Round2_reviewer2 == pairs$Round2_reviewer2[r]) |
                   (Round2_reviewer1 == pairs$Round2_reviewer2[r] & Round2_reviewer2 == pairs$Round2_reviewer1[r])) %>%
    select(Round2_reviewer1, Round2_reviewer2, FirstAuthor, Title, SourcePublication, PublicationYear) %>%
    mutate(reviewed1 = Title %in% unique(prelimlong$Title[prelimlong$Init == initials[pairs$Round2_reviewer1[r]]]),
           reviewed2 = Title %in% unique(prelimlong$Title[prelimlong$Init == initials[pairs$Round2_reviewer2[r]]]))
  names(temp)[names(temp) == "reviewed1"] <- paste0(initials[[pairs$Round2_reviewer1[r]]], "reviewed")
  names(temp)[names(temp) == "reviewed2"] <- paste0(initials[[pairs$Round2_reviewer2[r]]], "reviewed")
  
  files <- list.files("reviewcheck_20200318") %>% str_flatten()
  if(grepl(paste0(pairs$Round2_reviewer1[r], pairs$Round2_reviewer2[r]), files) | grepl(paste0(pairs$Round2_reviewer2[r], pairs$Round2_reviewer1[r]), files)){
    next
  }
  write_csv(temp, paste0("round2_metareview/clean_qa_data/review_status/reviewcheck_20200318/", pairs$Round2_reviewer1[r], pairs$Round2_reviewer2[r],"_round2progress_20200318.csv"))
  
}

## -- 2020/04/11: Reassign KG Papers (only do this once [2020/04/11]) -----
# 4/11 update: > after checking with LD, KG will do 18 rev1 that remain, Nick's 5 (she's rev2 anyway), and 5 of Tim's to get to 28

KG_outstanding <- subset(outstanding, Round2_reviewer1 == "Kathryn" | (Round2_reviewer1 == "Nick" & Round2_reviewer2 == "Kathryn"))
# subsample 5 of Tim's papers
set.seed(61)
rTim4KG <- outstanding[sample(row.names(subset(outstanding, Round2_reviewer1 == "Tim")), size = 5, replace = F),]
# id what's left for Tim so he knows
Tim_remain <- anti_join(subset(outstanding, Round2_reviewer1 == "Tim"), rTim4KG)
# papers that remain for Tim on 2020/04/11
Tim_remain$Title
# [1] "Landscape-level crop diversity benefits biological pest control" (Redlich)                                                         
# [2] "Stable nitrogen and carbon isotope ratios in wild native honeybees: the influence of land use and climate" (Taki)                         
# [3] "Science synthesis for management as a way to advance ecosystem restoration: evaluation of restoration scenarios for the Florida Everglades" (Wetzel)

# clean up cols in KG's to make readability easier
KG_outstanding <- rbind(KG_outstanding, rTim4KG) %>%
  # add full authors and abstract back in
  left_join(original[c("Title", "AuthorsFull", "Abstract", "Comments")]) %>%
  mutate(Round2_reviewer = "Kathryn") %>%
  dplyr::select(Round2_reviewer, Round1_reviewer, Comments, FirstAuthor, AuthorsFull, Title:ncol(.)) %>%
  rename(Round1_comments = Comments) %>%
  mutate_all(function(x) ifelse(is.na(x), "", x))
# write out for LD to send to KG
write_csv(KG_outstanding, "round2_metareview/clean_qa_data/review_status/ESRound2_reviewpapers_forKG_20200411.csv")

# > CTW emailed Tim and Travis 2020/04/11 to let them know of their 3 that remain --
# Tim's 3 outstanding above, and Travis' are:
outstanding$Title[outstanding$Round2_reviewer1 == "Travis"]
# [1] "Decoupled effects (positive to negative) of nutrient enrichment on ecosystem services"                                             
# [2] "Divergent flows of avian-mediated ecosystem services across forest-matrix interfaces in human-modified landscapes"                 
# [3] "Restoration potential of threatened ecosystem engineers increases with aridity: broad scale effects on soil nutrients and function"

# > otherwise, as of 2002/04/11, LD has 2 rev1 papers left (is aware), and IS, CK and SDJ know which ones they have left

# clean up environment
rm(KG_outstanding, rTim4KG, Tim_remain)






# -- NITTY GRITTY DATA CLEANING -----
# potential issues:
## 1) inconsistent answers *within* reviewed papers
## 2) questions not answered (esp questions added later [e.g. exclusion questions])
## 3) check for "exclude" in notes if person reviewed papers before all of the Q3 questions were made
## 4) inconsistent answers between double reviewed-papers [need to resolve]
## x) more to come I'm sure..

# suggested workflow..
# 1) flagging and logic checks:
# > 1a) exclude? (one of Q3 answered "Yes", check for "exclude in notes too)
# > 1b) errors/inconsistencies *within* reviews first [e.g. Kremen topics with how reviewers answered q12 and q19]
# 2) subset-double reviewed papers and screen:
# > 2a) if answer same, dissolve to one entry
# > 2b) if answers different, but answers nested (e.g. rev1 chose option a, rev2 chose options a and b, go with the more complete answer)
# > 2c) if entered notes, preserve all notes
# > 2d) if answers conflict, and can't resolve on logic checks, defer to reviewers to resolve
# 4) write out records that need reviewer feedback (put in holding)
# > double reviews AND single reviews that are inconsistent in their answers or flagging
# 5) for double-reviews that can be dissolved to one answer per question, rbind back to master (single-reviewed dataset)
# 6) write out cleaned data to data/cleaned subfolder
# x) at some point will need step to add back in re-reviewed info


## 1) Check exclusion -----
# look for "exclude" in final notes if answered before Q3b and c created
prelim$Q24[grep("exclud", prelim$Q24)] # 3 cases so far (4 returns):
exclude_notes <- c(
  "I think this paper should have been excluded in round 1 since it does not directly measure an EF/ES or a proxy, only human perception of ES",
  "Consider excluding? I'm really not sure if this paper fits....I had a really hard time filling out this form since it was more of a synthesis. They also use/mention tons of indicators so I wasn't sure how to fill out the table in Q12",
  "The ES table is not filled out because the paper is only an ecosystem service valuation paper (no drivers). I believe that it thus would have been excluded in round 1, but there were no questions to explicitly exclude it in round 2, so I filled it out (plus it may address some interesting scale questions). Specifically, the paper maps 'forest ecosystem services' by using land cover data to estimate ES provision based on forest age, contiguity, and size, assuming that old, connected and larger forests provide more ES's. It measures no services directly, nor looks at any drivers that are not already implicit in their valuation approach."
)

prelim$Q24[!is.na(prelim$Q24)] # maybe also have value for "unsure" in exclude 
## Examples: 
maybe_exclude_notes <- c(
  "tough paper to review because defining what were their response variables was difficult",
  "almost included at q3 with the thinking that abundance of crab burrows ",
  "paper didn’t do a great job at analyzing/showing the specific effects of env drivers ",
  "Looked at overall benefit/value ($) over time given land use change,",
  "This paper was confusing...",
  "it reads like a valuation of ES provision rather than a study of the ecology",
  "I don't think it clearly tests any other ecological questions about ES's",
  "I really don't know if this paper should be included",
  "No measurement of services/function except community structure",
  " (this study is in an econ journal).",
  "This paper didn't really have \"response variables\". Instead they looked at people's perceptions of services in urban parks, so everything was qualitative/descriptive. Wasn't sure how to express this in the big table!",
  "very hard to code- answers to survey about forest recovery's impact on water purification",
  "main focus of paper is on demographic processes, so considered excluding at first",
  "almost marked \"yes\" on Q3",
  "Honestly, this was a bit confusing. The paper mainly focused on the benefits of the model and the details of it.",
  "I wasn't sure if this paper should've been included. It almost seems like it stops at biodiversity but it mentions implications for ES. Also - the paper quantified diversity for animal groups based on weed index.",
  "This paper was tricky to evaluate", 
  "I'm not totally sure this paper should count towards our tallies for figure",
  "Didn't actually MEASURE services, but had expert panel weigh in on whether ES were improving or not based on land use change",
  "This paper was confusing...The response variables I listed were also drivers of change in well-being",
  "This paper is terrible science",
  "This was a challenging paper for me to read. Its main message is proposing a framework, but the authors also apply this framework to a case study, which I based my answers in this survey off of.",
  "Had lots of variables, but only experts' assessment of the influence of the driver (intervention/management strategy) on the service.",
  "Barely a data paper but did include one really small study that has barely any connection to ES. Not sure it should have made it through, but it is tough. Weird paper.",
  "The ES table is not filled out because the paper is only an ecosystem service valuation paper (no drivers). I believe that it thus would have been excluded in round 1, but there were no questions to explicitly exclude it in round 2, so I filled it out (plus it may address some interesting scale questions). Specifically, the paper maps 'forest ecosystem services' by using land cover data to estimate ES provision based on forest age, contiguity, and size, assuming that old, connected and larger forests provide more ES's. It measures no services directly, nor looks at any drivers that are not already implicit in their valuation approach.",
  "May want Laura to review if this actually includes enough ecology to retain (on the fence about whether it is only the social dimension)",
  "this paper didn't explicitly talk about ecosystem functions or ecosystem services in general nor in terms of their measured variables so that is why I left the q12 'paper reported response variables as...' blank; it did stay in since it didn't quite fall into the initial exclusion questions in this qualtrics survey",
  "This one was really tough. The main goal was determining the source of the resilience of the particular ecosystem."
)

# grab unique response ID 
# > note, can't pull by title because some double reviews have conflicting answers for whether to exclude or not
exclude_notes_ids <- prelim$ResponseId[pmatch(exclude_notes, prelim$Q24)]
maybe_exclude_ids <- prelim$ResponseId[pmatch(maybe_exclude_notes, prelim$Q24)]
prelim$Q24[pmatch(maybe_exclude_notes, prelim$Q24)]
# check it got all
# should be TRUE, if not, go back and see what got missed
length(c(exclude_notes, maybe_exclude_notes)) == length(c(exclude_notes_ids, maybe_exclude_ids))

# apply flag
# designate 1b for level 1b data (not yet corrected, but flags applied)
prelimlong1b <- prelimlong %>%
  group_by(ResponseId) %>%
  mutate(exclude = grepl("yes", str_flatten(unique(answer[qnum == "Q3" & !is.na(answer)])), ignore.case = T)) %>%
  ungroup() %>%
  mutate(exclude = ifelse(ResponseId %in% exclude_notes_ids, "Exclude",
                          ifelse(ResponseId %in% maybe_exclude_ids, "Maybe", exclude)))
# qualify exclusion
prelimlong1b$exclude_notes[prelimlong1b$exclude == "TRUE"] <- "by Q3"
prelimlong1b$exclude_notes[prelimlong1b$exclude == "Exclude"] <- "by survey notes"
prelimlong1b$exclude_notes[prelimlong1b$exclude == "Maybe"] <- "uncertainy in survey notes"
# clean up exclude values
prelimlong1b <- mutate(prelimlong1b, exclude = recode(exclude, "TRUE" = "Exclude", "FALSE" = "Keep"))

# how many papers excluded?
dplyr::select(prelimlong1b, ResponseId, Title, exclude) %>%
  distinct() %>%
  # make exclusion cats factor
  mutate(exclude = factor(exclude, levels = c("Keep", "Maybe", "Exclude"))) %>%
  group_by(Title) %>%
  mutate(reviews = length(ResponseId)) %>%
  #mutate(unique_paper = 1:nrow(prelim)) %>%
  ungroup() %>%
  ggplot(aes(exclude, Title, group = Title)) +
  geom_line() +
  geom_point(alpha = 0.6) +
  theme(axis.text.y = element_blank(),
        panel.grid.major.y = element_blank()) +
  labs(y = "Unique paper", x = "Exclude?",
       title = paste("Round 2 data QA, Q3 vs. final notes:\nExclude paper? (arrayed by # of reviewers per paper),", Sys.Date())) +
  facet_wrap(~reviews)
# I think either way, whether paper is a maybe exclude or definite exclude, answers that don't agree should be flagged for consesus among reviewers
ggsave("round2_metareview/clean_qa_data/qafigs/r2qa_q3excludepaper.pdf", width = 4, height = 4, units = "in", scale = 1.5)

# write out potential exclusion set for LD to judge
## read in current to indicate whether LD might have already reviewed
current_possibleexclude <- read.csv("round2_metareview/clean_qa_data/needs_classreview/excludenotes_review.csv", na.strings = na_vals)

possibleexclude_df <- dplyr::select(prelimlong1b, ResponseId, Init, Title, exclude, exclude_notes) %>%
  distinct() %>%
  # make exclusion cats factor
  mutate(exclude = factor(exclude, levels = c("Keep", "Maybe", "Exclude"))) %>%
  group_by(Title) %>%
  mutate(reviews = length(ResponseId),
         # add flag for inconsistent exclusion answer
         flag_inconsistent = (reviews >1 & length(unique(exclude)) > 1)) %>%
  #mutate(unique_paper = 1:nrow(prelim)) %>%
  ungroup() %>%
  # remove anything that is clear exclude on Q3 or is to keep
  filter((exclude %in% c("Exclude", "Maybe") & exclude_notes != "by Q3") |
           # keep papers with inconsistent reviews
           flag_inconsistent) %>%
  # join survey notes
  left_join(distinct(prelimlong1b[prelimlong1b$abbr == "SurveyNotes", c("ResponseId", "Title", "answer")])) %>%
  rename(SurveyNotes = answer) %>%
  #add assess_date and reorder cols
  mutate(assess_date = Sys.Date()) %>%
  # join study info for review convenience
  left_join(original[c("Title", "FirstAuthor",  "PublicationYear", "SourcePublication","Abstract")]) %>%
  dplyr::select(assess_date, ResponseId:exclude_notes, flag_inconsistent:ncol(.)) %>%
  arrange(Title, Init) %>%
  # add col for already pulled just in case LD has already reviewed
  mutate(newcase = !Title %in% unique(current_possibleexclude$Title)) %>%
  # move new case to after assess_date
  dplyr::select(assess_date, newcase, ResponseId:ncol(.))
# change NAs to blanks so not annoying in Excel
possibleexclude_df[is.na(possibleexclude_df)] <- ""
# write out
write_csv(possibleexclude_df, "round2_metareview/clean_qa_data/needs_classreview/excludenotes_review.csv")



## 2) Screen notes for possible issues -----
## read in current to indicate whether LD/IS might have already reviewed
current_ecosystemnotes <- read.csv("round2_metareview/clean_qa_data/needs_classreview/ecosystemnotes_review.csv", na.strings = na_vals)

Qs_wnotes <- headerLUT$qnum[grep("notes", headerLUT$abbr, ignore.case = T)]
View(subset(headerLUT, qnum %in% Qs_wnotes))
# 2a) Ecosystem (Q4)
sort(with(prelimlong1b, answer[abbr == "EcosystemNotes" & !is.na(answer) & exclude != "Exclude"])) # & exclude == "Keep"
# how did they code these?
q4_qa <- group_by(prelimlong1b, ResponseId) %>%
  filter(ResponseId %in% ResponseId[abbr == "EcosystemNotes" & !is.na(answer) & exclude != "Exclude"]) %>%
  ungroup() %>%
  filter(qnum == "Q4") %>%
  # add in assess_date
  mutate(assess_date = Sys.Date()) %>%
  dplyr::select(assess_date, ResponseId, Init, Title, abbr, answer) %>%
  spread(abbr, answer) %>%
  splitcom(keepcols = c("assess_date", "ResponseId", "Init", "Title", "EcosystemNotes", "Ecosystem"), splitcol = "Ecosystem") %>%
  rename(Ecosystem = answer) %>%
  # add col to indicate double review for comparison
  group_by(Title) %>%
  mutate(doublerev = length(unique(ResponseId))>1) %>%
  ungroup() %>%
  arrange(EcosystemNotes, Title) %>%
  # add citation info in case want to look at paper
  left_join(dplyr::select(original, Title, FirstAuthor, PublicationYear, SourcePublication)) %>%
  # indicate whether new case
  mutate(newcase = !Title %in% unique(current_ecosystemnotes$Title)) %>%
  # reorder cols
  dplyr::select(assess_date, newcase, ResponseId:ncol(.))

# set responseID as factor so ecosystem types will plot alphabetically
q4_qa %>%
  mutate(ResponseId = factor(ResponseId, levels = unique(ResponseId[order(EcosystemNotes)]))) %>%
  ggplot(aes(ResponseId, fill = Ecosystem)) +
  geom_bar(alpha = 0.65) +
  geom_text(aes(y = 0, label = EcosystemNotes), size = 2.8, color = "black", hjust = 0) +
  scale_y_discrete(expand = c(0.01,0.01)) +
  labs(title = paste("Round 2 data QA, Q4:\nOf answers with notes, ecosystem(s) selected,", Sys.Date()),
       subtitle = "Black text = entered note, ordered on y alphabetically to compare systems selected") +
  coord_flip() +
  theme(axis.text.y = element_blank())
# write out for class review
ggsave("round2_metareview/clean_qa_data/qafigs/r2qa_q4ecosystemnotes.pdf", width = 5, height = 4, units = "in", scale = 1.5)
# write out ecosystem notes for review
## change NA's to blanks so easier for reading in Excel
q4_qa[is.na(q4_qa)] <- ""
write_csv(q4_qa, "round2_metareview/clean_qa_data/needs_classreview/ecosystemnotes_review.csv")


# 2b) Methods (Q6)
# read in current methods notes
current_methodsnotes <- read.csv("round2_metareview/clean_qa_data/needs_classreview/methodsnotes_review.csv", na.strings = na_vals)
sort(with(prelimlong1b, answer[abbr == "MethodsNotes" & !is.na(answer)  & exclude != "Exclude"]))
sort(with(prelimlong1b, answer[abbr == "GenInfo" & !is.na(answer) & exclude != "Exclude"]))
# not sure if full GenInfo are preserved? might be byte limitation in csv format..
# > important to address in data cleaning (let class decide)
# GenInfo: [2] "*** THIS STUDY DID NOT TAKE PLACE IN AUSTRALIA. It was in New Zealand (Oceania)"
methodsnotes <- prelimlong1b %>%
  # remove papers to exclude
  subset(exclude != "Exclude") %>%
  group_by(ResponseId) %>%
  filter(ResponseId %in% ResponseId[(abbr == "MethodsNotes" & !is.na(answer)) | ((abbr == "GenInfo" & !is.na(answer)))]) %>%
  ungroup() %>%
  subset(abbr %in% c("Ecosystem", "TimeTrends","MultiScale", "MethodsNotes", "GenInfo","Methods")) %>%
  dplyr::select(ResponseId, Init, Title, abbr, answer) %>%
  spread(abbr, answer) %>%
  # edit comma in observational answer so doesn't split
  # also shorten time trends for readability
  mutate(Methods = gsub("Observational .* directly)", "Observational", Methods),
         Methods = gsub("/", "_", Methods),
         TimeTrends = gsub(" \\(e.g.*$", "", TimeTrends)) %>%
  # break out answers by comma
  splitcom(keepcols = names(.), splitcol = "Methods") %>%
  # make wide for easier class review
  mutate(num = 1) %>%
  spread(answer, num, fill = "") %>%
  # add in assess date
  mutate(assess_date = Sys.Date()) %>%
  # add col for double reviews for comparison
  group_by(Title) %>%
  mutate(doublerev = Title %in% Title[duplicated(Title)]) %>%
  ungroup() %>%
  # reorder cols
  dplyr::select(assess_date, doublerev, ResponseId, Init, Title, GenInfo, MethodsNotes, Observational, Experimental, `Model_Data Simulation`, 'Social survey_Interview', Other, Ecosystem, MultiScale, TimeTrends) %>%
  # order by by title for easier comparison
  arrange(Title, Init) %>%
  # add col for newcases and reorder cols
  mutate(newcase = !Title %in% unique(current_methodsnotes$Title)) %>%
  dplyr::select(assess_date, newcase, doublerev:ncol(.))
# make all text so no "NA" in csv
methodsnotes[is.na(methodsnotes)] <- ""  
# write out for review
write_csv(methodsnotes, "round2_metareview/clean_qa_data/needs_classreview/methodsnotes_review.csv")

# 2c) Scale (Q9)
# read in current scalenotes to check for new titles added
current_scalenotes <- read.csv("round2_metareview/clean_qa_data/needs_classreview/scalenotes_review.csv", na.strings = na_vals)
sort(with(prelimlong1b, answer[abbr == "ScaleNotes" & !is.na(answer)  & exclude != "Exclude"]))
# punt to Grant!
# except correct these:
#[120] "need to update this bc cant unselect . These three radii (2 km, 1 km and 300 m) were considered three different spatial scales in order to cover the “scales of effect” of bees with different foraging distances"
#[108] "ignore the 0 under 501m2. there were 19 sites, 3 large treatment plots per site, and 9 1m2 plots within each treatment plot"
#[171] "They consider how ES quantity and quality changes when you look at just local scales vs including regional scales; Data are remotely sensed, continuously over the landscape (meaning no 'sites' or 'plots'), but the size of the land areas included in the two scales is also unclear"
# > CTW considered pixel as "plot" (i think?)
#"Plot size is less than 1m^2!!
# "from table 1 in paper - **** delete the unclear or not specified and 1-10 km2***"
# "couldn't unclick the 100-1000km row there so just selected 0."
#"accidentally typed 0 for the # of plots 1-25m2 and for the number of sites at 101-500m2"

# write out scale notes, along with multiscale answer, sites and plots checked (ignore number checked?)
# pull actual scale intervals for review clarity
scaleintervals <- subset(headerLUT, qnum == "Q9" & !is.na(Group)) %>%
  mutate(interval = trimws(gsub("^.* whole study -", "", fullquestion, perl = T)))

scalenotes <- prelimlong1b %>%
  # remove papers to exclude
  subset(exclude != "Exclude") %>%
  group_by(ResponseId) %>%
  filter(ResponseId %in% ResponseId[(abbr == "ScaleNotes" & !is.na(answer)) | ((abbr == "GenInfo" & !is.na(answer)))]) %>%
  ungroup() %>%
  subset(abbr == "GenInfo" | qnum %in% c("Q8", "Q9", "Q10", "Q11")) %>%
  #replace_na(list(Group = "")) %>%
  unite(abbr,abbr, Group) %>%
  mutate(abbr = gsub("_NA", "", abbr)) %>%
  dplyr::select(ResponseId, Init, Title, abbr, answer) %>%
  spread(abbr, answer) %>%
  # gather sites and plots
  gather(met, val, names(.)[grep("^Sites|^Plots", names(.))]) %>%
  separate(met, into =c("abbr", "Group"), sep = "_") %>%
  # remove any NAs or 0 vals
  filter(!is.na(val) & !val == "0") %>%
  left_join(scaleintervals[c("abbr", "Group", "interval")]) %>%
  mutate(Group = factor(Group, levels = headerLUT$Group[headerLUT$abbr == "Plots"])) %>%
  arrange(Title, ResponseId, abbr, Group) %>%
  group_by(ResponseId) %>%
  mutate(entered = str_flatten(unique(abbr))) %>%
  ungroup() %>%
  mutate(nothing_entered = ifelse(entered == "PlotsSites", NA, entered),
         nothing_entered = recode(nothing_entered, "Sites" = "Plots", "Plots" = "Sites")) %>%
  group_by(ResponseId, abbr) %>%
  mutate(group_levels = 1:length(Group),
         assess_date = Sys.Date()) %>%
  ungroup() %>%
  rename(interval_abbr = Group, group = abbr, count = val) %>%
  # add col for double reviews for comparison
  group_by(Title) %>%
  mutate(doublerev = length(unique(Init)) >1) %>%
  ungroup() %>%
  dplyr::select(assess_date, doublerev, ResponseId:Title, GenInfo:group, group_levels, interval_abbr, interval, count, nothing_entered, Connect, ConnectDist) %>%
  # order by title for easier comparison
  arrange(Title, Init, group) %>%
  # add "." in front of range so excel doesn't read it as date
  mutate(count = paste0(".", count)) %>%
  # add in their response for KT4
  left_join(distinct(cbind(prelimlong1b[prelimlong1b$abbr == "KremenTopics" & grepl("Topic 4", prelimlong1b$answer),c("ResponseId", "Title")], KT4_scale = 1))) %>%
  replace_na(list("KT4_scale"=0)) %>%
  # indicate new titles added with updates, reorder cols
  mutate(newcase = !Title %in% unique(current_scalenotes$Title)) %>%
  dplyr::select(assess_date, newcase, doublerev:ncol(.)) %>%
  # also get TimeTrends in there.. should have been
  left_join(distinct(prelimlong1b[prelimlong1b$abbr == "TimeTrends", c("ResponseId", "Title","answer")])) %>%
  # clean up time trends
  rename(TimeTrends = answer) %>%
  mutate(TimeTrends = gsub(" \\(e.g.*$", "", TimeTrends))

# make all text so no "NA" in csv
scalenotes[is.na(scalenotes)] <- ""  
# write out for review
write_csv(scalenotes, "round2_metareview/clean_qa_data/needs_classreview/scalenotes_review.csv")



# 2d) Kremen topics addressed
# read in current to ID new cases
current_kremennotes <- read.csv("round2_metareview/clean_qa_data/needs_classreview/kremennotes_review.csv", na.strings = na_vals)
sort(with(prelimlong1b, answer[abbr == "KremenNotes" & !is.na(answer)  & exclude != "Exclude"]))
# think we want to review response and drivers listed, regardless of ES specified, along with kremen answers
kremennotes <-  prelimlong1b %>%
  # remove papers to exclude
  subset(exclude != "Exclude") %>%
  # select variables of interest
  subset(grepl("Drive|Response|Kremen|GenInfo|SurveyNotes", abbr, ignore.case = T)| qnum =="Q14") %>%
  # add in assessment date
  mutate(assess_date = Sys.Date()) %>%
  # ignore ES and collapse to distinct
  dplyr::select(assess_date, ResponseId, Init, Title, abbr, Group, answer) %>%
  distinct() %>%
  # remove NA answers
  filter(!is.na(answer)) %>%
  # bind driver group to abbr
  unite(abbr, abbr, Group) %>%
  mutate(abbr = gsub("_NA", "", abbr)) %>%
  #collapse answers by abbr so can spread (e.g. multiple responses across ES's won't spread) %>%
  group_by(ResponseId, abbr) %>%
  mutate(answer = str_flatten(unique(answer), collapse = ",")) %>%
  ungroup() %>%
  distinct() %>%
  spread(abbr, answer) %>%
  # clean up kremen topics for splitting (make less wordy)
  mutate(KremenTopics = gsub("Kremen Topic [1-4] : | influencing function| that.*[)]", "", KremenTopics)) %>%
  # split Kremen Topics selected
  splitcom(keepcols = names(.), splitcol = "KremenTopics") %>%
  # make positive selection = 1
  mutate(num = 1) %>%
  # spread out KT's
  spread(answer, num, fill = 0) %>%
  rename_all(function(x) gsub(" ", "_", x)) %>%
  # screen for other checked in Q12 before joining other text listed
  mutate(flag_otherEnv =  (!grepl("Other", Driver_Env, ignore.case = T) & !is.na(OtherDriver_Env)), # (grepl("Other", Driver_Env, ignore.case = T) & !is.na(OtherDriver_Env) |
         flag_otherBio = (!grepl("Other", Driver_Bio, ignore.case = T)  & !is.na(OtherDriver_Bio)),
         flag_otherAnthro = (!grepl("Other", Driver_Anthro, ignore.case = T)  & !is.na(OtherDriver_Anthro))) %>%
  # join scale info to assess KT scale topic
  left_join(prelimlong[prelimlong$abbr == "MultiScale", c("ResponseId", "Title", "answer")]) %>%
  rename(MultiScale = answer) %>%
  # join time info to assess KT scale topic
  left_join(prelimlong[prelimlong$abbr == "TimeTrends", c("ResponseId", "Title", "answer")]) %>%
  rename(TimeTrends = answer) %>%
  # clean up TimeTrends answers for readability
  mutate(TimeTrends = gsub(" [(]e.g.*[)]$", "", TimeTrends)) %>%
  # add col for double reviews for comparison
  group_by(Title) %>%
  mutate(doublerev = Title %in% Title[duplicated(Title)]) %>%
  ungroup() %>%
  # reorder cols
  dplyr::select(assess_date, doublerev, ResponseId, Init, Title, GenInfo, SurveyNotes, KremenNotes, None, Response, 
                # KT topic 1 cols (ESP)
                ESPs, Driver_Bio, flag_otherBio, OtherDriver_Bio, ESP_type,
                # KT topic 2 cols (community structure)
                Community_structure, 
                # KT topic 3 cols
                Environmental_factors, Driver_Env, flag_otherEnv, OtherDriver_Env,
                # KT topic 4 cols
                Scale, MultiScale, TimeTrends,
                # human drivers
                Driver_Anthro, flag_otherAnthro, OtherDriver_Anthro) %>%
  # arrange by Title for comparison
  arrange(Title, Init) %>%
  # rename Kremen Topics cols to indicate source of data
  rename_at(.vars = c("None", "ESPs", "Community_structure", "Environmental_factors", "Scale"), function(x) paste0("KT_", x)) %>%
  # add col to ID new case
  mutate(newcase = !Title %in% unique(current_kremennotes$Title)) %>%
  # join exclude col just in case we only want to review things we definitely will keep
  left_join(distinct(prelimlong1b[c("ResponseId", "Title", "exclude")])) %>%
  dplyr::select(assess_date, newcase, exclude, doublerev:ncol(.))
# unite main and other drivers
# make all text so no "NA" in csv
kremennotes[is.na(kremennotes)] <- ""  
# write out for review
write_csv(kremennotes, "round2_metareview/clean_qa_data/needs_classreview/kremennotes_review.csv")


# visualize kremen topics for QA
# scale: space and time
ggplot(kremennotes, aes(MultiScale, fill = as.factor(KT_Scale))) +
  geom_bar() +
  labs(title = "Round 2 data QA, Kremen Scale:",
       subtitle = paste("MultiScale (Q9) answer (x-axis) vs. Time Trends (Q7) (panel), vs. Kremen Scale topic (Q13),",Sys.Date()),
       x = "MultiScale (spatial)") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_grey(name = "KT4 (scale) checked?", labels = c("0" = "No", "1" = "Yes")) +
  theme(legend.position = "bottom") +
  facet_wrap(~TimeTrends)
ggsave("round2_metareview/clean_qa_data/qafigs/r2qa_kremenscale.pdf",
       width = 6.5, height = 6, units = "in", scale = 1.2)


# of those where ESP selected in Biotic Driver, how did they answer ESP_type?
# clean up kremen ESPs for simplicity in plotting
mutate(kremennotes, ESP_type = str_replace_all(ESP_type, " \\[e.g.,? ([:alpha:]|,|[:blank:])+\\]", ""),
       ESP_type = gsub("  ,| ,", ",", ESP_type),
       ESP_type = gsub(",", ", ", ESP_type),
       ESP_type = ifelse(ESP_type == "", "(Nothing selected)", ESP_type)) %>%
  subset(KT_ESPs == 1) %>%
  ggplot(aes(forcats::fct_infreq(ESP_type), fill = as.factor(KT_Community_structure))) +
  geom_bar() +
  coord_flip() +
  labs(x = "(Q13) ESP type(s) selected", 
       title = "Round 2 data QA, Kremen ESPs (Q12 v. 13 v. 14):\nWhen Kremen T1 (ESP) selected (Q13), what ESP types selected (Q14)?",
       subtitle = paste("Colored by whether KT2 (Community structure) checked (Q13),", Sys.Date())) +
  scale_y_continuous(expand = c(0.0,0.01)) +
  scale_fill_grey(name = "(Q13)\nCommunity\nStructure?", labels = c("0" = "No", "1" = "Yes")) +
  theme(legend.position = c(0.99,0.99),
        legend.justification = c(1,1)) +
  facet_wrap(~grepl("Service Provider", Driver_Bio), nrow = 2, scale = "free_y", labeller= labeller('grepl("Service Provider", Driver_Bio)' = c("FALSE" = "Q12 Biotic Driver: No ESP", "TRUE"= "Q12 Biotic Driver: Yes ESP")))
ggsave("round2_metareview/clean_qa_data/qafigs/r2qa_q14kremenESPs.pdf",
       width = 8, height = 5, units = "in", scale = 1.2)


# pull ESP


# 3) If-then questions -----
# 1) Time


# 2) Multiple scales

# 3) Connectivity


# 4) Kremen topics -----
# 0) pull NO Kremen checked (usually at least 1 should be? probably some cases where they really didn't, and those would be interesting to pull out)
noKremen <- kremennotes %>%
  subset(Title %in% Title[KT_None == 1]) %>%
  dplyr::select(-newcase)

# 1) If ESP checked in Biotic, ESP checked in Kremen
## kremennotes includes ALL papers not excluded (even "maybe" exclude)
## just need to pull ESP answers, and add GenInfo and SurveyNotes to screen for ESPs mentioned
## flag if ESP checked in biotic driver (or other.. keyword search?, or spp checked in ESP types?) but KT ESP NOT checked, or no service provider in biotic driver (and none indicated in ESP types Q) but KT ESP *IS* checked
ESPcheck <- dplyr::select(kremennotes, assess_date, exclude:ESP_type, KT_Community_structure) %>% distinct() %>%
  # > we weren't clear for ESP question whether we referred to ESP as driver or (as driver OR response) 
  # > can't use Q14 so easily to auto-assign 1 to kremen ESP when Q14 has species indicated
  # > BUT does seem like if Service Provider entered in Biotic driver, Kremen ESP topic should be checked
  # flag and write out for someone else to volunteer dealing with
  # case where Kremen ESP checked but Service Provider not in Driver
  mutate(flag_ktESP_noSerPro = (KT_ESPs == 1 & !grepl("Service", Driver_Bio)),
         # Service provider not selected in biotic driver but ESP indicated in ESP Type (Q14)
         flag_BioDrive_ESPtypehasESP = grepl("ESP", ESP_type) & !grepl("Service", Driver_Bio),
         # KT ESP not checked but service provider in biotic drivers
         flag_ktESP_unchecked = (KT_ESPs == 0 & grepl("Service",Driver_Bio))) %>%
  # only keep paper titles flagged
  filter(Title %in% Title[c(flag_ktESP_noSerPro, flag_BioDrive_ESPtypehasESP, flag_ktESP_unchecked)]) %>%
  # join paper citation info
  left_join(original[c("Title", "SourcePublication", "PublicationYear", "Abstract")])
# change any NAs to "
ESPcheck[is.na(ESPcheck)] <- ""
# write out
write_csv(ESPcheck, "round2_metareview/clean_qa_data/needs_classreview/KTesp_check.csv")


# 2) Keyword search variables for Community structure
# keywords to screen in driver variables (or geninfo and survey notes).. don't think we should code based response vars if end point is diversity or abundance and not actually function
# structure in kremen is about structure influecing function
# name keywords
structurewords <- c("abund|richn|shannon|divers|biodiv|evenn|size|mass")
structurecheck <- dplyr::select(kremennotes, assess_date:KT_Community_structure) %>% distinct() %>%
  # flag for papers where structure NOT checked, but ESP type indicates otherwise
  mutate(not_checked = (KT_Community_structure==0 & (grepl("Multiple|Across|Within", ESP_type) | grepl(structurewords, OtherDriver_Bio, ignore.case = T))),
         false_checked = KT_Community_structure==1 & (!grepl("Multiple|Across|Within", ESP_type) & !grepl(structurewords, OtherDriver_Bio, ignore.case = T)),
         # combine flags
         flag_KTstructure = (not_checked | false_checked),
         flag_structure_explanation = ifelse(not_checked, "Not checked; ESP type or keywords suggest structure",
                                             ifelse(false_checked, "Structure checked; ESP type and driver keywords do NOT suggest structure",
                                                    ""))) %>%
  # drop not_checked and false_checked
  dplyr::select(-c(newcase, not_checked, false_checked)) %>%
  # subset to titles where flag == TRUE
  subset(Title %in% Title[flag_KTstructure]) %>%
  # add paper citation info for reference
  left_join(original[c("Title", "FirstAuthor", "SourcePublication", "PublicationYear", "Abstract")])
# convert NAs to blank cells
structurecheck[is.na(structurecheck)] <- ""
# write out for IS
write_csv(structurecheck, "round2_metareview/clean_qa_data/needs_classreview/KTstructure_check.csv")


# 3) If environmental variables checked or listed, Environment checked
envcheck <- dplyr::select(kremennotes, assess_date, exclude:Response, KT_Environmental_factors:OtherDriver_Env) %>% distinct() %>%
  # case where KT Environmental factors not selected, but environmental drivers entered
  mutate(flag_KTenv_uncheck = (nchar(Driver_Env)>0 | nchar(OtherDriver_Env) > 0) & KT_Environmental_factors == 0,
         # case where KT Env Fac selected, but no environmental drivers entered [*but could be that env var is in response.. should that count as addressing a KT topic?]
         flag_KTenv_checked = (nchar(Driver_Env) == 0 & nchar(OtherDriver_Env) == 0) & KT_Environmental_factors == 1) %>%
  # subset to flagged papers only
  # subset to titles where flag == TRUE
  subset(Title %in% Title[c(flag_KTenv_uncheck, flag_KTenv_checked)]) %>%
  # add paper citation info for reference
  left_join(original[c("Title", "FirstAuthor", "SourcePublication", "PublicationYear", "Abstract")])
# convert NAs to blank cells
envcheck[is.na(envcheck)] <- ""
# write out for IS
write_csv(envcheck, "round2_metareview/clean_qa_data/needs_classreview/KTenvironment_check.csv")


# 4) If multiple scales checked (time or space), Scale checked
## > Grant and Julie handling this. Will define rules for flag, CTW will incorp in code




# -- APPLY CORRECTIONS -----


# -- CONDENSE DOUBLE REVIEWED ----
doubleprelim <- subset(prelimlong1b, Title %in% doubletitles) %>%
  group_by(Title, id) %>%
  mutate(same_answer = length(unique(answer)) ==1) %>%
  ungroup() %>%
  filter(!(same_answer & is.na(answer))) %>%
  arrange(Title, survey_order, RecordedDate)
# how many double reviewed?
length(unique(doubleprelim$Title))
# who?
sapply(split(doubleprelim$Title, doubleprelim$Init), function(x) length(unique(x)))
write_csv(doubleprelim, "round2_metareview/data/intermediate/round2_doublereviewed_tidy.csv")

# where are the most inconsistencies (by question)?
dplyr::select(doubleprelim, Title, abbr, same_answer) %>%
  distinct() %>%
  ggplot(aes(same_answer)) +
  geom_bar() +
  facet_wrap(~abbr)

# add flag for answer inconsistencies:
# 1) exclusion answers that don't agree


# -- APPLY CORRECTIONS TO DOUBLE REVIEWED -----



# -- WRITE OUT CLEANED L1 DATASET FOR POST-PROCESSING AND ANALYSIS -----



# -- EXTRACT WORDS USED FOR ES RESPONSE AND DRIVERS ----
# what's been classed as EF?
# what's been classed as ES?

# ES question
# question 25 (response variables)
q25df <- subset(firstreview, qnum =="Q12") %>%
  filter(!is.na(answer))

responses <- subset(q25df, qnum == "Q12" & abbr =="Response")
yclass <- subset(q25df, qnum == "Q12" & abbr == "Yclass") %>%
  dplyr::select(StartDate, Title, Init, ES, answer) %>%
  separate(answer, c("type1", "type2", "type3", "type4"), sep = ",") %>%
  # remove anything that's all NA
  dplyr::select(names(.)[sapply(., function(x) !all(is.na(x)))])
# join EFs and Es
ESresponses <- left_join(responses, yclass)

response_summary <- responses %>%
  mutate(answer =  casefold(answer)) %>%
  group_by(ES, answer) %>% #ES, 
  # get freq of each answer
  summarise(count = length(qnum))

response_summary2 <- ESresponses %>%
  mutate(answer =  casefold(answer)) %>%
  gather(yclass_num, yclass, type1, type2) %>%
  mutate(yclass_num = parse_number(yclass_num)) %>%
  filter(!(yclass_num == 2 & is.na(yclass))) 

response_summary3 <- response_summary2 %>%
  group_by(ES, yclass, answer) %>% #ES, 
  # get freq of each answer
  summarise(count = length(qnum)) %>%
  ungroup() %>%
  rename(yresponse = answer) %>%
  separate(yresponse, paste0("answer", 1:50), ",") %>%
  # remove anything that's all NA
  dplyr::select(names(.)[sapply(., function(x) !all(is.na(x)))]) %>%
  dplyr::select(-count) %>%
  gather(response_num, yresponse, answer1:ncol(.)) %>%
  filter(!is.na(yresponse)) %>%
  mutate(yresponse = trimws(yresponse)) %>%
  group_by(ES, yclass, yresponse) %>%
  summarise(count = length(response_num)) %>%
  ungroup() %>%
  arrange(yresponse, yclass)

write_csv(response_summary3, "round2_metareview/data/intermediate/ESresponse_summary.csv")

subset(response_summary) %>%
  ggplot(aes(answer, count)) +
  geom_col() +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank()) +
  facet_wrap(~ES, scales = "free_x")



# -- PRELIM SUMMARY FIGURE ----

select(q25df, Init, Title, ES) %>%
  filter(!is.na(ES)) %>%
  distinct() %>%
  ggplot(aes(ES)) +
  geom_bar() +
  coord_flip()

ytypefig <- select(q25df, Init, Title, ES, abbr, answer) %>%
  filter(abbr == "Yclass") %>%
  distinct() %>%
  mutate(answer = gsub("Proxy for ES", "ES proxy", answer),
         answer = factor(answer, levels = c("EF", "ES,EF", "ES", "ES,ES proxy", "ES proxy"))) %>%
  ggplot(aes(ES, fill = answer)) +
  geom_bar(color = "grey30") +
  labs(x = "Ecosystem service", y = "# of papers") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_viridis_d(name = "Response\ntype") +
  facet_wrap(~"") +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        #legend.justification = c(-1,-1),
        legend.title = element_text(size = 10, face = "bold"),
        axis.title = element_text(face = "bold")) +
  guides(fill = guide_legend(nrow = 2)) +
  coord_flip()
ytypefig

test <- select(q25df, Init, Title, ES, abbr, Group, answer) %>%
  # for now fill down any NAs with ES above it, grouped by Title
  group_by(Title) %>%
  fill(ES) %>%
  ungroup() %>%
  filter(grepl("Drive", abbr)) %>%
  distinct() %>%
  # remove other from Driver (assume if there was something people answered)
  mutate(count_driver = str_count(answer, "(,|;)(?!Other)"),
         #if Exploitation, subtract 1 bc has comma in answer
         #count_driver = ifelse(grepl("Exploitation [(]hunt", answer), count_driver-1, count_driver),
         count_driver = ifelse(!grepl("Other|Exploit", answer), count_driver+1, count_driver)) %>%
  select(-answer) %>%
  group_by(Init, Title, ES, Group) %>%
  summarise(CountDrivers = sum(count_driver, na.rm = T)) %>% #IDK if we can use other...
  ungroup()
# count the number of commas in each -- assume there is 1 answer provided if not NA

ggplot(test, aes(ES, fill = Group)) +
  geom_bar() +
  coord_flip()

driversfig <- test %>%
  mutate(CountDrivers = ifelse(CountDrivers == 0, 1, CountDrivers)) %>%
  group_by(Init, Title, ES, Group) %>%
  summarise(numberDrivers = sum(CountDrivers)) %>%
  ungroup()  %>%
  ggplot(aes(ES, fill = as.factor(numberDrivers))) + #as.factor(numberDrivers)
  geom_bar(color = "grey30") +
  labs(y = "# of papers") +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(name = "Number of\ndrivers", values = colors()[591:606]) + #length(unique(test$CountDrivers))
  facet_wrap(~Group, labeller = as_labeller(c("Anthro" = "Human", "Bio" = "Biotic", "Env"="Environmental"))) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_text(face = "bold"),
        legend.title = element_text(size = 10, face = "bold")) +
  guides(guide_legend(byrow = T)) +
  coord_flip()

prelimfig <- plot_grid(ytypefig, driversfig, nrow = 1, labels = "AUTO", align = "v") #, 
ggsave("round2_metareview/figs/round2_prelimfig.pdf", prelimfig, 
       width = 8, height = 5, units = "in", scale = 1.1)  
# google slides doesn't like pdfs
ggsave("round2_metareview/figs/round2_prelimfig.png", prelimfig, 
       width = 8, height = 5, units = "in", scale = 1.1)  
