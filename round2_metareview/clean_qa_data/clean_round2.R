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
# 5/20 all exploratory figs and review QA datasets commented out since we've completed survey, so there will be no new updates

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
library(readxl)
library(lubridate)
library(cowplot)
options(stringsAsFactors = F)
na_vals <- c("NA", NA, "NaN", NaN, " ", "", ".")
theme_set(theme_bw())

# read in raw data
rawdat <- list.files("round2_metareview/data/raw", full.names = T)
## skip first two lines to read data col types correctly
qualtrix <- read.csv(rawdat[!grepl("scale", rawdat)], na.strings = na_vals, skip  = 2) 
## re-read, no skipping, for colnames
headers <- read.csv(rawdat[!grepl("scale", rawdat)], na.strings = na_vals)
## read in header lookup table (exported headers and typed in abbreviations and group assignments bc easier doing in Excel than R)
headerLUT <- read.csv("round2_metareview/data/headersLUT.csv", na.strings = na_vals)

# read in new scale data from Grant and Julie
newscale <- read.csv(rawdat[grepl("scale", rawdat)], na.strings = na_vals)

# original round 2 assignment
original <- read.csv("round1_exclusion/output/review_assignments_round2_grpdsubset.csv", na.strings = na_vals)
# excluded papers from round 1 (to compile with excluded papers from round 2 and write out)
exclude_r1 <- read.csv("round1_exclusion/output/exclude_round1.csv", na.strings = na_vals)

# function for breaking out comma separated answers
splitcom <- function(df, keepcols = c("Title", "answer"), splitcol = "answer"){
  df <- df[keepcols] %>%
    # rename column to split as answer
    rename_at(splitcol, function(x) x <- "answer") %>%
    #dplyr::select_vars(keepcols) %>%
    distinct() %>%
    # break out answers by comma
    separate(answer, paste0("v",1:30), ",") # <- this will throw warning, fine. pad with extra cols for up to 20 comma-separated answers just in case
  # break pipe to ID empty vcols
  emptycols <- names(df)[grepl("^v[2-9]|^v[[:digit:]]{2,}$", names(df))] # v1 will always be a col to keep even if NA
  emptycols <- emptycols[sapply(df[emptycols], function(x) all(is.na(x)))]  
  # restart pipeline  
  df <- df %>%
    # remove cols that are all na
    dplyr::select(-(emptycols)) %>%
    # gather to tidy
    gather(num, answer, names(.)[grep("^v[[:digit:]]+$", names(.))]) %>% #v1:ncol(.)
    mutate(answer = trimws(answer)) %>%
    # make number of answers numeric
    mutate(num = parse_number(num)) %>%
    # remove any NAs in answer
    filter(!(is.na(answer)|answer %in% c("", " "))) # dat still tidy at this point
  return(df)
}


# read in reviewer revisions/comments/corrections
corrections <- list.files("round2_metareview/data/reviewer_revisions", full.names = T)
# > individual corrections
IScorrections <- read.csv(corrections[grep("ISreview", corrections)], na.strings = na_vals)
AIScorrections <- read.csv(corrections[grep("AIS", corrections)], na.strings = na_vals)
SDJcorrections <- read_excel(corrections[grep("SDJ", corrections)], na = na_vals, trim_ws = T)
# > NOTE!: Julie sent written corrections in an email
# > paper exclusions
excludecorrections <- read.csv(corrections[grep("exclude", corrections)], na.strings = na_vals)
# > ecosystem classification
systemcorrections <- read.csv(corrections[grep("ecosystem_class", corrections)], na.strings = na_vals)
# >> Isabel's wetland reclassification
wetlandcorrections <- read_excel(corrections[grep("wetland_abs", corrections)], sheet = 1, na = na_vals, trim_ws = T)
wetlandcode <- read_excel(corrections[grep("wetland_abs", corrections)], sheet = 2, na = na_vals, trim_ws = T)
# >>join code for wetland abstracts
wetlandcorrections <- left_join(wetlandcorrections, wetlandcode)
rm(wetlandcode)
# > Nick and Aislyn's methods corrections
methodscorrections <- read.csv(corrections[grep("model", corrections)], na.strings = na_vals)
# > response and driver binning
biodrivecorrections <- read_excel(corrections[grep("driversfreq", corrections)], sheet = "Bio")
anthdrivecorrections <- read_excel(corrections[grep("driversfreq", corrections)], sheet = "Anthro")
envdrivecorrections <- read_excel(corrections[grep("driversfreq", corrections)], sheet = "Environ")
responsecorrections <- read.csv(corrections[grep("response", corrections)], na.strings = na_vals)



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
# effects paper of dams paper is one where  web of science title is incorrect/weird (happened in R1 too). Correct title is the one entered in qualtrics 
## correct the original lookup table so pairs correctly
original$Title[grep("^eEde", original$Title)] <- needs_match$Q1[grep("Effects", needs_match$Q1)]
needs_match$clean_title[grep("Effects", needs_match$Q1)] <- needs_match$Q1[grep("Effects", needs_match$Q1)]

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
  dplyr::select(names(.)[1]:Q1, clean_title, Q2:ncol(.)) %>%
  data.frame()
# screen double entries
doubles <- subset(prelim, clean_title %in% clean_title[doubleentry]) %>%
  arrange(clean_title, Q27, desc(RecordedDate))
# 3 of 4 double entries have more of the Q3 exclusion questions answered in the recent, paper #4 identitical (excluded on first exclusion question)
# verdict: keep most recent entry for same reviewer-double reviews except for TM double entries:
# pull travis' double entries to see if same paper or not (same title, but are answers the same?)
tm_papers <- subset(doubles, Q27 == "TM") %>%
  gather(q, answer, Q2:ncol(.)) %>%
  mutate(qnum = str_extract(q,"^Q[0-9]+"),
         qnum = parse_number(qnum)) %>%
  filter(!is.na(answer)) %>%
  dplyr::select(ResponseId, RecordedDate, clean_title:ncol(.)) %>%
  arrange(qnum)
# these are not quite the same answers.. check which paper is Oceania and which is in S. America (TM missing one paper as of 4/22 so one of these is probably the missing paper, just with a wrong title)
## "Restoration potential of threatened ecosystem engineers increases with aridity: broad scale effects on soil nutrients and function" (Decker et al. 2019)
## > after reviewing, Barros paper corresponds to entry where Q5 == S. America, and Decker et al paper corresponds to entry where Q5 == Oceania (was Australia.. I guess Australia wasn't an option on the survey?)
# verdict: update the title in prelim and update doubleentry value for that paper so it doesn't get excluded
# use the responseID to update title
tm_update <- unique(tm_papers$ResponseId[tm_papers$qnum == 5 & tm_papers$answer == "Oceania"])
prelim$clean_title[prelim$ResponseId == tm_update] <- original$Title[(grepl("^Restoration potential", original$Title) & original$Round2_reviewer1 == "Travis")]
prelim$doubleentry[prelim$ResponseId %in% unique(tm_papers$ResponseId)] <- FALSE # both should be false

prelim <- prelim %>%
  #remove any doubleentries
  filter(!doubleentry) %>%
  # remove doubleentryrow
  select(-doubleentry) %>%
  data.frame()
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
#write.csv(forAK, "round2_metareview/data/intermediate/round2_doublereviewed.csv", row.names = F)

# pull titles for screening further down
doubletitles <- unique(forAK$clean_title)
# how many papers double-reviewed?
length(doubletitles)

# clean up environment
rm(doubles, qualtrix, unwanted, title_df, needs_match, https, tm_papers, tm_update, forAK)



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
summary(unique(original$Title) %in% unique(prelim$clean_title)) #5 still to go (4/22)..
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
# ggsave("round2_metareview/clean_qa_data/review_status/reviewstatus_r2papers.pdf", 
#        height = 6, width = 6, units = "in", scale = 1.5)

# what remains? (# comment out this code bc throws error when all papers done)
# outstanding <- cbind(assess_date = Sys.Date(), subset(original, !Title %in% unique(keydf$Title))) %>%
#                        dplyr::select(Round2_reviewer1, Round2_reviewer2, Round1_reviewer, FirstAuthor, Title, SourcePublication, PublicationYear)
#   # re-assign KG to any remaining Tim and Nick papers since they're done as of 4/15
#   mutate(Round2_reviewer1 = gsub("Nick|Tim", "Kathryn*", Round2_reviewer1)) %>%
#   arrange(Round2_reviewer1, Title)


# who remains to reach 28 papers
effort <- data.frame(assess_date = Sys.Date(), Init = unname(initials), Name = names(initials)) %>%
  left_join(data.frame(table(keydf$Name)), by = c("Name" = "Var1")) %>%
  rename(R2_reviewed = Freq) %>%
  # if count is NA, assign 0
  replace_na(list(R2_reviewed = 0)) %>%
  # add number of rev1 outstanding [no more]
  # left_join(data.frame(table(gsub("[*]", "", outstanding$Round2_reviewer1))), by = c("Name" = "Var1")) %>%
  # replace_na(list(Freq = 0 )) %>%
  # rename(Rev1_remain = Freq) %>%
  # add rev 2
  left_join(distinct(original[,1:2]), by = c("Name" = "Round2_reviewer1")) %>%
  group_by(Name) %>%
  mutate(pair = paste0("pair",1:2)) %>%
  ungroup() %>%
  spread(pair, Round2_reviewer2) %>%
  arrange(desc(R2_reviewed))

# write out both for LD to deal with
#write_csv(outstanding, "round2_metareview/clean_qa_data/review_status/outstanding_r2papers.csv")
#write_csv(effort, "round2_metareview/clean_qa_data/review_status/revieweffort_r2papers.csv")

# make tidy dataset
prelimlong <- prelim %>%
  # select cols to keep (drop Q1 -- old/incorrect title)
  dplyr::select(StartDate, EndDate, RecordedDate, ResponseId, Q27, clean_title:ncol(.)) %>%
  gather(id, answer, Q2:ncol(.)) %>%
  left_join(headerLUT) %>%
  arrange(RecordedDate) %>%
  rename(Init = Q27, Title = clean_title) %>%
  data.frame()

# for prelim results, just look at single/first reviewed
firstreview <- prelimlong %>%
  group_by(Title, abbr, id) %>%
  filter(!duplicated(Title)) %>%
  ungroup()
# write to cleaned data folder for now..
#write_csv(firstreview, "round2_metareview/data/intermediate/round2_prelim_singlereview.csv")
rm(firstreview)


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






# -- FLAG DATA FOR REVIEW -----
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


# clean up work environment
rm(stat_byname, stat_byrev, effort, supporting, regulating, provisioning, cultural)

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
  "Looked at overall benefit", #/value ($) over time given land use change
  "This paper was confusing",
  "it reads like a valuation of ES provision rather than a study of the ecology",
  "I don't think it clearly tests any other ecological questions about ES's",
  "I really don't know if this paper should be included",
  "No measurement of services/function except community structure",
  "(this study is in an econ journal)",
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
  "This one was really tough. The main goal was determining the source of the resilience of the particular ecosystem.",
  "We may want to re-assess this paper to see if we want to keep it",
  "but I don't think it clearly tests any other ecological questions about ES's",
  "The \"function\" was a human-mediated invasion pressure\". So kind of the opposite of a function"
)

# grab unique response ID 
# > note, can't pull by title because some double reviews have conflicting answers for whether to exclude or not
exclude_notes_ids <- prelim$ResponseId[pmatch(exclude_notes, prelim$Q24)]
# did it pull ids for all?
exclude_notes_ids # yes

maybe_exclude_ids <- prelim$ResponseId[pmatch(maybe_exclude_notes, prelim$Q24)]
# did it pull ids for all?
maybe_exclude_ids # no..
needs_match <- maybe_exclude_notes[is.na(maybe_exclude_ids)]
needs_match
# use for loop to append..
for(i in needs_match){
  maybe_exclude_ids <- c(maybe_exclude_ids, prelim$ResponseId[grep(i, prelim$Q24)])
}
# remove NAs
maybe_exclude_ids <- maybe_exclude_ids[!is.na(maybe_exclude_ids)]

# check it got all
# should be TRUE, if not, go back and see what got missed
length(na.exclude(c(exclude_notes, maybe_exclude_notes))) == length(na.exclude(c(exclude_notes_ids, maybe_exclude_ids)))

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
# ggsave("round2_metareview/clean_qa_data/qafigs/r2qa_q3excludepaper.pdf", width = 4, height = 4, units = "in", scale = 1.5)

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
  # add doublerev
  mutate(doublerev = Title %in% doubletitles) %>%
  # add col for already pulled just in case LD has already reviewed
  #mutate(newcase = !Title %in% unique(current_possibleexclude$Title)) %>%
  left_join(distinct(current_possibleexclude[c("Title", "newcase")])) %>% # join so don't overwrite new cases added on 4/17
  replace_na(list(newcase = TRUE)) %>%
  # move new case to after assess_date
  dplyr::select(assess_date, newcase, doublerev, ResponseId:ncol(.))
# change NAs to blanks so not annoying in Excel
possibleexclude_df[is.na(possibleexclude_df)] <- ""
# write out
#write_csv(possibleexclude_df, "round2_metareview/clean_qa_data/needs_classreview/excludenotes_review.csv")



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
  #mutate(newcase = !Title %in% unique(current_ecosystemnotes$Title)) %>%
  left_join(distinct(current_ecosystemnotes[c("Title", "ResponseId", "newcase")])) %>% # join so don't overwrite new cases added on 4/17
  replace_na(list(newcase = TRUE)) %>%
  # reorder cols
  dplyr::select(assess_date, newcase, doublerev, ResponseId:ncol(.))

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
#ggsave("round2_metareview/clean_qa_data/qafigs/r2qa_q4ecosystemnotes.pdf", width = 5, height = 4, units = "in", scale = 1.5)
# write out ecosystem notes for review
## change NA's to blanks so easier for reading in Excel
#q4_qa[is.na(q4_qa)] <- ""
#write_csv(q4_qa, "round2_metareview/clean_qa_data/needs_classreview/ecosystemnotes_review.csv")


# also pull anything that is potentially riparian/wetland
# look at potential wetland/riparian papers
wetlands <- subset(original, grepl(" basin| catchm| fen | riparian| wetland| meadow| watershed", Abstract, ignore.case = T))
# also screen by whether people selected coastal or freshwater for the ecosystem
watertitles <- subset(prelimlong1b, qnum == "Q4" & grepl("Coastal|Freshwater", answer))
# are these papers in q4_qa? or how were they coded?
summary(wetlands$Title %in% q4_qa$Title) #4 are there
wetland_abstracts <- subset(prelimlong1b, Title %in% unique(c(wetlands$Title, watertitles$Title, newscale$Title[!is.na(newscale$aquatic.)]))) %>%
  # add col for double rev
  group_by(Title) %>%
  mutate(doublerev = length(unique(ResponseId))>1) %>%
  ungroup() %>%
  filter(qnum == "Q4" & exclude != "Exclude") %>%
  mutate(assess_date = Sys.Date()) %>%
  dplyr::select(assess_date, doublerev, ResponseId:ncol(.)) %>%
  # drop anything LD excluded
  filter(!Title %in% excludecorrections$Title[excludecorrections$exclude_LD]) %>%
  # append citation info
  # add citation info in case want to look at paper
  left_join(dplyr::select(original, Title, FirstAuthor, PublicationYear, SourcePublication, Abstract)) %>%
  # join julie and grant info
  left_join(newscale[c("Title", "aquatic.")]) %>%
  rename(aquatic_JLGV = 'aquatic.') %>%
  # drop qcols
  dplyr::select(-c(fullquestion, order:survey_order, exclude_notes)) %>%
  filter(!is.na(answer)) %>%
  arrange(Title)


# write out
#write_csv(wetland_abstracts, "round2_metareview/clean_qa_data/needs_classreview/wetland_abstracts.csv", na = "")



# 2b) Methods (Q6)
# read in current methods notes
current_methodsnotes <- read.csv("round2_metareview/clean_qa_data/needs_classreview/methodsnotes_review.csv", na.strings = na_vals)
sort(with(prelimlong1b, answer[abbr == "MethodsNotes" & !is.na(answer)  & exclude != "Exclude"]))
sort(with(prelimlong1b, answer[abbr == "GenInfo" & !is.na(answer) & exclude != "Exclude"]))
# not sure if full GenInfo are preserved? might be byte limitation in csv format..
# > important to address in data cleaning (let class decide)
# GenInfo: [2] "*** THIS STUDY DID NOT TAKE PLACE IN AUSTRALIA. It was in New Zealand (Oceania)"
Datapapers <- with(prelimlong1b, ResponseId[(abbr == "Method" & grepl("Model", answer))])
methodsnotes <- prelimlong1b %>%
  # remove papers to exclude
  subset(exclude != "Exclude") %>%
  group_by(ResponseId) %>%
  filter(ResponseId %in% ResponseId[(abbr == "MethodsNotes" & !is.na(answer)) | 
                                      ((abbr == "GenInfo" & !is.na(answer))) | #]) %>% # |
                                      ((abbr == "Methods" & grepl("Model|Other", answer)))]) %>%
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
  #mutate(newcase = !Title %in% unique(current_methodsnotes$Title)) %>%
  left_join(distinct(current_methodsnotes[c("Title", "ResponseId", "newcase")])) %>% # join so don't overwrite new cases added on 4/17
  replace_na(list(newcase = TRUE)) %>%
  dplyr::select(assess_date, newcase, doublerev:ncol(.)) %>%
  # remove spaces from colnames
  rename_all(function(x) gsub(" ", "_", x)) %>%
  # since want to pull papers add citation info
  left_join(original[c("Title", "FirstAuthor", "SourcePublication", "PublicationYear", "Abstract")])
# make all text so no "NA" in csv
#methodsnotes[is.na(methodsnotes)] <- ""  
# write out for review
#write_csv(methodsnotes, "round2_metareview/clean_qa_data/needs_classreview/methodsnotes_review.csv")

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
  #mutate(newcase = !Title %in% unique(current_scalenotes$Title)) %>%
  left_join(distinct(current_scalenotes[c("Title", "ResponseId", "newcase")])) %>% # join so don't overwrite new cases added on 4/17
  replace_na(list(newcase = TRUE)) %>%
  dplyr::select(assess_date, newcase, doublerev:ncol(.)) %>%
  # also get TimeTrends in there.. should have been
  left_join(distinct(prelimlong1b[prelimlong1b$abbr == "TimeTrends", c("ResponseId", "Title","answer")])) %>%
  # clean up time trends
  rename(TimeTrends = answer) %>%
  mutate(TimeTrends = gsub(" \\(e.g.*$", "", TimeTrends))

# make all text so no "NA" in csv
# scalenotes[is.na(scalenotes)] <- ""  
# write out for review
# write_csv(scalenotes, "round2_metareview/clean_qa_data/needs_classreview/scalenotes_review.csv")



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
  #mutate(newcase = !Title %in% unique(current_kremennotes$Title)) %>%
  left_join(distinct(current_kremennotes[c("Title", "ResponseId", "newcase")])) %>% # join so don't overwrite new cases added on 4/17
  replace_na(list(newcase = TRUE)) %>%
  # join exclude col just in case we only want to review things we definitely will keep
  left_join(distinct(prelimlong1b[c("ResponseId", "Title", "exclude")])) %>%
  dplyr::select(assess_date, newcase, exclude, doublerev:ncol(.))
# unite main and other drivers
# make all text so no "NA" in csv
# kremennotes[is.na(kremennotes)] <- ""  
# write out for review
# write_csv(kremennotes, "round2_metareview/clean_qa_data/needs_classreview/kremennotes_review.csv")


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
# ggsave("round2_metareview/clean_qa_data/qafigs/r2qa_kremenscale.pdf",
#        width = 6.5, height = 6, units = "in", scale = 1.2)


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
# ggsave("round2_metareview/clean_qa_data/qafigs/r2qa_q14kremenESPs.pdf",
#        width = 8, height = 5, units = "in", scale = 1.2)


# pull ESP


# 3) If-then questions -----
# 1) Time


# 2) Multiple scales

# 3) Connectivity


# 4) Kremen topics -----
# 0) pull NO Kremen checked (usually at least 1 should be? probably some cases where they really didn't, and those would be interesting to pull out)
noKremen <- kremennotes %>%
  subset(Title %in% Title[KT_None == 1]) %>%
  dplyr::select(-newcase) %>%
  # add flags for potential Kremen topics
  mutate(flag_KTesp = KT_None == 1 & (grepl("Service", Driver_Bio) | grepl("ESP", ESP_type)),
         flag_KTstructure = KT_None == 1 & grepl("Multiple|Within|Across", ESP_type),
         flag_KTenv = KT_None == 1 & (nchar(Driver_Env) > 0 | nchar(OtherDriver_Env) > 0),
         flag_KTscale = KT_None == 1 & (MultiScale == "No" | TimeTrends != "No")) %>%
  # add citation info
  # join paper citation info
  left_join(original[c("Title", "SourcePublication", "PublicationYear", "Abstract")])
# change any NAs to "
#noKremen[is.na(noKremen)] <- ""
# write out
#write_csv(noKremen, "round2_metareview/clean_qa_data/needs_classreview/KTnone_check.csv")


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
#ESPcheck[is.na(ESPcheck)] <- ""
# write out
#write_csv(ESPcheck, "round2_metareview/clean_qa_data/needs_classreview/KTesp_check.csv")


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
#structurecheck[is.na(structurecheck)] <- ""
# write out for IS
#write_csv(structurecheck, "round2_metareview/clean_qa_data/needs_classreview/KTstructure_check.csv")


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
#envcheck[is.na(envcheck)] <- ""
# write out for IS
#write_csv(envcheck, "round2_metareview/clean_qa_data/needs_classreview/KTenvironment_check.csv")


# 4) If multiple scales checked (time or space), Scale checked
## > Grant and Julie handling this. Will define rules for flag, CTW will incorp in code



# -- PULL UNIQUE DRIVERS AND RESPONSES FOR REVIEW -----
# what's been classed as EF?
# what's been classed as ES?
# I think we also also interested in reviewing which drivers and responses have been used with which ecosystem services?

# ES question
# question 12 (response variables)
q12df <- subset(prelimlong1b, qnum =="Q12" & exclude != "Exclude") %>%
  filter(!is.na(answer)) %>%
  # change ES to not factor
  mutate(ES = as.character(ES))

responses <- subset(q12df, abbr %in% c("Response", "Yclass")) %>%
  dplyr::select(ResponseId:Title, answer, abbr, Group, ES, exclude) %>%
  distinct() %>%
  # enumerate Group to spread response and Yclass
  group_by(ResponseId, Title, ES, abbr) %>%
  mutate(Group = 1:length(answer)) %>%
  ungroup() %>%
  unite(abbr, abbr, Group, sep = "") %>%
  spread(abbr, answer) %>%
  rename(Yclass = Yclass1) %>%
  #combine response 1 and response 2
  unite(Response, Response1, Response2, sep = ", ") %>%
  #remove NAs
  mutate(Response = gsub(", NA", "", Response)) %>%
  #gather response and yclass then comma split
  gather(abbr, answer, Response, Yclass) %>%
  # split commas
  splitcom(keepcols = names(.), splitcol = "answer") %>%
  # spread out cols
  unite(abbr, abbr, num, sep = "") %>%
  spread(abbr, answer) %>%
  gather(ResponseCount, Response, names(.)[grep("^Response[0-9]+", names(.))]) %>%
  mutate(ResponseCount = parse_number(ResponseCount)) %>%
  filter(!is.na(Response)) %>%
  arrange(Title, ES, Init, ResponseCount) %>%
  # add in double review info and assess date
  mutate(doublerev = Title %in% doubletitles,
         assess_date = Sys.Date(),
         # clean up Response
         Response = trimws(Response)) %>%
  #reorder cols
  dplyr::select(assess_date, doublerev, exclude, ResponseId:ES, ResponseCount, Response, Yclass1:ncol(.)) %>%
  # add col to count overall yvar freq
  group_by(casefold(Response)) %>%
  # only want to count single use by reviewer-title (ignore duplications across ES within same paper)
  mutate(Yvar_studyfreq = length(unique(ResponseId))) %>%
  ungroup() %>%
  # drop casefold col
  dplyr::select(-'casefold(Response)') %>%
  # for any Yclass1 with no answer, replace with "no selection"
  replace_na(list(Yclass1 = "no selection"))

# summarize responses by their Yclass type and ES use..
response_summary <- dplyr::select(responses, ResponseId, ES, Response, Yclass1, Yclass2) %>%
  gather(Yclass, answer, Yclass1:Yclass2) %>%
  filter(!is.na(answer)) %>%
  group_by(ES, Response, answer) %>%
  summarise(typefreq = length(Yclass)) %>%
  ungroup() %>%
  rename(EcoServ = ES) %>%
  spread(answer, typefreq, fill = 0) %>%
  arrange(Response, EcoServ)


# change NAs to blanks for easier viewing in Excel
# responses[is.na(responses)] <- ""
#write out
# write_csv(responses, "round2_metareview/clean_qa_data/needs_classreview/ES_allresponses.csv")
# write_csv(response_summary, "round2_metareview/clean_qa_data/needs_classreview/ES_responseclass_review.csv")


# compile drivers 
# other drivers will be tricky because other not always checked (or notes added in "other drivers" field)
drivers <- subset(q12df, abbr %in% c("Driver", "OtherDriver")) %>%
  dplyr::select(ResponseId:Title, exclude, Group, ES, abbr, answer) %>%
  group_by(Group, ResponseId) %>%
  # lookup ES where "Other" checked 
  mutate(OtherES = ifelse(abbr == "OtherDriver", str_flatten(ES[grepl("Other", answer) & abbr == "Driver"], collapse = ", "), NA),
         # if "Other" not checked, pull whatever ES's are filled out for that Driver Group
         ## > this will pull ES's for honest Other Drivers where "Other" not checked (accident) but also where reviewer used "Other" as notes and not truly just for Other Drivers
         SingleES = ifelse(nchar(OtherES) == 0 & abbr == "OtherDriver", str_flatten(unique(ES[abbr == "Driver"]), collapse = ", "), OtherES)) %>%
  ## > if other not checked and no other drivers entered, there will be no ES pulled
  ## > this could be from honest error (forgot to check "Other"), but also unwanted human error (e.g. entered other driver in wrong box, or used other field for notes)  
  ungroup() %>%
  mutate(Other_checked = OtherES == SingleES)

# all we care about for now (4/19) is reviewing drivers by their ES group.. can deal with mistakes later.. or someone else can deal with it
# pull out other drivers, then add on to standard drivers
stddrivers <- subset(drivers, abbr == "Driver") %>%
  dplyr::select(ResponseId:answer) %>%
  mutate(Other_checked = grepl("Other", answer),
         # remove , from exploitation answer
         answer = gsub("hunting, fishing", "hunting or fishing", answer)) %>%
  splitcom(names(.), splitcol = "answer") %>%
  # drop enumeration
  dplyr::select(-num)


# prep other drivers to join with std drivers in wide format
# separate problem records from the rest
otherdrivers_errors <- subset(drivers, abbr == "OtherDriver" & nchar(SingleES) == 0)
otherdrivers <- subset(drivers, abbr == "OtherDriver") %>%
  anti_join(otherdrivers_errors) %>%
  mutate(ES = SingleES) %>%
  rename(OtherDriver = answer) %>%
  dplyr::select(-c(OtherES, SingleES)) %>%
  # first split ES's
  splitcom(keepcols = names(.), splitcol = "ES") %>%
  rename(ES = answer) %>%
  # drop ES number count col
  dplyr::select(-num) %>%
  #split Other drivers
  # spot correct a few answers so doesn't split by splitcom
  mutate(OtherDriver = gsub("\\[city 1, city 2\\]", "\\[city 1; city 2\\]", OtherDriver),
         OtherDriver = gsub("system function, not ESP", "system function not ESP", OtherDriver),
         OtherDriver = gsub("current and previous years min,max,mean", "current and previous years min max mean", OtherDriver)) %>%
  splitcom(names(.), splitcol = "OtherDriver") %>%
  mutate(answer = trimws(answer)) %>%
  # drop count col
  dplyr::select(-num)

# clean up other driver errors
otherdrivers_errors <- mutate(otherdrivers_errors, Other_checked = FALSE) %>%
  # drop ES col
  dplyr::select(-ES) %>%
  # join ES info from response vars
  left_join(distinct(dplyr::select(responses, ResponseId, Init, Title, ES))) %>%
  # for any ESs still blank, fill "none selected" (applies to Sierras's 82-var model paper)
  replace_na(list(ES = "no ES selected")) %>%
  #mutate(ES = ifelse(is.na(ES), "no ES selected", ES))
  # I think Aislyn's trout entry should be Bio not Env (she has Service Provider entered as Bio driver) .. think this entry is probably more akin to notes on that
  mutate(Group = ifelse(grepl("various trout", answer), "Bio", Group))

# rbind other drivers and add back to std drivers
alldrivers <- rbind(otherdrivers, otherdrivers_errors[names(otherdrivers)]) %>%
  rbind(stddrivers[names(.)]) %>%
  mutate(assess_date = Sys.Date(), doublerev = Title %in% doubletitles) %>% 
  # reorder cols
  dplyr::select(assess_date, doublerev, exclude, ResponseId:ncol(.)) %>%
  group_by(ResponseId, Group, ES) %>%
  mutate(OtherEntry = (answer == "Other" & length(answer[abbr == "OtherDriver"]) > 0)) %>%
  ungroup() %>%
  mutate(OtherEntry = ifelse((answer != "Other" & abbr == "Driver") | abbr == "OtherDriver", NA, OtherEntry)) %>%
  # not sure the best way to arrange this so most helpful to reviewers..
  arrange(Title, ES,  Init, Group, abbr)


# summarize drivers for review, similar to response summary
# just ES, driver, and frequency
alldrivers_summary <- group_by(alldrivers, ES, Group, casefold(answer)) %>%
  mutate(count = length(unique(ResponseId))) %>%
  ungroup() %>%
  dplyr::select(ES, Group, answer, count) %>%
  distinct() %>%
  arrange(answer, ES, Group, desc(count))

# write out both
## change NAs to blanks so reads easier in Excel
# alldrivers[is.na(alldrivers)] <- "" 
# write_csv(alldrivers, "round2_metareview/clean_qa_data/needs_classreview/ES_alldrivers.csv")
# write_csv(alldrivers_summary, "round2_metareview/clean_qa_data/needs_classreview/ES_driversfreq_review.csv")


# check for missing drivers and responses [5/28: CTW found when applying corrections some papers missing that info]
# how many Titles don't have any Response or Driver entered??
noResponseDriver <- subset(prelimlong1b, qnum =="Q12" & exclude != "Exclude") %>%
  mutate(ES = as.character(ES),
         # add doublerev
         doublerev = Title %in% doubletitles) %>%
  # remove anything in LD's exclusion csv
  filter(!Title %in% excludecorrections$Title[excludecorrections$exclude_LD]) %>%
  # also catch ones that are misspelled in the exclusion csv
  filter(!grepl("^Flattening of Caribbean", Title)) %>%
  group_by(ResponseId) %>%
  mutate(flag_noResponse = all(is.na(answer[abbr == "Response"])),
         flag_noDriver = all(is.na(answer[grepl("Driver", abbr)])),
         flag_noDirection = all(is.na(answer[abbr == "EffectDirect"]))) %>%
  ungroup() %>%
  filter(ResponseId %in% c(unique(ResponseId[flag_noResponse | flag_noDriver]))) %>%
  # check if, is paper is double reviewed, are both answers empty?
  group_by(Title) %>%
  # sum unique ResponseId
  mutate(count_RID = length(unique(ResponseId))) %>%
  ungroup() %>%
  # exclude if 2nd review has answers
  filter(!(doublerev & count_RID == 1)) %>%
  # subset to ES's with answers only
  group_by(ResponseId) %>%
  mutate(ES_wanswers = str_flatten(unique(ES[!is.na(answer)])),
         removeES = NA) %>%
  ungroup() %>%
  arrange(Title, survey_order) %>%
  data.frame()

for(i in 1:nrow(noResponseDriver)){
  noResponseDriver$removeES[i] <- grepl(noResponseDriver$ES[i], noResponseDriver$ES_wanswers[i])
}
# clean up
noResponseDriver <- filter(noResponseDriver, is.na(removeES) | removeES == 1 | is.na(ES_wanswers)) %>%
  data.frame() %>%
  # remove Response 2 option from df if is NA (assume ppl wrote Response in Response 1 question)
  filter(!grepl("Response 2", fullquestion)) %>%
  # drop unneeded cols
  dplyr::select(-c(removeES, count_RID, ES_wanswers, exclude, exclude_notes, doublerev, order)) %>%
  # add clean answer col for ppl to fill out
  mutate(clean_answer = NA) %>%
  # add citation info in case people need
  left_join(original[c("Title", "FirstAuthor", "PublicationYear", "SourcePublication")]) %>%
  dplyr::select(StartDate:answer, clean_answer, fullquestion:ncol(.))
# who and what?
sort(unique(noResponseDriver$Init))
sort(unique(noResponseDriver$Title))
#quote all so not annoying to look at in Excel
#noResponseDriver2[is.na(noResponseDriver2)] <- ""
# write out by person
for(i in sort(unique(noResponseDriver$Init))){
  tempdat <- subset(noResponseDriver, Init == i)
  write_csv(tempdat, paste0("round2_metareview/clean_qa_data/needs_classreview/missing_responsedriver/q12_review", i, ".csv"), na = "")
}


# check how much was entered for each (e.g. directions, response var type)
View(subset(q12df_clean, ResponseId %in% c(unique(noDriver$ResponseId), unique(noResponse$ResponseId))))


# 6/6: check for "Other" checked in "Driver" but no "OtherDriver" entered
othercheck <- subset(prelimlong1b, (abbr == "Driver" & !is.na(answer)) | abbr ==  "OtherDriver") %>%
  # remove anything in LD's exclusion csv
  filter(!Title %in% excludecorrections$Title[excludecorrections$exclude_LD]) %>%
  # also catch ones that are misspelled in the exclusion csv
  filter(!grepl("^Flattening of Caribbean", Title)) %>%
  # add doublerev
  mutate(doublerev = Title %in% doubletitles) %>%
  filter(grepl("Other", answer) | abbr == "OtherDriver") %>%
  group_by(ResponseId) %>%
  # filter for Driver answers, only those that have Other checked
  mutate(hasOther = grepl("Other", answer)) %>%
  ungroup() %>%
  filter(ResponseId %in% ResponseId[hasOther]) %>%
  filter(hasOther | abbr == "OtherDriver") %>%
  dplyr::select(ResponseId, Init, doublerev, Title, answer, abbr, Group) %>%
  distinct() %>%
  group_by(ResponseId, Group) %>%
  mutate(newDriver = str_flatten(unique(answer[abbr == "Driver"]))) %>%
  ungroup() %>%
  mutate(answer = ifelse(abbr == "Driver", newDriver, answer)) %>%
  # drop newDriver
  dplyr::select(-newDriver) %>%
  distinct() %>%
  spread(abbr, answer, fill = NA) %>%
  filter(grepl("Other", Driver)) %>%
  # drop anything that has other supplied to isolate errors
  filter(is.na(OtherDriver)) %>%
  arrange(Init, Title) %>%
  #join citation info
  left_join(original[c("Title","FirstAuthor", "PublicationYear", "SourcePublication")])
nrow(othercheck) # 15 titles.. dang

# write out to needs review
for(i in unique(othercheck$Init)){
  tempdat <- subset(othercheck, Init == i)
  write_csv(tempdat, paste0("round2_metareview/clean_qa_data/needs_classreview/missing_responsedriver/q12_otherdriver_review", i, ".csv"), na = "")
}




# -- APPLY CORRECTIONS -----
# clean up work environment
rm(current_ecosystemnotes, current_kremennotes, current_methodsnotes, current_possibleexclude, current_scalenotes, 
   exclude_notes, exclude_notes_ids, maybe_exclude_ids, maybe_exclude_notes, possibleexclude_df, watertitles, wetlands,
   noKremen, kremennotes, envcheck, scalenotes, methodsnotes, ESPcheck, needs_match, rawdat, Datapapers, records)

# start with exclusions, because if paper excluded then other corrections are moot
## notes from Laura on exclusions (email from LD to CTW 5/13/20):
# "I've also added the papers that Nick has ruled out based on the review of the methods questions at the end."
# "Thanks, Nick, for providing a second opinion on some of these!"
# "So, exclude based on the column exclude_LD == TRUE. Could we also track the papers that are Q1 in the column Reason_LD?"
# "I think we will want to add these to the ones that were excluded based on the Qualtrics survey and the round 1 abstract review to make a figure or report the number to % of studies that stopped at BD."
# > CTW thought: maybe best to write out csv of all excluded papers, noted if excluded in R1 or R2, and reason why.. then can make figures or summary tables from that DF

# notes from Nick on his excluded papers: 
# Hyvonen 2008- had listed “other” as the only choice on the type of methods, and when I looked it over it was too much of a review.
# Alvarez-Filipe 2009- The second was a meta analysis that should have been filtered out in the first round
# Elbakidze 2017- The last one was pretty much a methods paper.
## > CTW: so all should be excluded based on "review/metaanalysis/methods", which is equivalent to LD's Q3

# after address exclusions, then apply IS corrections
# then others in order of survey question number...


# 1. Correct exclusions (LD + ND) -----
# first, assign ResponseID and reason_LD to Nick's papers in correction set (one of Nick's titles doesn't match [case letter issue])

for(i in which(is.na(excludecorrections$ResponseId))){
  # infill responseID
  excludecorrections$ResponseId[i] <- unique(prelimlong1b$ResponseId[casefold(prelimlong1b$Title) %in% with(excludecorrections, casefold(Title[i]))])
  # correct title
  excludecorrections$Title[i] <- unique(prelimlong1b$Title[casefold(prelimlong1b$Title) %in% with(excludecorrections, casefold(Title[i]))])
  # infill reason
  excludecorrections$Reason_LD[i] <- "Q3 - Nick et al. review" 
}
# Grant and Anna's ResponseIds not recognized for some reason, clean up
excludecorrections$ResponseId[grepl("^Adapting the adaptive", excludecorrections$Title)] <- unique(prelimlong1b$ResponseId[grepl("^Adapting the adaptive", prelimlong1b$Title)])
excludecorrections$ResponseId[grepl("^Habitat connectivity for pollinator", excludecorrections$Title)] <- unique(prelimlong1b$ResponseId[grepl("^Habitat connectivity for pollinator", prelimlong1b$Title)])
# clean up "TRUE" in reason_LD, it's not a reason..
excludecorrections$Reason_LD[excludecorrections$Reason_LD == "TRUE"] <- NA
# add missing reason to Check dam paper (Nick's review)
excludecorrections$Reason_LD[grep("Check dam", excludecorrections$Title)] <- "No EF/ES, should be excluded in R1"
# fill down reasons if exclude_LD == TRUE
excludecorrections <- excludecorrections %>%
  arrange(Reason_LD) %>%
  group_by(Title) %>%
  fill(Reason_LD) %>%
  ungroup() %>%
  arrange(Title)


# pull out any to exclude based on LD and ND review by their responseID and Title
excludeLD <- unique(with(excludecorrections, ResponseId[exclude_LD]))
keepLD <- excludecorrections$ResponseId[!excludecorrections$exclude_LD]
# also grab straightforward excluded papers (didn't need LD's review), combine with 
exclude_qualtrics <- unique(prelimlong1b$ResponseId[prelimlong1b$exclude == "Exclude" & !prelimlong1b$Title %in% excludecorrections$Title])
# pull data for titles to exclude
exclude_r2 <- subset(prelimlong1b, ResponseId %in% exclude_qualtrics) %>% #, excludeLD
  #keep only qnum = q3
  filter(qnum == "Q3" & !is.na(answer)) %>%
  # add reason for exclusion
  mutate(reviewer_exclusion = ifelse(exclude == "Exclude" & answer == "Yes", abbr, NA),
         # add exclude LD and reason LD for binding with LD's reviewed papers
         review_LD = FALSE, exclude_LD = NA, Reason_LD = NA)
length(unique(exclude_r2$Title))

exclude_r2LD <- subset(prelimlong1b, ResponseId %in% excludeLD) %>%
  filter(qnum == "Q3") %>%
  # join LD's reason for exclusion
  left_join(distinct(excludecorrections[c("Title","exclude_LD", "Reason_LD")])) %>%
  # capture which question entered yes by reviewer for comparison
  mutate(reviewer_exclusion = ifelse(exclude == "Exclude" & answer == "Yes", abbr, NA),
         review_LD = TRUE)
length(unique(exclude_r2LD$Title))

# bind all
r2excluded_final <- rbind(exclude_r2, exclude_r2LD[names(exclude_r2)]) %>%
  # select title, reason, and how/who excluded
  dplyr::select(Title, reviewer_exclusion:ncol(.)) %>%
  mutate(exclusion_reason = ifelse(!is.na(reviewer_exclusion), reviewer_exclusion, Reason_LD),
         # recode LD's reasons to match abbreviated reasons
         exclusion_reason = ifelse(grepl("^Q3|framework", exclusion_reason, ignore.case = T), "ReviewOnly",
                                   ifelse(grepl("^Q2|value|social|perception", exclusion_reason, ignore.case = T), "SocialOnly",
                                          ifelse(grepl("Q1", exclusion_reason), "BiodivOnly", exclusion_reason)))) %>%
  filter(!is.na(exclusion_reason) & !exclusion_reason == "by Q3") %>%
  mutate_all(trimws) %>%
  # add in col to indicate whether double reviewed or not
  mutate(doublerev = Title %in% doubletitles) %>%
  dplyr::select(Title, doublerev, review_LD:ncol(.)) %>%
  distinct() %>%
  arrange(Title) %>%
  # flag anything that doesn't have agreement in exclusion reasons
  group_by(Title) %>%
  mutate(count_reasons = length(exclusion_reason)) %>%
  ungroup()

# write out excluded papers
write_csv(r2excluded_final, "round2_metareview/data/intermediate/round2_excluded.csv") #, na = ""


# retain kept papers in prelimlong_1c
prelimlong1c <- filter(prelimlong1b, !Title %in% unique(r2excluded_final$Title)) %>%
  # create clean answer column for storing cleaned answers and qa note
  mutate(clean_answer = trimws(answer),
         qa_note = NA,
         # add col to indicate double review
         doublerev = Title %in% doubletitles) %>%
  dplyr::select(StartDate:Init, doublerev, Title:answer, clean_answer, fullquestion:ncol(.)) %>%
  # change any "Yes"s in Q3 to No for clean answer since allowed by LD
  mutate(qa_note = ifelse(answer == "Yes" & qnum == "Q3", "Keep paper, reviewed by LD", qa_note),
         clean_answer = ifelse(answer == "Yes" & qnum == "Q3", "No", clean_answer)) %>%
  # drop excluded cols
  dplyr::select(-c(exclude, exclude_notes))
# check LD's kept papers in here
summary(keepLD %in% prelimlong1c$ResponseId) #yes
# clean up environment
rm(exclude_r2LD, excludeLD, exclude_r2, keepLD, exclude_qualtrics, excludecorrections)


# 2. IS corrections (to her answers) ----
# only retain what needs to be corrected
IScorrections <- subset(IScorrections, !is.na(EmailNoteToCaitlin)) %>%
  dplyr::select(Title, QualtricsNotes:ncol(.))
# list notes
IScorrections$EmailNoteToCaitlin
# [1] "didn't have a Global Freshwater option but that would be the most suitable as its more of a conceptual modeling diatom science to ES paper"
## > CTW: Nothing to do about this. Freshwater selected, and that's the best fit
# [2] "q6 should have been observational"
## > CTW will change
# [3] "should have selected proxy for EF"
## > CTW will change

# [2] find Q6 answer, update, and add qa note
prelimlong1c$clean_answer[prelimlong1c$abbr == "Methods" & prelimlong1c$Title == IScorrections$Title[2]] <- "Observational (Includes data observed in situ OR via remote sensing, if used directly)"
prelimlong1c$qa_note[prelimlong1c$abbr == "Methods" & prelimlong1c$Title == IScorrections$Title[2]] <- "Reviewer correction"

# [3] update proxy answer
prelimlong1c$answer[prelimlong1c$abbr == "Yclass" & !is.na(prelimlong1c$answer) & prelimlong1c$Title == IScorrections$Title[3]]
unique(prelimlong1c$answer[prelimlong1c$abbr == "Yclass"]) # there is no "Proxy for EF".. emailed IS



# 3. Ecosystem correction (IS + LD) -----
# change new Ag class in LD and IS ecosystem correction so doesn't create problem with splitcom
unique(systemcorrections$AssignToEcosystem)
systemcorrections$AssignToEcosystem <- gsub("Agricultural, Agroforestry, and Rural", "Agricultural/Agroforestry/Rural", systemcorrections$AssignToEcosystem)

# use q4_qa data frame.. and a for loop
View(q4_qa)
View(systemcorrections)
ecosystemRIDs <- unique(prelimlong1c$ResponseId[prelimlong1c$Title %in% q4_qa$Title])
for(i in ecosystemRIDs){
  
  # find relevant rows in dataset
  temprow_Q4 <- which(prelimlong1c$ResponseId == i & prelimlong1c$abbr == "Ecosystem")
  temprow_Q4notes <- which(prelimlong1c$ResponseId == i & prelimlong1c$abbr == "EcosystemNotes")
  temprow_Q4geninfo <- which(prelimlong1c$ResponseId == i & prelimlong1c$abbr == "GenInfo")
  
  # ID relevant records for title, and ID ResponseID that has the relevant "other" comments
  temp <- subset(prelimlong1c, qnum == "Q4" & ResponseId == i)
  # add in check for "Other", if not then next (e.g. double reviews, one person didn't check other)
  if(is.na(temp$answer[temp$abbr == "EcosystemNotes"])){
    next
  }
  replacetemp <- subset(systemcorrections, grepl(paste0("^", str_remove_all(temp$answer[temp$abbr == "EcosystemNotes"], "[:punct:]| ")), str_remove_all(OriginalNoteCommaSep, "[:punct:]| "), ignore.case = T)) %>%
    mutate(OriginalNoteCommaSep = casefold(OriginalNoteCommaSep)) %>%
    distinct()
  stopifnot(nrow(replacetemp) == 1)
  
  # replace answer to Q4 for LD + IS new classification
  prelimlong1c$clean_answer[temprow_Q4] <- replacetemp$AssignToEcosystem
  # paste original other comment to GenInfo text field
  temp_Q4geninfo <- prelimlong1c$answer[prelimlong1c$abbr == "GenInfo" & prelimlong1c$ResponseId == i]
  if(!is.na(temp_Q4geninfo)){
    prelimlong1c$clean_answer[temprow_Q4geninfo]  <- paste0(temp_Q4geninfo, "; system notes: ", temp$answer[temp$abbr == "EcosystemNotes"])
  }else{
    prelimlong1c$clean_answer[temprow_Q4geninfo] <- paste("System notes:", temp$answer[temp$abbr == "EcosystemNotes"], sep = " ")
  }
  # NA other text field
  prelimlong1c$clean_answer[temprow_Q4notes] <- NA
  
  # add QA notes on corrections
  prelimlong1c$qa_note[temprow_Q4] <- "Reclass 'other' system, LD+IS review"
  prelimlong1c$qa_note[temprow_Q4notes] <- "Reclass 'other' system, LD+IS review"
  prelimlong1c$qa_note[temprow_Q4geninfo] <- "Append 'other' system notes to geninfo notes"
}

# screen for any other "Others" that got missed
# > if returns 0 rows, proceed! if not, triage
subset(prelimlong1c, qnum == "Q4" & grepl("Other", answer, ignore.case = T) & grepl("Other", clean_answer, ignore.case = T))

# append "Terrestrial" to any Ag entry, and gsub Ag label with new category IS and LD created
## extract response IDs with Ag
agRIDs <- unique(with(prelimlong1c, ResponseId[qnum == "Q4" & grepl("Agri", clean_answer, ignore.case = T)]))
View(subset(prelimlong1c, qnum == "Q4" & grepl("Ag", clean_answer)))
View(subset(prelimlong1c, ResponseId %in% agRIDs & qnum == "Q4"))
for(i in agRIDs){
  # ID row
  temprowAG <- which(prelimlong1c$ResponseId == i & prelimlong1c$abbr == "Ecosystem")
  # gsub Agri label
  prelimlong1c$clean_answer[temprowAG] <- gsub("Agricultural/Rural", "Agricultural/Agroforestry/Rural", prelimlong1c$clean_answer[temprowAG])
  # add Terrestrial if not there
  if(!grepl("Terrestrial", prelimlong1c$clean_answer[temprowAG])){
    prelimlong1c$clean_answer[temprowAG] <- paste0("Terrestrial,", prelimlong1c$clean_answer[temprowAG])
  }
  # add QA note
  if(is.na(prelimlong1c$qa_note[temprowAG])){
    prelimlong1c$qa_note[temprowAG] <- "Standardize ag/rural system label, LD+IS review"
  }
}


# apply wetland corrections from IS
# > clean up df a little bit so easier to look at
wetlandcorrections <- dplyr::select(wetlandcorrections, ResponseId:abbr, aquatic_JLGV:designation)
# > UseEcoAnswer_IS: 0 means ignore, 1 means keep as is, 2 mean change
# > will need to clean up IS's reclass codes.. and can add her comments to qa_note (for both 1 and 2 codes)

# look at new bins
sort(unique(wetlandcorrections$EcoAnswerReview_IS))
# Ag/Rural needs new code created by LD and IS
# "Coastal/Marine/Off-shore" needs correction
wetlandclass <- str_flatten(sort(unique(wetlandcorrections$EcoAnswerReview_IS)), collapse = ",") %>%
  strsplit(., ",") %>%
  data.frame() %>%
  rename_all(function(x) x <- "code") %>%
  distinct() %>%
  arrange(code)
View(wetlandclass) # maybe easiest just to gsub in Isabel's df..


wetlandcorrections <- wetlandcorrections %>%
  mutate(EcoAnswerReview_IS = gsub(" ?, ?", ",", EcoAnswerReview_IS), # trimws before or after comma
         # clean up coastal/marine/off-shore
         EcoAnswerReview_IS = gsub("Coastal/Marine", "Coastal,Marine", EcoAnswerReview_IS, fixed = T),
         # assign new ag code
         EcoAnswerReview_IS = gsub("Agricultural/Rural", "Agricultural/Agroforestry/Rural", EcoAnswerReview_IS, fixed = T),
         # aquatic to freshwater,
         EcoAnswerReview_IS = gsub("Aquatic", "Freshwater", EcoAnswerReview_IS, fixed = T),
         # standardize wetland/riparian
         EcoAnswerReview_IS = gsub("Riparian/Wetland", "Wetland/Riparian", EcoAnswerReview_IS, fixed = T)) %>%
  # keep only titles that need to be changed
  subset(UseEcoAnswer_IS == 2) %>%
  # add col for clean_answer
  mutate(clean_answer = NA)
# now need to go through and order categories how ordered in survey.. so not creating combo-categories that are actually the same but appear distinct because of IS ordering
# order on the survey goes:
# > Terr, Urb, Ag, Mar, Coast, Fresh, Other (which are now all addressed), then can add IS's wetland/riparian
ecosystem_order <- data.frame(system = c("Terrestrial", "Urban", "Agricultural/Agroforestry/Rural", "Marine/Off-shore", "Coastal", "Freshwater", "Wetland/Riparian", "Other")) %>%
  mutate(orderseq = seq(1, nrow(.)))
for(i in 1:nrow(wetlandcorrections)){
  # pull current answer
  tempanswer <- data.frame(strsplit(wetlandcorrections$EcoAnswerReview_IS[i], split = ",")) %>%
    rename("system" = names(.)) %>%
    left_join(ecosystem_order, by = "system") %>%
    arrange(orderseq)
  # check that all systems have a number for ordering
  stopifnot(all(!is.na(tempanswer$orderseq)))
  # reorder based on number sequence
  wetlandcorrections$clean_answer[i] <- str_flatten(tempanswer$system, collapse = ",")
}
# adjust: Tomscha paper should have terrestrial appended (has ag and assessed land cover in floodplains), but nortey paper no -- was done in mangroves
wetlandcorrections$clean_answer[grepl("The spatial organization of ecosystem services", wetlandcorrections$Title)] <-paste0("Terrestrial,",wetlandcorrections$clean_answer[grepl("The spatial organization of ecosystem services", wetlandcorrections$Title)])
# remove "looked at paper" and clean up
wetlandcorrections$EcoAnswerReview_IS_notes <- gsub("looked at paper,?;? ?", "", wetlandcorrections$EcoAnswerReview_IS_notes)
wetlandcorrections$EcoAnswerReview_IS_notes[wetlandcorrections$EcoAnswerReview_IS_notes == ""] <- NA
# append note to Tomscha paper (is NA currently)
wetlandcorrections$EcoAnswerReview_IS_notes[grepl("The spatial organization of ecosystem services", wetlandcorrections$Title)] <- "added 'Terrestrial' as well--paper assessed land cover in floodplains."
# append note to mangrove paper (is NA currently)
wetlandcorrections$EcoAnswerReview_IS_notes[grepl("Comparative Assessment of Mangrove Biomass", wetlandcorrections$Title)] <- "although ag, is not terrestrial. Assessed mangroves."

# now go through working master dataset by title, correct system, and add qa_note
for(i in unique(wetlandcorrections$Title)){
  tempdat <- subset(prelimlong1c, qnum == "Q4" & Title == i)
  #id rows
  # > system
  temprows <- with(prelimlong1c, which(abbr == "Ecosystem" & Title == i))
  # > other (there shouldn't be anything but to be sure)
  temprows_other <- with(prelimlong1c, which(abbr == "EcosystemNotes" & Title == i))
  # assign wetland ecosystem
  prelimlong1c$clean_answer[temprows] <- wetlandcorrections$clean_answer[wetlandcorrections$Title == i]
  # NA other field
  prelimlong1c$clean_answer[temprows_other] <- NA
  # add qa note (and IS note if there)
  if(!is.na(wetlandcorrections$EcoAnswerReview_IS_notes[wetlandcorrections$Title == i])){
    prelimlong1c$qa_note[temprows] <- paste("Ecosystem reviewed and assigned by IS;", wetlandcorrections$EcoAnswerReview_IS_notes[wetlandcorrections$Title == i])
  }else{
    prelimlong1c$qa_note[temprows] <- "Ecosystem reviewed and assigned by IS" 
  }                    
}

# need to undo reclass systems for double-reviewed papers where reviewers excluded paper
excludeddbl <- unique(with(prelimlong1c, ResponseId[qnum == "Q3" & answer == "Yes" & !is.na(answer)]))
prelimlong1c$clean_answer[prelimlong1c$ResponseId %in% excludeddbl & prelimlong1c$abbr == "Ecosystem"] <- NA
prelimlong1c$qa_note[prelimlong1c$ResponseId %in% excludeddbl & prelimlong1c$abbr == "Ecosystem"] <- NA


# clean up environment
rm(replacetemp, temp, agRIDs, ecosystemRIDs, i, temprowAG, wetlandclass, wetlandcorrections,
   temp_Q4geninfo, temprow_Q4geninfo, temprow_Q4, temprow_Q4notes, ecosystem_order,
   IScorrections, systemcorrections, wetland_abstracts, temprows, temprows_other)


# save work
copydf <- prelimlong1c



# 4. Methods corrections (AK + ND) -----
# rule (from AK):
# The column I added is "Changed?" 1 if the observational column was changed and NA if not.
# The rule was: if a paper selected model/data simulation but used existing data (e.g. fisheries records, LTER) then it should not select observational too. 
# Observational should only be checked if they collected data for that study.

# all that's needed is ResponseIds where Changed == 1 
unique(methodscorrections$Changed)
methodsRIDs <- methodscorrections$ResponseId[!is.na(methodscorrections$Changed)] # anything not NA is 1
for(i in methodsRIDs){
  # ID row
  temprow <- which(prelimlong1c$ResponseId == i & prelimlong1c$abbr == "Methods")
  # sub out Observational..
  prelimlong1c$clean_answer[temprow] <- gsub(",Observatio.*directly[)]", "", prelimlong1c$answer[temprow])
  # add QA note
  prelimlong1c$qa_note[temprow] <- "Removed 'observational' method. Used published data in model/data sim, did not collect, ND+AK review"
}

# also append note to others reviewed by ND and AK to confirm response valid
allowmethods <- methodscorrections$ResponseId[is.na(methodscorrections$Changed)]
for(i in allowmethods){
  # ID row
  temprow <- which(prelimlong1c$ResponseId == i & prelimlong1c$abbr == "Methods")
  # add QA note
  prelimlong1c$qa_note[temprow] <- "Experiment, observation, and/or modeling methods confirmed by ND+AK review"
  
}
View(subset(prelimlong1c, ResponseId %in% c(methodsRIDs, allowmethods) & abbr == "Methods"))
# remove "Other" from Data Simulation records (Other value is "used open data from published sources" which is implicit)
# what are the records with Other in methods?
othermethodsRIDs <- with(prelimlong1c, ResponseId[grepl("Other", clean_answer) & abbr == "Methods"])
with(prelimlong1c, clean_answer[ResponseId %in% othermethodsRIDs & abbr == "MethodsNotes"])
# >> most of these are observational (which includes geospatial data)
# >> models are models, and quasi-experimental can be Experimental
# >> put in df to update masters dataset
othermethodsdf <- data.frame(othermethod = with(prelimlong1c, clean_answer[ResponseId %in% othermethodsRIDs & abbr == "MethodsNotes"])) %>%
  mutate(methodclass = ifelse(grepl("model", othermethod), "Model/Data Simulation",
                              ifelse(grepl("experiment", othermethod), "Experimental",
                                     "Observational (Includes data observed in situ OR via remote sensing, if used directly)")))
for(i in othermethodsRIDs){
  # ID row
  temprow_methods <- which(prelimlong1c$ResponseId == i & prelimlong1c$abbr == "Methods")
  temprow_methodsnotes <- which(prelimlong1c$ResponseId == i & prelimlong1c$abbr == "MethodsNotes")
  temprow_geninfo <- which(prelimlong1c$ResponseId == i & prelimlong1c$abbr == "GenInfo")
  # grab methods to apply for data noted in "other"
  tempmethod <- with(othermethodsdf, methodclass[othermethod == prelimlong1c$clean_answer[temprow_methodsnotes]])
  
  # sub out Other
  prelimlong1c$clean_answer[temprow_methods] <- gsub(",Other", "", prelimlong1c$clean_answer[temprow_methods])
  # if method mentioned in other note not in main answer, append
  if(!grepl(tempmethod, prelimlong1c$clean_answer[temprow_methods])){
    prelimlong1c$clean_answer[temprow_methods] <- paste(prelimlong1c$clean_answer[temprow_methods], tempmethod, sep = ",")
  }
  # append original methods notes to GenInfo -- based on whether NA or not
  if(!is.na(prelimlong1c$answer[temprow_geninfo])){
    prelimlong1c$clean_answer[temprow_geninfo] <- paste0(prelimlong1c$clean_answer[temprow_geninfo], "; methods notes: ", prelimlong1c$answer[temprow_methodsnotes])
  }else{
    prelimlong1c$clean_answer[temprow_geninfo] <- paste("Methods notes:", prelimlong1c$answer[temprow_methodsnotes], sep = " ")
  }
  # NA methods notes and remove other
  prelimlong1c$clean_answer[temprow_methodsnotes] <- NA
  # add QA note
  prelimlong1c$qa_note[c(temprow_methods, temprow_methodsnotes)] <- "Reclass 'other' method"
  # add QA note to gen info, based on whether NA or not
  if(is.na(prelimlong1c$qa_note[temprow_geninfo])){
    prelimlong1c$qa_note[temprow_geninfo] <- "Append 'other' methods notes to geninfo notes"
  }else{
    prelimlong1c$qa_note[temprow_geninfo] <- paste0(prelimlong1c$qa_note[temprow_geninfo], "; append 'other' methods notes to geninfo notes")
  }
}

# clean up environment
rm(temprow, temprow_geninfo, temprow_methods, temprow_methodsnotes, othermethodsdf,
   methodsRIDs, othermethodsRIDs, methodscorrections)

# make copy to save results in progress
copydf <- prelimlong1c



# 5. Driver/Response corrections (KG + SDJ) ----
# 5/22/20: write basic code to read in and correct responses and drivers, can update when KG + SDJ finished

# see if can collapse down response and driver categories based on work already done
# i.e. if remove ES's, is everything labeled?

# need to first add in missing driver/response corrections from ppl here before apply bins

# Julie
# > in email from JL to CTW, 6/12/20
# So for my paper, I entered 'other' under environmental driver, and I believe this is a mistake.
# The only non-biotic driver in this paper was a compost treatment, which I believe should fall under the 'human driver' category, especially since the groups' later discussions about environment vs management drivers.
# I checked out the full data for this paper, however, and it looks like I did NOT enter it as an anthropogenic driver, but rather was going to or mistakenly entered it as an 'other' environmental driver. 
# So I think the best thing would be to both remove the reference to an 'other' environmental driver, but also add compost as an anthropogenic driver via the category 'Management Practices' for all three of the services the paper considered (AQreg, ClimReg, coastWQReg). 
# > CTW: correction needed is remove "Other" from Env driver and check "Management Practices" in Anthro for the three ES's

# read in Julie's to-correct csv to ID paper that needs update
allmissing <- list.files("round2_metareview/clean_qa_data/needs_classreview/missing_responsedriver/", full.names = T)
JLcorrections <- read.csv(allmissing[grep("JL", allmissing)], na.strings = na_vals)
# for each ES where "Other" entered in enviro driver, want to pull EffectDirect, assign to Anthro Driver, and NA the driver and check Management Practices

# subset dataset and apply corrections in temp df, then apply to master
tempdat <- subset(prelimlong1c, ResponseId == JLcorrections$ResponseId & qnum == "Q12")
tempES <- tempdat$ES[tempdat$clean_answer == "Other" & !is.na(tempdat$clean_answer)]
# correct driver answer
tempdat$clean_answer[tempdat$ES %in% tempES & tempdat$Group == "Anthro" & tempdat$abbr == "Driver"] <- "Management Practices"
tempdat$clean_answer[tempdat$ES %in% tempES & tempdat$Group == "Env" & tempdat$abbr == "Driver"] <- NA
# correct effect direct answer
tempdat$clean_answer[tempdat$ES %in% tempES & tempdat$Group == "Anthro" & tempdat$abbr == "EffectDirect"] <- tempdat$clean_answer[tempdat$ES %in% tempES & tempdat$Group == "Env" & tempdat$abbr == "EffectDirect"]
tempdat$clean_answer[tempdat$ES %in% tempES & tempdat$Group == "Env" & tempdat$abbr == "EffectDirect"] <- NA
# subset tempdat to just those rows that need update
tempdat <- subset(tempdat, is.na(answer) & !is.na(clean_answer) | !is.na(answer) & is.na(clean_answer))

# iterate through and correct
for(i in 1:nrow(tempdat)){
  prelimlong1c$clean_answer[prelimlong1c$ResponseId == unique(tempdat$ResponseId) & prelimlong1c$survey_order == tempdat$survey_order[i]] <- tempdat$clean_answer[i]
  prelimlong1c$qa_note[prelimlong1c$ResponseId == unique(tempdat$ResponseId) & prelimlong1c$survey_order == tempdat$survey_order[i]] <- "Reviewer correction"
}

copydf <- prelimlong1c

# Anna
# iterate through corrections with for-loop, as done for JL corrections
for(r in unique(AIScorrections$ResponseId)){
  # check that review still in working dataset (not excluded)
  if(!r %in% unique(prelimlong1c$ResponseId)){
    next
  }
  # subset corrections df to just answers that need corrections
  # Anna only put something in clean_answer if it was a correction, so can subset for that
  tempdat <- subset(AIScorrections, ResponseId == r & !is.na(clean_answer))
  for(i in 1:nrow(tempdat)){
    prelimlong1c$clean_answer[prelimlong1c$ResponseId == r & prelimlong1c$survey_order == tempdat$survey_order[i]] <- tempdat$clean_answer[i]
    prelimlong1c$qa_note[prelimlong1c$ResponseId == r & prelimlong1c$survey_order == tempdat$survey_order[i]] <- "Reviewer correction"
  }
}

copydf <- prelimlong1c

# Sierra
# remove empty rows
SDJcorrections <- subset(SDJcorrections, !is.na(ResponseId))
# > SDJ coded all 82 inidividual indicator variables used in Meyer et al. 2018, but looking at papers those were used in combination for a multifunctionality index and that index is the main y-var of interest in the paper
# > infilling Response as "multifunctionality index" to not inflate # of response variables across studies (since others did not break down all variables that went into indices in other papers)
# > and "mixed" for effect direction since that's what it was based on Sierra's review of all indicators
# > can add note to say see Sierra's excel sheet for more detailed review
# > "Other" for driver and use her OtherDriver response
# > will need to correct this paper manually

# apply corrections to other paper (Staes et al. 2017) with for loop
tempdat <- subset(SDJcorrections, grepl("Quantification of the potential impact of nature ", Title) & !is.na(clean_answer)) 
for(i in 1:nrow(tempdat)){
  prelimlong1c$clean_answer[prelimlong1c$ResponseId == unique(tempdat$ResponseId) & prelimlong1c$survey_order == tempdat$survey_order[i]] <- tempdat$clean_answer[i]
  prelimlong1c$qa_note[prelimlong1c$ResponseId == unique(tempdat$ResponseId) & prelimlong1c$survey_order == tempdat$survey_order[i]] <- "Reviewer correction"
}

# treat Meyer et al. 2018
# read in SDJ to-correct file
SDJtocorrect <- read.csv("round2_metareview/clean_qa_data/needs_classreview/missing_responsedriver/q12_reviewSDJ.csv", na.strings = na_vals)
MeyerES <- unique(with(SDJcorrections, ES[grepl("multifunctionality", Title)]))
tempdat <- subset(SDJtocorrect, grepl("multifunctionality", Title) & ES %in% MeyerES) %>%
  # infill with info from Sierra
  mutate(clean_answer = ifelse(abbr == "Response", "Multifunctionality index (from 82 indicator variables)", clean_answer),
         clean_answer = ifelse(abbr == "Yclass", unique(SDJcorrections$clean_answer[SDJcorrections$abbr == "Yclass" & grepl("multifunc", SDJcorrections$Title)]), clean_answer), #& !is.na(SDJcorrections$clean_answer
         clean_answer = ifelse(abbr == "EffectDirect" & Group == "Bio", "Mixed", clean_answer),
         clean_answer = ifelse(abbr == "Driver" & Group == "Bio", "Other", clean_answer),
         clean_answer = ifelse(abbr == "OtherDriver" & Group == "Bio", with(SDJcorrections, clean_answer[abbr == "OtherDriver" & grepl("multifunc", Title) & !is.na(clean_answer)]), clean_answer))
for(i in 1:nrow(tempdat)){
  prelimlong1c$clean_answer[prelimlong1c$ResponseId == unique(tempdat$ResponseId) & prelimlong1c$survey_order == tempdat$survey_order[i]] <- tempdat$clean_answer[i]
  prelimlong1c$qa_note[prelimlong1c$ResponseId == unique(tempdat$ResponseId) & prelimlong1c$survey_order == tempdat$survey_order[i]] <- "Reviewer correction"
}

copydf <- prelimlong1c

# Caitlin corrections
# Breland et al. should have "No" for threshold question (accidentally checked Mentioned)
prelimlong1c$clean_answer[prelimlong1c$ResponseId == "R_1QaoAiXNrrXyPUS" & prelimlong1c$abbr == "Thresholds"] <- "No"
prelimlong1c$qa_note[prelimlong1c$ResponseId == "R_1QaoAiXNrrXyPUS" & prelimlong1c$abbr == "Thresholds"] <- "Reviewer correction"

# also correction species richness and ESP for Chalcraft et al. 2013
# study used "local" mean species richness (plot level) and mean "regional" (site-level)
prelimlong1c$clean_answer[prelimlong1c$ResponseId == "R_3PNBrx8CScS4JTm" & prelimlong1c$survey_order == 205] <- "Mean species richness (by plot and by site)"
prelimlong1c$qa_note[prelimlong1c$ResponseId == "R_3PNBrx8CScS4JTm" & prelimlong1c$survey_order == 205] <- "Reviewer correction"
# correct ESP type (and notes)
prelimlong1c$clean_answer[prelimlong1c$ResponseId == "R_3PNBrx8CScS4JTm" & prelimlong1c$abbr == "ESP_type"] <- "Across species  [e.g. community structure, diversity]"
prelimlong1c$clean_answer[prelimlong1c$ResponseId == "R_3PNBrx8CScS4JTm" & prelimlong1c$abbr == "KremenNotes"] <- "Mean species richnes"
prelimlong1c$qa_note[prelimlong1c$ResponseId == "R_3PNBrx8CScS4JTm" & prelimlong1c$qnum == "Q14"] <- "Reviewer correction"

# save working copy
copydf <- prelimlong1c


# update no response/driver df before proceeding
correctedRIDs <- unique(c(SDJcorrections$ResponseId, JLcorrections$ResponseId, AIScorrections$ResponseId, ))
noResponseDriver <- subset(noResponseDriver, !ResponseId %in% correctedRIDs)



rm(MeyerES, SDJcorrections, SDJtocorrect, tempdat, AIScorrections, r, tempES, JLcorrections)


# 5.a. Driver corrections -----
# !> note: need to add in *new* response and driver variables from corrections in section preceeding this one..
# > .. maybe remake all_drivers_summary and response_summary?

all_driver_summary2 <- subset(prelimlong1c, abbr %in% c("Driver", "OtherDriver")) %>%
  # clean up answers that should be split
  mutate(clean_answer = gsub("hunting, fishing", "hunting or fishing", clean_answer),
         clean_answer = ifelse(grepl("^different olive grove |^Water level ", clean_answer), gsub(",", " ", clean_answer), clean_answer)) %>%
  dplyr::select(clean_answer, Group, ES) %>%
  arrange(clean_answer) %>%
  distinct() %>%
  splitcom(keepcols = names(.), splitcol = "clean_answer") %>%
  distinct() %>%
  subset(!answer %in% unique(alldrivers_summary$answer)) %>%
  arrange(Group, answer) %>%
  # drop vars i know are already accounted for
  subset(!grepl("different olive grove|Ecosystem service types", answer)) %>%
  # add count col (to rbind with drivers)
  rename(count = num)


# review bio drivers..
test_biodrivers <- dplyr::select(biodrivecorrections, answer, Driver_Finer) %>%
  distinct() %>%
  arrange(answer)

# notes:
# call land cover "habitat" as a biotic driver

biodiv_terms <- "divers|shannon|richn|even|loss of seed source|composition" # LD says "macroinvertebrate and fish" shoud remain ESP abundance, and composition should go in community structure
abund_terms <- "abundance"
# clean up
rm(test_biodrivers)

# go through environmental drivers and clean up bins, and assign what's outstanding
clean_biodriver_corrections <- rbind(alldrivers_summary, all_driver_summary2[names(alldrivers_summary)]) %>%
  subset(Group == "Bio") %>%
  #subset(alldrivers_summary, Group == "Bio") %>%
  #rbind(all_driver_summary2[names(alldrivers_summary)])
  #remove count since that may have changed
  dplyr::select(-count) %>%
  # rename group so joins properly
  rename(Driver_Group = Group) %>%
  left_join(dplyr::select(biodrivecorrections, -count)) %>%
  mutate(clean_driver_finer = trimws(casefold(Driver_Finer))) %>%
  # arrange by answer and fill down anything that wasn't added to list when SJD + KG filled it out
  arrange(answer) %>%
  group_by(answer) %>%
  fill(clean_driver_finer) %>%
  ungroup() %>%
  # re-class land cover as habitat
  mutate(clean_driver_finer = recode(clean_driver_finer, "land cover" = "habitat"),
         # characteristics of plot = biotic char of plot
         clean_driver_finer = gsub("characteristics of", "biotic characteristics of", clean_driver_finer),
         # infill others
         # indices
         clean_driver_finer = ifelse(grepl("index|82 different", answer, ignore.case = T), "index", clean_driver_finer),
         # diversity and richness drivers
         clean_driver_finer = ifelse(grepl(biodiv_terms, answer, ignore.case = T), "service provider diversity/richness", clean_driver_finer),
         # abundance drivers
         clean_driver_finer = ifelse(grepl(abund_terms, answer, ignore.case = T), "service provider abundance", clean_driver_finer),
         # general service provider option
         clean_driver_finer = ifelse(grepl("^service provider|biotic driver=|Aphodius", answer, ignore.case = T) & !grepl("yield", answer), "service provider identity", clean_driver_finer),
         # ESP density
         clean_driver_finer = ifelse(grepl("density", answer, ignore.case = T), "service provider density", clean_driver_finer),
         # clean up randos
         clean_driver_finer = ifelse(grepl("polychaete", answer), "pathogens/natural enemies",
                                     # these particular variables go with one of AK's papers on population modeling and individual maintenance, feeding, groth, and reproduction
                                     ifelse(grepl("reproduc|maintenance|growth|feeding", answer, ignore.case = T), "Service provider reproduction, growth, and population maintenance",
                                            ifelse(grepl("root depth|vegetation cover", answer, ignore.case = T), "biotic characteristics of the plot", clean_driver_finer))),
         # call Other 'Other' just in case no otherdriver text entered (can at least count Other, if other text available, can drop Other)
         clean_driver_finer = ifelse(answer == "Other", "other", clean_driver_finer),
         # move management to anthro category
         clean_driver_group = ifelse(clean_driver_finer == "management", "Anthro", Driver_Group)) %>%
  #didn't get NAs
  replace_na(list(clean_driver_group ="Bio")) %>%
  # fix various trout (correct original group -- should be Env but CTW wrote out changed to Bio, so code below doesn't pick it up)
  mutate(Driver_Group = ifelse(grepl("various trout", answer), "Env", Driver_Group)) %>%
  # also change "measured 82 variables.." back to NA [CTW infilled to "no ES selected" when wrote out above]
  #ES = ifelse(grepl("^measured 82 different", answer), NA, ES)) %>%
  #trim ws on all
  mutate_all(trimws)



# review envdrivers..
test_envdrivers <- dplyr::select(envdrivecorrections, answer, Driver_Finer) %>%
  distinct() %>%
  arrange(answer)
## ctw impressions:
# > if soil chemical components are separate from aquatic chem, "soil characteristics" should be soil physio-chemical characteristics to be clear
# > another options would be anything that's chemical (whether soil or aquatic) gets labeled as biogeochem

# > for "Topography and landscape position", distance to... in env should fall in that too (e.g. distance to river)
# > I also think any response or driver that's a composite should be in an "Index" category
# > anything that's management in "environment" should be moved to a human driver (e.g. prescribed burn in environmental driver should be changed to management per our convo 5/21)
# can ignore "Other".. and some weird ones:
## > "Ecosystem service types," seems to be error.. in Travis' paper in "OtherDriver" for Env.. but he never checked "Other". Doesn't make sense as an env driver, so leaning towards it's a mistake
## > "rewiring" is network structure rearrangement (IS paper [Costa et al. 2018].. looked at abstract and they manipulated interactions in an ecosystem network, simulating network with and without rearrangement ("rewiring")))... also think rewiring should be moved to biotic since they were removing species to see how affected ES, not anything env-related
## > "turnover time" .. is from a CW paper that got excluded by LD review so doesn't matter now.. but could be abiotic characteristic: aquatic
## > "hypothetical stressors (abstract and vague)" .. from "When things don't add up: quantifying impacts.." GV and AK reviewed. AK wrote "Temperature".. I looked at paper (Gavin et al. 2018, Ecol Letter).. they are hypothetical (unnamed) stressors in pairwise combos that are either A, B, additive, synergistic +, synergistic - ... call it index
## > "daytime light" .. from "Linking environmental variables with regional-scale..", AIS reviewed, is about carbon and kelp forests in UK, so ultimately an abiotic characteristics--aquatic 
## > "inundation" .. was from greenhouse study looking at roots and chemical response properties.. abiotic characteristics--terrestrial
## > "evapotrans" = climate (from NBD paper on Loess Plateau in China.. I've read that too and it's an remote sensing study, so used an ET layer, modeled off of climate and abiotic factors.. since ET function of plant and weather, binning as climate)

topo_keywords <- "distance|DEM|elevation|slope|aspect|geograph|altitude|latitud|longitud|topography|topology| position" # considered nodes as well.. but leaving in hydrology.. could add "connectivity" to bin label though.. 
# clean up
rm(test_envdrivers)

# go through environmental drivers and clean up bins, and assign what's outstanding
clean_envdriver_corrections <- rbind(alldrivers_summary, all_driver_summary2[names(alldrivers_summary)]) %>%
  subset(Group == "Env") %>%
  #remove count since that may have changed
  dplyr::select(-count) %>%
  # rename group so joins properly
  rename(Driver_Group = Group) %>%
  left_join(dplyr::select(envdrivecorrections, -count)) %>%
  mutate(clean_driver_finer = trimws(casefold(Driver_Finer))) %>%
  # arrange by answer and fill down anything that wasn't added to list when SJD + KG filled it out
  arrange(answer) %>%
  group_by(answer) %>%
  fill(clean_driver_finer) %>%
  ungroup() %>%
  # begin final cleanup
  mutate(
    # windthrow is a disturbance not abiotic characteristic, change to "extreme event"
    clean_driver_finer = ifelse(grepl("fire|burn|windthrow", answer, ignore.case = T), "extreme events", clean_driver_finer),
    # change index and hypothetical stressors type to index bin
    clean_driver_finer = ifelse(grepl("index|hypothetical stress", answer, ignore.case = T), "index", clean_driver_finer), 
    # fix soil
    clean_driver_finer = ifelse(grepl("soil", answer, ignore.case = T), "soil physiochemical characteristics", clean_driver_finer),
    # assign topography and landscape position
    clean_driver_finer = ifelse(grepl(topo_keywords, answer, ignore.case = T), "topography and position", clean_driver_finer),
    # landscape goes in land cover
    clean_driver_finer = ifelse(grepl("landscape|habitat provis|land cover type", answer, ignore.case = T), "land cover", clean_driver_finer),
    # add weirdos to abiotic char--aquatic
    clean_driver_finer = ifelse(grepl("daytime light|turnover|HDO satu", answer, ignore.case = T), "abiotic characteristics of the landscape: aquatic", clean_driver_finer),
    # add weirdos to abiotic char--terrestrial
    clean_driver_finer = ifelse(grepl("inundat|NDWI", answer, ignore.case = T), "abiotic characteristics of the landscape: terrestrial", clean_driver_finer),
    # add weirdos + newbies to climate (wind itself should be climate if annual windspeed is..)
    clean_driver_finer = ifelse(grepl("evapotrans|drought|annual wind", answer, ignore.case = T) | answer == "wind", "climate", clean_driver_finer),
    # assign biotic driver bin to rewiring
    clean_driver_finer = ifelse(grepl("rewiring", answer, ignore.case = T), "network property", clean_driver_finer),
    # maybe to be sure specify "Ecosystem service types," as "remove"
    clean_driver_finer = ifelse(grepl("^Ecosystem service", answer), "index", clean_driver_finer), # "Ecosystem service types = ES bundles
    # call Other 'Other' just in case no otherdriver text entered (can at least count Other, if other text available, can drop Other)
    clean_driver_finer = ifelse(answer == "Other", "other", clean_driver_finer),
    # ådd col for new driver type if needs to be re-assigned
    clean_driver_group = ifelse(clean_driver_finer %in% c("productivity", "vegetation cover") | answer == "rewiring", "Bio", 
                                ifelse(clean_driver_finer == "management", "Anthro", Driver_Group)),
    # infill group for NA just in case (only applies to answer == Other)
    clean_driver_group = ifelse(answer == "Other", Driver_Group, clean_driver_group))

# clean up driver answers that need commas adjusted
clean_envdriver_corrections <- clean_envdriver_corrections %>%
  # remove heterogenous bc should be split
  filter(!grepl("Heterogeneous\\)", answer)) %>%
  # add Heterogeneous back to Landscape type and clean up other comma answers
  mutate(answer = ifelse(grepl("^Landscape types", answer), paste0(answer, "; Heterogeneous)"), 
                         ifelse(grepl("olive grove type", answer), gsub(",", ";", answer), gsub(",", "", answer))),
         answer = trimws(answer))


# review human drivers..
test_anthdrivers <- dplyr::select(anthdrivecorrections, answer, Driver_Finer) %>%
  distinct() %>%
  arrange(answer)

# notes: review characteristics of plot (should any of these be in env or biotic??)
## > shade/canopy cover... correspond to "Anolis lizards as biocontrol agents in mainland..." (SDJ).. lizard predation on pest beetles in coffee farm.. seems like shade and canopy cover should be environment (both proxy shade)
## > mean area/edge, spatial agg, mean lot size, mean patch size ... come from "Landscape structure influences urban vegetation ver..." (SDJ).. all seem like characteristics of place
## > wildflower strips ... seems like this should be a biotic driver, not anthro .. either that or it goes under management/restoration
## > tree volume, stand age .. comes from Truchy et al. 2019 (CTW's paper).. looked at paper and using those in land cover type way (stand age and tree volume = proxies for less disturbed soil)

# specify terms that need matching or re-class
management_terms <- "age|tillage|manipulation|livestock|wildflower" #add tillage to switch from surface disturbance to management, since that's what it is
development_terms <- "distance|proximity|roads|urban areas"
human_disturbance_terms <- "disturbance|impacts|ditching"
char_humanpop <- "composition|structure of the homog|orientation|policy|socioecon"
place_terms <- "city|apartment|municipal|green roof|location|street density|mean edge|mean lot|mean patch|spatial agg" #characteristics of place
landuse_terms <- "deforestation|urbanization|land use change|tree volume|stand age|urban devel" # as long as stand age comes after setting mgmt terms, will apply correct finer driver
# clean up
rm(test_anthdrivers)

# go through environmental drivers and clean up bins, and assign what's outstanding
clean_anthdriver_corrections <- rbind(alldrivers_summary, all_driver_summary2[names(alldrivers_summary)]) %>%
  subset(Group == "Anthro") %>%
  #remove count since that may have changed
  dplyr::select(-count) %>%
  # rename group so joins properly
  rename(Driver_Group = Group) %>%
  left_join(dplyr::select(anthdrivecorrections, -count)) %>%
  mutate(clean_driver_finer = trimws(casefold(Driver_Finer))) %>%
  # arrange by answer and fill down anything that wasn't added to list when SJD + KG filled it out
  arrange(answer) %>%
  group_by(answer) %>%
  fill(clean_driver_finer) %>%
  ungroup() %>%
  # fix typos in drivers
  mutate(clean_driver_finer = gsub("distrubances", "disturbances", clean_driver_finer),
         clean_driver_finer = gsub(" adn", " and", clean_driver_finer),
         # change land use to LULCC
         clean_driver_finer = recode(clean_driver_finer, 'land use' = 'land use and land cover change',
                                     # land cover becomes proximity to development
                                     'land cover' = 'proximity to development'),
         # start infilling
         # human management and restoration
         clean_driver_finer = ifelse(grepl(management_terms, answer, ignore.case = T), "management/restoration", clean_driver_finer),
         # proximity to development
         clean_driver_finer = ifelse(grepl(development_terms, answer, ignore.case = T), "proximity to development", clean_driver_finer),
         # human disturbance
         clean_driver_finer = ifelse(grepl(human_disturbance_terms, answer, ignore.case = T), "human disturbance", clean_driver_finer),
         # characteristics of human population
         clean_driver_finer = ifelse(grepl(char_humanpop, answer, ignore.case = T), "characteristics of human population", clean_driver_finer),
         # characteristics of locality
         clean_driver_finer = ifelse(grepl(place_terms, answer, ignore.case = T), "characteristics of locality", clean_driver_finer),
         # add to land use
         clean_driver_finer = ifelse(grepl(landuse_terms, answer, ignore.case = T), "land use and land cover change", clean_driver_finer),
         # add index
         clean_driver_finer = ifelse(grepl("index", answer, ignore.case = T), "index", clean_driver_finer),
         # shade and canopy cover (proxy for shade) should be abiotic char-terrestrial and assigned to Env
         clean_driver_finer = ifelse(answer %in% c("shade", "canopy cover"), "abiotic characteristics of the landscape: terrestrial", clean_driver_finer),
         # call Other 'Other' just in case no otherdriver text entered (can at least count Other, if other text available, can drop Other)
         clean_driver_finer = ifelse(answer == "Other", "other", clean_driver_finer),
         # clean up group
         clean_driver_group = ifelse(grepl("abiotic", clean_driver_finer), "Env", Driver_Group)) %>%
  # change Exploitation from or to semicolon
  mutate(answer = gsub("hunting or fishing", "hunting; fishing", answer))
# fix "not just land use change but..." (had a comma)
p1 <- grep("^not just land use change", clean_anthdriver_corrections$answer)
p2 <- grep("^but land use intensification", clean_anthdriver_corrections$answer)
clean_anthdriver_corrections$answer[p2] <- paste(clean_anthdriver_corrections$answer[p1], clean_anthdriver_corrections$answer[p2])
# remove p1
clean_anthdriver_corrections <- clean_anthdriver_corrections[-p1,]
rm(p1, p2)

# stack all for master driver reference set
master_driver_corrections <- dplyr::select(clean_biodriver_corrections, -c('Binning Notes', Driver_Finer)) %>%
  rbind(clean_anthdriver_corrections[names(.)], clean_envdriver_corrections[names(.)]) %>%
  arrange(answer, ES, Driver_Group) %>%
  #capitalize first letter in clean_driver_finer
  mutate(clean_driver_finer = ifelse(!is.na(clean_driver_finer), 
                                     paste0(casefold(substr(clean_driver_finer, 1,1), upper = T), substr(clean_driver_finer, 2, nchar(clean_driver_finer))),
                                     clean_driver_finer)) %>%
  # make driver group and driver_finer more generic (so can rbind with cleaned responses)
  rename(clean_answer_finer = clean_driver_finer,
         clean_group = clean_driver_group) %>%
  # be sure all has no white space
  mutate_all(trimws)

# write out for LD, AK, SDJ, and KG to review
write_csv(master_driver_corrections, "round2_metareview/data/intermediate/round2_master_driver_bins.csv", na = "")



# 5.b. Response corrections -----
# pull added response vars from corrections
all_responses2 <- subset(prelimlong1c, abbr == "Response") %>%
  # clean up answers that should be split
  # mutate(clean_answer = gsub("hunting, fishing", "hunting or fishing", clean_answer),
  #        clean_answer = ifelse(grepl("^different olive grove |^Water level ", clean_answer), gsub(",", " ", clean_answer), clean_answer)) %>%
  dplyr::select(clean_answer, ES) %>%
  arrange(clean_answer) %>%
  distinct() %>%
  splitcom(keepcols = names(.), splitcol = "clean_answer") %>%
  distinct() %>%
  subset(!answer %in% unique(response_summary$Response)) %>%
  arrange(answer) %>%
  rename(Response = answer, EcoServ = ES) %>%
  # add cols in corrections not here so can rbind
  cbind(data.frame(matrix(nrow = nrow(.), ncol = length(names(response_summary)[!names(response_summary) %in% names(.)]),
                          dimnames = list(NULL, names(response_summary)[!names(response_summary) %in% names(.)])))) %>%
  rename_all(function(x) gsub("[.]", " ", x)) %>%
  dplyr::select(names(response_summary))


test_responses <- dplyr::select(responsecorrections, Response, Key.word) %>%
  distinct() %>%
  arrange(Response)
summary(is.na(test_responses$Key.word))

# set functional biodiv keywords
fxnlbiodiv_terms <- "functional even|functional disp|functional diver|functional trait|fish trait|invertebrate trait"

# go through responses and clean up bins, assign what's outstanding
clean_responses_corrections <- response_summary %>%
  rbind(all_responses2) %>%
  # drop EF/ES counts in corrections file since not up to date
  left_join(dplyr::select(responsecorrections, EcoServ:Notes)) %>%
  mutate(response_finer = trimws(casefold(Key.word))) %>%
  # arrange by answer and fill down anything that wasn't added to list when SJD + KG filled it out
  arrange(Response, EcoServ) %>%
  group_by(Response) %>%
  fill(response_finer) %>%
  ungroup() %>%
  # remove NA values (think that's just from remnant empty/NA cell)
  filter(!Response == "NA") %>%
  # clean up typos in finer_responses
  mutate(response_finer = gsub("service provider diversity/richness/composition", "service provider species biodiversity", response_finer),
         response_finer = gsub("service provider function/functional traits", "service provider functional biodiversity and traits", response_finer),
         # triage function biodiversity
         response_finer = ifelse(grepl("function", Response), gsub("species biodiversity", "functional biodiversity and traits", response_finer), response_finer),
         # standardize soil char label with label used in env driver
         response_finer = ifelse(grepl("^soil", response_finer), "soil physiochemical characteristics", response_finer),
         # standardize abiotic characteristics
         response_finer = ifelse(grepl("aquatic$", response_finer, ignore.case = T), "abiotic characteristics of the landscape: aquatic", response_finer),
         # fix abiotic typo
         response_finer = gsub("abitic", "abiotic", response_finer),
         # fix exploitation typo
         response_finer = gsub("expolitation", "exploitation", response_finer),
         # fix hydrology typo
         response_finer = gsub("hydology", "hydrology", response_finer),
         # fix tourism typo
         response_finer = gsub("toursim", "tourism", response_finer),
         # fix landscape typo
         response_finer = gsub("lanscape", "landscape", response_finer),
         # standardize pollution bin
         response_finer = recode(response_finer, "pollution/water quality" = "pollution/water quality/air quality"),
         # infill what needs infilling...
         rf2 = response_finer,
         # abundance
         rf2 = ifelse(grepl(abund_terms, Response, ignore.case = T), "service provider abundance", rf2),
         # species biodiversity (richness, diversity, evenness)
         rf2 = ifelse(grepl(biodiv_terms, Response, ignore.case = T)  & !grepl("decomposition", Response, ignore.case = T), "service provider species biodiversity", rf2),
         # functional biodiversity and traits -- needs to come after species biodiversity to replace
         rf2 = ifelse(grepl(fxnlbiodiv_terms, Response,ignore.case = T), "service provider functional biodiversity and traits", rf2),
         # change decomposition to TEST_VALUE (placeholder) -- got assigned biodiv since has "composition" in it
         #rf2 = ifelse(grepl("decomposition", Response, ignore.case = T), "TEST_VALUE"
         # if has biomass or aboveground or AGB, productivity
         rf2 = ifelse(grepl("biomass|AGB|aboveground|NPP|NDVI", Response, ignore.case = T), "productivity", rf2),
         # if it has carbon, then carbon
         rf2 = ifelse(grepl("carbon|CO2", Response, ignore.case = T), "carbon", rf2),
         # triage value and appreciation
         rf2 = ifelse(grepl("value|appreciation|aesthetic|revenue|social vulner|social set|socio-econo", Response, ignore.case = T) & Response != "grass nutritional value", "characteristics of human population", rf2),
         # assign index/composite
         rf2 = ifelse(grepl("index|total diversity|score|integrity", Response, ignore.case = T), "index", rf2),
         # reclass velocity, flow, discharge, etc. as hydrology
         rf2 = ifelse(grepl("flow", Response, ignore.case = T) & grepl("WQ", EcoServ)|
                        grepl("discharge|flow velocity|water yield", Response, ignore.case = T), "hydrology", rf2), # buuut "sap flow" and "stemflow" need to be something else
         rf2 = ifelse((EcoServ == "Materials" & !grepl("value", Response)) | grepl("yield", Response, ignore.case = T), "exploitation/harvest", rf2)) %>%
  # recode any NA with "test" just so can move on with coding
  replace_na(list(rf2="TEST_VALUE"))


# write file for LD and AK to finish assigning
responses_forLDAK <- clean_responses_corrections
write_csv(responses_forLDAK, "round2_metareview/clean_qa_data/needs_classreview/partially_binned_ESresponse_review.csv", na = "")

# apply same keywords 


# 5.c. Apply driver and response corrections -----
# maybe try pulling out Q12 to clean and tidy on its own?
q12df_clean <- subset(prelimlong1c, qnum == "Q12")

copydf <- prelimlong1c
# importante! need to erase commas from drivers where shouldn't be comma-split (and so pairs appropriately with driver corrections)
## Exploitation (hunting, fishing) -> (hunting or fishing)
## Water level (current and previous years min,max,mean), -> gsub out commas

drivers_removesinglecommas <- c("previous years min,max,mean") 
drivers_removedblcommas <- c("not just land use change, but|system function, not ESP")
drivers_comma2semicol <- c("Homogeneous, Heterogeneous|organic, conventional, abandoned, natural areas")
driver_commacheck <- subset(q12df_clean, abbr %in% c("Driver", "OtherDriver") & !is.na(answer) & grepl(",", answer)) %>%
  dplyr::select(answer) %>%
  arrange(answer) %>%
  distinct()
q12df_clean <- q12df_clean %>%
  mutate(clean_answer = ifelse(grepl(drivers_removesinglecommas, clean_answer), gsub(",", " ", clean_answer),
                               ifelse(grepl(drivers_removedblcommas, clean_answer), gsub(",", "", clean_answer),
                                      ifelse(grepl(drivers_comma2semicol, clean_answer), gsub(",", ";", clean_answer), 
                                             gsub("hunting, fishing", "hunting; fishing", clean_answer)))))

kept_ResponseId <- unique(q12df_clean$ResponseId)
master_clean_q12 <- data.frame()
#unique(q12df_clean$ResponseId)
for(rid in kept_ResponseId){
  # the only way to do this cleanly is to go by ResponseId
  temp_q12<- subset(q12df_clean, ResponseId == rid)
  
  # need to assign "MISSING" to response, drivers, yclass and effect direct for cases where missing AND for cases where missing when doublerev = TRUE
  # id which answers (if any) are missing, and infill so needed rows don't get removed
  if(rid %in% unique(noResponseDriver$ResponseId)){
    temp_infill_responsedriver <- subset(noResponseDriver, ResponseId == rid)
    if(unique(temp_infill_responsedriver$flag_noResponse)){
      # subset which drivers are applicable if that's the case
      temp_infill_responsedriver <- filter(temp_infill_responsedriver, 
                                           (Group == unique(Group[!is.na(answer)]) & grepl("Driver|Effect", abbr)) | !grepl("Driver|Effect", abbr)) %>%
        # assign missing
        mutate_at(vars("answer"), function(x) ifelse(is.na(x) & .[,"abbr"] %in% c("Response", "Yclass"), "MISSING", x))
      # add qa note
      temp_q12$qa_note[temp_q12$survey_order %in% temp_infill_responsedriver$survey_order] <- "Driver variables for ES provided, but no response variables"
    }
    if(unique(temp_infill_responsedriver$flag_noDriver)){
      temp_infill_responsedriver <- temp_infill_responsedriver %>%
        # infill driver and effect
        mutate_at(vars("answer"), function(x) ifelse(is.na(x) & .[,"abbr"] %in% c("Driver", "EffectDirect"), "MISSING", x))
    }
    # infill answers in working q12 df
    temp_q12$answer[temp_q12$survey_order %in% temp_infill_responsedriver$survey_order] <- temp_infill_responsedriver$answer
    temp_q12$clean_answer[temp_q12$survey_order %in% temp_infill_responsedriver$survey_order] <- temp_infill_responsedriver$answer
    # add qa note
    temp_q12$qa_note[temp_q12$answer == "MISSING" & temp_q12$abbr %in% c("Driver", "EffectDirect")] <- "Response variable for ES provided, but no driver variables"
  }
  
  # infill MISSING to OtherDriver if id in othercheck
  if(rid %in% unique(othercheck$ResponseId)){
    temp_othercheck <- subset(othercheck, ResponseId == rid) 
    temp_q12$answer[temp_q12$abbr == "OtherDriver" & temp_q12$Group %in% unique(temp_othercheck$Group)] <- "MISSING"
    temp_q12$clean_answer[temp_q12$abbr == "OtherDriver" & temp_q12$Group %in% unique(temp_othercheck$Group)] <- "MISSING"
    # add qa_notes
    temp_q12$qa_note[temp_q12$abbr == "OtherDriver" & temp_q12$Group %in% unique(temp_othercheck$Group)] <- "'Other' checked for driver variable but no Other Driver provided"
  }
  
  # catch missing "Other" in abbr == Driver if OtherDriver present so code below infills clean_answer_finer correctly
  otherdrivergrps <- unique(with(temp_q12, Group[abbr == "OtherDriver" & !is.na(clean_answer)]))
  # just need to be sure there is some Driver entered for given group ["Other" will be appended below if not there but some other Driver is]
  drivergrps <- unique(with(temp_q12, Group[abbr == "Driver" & !is.na(clean_answer)]))
  missinggrps <- otherdrivergrps[!otherdrivergrps %in% drivergrps]
  if(length(missinggrps)>0){
    # assign "Other" and add qa note
    # > need to assume Other applies to any Response ES
    temp_unique_ES <- unique(with(temp_q12, as.character(ES[!is.na(answer) & !is.na(ES) & grepl("Driver|Response", abbr)])))
    # id rows for applicable ES and Driver
    temprows <- which(temp_q12$ES %in% temp_unique_ES & temp_q12$abbr =="Driver" & temp_q12$Group %in% missinggrps)
    temp_q12$clean_answer[temprows] <- "Other"
    temp_q12$qa_note[temprows] <- "Appended 'Other' to Driver (Other driver entered but 'Other' not checked)"
  }
  # clean up
  rm(otherdrivergrps, drivergrps, missinggrps)
  
  # moving on..
  temp_q12 <- temp_q12 %>%
    filter(!is.na(answer) | (is.na(answer) & !is.na(clean_answer))) %>%
    # unfactor ES's
    mutate(ES = as.character(ES))
  
  # some things.. 
  # > need to assign "Other" to abbr == "Driver" if it's not there and text in "OtherDriver" present
  
  # break out responses
  temp_q12responses <- subset(temp_q12, abbr == "Response")
  # which ESs ID'd in response
  temp_unique_ESresponse <- unique(with(temp_q12responses, ES[which(!is.na(answer))]))
  # break out responses by splitcom, append finer response group
  temp_q12responses <- temp_q12responses %>%
    # change answer to orig_answer before splitcom
    rename(orig_answer = answer) %>%
    splitcom(keepcols = names(.), splitcol = "clean_answer") %>%
    # join finer response bins
    left_join(clean_responses_corrections[c("EcoServ", "Response", "rf2")], by = c("ES" = "EcoServ", "answer" = "Response")) %>% # will need to change rf2 later..
    # fill in MISSING for answer_finer if clean answer is missing
    mutate(rf2 = ifelse(answer == "MISSING", "MISSING", rf2)) %>%
    # rename cols to rbind with cleaned up drivers later on..
    rename(clean_answer = answer, answer = orig_answer, clean_answer_finer = rf2, varnum = num) %>%
    # add tracking numer and clean group col to match cleaned up driver df
    mutate(track = 1, clean_group = NA)
  
  # break out drivers
  temp_q12drivers <- subset(temp_q12, grepl("Driver", abbr))
  # first, ID which has other drivers listed and whether Other present in Driver field
  temp_otherdriver_grp <- unique(with(temp_q12drivers, Group[which(!is.na(answer) & abbr == "OtherDriver")]))
  # then go through and check for Other in Driver, if not present append
  # > if other driver entered for a given group, must be checked in at least 1 ES for that group
  # > this for-loop won't do anything if temp_otherdriver_grp is empty (i.e. no other drivers entered)
  for(i in temp_otherdriver_grp){
    # ID which rows (i.e. which ESs) have drivers entered
    grprow <- with(temp_q12drivers, which(Group == i & abbr == "Driver"))
    # only paste if it's not in any Driver field for that Group (look across all ES's where filled out)
    # > this will do nothing if grprow empty (e.g. someone entered other driver for given group, but no drivers entered in that group)
    if(!any(grepl("Other", temp_q12drivers$clean_answer[grprow]))){
      temp_q12drivers$clean_answer[grprow] <- paste0(temp_q12drivers$clean_answer[grprow], ",Other")
    }
  }
  
  # now break out drivers by splitcom..
  temp_q12drivers <- temp_q12drivers %>%
    rename(orig_answer = answer) %>%
    splitcom(keepcols = names(.), splitcol = "clean_answer") %>%
    dplyr::select(StartDate:orig_answer, answer, num, fullquestion:ncol(.)) %>%
    arrange(survey_order, num) %>%
    data.frame()
  
  # add QA note for Other
  # id rows that have "Other"
  temp_otheradded_rows <- with(temp_q12drivers, which(answer == "Other" & !grepl("Other", orig_answer) & !is.na(orig_answer)))
  # append QA note
  # > if no "Other" added to abbr== Driver rows, for-loop will not do anything
  for(i in temp_otheradded_rows){
    if(is.na(temp_q12drivers$qa_note[i])){
      # add append other to qa note
      temp_q12drivers$qa_note[i] <- "Appended 'Other' to Driver (Other driver entered but 'Other' not checked)"
    }else{
      # paste append other to existing qa note
      temp_q12drivers$qa_note[i] <- paste0(temp_q12drivers$qa_note[i], "; appended 'Other' to Driver (Other driver entered but 'Other' not checked)")
    }
  }
  
  # if Other Drivers present, need to now iterate through Other and assign an ES to Other Driver fields
  if("OtherDriver" %in% unique(temp_q12drivers$abbr)){
    # compile list of different ES's listed in driver
    temp_ESdriver_list <- list("Env" = sort(with(temp_q12drivers, unique(ES[answer == "Other" & Group == "Env"]))),
                               "Anthro" = sort(with(temp_q12drivers, unique(ES[answer == "Other" & Group == "Anthro"]))),
                               "Bio" = sort(with(temp_q12drivers, unique(ES[answer == "Other" & Group == "Bio"]))))
    # only keep the components that have "Other" listed
    temp_ESdriver_list <- temp_ESdriver_list[sapply(temp_ESdriver_list, length) > 0]
    # if no ESs ID'd. generate list from ES's available where Driver entered
    # > ex. case: RID R_306ID7zs5CnvD81, Drivers entered for Anthro group in several ES's, OtherDriver entered for Env with NA ES (bc other).. Env Driver never entered 
    if(length(unlist(temp_ESdriver_list)) == 0){
      # set trigger to use different infill message
      trigger_otherinfill <- TRUE}else{
        # if list populated, turn off trigger
        trigger_otherinfill <- FALSE
      }
    if(trigger_otherinfill){
      # reset list
      temp_ESdriver_list <- list()
      # need to see which Group has "OtherDriver" and then assign ES's from Driver to that group .. or just use unique ES from response?
      for(i in 1:length(temp_otherdriver_grp)){
        # build list
        temp_ESdriver_list[[i]] <- temp_unique_ESresponse
        names(temp_ESdriver_list)[i] <- temp_otherdriver_grp[i]
      }
    }
    tempother_df <- data.frame()
    for(i in 1:length(temp_ESdriver_list)){
      if(length(temp_ESdriver_list[[i]]) > 0){
        # subset env 
        tempdat_other <- subset(temp_q12drivers, abbr == "OtherDriver" & Group == names(temp_ESdriver_list[i]))
        # if no rows pulled, it means "Other" checked but no OtherDriver entered
        if(nrow(tempdat_other) == 0){
          tempdat_other <- subset(q12df_clean, ResponseId == rid & abbr == "OtherDriver" & Group == names(temp_ESdriver_list[i])) %>%
            mutate(answer = "MISSING", clean_answer = "MISSING", num = 1) %>%
            rename(orig_answer = answer, answer = clean_answer) %>%
            dplyr::select(names(tempdat_other))
        }
        # expand tempdat_otherenv by however many other env ES's are indicated
        tempdat_other2 <- do.call("rbind", replicate(length(temp_ESdriver_list[[i]]), tempdat_other, simplify = FALSE))
        # infill ES's
        tempdat_other2$ES <- rep(temp_ESdriver_list[[i]], each = max(tempdat_other2$num))
        # bind to master
        tempother_df <- rbind(tempother_df, tempdat_other2)
      }
      # clean up
      rm(tempdat_other, tempdat_other2)
    }
    # infill ESnum
    tempother_df$ESnum <- merge(tempother_df[c("ES")], distinct(headerLUT[c("ES", "ESnum")]), by = "ES", all.x = T)$ESnum
    if(trigger_otherinfill){
      # add special QA note
      tempother_df$qa_note <- paste(tempother_df$qa_note, "Other not checked but OtherDriver entered, infilled ES based on ES rows where data entered", sep = "; ")
    }else{
      # add regular QA note
      tempother_df$qa_note <- paste(tempother_df$qa_note, "Infilled ES from Driver where 'Other' specified", sep = "; ")
    }
    # clean up if pasted to empty string
    tempother_df$qa_note <- gsub("NA; ", "", tempother_df$qa_note)
    # if trigger is true, also need to create Other rows for abbr == Driver for ES's ID'd
    if(trigger_otherinfill){
      # subset q12df_clean again
      tempdriver_infill <- subset(q12df_clean, ResponseId == rid & Group %in% unique(tempother_df$Group) & abbr == "Driver" & ES %in% unique(tempother_df$ES)) %>%
        # append "Other" to clean_answer
        # > bc "Other" infilled earlier in loop, if we're at this point it means rows for Driver are empty
        mutate(clean_answer = "Other",
               # will be only variable for given ES and Group
               num = 1,
               # add QA note
               qa_note = ifelse(is.na(qa_note), "Added 'Other' to Driver based on Other Driver answer",
                                # if qa_note not empty, append comment
                                paste0(qa_note, "; added 'Other' to Driver based on Other Driver answer"))) %>%
        #rename answer cols so can rbind
        rename(orig_answer = answer,
               answer = clean_answer)
      
      # rbind infilled driver rows to infilled other driver rows 
      tempother_df <- rbind(tempother_df, tempdriver_infill[names(tempother_df)]) %>%
        arrange(survey_order)
      
    }
    # add Other Drivers back to data frame (swap cleaned up for original/unclean)
    temp_q12drivers <- subset(temp_q12drivers, !survey_order %in% tempother_df$survey_order) %>% #Added 'Other' to Driver based on Other Driver answer
      rbind(tempother_df) %>%
      arrange(survey_order)
  }
  
  # then add variable bin..
  temp_q12drivers <- temp_q12drivers %>%
    left_join(master_driver_corrections, by = c("ES", "Group" = "Driver_Group", "answer")) %>%
    # assign clean_driver_finer for MISSING answers
    mutate(clean_answer_finer = ifelse(answer == "MISSING", "MISSING", clean_answer_finer),
           # infill clean_answer finer for any clean_answer that's "Other" and wasn't caught in driver binning
           clean_answer_finer = ifelse(answer == "Other" & is.na(clean_answer_finer) & abbr == "Driver", "Other", clean_answer_finer)) %>%
    # reorder cols
    dplyr::select(StartDate:num, clean_answer_finer, fullquestion, abbr, order, Group, clean_group, ESnum:ncol(.)) %>%
    # rename answers back to answer and clean_answer
    rename(clean_answer = answer,
           answer = orig_answer,
           varnum = num) %>%
    mutate(track = 1)
  
  
  # need to put back in data frame..
  # build data frame for the Response Id in current iteration
  temp_q12_clean <- subset(q12df_clean, ResponseId == rid) %>%
    mutate(ES = as.character(ES),
           track = 0) %>%
    cbind(data.frame(matrix(nrow = nrow(.), 
                            ncol = sum(!names(temp_q12drivers) %in% names(.)),
                            dimnames = list(NULL, names(temp_q12drivers)[!names(temp_q12drivers) %in% names(.)])))) %>%
    dplyr::select(names(temp_q12drivers)) %>%
    data.frame() %>%
    # filter out any rows replaced in cleaning (are there downsides to doing it this way??)
    filter(!survey_order %in% unique(c(temp_q12drivers$survey_order, temp_q12responses$survey_order))) %>% # <-- add in response survey_order rows too
    rbind(temp_q12drivers, temp_q12responses[names(temp_q12drivers)]) %>% # add in temp_q12responses
    arrange(survey_order, track, varnum)
  
  # double check all survey_order nums that should be there are there
  stopifnot(all(unique(temp_q12_clean$survey_order) %in% unique(headerLUT$survey_order[headerLUT$qnum == "Q12"])))
  
  # rbind into master
  master_clean_q12 <- rbind(master_clean_q12, temp_q12_clean) %>% data.frame()
}

# clean enviro after for loop done
rm(i, rid, temp_q12, temp_q12_clean, temp_q12drivers, temp_q12responses, tempother_df,
   temp_othercheck, temp_unique_ESresponse, temp_otheradded_rows, temp_otherdriver_grp)

# check for any drivers or responses that didn't have clean_answer_finer assigned
missingbin <- subset(master_clean_q12, is.na(clean_answer_finer) & !is.na(clean_answer) & grepl("Driver|Response", abbr))
# only 1 missing.. (can change to for loop later if more)
for(i in unique(missingbin$clean_answer)){
  temprows <- as.numeric(rownames(missingbin[missingbin$clean_answer == i,]))
  master_clean_q12$clean_answer_finer[temprows] <- unique(master_driver_corrections$clean_answer_finer[master_driver_corrections$answer == i])
}
# final clean up then join to master prelimlong dataset
master_clean_q12 <- dplyr::select(master_clean_q12, -track)
# id new colnames to master dataset
addcols <- names(master_clean_q12)[!names(master_clean_q12) %in% names(prelimlong1c)]

# new version prelimlong since including the expanded vars now
prelimlong1d <- subset(prelimlong1c, qnum != "Q12") %>%
  mutate(ES = as.character(ES)) %>%
  # need to add in new cols in master_clean_q12
  cbind(matrix(ncol = length(addcols), nrow = nrow(.), dimnames = list(NULL, addcols))) %>%
  data.frame() %>%
  dplyr::select(names(master_clean_q12)) %>%
  rbind(master_clean_q12) %>%
  arrange(RecordedDate, survey_order)


# write out temp prelim cleaned so ppl can start code for analysis..
#write_csv(prelimlong1d, "round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv")

# clean up
rm(missingbin, addcols)



# 6. Add in new scale data (JL & GV) -----
# prep data
# for now need dates, ResponseId, Init can be different.. some other cols will be different 
# check for unique, consistent answers
sapply(newscale, function(x) sort(unique(x)))
# are all titles clean? (in original$Title?)
summary(newscale$Title %in% original$Title) # great
# any duplicates?
newscale$Title[duplicated(newscale$Title)] # ruh roh..
View(subset(newscale, Title %in% Title[duplicated(Title)])) # condense these, then add back in to wide df
newscale_multi <- subset(newscale, Title %in% Title[duplicated(Title)]) %>%
  group_by(Title) %>%
  summarise_all(function(x) str_flatten(unique(x[!is.na(x)]), collapse = "; ")) %>%
  ungroup() %>%
  mutate_all(function(x) ifelse(x == "", NA, x)) %>%
  mutate(Reviewed_by = "JL/GV")
# put back in main newscale df
newscale <- subset(newscale, !Title %in% newscale_multi$Title) %>%
  rbind(newscale_multi[names(.)]) %>%
  arrange(rownames(.))
# clean up
rm(newscale_multi)

newscale_tidy <- dplyr::select(newscale, -'aquatic.') %>%
  rename(Init = Reviewed_by) %>%
  gather(abbr, answer, extent_answer:ncol(.)) %>%
  # add cols in main set that don't exist
  cbind(matrix(nrow = nrow(.), ncol = length(names(prelimlong1d)[!names(prelimlong1d) %in% names(.)]),
               dimnames = list(NULL, names(prelimlong1d)[!names(prelimlong1d) %in% names(.)]))) %>%
  data.frame() %>%
  dplyr::select(names(prelimlong1d)) %>%
  # clean up: GV says NAs in extent_answer can be "Undefined" (no scale-- they are meaningful NAs)
  mutate(clean_answer = ifelse(abbr == "extent_answer" & is.na(answer), "Undefined/no scale", answer),
         # standardize macro-scale
         clean_answer = gsub("macro-sacle", "Macro-scale", clean_answer),
         # fill in the rest
         doublerev = Title %in% doubletitles)
# set date as 6/3/2020 (when GV pushed dataset)
newscale_tidy[grep("Date", names(newscale_tidy))] <- as.POSIXct("2020-06-03")

#convert col classes to match prelimlong1d col classes
for(i in names(newscale_tidy)){
  class(newscale_tidy[,i]) <- class(prelimlong1d[, i])
}

# what are the existing survey ids?
sort(unique(headerLUT$id[grepl("^Q", headerLUT$id)]))
# next number would be Q31
unique(newscale_tidy$abbr)
# survey_order for scale notes is 56.. maybe these new questions can be 56.x? to show that i'm inserting?
# Q9 = # of scale and plots.. Q8 = original multiscale question, and is survey_order 31.. maybe julie and grant questions should replace q8, and i'll just keep q9 notes..
newscale_tidy <- newscale_tidy %>%
  mutate(abbr = recode(abbr, "extent_answer" = "Extent", "extent_notes" = "ExtentNotes", "nested_answer" =  "Nested", "nested_notes" = "NestedNotes"),
         qnum = "Q8",
         survey_order = ifelse(abbr == "Extent", 31.1, ifelse(abbr == "ExtentNotes", 31.2,
                                                              ifelse(abbr == "Nested", 31.3, 31.4))),
         id = paste0("Q", survey_order),
         fullquestion = ifelse(abbr =="Extent", "Q8.1a: What is the spatial extent of the study? Select one (choose the largest extent covered, even if multiple scales are analyzed).",
                               ifelse(abbr == "ExtentNotes", "Q8.1b: Reviewer notes on spatial extent (optional).",
                                      ifelse( abbr == "Nested", "Q8.2a: Does the study consider nested or multiple spatial scales in the analysis?",
                                              "Q8.2b: Reviewer notes on nested or multiple spatial scales (optional)."))),
         qa_note = "This Q8 replaces original Qualtrics multiscale Q8, and all but reviewer notes in Q9 (# sites and # plots question)")
# ResponseId will be assigned later..

# keep ScaleNotes in Q9, but drop everything else
prelimlong1e <- rbind(prelimlong1d, newscale_tidy) %>%
  # add ordercol for ordering Titles by their original date recorded (Julie and Grant's recordeddate throws things off)
  group_by(Title) %>%
  mutate(ordercol = min(RecordedDate)) %>%
  ungroup() %>%
  arrange(ordercol, survey_order)

# out of curiousity, compare JLGV multiscale with original multiscale
ggplot(subset(prelimlong1e, survey_order %in% c(31, 31.3) & !doublerev), aes(Title, clean_answer, col = abbr, group = Title)) +
  geom_line() +
  geom_point() +
  theme(axis.text.y = element_blank()) +
  coord_flip() #every line across is where original and new don't agree.. so lots of lines

# continue cleaning
# drop original q8 and q9 except for q9 notes
prelimlong1e <- filter(prelimlong1e, !abbr %in% c("MultiScale", "Plots", "Sites")) %>%
  # append note to Q9 to indicate only original answer left
  mutate(qa_note = ifelse(abbr == "ScaleNotes", "Original Qualtrics Q9 (#sites, #plots) removed, only original reviewer notes from Q9 preserved.", qa_note),
         # add col to indicate whether original answer or final
         # > single review will always be final, anything from JL or GV will be final, but doublrev will be original or final (condensed)
         version = ifelse(!doublerev | qnum == "Q8", "final", "original")) %>%
  # reorder cols.. put after doublrev, since it mostly has to do with that?
  dplyr::select(StartDate:doublerev, version, Title:ncol(.)) %>%
  # drop certain cols that aren't needed anymore
  dplyr::select(-'order') # only applied to sites/plots question

# write out for the homeez
write_csv(prelimlong1e, "round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv")



# 7. Logic check corrections -----
# 6/8: CTW chatted with LD to confirm logic check corrections to Kremen Topics:
# > KT 1 = ESP box checked, or has density or abundance [some property of ESP or small group of species--not enough to be composition type data] (CTW: will need to think through Nick's example case)
# > KT 2 = composition, richness, diversity (property of community)
# > KT 3 = has env driver (check to see related to biotic response OR ES/EF-- basically, env driver is not related to abiotic-type bin)
# > KT 4 = spatial or temporal component (how does ES provision change through time or space?)

# assign new iteration of working df in case need to start over with logic corrections
prelimlong1f <- prelimlong1e


# 7.1. Q13: Kremen ESP ----
## > if ESP check in Biotic drivers, then ESP checked in Kremen topics
has_ESPdriver <- subset(prelimlong1f, abbr %in% c("Driver", "OtherDriver") | qnum %in% c("Q13", "Q14")) %>%
  arrange(RecordedDate) %>%
  # check for "Service Provider" in clean_answer_finer by ResponseId
  group_by(ResponseId) %>%
  mutate(Bio_answer = str_flatten(unique(clean_answer_finer[clean_group == "Bio" & !is.na(clean_answer_finer)])), # flatten all biotic driver bin labels that aren't NA
         # screen for Service provider in all Bio group coarse bins
         has_ESP = grepl("identity|abundan|densi|reproduc", Bio_answer),
         has_biodiv = grepl("diver", Bio_answer),
         # check the Kremen topics indicates ESP
         KT_ESP = grepl("Kremen Topic 1 : ESP", clean_answer[qnum == "Q13"]),
         KT = clean_answer[abbr == "KremenTopics"],
         across_spp = grepl("Across", clean_answer[abbr == "ESP_type"]),
         singlemulti_ESP = grepl("Single|Multiple", clean_answer[abbr == "ESP_type"]),
         ESP_type = clean_answer[abbr == "ESP_type"]) %>%
  # keep only conflicting records -- ESP indicated in driver but not in KT topics, or vv
  filter((has_ESP & !KT_ESP) | (!has_ESP & KT_ESP)) %>%
  # can allow KT_ESP IFF singlemulti_ESP == TRUE
  ungroup() %>%
  # keep only ResponseId, Title and flagging
  #dplyr::select(ResponseId, doublerev, Title, Bio_answer:ncol(.)) %>%
  subset(abbr %in%  c("KremenTopics", "KremenNotes", "ESP_type", "Driver", "OtherDriver") & !is.na(clean_answer)) %>%
  filter(is.na(clean_group) | clean_group == "Bio") %>%
  distinct()

# > looking at notes, it seems like people had reasons to check community structure even if only ESP checked, or to check KT2 even if measurement is abundance..
# > so best practice is to ensure X checked if y present, but not remove any KT topic (except for scale, because that's more cut and dry)
# > assume if people answered KT 1 or KT 2, they had good reason to do it
# > if driver answer is MISSING and ESP checked in KT topics, assume it probably included ESP.. give reviewer benefit of doubt.. but also see how many cases that applies to..

## RULES for KT1:
# 1) If "Single species" in Q14 and has_ESP, then KT1 should be appended
# 2) has ESP indicated, no KT1 or KT2 in Q13

addKT1 <- has_ESPdriver %>%
  group_by(ResponseId) %>%
  # either has ESP driver indicated and "Single Species" checked in Q14
  # OR has ESP driver indicated, no Response to Q14 and KT1 or KT2 not indicated
  filter((has_ESP & grepl("Single", ESP_type)) | (has_ESP & !grepl("1|2", KT))) %>%
  ungroup()

for(i in unique(addKT1$ResponseId)){
  # id row
  temprow <- which(prelimlong1f$qnum == "Q13" & prelimlong1f$ResponseId == i)
  # add KT1 -- if "None" checked change answer, otherwise append to front of answer
  if(prelimlong1f$clean_answer[temprow] == "None"){
    prelimlong1f$clean_answer[temprow] <- "Kremen Topic 1 : ESPs"
  }else{
    prelimlong1f$clean_answer[temprow] <- paste("Kremen Topic 1 : ESPs", prelimlong1f$clean_answer[temprow], sep = ",")
  }
  # add qa note
  if(is.na(prelimlong1f$qa_note[temprow])){
    prelimlong1f$qa_note[temprow] <- "Added KT1 based on biotic driver and Q14 Single Species response or non-response"
  }else{
    prelimlong1f$qa_note[temprow] <- paste0(prelimlong1f$qa_note[temprow], "; added KT1 based on biotic driver and Q14 response or non-response")
  }
}



# 7.2. Q13: Kremen Structure ----
## > if structure keyword present in drivers (or responses?), then structure checked in Kremen topics
## pull records with biodiv in clean_answer_finer or across species in ESP_type & KT2 not checked

has_commstr <- subset(prelimlong1f, abbr %in% c("Driver", "OtherDriver", "Response") | qnum %in% c("Q13", "Q14")) %>%
  arrange(RecordedDate) %>%
  # check for "Service Provider" in clean_answer_finer by ResponseId
  group_by(ResponseId) %>%
  mutate(Bio_answer = str_flatten(unique(clean_answer_finer[clean_group == "Bio" & !is.na(clean_answer_finer) & !is.na(clean_group)])), # flatten all biotic driver bin labels that aren't NA
         # screen for Service provider in all Bio group coarse bins
         has_ESP = grepl("identity|abundan|densi|reproduc", Bio_answer),
         has_biodiv = grepl("diver", Bio_answer),
         # check the Kremen topics indicates ESP
         KT_CS = grepl("Kremen Topic 2", clean_answer[qnum == "Q13"]),
         KT = clean_answer[abbr == "KremenTopics"],
         across_spp = grepl("Across", clean_answer[abbr == "ESP_type"]),
         singlemulti_ESP = grepl("Single|Multiple", clean_answer[abbr == "ESP_type"]),
         ESP_type = clean_answer[abbr == "ESP_type"]) %>%
  # keep only conflicting records -- diversity indicated in driver but not in KT topics, or vv
  # also grab any title that has "composition" in response
  filter((has_biodiv & !KT_CS) | (!has_biodiv & KT_CS)) %>%
  # can allow KT_ESP IFF singlemulti_ESP == TRUE
  ungroup() %>%
  # keep only ResponseId, Title and flagging
  #dplyr::select(ResponseId, doublerev, Title, Bio_answer:ncol(.)) %>%
  subset(abbr %in%  c("KremenTopics", "KremenNotes", "ESP_type", "Driver", "OtherDriver", "Response") & !is.na(clean_answer)) %>%
  filter(is.na(clean_group) | clean_group == "Bio") %>%
  distinct()

# RULES FOR KT2:
# 1) has_biodiv in driver bin and no KT1
# 2) across_spp & no KT2 [no cases! anyone who checked across also checked KT 2]
addKT2 <- subset(has_commstr, (has_biodiv & !grepl("1", KT)) | (across_spp & !KT_CS))
KT2answer <- "Kremen Topic 2 : Community structure influencing function"

for(i in unique(addKT2$ResponseId)){
  # id row
  temprow <- which(prelimlong1f$qnum == "Q13" & prelimlong1f$ResponseId == i)
  # add KT2 -- KT1 present, add behind otherwise append to front of answer
  if(grepl("1", prelimlong1f$clean_answer[temprow])){
    prelimlong1f$clean_answer[temprow] <- gsub("ESPs|ESPs,", paste0("ESPs,", KT2answer), prelimlong1f$clean_answer[temprow])
  }else{
    # append to front
    prelimlong1f$clean_answer[temprow] <- paste(KT2answer, prelimlong1f$clean_answer[temprow], sep = ",")
  }
  # clean up NA or None
  prelimlong1f$clean_answer[temprow] <- gsub(",NA|,None", "", prelimlong1f$clean_answer[temprow])
  
  # add qa note
  if(is.na(prelimlong1f$qa_note[temprow])){
    prelimlong1f$qa_note[temprow] <- "Added KT2 based on biodiversity biotic driver and no KT1 or KT2 checked"
  }else{
    prelimlong1f$qa_note[temprow] <- paste0(prelimlong1f$qa_note[temprow], "; added KT2 based on biodiversity biotic driver and no KT1 or KT2 checked")
  }
}

View(subset(prelimlong1f, ResponseId %in% addKT2$ResponseId))

# in these cases, KT2 doesn't seem appropriate:
# 1) No ESP indicated, no biodiv indicated, and no across spp in ESP type .. but wait for response bins to be complete.. bc I guess KT2 could be checked based on response?
removeKT2 <- subset(has_commstr, !has_ESP & !has_biodiv) # looking at response variables.. I guess best thing to do is leave as is?



# 7.3. Q13: Kremen Environment ----
## > if environmental driver listed (either one of the canned answers or other AND other described), then env check in Kremen topics
## > if other described but other not checked, review (most likely errors looking at the review file)
## > also need to repeat screen because envcheck csv subsetted kremen notes, which only pulled records that had notes..

# I don't even know if env factors can be logic checked bc revisiting kremen it says it has to do with env factors' influence on ESPs that in turn affect levels of ES provision..
# so the ESP would need to be in the response, env in the driver..
# > LD says to interpret environment more loosely .. if study included env driver, then KT env should be checked. simple rule.

has_env <- subset(prelimlong1f, abbr %in% c("Driver", "OtherDriver") | qnum %in% c("Q13", "Q14")) %>%
  arrange(RecordedDate) %>%
  # check for "Service Provider" in clean_answer_finer by ResponseId
  group_by(ResponseId) %>%
  mutate(env_answer = str_flatten(unique(clean_answer_finer[clean_group == "Env" & !is.na(clean_answer_finer) & !is.na(clean_group)])), # flatten all biotic driver bin labels that aren't NA
         # screen for Service provider in all Bio group coarse bins
         has_env = env_answer != "",
         KT = clean_answer[abbr == "KremenTopics"],
         KT_env = grepl("3", KT)) %>%
  filter((has_env & !KT_env) | (KT_env & !has_env)) %>%
  ungroup() %>%
  # keep only ResponseId, Title and flagging
  #dplyr::select(ResponseId, doublerev, Title, Bio_answer:ncol(.)) %>%
  subset(abbr %in%  c("KremenTopics", "KremenNotes", "ESP_type", "Driver", "OtherDriver", "Response") & !is.na(clean_answer)) %>%
  #filter(is.na(clean_group)) %>%
  distinct()

# RULES FOR KT ENV (this is more clear cut)
# 1) has env driver (has_env) but Env not checked in KT Topics (!KT_env)
# 2) does NOT have env driver (!has_env) but Env check in KT Topics (KT_env)

add_KTenv <- unique(with(has_env, ResponseId[has_env & !KT_env]))
remove_KTenv <- unique(with(has_env, ResponseId[!has_env & KT_env]))
KT3answer <- "Kremen Topic 3 : Environmental factors that influence provision (env. drivers, not including human drivers))"
for(i in add_KTenv){
  # id row
  temprow <- which(prelimlong1f$qnum == "Q13" & prelimlong1f$ResponseId == i)
  # add KT3 -- if "None" checked change answer, otherwise append end or before KT 4
  if(grepl("4", prelimlong1f$clean_answer[temprow])){
    prelimlong1f$clean_answer[temprow] <- gsub("Kremen Topic 4", paste0(KT3answer, ",Kremen Topic 4"), prelimlong1f$clean_answer[temprow])
  }else{
    # append to back because will be comma split anyway
    prelimlong1f$clean_answer[temprow] <- paste(prelimlong1f$clean_answer[temprow], KT3answer, sep = ",")
  }
  # clean up NA or None if needed
  prelimlong1f$clean_answer[temprow] <- gsub("NA,|None,", "", prelimlong1f$clean_answer[temprow])
  #clean up double commas
  prelimlong1f$clean_answer[temprow] <- gsub(",,", ",", prelimlong1f$clean_answer[temprow])
  
  # add qa note
  if(is.na(prelimlong1f$qa_note[temprow])){
    prelimlong1f$qa_note[temprow] <- "Added KT3 based on environmental driver and no KT3 checked"
  }else{
    prelimlong1f$qa_note[temprow] <- paste0(prelimlong1f$qa_note[temprow], "; added KT3 based on environmental driver and no KT3 checked")
  }
}

View(subset(prelimlong1f, ResponseId %in% add_KTenv))

# remove KT env if no env driver provided
for(i in remove_KTenv){
  # id row
  temprow <- which(prelimlong1f$qnum == "Q13" & prelimlong1f$ResponseId == i)
  # remove KT3 -- if only answer is KT3 change to None, otherwise gsub out
  if(prelimlong1f$clean_answer[temprow] == KT3answer){
    prelimlong1f$clean_answer[temprow] <- "None"
  }else{
    # gsub out
    prelimlong1f$clean_answer[temprow] <- gsub(KT3answer, "", prelimlong1f$clean_answer[temprow], fixed = T)
  }
  #clean up double commas or end comma
  prelimlong1f$clean_answer[temprow] <- gsub(",,", ",", prelimlong1f$clean_answer[temprow])
  prelimlong1f$clean_answer[temprow] <- gsub(",$", "", prelimlong1f$clean_answer[temprow])
  
  # add qa note
  if(is.na(prelimlong1f$qa_note[temprow])){
    prelimlong1f$qa_note[temprow] <- "Removed KT3 based on lack of environmental driver"
  }else{
    prelimlong1f$qa_note[temprow] <- paste0(prelimlong1f$qa_note[temprow], "; removed KT3 based on lack of environmental driver")
  }
}

View(subset(prelimlong1f, ResponseId %in% remove_KTenv & abbr == "KremenTopics"))


copydf <- prelimlong1f

# 7.4. Q13: Kremen Scale ----
## > if Multiscale == "Yes", then scale checked in Kremen topics
## > note, 5/21: waiting on Grant and Julie to confirm Multiscale reliable question for triggering correction (CTW sent email)

# this feels like the only question i can actually logic check, based on GVJL new scale answer and how reviewer answered time
scalecheck <- subset(prelimlong1f, abbr %in% c("TimeTrends", "KremenTopics")) %>%
  dplyr::select(StartDate:Title, clean_answer, abbr) %>%
  distinct() %>%
  spread(abbr, clean_answer) %>%
  left_join(distinct(newscale[c("Title", "nested_answer")])) %>%
  group_by(ResponseId) %>%
  # if time or nested == YES, then Topic 4: Scale should be present
  # > # if KremenTopics is NA because reviewer excluded paper [doublereviewed, but LD judged allow] then both checks should be NA
  mutate(needs_scale = ifelse(is.na(KremenTopics), NA, grepl("Yes", TimeTrends) | grepl("Yes", nested_answer)),
         has_scale = ifelse(is.na(KremenTopics), NA, grepl("Topic 4", KremenTopics))) %>%
  # filter to inconsistent answers
  filter((needs_scale & !has_scale) | (!needs_scale & has_scale)) %>%
  ungroup()

# go in and correct Kremen Topic 4
for(i in scalecheck$ResponseId){
  temp_note <- prelimlong1f$qa_note[prelimlong1f$abbr == "KremenTopics" & prelimlong1f$ResponseId == i]
  # if need_scale is TRUE, means does not have KT 4 and needs it
  if(scalecheck$needs_scale[scalecheck$ResponseId == i]){
    prelimlong1f$clean_answer[prelimlong1f$abbr == "KremenTopics" & prelimlong1f$ResponseId == i] <- ifelse(with(scalecheck, KremenTopics[ResponseId == i]) == "None", 
                                                                                                            "Kremen Topic 4 : Scale", paste0(with(scalecheck, KremenTopics[ResponseId == i]), ",Kremen Topic 4 : Scale"))
    # add QA note
    prelimlong1f$qa_note[prelimlong1f$abbr == "KremenTopics" & prelimlong1f$ResponseId == i] <- ifelse(is.na(temp_note), "Appended KT 4:Scale (has multi time or spatial scale but KT 4 not checked)", 
                                                                                                       paste0(temp_note, "; appended KT 4:Scale (has multi time or spatial scale but KT 4 not checked)"))
  }else{
    # needs KT 4 removed (needs scale is not true)
    prelimlong1f$clean_answer[prelimlong1f$abbr == "KremenTopics" & prelimlong1f$ResponseId == i] <- gsub(",?Kremen Topic 4 : Scale", "", with(scalecheck, KremenTopics[ResponseId == i]))
    # add QA note
    prelimlong1f$qa_note[prelimlong1f$abbr == "KremenTopics" & prelimlong1f$ResponseId == i] <- ifelse(is.na(temp_note), "Removed KT 4:Scale (does not have multi time or spatial scale but KT 4 checked)", 
                                                                                                       paste0(temp_note, "; removed KT 4:Scale (does not have multi time or spatial scale but KT 4 checked)"))
  }
}

# clean up
rm(temp_note, i, scalecheck)

# clean up env driver answer so that comma split function doesn't split answer
prelimlong1f$clean_answer <- gsub("drivers, not including human drivers))", "drivers not including human drivers)", prelimlong1f$clean_answer)

# write out working csv
write_csv(prelimlong1f, "round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv")



# 7.5. Q14: Service Providers ----
# > note: we should have had a "how many species in this study" question..
# within = "genetic"
# single.. hard to pull by keywords
# multiple ESPs.. also tough
# across = "structure|diversity" <-- this is not always true tho
# among = "interact"
# only land cover .. not sure how to screen for this one either..

# CAN look for responses with no answer and infill none? or maybe should leave NA with flag.. see how many papers it is, maybe need to write out to have ppl answer

check_ESPtype <- subset(prelimlong1f, qnum %in% c("Q14", "Q13") | grepl("Driver|Response", abbr)) %>%
  arrange(RecordedDate) %>%
  # check for "Service Provider" in clean_answer_finer by ResponseId
  group_by(ResponseId) %>%
  mutate(Bio_answer = str_flatten(unique(clean_answer_finer[clean_group == "Bio" & grepl("Driver", abbr) & !is.na(clean_answer_finer)])), # flatten all biotic driver bin labels that aren't NA
         Response_answer = str_flatten(unique(clean_answer_finer[grepl("Response", abbr) & !is.na(clean_answer_finer)])),
         # screen for Service provider in all Bio group coarse bins
         has_ESPdriver = grepl("identity|abundan|densi|reproduc", Bio_answer),
         has_biodiv = grepl("diver", Bio_answer),
         missing_ESPtype = is.na(clean_answer[abbr == "ESP_type"]),
         ESP_type = clean_answer[abbr == "ESP_type"],
         KT = clean_answer[abbr == "KremenTopics"],
         KT_ESP = grepl("1", KT),
         KT_CS = grepl("2", KT)) %>%
  ungroup() %>%
  subset(abbr == "KremenNotes")
# assign no ESP in NA answers so it's clear .. but do confirm no ESP indicated in those papers




# -- CONDENSE DOUBLE REVIEWED ----
# add "unified" record to to double reviews
doubleprelim <- subset(prelimlong1f, Title %in% doubletitles) %>%
  group_by(Title, id) %>%
  mutate(same_answer = length(unique(clean_answer)) == 1) %>%
  ungroup() %>%
  filter(!(same_answer & is.na(clean_answer))) %>%
  arrange(Title, survey_order, RecordedDate)
# how many double reviewed?
length(unique(doubleprelim$Title))
# who?
sort(sapply(split(doubleprelim$Title, doubleprelim$Init), function(x) length(unique(x))))
write_csv(doubleprelim, "round2_metareview/data/intermediate/round2_doublereviewed_tidy.csv")

# where are the most inconsistencies (by question)?
# factor abbr by survey order
doubleprelim %>%
  mutate(abbr = factor(abbr, levels= unique(abbr[order(survey_order)]))) %>%
  # remove exclusion question since these are all kept papers
  subset(qnum != "Q3") %>%
  dplyr::select(Title, abbr, same_answer) %>%
  distinct() %>%
  ggplot(aes(same_answer)) +
  geom_bar() +
  facet_wrap(~abbr) +
  ggtitle(paste0("Round 2: double-reviewed kept papers (n=", length(unique(doubleprelim$Title)),"),\nagreement in answers by question, ", Sys.Date()))
ggsave("round2_metareview/clean_qa_data/qafigs/r2qa_doublereview_congruency.pdf",
       width = 5, height = 5, units = "in")

# see what can be dissolved..
## > any text field can be combined (e.g. paste(notes from rev1, notes from rev2))
## > if multi-choice answers are nested (e.g. rev1 = [a], rev2 = [a,b]), dissolve answer (we said as a group this would be okay rule)
## > if multi-choice answers do not agree (not nested).. kick back to outside reviewer or original reviewers

# screen for congruency in ES 
ES_check <- subset(doubleprelim, qnum == "Q12" & abbr == "Response") %>%
  dplyr::select(RecordedDate, ResponseId, Init, Title, ES) %>%
  distinct() %>%
  arrange(RecordedDate, ES) %>%
  group_by(ResponseId) %>%
  mutate(allES = str_flatten(unique(ES))) %>%
  ungroup() %>%
  group_by(Title) %>%
  mutate(sameanswer = length(unique(allES)) == 1) %>%
  ungroup() %>%
  arrange(Title)
summary(ES_check$sameanswer) # pretty amazing all ES's by double reviewers agree. good!

# clean up
rm(doubleprelim, ES_check)

# go by question and condense into master df OR append to df for reviewers to re-review
# refer to rev 1 answer as primary answer..
# assign response id to unified titles.. based on alphabetic title to keep things simple (doesn't really matter)
keptdoubles <- data.frame(Title = sort(doubletitles)) %>%
  # only keep papers note excluded in r2
  filter(!Title %in% r2excluded_final$Title) %>%
  left_join(original[c("Title", "Round2_reviewer1")]) %>%
  left_join(data.frame(rev1init = initials, Name = names(initials)), by = c("Round2_reviewer1" = "Name")) %>%
  left_join(original[c("Title", "Round2_reviewer2")]) %>%
  left_join(data.frame(rev2init = initials, Name = names(initials)), by = c("Round2_reviewer2" = "Name")) %>%
  # drop full first names
  dplyr::select(-c(Round2_reviewer1, Round2_reviewer2)) %>%
  # finally, assign new response id
  mutate(new_rid = paste0("R", 1:nrow(.), "unified"))
# need to correct for papers KCG reviewed (shifted assignment during review)

KCGpapers <- subset(prelimlong1f, Init == "KCG") %>%
  dplyr::select(Title, Init) %>%
  distinct()

keptdoubles <- left_join(keptdoubles, KCGpapers) %>%
  # Kathryn replaced Julie (rev2) on double-review
  mutate(rev2init = ifelse(!is.na(Init), Init, rev2init)) %>%
  # drop Init col
  dplyr::select(-Init)



# 1. Collapse inconsisent exclusions reviewed by LD -----
# specify date cols to avoid repeat typing
datecols <- c("StartDate", "EndDate", "RecordedDate")

# deal with papers that were kept after LD's review but reviewers disagreed (those are easy to collapse)
Q3doubles <- subset(prelimlong1f, Title %in% Title[answer == "Yes" & qnum == "Q3"]) %>%
  # drop scale question (will add back in at the end)
  subset(qnum != "Q8") %>%
  # easiest thing to do is probably ID the person who did not exclude paper and keep their answers
  group_by(Title) %>%
  mutate(keepInit = unique(Init[!is.na(clean_answer) & abbr == "Ecosystem"])) %>%
  ungroup()
# > want to append who initially said exclude, and who said keep, and on which of the q3 questions, and preserve note that LD cleared it
# > after that can collapse remove (NA answers)

q3rows <- subset(Q3doubles, qnum == "Q3" & answer == "Yes") %>%
  dplyr::select(ResponseId, Title, Init, id, answer, clean_answer, abbr, qa_note, keepInit) %>%
  # edit qa_note
  group_by(ResponseId) %>%
  mutate(qa_note = paste("Double reviewed and inconsistent answers.", Init, "answered 'Yes' to ", abbr, "exclusion question but", keepInit, "answered 'No'.", qa_note),
         # add period to end
         qa_note = paste0(qa_note, "."))
# replace qa_note
for(i in unique(q3rows$Title)){
  # id pertinent rows in Q3doubles
  temprows <- which(Q3doubles$Title == i & Q3doubles$abbr == q3rows$abbr[q3rows$Title == i])
  Q3doubles$qa_note[temprows] <- q3rows$qa_note[q3rows$Title == i]
}

# subset to answers from keepInit
Q3doubles <- filter(Q3doubles, keepInit == Init | qnum == "Q8")
for(i in unique(q3rows$Title)){
  # replace Init (not for Q8, which is new scale questions) and ResponseId
  Q3doubles$Init[Q3doubles$Title == i & Q3doubles$qnum != "Q8"] <- paste(keptdoubles$rev1init[keptdoubles$Title == i], keptdoubles$rev2init[keptdoubles$Title == i], sep = "/")
  Q3doubles$ResponseId[Q3doubles$Title == i] <- keptdoubles$new_rid[keptdoubles$Title == i]
}
# NA dates
Q3doubles[, datecols] <- NA
# change version to final
Q3doubles$version <- "final"


# 2. Collapse all else: notes fields -----
# notes get combined..
# scale notes already single reviewed by JL + GV
# > generally, Q8 can just be taken out then added back in since only single rows for each
# double notes that need to be collapsed are abbr %in% c("GenInfo", "ScaleNotes", "KremenNotes", "SurveyNotes)
notesfields <- c("EcosystemNotes", "GenInfo", "ScaleNotes", "KremenNotes", "Uncertainty", "SurveyNotes")
doublenotes <- subset(prelimlong1f, abbr %in% notesfields & Title %in% doubletitles) %>%
  # take out Q3doubles dealt with above
  filter(!Title %in% unique(Q3doubles$Title)) %>%
  #first, both answer and clean_answer need reviewer init appended in front
  # qa_notes present scale notes (which are the same for all, because that was the only subquestion kept from the original q9) and sometimes Q6
  mutate(answer = ifelse(!is.na(answer), paste(Init, answer, sep = ": "), answer),
         qa_note = ifelse(!is.na(qa_note) & qnum != "Q9", paste(Init, "review only:", qa_note, sep = " "), qa_note)) %>%
  left_join(keptdoubles) %>%
  mutate(initorder = ifelse(rev1init == Init, 1, 2)) %>%
  arrange(Title, survey_order, initorder) %>%
  group_by(Title, abbr) %>%
  mutate(clean_answer = str_flatten(answer[!is.na(answer)], collapse = "; "),
         qa_note = str_flatten(qa_note[!is.na(qa_note)], collapse = "; ")) %>%
  ungroup() %>%
  mutate(Init = paste(rev1init, rev2init, sep = "/"),
         ResponseId = new_rid,
         answer = clean_answer,
         version = "final") %>%
  mutate_at(vars(datecols), function(x) x <- as.character(Sys.time())) %>%
  dplyr::select(StartDate:ordercol) %>%
  distinct() %>%
  mutate_at(vars("answer", "clean_answer", "qa_note"), function(x) ifelse(x == "", NA, x))




# 3. Collapse all else: non-notes fields -----
# > may need to triage Q12 separate from the rest??
# > or go by question? (and make a function?)

#q8 won't be an issue so could break that out and just deal with other q's that aren't notes
doublemulti <- subset(prelimlong1f, qnum != "Q8" & Title %in% doubletitles[!doubletitles %in% Q3doubles$Title]) %>%
  # remove notes fields since those are already cleaned
  subset(!abbr %in% notesfields)

# need to go by question because each consolidation (answer prioritization) will be slightly unique to that question
# 3.1 Exclusion question----
# all of these are No because paper kept, so just need to fix reviewer, rid, date cols, and version
q3no_doubles <- subset(doublemulti, qnum == "Q3") %>%
  left_join(keptdoubles) %>%
  # make clean_answer No for all bc should be
  # > some Review Only q's are NA from both reviewers, but if this were a problem would have got pulled.. can add a qa_note all the same
  mutate(rev1 = ifelse(rev1init == Init, 1, 2),
         # append init to answer (same as clean_answer for now)
         answer = ifelse(!is.na(answer), paste(Init, answer, sep = ": "), answer)) %>%
  arrange(Title, survey_order, rev1) %>%
  group_by(Title, abbr) %>%
  mutate(clean_answer2 = ifelse(length(unique(clean_answer[!is.na(clean_answer)]))>0, unique(clean_answer[!is.na(clean_answer)]), NA),
         answer = ifelse(is.na(clean_answer2), unique(clean_answer2), str_flatten(answer[!is.na(answer)], collapse = "; "))) %>%
  ungroup() %>%
  # add qa note if infilling double NA with no
  mutate(qa_note = ifelse(is.na(clean_answer2), "For both reviewers: Infilled 'No'; question not yet created in survey when reviewers completed but nothing in answers to indicate paper should be excluded", qa_note),
         # assign new inits, rids, times
         Init = paste(rev1init, rev2init, sep = "/"),
         ResponseId = new_rid,
         version = "final",
         # fill all
         clean_answer = ifelse(!is.na(clean_answer2), clean_answer2, "No")) %>%
  # make all times for write out the same, so NA for now -- can also assign "final" to version at the end
  mutate_at(vars(StartDate, EndDate, RecordedDate), function(x) x <- NA) %>%
  dplyr::select(StartDate:qa_note) %>%
  distinct()


# 3.2 Collapse all else multi-choice (not Q12-Q14) ----- 
# might be best to first ID clean_answers that are inconsistent (everything together), 
# then separate ready to go from inconsistent
# then apply nested dissolve
# then write out anything that still needs review, sorted by paper (so that ppl volunteering to triage can be responsible for paper)
# then clean all else that's ready to go
# q12 will likely still need to be handled separately .. but Q13 and Q14 dependent on what was entered in Q12

doublemulti_base <- subset(doublemulti, !abbr %in% notesfields & !qnum %in% c("Q3", "Q12")) %>%
  group_by(Title, abbr) %>%
  # ignore NAs
  mutate(sameanswer =  ifelse(abbr %in% c("YrsData", "ConnectDist"), # these are follow up questions to TimeTrends and Connect -- if one has answer and other NA, can't overwrite NA
                              length(unique(clean_answer)) == 1,
                              # treat all else (ignore NAs -- can be overwritten with available answer if exists)  
                              ifelse(!any(is.na(clean_answer)), # if neither answer is NA
                                     length(unique(clean_answer))==1, # are they the same answer?
                                     TRUE))) %>% # otherwise, either both are NA or 1 is NA, in which case default to whichever row has answer
  ungroup() %>%
  left_join(keptdoubles) %>%
  mutate(revorder = ifelse(rev1init == Init, 1, 2),
         clean_answer2 = NA)

length(unique(doublemulti_base$Title[!doublemulti_base$sameanswer])) # 27/31 papers have some sort of inconsistency, not including Q12-Q14..
# where are the most inconsistences?
group_by(doublemulti_base, abbr, sameanswer) %>%
  summarise(nobs = length(unique(Title))) %>%
  ggplot(aes(sameanswer, nobs)) +
  geom_col() +
  facet_wrap(~abbr)

# 3.2.a Dissolve inconsistent (if possible) -----
# pull out what's different
doublemulti_inconsistent <- subset(doublemulti_base, !sameanswer)

# need to go through and collapse nested answers, by abbr..
# but the only questions I'll be able to do that for is methods and ecosystem.. all else is more binary
doublemulti_notsame_metheco <- subset(doublemulti_inconsistent, abbr %in% c("Methods", "Ecosystem"))


doublemulti_dissolved <- data.frame()
for(i in unique(doublemulti_notsame_metheco$Title)){
  # iterate by title
  tempdat <- subset(doublemulti_notsame_metheco, Title == i)
  
  # iterate by question
  for(a in unique(tempdat$abbr)){
    tempdat2 <- subset(tempdat, abbr ==a)
    # check for nestedness first
    tempanswer <- c(grepl(tempdat2$clean_answer[2], tempdat2$clean_answer[1], fixed = T), grepl(tempdat2$clean_answer[1], tempdat2$clean_answer[2], fixed = T))
    if(any(tempanswer)){
      tempdat2$clean_answer2 <- tempdat2$clean_answer[which(tempanswer==TRUE)]
      # update qa note for whatever row was dissolved
      tempdat2$qa_note[which(!tempanswer)] <- paste0(tempdat2$qa_note[which(!tempanswer)], "; dissolved ", tempdat2$Init[which(!tempanswer)], " clean answer into ", tempdat2$Init[which(tempanswer)], " clean answer")
      # clean up NA
      tempdat2$qa_note[which(!tempanswer)] <- gsub("NA; d", "D", tempdat2$qa_note[which(!tempanswer)])
      doublemulti_dissolved <- rbind(doublemulti_dissolved, tempdat2)
    }else{
      # if not nested, prioritize answer looked at by AK/ND or LD/IS (if there)
      if(any(!is.na(tempdat2$qa_note))){
        # check for multiple reviews
        stopifnot(sum(!is.na(tempdat2$qa_note))==1)
        # go with answer that has qa_note
        tempdat2$clean_answer2 <- tempdat2$clean_answer[!is.na(tempdat2$qa_note)]
        tempdat2$qa_note[is.na(tempdat2$qa_note)] <- ifelse(a == "Methods","Assign answer reviewed by ND+ÀK",
                                                            "Assign answer reviewed by LD+IS")
        doublemulti_dissolved <- rbind(doublemulti_dissolved, tempdat2)
      }
    }
  }
}
#clean up
rm(tempdat, tempdat2,i,a, doublemulti_notsame_metheco)
# drop whatever was cleand up from doublemulti_inconsistent
doublemulti_inconsistent <- anti_join(doublemulti_inconsistent, doublemulti_dissolved[c("ResponseId", "abbr")])
# clean up dissolved
doublemulti_dissolved <- doublemulti_dissolved %>%
  # need to assign original clean_answer to answer with inits
  mutate(clean_answer = paste(Init, clean_answer, sep = ": "),
         # add inits to qa_note
         qa_note = ifelse(!is.na(qa_note), paste0("For ", Init, " review only: ", qa_note), qa_note)) %>%
  group_by(Title, abbr) %>%
  # then collapse each, assign clean_answer2 to clean_answer
  mutate(answer = str_flatten(clean_answer, collapse = "; "),
         qa_note = str_flatten(qa_note[!is.na(qa_note)], collapse = "; ")) %>%
  ungroup() %>%
  # assign new_rid, NA dates, combine inits
  mutate(clean_answer = clean_answer2, 
         ResponseId = new_rid,
         Init = paste(rev1init, rev2init, sep = "/"),
         version = "final") %>%
  mutate_at(vars(datecols), function(x) x <- NA) %>%
  # drop cols then winnow to unique rows
  dplyr::select(StartDate:qa_note) %>%
  distinct()



# 3.2b Collapse consistent answers -----
# prep newscale questions to rbind
newscale_double <- subset(newscale_tidy, Title %in% keptdoubles$Title) %>%
  left_join(keptdoubles) %>%
  mutate(version = "final",
         ResponseId = new_rid)

# collapse double reviewed answers that agree
doublemulti_consistent <- subset(doublemulti_base, sameanswer) %>%
  # append inits to clean_answer and qa_note (if present)
  # then assigned collapsed clean_answer to "answer" and final (no inits) to clean_answer
  # order of answer and qa_note determined by order of reviewer (1 or 2)
  # also assign new rid and NA date cols
  group_by(Title, abbr) %>%
  mutate(clean_answer2 = unique(clean_answer)) %>%
  ungroup() %>%
  mutate(clean_answer = ifelse(!is.na(clean_answer), paste(Init, clean_answer, sep = ": "), clean_answer),
         qa_note = ifelse(!is.na(qa_note), paste0("For ", Init, " review only: ", qa_note), qa_note),
         Init = paste(rev1init, rev2init, sep = "/"),
         ResponseId = new_rid) %>%
  mutate_at(vars(datecols), function(x) x <- NA) %>%
  group_by(Title, abbr) %>%
  mutate(answer = str_flatten(clean_answer, collapse = "; "),
         qa_note = str_flatten(qa_note, collapse = "; ")) %>%
  ungroup() %>%
  mutate(clean_answer = clean_answer2,
         version = "final") %>%
  dplyr::select(StartDate:qa_note) %>%
  distinct() %>%
  #append dissolved answers
  rbind(doublemulti_dissolved) %>%
  # add cleaned up Q3
  rbind(Q3doubles[names(.)]) %>%
  rbind(q3no_doubles[names(.)]) %>%
  # add cleaned up notes
  rbind(doublenotes[names(.)]) %>%
  # add sys time
  mutate_at(vars(datecols), function(x) x = Sys.time()) %>%
  #rbind new scale double reviewed papers
  rbind(newscale_double[names(.)]) %>%
  arrange(Title, survey_order)



# 3.3 Write out Q12, inconsistent answers that need review still -----
Q12doubles <- subset(doublemulti, qnum %in% c("Q12")) %>%
  # sort clean answer alphabetically within question for easier comparison
  arrange(Title, survey_order, clean_answer) %>%
  # only keep records where at least one answer is not NA
  group_by(Title, survey_order) %>%
  mutate(hasanswer = length(answer[!is.na(answer)])>0,
         sameanswer = length(unique(clean_answer))==1 | length(unique(answer))==1) %>%
  subset(hasanswer) %>%
  left_join(keptdoubles) %>%
  data.frame()
# drop cols not needed for review
# > can drop dates, version, ordercol, varnum, ESnum, hasanswer, new_rid
# and move clean_answer2 after clean_answer.. call reviewed_answer or something

double_inconsistent_all <- doublemulti_inconsistent %>%
  dplyr::select(ResponseId:doublerev, Title:clean_group, qnum:qa_note, sameanswer:rev2init) %>%
  rbind(Q12doubles[names(.)]) %>%
  mutate(revorder = ifelse(Init == rev1init, 1, 2)) %>%
  distinct() %>%
  arrange(Title, survey_order, revorder) %>%
  #drop varnum, clean_answer_finer, rev1 and 2 inits -- drop double rev too because they're all doublerev
  dplyr::select(-c(doublerev, varnum, clean_answer_finer)) %>%
  # add col for final_answer and reviewer_notes
  mutate(final_answer = NA,
         review_notes = NA) %>%
  # move same answer to near double rev
  dplyr::select(ResponseId:id, sameanswer, answer:clean_answer, final_answer, review_notes, fullquestion:ncol(.))

# what's the breakdown by person?
sort(sapply(split(double_inconsistent_all$Title, double_inconsistent_all$Init), function(x) length(unique(x))))
dbl_init <- sort(unique(double_inconsistent_all$Init))
# remove CK and AIS
dbl_init <- dbl_init[!dbl_init %in% c("AIS", "CK")]
# write out by people who are responsive.. they can figure out how to divvy out work
# Laura (paired with Tim), Aislyn (Anna and Grant), Grant (AK and Sierra), Julie (Tim and Claire), Kathryn (Tim), Sierra (Grant)
# send Tim and Sierra all of theirs.. but include in Laura, Kathryn, Julie and Grant's (let them decide how to manage)

for(i in dbl_init){
  tempdat <- subset(double_inconsistent_all, rev1init == i | rev2init == i)
  write_csv(tempdat,paste0("round2_metareview/clean_qa_data/needs_classreview/doublerev_inconsistent/doublerev_inconsistent_", i,".csv"), na = "")
}

# 




# -- APPLY CORRECTIONS TO DOUBLE REVIEWED -----



# -- WRITE OUT CLEANED L1 DATASET FOR POST-PROCESSING AND ANALYSIS -----
# write out data as it's ready for others to work on.. (e.g. Q12 matrix will probably come later)
# temp add scale in double revs not yet cleaned for grant


# stack final, clean single reviews and original doubles, and cleaned up doubles
# remove Q8 from double review original answers because will be in final
# > note.. Julie and Grant answers for single review don't have a response id.. infill yay or nay?
clean_master <- subset(prelimlong1f, !(doublerev & qnum == "Q8")) %>%
  # drop ordercol
  dplyr::select(-ordercol) %>%
  # infill ResponseId for single review JL/GV scale answers
  fill(ResponseId) %>%
  #rbind(Q3doubles[names(prelimlong1f)]) %>%
  # add consistent double reviewed papers
  rbind(doublemulti_consistent[names(.)]) %>%
  # arrange by Title, then survey_order then RecordedDate?
  arrange(Title, survey_order, RecordedDate) %>%
  # double check single ResponseId per title (except for original double revs)
  group_by(Title) %>%
  mutate(ridcheck = length(unique(ResponseId))) %>%
  ungroup() %>%
  data.frame()

sapply(split(clean_master$ridcheck, clean_master$doublerev), unique) # 2 for double reviews that aren't processed yet, and 3 for ones that are..
# check stucture
str(clean_master) # can drop ordercol and ridcheck, as well as order (obsolete-- only applied to old scale question)

# drop rid check and other cols not needed
head(clean_master[,!names(clean_master) %in% c("ordercol", "ridcheck", "varnum", "ESnum")])

# write out
write_csv(clean_master[,!names(clean_master) %in% c("ordercol", "ridcheck", "varnum", "ESnum")], "round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv")



# -- PRELIM SUMMARY FIGURE ----

select(q12df, Init, Title, ES) %>%
  filter(!is.na(ES)) %>%
  distinct() %>%
  ggplot(aes(ES)) +
  geom_bar() +
  coord_flip()

ytypefig <- select(q12df, Init, Title, ES, abbr, answer) %>%
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

test <- select(q12df, Init, Title, ES, abbr, Group, answer) %>%
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
