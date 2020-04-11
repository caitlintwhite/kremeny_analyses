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
  mutate_all(function(x) ifelse(is.na(x), "", x))
write.csv(forAK, "round2_metareview/data/intermediate/round2_doublereviewed.csv", row.names = F)

# pull titles for screening further down
doubletitles <- unique(forAK$clean_title)
# how many papers double-reviewed?
length(doubletitles)

# clean up environment
rm(doubles, qualtrix, unwanted, title_df, needs_match, available_titles, https)



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






# -- TIDY DATA + ASSESS REVIEW STATUS ----
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
              "Grant" = "GV", "Isabel" = "IS", "Julie" = "JL", "Kathryn" = "KG",
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
  labs(title = paste("Round 2 progress:", length(unique(keydf2$Title)), "of", length(original$Title), "unique papers reviewed on", Sys.Date()),
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
ggsave("round2_metareview/clean_qa_data/figs/reviewstatus.pdf", 
       height = 6, width = 6, units = "in", scale = 1.5)
# what remains?
outstanding <- subset(original, !Title %in% unique(keydf$Title))
# who remains to reach 28 papers
effort <- data.frame(Init = unname(initials), Name = names(initials)) %>%
  left_join(data.frame(table(keydf$Name)), by = c("Name" = "Var1")) %>%
  # if count is NA, assign 0
  replace_na(list(Freq = 0)) %>%
  # add rev 2
  left_join(distinct(original[,1:2]), by = c("Name" = "Round2_reviewer1")) %>%
  group_by(Name) %>%
  mutate(pair = paste0("pair",1:2)) %>%
  ungroup() %>%
  spread(pair, Round2_reviewer2) %>%
  arrange(desc(Freq))

# make tidy dataset
prelimlong <- prelim %>%
  # select cols to keep (drop Q1 -- old/incorrect title)
  dplyr::select(StartDate, EndDate, RecordedDate, Q27, clean_title:ncol(.)) %>%
  gather(id, answer, Q2:ncol(.)) %>%
  left_join(headerLUT) %>%
  arrange(RecordedDate) %>%
  rename(Init = Q27, Title = clean_title)


# ---- LOOP TO SUSBET PAIRED REVIEWERS <----- only run this one time (pre spring bring)
# pairs <- distinct(original[,1:2])

# unique(prelimlong$Init)
# 
# reviewlist <- data.frame()
# for(r in 1:nrow(pairs)){
#   temp <- subset(original, (Round2_reviewer1 == pairs$Round2_reviewer1[r] & Round2_reviewer2 == pairs$Round2_reviewer2[r]) |
#                    (Round2_reviewer1 == pairs$Round2_reviewer2[r] & Round2_reviewer2 == pairs$Round2_reviewer1[r])) %>%
#     select(Round2_reviewer1, Round2_reviewer2, FirstAuthor, Title, SourcePublication, PublicationYear) %>%
#     mutate(reviewed1 = Title %in% unique(prelimlong$Title[prelimlong$Init == initials[pairs$Round2_reviewer1[r]]]),
#            reviewed2 = Title %in% unique(prelimlong$Title[prelimlong$Init == initials[pairs$Round2_reviewer2[r]]]))
#     names(temp)[names(temp) == "reviewed1"] <- paste0(initials[[pairs$Round2_reviewer1[r]]], "reviewed")
#     names(temp)[names(temp) == "reviewed2"] <- paste0(initials[[pairs$Round2_reviewer2[r]]], "reviewed")
#   
#   files <- list.files("reviewcheck_20200318") %>% str_flatten()
#   if(grepl(paste0(pairs$Round2_reviewer1[r], pairs$Round2_reviewer2[r]), files) | grepl(paste0(pairs$Round2_reviewer2[r], pairs$Round2_reviewer1[r]), files)){
#     next
#   }
#   write_csv(temp, paste0("reviewcheck_20200318/", pairs$Round2_reviewer1[r], pairs$Round2_reviewer2[r],"_round2progress_20200318.csv"))
#    
# }


# for prelim results, just look at single/first reviewed
firstreview <- prelimlong %>%
  group_by(Title, abbr, id) %>%
  filter(!duplicated(Title)) %>%
  ungroup()

write_csv(firstreview, "round2_metareview/data/cleaned/prelim_singlereview.csv")






# exclusion questions
ggplot(subset(firstreview, qnum == "Q3"), aes(abbr, fill = answer)) +
  geom_bar()

# ES question
# question 25 (response variables)
q25df <- subset(firstreview, qnum =="Q12") %>%
  filter(!is.na(answer))


# -- EXTRACT WORDS USED FOR ES RESPONSE AND DRIVERS ----
# what's been classed as EF?
# what's been classed as ES?

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

# -- DOUBLE REVIEWED ----
doubleprelim <- subset(prelimlong, Title %in% records$Q1[records$nobs == 2]) %>%
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



# -- NITTY GRITTY DATA CLEANING -----
# potential issues:
## 1) titles entered incorrectly
## 2) questions not answered (esp questions added later [e.g. exclusion questions])


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
