# jan 2022 r2 keep dataset additional clean-up
# author(s): CTW
# questions?: caitlin.t.white@colorado.edu

# script purpose:
# jan 2022 noticed 3 double reviewed papers have missing answers when pulling references for writers
# rather than go back into compilation code (long process, potential for overwriting archives), treat missing answers here
# there are also some redundant answers in methods when things got reclasses -- check for that

# workflow:
# 1. read in data
# 2. id questions that need a 3rd review to resolve missing answers
# 3. assign final answers
# 4. clean redundant answers (e.g., from reclassed answers)
# 5. clean incidental errors found
# 6. re-recompile r2 keep data and final list of drivers and their binned answers, write out for analysis


# -- SETUP --
rm(list = ls())
library(tidyverse)
options(stringsAsFactors = F)
na_vals <- c("NA", "NaN", ".", "", " ", NA, NaN)

# of abstracts kept, papers assigned for round 2 (half of round 1 kept with the intent for each paper to have 2 reviewers)
r2assigned <- read.csv("round1_exclusion/output/review_assignments_round2_grpdsubset.csv", na.strings = na_vals) # includes papers that got excluded in round 2
# r2keep dataset -- data from papers that made it through full review
qdat <- read.csv("round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned_Aug2020_archive.csv", na.strings = na_vals) 


# -- REALITY CHECK: ALL ANSWERS PRESENT? UNIQUE? -----
# add this check Jan 2022 when realize three Q12 double-rev papers didn't get finalized correctly (were never re-reviewed)
# also found in methods question, duplicated methods present when re-assigned methods == already present methods (e.g., Observational, Observational)
# what else might be missing or off?
# > there should be 273 answers (# papers kept) for most when version == "final"

# specify required questions
required <- with(qdat, unique(abbr[!grepl("Notes|Info|YrsDat|Dist|ESP_|Uncer", abbr, ignore.case = T)]))
required
# pull abbr + qnum for reference
abbr <- distinct(qdat[c("qnum", "abbr")])

checkanswers <- subset(qdat, !is.na(clean_answer) & version == "final", select = c(Title, Init, version, abbr, qnum, clean_answer)) %>%
  group_by(Title, Init, qnum, abbr) %>%
  summarise(nobs = length(clean_answer)) %>%
  ungroup() %>%
  rename(actual_qnum = qnum) %>% #, actual_abbr = abbr
  full_join(distinct(qdat[qdat$version == "final", c("Title", "qnum", "abbr")])) %>%
  # subset to questions that require an answer
  subset(abbr %in% required)
str(checkanswers)
# review NAs present
with(checkanswers, sapply(split(nobs, abbr), function(x) summary(is.na(x))))
# to review with NAs:
# Place, Response, Driver, KremenTopics -- all other NAs plausible/allowable

toreview <- subset(checkanswers, grepl("Pla|Resp|^Drive|KremenT", abbr) & is.na(nobs), select = c(Title, abbr, qnum)) %>%
  # join reviewer info and refdat
  left_join(distinct(qdat[qdat$version == "final" & qdat$qnum != "Q8", c("Title", "Init", "doublerev")])) %>%
  #join refdat
  left_join(distinct(dplyr::select(r2assigned, Title, FirstAuthor, SourcePublication, PublicationYear)))
# 3 papers reviewed by GV and AK
# 1 paper reviewed by SDJ, but notes say no driver present (intentional NA)


# look for redundant answers
# e.g. multi-choice that got repeated .. need to split all multi-choice answers into their own row then look for duplicates
# > this applies to anything note notes (write-in text answer) and not q12
redundant <- subset(qdat, abbr %in% required & !qnum == "Q12") %>%
  # subset to final version and non-NA clean answers
  subset(version == "final" & !is.na(clean_answer)) %>%
  # keep things that have a comma (means more than one answer)
  subset(grepl(",[A-Z]", clean_answer)) %>%
  separate_rows(clean_answer, sep = ",(?=[A-Z])") %>%
  # count unique answers per paper per questions
  group_by(Title, Init, id, clean_answer) %>%
  mutate(nobs = length(answer)) %>%
  subset(nobs > 1) %>%
  ungroup()

redundant$clean_answer # it's just Observational that got repeated for 3 papers
distinct(redundant[c("Title", "clean_answer")])

# small fixes to make

# make lookup tables for corrections
# specify order of Inits and papers in the dataset
datorder <- distinct(qdat[c("Title","Init", "ResponseId", "doublerev", "version")]) %>%
  mutate(order = 1:length(ResponseId))
# pull ES order for making survey order
ESsurveyorder <- distinct(subset(qdat, qnum == "Q12", select = c(id, fullquestion:Group, qnum:survey_order))) %>%
  arrange(survey_order)  



# -- REVIEW AND WRITE OUT Q12 TO FIX ------
q12tofix <- inner_join(toreview[c("Title", "qnum", "doublerev")], qdat) %>%
  # only keep questions that have any answer within Title across reviewers
  group_by(Title, qnum, ES) %>%
  mutate(keepq = any(!is.na(clean_answer))) %>%
  ungroup() %>%
  subset(keepq) %>%
  arrange(doublerev, Title, survey_order, desc(version), Init) %>%
  distinct()
# add otherdriver just in case need (other driver not attached to any ES in the survey to start)
otherdrive <- subset(qdat, abbr == "OtherDriver" & Title %in% unique(q12tofix$Title)) %>%
  group_by(Title) %>%
  mutate(hasother = any(!is.na(clean_answer) & abbr == "OtherDriver")) %>%
  ungroup() %>%
  # drop papers that already have other driver
  subset(!hasother) %>%
  arrange(doublerev, Title, survey_order, desc(version), Init) %>%
  distinct()
# combine, rearrane, write out
q12tofix <- subset(q12tofix, select = names(qdat)) %>%
  rbind(otherdrive[names(.)]) %>%
  # also include general notes in case want to review
  rbind(subset(qdat, Title %in% toreview$Title & qnum == "Q18")) %>%
  arrange(doublerev, Title, survey_order, desc(version), Init)

# write out to treat (only run this the first time since once corrections incorporated there won't be errors anymore)
# write_csv(q12tofix, "round2_metareview/clean_qa_data/needs_classreview/doublerev_inconsistent/jan2022cleanup/missing_answers_jan2022.csv", na = "")



# -- FIX Q12 AND OTHER MISSING ANSWERS -----
newdat <- read.csv("round2_metareview/clean_qa_data/needs_classreview/doublerev_inconsistent/jan2022cleanup/missing_answers_jan2022_ctw.csv", 
                   na.strings = na_vals) %>%
  # add rowid to track
  mutate(rowid = as.numeric(row.names(.)))
# check how it read in
str(newdat)
# check edits
sapply(newdat[c("ctw_answer", "ctw_comment")], function(x) sort(unique(x))) # looks okay


# make clean binned answer df for new drivers adding
cleanbins <- subset(qdat, abbr == "OtherDriver" & !is.na(clean_answer_binned), select = c(clean_answer, clean_answer_binned, abbr, Group, clean_group)) %>%
  distinct()
# check for duplicates
subset(cleanbins, clean_answer == cleanbins$clean_answer[duplicated(cleanbins$clean_answer)]) # something else to fix in main dataset
# canopy cover should be biotic
# who is it?
View(subset(qdat, grepl(cleanbins$clean_answer[duplicated(cleanbins$clean_answer)], clean_answer, ignore.case = T)))
# id paper
fixbin <- with(qdat, unique(Title[grepl(cleanbins$clean_answer[duplicated(cleanbins$clean_answer)], clean_answer, ignore.case = T) & grepl("abiotic", clean_answer_binned, ignore.case = T)]))
fixbin_df <- subset(qdat, Title == fixbin & grepl("Driver", abbr)) %>%
  group_by(ES) %>%
  filter(ES %in% ES[any(!is.na(clean_answer))])

# for single review, want to replace missing with ctw_answer and use ctw_comment for qa_note
# also check if Kremen Topics or anything else should change based on new Q12 answers

# for double review, want to assign missing answer as clean_answer in "final" version row only and update qa_note
# collapsed (str_flatten) individual review answers should go in (raw) "answer" column
# if reviewer did not answer a question, the format for the answer is "FL (inits): (no answer or NA)"

# if reviewer used both response boxes (later we switched to only using one and separating multiple responses with commas)
# keep all response vars in the first box, can retain original answer for review in box 2 and clean_answer will show it's NA now

# should be able to iterate through each question, infill answer and qa_note, then replace rows in main dataset
insertdata <- group_by(newdat, Title) %>%
  filter(survey_order %in% survey_order[!is.na(ctw_comment)]) %>%
  ungroup()

# pull other drivers to see how to edit
otherdrivers_toadd <- subset(insertdata, grepl("Driver", abbr)) %>%
  group_by(Title) %>%
  filter(survey_order %in% survey_order[(abbr == "Driver" & clean_answer == "Other") | ctw_answer == "Other" | (!is.na(ctw_comment) & abbr == "OtherDriver")]) %>%
  ungroup()


# 1. correct single review ------
# each needs to be constructed differently (one new bc missing, one present already in reviewer 1 answer)
# make other driver rows manually
sdjother <- subset(otherdrivers_toadd, Init == "SDJ") %>%
  mutate(answer = ctw_answer,
         clean_answer = ctw_answer) %>%
  separate_rows(clean_answer, sep = ",") %>%
  # clean up
  mutate(clean_answer = trimws(clean_answer),
         # capitalize clean answer
         clean_answer = paste0(casefold(substr(clean_answer, 1,1), upper = T), substr(clean_answer, 2, nchar(clean_answer)))) %>%
  arrange(Group) %>%
  fill("ES", .direction = "down") %>%
  # note infilled ES from driver where "Other" specified
  mutate(qa_note = ifelse(abbr == "OtherDriver", "Infilled ES from Driver where 'Other' specified", qa_note),
         # append ctw_commend to qa_note
         qa_note = ifelse(is.na(qa_note), ctw_comment, paste(qa_note, ctw_comment, sep = "; "))) %>%
  # add varnum to preserve split order. varnum is across group types, by ES
  group_by(Title, ES) %>%
  mutate(varnum = ifelse(clean_answer == "Other", NA, 1:length(clean_answer[!is.na(clean_answer)]))) %>%
  # not perfect but okay because will renumber when ordered anyway
  ungroup() %>%
  # drop vars to rejoin
  dplyr::select(-c(clean_answer_binned, clean_group)) %>%
  # try lowcase join to clean bins
  mutate(clean_answer_low = casefold(clean_answer)) %>%
  left_join(distinct(mutate(cleanbins[c("clean_answer", "clean_answer_binned", "clean_group")], clean_answer = casefold(clean_answer))), by = c("clean_answer_low" = "clean_answer"))
# only one joined, others are new. treat manually
# groups are fine as they are
sdjother$clean_group <- with(sdjother,  ifelse(is.na(clean_group), Group, clean_group)) 
sdjother$clean_answer_binned[grepl("Land cov", sdjother$clean_answer)]
cleanbins[grepl("land cover", cleanbins$clean_answer, ignore.case = T), c("clean_answer", "clean_answer_binned", "clean_group")]
# potentially another problem to fix..
review_landcover <- subset(qdat, grepl("^land cover", clean_answer, ignore.case = T) & qnum == "Q12", select = c(Title, Init, version)) %>% distinct()
# note:
# > ctw looked at all of these papers, siding with CK on land cover as biotic over environmental variable (2 AIS papers)
# > AIS papers use land cover to directly model/inform ES provisioning (e.g., through ARIES, and as suitable habitat to determine where ES occurs across landcape and were mulltiple ES bundles can occur based on land cover)
# > because emphasis is more on land cover potential for biotic support/provisioning, re-binning as clean_group = biotic for any case where land cover used
# > land use still remains as human driver

# e.g., from Meachem et al. 2016 paper (one of AIS papers):
# "The land-use driver category represents different suites of ecological factors that are managed for. "
# "The use of land use stems from human ecology and emphasizes that it is the biophysical landscape that constrains human activity (York et al. 2003)." 
# "Land use has been widely used as a proxy of ecosystem service supply in ecosystem service assessments (Costanza et al. 1997, Burkhard et al. 2009, Nelson et al. 2009)."


# so in addition to assigning clean binned answer for SDJ paper, need to re-assign human driver to two AIS papers and standardize clean binned answer
# looking at other clean_answer_binned == Land cover, seems like "Habitat type" should be biotic too.. maybe specify "Land cover - habitat" for biotic to discern from "Land cover" in environmental group
# > in Nick's paper (env driver = habitat type) using "habitat use" as a site/location variable within ocean to reef system. Env seems appropriate here.
# > authors are not focusing on biotic support potential of habitat type per se, but rather looking at species richness in each place (it's really a location variable)
# > can add a note that reviewed and okay, and *do* change clean_answer_binned to Topography and position since that's more how it's being used
# ctw comment: having reviewed these several papers, "land cover" is a bit fraught in how to assign for clean bin or driver type bc ppl tend use it to refer to biophysical factors *and also* human management..
# > maybe another example of how comparison btwn these types of studies is tricky?

# add Nick's paper, AIS papers, and olive grove papers to to-fix df to keep track
# put canopy cover fix in here too
reclass_drivers <- subset(qdat, grepl("^land cover|^habitat|olive grove|canopy cover", clean_answer, ignore.case = T) & qnum == "Q12" & grepl("Driver", abbr)) %>%
  # drop canopy cover as biotic
  subset(!(grepl("canopy cover", clean_answer, ignore.case = T) & clean_group == "Biotic"))
# > note: AIS paper Assessing bundles of ecosystem services.. (Crouzat) has Land cover type and habitat provision listed as other env drivers. Should just keep Land cover type because that is variable fed in to model, and hab provision is implicit
# > the Crouzet paper is weird to code because is analyzes ES bundles across a region based on modeling 16 ecological parameters (what AIS coded as the responses.. but seem more like explanatory here? but not a "driver" of ES because they are the ES..) and then sees how that correlated/overlaps with land cover type
# > it's not really clear what they base their ES parameters on (without digging into supplement.. and even then only cite used "primary information" or "expert knowledge" in methods)

# clean up env
rm(toreview, review_landcover, fixbin_df, q12tofix)

# for now, infill SDJ clean_answer_binned and clean_group manually since vars are new
sdjother$clean_answer[is.na(sdjother$clean_answer_binned)] # these still need clean_answer_binned
sdjother$clean_answer_binned[sdjother$clean_answer =="Other"] <- "Other"
sdjother$clean_answer_binned[grepl("Land cover", sdjother$clean_answer)] <- "Land cover"
sdjother$clean_answer_binned[grepl("Algal", sdjother$clean_answer)] <- "Pathogens/natural enemies" # because algal bloom had neg impact on ES provision in paper's model
sdjother$clean_answer_binned[grepl("clarity", sdjother$clean_answer)] <- "Abiotic characteristics of the landscape: aquatic"
sdjother$clean_answer_binned[grepl("Surface geol", sdjother$clean_answer)] <- "Soil physiochemical characteristics"
sdjother$clean_answer_binned[grepl("location|connectivity|bathym", sdjother$clean_answer)] <- "Topography and position"

# clean up survey_order and cols
sdjother <- subset(sdjother, select = -survey_order) %>%
  left_join(ESsurveyorder) %>%
  dplyr::select(names(qdat))

# clean up other questions
sdj_standard <- subset(insertdata, Init == "SDJ" & abbr != "OtherDriver" & ctw_answer != "Other") %>%
  mutate(answer = ctw_answer, clean_answer = ctw_answer,
         clean_answer_binned = ifelse(abbr == "EffectDirect", clean_answer,
                                      "Exploitation/harvest"), # only other answer to infill is Exploitation (harvest;fishing)
         clean_group = Group,
         qa_note = ctw_comment) %>%
  #rejoin survey order to be sure
  dplyr::select(-survey_order) %>%
  left_join(ESsurveyorder) %>%
  dplyr::select(names(qdat))

# stack
sdj_stacked <- rbind(sdjother, sdj_standard) %>%
  ungroup() %>%
  data.frame() %>%
  arrange(Title, survey_order) %>%
  # create group alt abbr for lumping driver and other driver for varnum
  mutate(abb2 = ifelse(grepl("Driver", abbr), "Driver", abbr)) %>%
  #redo varnum
  # number variables studied per ES across driver groups -- canned answers first followed by other driver
  mutate(needs_number = grepl("Driv|Respon", abb2) & !is.na(clean_answer) & clean_answer != "Other") %>%
  arrange(Title, ES, abb2, needs_number, survey_order,varnum) %>%
  group_by(Title, ES, abb2, needs_number) %>%
  mutate(varnum = ifelse(needs_number, 1:length(clean_answer[needs_number]), NA)) %>%
  ungroup() %>%
  arrange(Title, survey_order, varnum) %>%
  dplyr::select(names(qdat))



# 2. correct double review -----
# already have good clean_group and clean_answer_binned, just choosing whose answer to go with
akgv_otherdrivers <- subset(otherdrivers_toadd, Init != "SDJ") %>%
  group_by(Title, survey_order) %>%
  mutate(prepped_answer = ifelse(version == "final", NA, 
                                 # if no answer present, paste Init and no answer note
                                 ifelse(is.na(clean_answer), paste0(Init, ": (no answer or NA)"),
                                        # if present, paste initials and answer
                                        paste(Init, answer, sep = ": "))),
         # separate double responses with semi colon
         combined_answer = str_flatten(unique(prepped_answer[!is.na(prepped_answer)]), collapse = "; "),
         # clean up combined answer for rows where one didn't respon
         combined_answer = ifelse(!grepl("GV", combined_answer), paste(combined_answer, "GV: (no answer or NA", sep = "; "),
                                  ifelse(!grepl("AK", combined_answer), paste("AK: (no answer or NA)", combined_answer, sep = "; "), combined_answer))) %>%
  # treat clean_answer only and then add collapsed raw answer
  mutate(clean_answer = ifelse(Init == "AK/GV" | abbr == "OtherDriver", ctw_answer, clean_answer)) %>%
  # fix qa_note for final answer
  mutate(qa_note = ifelse(abbr == "OtherDriver", paste("For", Init, "review only:", qa_note), qa_note),
         Init = ifelse(abbr == "OtherDriver", "AK/GV", Init),
         version = ifelse(Init == "AK/GV", "final", version),
         answer = ifelse(Init == "AK/GV", combined_answer, answer)) %>%
  subset(Init == "AK/GV") %>%
  mutate(qa_note = ifelse(is.na(clean_answer) | is.na(qa_note), ctw_comment, paste(qa_note, ctw_comment, sep = "; ")),
         varnum = ifelse(is.na(clean_answer), NA, varnum),
         clean_answer_binned = ifelse(clean_answer == "Other", "Other", clean_answer_binned),
         clean_group = ifelse(!is.na(clean_answer), Group, NA)) %>%
  ungroup() %>%
  #drop ID cols
  subset(select = Init:qa_note) %>%
  #join response dates and responseID
  left_join(distinct(subset(qdat, select= c(StartDate:Title)))) %>%
  # rearrange names to match qdat
  subset(select = names(qdat))

# fix the other double-review questions (what's not already addressed in other driver inconsistencies)
akgv_standard <- anti_join(insertdata, akgv_otherdrivers[c("Title", "id", "qnum", "abbr")]) %>%
  # drop SDJ paper
  subset(Init != "SDJ") %>%
  group_by(Title, survey_order) %>%
  mutate(prepped_answer = ifelse(version == "final", NA, 
                                 # if no answer present, paste Init and no answer note
                                 ifelse(is.na(clean_answer), paste0(Init, ": (no answer or NA)"),
                                        # use individual clean_answer for kremen topics since those were corrected
                                        ifelse(abbr == "KremenTopics", paste(Init, clean_answer, sep = ": "),
                                               # if present, paste initials and answer
                                               paste(Init, answer, sep = ": ")))),
         
         # separate double responses with semi colon
         combined_answer = str_flatten(unique(prepped_answer[!is.na(prepped_answer)]), collapse = "; "),
         # clean up combined answer for rows where one didn't respon
         combined_answer = ifelse(!grepl("GV", combined_answer), paste(combined_answer, "GV: (no answer or NA", sep = "; "),
                                  ifelse(!grepl("AK", combined_answer), paste("AK: (no answer or NA)", combined_answer, sep = "; "), combined_answer)),
         # combine qa_note if there is one
         combined_qa_note = ifelse(version == "final", NA,
                                   ifelse(all(is.na(qa_note[version == "original"])), NA,
                                          ifelse(length(unique(qa_note[version == "original"])==1), paste("For both AK and GV reviews:", unique(qa_note[version == "original"])),
                                                 paste(Init, qa_note, sep = ": ")))),
         # str_flatten
         combined_qa_note = ifelse(all(is.na(combined_qa_note)), NA, str_flatten(unique(combined_qa_note[!is.na(combined_qa_note)])))) %>%
  ungroup()

# be sure subsetting to just AK/GV will cover all Q's
nrow(distinct(akgv_standard[c("Title", "id", "qnum")]))
nrow(subset(akgv_standard, Init == "AK/GV", select = c("Title", "id", "qnum"))) # yes

akgv_standard <- subset(akgv_standard, Init == "AK/GV") %>%
  mutate(clean_answer = ctw_answer, answer = combined_answer, qa_note = combined_qa_note,
         # add ctw comment to qa_note
         qa_note = ifelse(is.na(qa_note), ctw_comment, paste(qa_note, ctw_comment, sep = "; "))) %>%
  # drop clean_answer_binned and clean_group and rejoin
  subset(select = -c(clean_answer_binned, clean_group)) %>%
  # separate clean_answer -- just for Responses and Drivers or OtherDrivers
  separate_rows(clean_answer, sep = ", |,(?=Management)") %>%
  left_join(distinct(subset(newdat, qnum == "Q12" & !is.na(clean_answer), select = c("Title", "clean_answer", "clean_answer_binned", "Group", "clean_group", "ES")))) %>%
  # organize cols as in main dataset
  subset(select = names(qdat)) %>%
  # replace varnum
  group_by(Title, abbr, ES) %>%
  mutate(varnum = ifelse(grepl("Driver|Response", abbr) & !is.na(clean_answer), 1:length(clean_answer[!is.na(clean_answer)]), NA)) %>%
  ungroup()

# stack treated akgv final answers
akgv_stacked <- rbind(akgv_standard, akgv_otherdrivers) %>%
  arrange(Title, desc(version), Init, survey_order)


# 3. Replace rows with corrections in the main dataset ----
# id which rows in qdat correspond to Title, Inits, id, qnum, and survey_order
qdat$rowid <- as.numeric(rownames(qdat))
# ID relevant rows
sdj_swap <- distinct(subset(sdj_stacked, select = c(Title, Init, qnum, abbr, survey_order))) %>%
  left_join(subset(qdat, select = c(c(Title, Init, qnum, abbr, survey_order, rowid))))
akgv_swap <- distinct(subset(akgv_stacked, select = c(Title, Init, qnum, abbr, survey_order))) %>%
  left_join(subset(qdat, select = c(c(Title, Init, qnum, abbr, survey_order, rowid)))) #looks ok. other drivers were not listed qdat for the combined answers (part of why flagged)
# drop rows from main dataset
qdat_revised <- qdat %>%
  subset(!rowid %in% c(sdj_swap$rowid, akgv_swap$rowid)) %>%
  rbind(cbind(sdj_stacked, rowid = -1), cbind(akgv_stacked, rowid = -1)) %>% # assign dummy rowid to added rows
  arrange(Title, version, desc(version), Init, survey_order, varnum)



# -- CORRECT INCIDENTAL FINDINGS ----
# these just require direct edits to the dataset (e.g., change clean_driver, clean_answer_binned, and add a qa_note)
# can ID by rowid, these need to be manual corrections
reclass_drivers <- left_join(reclass_drivers, qdat_revised)
distinct(reclass_drivers[c("clean_answer", "clean_answer_binned", "clean_group")]) 
# > remove habitat provision from AIS paper because redundant with Land cover type (paper didn't call out habitat provision)

# 1. standardize land cover
reclass_landcover <- with(reclass_drivers, rowid[grepl("land cover", clean_answer, ignore.case = T) & Group == "Biotic"])
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% reclass_landcover] <- "Land cover"
qdat_revised$clean_group[qdat_revised$rowid %in% reclass_landcover] <- "Environmental"
qdat_revised$qa_note[qdat_revised$rowid %in% reclass_landcover] <- paste(qdat_revised$qa_note[qdat_revised$rowid %in% reclass_landcover], "CTW reclassed group and binned answer to standardize land cover variables", sep = "; ")
# 2. standardize canopy cover
reclass_canopycov <- with(reclass_drivers, rowid[grepl("canopy cov", clean_answer) & grepl("Abiotic", clean_answer_binned)])
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% reclass_canopycov] <- cleanbins$clean_answer_binned[grepl("canopy cover", cleanbins$clean_answer) & grepl("Biotic", cleanbins$clean_answer_binned)]
qdat_revised$clean_group[qdat_revised$rowid %in% reclass_canopycov] <- "Biotic"
qdat_revised$qa_note[qdat_revised$rowid %in% reclass_canopycov] <- paste(qdat_revised$qa_note[qdat_revised$rowid %in% reclass_canopycov], "CTW reclassed group and binned answer to standardize canopy cover variables", sep = "; ")
# 3. olive grove types becomes human and management
reclass_olive <- with(reclass_drivers, rowid[grepl("olive", clean_answer)])
qdat_revised$clean_answer[qdat_revised$rowid %in% reclass_olive]
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% reclass_olive] <- "Management/restoration"
qdat_revised$clean_group[qdat_revised$rowid %in% reclass_olive] <- "Human"
qdat_revised$qa_note[qdat_revised$rowid %in% reclass_olive] <- paste(qdat_revised$qa_note[qdat_revised$rowid %in% reclass_olive], "CTW reclassed group and binned answer to emphasize management type", sep = "; ")

# also want to remove habitat provision from AIS paper since redundant
remove_habprovision <- with(reclass_drivers, rowid[grepl("habitat prov", clean_answer) & Init == "AIS"])
# edit qa_note for kept env variable
edit_AISnote <- with(reclass_drivers, rowid[grepl("land cov", clean_answer, ignore.case = T) & grepl("Assessing bundles of ecosystem ", Title)])
qdat_revised$clean_answer[qdat_revised$rowid %in% remove_habprovision] # there was no original answer because clean_answer was the reviewer correction
# since variable was added as correction and hab prov was the 2nd variable, just remove the rows
qdat_revised$qa_note[qdat_revised$rowid %in% edit_AISnote] <- paste(qdat_revised$qa_note[qdat_revised$rowid %in% edit_AISnote], "Correction was 'Land cover type, habitat provision' but CTW removed habitat provision because redundant with land cover and paper only mentioned land cover.")
qdat_revised <- qdat_revised[!(qdat_revised$rowid %in% remove_habprovision),]

#review data
summary(qdat_revised)
summary(is.na(qdat_revised[!is.na(qdat_revised$clean_answer) & grepl("Driver", qdat_revised$abbr),c("clean_answer_binned", "clean_group")]))
# check that anything with clean_answer and is driver or other driver has a clean_answer_binned and clean_group
missing_bin_group <- subset(qdat_revised, !is.na(clean_answer) & grepl("Driver|Effect", abbr) & (is.na(clean_answer_binned) | is.na(clean_group))) # new errors..
# check reverse (no clean answer but clean_answer_binned and clean_group populated for driver/other driver or effect direction)
has_bin_group <- subset(qdat_revised, is.na(clean_answer) & grepl("Driver|Effect", abbr) & (!is.na(clean_answer_binned) | !is.na(clean_group))) # more effect direct flubs

# correct missing or present EffectDirect and Drivers that should/should not be there
# > what it should be is in QA note, so these will need to be manual corrections too
# clean_answer_binned and clean_group missing/present erroneously -----
unique(missing_bin_group$Title)
View(subset(qdat_revised, Title == missing_bin_group$Title[1] & qnum == "Q12" & (!is.na(clean_answer) | !is.na(clean_answer_binned))))
# Effect Direction is correctly in clean_answer_binned, but looking at this now is confusing the direction is still noted in the "clean answer"
# fix all cases like this
effectdir_cases <- subset(qdat_revised, grepl("^Effect direction removed", qa_note)) # includes 2 papers in missing_bin_group + 1 more not
effectdir_cases <- rbind(effectdir_cases, subset(qdat_revised, clean_answer != clean_answer_binned & abbr == "EffectDirect"))
effectdir_cases <- rbind(effectdir_cases, subset(qdat_revised, is.na(clean_answer) & !is.na(clean_answer_binned) & abbr == "EffectDirect"))
effectdir_cases <- distinct(effectdir_cases) %>%
  arrange(Title, survey_order)
View(subset(qdat_revised, !is.na(clean_answer) & is.na(clean_answer_binned) & abbr == "EffectDirect")) # these are ok. not sure why I put final answer for effect directionin clean_answer_binned
unique(effectdir_cases$Title) # 8 papers to treat


# 1. TM papers (3) ----
TMpapers <- with(effectdir_cases, unique(ResponseId[Init == "TM"]))
subset(qdat_revised, ResponseId == TMpapers[1] & qnum == "Q12" & grepl("Driver|Effect", abbr)) %>%
  group_by(ES) %>%
  subset(ES %in% ES[(!is.na(answer) | !is.na(clean_answer_binned))]) %>%
  arrange(ES, survey_order) %>%
  View()
# effect direct reclassed group should be in the original row, just update clean_group (like with driver) -- "Other" needs updated clean_group too
# Pest Path -- Mixed and Other in Env --> Biotic clean_group
qdat_revised$clean_group[qdat_revised$rowid %in% c(16572, 16507, 16509, 16574)] <- "Biotic" # reclass Env -> Biotic driver
# updated clean_answer binned for effect direct
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% c(16572)] <- qdat_revised$clean_answer[qdat_revised$rowid %in% c(16572)]
# update qa_note
qdat_revised$qa_note[qdat_revised$rowid %in% c(16572, 16507,16509, 16574)] <- "clean_group reassigned based on Other Driver clean_group"
# NA biotic row
qdat_revised[qdat_revised$rowid == 16604, c("clean_answer_binned", "clean_group", "qa_note")] <- NA
# make clean_answer binned + for clean_group biotic (both + in env and + in biotic)
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% c(16574, 16606)] <- qdat_revised$clean_answer[qdat_revised$rowid %in% c(16574, 16606)]

# TM paper # 2
subset(qdat_revised, ResponseId == TMpapers[2] & qnum == "Q12" & grepl("Driver|Effect", abbr)) %>%
  group_by(ES) %>%
  subset(ES %in% ES[(!is.na(answer) | !is.na(clean_answer_binned))]) %>%
  arrange(ES, survey_order) %>%
  View()
# one of several other drivers got reclassed to biotic, but others still remain environmental.. can see case for moving effect to biotic row..
# in this case add Mixed to clean_answer because it should be in both env and biotic -- env needs to stay
qdat_revised$clean_answer[qdat_revised$rowid == 15750] <- qdat_revised$clean_answer_binned[qdat_revised$rowid == 15750]
# also add other to biotic row
qdat_revised[qdat_revised$rowid == 15686, c("clean_answer", "clean_answer_binned")] <- "Other"
qdat_revised$clean_group[qdat_revised$rowid == 15686] <- qdat_revised$Group[qdat_revised$rowid == 15686]
qdat_revised$qa_note[qdat_revised$rowid == 15686] <- "Added 'Other' based on clean_group assigned to Other Driver"
View(qdat_revised[qdat_revised$rowid == 15686,])


# TM paper #3
subset(qdat_revised, ResponseId == TMpapers[3] & qnum == "Q12" & grepl("Driver|Effect", abbr)) %>%
  group_by(ES) %>%
  subset(ES %in% ES[(!is.na(answer) | !is.na(clean_answer_binned))]) %>%
  arrange(ES, survey_order) %>%
  View() # similar type of correction as paper #2 (other driver got reclassed as biotic from env, but  env other drivers also present still)
# in this case add Mixed to clean_answer because it should be in both env and biotic -- env needs to stay
qdat_revised$clean_answer[qdat_revised$rowid == 14704] <- qdat_revised$clean_answer_binned[qdat_revised$rowid == 14704]
# also add other to biotic row
qdat_revised[qdat_revised$rowid == 14640, c("clean_answer", "clean_answer_binned")] <- "Other"
qdat_revised$qa_note[qdat_revised$rowid == 14640] <- "Added 'Other' based on clean_group assigned to Other Driver"
View(qdat_revised[qdat_revised$rowid == 14640,])


# 2. CW papers (1) ----
CWpapers <- with(effectdir_cases, unique(ResponseId[Init == "CW"]))
subset(qdat_revised, ResponseId == CWpapers & qnum == "Q12" & grepl("Driver|Effect", abbr)) %>%
  group_by(ES) %>%
  subset(ES %in% ES[(!is.na(answer) | !is.na(clean_answer_binned))]) %>%
  arrange(ES, survey_order) %>%
  View() # env reclassed to human, just update clean group and NA orphan clean_answer_binned in human
qdat_revised$clean_group[qdat_revised$rowid %in% c(8348, 8412)] <- "Human"
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% c(8412)] <- qdat_revised$clean_answer[qdat_revised$rowid %in% c(8412)]
# update qa_note
qdat_revised$qa_note[qdat_revised$rowid %in% c(8348, 8412)] <- "clean_group reassigned based on Other Driver clean_group"
# NA orphan in human driver row
qdat_revised[qdat_revised$rowid %in% c(8428), c("clean_answer_binned","clean_group", "qa_note")] <- NA

# 3. IS papers (1) ----
ISpapers <- with(effectdir_cases, unique(ResponseId[Init == "IS"]))
subset(qdat_revised, ResponseId == ISpapers & qnum == "Q12" & grepl("Driver|Effect", abbr)) %>%
  group_by(ES) %>%
  subset(ES %in% ES[(!is.na(answer) | !is.na(clean_answer_binned))]) %>%
  arrange(ES, survey_order) %>%
  View() # all other drivers are biotic based on reclassed clean_group
# change clean_anser and clean_answer_binned for biotic to Mixed based on Environmental. Make all clean_group biotic
qdat_revised[qdat_revised$rowid %in% c(25060, 25092), c("clean_answer", "clean_answer_binned")] <- "Mixed"
qdat_revised$clean_group[qdat_revised$rowid %in% c(25060, 24996)] <- "Biotic"
# add qa_note to effect direct
qdat_revised$qa_note[qdat_revised$rowid %in% c(25060, 25092)] <- "Effect Direction changed to 'Mixed' based on environmental Other Driver reclassed to Biotic clean_group"
# note clean_group reclassed for other
qdat_revised$qa_note[qdat_revised$rowid == 24996] <- "clean_group reassigned based on Other Driver clean_group"


# 4. LB papers (1) ----
LBpapers <- with(effectdir_cases, unique(ResponseId[Init == "LB"]))
subset(qdat_revised, ResponseId == LBpapers & qnum == "Q12" & grepl("Driver|Effect", abbr)) %>%
  group_by(ES) %>%
  subset(ES %in% ES[(!is.na(answer) | !is.na(clean_answer_binned))]) %>%
  arrange(ES, survey_order) %>%
  View() # other env driver reclassed to biotic, and other env drivers still present. update biotic rows for Other --> Driver and Effect Direct
qdat_revised[qdat_revised$rowid %in% c(21873), c("clean_answer", "clean_answer_binned")] <- "Other"
qdat_revised$clean_group[qdat_revised$rowid %in% c(21873)] <- qdat_revised$Group[qdat_revised$rowid %in% c(21873)] 
qdat_revised$clean_answer[qdat_revised$rowid %in% c(21937)] <- qdat_revised$clean_answer_binned[qdat_revised$rowid %in% c(21937)]
#update qa note for Driver and EffectDirect -- added bc other driver reclassed to biotic
qdat_revised$qa_note[qdat_revised$rowid %in% c(21873)] <- "Added 'Other' based on clean_group assigned to Other Driver"
qdat_revised$qa_note[qdat_revised$rowid %in% c(21937)] <- "Added 'Mixed' effect direction based on clean_group assigned to Other Driver"


# 5. AK papers (1) ----
AKpapers <- with(effectdir_cases, unique(ResponseId[Init == "AK"]))
subset(qdat_revised, ResponseId == AKpapers & qnum == "Q12" & grepl("Driver|Effect", abbr)) %>%
  group_by(ES) %>%
  subset(ES %in% ES[(!is.na(answer) | !is.na(clean_answer_binned))]) %>%
  arrange(ES, survey_order) %>%
  View() # standard env driver present + 1 other driver that got reclassed to biotic
# change other clean_group to Biotic
qdat_revised$clean_group[qdat_revised$rowid %in% c(8849)] <- "Biotic"
qdat_revised$qa_note[qdat_revised$rowid %in% c(8849)] <- "clean_group reassigned based on Other Driver clean_group"
# add Mixed effect direct to biotic row 
qdat_revised$clean_answer[qdat_revised$rowid %in% c(8945)] <- qdat_revised$clean_answer_binned[qdat_revised$rowid %in% c(8945)]
# qa note already has infill note so leave as is


# 5. SDJ papers (1) ----
SDJpapers <- with(effectdir_cases, unique(ResponseId[Init == "SDJ"]))
subset(qdat_revised, ResponseId == SDJpapers & qnum == "Q12" & grepl("Driver|Effect", abbr)) %>%
  group_by(ES) %>%
  subset(ES %in% ES[(!is.na(answer) | !is.na(clean_answer_binned))]) %>%
  arrange(ES, survey_order) %>%
  View()
# env and biotic need mixed and other in clean_answer and clean_answer binned for effect direct and driver
qdat_revised[qdat_revised$rowid %in% c(49903, 49936), c("clean_answer", "clean_answer_binned")] <- "Other"
# update qa note
qdat_revised$qa_note[qdat_revised$rowid %in% c(49903, 49936)] <- "Added 'Other' based on clean_group assigned to Other Driver"
# add mixed effect direct
qdat_revised[qdat_revised$rowid %in% c(49968, 50000), c("clean_answer", "clean_answer_binned")] <- "Mixed"
qdat_revised$qa_note[qdat_revised$rowid %in% c(49968, 50000)] <- "Added 'Mixed' effect direction based on clean_group assigned to Other Driver"
# need to add clean_group to env and biotic
qdat_revised$clean_group[qdat_revised$rowid %in% c(49903, 49936, 50000)] <- qdat_revised$Group[qdat_revised$rowid %in% c(49903, 49936, 50000)]
# done!

# recheck erroneous missing or present bins
missing_bin_group <- subset(qdat_revised, !is.na(clean_answer) & grepl("Driver|Effect", abbr) & (is.na(clean_answer_binned) | is.na(clean_group))) # new errors..
# blanket assign clean answer to clean_answer_binned and Group -> clean_group ("Other" or standard driver answers)
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% missing_bin_group$rowid] <- qdat_revised$clean_answer[qdat_revised$rowid %in% missing_bin_group$rowid]
qdat_revised$clean_group[qdat_revised$rowid %in% missing_bin_group$rowid] <- qdat_revised$Group[qdat_revised$rowid %in% missing_bin_group$rowid]
# manually review looks okay
View(subset(qdat_revised, qnum == "Q12" & grepl("Driver|Effect", abbr) & ResponseId %in% missing_bin_group$ResponseId)) # ok
# check reverse (no clean answer but clean_answer_binned and clean_group populated for driver/other driver or effect direction)
has_bin_group <- subset(qdat_revised, is.na(clean_answer) & grepl("Driver|Effect", abbr) & (!is.na(clean_answer_binned) | !is.na(clean_group))) # more effect direct flubs
# nothing here

# make sure any ES with an answer for Q12 has at least one response and one driver
EScheck <- subset(qdat_revised, qnum == "Q12") %>%
  group_by(Title, Init, ES, Group) %>%
  mutate(ES_checked = any(!is.na(clean_answer))) %>%
  subset(ES_checked) %>%
  ungroup() %>%
  group_by(Title, Init, ES, abbr) %>%
  mutate(has_answer = any(!is.na(clean_answer))) %>%
  ungroup() %>%
  group_by(Title, Init) %>%
  mutate(flag_paper = any(grepl("Driver|Respon", abbr) & !has_answer)) %>%
  subset(flag_paper) %>%
  arrange(Title, Init, ES, survey_order)
# 2 papers..
# LB's has Mixed effect for ocean reg. Think accidental check. Several other ESes in paper have all their responses. OceanReg is in between ClimateReg and Freshwater and those both have answers.
# action: simple remove and add qa note
qdat_revised[qdat_revised$rowid == 28049, c("clean_answer", "clean_answer_binned", "clean_group")] <- NA
qdat_revised$qa_note[qdat_revised$rowid == 28049] <- "CTW removed effect direction response. Only entry for Ocean Reg ES, all other ESes indicated have complete responses."

#TM's is missing a biotic driver for Energy. ESP driver entered for all other ESes, so probably that. CTW will check paper.
# action: update based on CTW review and add qa note
# >> after review, think it should be a human driver for energy ES:
# "We used machinery, fuel and farm chemical inputs for regional no-tillage winter wheat and perennial hay production to asses in-field energy use"
# It says in Table 1 they measured In-field energy inputs (human inputs), and in Table 3 report summary stats of each input..
# I don't really think there is an effect direction because it's categorical

# NA biotic effect direct
qdat_revised[qdat_revised$rowid == 15056, c("clean_answer", "clean_answer_binned", "clean_group")] <- NA
qdat_revised$qa_note[qdat_revised$rowid == 15056] <- "Response missing for biotic driver. CTW reviewed study, opines only human drivers present for Energy ES and no effect direction because categorical variables."

# add 	Management Practices (bin = Management/restoration) and Other (bin = Other) for Human Driver
# add fertilizer (nitrogen and phos) and pesticides/fungides/insecticides for other drivers 
# take out Energy ES human driver rows for TM paper, edit, then rbind back in
humandriversTM <- subset(qdat_revised, ResponseId == unique(EScheck$ResponseId[EScheck$Init =="TM"]) & grepl("Driver|Direct", abbr) & Group == "Human" & (grepl("Energ", ES) | is.na(ES)))
humandriversTM[humandriversTM$abbr == "Driver", c("answer", "clean_answer")] <- "Management Practices,Other"
humandriversTM[humandriversTM$abbr == "OtherDriver", c("answer", "clean_answer")] <- "Fertilizer (N and P), Herbicides/Insecticides/Fungicides"
humandriversTM[humandriversTM$abbr == "EffectDirect", c("answer", "clean_answer", "clean_answer_binned")] <- "Mixed"
humandriversTM$qa_note[humandriversTM$abbr == "EffectDirect"] <- "CTW reviewed. Effect of each energy input is positive or null, response variable is categorical (type of grass yield)."
# add qa note for drivers
humandriversTM$qa_note[grepl("Driver", humandriversTM$abbr)] <- "CTW reviewed. TM originally indicated Biotic driver for Energy ES but no answer provided. Drivers for Energy ES seem more related to human drivers."
# give Other Driver row Energy ES
humandriversTM$ES[humandriversTM$abbr == "OtherDriver"] <- humandriversTM$ES[humandriversTM$abbr == "Driver"]
# separate rows based on comma
humandriversTM <- separate_rows(humandriversTM, clean_answer, sep = ",")
humandriversTM$clean_answer <- trimws(humandriversTM$clean_answer)
# add clean_answer_binned
humandriversTM$clean_answer_binned[grepl("Manag" ,humandriversTM$clean_answer)] <- with(qdat, unique(clean_answer_binned[grepl("^Management Pr", clean_answer) & clean_group == "Human"]))
humandriversTM$clean_answer_binned[grepl("^Oth" ,humandriversTM$clean_answer)] <- "Other"
humandriversTM$clean_answer_binned[grepl("^Fert" ,humandriversTM$clean_answer)] <- with(cleanbins, unique(clean_answer_binned[grepl("fertilize", clean_answer) & Group == "Human"]))
humandriversTM$clean_answer_binned[grepl("ticide" ,humandriversTM$clean_answer)] <- with(cleanbins, unique(clean_answer_binned[grepl("herbicide", clean_answer) & Group == "Human"]))
# correct survey order for Other Driver
humandriversTM$survey_order[humandriversTM$abbr == "OtherDriver"] <- with(ESsurveyorder, survey_order[Group == "Human" & abbr == "OtherDriver" & grepl("Energy", ES)])
# number driver variables
humandriversTM$varnum[grepl("Driver", humandriversTM$abbr) & !grepl("Other", humandriversTM$clean_answer)] <- 1:length(humandriversTM$clean_answer[grepl("Driver", humandriversTM$abbr) & !grepl("Other", humandriversTM$clean_answer)]) 
# assign clean_group
humandriversTM$clean_group <- humandriversTM$Group
# add back in to qdat_revised
qdat_revised <- qdat_revised[!qdat_revised$rowid %in% humandriversTM$rowid,]
qdat_revised <- rbind(qdat_revised, humandriversTM) %>%
  arrange(Title, version, desc(version), Init, survey_order, varnum)

summary(qdat_revised)
# are there any drivers or effect direct that don't have a clean_group?
with(qdat_revised, summary(!is.na(clean_answer[grepl("Driver|Effect", abbr)]) & is.na(clean_group[grepl("Driver|Effect", abbr)]))) # all okay


# 6. standardize and QA clean_answer bins -----
# while preparing data for publication noticed "vegetation cover" binned variables overlap with "biotic characteristics of the plot
# > make all biotic characteristics to standardize
vegcanopy <- with(qdat_revised, grep("Vegetation cov|Biotic char", clean_answer_binned))
View(arrange(qdat_revised[vegcanopy,], clean_answer_binned))
# ok to make these all biotic char, all clean groups and survey orders fine (already Biotic)
biolabel <- unique(qdat_revised$clean_answer_binned[grepl("Biotic char", qdat_revised$clean_answer_binned)])
qdat_revised$clean_answer_binned[vegcanopy] <- biolabel

# because land use land cover change gets treated as their own driver type, review to be sure all individual vars appropriate for land use/land cover
lulcvars <- subset(qdat_revised, grepl("Land", clean_answer_binned) & abbr == "OtherDriver")
sort(unique(lulcvars$clean_answer))

# rock fragment cover, stage age, tree volume, Location (fore reef or lagoon) stick out as ones to check
checklulcbin <- subset(lulcvars, grepl("rock frag|stand age|tree vol|Location [(]fo", clean_answer)) %>%
  left_join(r2assigned[c("Title", "FirstAuthor", "SourcePublication")])

# rock fragment (Hernandez et al. 2013):
# > %rock cover taken from point intercept surveys in plots along with other veg monitoring cover
# > more appropriate to list as abiotic char of plot and environmental var than human (need to switch survey internal # and effect direct to env)
# need to adjust in final answer and individual answer
rockrowids <- with(checklulcbin, rowid[grepl("rock", clean_answer)])
# be sure these are rows to adjust (there aren't duplicate rowids)
View(qdat_revised[qdat_revised$rowid %in% rockrowids,]) # yes
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% rockrowids] <- unique(with(qdat_revised, clean_answer_binned[grepl("Abiotic char.* terres", clean_answer_binned)]))
qdat_revised$clean_group[qdat_revised$rowid %in% rockrowids] # don't need to change group, no qa note needed


# stand age and tree volume (Truchy et al. 2019):
# from methods:
# Catchment land use information (comprising percent agricultural land and coniferous forest, spatial location of clear-cuts) were obtained using the Swedish Landcover Map (2004) along with the Swedish Forestry Agency’s clear-cut records since 2001. 
# Stand age and tree volume information were also retrieved from the SLU Forest Map (Department of Forest Resource Management, Swedish University of Agricultural Sciences)
# from website: each raster is 12.5x12.5x metres (taken with Sentinel 2)
# intent is to characterize catchment land use, so keep as land use var (but can add QA note to show checked)

truchyrowid <-  with(checklulcbin, rowid[grepl("Truchy", FirstAuthor)])
# be sure these are rows to add note to (there aren't duplicate rowids)
View(qdat_revised[qdat_revised$rowid %in% truchyrowid,]) # yes
newnote <- qdat_revised$qa_note[qdat_revised$rowid %in% truchyrowid] 
newnote <- paste(newnote, "CTW confirmed driver variable used more like land use [silviculture inventory from raster data] than biotic characteristic of plot", sep = "; ")
qdat_revised$qa_note[qdat_revised$rowid %in% truchyrowid] <- newnote


# Location (fore reef or lagoon) Dubois et al. 2019
# > they use inside-outside lagoon more like habitat type (type of reef species community present, but consider mobiles species than can swim between the two)
# > use is about relative distance from reef. reclass to topography & position (Env type so don't need to change group or survey order)
lagoonrowid <- with(checklulcbin, rowid[grepl("lagoon", clean_answer)])
# be sure these are rows to adjust (there aren't duplicate rowids)
View(qdat_revised[qdat_revised$rowid %in% lagoonrowid,]) # yes
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% lagoonrowid] <- unique(with(qdat_revised, clean_answer_binned[grepl("position", clean_answer_binned)]))
qdat_revised$clean_group[qdat_revised$rowid %in% lagoonrowid] # don't need to change group, no qa note needed



# -- RULE-CHECK KREMEN TOPICS ----
# > from original cleaning code:
## RULES for KT1: (ESPs)
# 1) If "Single species" in Q14 and has_ESP, then KT1 should be appended
# 2) has ESP indicated, no KT1 or KT2 in Q13

## > if ESP check in Biotic drivers, then ESP checked in Kremen topics
has_ESPdriver <- subset(qdat_revised, abbr %in% c("Driver", "OtherDriver") | qnum %in% c("Q13", "Q14")) %>%
  # arrange(RecordedDate) %>%
  # check for "Service Provider" in clean_answer_finer by ResponseId
  group_by(ResponseId) %>%
  mutate(Bio_answer = str_flatten(unique(clean_answer_binned[grepl("Bio", clean_group) & !is.na(clean_answer_binned)])), # flatten all biotic driver bin labels that aren't NA
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
  filter((has_ESP & !KT_ESP) | (!has_ESP & KT_ESP)) %>% #(grepl("Single", ESP_type) & !KT_ESP)
  # can allow KT_ESP IFF singlemulti_ESP == TRUE
  ungroup() %>%
  # keep only ResponseId, Title and flagging
  subset(abbr %in%  c("KremenTopics", "KremenNotes", "ESP_type", "Driver", "OtherDriver") & !is.na(clean_answer)) %>%
  filter(is.na(clean_group) | clean_group == "Bio") %>%
  distinct()
# looking at Kremen 2005.. seems like if the study stopped at ESP as the response, and did not connect/measure ESP effect on function or ES provision, should not count towards Kremen Topics 1


addKT1 <- has_ESPdriver %>%
  group_by(ResponseId) %>%
  # either has ESP driver indicated and "Single Species" checked in Q14
  # OR has ESP driver indicated, no Response to Q14 and KT1 or KT2 not indicated
  filter((has_ESP & grepl("Single", ESP_type)) | (has_ESP & !grepl("1|2", KT))) %>%
  ungroup() 
# nothing to update

# RULES FOR KT2: (Community structure influencing function)
# 1) has_biodiv in driver bin and no KT1
# 2) across_spp & no KT2 [no cases! anyone who checked across also checked KT 2]

# > no edits involved an ESP so don't need to recheck KT1
# > only biodiversity variable was in AK/GV paper but was more of an index related to ES bundles, so not applicable to biodiv influencing EF (KT2)
# > since scale question not revised, KT4 check not applicable

# RULES FOR KT3 ENV: (Environmental factors that influence provision)
# 1) has env driver (has_env) but Env not checked in KT Topics (!KT_env)
# 2) does NOT have env driver (!has_env) but Env check in KT Topics (KT_env)

# check KT3 with latest edits
has_env <- subset(qdat_revised, abbr %in% c("Driver", "OtherDriver", "Response") | qnum %in% c("Q13", "Q14", "Q8")) %>%
  #arrange(RecordedDate) %>%
  # check for "Service Provider" in clean_answer_finer by ResponseId
  group_by(ResponseId) %>%
  # look for any answer that is assigned to an environmental group
  mutate(env_answer = str_flatten(unique(clean_answer_binned[grepl("Env", clean_group) & !is.na(clean_answer_binned)])), # flatten all env driver bin labels that aren't NA
         # screen for Service provider in all Bio group coarse bins
         has_env = env_answer != "",
         KT = clean_answer[abbr == "KremenTopics"],
         # look for Krement Topic 3 (environmental) in Kremen Topics answer
         KT_env = grepl("3", KT)) %>%
  # subset to either has an env answer and no KT 3 checked *or* the reverse
  filter((has_env & !KT_env) | (KT_env & !has_env)) %>%
  ungroup() %>%
  # keep only ResponseId, Title and flagging
  #dplyr::select(ResponseId, doublerev, Title, Bio_answer:ncol(.)) %>%
  subset(abbr %in%  c("KremenTopics", "KremenNotes", "ESP_type", "Driver", "OtherDriver", "Response", "Nested", "Extent") & !is.na(clean_answer)) %>%
  #filter(is.na(clean_group)) %>%
  distinct()

add_KTenv <- subset(has_env, (has_env & !KT_env) & abbr == "KremenTopics", select = c("ResponseId", "rowid", "Init"))
remove_KTenv <- subset(has_env, !has_env & KT_env & abbr == "KremenTopics", select = c("ResponseId", "rowid", "Init"))
View(subset(has_env, ResponseId %in% add_KTenv$ResponseId))  # this is SDJ paper with updated drivers
View(subset(has_env, ResponseId %in% remove_KTenv$ResponseId)) #AK/AIS paper shouldn't have KT3 (isn't in either of their individual responses, and no env present.. not sure how that happened)
# > mostly lingering KT3 based on Group == Env, not clean_group == Env

# because only 4 cases, treat manually
KT3answer <- "Kremen Topic 3 : Environmental factors that influence provision (env. drivers not including human drivers)"
# add KT 3 where needed
qdat_revised$clean_answer[grepl(add_KTenv$rowid, qdat_revised$rowid)] <- KT3answer # Original answer here is "None" so can overwrite
qdat_revised$qa_note[grepl(add_KTenv$rowid, qdat_revised$rowid)] <- "Added KT3 based on updated environmental drivers"


# remove KT env where needed
# > via gsub for all, then clean up as needed
qdat_revised$clean_answer[qdat_revised$rowid %in% remove_KTenv$rowid] <- gsub(",?Kremen Topic 3 : Environmental factors that influence provision [(]env[.] drivers not including human drivers[)],?", "", qdat_revised$clean_answer[qdat_revised$rowid %in% remove_KTenv$rowid])
# assign "None" to blanks
qdat_revised$clean_answer[qdat_revised$rowid %in% remove_KTenv$rowid & qdat_revised$clean_answer == ""] <- "None"
# update qa_notes
qdat_revised$qa_note[qdat_revised$rowid %in% remove_KTenv$rowid & qdat_revised$clean_answer == "None"] <- "Removed KT3 based on lack of clean_group environmental driver (drivers reassigned from Env Group to a different clean_group)"
qdat_revised$qa_note[qdat_revised$rowid %in% remove_KTenv$rowid[remove_KTenv$Init == "IS"]] <- NA #ctw added KT3, was never in IS answer, so removal of it is fine
qdat_revised$qa_note[qdat_revised$rowid %in% remove_KTenv$rowid[remove_KTenv$Init == "AK/AIS"]] # no edit needed

# clean up environment
rm(has_env, has_ESPdriver, remove_KTenv, addKT1, add_KTenv, has_bin_group, missing_bin_group)



# -- FIX REDUNDANT -----
# this is a simple gsub
# id relevant rows
redundant_methods <- with(qdat_revised, which(Title %in% unique(redundant$Title) & survey_order == unique(redundant$survey_order)))
# verify
qdat_revised[redundant_methods, c("answer", "clean_answer")] # correct
qdat_revised$clean_answer[redundant_methods] <- unique(redundant$clean_answer) 


# -- LAST CHECK -----
# check unique answers per question
with(subset(qdat_revised, !grepl("Notes|Response|OtherDri|GenIn", abbr)), sapply(split(clean_answer, abbr), function(x) unique(strsplit(x, ",(?=[A-Z])", perl = T))))
# seems okay
with(subset(qdat_revised, grepl("Driver|Effec", abbr)), sapply(split(clean_answer_binned, abbr), function(x) sort(unique(x)))) # typo in Extreme Events (should be events)
qdat_revised$clean_answer_binned <- gsub("Extreme Events", "Extreme events", qdat_revised$clean_answer_binned)
with(subset(qdat_revised, grepl("Driver|Effec", abbr)), sapply(split(clean_answer_binned, clean_group), function(x) sort(unique(x))))
# Habitat still present.. for one paper.. driver = forest fragment adjacency and size..
# what are the indices in Human and Env new_group? (CTW manually reviewed and fine. Different indices used in each new_group)

# looking at variables binned in land cover, things that are like fragmentation are in there, so will reclass forest frag in that as well
# > GV + AK say land cover and land use are getting put in their own driver anyway

View(qdat_revised[which(grepl("Habitat", qdat_revised$clean_answer_binned)),])
# grab id and subset to Q12-Q14 to review
habitat_paper <- subset(qdat_revised, Title %in% Title[clean_answer_binned == "Habitat"] & qnum %in% c("Q12", "Q13", "Q14") & !is.na(clean_answer)) #%>%
# only other drivers ("Other" in Driver question also), and all biotic
# Kremen Topics is scale only so add KT3
# re-assign all drivers and effect direct as biotic
habitat_rowids <- with(habitat_paper, rowid[grepl("Bio", clean_group)])
# make sure correct rows
View(qdat_revised[qdat_revised$rowid %in% habitat_rowids,]) # yes
qdat_revised$clean_group[qdat_revised$rowid %in% habitat_rowids] <- "Environmental"
hab_note <- "Other driver reclassed to Land cover (clean_group == Env) to standardize with similar variables."
qdat_revised$qa_note[qdat_revised$rowid %in% habitat_rowids & is.na(qdat_revised$qa_note)] <- hab_note
qdat_revised$qa_note[qdat_revised$rowid %in% habitat_rowids & grepl("Infilled", qdat_revised$qa_note)] <- paste(qdat_revised$qa_note[qdat_revised$rowid %in% habitat_rowids & grepl("Infilled", qdat_revised$qa_note)], hab_note, sep = "; ")
# update land cover as clean_answer_binned
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% habitat_rowids & qdat_revised$abbr == "OtherDriver"] <- "Land cover"
# review
View(qdat_revised[qdat_revised$rowid %in% habitat_rowids, ]) # ok
# update Kremen Topics (has scale already)
habKT <- with(habitat_paper, rowid[abbr == "KremenTopics"])
qdat_revised$clean_answer[qdat_revised$rowid %in% habKT] <- paste(KT3answer, with(qdat_revised, clean_answer[rowid %in% habKT]), sep = ",")
qdat_revised$qa_note[qdat_revised$rowid %in% habKT] <- paste("Added KT3 based on clean_group environmental driver and no KT3 checked", with(qdat_revised, qa_note[rowid %in% habKT]), sep = "; ")
# lowcase second note to make similar to other QA notes
qdat_revised$qa_note[qdat_revised$rowid %in% habKT] <- gsub("Appended", "appended", qdat_revised$qa_note[qdat_revised$rowid %in% habKT])

# review
View(qdat_revised[qdat_revised$rowid %in% habitat_paper$rowid, ]) # ok
with(habitat_paper, qa_note[abbr == "KremenTopics"])

# make comprehensive bins for AIS, SDJ, LD (as done in clean_round2, updating with any new bins made here)
driverbins <- subset(qdat_revised, abbr == "OtherDriver" & !is.na(clean_answer), select = c(clean_answer, clean_answer_binned, Group, clean_group)) %>%
  distinct() %>%
  arrange(clean_group, clean_answer_binned, clean_answer)
# notice that "Management" and "Mangement/restoration" exists as bins for Human drivers. Standardize.
# also notice time-related variables aren't all binned at Timing/seasonality. Standardize.
driverbins[grepl("time|season|^age| age ", driverbins$clean_answer, ignore.case = T) | grepl("Timing", driverbins$clean_answer_binned), ]
# "Time since restoration", "time postfire", and age-related huamn drivers should fall in Timing/seasonality
# find the rows that have a time-variable that need new bin
timerows <- with(qdat_revised, rowid[grepl("Time since|time postfire|^age", clean_answer)])
# be sure correct rows
View(qdat_revised[qdat_revised$rowid %in% timerows,])
timerows <- with(qdat_revised, rowid[grepl("Time since|time postfire", clean_answer)])
agerows <- with(qdat_revised, rowid[grepl("^age", clean_answer) & clean_group == "Human"])
# the age vars seem like they should fall in human characteristics (age of person, and age person began harvesting)
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% timerows] <- with(cleanbins, unique(clean_answer_binned[grepl("Timing", clean_answer_binned)]))
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% agerows] <- with(cleanbins, unique(clean_answer_binned[grepl("human pop", clean_answer_binned)]))
# fix mgmt bin
mgmtrows <- with(qdat_revised, rowid[grepl("^Management$", clean_answer_binned)])
# be sure correct rows
View(qdat_revised[qdat_revised$rowid %in% mgmtrows,]) # yes
qdat_revised$clean_answer_binned[qdat_revised$rowid %in% mgmtrows] <- with(cleanbins, unique(clean_answer_binned[grepl("restor", clean_answer_binned)]))

# remake driverbins
driverbins <- subset(qdat_revised, grepl("Driver", abbr) & !is.na(clean_answer), select = c(clean_answer, clean_answer_binned, Group, clean_group, abbr)) %>%
  distinct() %>%
  # remove other
  subset(clean_answer != "Other") %>%
  arrange(clean_group, clean_answer_binned, abbr, clean_answer) %>%
  rename(question_abbr = abbr)

# notice a few more odd variables in manual review (seem like notes that got entered in other field?)..
# review unusual drivers -----
humanstructure <- with(driverbins, clean_answer[grepl("preference", clean_answer, ignore.case = T)])
notevar <- with(driverbins, clean_answer[grepl("Note: ", clean_answer, ignore.case = T)])
humanstructure_dat <- subset(qdat_revised, Title %in% unique(Title[grepl(humanstructure, qdat_revised$clean_answer)]) & qnum %in% c("Q12", "Q13", "Q14", "Q18"))
notevar_dat <- subset(qdat_revised, Title %in% unique(Title[grepl(notevar, qdat_revised$clean_answer)]) & qnum %in% c("Q12", "Q13", "Q14", "Q18"))
# after reviewing Pearse et al. 2018, agree with SDJ that ES was maintenance of options
# SDJ listed veg homogenization as response, but actual variables measured were:
# diversity: mean phylogenetic distance, species richness
# composition: Sorenson's index, and phylosorenson's metric of phylogenetic distance
# structure: tree height, tree density, leaf traits (leaf surface area and leaf perimeter:area)
# explanatory vars were always habitat pool (urban cultivated, urban spontaneous, or natural), and PDSI
# redo MaintOpts for Q12, and look at Kremen Topics and ESP type
# PDSI is continuous variables (+ for richness, but no relationship for other response vars)
# pool type is categorical

# just keep ES that has answer, other drivers + other questions
humanstructure_dat <- group_by(humanstructure_dat, ES) %>%
  mutate(has_answer = any(!is.na(clean_answer))) %>%
  ungroup() %>%
  subset(has_answer | abbr %in% c("OtherDriver", "ESP_type", "KremenTopics", "SurveyNotes"))

# remove rows from qdat_revised
qdat_revised <- qdat_revised[!qdat_revised$rowid %in% humanstructure_dat$rowid,]
# update response variable
humanstructure_dat$clean_answer[humanstructure_dat$survey_order == 87] <- "Species richness (diversity), Mean phylogenetic distance (diversity), Sorenson's index (composition), Phylosorenson's metric of phylogenetic distance (composition), Tree height (structure), Tree density (structure), Leaf surface area (structure), Leaf perimeter:area (structure)" 
humanstructure_dat$qa_note[humanstructure_dat$survey_order == 87] <- "CTW reviewed paper to confirm other drivers. Study measured several response variables for diversity, composition, and structure. Updating SDJ original response of 'vegetation homogenization'."
# remove Other drivers in Env and Human -- is just Climate and Management Practices (standard answers)
humanstructure_dat <- humanstructure_dat[!humanstructure_dat$rowid %in% c(50266, 50350, 50351),]
# NA other driver ES rows
humanstructure_dat$ES[humanstructure_dat$abbr == "OtherDriver"] <- NA
humanstructure_dat[humanstructure_dat$abbr == "OtherDriver", c("clean_answer", "clean_answer_binned", "varnum", "clean_group", "qa_note")] <- NA
# clean up survey order for Other Driver
humanstructure_dat$survey_order[humanstructure_dat$abbr == "OtherDriver"] <- with(ESsurveyorder, survey_order[abbr == "OtherDriver" & is.na(ES)])
humanstructure_dat[humanstructure_dat$survey_order == 105, c("clean_answer","clean_answer_binned")] <- "Climate"
humanstructure_dat$qa_note[humanstructure_dat$survey_order == 203] <- "CTW moved raw answer to survey notes. No other environmental drivers present."
humanstructure_dat$qa_note[humanstructure_dat$survey_order == 204] <- "CTW reviewed other human drivers listed. Management practices is sufficient answer driver. Explanatory was species pool/habitat type (cultivated, spontaneous, or natural). Moved SDJ raw answer to survey notes." 
# separate variables by comma
humanstructure_dat <- separate_rows(humanstructure_dat, clean_answer, sep  = ", ")
humanstructure_dat$clean_answer <- trimws(humanstructure_dat$clean_answer)
# move raw answers from other driver to survey notes
humanstructure_dat$clean_answer[humanstructure_dat$qnum == "Q18"] <- with(humanstructure_dat, paste(answer[qnum == "Q18"], trimws(answer[survey_order == 204]), trimws(answer[survey_order == 203]), sep = "; "))
humanstructure_dat$qa_note[humanstructure_dat$qnum == "Q18"] <- "Moved raw answers for Other Drivers to to survey notes. Environmental and human drivers fall in fixed answer choices."                          
#redo varnum
humanstructure_dat$varnum[humanstructure_dat$abbr == "Response" & !is.na(humanstructure_dat$clean_answer)] <- 1:length(with(humanstructure_dat, varnum[abbr == "Response" & !is.na(clean_answer)]))
humanstructure_dat$varnum[humanstructure_dat$abbr == "Driver" & !is.na(humanstructure_dat$clean_answer)] <- 1:length(with(humanstructure_dat, varnum[abbr == "Driver" & !is.na(clean_answer)]))
# rbind to dataset
qdat_revised <- rbind(qdat_revised, humanstructure_dat[names(qdat_revised)]) 


# CK (notevar) paper has inconsistent entries for ES = Food and ES = HabCreate.. review
# > looking at Pearse et al. 2017, would say paper's focus is on habitat and not food. Ultimately trying to to link oyster bed habitat to other services like coastal protection (erosion mitigation) from oil spills
# > explanatory vars are shoreline oiling category, and freshwater diversion exposure (based on polygons, and whether a site fell in or out)
# > also freshwater influence (inside or outside [location/position variable])
# > THEN, they also related oyster habitat (as explanatory) to shoreline erosion (wind exposure is the variable they used for shorelines erosion)
# > % oyster hab and oyster size were response vars for habitat ES
# > oyster habitat was response var for eroded shoreline (found no relationship)
# > look at ESP_type question and kremen topics too

# pull out hab, food, and coastal ES (go with Soil Protection because Coastal ES is about water quality not erosion control), edit then rebind
notevar_dat <- subset(notevar_dat, !is.na(clean_answer) | grepl("Food|Hab|Soil", ES) | grepl("Kremen|ESP|Survey|OtherD", abbr)) %>%
  data.frame()

# remove rowids from qdat_revised
qdat_revised <- qdat_revised[!qdat_revised$rowid %in% notevar_dat$rowid,]
# as qa note and NA Food ES answers
notevar_dat$qa_note[grepl("Food", notevar_dat$ES) & !is.na(notevar_dat$clean_answer)] <- "CTW reviewed paper for incomplete answers. Reassigning CK answers from Food ES to Habitat ES. Focus of paper was on oyster habitat and coastline structural protection after oil spills."
notevar_dat[grepl("Food", notevar_dat$ES) | grepl("Note: ", notevar_dat$clean_answer), c("clean_answer", "clean_answer_binned", "varnum", "clean_group")]  <- NA

# > edit HabCreate ES
# change survey_order for Other from HabCreate to empty ES and add qa note
notevar_dat$survey_order[grepl("Infilled ES", notevar_dat$qa_note)] <- with(ESsurveyorder, survey_order[grepl("OtherD", abbr) & grepl("Human", Group) & is.na(ES)])
notevar_dat$qa_note[grepl("Note: ", notevar_dat$answer)] <- "Clarifying note about human driver entered in Other Driver field. Moved note to Survey Notes."
# NA ES for human other driver
notevar_dat$ES[grepl("Note: ", notevar_dat$answer)] <- NA 
# move to survey note
notevar_dat$clean_answer[grepl("SurveyN", notevar_dat$abbr)] <- notevar_dat$answer[grepl("Note: ", notevar_dat$answer)]
notevar_dat$qa_note[grepl("SurveyN", notevar_dat$abbr)] <- "Moved note in human Other Driver field to Survey Notes"
# drop "Other" human driver
notevar_dat <- notevar_dat[-grep("Other", notevar_dat$clean_answer_binned),]

# add environmental *other* driver (freshwater exposure location) to HabCreate ES
notevar_dat[notevar_dat$survey_order == 203, c("clean_answer", "clean_answer_binned", "clean_group", "ES")] <- list("Freshwater influence (coastal position)", "Topography and position", "Environmental", "HabCreate")
notevar_dat$qa_note[notevar_dat$survey_order == 203] <- "CTW reviewed paper for missing answers in drivers and responses. Freshwater influence is an environmental factor variable for oyster habitat."
# edit survey_order
notevar_dat$survey_order[notevar_dat$survey_order == 203] <- with(ESsurveyorder, survey_order[grepl("OtherDr", abbr) & grepl("Env", Group) & grepl("HabC", ES)])
# add oyster size to habitat response var
notevar_dat$clean_answer[grepl("Oyster hab", notevar_dat$answer) & grepl("Hab", notevar_dat$ES)] <- c("Oyster habitat, Oyster size")
notevar_dat <- separate_rows(notevar_dat, clean_answer, sep = ", ")
# add note to oyster size
notevar_dat$qa_note[grepl("Oyster siz", notevar_dat$clean_answer) & grepl("Hab", notevar_dat$ES)] <- "Moved CK answer to response variable from Food ES to Habitat ES."
# would say habitat and size is EF rather than ES
notevar_dat$clean_answer[notevar_dat$abbr == "Yclass" & grepl("Hab", notevar_dat$ES)] <- "EF"
notevar_dat$qa_note[notevar_dat$abbr == "Yclass" & grepl("Hab", notevar_dat$ES)] <- "CTW reviewed paper for missing answers. Oyster habitat and size are more ecosystem function metrics in the way the study is using them."
# add "Other" to Driver for Env
notevar_dat[notevar_dat$survey_order == 91, c("clean_answer", "clean_answer_binned")] <- "Other"
notevar_dat$qa_note[notevar_dat$survey_order == 91] <- "CTW reviewed paper for missing answer. Added other environmental driver for habitat creation ES."
# add EffectDirect
notevar_dat[notevar_dat$survey_order == 155, c("clean_answer", "clean_answer_binned")] <- "Didn't quantify" # in 2-way ANOVA, just checked for interaction
# add qa note for effect direct
notevar_dat$qa_note[notevar_dat$survey_order %in% c(155)] <- "CTW reviewed paper for missing answers. Freshwater exposure was a categorical environmnetal variable."
# add clean_group to Driver and Effect Direct
notevar_dat$clean_group[notevar_dat$survey_order %in% c(91,155)] <- "Environmental"

# > edit Soil Protection ES
# add coastal erosion as response and oyster habitat as *other* driver for Soil Protection
notevar_dat$clean_answer[notevar_dat$survey_order == 73] <- "Coastal erosion (wind exposure aggregate index)"
# > other driver
notevar_dat[notevar_dat$survey_order == 98, c("clean_answer", "clean_answer_binned", "clean_group")] <- list("Other", "Other", "Environmental")
# rbind env other driver row to create one for soil protection
addother_env <- notevar_dat[notevar_dat$rowid == 39789,]
addother_env[c("clean_answer", "clean_answer_binned", "clean_group", "ES")] <- list("Percent oyster habitat","Land cover", "Environmental", "SoilProtect")
# clean up survey_order and qa note 
addother_env$survey_order <- with(ESsurveyorder, survey_order[grepl("OtherDr", abbr) & grepl("Env", Group) & grepl("Soil", ES)])
addother_env$qa_note <- "CTW reviewed paper for missing answers. Study looked at oyster habitat provision and coastal erosion mitigration from oyster habitat."
# rbind to notevar_dat
notevar_dat <- rbind(notevar_dat, addother_env)
# add ES as Yclass
notevar_dat$clean_answer[notevar_dat$survey_order == 146] <- "ES"
# add effect direction (null)
notevar_dat[notevar_dat$survey_order == 162, c("clean_answer", "clean_answer_binned", "clean_group")] <- list("0", "0", "Environmental")
# add qa note to soil protection ES answers filled in
notevar_dat$qa_note[notevar_dat$survey_order %in% c(73, 98, 146, 162)] <- "CTW reviewed paper for missing answers. Study looked at oyster habitat provision and coastal erosion mitigration from oyster habitat."
# ESP type should be land cover or hab as proxy
notevar_dat$clean_answer[notevar_dat$abbr == "ESP_type"] <- "Only land cover or habitat type as proxy"
# Kremen Topics should have KT3
notevar_dat$clean_answer[notevar_dat$abbr == "KremenTopics"] <- KT3answer
# add QA notes
notevar_dat$qa_note[grepl("ESP_t", notevar_dat$abbr)] <- "CTW reviewed paper for missing answers in drivers and responses. Oyster habitat was proxy for ESP."
notevar_dat$qa_note[grepl("KremenT", notevar_dat$abbr)] <- "CTW reviewed paper for missing answers in drivers and responses. Study related environmental factors (freshwater loation) and habitat (land cover) to EF/ES."
# redo varnum
notevar_dat <- ungroup(notevar_dat) %>%
  mutate(abbr2 = ifelse(grepl("Res", abbr) & !is.na(clean_answer), "Res", ifelse(grepl("Driv", abbr) & !is.na(clean_answer), "Driver", NA)),
         abbr2 = ifelse(grepl("^Other$", clean_answer_binned), NA, abbr2)) %>%
  arrange(survey_order, varnum) %>%
  group_by(ES, abbr2) %>%
  mutate(varnum = ifelse(grepl("R|D", abbr2) & !is.na(clean_answer), 1:length(abbr2[!is.na(clean_answer)]), NA)) %>%
  ungroup() %>%
  arrange(survey_order, varnum)

# rbind CK revised answers back into qdat_revised
qdat_revised_out <- rbind(qdat_revised, notevar_dat[names(qdat_revised)]) %>%
  arrange(Title, desc(version), Init, survey_order, varnum) %>%
  # clean up
  dplyr::select(-rowid) %>%
  mutate_at(.vars = c("StartDate", "EndDate", "RecordedDate"), function(x) as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>% 
  data.frame() %>%
  distinct()

summary(qdat_revised_out)

# remake driverbins
driverbins <- subset(qdat_revised_out, grepl("Driver", abbr) & !is.na(clean_answer), select = c(clean_answer, clean_answer_binned, Group, clean_group, abbr)) %>%
  distinct() %>%
  # remove other
  subset(clean_answer != "Other") %>%
  arrange(clean_group, clean_answer_binned, abbr, clean_answer) %>%
  rename(question_abbr = abbr)

# in review.. 
# NDWI shoulds be abiotic char aquatic not terrestrial (normalized difference water index, not NDVI)
# can standardize spelling of drivers (e.g. pH, ph, PH; temp, Temp, Temperature, latitude, Latitude; rain, Rainfall, rainfall; slope, Slope; vegetation cover)
standardize_drivers <- grep("^temp|^latitude|^rain|^slope|^vegetation cov|^canopy cov|^Land cov|^Topo",qdat_revised_out$clean_answer, ignore.case = T)
sort(qdat_revised_out$clean_answer[standardize_drivers]) # correct
qdat_revised_out$clean_answer[standardize_drivers] <- with(qdat_revised_out[standardize_drivers,], paste0(casefold(substr(clean_answer, 1,1), upper = T), substr(clean_answer, 2, nchar(clean_answer))))
qdat_revised_out$clean_answer[grep("^Rain$", qdat_revised_out$clean_answer)] <- "Rainfall"
qdat_revised_out$clean_answer[grep("^Temp$", qdat_revised_out$clean_answer)] <- "Temperature"
# fix pH
qdat_revised_out$clean_answer[grep("^pH$", qdat_revised_out$clean_answer, ignore.case = T)] <- "pH"
# to be sure
qdat_revised_out[standardize_drivers, c("answer", "clean_answer")] # LD has precip and rainfall as other drivers in same paper.. looked at paper.. too much going in paper on so defer to LD
# fix NDWI (change to abiotic char of aquatic)
qdat_revised_out$clean_answer_binned[grep("^NDWI", qdat_revised_out$clean_answer)] <- gsub("terrestrial", "aquatic", qdat_revised_out$clean_answer_binned[grep("^NDWI", qdat_revised_out$clean_answer)])
# trim whitespace from all clean_answer
qdat_revised_out$clean_answer <- trimws(qdat_revised_out$clean_answer)

# remake driverbins
driverbins <- subset(qdat_revised_out, grepl("Driver", abbr) & !is.na(clean_answer), select = c(clean_answer, clean_answer_binned, Group, clean_group, abbr)) %>%
  distinct() %>%
  # remove other
  subset(clean_answer != "Other") %>%
  arrange(clean_group, clean_answer_binned, abbr, clean_answer) %>%
  rename(question_abbr = abbr)

# check for duplicates
qdat_revised_out <- group_by(qdat_revised_out, ResponseId, clean_answer, ES, survey_order) %>%
  mutate(check_dup = any(duplicated(id)))
# not a cleaning error! these are drivers that are entered twice by the reviewer
# since ordered by varnum, can remove the row that is flagged as a duplicate
qdat_revised_out <- mutate(qdat_revised_out, removedup = duplicated(id))
qdat_revised_out[qdat_revised_out$removedup, c("answer", "clean_answer", "varnum")] # for last paper, slope is not the final variable.. may need to renumber varnum
qdat_revised_out$qa_note[qdat_revised_out$check_dup & !qdat_revised_out$removedup]
# assign rowid again to make edits easy
qdat_revised_out$rowid <- as.numeric(rownames(qdat_revised_out))

# remove duplicates
# > grab ES where duplicate lives to renumber varnum then add back in
fix_duplicates <- ungroup(qdat_revised_out) %>%
  group_by(ResponseId, qnum, ES) %>%
  mutate(dupES = any(check_dup)) %>%
  # pare to impacted ES and response or driver since that's all that's affected
  subset(dupES & grepl("Respon|Driv", abbr)) %>%
  ungroup()

# remove these rowids from main dataset
qdat_revised_out <- qdat_revised_out[!qdat_revised_out$rowid %in% fix_duplicates$rowid,]
# remove dups, annotate fix duplicates, and renumber varnum
fix_duplicates <- subset(fix_duplicates, !(check_dup & removedup)) %>%
  # only rows to annotate remain of duplicates
  mutate(qa_note = ifelse(check_dup & is.na(qa_note), paste0("Other driver '", clean_answer, "' entered twice. Removed duplicate."),
                          ifelse(check_dup & !is.na(qa_note), paste(qa_note, paste0("Other driver '", clean_answer, "' entered twice. Removed duplicate."), sep = "; "), 
                                 qa_note)),
         abbr2 = ifelse(grepl("Res", abbr) & !is.na(clean_answer), "R", ifelse(grepl("Drive", abbr) & !grepl("^Other$", clean_answer) & !is.na(clean_answer), "D", NA))) %>%
  arrange(ResponseId, survey_order, varnum) %>%
  group_by(ResponseId, ES, abbr2) %>%
  mutate(varnum = 1:length(!is.na(clean_answer))) %>%
  ungroup() %>%
  mutate(varnum = ifelse(is.na(abbr2), NA, varnum))

# add back in to main dataset  
qdat_revised_out <- rbind(qdat_revised_out, fix_duplicates[names(qdat_revised_out)]) %>%
  arrange(RecordedDate, Title, desc(version), Init, survey_order, varnum) %>%
  # clean up
  dplyr::select(StartDate:qa_note)

# remake driverbins
driverbins <- subset(qdat_revised_out, grepl("Driver", abbr) & !is.na(clean_answer), select = c(clean_answer, clean_answer_binned, Group, clean_group, abbr)) %>%
  distinct() %>%
  # remove other
  subset(clean_answer != "Other") %>%
  arrange(clean_group, clean_answer_binned, abbr, clean_answer) %>%
  rename(question_abbr = abbr)


# -- WRITE OUT ----
# order dataset by order of answers received (same as how written out in original cleaning script)
# > create paper order as done in original cleaning script
paperorder <- ungroup(qdat_revised_out) %>%
  subset(qnum == "Q3") %>% 
  distinct(ResponseId, RecordedDate) %>%
  arrange(RecordedDate) %>%
  mutate(titleorder = 1:nrow(.))
# order dataset
qdat_revised_out <- ungroup(qdat_revised_out) %>%
  # sort dataset by order answers came in, then by survey order and varnum
  left_join(paperorder) %>%
  arrange(titleorder, survey_order, varnum) %>%
  select(StartDate:qa_note)

# write out main datasaet
write_csv(qdat_revised_out, "round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv")

# write out clean bins
write_csv(driverbins, "round2_metareview/data/intermediate/round2_master_driver_bins.csv", na = "")

