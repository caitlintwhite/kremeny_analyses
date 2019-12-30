# compile results from class coding round 1 (exclusion screening)

# script workflow:
# search G Drive for all instances of exclusion criteria coding form results (as of 12/23/19, there are at least 4-5 copies)
# compare coding results files and distill to unique rows
# prioritize more recent scoring for any papers reviewed twice (e.g. papers re-scored to answer Q8)
# output: final set of papers that make it through to round 2 coding
# random-assign equal number of papers to class list

# notes:



# -- SETUP ----
# clean up environnment
rm(list = ls())
# load needed libraries
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(readxl)
options(stringsAsFactors = F)
theme_set(theme_bw())

# read in data...
# find all instances of exclusion criteria results on GDrive
# > note: CTW tried to read in results dynamically from Google form, but everything I searched shows savings results to GSheet and reading that in
# > tried reading in form via xml/html, but reads form only, not results. Think, for now, must manually export form results to GDrive first before reading into R..
results <- drive_find(pattern = "Exclusion Criteria for Kremen Review", type = "spreadsheet")
# import all results to list
resultsls <- list()
for(i in 1:nrow(results)){
  resultsls[[i]] <- read_sheet(results$id[i]) %>% as.data.frame() %>%
    mutate(filename = results$name[i],
           owner = results$drive_resource[[i]]$owner[[1]]$emailAddress,
           createdTime = results$drive_resource[[i]]$createdTime,
           modifiedTime = results$drive_resource[[i]]$modifiedTime,
           records = nrow(.))
}


# read in paper assignments
assignments <- drive_find(pattern = "EcosystemServicesPapersNov2019.xlsx", n_max = 10) # first should be assignments
# create temp file for downloading excel sheet
tempxl <- tempfile(fileext = ".xlsx")
drive_download(assignments[1,], path = tempxl, overwrite = T) # downloads paper assignments to github repo.. until can figure out how to read excel directly from gdrive..
assignmentsdf <- read_excel(tempxl) 
# delete file downloaded locally
file.remove(tempxl)

# read in LD/CTW abstracts to cross check re-eval'd papers
abstracts_LD <- read_sheet(drive_find(pattern = "Laura/Caitlin", type = "spreadsheet", n_max = 1))
glimpse(abstracts_LD) # read in correct file .. pubdate col is list, header didn't import correctly


# list papers in Meets Criteria folder for cross checking
meetscriteria <- drive_ls(drive_find(pattern = "Meets Criteria", type = "folder", n_max = 10))


# -- PRELIM REVIEW DATA ----
# quick check of assignments df
glimpse(assignmentsdf)

# what is the structure of all coding dats?
lapply(resultsls, names)
lapply(resultsls, head) # items 2-4 have list for title col..
# troublshoot title col as list..
test <- resultsls[[4]]
test$Title_extract <- NA
for(i in 1:nrow(test)){
  test$Title_extract[i] <- test$Title[[i]][1]
} # row 246 no bueno (Title == NULL) in list items 2-4 .. fix after combining all dats
test[246,]

# colnames a little different in each..
## 1 has email address, no q8 -- these would have papers reviewed before q8 created
## 2 and 3 have a column ...9 (??)
View(resultsls[[2]])
sapply(resultsls[[2]], function(x) summary(is.na(x))) # mystery ...9 col is empty..
# how many rows empty? (excluding timestamp, title, and last 3 file ID cols I added)
summary(apply(resultsls[[2]][,3:(ncol(resultsls[[2]])-4)], 1, function(x) all(is.na(x)))) # only 1
#check for empty cols in all
lapply(resultsls, function(x) summary(is.na(x)))



# -- COMPILE RESULTS INTO MASTER ----
# as compile, 1) remove empty cols, make sure has cols equivalent to most recent version
# gather times created and last modified..
times <- unlist(lapply(results$drive_resource, function(x) x[names(x) == "createdTime"])) %>% gsub("T|Z", " ", .) %>% as.POSIXct()
modtimes <- unlist(lapply(results$drive_resource, function(x) x[names(x) == "modifiedTime"])) %>% gsub("T|Z", " ", .) %>% as.POSIXct()
records <- sapply(resultsls, nrow)
which.max(times) #idk why earliest made (ie.e. pre-q8) has most recent create date..
which.max(modtimes) # go with mod times for prioritizing results
refdf <- data.frame(createdTime = times, modifiedTime = modtimes, `records` = records) %>%
  arrange(desc(records), desc(modifiedTime), desc(createdTime)) %>%
  # assign priority rank
  mutate(rank = seq(1,nrow(.), 1))

# stack all dats
master <- data.frame()
for(i in 1:length(resultsls)){
  tempdat <- resultsls[[i]]
  # ensure Title col named Title to match with assignemnts dat
  names(tempdat)[grepl("Title", names(tempdat), ignore.case = T)] <- "Title"
  # remove any cols that are all NA
  tempdat <- tempdat[,!sapply(tempdat, function(x) all(is.na(x)))]
  # if all colnames agree or is first instance, rbind as is
  if(all(names(master) == names(tempdat)) | nrow(master) == 0){ # <-- this line throws a warning, it's fine
    master <- rbind(master, tempdat)
  } else{
    # find and resolve name mismatch
    masternames <- names(master)
    tempnames <- names(tempdat)
    missingmaster <- masternames[!masternames %in% tempnames] # names in master missing from tempdat
    missingtemp <- tempnames[!tempnames %in% masternames] # names in tempdat missing from master
    # check if missing name slight mod of name in other dataset (e.g. Q1 Title vs Title) -- partial match, first 70% of string
    #synonym <- sapply(missingtemp, function(x) grepl(substr(x, 1, round(nchar(x)*.7,0)), missingmaster, ignore.case = T))
    master <- cbind(master, matrix(nrow = nrow(master), ncol=(length(missingtemp)), dimnames = list(NULL, missingtemp)))
    tempdat <- cbind(tempdat, matrix(nrow = nrow(tempdat), ncol=(length(missingmaster)), dimnames = list(NULL, missingmaster))) %>%
      # reoganize cols in temp to match master
      dplyr::select(names(master))
    # stack temp to master
    master <- rbind(master, tempdat)
  }
}
# check data stacking
glimpse(master) # fix title col as list (all questions NA and title == NULL)
#ID which rows have no entries
rowNA <- apply(master[,grep("^Q[0-9]|[?]|Comments",names(master))], 1, function(x)all(is.na(x)))
View(master[rowNA,]) #good
# remove empty rows or rows with NULL, then unlist title values
master <- master[!rowNA,]
master$Title <- unlist(master$Title)
str(master) # okay now

# fix datetime cols
master <- mutate_at(master, vars("createdTime", "modifiedTime"), function(x)as.POSIXct(gsub("T", "", x)))
# assign ranking
master <- left_join(master, refdf)



# -- CLEAN UP COMPILED -----
# qa titles and append reviewer info (emails collected in first version of form)
namecheck <- dplyr::select(master, Title) %>% distinct() %>%
  mutate(reviewed = 1) %>%
  left_join(dplyr::select(assignmentsdf, EBIOReviewer, Number, Title)) %>%
  mutate(final_name = ifelse(reviewed == 1 & !is.na(EBIOReviewer), Title, NA))
# run check on name mispell
for(n in namecheck$Title[is.na(namecheck$final_name) & !is.na(namecheck$Title)]) {
  words <- strsplit(n, " ") %>% unlist()
  # look for max word match
  ncheck <- sapply(assignmentsdf$Title, function(x) sum(words %in% unlist(strsplit(x, " "))))
  if(max(ncheck) > length(words)*.7){
    # replace title
    namecheck$final_name[namecheck$Title == n & !is.na(namecheck$Title)] <- assignmentsdf$Title[which.max(ncheck)]
  }
  # try matching abstract
  if(!is.na(pmatch(n,assignmentsdf$Abstract)) & pmatch(n, assignmentsdf$Abstract) > 0){
    namecheck$final_name[namecheck$Title == n] <- assignmentsdf$Title[pmatch(n, assignmentsdf$Abstract)]
  }
}
# who still needs match?
namecheck$Title[is.na(namecheck$final_name)]
# what is NA title in master?
View(master[is.na(master$Title),]) # can ignore, looks like initial testing before launching coding doc
# ditch NA title in namecheck
namecheck <- subset(namecheck, !is.na(Title))
# manual corrections on the rest..
## 1190 = ref num
namecheck$final_name[namecheck$Title == "1190"] <- assignmentsdf$Title[assignmentsdf$Number == 1190]
## SUDS
namecheck$final_name[grepl("SUDS", namecheck$Title)] <- assignmentsdf$Title[grepl("SUDS", assignmentsdf$Abstract)]
## bat comment -- check LD abstracts sheet -- can get it from matching comment in master
namecheck$final_name[grepl("Only really focused on bat", namecheck$Title)] <- unique(master$Title[grepl("Only really focused on bat", master$`Comments on the paper or any questions that are unclear`)])
## I can't find any paper assigned to LB that's on TRY database.. given it was from first version of coding form and only presents in that one record, maybe was just a test entry.. going to ignore
# reassign reviewers and record nums
namecheck <- namecheck[c("Title", "final_name", "reviewed")] %>%
  left_join(dplyr::select(assignmentsdf, EBIOReviewer, Number, Title), by = c("final_name" = "Title")) %>%
  distinct()

# distill to unique records
# select colnames on which to check dups
qnames <- names(master)[grep("[?]|^Q[0-9]|Comments|Titl", names(master))]
# use second copy to preserve original
# append reviewer and clean title info
master2 <-  left_join(master, distinct(namecheck[c("Title", "final_name", "EBIOReviewer", "Number")])) %>% # added 4 extra rows.. not sure why
  # remove any record that doesn't have reviewier assigned (e.g. NA or TRY title from round 1 coding)
  filter(!(is.na(EBIOReviewer) & rank == max(master$rank))) %>%
  # sort dat by date modified so remove earlier duplicate entries
  arrange(final_name, rank) %>%
  # reorder cols and ditch email address (only contains 4 unique addresses and have all names now anyway)
  dplyr::select(filename:records, rank, EBIOReviewer, Number, Timestamp, `Email Address`, final_name, Title, names(.)[grep("meta-analy", names(.))]:names(.)[grep("^Q7", names(.))], names(.)[grep("^Q8", names(.))], names(.)[grep("^Comments", names(.))]) %>%
  distinct()

# remove duplicate records (removes 2nd+ instance)
masterdups <- master2[(duplicated(master2[,qnames])),] # keep just in case need
master3 <- master2[!(duplicated(master2[,qnames])),]


# make abbrev colnames for review
nameref <- data.frame(orig = names(master3),
                      abbr = c(names(master3)[1:grep("Title", names(master3))],"meta", "review", "no_efes", "valrisk", "tool", "biodiv", "comments")) %>%
  mutate(abbr = ifelse(grepl("Emai", abbr), "email", abbr))

names(master3) <- nameref$abbr
# think I should split papers re-eval'd for Q8 vs ones that haven't been...
reval_titles <- master3$final_name[duplicated(master3$final_name)]
# select records for titles that were rescored
rescored <- subset(master3, final_name %in% reval_titles)
# who rescored? [CTW re-eval'd what LD had done prior to Q8 development]
summary(as.factor(rescored$EBIOReviewer))
# select records for titles only eval'd once
singlerev <- subset(master3, !final_name %in% reval_titles)



# -- SCREEN SINGLE-REVIEWED RECORDS -----
# how many do not have q8 scored? do reviewers want to rescore before we move on to coding, or let round 2 score biodiv question?
# for laura-caitlin abstracts, need to infill No to anything that's NA (CTW re-eval'd anything that might be Yes for Q8), also correctly assign reviewer (LD or CTW)

# infill LD/CTW Q8 with No
## check to be sure all make sense for No infilling
sapply(with(singlerev, singlerev[EBIOReviewer == "Caitlin", names(singlerev)[13:18]]), function(x) summary(as.factor(x)))
## what are the NAs?
View(subset(singlerev, EBIOReviewer == "Caitlin" & (is.na(review)|is.na(no_efes)))) # looks like both should be no
# how many others are empty when should be answered?
subset(singlerev, EBIOReviewer != "Caitlin") %>%
  dplyr::select(meta:tool) %>%
  mutate_all(as.factor) %>%
  summary()
# were q5-q7 not scored bc scored Yes for meta-analysis or review question?
subset(singlerev, EBIOReviewer != "Caitlin" & (meta == "Yes" | review == "Yes")) %>%
  dplyr::select(meta:tool) %>%
  mutate_all(as.factor) %>%
  summary() # not necessarily..
# how many rows have NAs (excluding Q8) when at least one question scored YES, or all else NO
yesno_tally <- dplyr::select(singlerev, EBIOReviewer, rank, meta:tool) %>%
  subset(complete.cases(.) == F) %>%
  mutate(Yes = apply(.,1, function(x) sum(x == "Yes", na.rm = T)),
         No = apply(.,1, function(x) sum(x == "No", na.rm = T)),
         empty = apply(.,1, function(x) sum(is.na(x), na.rm = T)),
         tally = Yes+No)
View(yesno_tally)
# -- 12/26 question: -----
## asked group how to address NAs ... save for later and move on ...


# picking up LD/CTW NA infill..
# infill NAs with No and Q8 NAs with No
singlerev <- singlerev %>%
  mutate_at(grep("meta", names(singlerev)):grep("biodi", names(singlerev)), function(x) ifelse(singlerev$EBIOReviewer == "Caitlin" & is.na(x), "No", x))
sapply(with(singlerev, singlerev[EBIOReviewer == "Caitlin", names(singlerev)[13:18]]), function(x) summary(as.factor(x))) # no more NAs
sapply(with(singlerev, singlerev[EBIOReviewer != "Caitlin" & rank == 1, names(singlerev)[13:18]]), function(x) summary(as.factor(x)))
# check for NAs by coding form version..
sapply(singlerev[names(singlerev)[13:18]], function(x) sapply(split(x, singlerev$rank), function(x) summary(as.factor(x))))
# who filled out earliest version?
subset(singlerev, rank > 1) %>% dplyr::select(rank, EBIOReviewer, Number) %>%
  group_by(rank, EBIOReviewer) %>%
  summarise(nobs = length(Number))
# do N, S and T have entries in rank == 1 (most recent version) form?
subset(singlerev, rank == 1 & grepl("Trav|Sier|Nick", EBIOReviewer)) %>% dplyr::select(rank, EBIOReviewer, Number) %>%
  group_by(rank, EBIOReviewer) %>%
  summarise(nobs = length(Number)) #hm.. nick only for now (12/26)
# update actual reviewer for Caitlin-assigned abstracts
singlerev <- singlerev %>%
  # use LD/CTW abstracts form
  left_join(abstracts_LD[c("Reviewer", "Title")], by = c("final_name" = "Title")) %>%
  # standardize Laura name first
  mutate(Reviewer = ifelse(!grepl("Cait", Reviewer), "Laura", Reviewer),
         EBIOReviewer = ifelse(EBIOReviewer == "Caitlin", Reviewer, EBIOReviewer)) %>%
  dplyr::select(-Reviewer)

# q8 NA vs answered (12/26-- wrote group to ask if people will re-eval or not)
table(is.na(singlerev$biodiv)) # more not answered than answered




# -- ADDRESS RESCORED RECORDS -----
# prioritize most recent entry, but check scoring against first round to see that it makes sense (e.g. filling in  NA)
# look for yes/no conflicts in repeat scoring
# for ctw-ld entries, ctw is most recent and should be used, but append earlier comment as well

# sum Yes's and No's as first check
rescored <- rescored %>%
  mutate(Yes = apply(.,1, function(x) sum(x == "Yes", na.rm = T)),
         No = apply(.,1, function(x) sum(x == "No", na.rm = T)),
         # only sum NAs in exclusion questions
         empty = apply(.[names(.)[grep("meta", names(.)):grep("biodiv", names(.))]],1, function(x) sum(is.na(x), na.rm = T)),
         tally = Yes+No) %>%
  #remove names created by apply fxn
  mutate_at(vars("Yes", "No", "empty", "tally"), as.numeric) %>%
  # flag any rescore whose tally is not greater than earlier score
  group_by(final_name) %>%
  mutate(flag_tally = tally[which.max(Timestamp)] <= tally[which.min(Timestamp)]) %>%
  ungroup()

# -- triage rescores where tally doesn't increase -----
# who are the problem cases?
View(subset(rescored, flag_tally)) 
## CTW forgot to answer question, can infill with LD's answer
## Isabel's rescore is the same, but more recent entry lacks comment (comment accidentally pasted to title when filling out form)
## Aislyn changed 2 papers to YES for doesn't directly measure EF or ES, but those papers are in the Meets Criteria folder.. need to follow up?
conflict_rescore <- subset(rescored, flag_tally) %>%
  group_by(final_name)
# break piping bc can't figure out using mutate_at
# this line will infill later timestamp NAs with previous version if non-NA value present
conflict_rescore[names(conflict_rescore)[13:17]] <- sapply(conflict_rescore[names(conflict_rescore)[13:17]], function(x) ifelse(is.na(x) & conflict_rescore$Timestamp == max(conflict_rescore$Timestamp), x[which.min(conflict_rescore$Timestamp)], x))
# keep Aislyn's NO to question 3 since those articles are in meets criteria folder (write line to search for authors in files within meets criteria folder, i.e. automate)
conflict_rescore <- ungroup(conflict_rescore) %>%
  left_join(distinct(assignmentsdf[c("Title", "AuthorsFull")]), by = c("final_name" = "Title")) %>%
  # extract First author last name to search Meets Criteria folder
  mutate(FirstAuthor = word(AuthorsFull, sep = ",")) %>%
  group_by(final_name) %>%
  mutate(in_meetscriteria = sum(grepl(unique(FirstAuthor), meetscriteria$name))) %>%
  ungroup() %>%
  #recrunch tallies
  mutate(Yes = apply(.,1, function(x) sum(x == "Yes", na.rm = T)),
         No = apply(.,1, function(x) sum(x == "No", na.rm = T)),
         # only sum NAs in exclusion questions PLUS comments
         empty = apply(.[names(.)[grep("meta", names(.)):grep("comments", names(.))]],1, function(x) sum(is.na(x), na.rm = T)),
         tally = Yes+No) %>%
  # choose whichever has most Nos or fewest empty cells (kind of specific to these cases)
  group_by(final_name) %>%
  mutate(keep = (No == max(No)) & empty == min(empty)) %>%
  ungroup()%>%
  # keep only those entries that meet keep criteria
  filter(keep)


# -- prep unproblematic rescored abstracts -----
# see if comments for those not CTW-LD should be appended to more recent score
View(subset(rescored, !flag_tally & EBIOReviewer != "Caitlin")) # looks like people re-entered their comments for re-score (don't append old comments)
# append LD comments to CTW re-scored
okay_rescored <- subset(rescored, !Number %in% conflict_rescore$Number) %>%
  # append LD to comments if reviewed by LD and comment not NA
  mutate(comments = ifelse(EBIOReviewer == "Caitlin" & tally != max(tally) & !is.na(comments), paste("LD:", comments), comments)) %>%
  group_by(final_name) %>%
  # str_flatten won't collapse comments within paper if one cell is NA, so leave CTW re-eval'd comment for now
  mutate(comments2 = ifelse(EBIOReviewer == "Caitlin", str_flatten(comments, collapse = "; "), comments)) %>%
  # filter to most recent eval within paper
  filter(tally == max(tally)) %>%
  ungroup() %>%
  # clean up comments  
  mutate(comments2 = trimws(gsub("; CTW .* Q8$", "", comments2)),
         # if only comment is CTW re-evaluated article for Q8 or something like that, NA
         comments = ifelse(grepl("^CTW .* for Q8$", comments), NA, comments),
         comments = ifelse(!is.na(comments2), comments2, comments),
         # update reviewer
         EBIOReviewer = recode(EBIOReviewer, "Caitlin" ="Laura/Caitlin"))

# stack treated rescored
final_rescore <- dplyr::select(okay_rescored, -comments2) %>% rbind(conflict_rescore[names(.)])



# -- COMPILE PREPPED SCORING, CREATE FLAGS, SEPARATE FOR PROCESSING -----
# restack single-reviewed and re-eval'd cleaned up results
# recrunch Yes and No tallies as I calculated them a few ways above for flagging
results_clean <- rbind(singlerev, final_rescore[names(singlerev)]) %>%
  mutate(Yes = apply(.,1, function(x) sum(x == "Yes", na.rm = T)),
         No = apply(.,1, function(x) sum(x == "No", na.rm = T)),
         # only sum NAs in exclusion questions PLUS comments
         empty = apply(.[names(.)[grep("meta", names(.)):grep("tool", names(.))]],1, function(x) sum(is.na(x), na.rm = T)),
         tally = Yes+No) %>%
  # remove attr names created during apply
  mutate_at(vars("Yes", "No", "empty", "tally"), as.numeric) %>%
  # bring in authors to check for presence in Meets Criteria
  left_join(distinct(assignmentsdf[c("Title", "AuthorsFull")]), by = c("final_name" = "Title")) %>% # may want to also join pubdate or pubyear to search meets criteria folder
  mutate(FirstAuthor = ifelse(grepl(", ", AuthorsFull), word(AuthorsFull, sep = ","), word(AuthorsFull)),
         # remove white space
         FirstAuthor = trimws(FirstAuthor)) %>%
  # check for author in Meets Criteria (won't be perfect bc not everyone named files by first author last name and year)
  group_by(final_name) %>%
  # this is a pretty coarse check and not perfect, but at least quick screen
  mutate(in_meetscriteria = sum(grepl(FirstAuthor, meetscriteria$name))) %>%
  ungroup() %>%
  data.frame()

# triage: if any papers are incomplete-eval'd (i.e. NAs exists, for all but biodiv q) but paper exists in meet criteria folder, infill NAs with NO
incomplete <- which(results_clean$Yes == 0 & results_clean$empty > 0)
for(i in incomplete){
  if(results_clean$in_meetscriteria[i]>0){
    # could generically assign No to all cols, but to be sure only infill confirmed NAs with No
    results_clean[i, names(results_clean)[grep("meta", names(results_clean)):grep("tool", names(results_clean))]] <- sapply(results_clean[i, names(results_clean)[grep("meta", names(results_clean)):grep("tool", names(results_clean))]], function(x) ifelse(is.na(x), "No", x))
  }
}
# continue with flagging create col for exlusion
results_clean  <- results_clean %>%
  #recrunch tallies
  mutate(Yes = apply(.,1, function(x) sum(x == "Yes", na.rm = T)),
         No = apply(.,1, function(x) sum(x == "No", na.rm = T)),
         # only sum NAs in exclusion questions PLUS comments
         empty = apply(.[names(.)[grep("meta", names(.)):grep("tool", names(.))]],1, function(x) sum(is.na(x), na.rm = T)),
         tally = Yes+No) %>%
  # remove attr names created during apply
  mutate_at(vars("Yes", "No", "empty", "tally"), as.numeric) %>%
  data.frame() %>%
  # create flagging
  mutate(exclude = Yes > 0,
         # create flagging
         ## flag 1 is if only NOs answered and rest are NAs
         ## flag 2 is any NAs in q1-7
         flag_NO = No > 0 & Yes == 0 & empty > 0,
         flag_NA = empty > 0,
         flag_Q8 = is.na(biodiv),
         # flag if paper should be in folder and isn't
         flag_paper = in_meetscriteria == 0 & Yes == 0 & No >= 5)


# separate exclude and needs further review from keep for to round 2
exclude <- subset(results_clean, exclude)
questions <- subset(results_clean, flag_NO)
keep <- subset(results_clean, (!exclude | is.na(exclude)) & !flag_NO)

# check all papers accounted for
nrow(results_clean) == sum(nrow(exclude), nrow(questions), nrow(keep)) # <-- this should be TRUE before proceeding



# -- SUMMARIZE EXCLUSIONS ---- 
dplyr::select(exclude,Number, meta:biodiv) %>%
  data.frame() %>%
  gather(question, answer, meta:biodiv) %>%
  mutate(answer = ifelse(is.na(answer), "NA", answer)) %>%
  group_by(question, answer) %>%
  summarise(nobs = length(Number)) %>%
  ungroup() %>%
  mutate(question = factor(question, levels = c("meta", "review", "no_efes", "tool", "valrisk", "biodiv"),
                           labels = c("Is meta-analysis", "Is review", "No EF/ES", "Tool only", "Valuation/risk only", "Biodiv not EF/ES proxy"))) %>%
  ggplot(aes(answer, nobs)) +
  geom_col() +
  labs(y = "Count", x = "Response",
       title = paste0("Excluded abstracts: summary of round 1 responses"),
       subtitle = paste0("Last updated: ", Sys.Date(),", ", nrow(exclude), " excluded of ", nrow(results_clean), " reviewed")) +
  facet_wrap(~question)

# write out 
ggsave("figs/excluded_abstracts_summary.pdf", width = 6, height = 5, units = "in")



# -- PARSE KEEP PAPERS ----
# placeholder section for any future data treatment needed for keep papers (e.g. manual corrections on coding outstanding question abstracts)




# -- RANDOM ASSIGN ROUND 2 PAPERS -----
# write out to google sheets? or csv?
# rules: 
## if reviewed paper in round 1, cannot review same paper in round 2
## everyone gets roughly equal amount of papers to review
## random number assign

reviewers <- unique(master2$EBIOReviewer)
                    
assign_round2 <- dplyr::select(keep, Number, EBIOReviewer, final_name, AuthorsFull, FirstAuthor, comments, in_meetscriteria) %>%
  rename(actual_reviewer = EBIOReviewer) %>%
  mutate(EBIOReviewer = ifelse(grepl("Laura|Caitl", actual_reviewer), "Caitlin", actual_reviewer),
         round2_reviewer = NA) %>%
  dplyr::select(round2_reviewer, Number, EBIOReviewer, actual_reviewer, comments, final_name:ncol(.))

# start with unique numbers
numbers <- unique(keep$Number)
for(r in reviewers){
  # random sample papers in keep not previously reviewed by person r
  select <- sample(numbers[!numbers %in% assign_round2$Number[assign_round2$EBIOReviewer == r]], size = floor(nrow(keep)/length(reviewers)))
  # assign person r to selected numbers
  assign_round2$round2_reviewer[assign_round2$Number %in% select] <- r
  #update numbers before next iteration (remove papers selected)
  numbers <- numbers[!numbers %in% select]
}

# for remaining unassigned papers (if not equal amount), random draw who will review
for(n in numbers){
  # random sample papers in keep not previously reviewed by person r
  select <- sample(reviewers[!reviewers == assign_round2$EBIOReviewer[assign_round2$Number == n]], size = 1)
  # assign person r to selected numbers
  assign_round2$round2_reviewer[assign_round2$Number == n] <- select
  #update reviewers before next iteration (remove person selected)
  reviewers <- reviewers[!reviewers == select]
}

# check all assigned
summary(is.na(assign_round2$round2_reviewer)) # looks good
# check no one reviewing something they already have
summary(assign_round2$round2_reviewer == assign_round2$EBIOReviewer) # looks good

# clean up before writing out
assign_round2 <- rename(assign_round2, round1_reviewer = actual_reviewer, Title = final_name) %>%
  # join full citation info
  left_join(assignmentsdf, by = c("EBIOReviewer", "Title", "Number", "AuthorsFull")) %>%
  # clean up
  dplyr::select(round2_reviewer, Number, in_meetscriteria, round1_reviewer, comments, FirstAuthor, Title:PublicationYear) %>%
  # redo in_meets criteria using first author last name and year of publication
  mutate(searchterm = ifelse(is.na(PublicationYear), FirstAuthor, paste0(FirstAuthor,".*", PublicationYear))) %>%
  group_by(Number) %>%
  mutate(in_meetscriteria = sum(grepl(searchterm, meetscriteria$name))) %>%
  ungroup() %>%
  rename(MeetsCriteria_matches = in_meetscriteria) %>%
  dplyr::select(-searchterm) %>%
  # capitalize first letter in colnames
  rename_all(function(x) paste0(casefold(substr(x,1,1), upper = T), substr(x, 2, nchar(x)))) %>%
  # sort by reviewer 2 name, first author, and year since that's how articles are saved in meets criteria folder
  arrange(Round2_reviewer, FirstAuthor, PublicationYear)

# write out
write.csv(assign_round2, "review_assignments_round2.csv", row.names = F)



# -- PROGRESS REPORT ----
# all abstracts done? who still needs to complete if not?
summary(unique(assignmentsdf$Title) %in% results_clean$final_name)
sapply(split(assignmentsdf$Title, assignmentsdf$EBIOReviewer), function(x) summary(x %in% results_clean$final_name))
# 12/28: Grant, Caitlin, Nick done (yay!) .. emailed Aislyn with outstanding paper

# Aislyn missing papers
assignmentsdf$Title[assignmentsdf$EBIOReviewer == "Aislyn" & !assignmentsdf$Title %in% results_clean$final_name]
# not master.. still need to be coded
# Laurel also missing one
assignmentsdf$Title[assignmentsdf$EBIOReviewer == "Laurel" & !assignmentsdf$Title %in% results_clean$final_name]

# write out still needs review if others want to check it
needs_review <- subset(assignmentsdf, !assignmentsdf$Title %in% results_clean$final_name)

