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
View(subset(singlerev, EBIOReviewer != "Caitlin")) %>%
  dplyr::select(meta:tool) %>%
  mutate_all(as.factor) %>%
  summary()
# were q5-q7 not scored bc scored Yes for meta-analysis or review question?
View(subset(singlerev, EBIOReviewer != "Caitlin" & (meta == "Yes" | review == "Yes"))) %>%
  dplyr::select(meta:tool) %>%
  mutate_all(as.factor) %>%
  summary() # not necessarily..
# how many rows have NAs (excluding Q8) when at least one question scored YES, or all else NO
test <- dplyr::select(singlerev, EBIOReviewer, rank, meta:tool) %>%
  subset(complete.cases(.) == F) %>%
  mutate(Yes = apply(.,1, function(x) sum(x == "Yes", na.rm = T)),
         No = apply(.,1, function(x) sum(x == "No", na.rm = T)),
         empty = apply(.,1, function(x) sum(is.na(x), na.rm = T)),
         tally = Yes+No)
  
sapply(with(singlerev, singlerev[EBIOReviewer == "Caitlin" & is.na(names(singlerev)), names(singlerev)[13:18]]), function(x) summary(as.factor(x)))
View(subset(singlerev, EBIOReviewer == "Caitlin"))

# q8 NA vs answered
table(is.na(singlerev$`Q8: This paper only measures biodiversity/abundance but NOT as an explicit proxy for ES/EF`))




# -- ADDRESS RESCORED RECORDS -----

