# compile results from class coding round 1 (exclusion screening)
# author(s): CTW (caitlin.t.white@colorado.edu)

# script workflow:
# search G Drive for all instances of exclusion criteria coding form results (as of 12/23/19, there are at least 4-5 copies)
# compare coding results files and distill to unique rows
# prioritize more recent scoring for any papers reviewed twice (e.g. papers re-scored to answer Q8)
# sort: 1) set of papers excluded, 2) final set of papers that make it through to round 2 coding, 3) any scoring that needs follow-up (e.g. conflicting answers)
# output: 
## 1) random-assignly round 2 papers to class list, write out to repo and GDrive round 2 review subfolder
### > 1/29/20: we decided to compromise around logistic ease, reduce bias + make worload manageable: rando subsample 50% papers, make rando groups of 3, each person splits 1/2 of their papers with someone else in their group of three (so in total 2 ppl reviewing same paper for every paper subsampled)
## 2) summary figure out abstracts excluded and why (figure written out to figs subfolder in repository)


# notes:
# 1/15/20 (class meeting): 
## Sierra, Aislyn and Nick confirmed in class: if NA can assume NO in their abstracts questions (code needs to infill)
## we decided everyone will go back and score any NAs for q8 only IFF the paper would otherwise make it round 2
## if abstract is excluded based on some other question, ignore any NAs that exist for that record




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
## with next line, can choose to cache Google credentials on your machine or not.. caching means you won't have to login each time you run code
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


# read in round 1 paper assignments
ES_folder <- drive_find(pattern = "EBIO: Ecology of Ecosystem Services", type = "folder", n_max = 10) %>% drive_ls()
ES_folder
abstracts_folder <- drive_get(id = ES_folder$id[grep("^Abstract Review", ES_folder$name)]) %>% drive_ls()
abstracts_folder
assignments <- drive_get(id = abstracts_folder$id[grep("^EcosystemServicesPapersNov2019.xls", abstracts_folder$name)]) # first should be assignments
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
# 1/22: we moved this folder from abstracts review to round 2 subfolder
# > note: drive_ls() doesn't appear to consistently list all files within the meets criteria folder, e.g. ranges from 406-429 (with duplicates listed for the higher end range)
round2_folder <-drive_get(id = ES_folder$id[grep("^Round 2", ES_folder$name)]) %>% drive_ls()  
round2_folder
meetscriteria <- drive_get(id = round2_folder$id[round2_folder$name == "Meets Criteria"]) %>% 
  drive_ls()
# remove duplicated files
meetscriteria <- filter(meetscriteria, !duplicated(id))


# read in any abstracts re-evald for Q8 or NAs (in Excel or method other than Google Form)
## locate folder
reval_files <- drive_get(id = abstracts_folder$id[grep("^Round1_NA", abstracts_folder$name)]) %>% drive_ls()
# initiate empty list for storing different people's responses
revalls <- list()
# iterate through each file
for(pos in 1:nrow(reval_files)){
  # extract file name
  temp_name <- reval_files$name[pos]
  # print status
  print(paste("Reading in",temp_name))
  #if excel spreadsheet, temp download file, read in file to list, and delete temp file
  if(grepl("xls", temp_name)){
    # specify extension type
    tempext <- str_extract(temp_name, pattern = "xls[x]?$") 
    tempxl <- tempfile(fileext = tempext)
    drive_download(as_id(reval_files$id[pos]), path = tempxl, overwrite = T) # downloads paper assignments to github repo.. until can figure out how to read excel directly from gdrive..
    revalls[[pos]] <- read_excel(tempxl, sheet = 1) 
    # name item in list
    names(revalls)[pos] <- temp_name
    # delete file downloaded locally
    file.remove(tempxl)
  }
  #if googlesheet, read sheet directly
  if(grepl("PotentialRound2", temp_name)){
    revalls[[pos]] <- read_sheet(reval_files$id[reval_files$name == temp_name])
    names(revalls)[pos] <- temp_name
  }
}



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
## 1 and 3 have a column ...9 (??)
View(resultsls[[1]])
sapply(resultsls[[1]], function(x) summary(is.na(x))) # mystery ...9 col is empty..
# how many rows empty? (excluding timestamp, title, and last 5 file ID cols I added)
summary(apply(resultsls[[1]][,3:(ncol(resultsls[[2]])-5)], 1, function(x) all(is.na(x)))) # only 1
#check for empty cols in all
lapply(resultsls, function(x) summary(is.na(x)))

# check out re-evaluated abstracts in NA folder
glimpse(revalls[[1]]) # googlesheet CTW made for class (anyone listed on there just supposed to infill Q8 in the googlesheet)
summary(as.factor(revalls[[1]]$Q8_Biodiversity)) # <- when answered, all NO
# who hasn't answered yet? or who used this form?
sapply(split(revalls[[1]]$Q8_Biodiversity, revalls[[1]]$EBIOReviewer), function(x) summary(as.factor(x)))
## Aislyn added herself -- remove bc using her spreadsheet; 
## Anna, Kathryn, Laurel have all NAs.. check to see if they rescored in the Google form

glimpse(revalls[[2]]) # aislyn's
summary(as.factor(revalls[[2]]$Q8)) # <- when answered, all NO

glimpse(revalls[[3]]) # nick's
summary(as.factor(revalls[[3]]$`Q8:Biodiversity`)) # mix, need to standardize char-casing



# -- COMPILE RESULTS INTO MASTER ----
# as compile, 1) remove empty cols, make sure has cols equivalent to most recent version
# gather times created and last modified..
times <- unlist(lapply(results$drive_resource, function(x) x[names(x) == "createdTime"])) %>% gsub("T|Z", " ", .) %>% as.POSIXct()
modtimes <- unlist(lapply(results$drive_resource, function(x) x[names(x) == "modifiedTime"])) %>% gsub("T|Z", " ", .) %>% as.POSIXct()
records <- sapply(resultsls, nrow)
which.max(times) # results with most recent create time not the results most recently modified 
which.max(modtimes) # go with mod times for prioritizing results
refdf <- data.frame(createdTime = times, modifiedTime = modtimes, `records` = records) %>%
  arrange(desc(records), desc(modifiedTime), desc(createdTime)) %>%
  # assign priority rank
  mutate(rank = seq(1,nrow(.), 1))

# stack all google form dats
master <- data.frame()
# > note: this will throw warning mssgs but it's okay
for(i in 1:length(resultsls)){
  tempdat <- resultsls[[i]]
  # ensure Title col named Title to match with assignemnts dat
  names(tempdat)[grepl("Title", names(tempdat), ignore.case = T)] <- "Title"
  # remove any cols that are all NA
  tempdat <- tempdat[,!sapply(tempdat, function(x) all(is.na(x)))]
  # if all colnames agree or is first instance, rbind as is
  if(all(names(master) == names(tempdat)) | nrow(master) == 0){ # <-- this line throws a warning, it's fine. error is string vectors being compared are not same length; some results have different number columns.
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
glimpse(master) # fix title col as list (all questions NA and title == NULL), also clean up Q8 name mismatch (someone edited colanme in one of the files since I wrote the code..)
#ID which rows have no entries
rowNA <- apply(master[,grep("^Q[0-9]|[?]|Comments",names(master))], 1, function(x)all(is.na(x)))
View(master[rowNA,]) #good
# remove empty rows or rows with NULL, then unlist title values
master <- master[!rowNA,]
#master$Title <- unlist(master$Title) #1/20 -- no longer works because of NULL in title (if title not entered in google form)
tempdf <- data.frame(rowid = rownames(master), Timestamp = master$Timestamp, Title2 = NA)
for(i in 1:nrow(master)){
  val <- master$Title[[i]] # if someone didn't enter title, response is NULL which creates problems in R code..
  if(is.null(val)){
    val <- "Error: No title provided"
  }
  tempdf$Title2[i] <- val
}
master <- master %>%
  # to be sure binding with correct row (can use cbind, but being extra cautious..)
  mutate(rowid = rownames(.)) %>%
  left_join(tempdf) %>%
  mutate(Title = Title2) %>%
  dplyr::select(-c(rowid:Title2))
str(master) # okay now
# what are the unique vals in each Q8 col?
sapply(master[grepl("Q8", names(master))], function(x) summary(as.factor(x))) # there are only 2 NOs in last q8 question, all else na..
subset(master, !is.na(`Q8: This paper only measures diversity/abundance but NOT as an explicit proxy for ES/EF`))
# assign non-NAs to main Q8
master <- mutate(master,
                 #mouthfull of a colname.. if the alt QA question is not NA, assign answer to main question, else leave main question response be
                 `Q8: This paper only measures biodiversity/abundance but NOT as an explicit proxy for ES/EF` = ifelse(!is.na(`Q8: This paper only measures diversity/abundance but NOT as an explicit proxy for ES/EF`), 
                                                                                                                       `Q8: This paper only measures diversity/abundance but NOT as an explicit proxy for ES/EF`,
                                                                                                                       `Q8: This paper only measures biodiversity/abundance but NOT as an explicit proxy for ES/EF`)) %>%
  # drop alt Q8
  dplyr::select(-`Q8: This paper only measures diversity/abundance but NOT as an explicit proxy for ES/EF`)
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
# run check on title mispell
for(n in namecheck$Title[is.na(namecheck$final_name) & !is.na(namecheck$Title)]) {
  words <- strsplit(n, " ") %>% unlist()
  # look for max word match
  ncheck <- sapply(assignmentsdf$Title, function(x) sum(words %in% unlist(strsplit(x, " "))))
  if(max(ncheck) > length(words)*.7){
    # replace title
    namecheck$final_name[namecheck$Title == n & !is.na(namecheck$Title)] <- assignmentsdf$Title[which.max(ncheck)]
    next
  }
  # try partial character match
  tempchars <- casefold(unlist(str_extract_all(n, "[:alpha:]")))
  # if any alpha-character-extracted title is length 0, next (happens for title 1190)
  if(length(tempchars) == 0){
    next
  }
  # extract final titles not assigned yet
  needs_match <- assignmentsdf$Title[!assignmentsdf$Title %in% namecheck$final_name]
  for(i in needs_match){
    # split out characters in available title that needs a match
    comparechars <- casefold(unlist(str_extract_all(i, "[:alpha:]")))
    # run partial match
    match1 <- pmatch(comparechars, tempchars)
    # if partial match results in sequential order (i.e. is the same as if sorted from low to hi, assign title)
    ## > if title is a match, characters should match in sequential order of comparison
    if(all(match1[!is.na(match1)] == sort(match1[!is.na(match1)]))){
      namecheck$final_name[namecheck$Title == n & !is.na(namecheck$Title)] <- i
      next
    }
  }
  # try matching abstract if partial title matches don't work
  if(!is.na(pmatch(n,assignmentsdf$Abstract)) & pmatch(n, assignmentsdf$Abstract) > 0){
    namecheck$final_name[namecheck$Title == n] <- assignmentsdf$Title[pmatch(n, assignmentsdf$Abstract)]
  }
}
# who still needs match?
namecheck$Title[is.na(namecheck$final_name)]
# what is NA title in master?
View(master[is.na(master$Title),])  # can ignore november entries, looks like initial testing before launching coding doc
# ditch NA title in namecheck
namecheck <- subset(namecheck, !is.na(Title))
# how many have no title entered?
master[master$Title == "Error: No title provided",] #2, others are NAs
# manual corrections on the rest..
## 1190 = ref num
namecheck$final_name[namecheck$Title == "1190"] <- assignmentsdf$Title[assignmentsdf$Number == 1190]
## SUDS
namecheck$final_name[grepl("SUDS", namecheck$Title)] <- assignmentsdf$Title[grepl("SUDS", assignmentsdf$Abstract)]
## bat comment -- check LD abstracts sheet -- can get it from matching comment in master
namecheck$final_name[grepl("Only really focused on bat", namecheck$Title)] <- unique(master$Title[grepl("Only really focused on bat", master$`Comments on the paper or any questions that are unclear`)])
## "kathyrn abstracts" 
namecheck$final_name[grepl("Kathryn", namecheck$Title)] <- assignmentsdf$Title[grepl("Ecosystem change assessment", assignmentsdf$Title) & assignmentsdf$EBIOReviewer == "Kathryn"]
## TRY -- I can't find any paper assigned to LB that's on TRY database.. given it was from first version of coding form and only presents in that one record, maybe was just a test entry.. going to ignore
## SOIL CARBON SEQUESTRATION... -- article reviewed by Aislyn is wrong article (correct author, wrong article) -- can ignore
## Effects of dams..
# > This corresponds to title "eEde predator-prey interactions" in assignmentsdf
master$`Comments on the paper or any questions that are unclear`[grepl("Effects of dams on downs", master$Title)]
# > For selecting correct row of answers, keep assignmentsdf title (eEde..) for now, but apply correct title at end for round2 write-out
namecheck$final_name[grepl("Effects of dams on downstream moll", namecheck$Title)] <- assignmentsdf$Title[grepl("eEde pred", assignmentsdf$Title)]
# reassign reviewers and record nums
namecheck <- namecheck[c("Title", "final_name", "reviewed")] %>%
  left_join(dplyr::select(assignmentsdf, EBIOReviewer, Number, Title), by = c("final_name" = "Title")) %>%
  distinct()
# review what's left with NAs
subset(namecheck, is.na(final_name)) # okay -- ignore TRY and SOIL CARBON SEQUESTRATION (reviewed wrong paper, not assigned), nothing to do about no title.. whoever that is will have to re-enter

# distill to unique records
# select colnames on which to check dups
qnames <- names(master)[grep("[?]|^Q[0-9]|Comments|Titl", names(master))]
# use second copy to preserve original
# append reviewer and clean title info
master2 <-  left_join(master, distinct(namecheck[c("Title", "final_name", "EBIOReviewer", "Number")])) %>% # added 4 extra rows.. not sure why
  # remove any record that doesn't have review Number assigned -- no match in assignmnets df
  filter(!(is.na(Number))) %>%
  # sort dat by date modified so remove earlier duplicate entries
  arrange(final_name, rank, desc(Timestamp)) %>%
  # reorder cols and ditch email address (only contains 4 unique addresses and have all names now anyway)
  dplyr::select(filename:records, rank, EBIOReviewer, Number, Timestamp, `Email Address`, final_name, Title, names(.)[grep("meta-analy", names(.))]:names(.)[grep("^Q7", names(.))], names(.)[grep("^Q8", names(.))], names(.)[grep("^Comments", names(.))]) %>%
  distinct()

# remove duplicate records (removes 2nd+ instance)
masterdups <- master2[(duplicated(master2[,qnames])),] # keep just in case need
master2 <- master2[!(duplicated(master2[,qnames])),]

# make abbrev colnames for review
nameref <- data.frame(orig = names(master2),
                      abbr = c(names(master2)[1:grep("Title", names(master2))],"meta", "review", "no_efes", "valrisk", "tool", "biodiv", "comments")) %>%
  mutate(abbr = ifelse(grepl("Emai", abbr), "email", abbr))

# make new version to manipulate
master3 <- master2
names(master3) <- nameref$abbr


# append/infill reval'd Q8 answers to master3
View(revalls[[1]])
# perhaps iterate through master 3.. if Q8 is NA, then infill with googlesheet by *final_name* (clean title) and EBIOReviewer
# also review Nick's separately bc he also has answers to questions 1-7, can double check no NAs in those or infill with answers from his excel sheet
# > infill AS's q8 answers here because if try to triage further down in code run into issues merging comments from multiple scores due to not using the same google form (it's less of a pain to take care of it here)

# 1) check to see if Nick's answers agree with what's in google form.. if so, can append answers to googlesheet
nick_q8 <- revalls[[grep("Dragone", names(revalls))]] %>% dplyr::select(-Timestamp) %>% rename(Title = 'Q1: Paper Title')
#bc will join by q1-q7, if nick's answers agree with what's in master, everything will pair
agrees <- left_join(nick_q8, distinct(master2[master2$EBIOReviewer == "Nick", !colnames(master2) %in% c("Timestamp", names(refdf))])) %>% subset(!is.na(filename)) %>% distinct()
summary(is.na(agrees$filename)) #no NAs from joining master, all answers agree, can append q8 answers to googlesheet

# 2) rbind all q8 responses, remove any NAs in googlesheet, and standardize responses
q8answers <- revalls[[grep("PotentialRound", names(revalls))]] %>% 
  # remove FirstAuthor to rbind with Aislyn's excel sheet
  # get rid of Anna's added column
  dplyr::select(-c(FirstAuthor, Answered_Q8)) %>%
  # remove Aislyn bc I have her answers elsewhere
  filter(!EBIOReviewer == "Aislyn") %>%
  #rename Q8 to match Aislyn's spreadsheet for rbinding
  #rbind Aislyn's answers from her excel sheet (rename her colnames)
  rbind(setNames(revalls[[grep("Aislyn", names(revalls))]][c("EBIOReviewer", "Number", "Title", "Q8")], names(.))) %>%
  # append Nick's answers from joined df above (bind Nick's q8 column tho)
  rbind(setNames(agrees[,c("EBIOReviewer", "Number", "final_name", "Q8:Biodiversity")], names(.))) %>%
  # append Anna's answers
  left_join(master3[master3$EBIOReviewer == "Anna" & !is.na(master3$biodiv), c("EBIOReviewer", "Number", "Title", "biodiv")]) %>%
  mutate(Q8_Biodiversity = ifelse(EBIOReviewer == "Anna", biodiv, Q8_Biodiversity)) %>%
  #drop biodiv
  dplyr::select(-biodiv) %>%
  # clean up data frame
  subset(Q8_Biodiversity != "NA" & !is.na(Q8_Biodiversity)) %>%
  #standardize character casing (first letter = capital, rest = lowcase)
  mutate(Q8_Biodiversity = paste0(casefold(substr(Q8_Biodiversity, 1,1), upper = T), casefold(substr(Q8_Biodiversity, 2, nchar(Q8_Biodiversity)))))


# 3) infill q8 answers
for(i in 1:nrow(q8answers)){
  temprow <- left_join(q8answers[i,], cbind(master3, rowid = rownames(master3)), by = c("EBIOReviewer","Number")) %>%
    # just in case multiple rows, sort by timestamp and rank, choose first row
    arrange(desc(Timestamp), rank) %>% 
    # choose most recent
    filter(rank ==min (rank)) %>%
    filter(Timestamp == max(Timestamp)) %>%
    data.frame()
  stopifnot(nrow(temprow) == 1)
  # if biodiv in master is NA, infill with response from googlesheet
  if(is.na(temprow$biodiv)){
    master3$biodiv[rownames(master3) == temprow$rowid] <- temprow$Q8_Biodiversity
  }else{
    # if not NA, check that answer agrees with what they answered in googlesheet
    stopifnot(temprow$biodiv == temprow$Q8_Biodiversity)
  }
}

# also.. because Aislyn lists yes/no exclued in her sheet, infill any NAs for q1-8 with NO if she indicates paper should not be excluded
check_NA <- filter(master3, EBIOReviewer == "Aislyn") %>%
  mutate(hasNA = apply(.[names(.)[grep("meta", names(.)):grep("biodiv", names(.))]],1, function(x) any(is.na(x)))) %>%
  filter(hasNA) %>%
  # join Aislyn's exclusion values
  left_join(revalls[[2]][c("Number", "Excluded?")])
# maybe save for later down in code.. let the code select the most recent/most complete version and if there are still NAs after that look at Aislyn's answers
## e.g. the cultural ES paper here is excluded based on no direct EF/ES in q3, but record that has NA is an earlier version of her scoring, which would get thrown out in the code

# extract Aislyn's rescoring in its own df
aislyn_reval <- revalls[[grep("Aislyn", names(revalls))]]



# -- SPLIT ABSTRACTS BASED ON TIMES SCORED-----
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
# look at NAs in non-CTW rows
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
  summarise(nobs = length(Number)) #nick and sierra only (1/8)
# update actual reviewer for Caitlin-assigned abstracts (LD reviewed some)
singlerev <- singlerev %>%
  # use LD/CTW abstracts form
  left_join(abstracts_LD[c("Reviewer", "Title")], by = c("final_name" = "Title")) %>%
  # standardize Laura name first
  mutate(Reviewer = ifelse(!grepl("Cait", Reviewer), "Laura", Reviewer),
         EBIOReviewer = ifelse(EBIOReviewer == "Caitlin", Reviewer, EBIOReviewer)) %>%
  dplyr::select(-Reviewer)

# q8 NA vs answered (12/26-- wrote group to ask if people will re-eval or not)
table(is.na(singlerev$biodiv)) # more answered than not answered




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
## Sierra changed 2 papers to YES for no EF/ES directly measured
## Travis' paper lacks answer for most recent scoring (can infill with earlier version)
## Anna is only answering q8 in most recent Timestamp, using an old form; appended her q8 answers to more complete records (above)
conflict_rescore <- subset(rescored, flag_tally) %>% 
  # sort by title and Timestamp (earlier appears first for filling down NAs)
  arrange(final_name, Timestamp) %>%
  group_by(final_name) %>%
  # infill later Timestamp NAs with earlier values (by paper, by question) if present
  fill(meta, review, no_efes, tool, valrisk) %>%
  ungroup() %>%
  # 1/20/20: new way -- use Aislyn's exclude yes/no col to tease which row to keep (she has papers in meets criteria folder, but last score and her excel sheet says to exclude)
  # old way: keep Aislyn's NO to question 3 since those articles are in meets criteria folder (write line to search for authors in files within meets criteria folder, i.e. automate)
  left_join(distinct(assignmentsdf[c("Title", "AuthorsFull")]), by = c("final_name" = "Title")) %>%
  # extract First author last name to search Meets Criteria folder
  mutate(FirstAuthor = word(AuthorsFull, sep = ",")) %>%
  group_by(final_name) %>%
  mutate(in_meetscriteria = sum(grepl(paste0("^", unique(FirstAuthor), "(?![a-z])"), meetscriteria$name, ignore.case = T, perl = T))) %>%
  ungroup() %>%
  #recrunch tallies
  mutate(Yes = apply(.,1, function(x) sum(x == "Yes", na.rm = T)),
         No = apply(.,1, function(x) sum(x == "No", na.rm = T)),
         # only sum NAs in exclusion questions PLUS comments
         empty = apply(.[names(.)[grep("meta", names(.)):grep("comments", names(.))]],1, function(x) sum(is.na(x), na.rm = T)),
         tally = Yes+No) %>% 
  # add in Aislyn's excluded column
  left_join(aislyn_reval[c("EBIOReviewer", "Number", "Excluded?")])


# rules for choosing:
## 1) has the most questions answered
## 2) if tally tied, and has at least 1 Yes and not in Meets Criteria, keep
## 3) if tally tied, and if all NOs and in Meets Criteria, keep
## 4) choose by most recent Timestamp -- Timestamp not necessarily best filter for this (e.g. see Aislym's Pueyo-Ros example, or Isabel's example)
conflict_select <- conflict_rescore %>%  
  group_by(final_name) %>%
  mutate(keep = (tally == max(tally) & empty == min(empty) & Timestamp == max(Timestamp)),
         sumcheck = sum(keep),
         # if nothing selected and all else equal, choose whichever has most answers)
         keep = ifelse(sumcheck != 1, (tally == max(tally)& empty == min(empty)), keep)) %>%
  #filter(keep)
  filter(keep | (!keep & !is.na(comments))) %>%
  # combine comments that are different (whatever is being dropped comes after what's being kept)
  mutate(unique_comments = length(unique(comments)),
         comments = ifelse(unique_comments >1, paste(comments[keep], comments[!keep], sep = "; "), comments),
         #recrunch sumcheck
         sumcheck = sum(keep)) %>%
  ungroup() %>%
  filter(keep)
# infill Anna's NAs
conflict_select[conflict_select$EBIOReviewer == "Anna" & conflict_select$tally < 6 & conflict_select$in_meetscriteria == 1, c("no_efes", "valrisk", "tool")] <- "No"



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
final_rescore <- dplyr::select(okay_rescored, -comments2) %>% rbind(conflict_select[names(.)])



# -- COMPILE PREPPED SCORING, CREATE FLAGS, SEPARATE FOR PROCESSING -----
# restack single-reviewed and re-eval'd cleaned up results
# > infill Sierra's Nos here  -- she said in class that if all her q1-q7 are no, then q8 should be no
# recrunch Yes and No tallies as I calculated them a few ways above for flagging
results_clean <- rbind(singlerev, final_rescore[names(singlerev)]) %>%
  mutate(Yes = apply(.,1, function(x) sum(x == "Yes", na.rm = T)),
         No = apply(.,1, function(x) sum(x == "No", na.rm = T)),
         # only sum NAs in exclusion questions PLUS comments
         empty = apply(.[names(.)[grep("meta", names(.)):grep("tool", names(.))]],1, function(x) sum(is.na(x), na.rm = T)),
         tally = Yes+No) %>%
  # remove attr names created during apply
  mutate_at(vars("Yes", "No", "empty", "tally"), as.numeric) %>%
  # infill Sierra's No's -- if Yes tally = 0, Reviewer == Sierra and biodiv is NA, then no, else leave biodiv as is
  mutate(biodiv = ifelse(Yes == 0 & EBIOReviewer == "Sierra" & is.na(biodiv), "No", biodiv)) %>%
  # bring in authors to check for presence in Meets Criteria
  left_join(distinct(assignmentsdf[c("Title", "AuthorsFull")]), by = c("final_name" = "Title")) %>% # may want to also join pubdate or pubyear to search meets criteria folder
  mutate(FirstAuthor = ifelse(grepl(", ", AuthorsFull), word(AuthorsFull, sep = ","), word(AuthorsFull)),
         # remove white space
         FirstAuthor = trimws(FirstAuthor)) %>%
  # check for author in Meets Criteria (won't be perfect bc not everyone named files by first author last name and year)
  group_by(final_name) %>%
  # this is a pretty coarse check and not perfect, but at least quick screen
  mutate(in_meetscriteria = sum(grepl(paste0("^", FirstAuthor, "(?![a-z])"), meetscriteria$name, ignore.case = T, perl = T))) %>%
  ungroup() %>%
  data.frame()



# triage: if any papers are incomplete-eval'd (i.e. NAs exists, for all but biodiv q) but paper exists in meet criteria folder, infill NAs with NO
incomplete <- which(results_clean$Yes == 0 & results_clean$empty > 0)
for(i in incomplete){
  if(results_clean$in_meetscriteria[i]>0 | results_clean$Number[i] %in% aislyn_reval$Number[grepl("no", aislyn_reval$`Excluded?`, ignore.case = T)]){
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
questions <- subset(results_clean, flag_NO | empty >= 5)
keep <- subset(results_clean, (!exclude | is.na(exclude)) & !flag_NO & !empty>=5)
#keep2 <- anti_join(results_clean, rbind(exclude, questions))

# check all papers accounted for
nrow(results_clean) == sum(nrow(exclude), nrow(questions), nrow(keep)) # <-- this should be TRUE before proceeding
# alternative check all titles accounted for
summary(results_clean$final_name %in% c(exclude$final_name, questions$final_name, keep$final_name)) # <-- should be all TRUE
# check titles against everything read in initially
summary(results_clean$Title %in% unique(master$Title)) # <-- should also be true 

# check that none of aislyn's excluded are in keep/keep in excluded
summary(aislyn_reval$Number[aislyn_reval$`Excluded?` == "yes"] %in% keep$Number)
summary(aislyn_reval$Number[aislyn_reval$`Excluded?` == "no"] %in% keep$Number)

summary(aislyn_reval$Number[aislyn_reval$`Excluded?` == "no"] %in% exclude$Number)
summary(aislyn_reval$Number[aislyn_reval$`Excluded?` == "yes"] %in% exclude$Number)

AK_keepcheck <- left_join(keep, aislyn_reval) %>% filter(`Excluded?` == "yes")
AK_keepcheck$final_name
AK_excludecheck <- left_join(exclude, aislyn_reval) %>% filter(`Excluded?` == "no")
AK_excludecheck$final_name
# 1/22/20: CTW emailed AK to ask for clarification
# 1/22/20: AK emailed CTW these should all be kept for round 2 (except Lal *does* look like a review paper so keeping in exclude), manual edit scores and add to keep df if not already there
AK_excludetokeep <- filter(AK_excludecheck, FirstAuthor != "Lal") %>%
  # these all have Yes for meta-analysis
  mutate(meta = "No") %>%
  #recrunch tallies and flagging
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
         flag_paper = in_meetscriteria == 0 & Yes == 0 & No >= 5) %>%
  dplyr::select(names(keep))

# append these to keep
keep <- rbind(keep, AK_excludetokeep)
# update exclude
exclude <- filter(exclude, !final_name %in% AK_excludetokeep$final_name)



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
# what's average number of kept papers per reviewer?
mutate(keep, EBIOReviewer = ifelse(grepl("Laura", EBIOReviewer), "Caitlin", EBIOReviewer)) %>%
  group_by(EBIOReviewer) %>%
  summarise(papers = length(final_name)) %>%
  ggplot(aes(0, papers)) +
  geom_boxplot() +
  geom_point(aes(color = EBIOReviewer), size = 3, position = position_dodge(width = 0.15), alpha = 0.75, show.legend = F) +
  annotate(geom = "text", x = -0.23, y = 105, label = "Points = unique reviewers", fontface = "italic") +
  scale_color_viridis_d() +
  labs(y = "Count", x = NULL,
       title = paste0("Round 1 summary:\nNumber of unexcluded abstracts per reviewer"),
       subtitle = paste0("Last updated: ", Sys.Date(),", ", nrow(keep), " unexcluded of ", nrow(results_clean), " reviewed")) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# write out 
ggsave("figs/kept_abstracts_summary.pdf", width = 4, height = 5, units = "in", scale = 1.2)


# visualize the NAs in keep papers, similar to excluded
dplyr::select(keep,Number, meta:biodiv) %>%
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
       title = paste0("Kept abstracts: summary of round 1 responses"),
       subtitle = paste0("Last updated: ", Sys.Date(),", ", nrow(keep), " kept of ", nrow(results_clean), " reviewed")) +
  facet_wrap(~question)

# pull out kept abstracts with NA in biodiov to send back to class to answer
keep_biodivNAs <- filter(keep, is.na(biodiv)) %>%
  # append questions
  #rbind(questions) %>%
  # arrange by reviewer
  arrange(EBIOReviewer)

# who still has q8 NA vals in kept papers?
sort(unique(keep_biodivNAs$EBIOReviewer)) # just one NA in Kathryn's paper (didn't fill out GDrive Gsheet, but can assume no)


# which journals tended to have papers passed to round 2?
# > there are 128 unique journals of the starting set of abstracts.. visualizing all of that would be bananas
journaltrends <- left_join(dplyr::select(keep, EBIOReviewer, final_name), distinct(dplyr::select(assignmentsdf, Title, SourcePublication)), by = c("final_name" = "Title"))
# how many unique journals?
length(unique(journaltrends$SourcePublication)) # still a lot..
journalsum <- group_by(journaltrends, SourcePublication) %>%
  summarise(r2papers = length(final_name)) %>%
  arrange(desc(r2papers))

boxplot(journalsum$r2papers) # most 10 papers or fewer.. about 10% of the journals have 20 or more papers
# plot top 10 journals
journalsum[1:20,] %>%
  mutate(SourcePublication = factor(SourcePublication, levels = journalsum$SourcePublication[1:20])) %>%
  ggplot(aes(SourcePublication, r2papers)) +
  geom_col() +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, y = "Number of round 2 papers", 
       title = "Round 1 summary: top 20 journals sourcing papers for round 2, ordered by contribution") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 1))

# write out 
ggsave("figs/kept_abstracts_topjournals.pdf", width = 6, height = 5, units = "in", scale = 1.2)

# what about on a percent pass basis?
gross_journals <- dplyr::select(assignmentsdf, Title, SourcePublication) %>%
  distinct() %>%
  # remove anything not yet reviewed
  filter(Title %in% master3$final_name) %>%
  # tally total papers
  group_by(SourcePublication) %>%
  summarise(tot_papers = length(Title)) %>%
  # join numbers of round 2 papers
  left_join(journalsum[c("SourcePublication", "r2papers")]) %>%
  # any NA means no round 2 papers
  mutate(r2papers = ifelse(is.na(r2papers), 0, r2papers),
         passrate = (r2papers/tot_papers)*100)

#look at top starting contributors (most papers to begin round 1)
subset(gross_journals, tot_papers %in% sort(unique(gross_journals$tot_papers), decreasing = T)[1:20]) %>%
  arrange(desc(tot_papers)) %>%
  mutate(jrank = seq(1, nrow(.), 1),
         SourcePublication = factor(SourcePublication, levels = SourcePublication[ordered(jrank)])) %>%
  ggplot(aes(SourcePublication, passrate, fill = tot_papers)) +
  geom_col(col = "grey60") +
  scale_y_continuous(expand = c(0,0)) +
  labs(x = NULL, y = "Percent of abstracts passed to round 2", 
       title = "Round 1 summary: pass rate for top 20 journals sourcing abstracts for round 1", 
       subtitle = "Ordered and colored by number of starting abstracts") +
  scale_fill_viridis_c(name = "# starting\nabstracts", breaks = seq(20, 150, 30)) +
  coord_flip() +
  theme(plot.title = element_text(hjust = 1),
        plot.subtitle = element_text(hjust = 1))

# write out 
ggsave("figs/kept_abstracts_journalpassrate.pdf", width = 8, height = 6, units = "in", scale = 1.2)



# -- code here is what I used in class on 1/15 to write out googlesheet in the needs8 folder. don't re-run
# # infill Aisyln
# keep_biodivNAs2 <- 
#   q8response <- revalls[[1]] %>%
#   dplyr::select(EBIOReviewer, Number, Title, Q8) %>%
#   rename(Number2 = Number) %>%
#   rbind(data.frame(EBIOReviewer = "Nick", Number2 = NA, Title = revalls[[2]]$`Q1: Paper Title`, Q8 = revalls[[2]]$`Q8:Biodiversity`)) %>%
#   filter(!is.na(Q8)) %>%
#   # standardize casefold
#   mutate(Q8 = paste0(casefold(substr(Q8,1,1), upper = T), casefold(substr(Q8, 2, nchar(Q8))))) %>%
#   left_join(assignmentsdf[c("Title", "Number")]) %>%
#   mutate(Number = ifelse(is.na(Number), Number2, Number)) %>%
#   dplyr::select(-Number2)
# # assign numbers for titles that didn't match
# q8response$Title[is.na(q8response$Number)]
# q8response$Number[grep("Microbial ecological response of the intestinal flora", q8response$Title)] <- assignmentsdf$Number[grep("Microbial ecological response of the intestinal flora", assignmentsdf$Title)]
# 
# keep_biodivNAs2 <- left_join(keep_biodivNAs, q8response[c("EBIOReviewer", "Number", "Q8")], by = c("EBIOReviewer", "Number")) %>%
#   mutate(biodiv = ifelse(is.na(biodiv), Q8, biodiv)) %>%
#   # Sierra said all of hers should be No
#   mutate(biodiv = ifelse(EBIOReviewer == "Sierra", "No", biodiv)) %>%
#   filter(is.na(biodiv) & empty == 0) %>%
#   dplyr::select(EBIOReviewer, Number, final_name, FirstAuthor, biodiv) %>%
#   rename(Q8_Biodiversity = biodiv,
#          Title = final_name) %>%
#   #sort by reviewer and Number
#   arrange(EBIOReviewer, Number)

# # create temp csv file for writing to google drive
# #tempcsv <- tempfile(fileext = ".csv")
# write.csv(keep_biodivNAs2, tempcsv, row.names = F)
# #write temp file to google drive
# drive_upload(tempcsv, path = as_id(abstracts_folder$id[grep("^Round1_NA", abstracts_folder$name)]), type = "spreadsheet", 
#              name = "PotentialRound2Abstracts_NeedsQ8", overwrite = T)
# file.remove(tempcsv)



# -- PROGRESS REPORT ----
# all abstracts done? who still needs to complete if not?
summary(unique(assignmentsdf$Title) %in% results_clean$final_name)
sapply(split(assignmentsdf$Title, assignmentsdf$EBIOReviewer), function(x) summary(x %in% results_clean$final_name))
# 1/24/20: Everyone done! Huzzah!


#assignmentsdf$Title[assignmentsdf$EBIOReviewer == "Claire" & !assignmentsdf$Title %in% results_clean$final_name]


# write out still needs review if others want to check it
#needs_review <- subset(assignmentsdf, !assignmentsdf$Title %in% results_clean$final_name)



# -- RANDOM ASSIGN ROUND 2 PAPERS -----
# write out to csv
# final rules (1/30/20): 
## if reviewed paper in round 1, cannot review same paper in round 2
## add Laura to list of reviewers, Laura should not review Caitlin papers from round 1
## random subsample 50% of abstracts that made it to round 2
## > will start with of all papers, but could divide this out based on journals (have more equal journal rep) or round 1 reviewer (have more equal # abstracts per round 1 reviewer)
## > e.g. trying to avoid lots of abstracts from journal with most abstracts contributed, and lots of abstracts from person who passed on the most abstracts, for example
## randomize groups of 3, each person splits half of their papers with the other 2 in the group so in total all papers have 2 reviewers for round 2


# 1) FINAL WAY (this is what's used and written out to GDrive)

# a) compile reviewers, ordered by number of papers they passed through


# b) subsample papers to 50%
set.seed(8) #mamba
r2_subsample.50pct <- sample(keep$Number, size = round(nrow(keep)*.5,0))
# what's the breakdown of abstracts by reviewer?
subset(keep, Number %in% r2_subsample.50pct) %>%
  group_by(EBIOReviewer) %>%
  summarise(nobs = length(Number)) %>%
  arrange(nobs)
# what's the breakdown by journal?
subset(keep, Number %in% r2_subsample.50pct) %>%
  left_join(distinct(assignmentsdf[c("Number", "SourcePublication")])) %>%
  group_by(SourcePublication) %>%
  summarise(nobs = length(Number)) %>%
  arrange(nobs) %>% # it's actually not too bad.. 80 journals of 92 subsampled
  ggplot() +
  geom_bar(aes(nobs)) +
  labs(x = "Number of abstracts selected", y = "Number of distinct journals in bin")

# subset keep dataframe to subsampled abstracts
assign_round2 <- dplyr::select(keep, Number, EBIOReviewer, final_name, AuthorsFull, FirstAuthor, comments, in_meetscriteria) %>%
  subset(Number %in% r2_subsample.50pct) %>%
  rename(actual_reviewer = EBIOReviewer) %>%
  mutate(EBIOReviewer = ifelse(grepl("Laura|Caitl", actual_reviewer), "Caitlin", actual_reviewer),
         round2_reviewer = NA) %>%
  dplyr::select(round2_reviewer, Number, EBIOReviewer, actual_reviewer, comments, final_name:ncol(.))


# c) define pairs
reviewersdf <- subset(keep, Number %in% r2_subsample.50pct) %>%
  #mutate(keep, EBIOReviewer = ifelse(EBIOReviewer == "Laura/Caitlin", "Caitlin", EBIOReviewer)) %>%
  mutate(EBIOReviewer = ifelse(grepl("Laura", EBIOReviewer), "Caitlin", EBIOReviewer)) %>%
  group_by(EBIOReviewer) %>%
  summarise(nobs = length(final_name)) %>%
  ungroup() %>%
  arrange(desc(nobs))
reviewers <- c("Laura", reviewersdf$EBIOReviewer)

r2_pairs <- data.frame(reviewer1 = reviewers, p1 = NA, p2 = NA)
choices <- reviewers
for(r in reviewers){
    # sample pair
    set.seed(24) #mamba
    selectp <- sample(choices[choices!= r], 1)
    r2_pairs$p1[r2_pairs$reviewer1 == r] <- selectp
    # udpate choices
    choices <- choices[choices != selectp]
}
# infill p2 with reviewer 1 for p1
for(r in r2_pairs$p1){
  r2_pairs$p2[r2_pairs$reviewer1 == r] <- r2_pairs$reviewer1[r2_pairs$p1 == r]
}


# c) assign papers
# try using old code and see if feasible..
# start with unique numbers subsampled
numbers <- as.character(r2_subsample.50pct)
for(r in reviewers){
  if(r != "Laura"){
    # random sample papers in keep not previously reviewed by person r
    set.seed(24)
    selectp <- sample(numbers[!numbers %in% assign_round2$Number[assign_round2$EBIOReviewer == r]], size = 27) #(floor(nrow(assign_round2)/length(reviewers)))) # leave a little bit of grace in floor, otherwise code will bonk
  }else{
    # write different line for Laura
    set.seed(24)
    selectp <- sample(numbers[!numbers %in% assign_round2$Number[assign_round2$EBIOReviewer == "Caitlin"]], size = 27)
  }
  # assign person r to selected numbers
  assign_round2$round2_reviewer[assign_round2$Number %in% selectp] <- r
  #update numbers before next iteration (remove papers selected)
  numbers <- numbers[!numbers %in% selectp]
  
}

# finish out last 14
## define who is left among EBIO Reviewers -- assign those first
remain <- subset(assign_round2, is.na(round2_reviewer)) %>% group_by(EBIOReviewer) %>% summarise(nobs = length(Number)) %>% arrange(desc(nobs))

for(r in c(remain$EBIOReviewer, reviewers[!reviewers %in% remain$EBIOReviewer])){
  if(r != "Laura"){
    # random sample papers in keep not previously reviewed by person r
    set.seed(24)
    selectp <- sample(numbers[!numbers %in% assign_round2$Number[assign_round2$EBIOReviewer == r]], size = 1) #(floor(nrow(assign_round2)/length(reviewers)))) # leave a little bit of grace in floor, otherwise code will bonk
  }else{
    # write different line for Laura
    set.seed(24)
    selectp <- sample(numbers[!numbers %in% assign_round2$Number[assign_round2$EBIOReviewer == "Caitlin"]], size = 1)
  }
  # assign person r to selected numbers
  assign_round2$round2_reviewer[assign_round2$Number == selectp] <- r
  #update numbers before next iteration (remove papers selected)
  numbers <- numbers[!numbers %in% selectp]
}

# double check everyone has equal number
sapply(split(assign_round2$Number, assign_round2$round2_reviewer), length) #yes (28)

# within groups, assign 2nd paper
assign_round2 <- cbind(r2r2 = NA, assign_round2)
reviewers2 <- reviewers
for(r in reviewers2){
  pool <- assign_round2$Number[assign_round2$round2_reviewer == r2_pairs$reviewer1[r2_pairs$p1 == r]]
  if(r != "Laura"){
    # random sample papers in keep not previously reviewed by person r
    set.seed(24)
    choice <- sample(pool[!pool %in% assign_round2$Number[assign_round2$EBIOReviewer == r]], size = 14) #(floor(nrow(assign_round2)/length(reviewers)))) # leave a little bit of grace in floor, otherwise code will bonk
  }else{
    # write different line for Laura
    set.seed(24)
    choice <- sample(pool[!pool %in% assign_round2$Number[assign_round2$EBIOReviewer == "Caitlin"]], size = 14)
  }
  # assign person r to selected numbers
  assign_round2$r2r2[assign_round2$Number %in% choice] <- r
  #by default other person will get the rest
  assign_round2$r2r2[assign_round2$Number %in% pool[!pool %in% choice]] <- r2_pairs$p2[r2_pairs$p1 == r]
  #update reviewers 2
  
}

# 2) OLD WAY -- keeping code to preserve how random assign to single reviewer would work (just in case needed in future)
## rules:
## if reviewed paper in round 1, cannot review same paper in round 2
## add Laura
## everyone gets roughly equal amount of papers to review
## random number assign


# order reviewers by who has most number of round 1 reviewed papers
#reviewers <- c(unique(master2$EBIOReviewer[!is.na(master2$EBIOReviewer)]), "Laura")
reviewersdf <- keep %>%
  #mutate(keep, EBIOReviewer = ifelse(EBIOReviewer == "Laura/Caitlin", "Caitlin", EBIOReviewer)) %>%
  mutate(EBIOReviewer = ifelse(grepl("Laura", EBIOReviewer), "Caitlin", EBIOReviewer)) %>%
  group_by(EBIOReviewer) %>%
  summarise(nobs = length(final_name)) %>%
  ungroup() %>%
  arrange(desc(nobs))
reviewers <- c("Laura", reviewersdf$EBIOReviewer)

assign_round2 <- dplyr::select(keep, Number, EBIOReviewer, final_name, AuthorsFull, FirstAuthor, comments, in_meetscriteria) %>%
  rename(actual_reviewer = EBIOReviewer) %>%
  mutate(EBIOReviewer = ifelse(grepl("Laura|Caitl", actual_reviewer), "Caitlin", actual_reviewer),
         round2_reviewer = NA) %>%
  dplyr::select(round2_reviewer, Number, EBIOReviewer, actual_reviewer, comments, final_name:ncol(.))

# start with unique numbers
numbers <- unique(keep$Number)
for(r in reviewers){
  if(r != "Laura"){
    # random sample papers in keep not previously reviewed by person r
    select <- sample(numbers[!numbers %in% assign_round2$Number[assign_round2$EBIOReviewer == r]], size = (floor(nrow(keep)/length(reviewers)))) # leave a little bit of grace in floor, otherwise code will bonk
  }else{
    # write different line for Laura
    select <- sample(numbers[!numbers %in% assign_round2$Number[assign_round2$EBIOReviewer == "Caitlin"]], size = (floor(nrow(keep)/length(reviewers))))
  }
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
  mutate(searchterm = ifelse(is.na(PublicationYear), FirstAuthor, paste0("^",FirstAuthor,"(?![a-z]).*(", PublicationYear-1, "|", PublicationYear, "|", PublicationYear+1,")"))) %>%
  group_by(Number) %>%
  mutate(in_meetscriteria = sum(grepl(searchterm, meetscriteria$name, ignore.case = T, perl = T))) %>%
  #in_meetscriteria2 = sum(grepl(paste0("^", FirstAuthor, "(?![a-z])"), meetscriteria$name, ignore.case = T, perl = T))) %>%
  ungroup() %>%
  rename(MeetsCriteria_matches = in_meetscriteria) %>%
  dplyr::select(-searchterm) %>%
  # capitalize first letter in colnames
  rename_all(function(x) paste0(casefold(substr(x,1,1), upper = T), substr(x, 2, nchar(x)))) %>%
  # sort by reviewer 2 name, first author, and year since that's how articles are saved in meets criteria folder
  arrange(Round2_reviewer, FirstAuthor, PublicationYear)

# write out
write.csv(assign_round2, "review_assignments_round2.csv", row.names = F)

# write out to google drive for class (whenever we decide the folder where it lives)
#drive_upload("review_assignments_round2.csv", path = as_id(abstracts_folder$id[grep("^Round1_NA", abstracts_folder$name)]), type = "spreadsheet", 
#              name = "ESpapers2review_Round2Jan2020_batch1", overwrite = T)




