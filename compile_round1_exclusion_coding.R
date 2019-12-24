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
           modifiedTime = results$drive_resource[[i]]$modifiedTime)
}


# read in paper assignments
assignments <- drive_find(pattern = "EcosystemServicesPapersNov2019.xlsx", n_max = 10) # first should be assignments
# create temp file for downloading excel sheet
tempxl <- tempfile(fileext = ".xlsx")
drive_download(assignments[1,], path = tempxl, overwrite = T) # downloads paper assignments to github repo.. until can figure out how to read excel directly from gdrive..
assignmentsdf <- read_excel(tempxl) 
# delete file downloaded locally
file.remove(tempxl)



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
which.max(times) #idk why earliest made (ie.e. pre-q8) has most recent create date..
which.max(modtimes) # go with mod times for prioritizing results

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
# remove empty rows or rows with NULL
master <- master[!rowNA,]
# fix datetime cols
master <- mutate_at(master, vars("createdTime", "modifiedTime"), function(x)as.POSIXct(gsub("T", "", x)))

# join assigment dats
# are all titles in the assignment dataset?
summary(unique(master$Title) %in% assignmentsdf$Title) # 22 don't match..
needsmatch <- unique(master$Title[!master$Title %in% assignmentsdf$Title])

summary(duplicated(master$Timestamp))




# -- CLEAN UP COMPILED -----
# distill to unique records
# select colnames on which to check dups
qnames <- names(master)[grep("[?]|^Q[0-9]|Comments|Title", names(master))]
# sort dat by date modified
master2 <- arrange(master, modifiedTime, Timestamp)
master2 <- master[!(duplicated(master[,qnames])),]


