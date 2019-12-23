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
options(stringsAsFactors = F)
theme_set(theme_bw())

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
           createdTime = results$drive_resource[[i]]$createdTime)
}
# what is the structure of all dats?
lapply(resultsls, names)
lapply(resultsls, head)
# colnames a little different in each..
## 1 has email address, no q8
## 2 and 3 have a column ...9 (??)

