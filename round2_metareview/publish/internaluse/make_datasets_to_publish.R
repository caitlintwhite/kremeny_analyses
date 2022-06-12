# internal use R script to prep data and EML metadata for publication
# author(s): CTW
# questions?: caitlin.t.white@colorado.edu
# date init: 2022-05-15

# script purpose:
# we should publish 2 datasets:
# 1. all abstracts and full text excluded with reasons and citation info
# 2. full text studies retained, with qualtrics data used for analyses, and citation info
# > #2 is our "map database" (anyone interested in particular aspects our our survey could filter studies based on coding responses)


# once datasets prepped with citation info and unecessary columns removed (e.g., anything internal use not critical to publish), write metadata with EML package
# write out all to main level of "publish" subfolder



#### SETUP -----
rm(list = ls()) # start w clean environment (ctw does not care this is not best practice code)
# load needed libraries
library(tidyverse)
library(EML)
library(readxl)
options(stringsAsFactors = F)
na_vals = c(NA, "NA", " ", "", ".")

# read in relevant datasets
datpath <- "round2_metareview/data/cleaned/"
# lulc adjusted main analysis dataset
r2keep_lulc <- read.csv(paste0(datpath, "ESqualtrics_r2keep_cleaned_lulc.csv"), na.strings = na_vals)
# summary of excluded papers with reasons
excludedpapers <- read.csv(paste0(datpath, "ESreview_allexcluded_allrounds.csv"), na.strings = na_vals)
# all papers kept after round 1 (to grab papers that passed round 1 but were not selected for round 2 review by random chance)
keep_round1 <- read.csv("round1_exclusion/output/keep_round1.csv", na.strings = na_vals)
# WOS return for paper citations
citations <- read.csv("round1_exclusion/EcosystemServicesPapersNov2019.csv", na.strings = na_vals)

  
# review how all read in
str(r2keep_lulc)
str(excludedpapers)
str(keep_round1)
str(citations)

# make lut to anonymize reviewers
reviewers <- unique(c(r2keep_lulc$Init, excludedpapers$reviewer_init))
# remove double review inits
single_reviewers <- data.frame(Init = sort(trimws(reviewers[!grepl("[/]", reviewers)])))
single_reviewers$ID <- paste0("R", 1:nrow(single_reviewers))
double_reviewers <- data.frame(Init = sort(reviewers[grepl("[/]", reviewers)])) %>%
  separate(col = Init, into = c("Init1", "Init2"), "[/]", remove = F) %>%
  mutate_at(.vars = c("Init1", "Init2"), .funs =  function(x) trimws(x))
for(i in 1:nrow(double_reviewers)){
  double_reviewers$alpha_Init[i] <- str_flatten(sort(c(double_reviewers$Init1[i], double_reviewers$Init2[i])), collapse = "/")
}
double_reviewers <- dplyr::select(double_reviewers, -c(Init1, Init2)) %>%
  separate(col = alpha_Init, into = c("Init1", "Init2"), "[/]", remove = F) %>%
  mutate_at(.vars = c("Init1", "Init2"), .funs =  function(x) trimws(x)) %>%
  left_join(single_reviewers, by = c("Init1" = "Init")) %>%
  rename(ID1 = ID) %>%
  left_join(single_reviewers, by = c("Init2" = "Init")) %>%
  rename(ID2 = ID) %>%
  mutate(ID = paste(ID1, ID2, sep = "/"))

reviewers_lut <- rbind(single_reviewers, double_reviewers[c("Init", "ID")])
# add ND row
reviewers_lut <- rbind(reviewers_lut, data.frame(Init = "ND", ID = reviewers_lut$ID[reviewers_lut$Init == "NBD"]))

# cross walk full names from round 1 review
fullnames <- distinct(keep_round1, EBIOReviewer) %>%
  mutate(Init = "CW") # placeholder to make char vector
# think need to do this manually
unique(reviewers_lut$Init)
# assign initials
fullnames$Init[grepl("Ais", fullnames$EBIOReviewer)] <- "AK"
fullnames$Init[grepl("An", fullnames$EBIOReviewer)] <- "AIS"
fullnames$Init[grepl("Cla", fullnames$EBIOReviewer)] <- "CK"
fullnames$Init[grepl("Gra", fullnames$EBIOReviewer)] <- "GV"
fullnames$Init[grepl("Isa", fullnames$EBIOReviewer)] <- "IS"
fullnames$Init[grepl("Jul", fullnames$EBIOReviewer)] <- "JL"
fullnames$Init[grepl("Ka", fullnames$EBIOReviewer)] <- "KCG"
fullnames$Init[grepl("Laura/", fullnames$EBIOReviewer)] <- "LD/CW"
fullnames$Init[grepl("Laura$", fullnames$EBIOReviewer)] <- "LD"
fullnames$Init[grepl("Laure", fullnames$EBIOReviewer)] <- "LB"
fullnames$Init[grepl("Ni", fullnames$EBIOReviewer)] <- "ND"
fullnames$Init[grepl("Tra", fullnames$EBIOReviewer)] <- "TM"
fullnames$Init[grepl("Ti", fullnames$EBIOReviewer)] <- "TK"
fullnames$Init[grepl("Si", fullnames$EBIOReviewer)] <- "SDJ"
# join IDs
fullnames <- left_join(fullnames, reviewers_lut)  %>%
  # remove Init col and rename fullname col to Init to rbind to main LUT
  subset(select = -Init) %>%
  rename(Init = EBIOReviewer)
# add to main LUT
reviewers_lut <- rbind(reviewers_lut, fullnames)


# prep simple citation df for all papers
# > note: one of these papers had a bad title -- defer to final_name in round1_keep
summary(is.na(citations))
View(subset(citations, is.na(PublicationYear))) # early access date 'ea' has at least the year when pubyear not available
simple_citations <- mutate(citations, pubyear = ifelse(is.na(PublicationYear), 
                                                       as.numeric(paste0(20, str_extract(EA,"[0-9]{2}$"))),
                                                       PublicationYear)) %>% #NA warnings thrown but it looks fine on manual review
  subset(select = names(.)[grepl("Tit|Auth|Sour|pub", names(.))]) %>%
  rename(sourcepub = SourcePublication, title = Title, authors = AuthorsFull) %>%
  # rearrange columns
  subset(select = c(title, authors, pubyear, sourcepub))
# remove duplicate title (two records from same paper for different pubyear [off by 1yr])
simple_citations[duplicated(simple_citations$title), ] # EcolLetters online says pubdate = 03 February 2019; removing dup row will remove 2018 row
simple_citations <- subset(simple_citations, !duplicated(title)) #1932 distinct papers
# replace bad title "eEde predator-prey interactions"
good_title <- unique(keep_round1$Title[grepl("eEde p", keep_round1$final_name)])
# clean up typo in predator-prey
good_title <- gsub("predator prey-", "predator–prey", good_title)
simple_citations$title[grepl("eEde p", simple_citations$title)] <- good_title




#### MAKE FULL TEXT DATA OUT ----
# anonymize dataset:
# > Init column, answers for double reviews, any inits in QA note

r2keep_out <- dplyr::select(r2keep_lulc, ResponseId:only_lulc) %>% # don't need start, end, recorded date
  # join lut col to anonymize reviewers
  left_join(reviewers_lut) %>%
  # replace reviewer
  mutate(Init = ID) %>%
  # drop ID col
  dplyr::select(-ID) %>%
  # prefix "raw" to answer and Group to distinguish more intuitively from clean_group
  rename(raw_answer = answer, raw_group = Group, qid = id) %>%
  # lowcase colnames
  rename_all(casefold)

# anonymize reviewer answers
for(i in 1:nrow(reviewers_lut)){
  r2keep_out$raw_answer <- gsub(paste0(reviewers_lut$Init[i],":"), paste0(reviewers_lut$ID[i],":"), r2keep_out$raw_answer)
  r2keep_out$clean_answer <- gsub(paste0(reviewers_lut$Init[i],":"), paste0(reviewers_lut$ID[i],":"), r2keep_out$clean_answer)
}

# anonymize qa_notes
for(i in 1:nrow(reviewers_lut)){
  r2keep_out$qa_note <- gsub(reviewers_lut$Init[i], reviewers_lut$ID[i], r2keep_out$qa_note)
}

# spot review anonymous subbing
View(subset(r2keep_out, grepl("R[0-9]{1,2}", raw_answer)))
sort(unique(r2keep_out$qa_note[grepl("R[0-9]{1,2}", r2keep_out$qa_note)]))
sort(unique(r2keep_out$raw_answer[grepl("R[0-9]{1,2}", r2keep_out$raw_answer)])) # seems okay

# rename init col to reviewer
names(r2keep_out)[names(r2keep_out)== "init"] <- "reviewer"


# join simple citations to clean version dataset
r2keep_out_clean <- left_join(r2keep_out, simple_citations, by = "title") %>%
  # recode "original" to "individual" in version to make more intuitide (individual responses for double reviewed papers)
  mutate(version = recode(version, "original" = "individual"))

# look at current colname order
names(r2keep_out_clean)
# rearrange cols for writing out
r2keep_out_clean <- dplyr::select(r2keep_out_clean, title, authors:sourcepub, # paper info
                                  reviewer:qid, qnum, abbr, fullquestion, # question info
                                  raw_answer:only_lulc) # answers, groups, ESes and notes


# write out final full-text dataset
# write.csv(r2keep_out_clean, "round2_metareview/publish/ecolofES_extracted_data.csv", row.names = F)



#### MAKE EXCLUDED PAPERS DATA OUT----
# similar order + format as main analysis dataset out:
# 1. paper info, then question/reason, and any qa notes
# anonymize dataset
# 29 partially excluded lulc papers were not used in drivers analyses, but used in everything else.. so don't include in fully excluded papers
names(excludedpapers)
exclude_out <- left_join(excludedpapers, reviewers_lut, by = c("reviewer_init" = "Init")) %>%
  rename(reviewer = ID) %>%
  left_join(reviewers_lut, by = c("outsidereviewer" = "Init")) %>%
  # add CW ID for initals CTW
  mutate(ID = ifelse(is.na(ID) & outsidereviewer == "CTW", reviewers_lut$ID[reviewers_lut$Init == "CW"], ID))
# be sure all IDs joined
with(exclude_out, summary(!is.na(reviewer_init) & is.na(reviewer))) # main reviewer. none missing.
with(exclude_out, summary(!is.na(outsidereviewer) & is.na(ID))) # outside reviewer. none missing.
# anonymize note cols as above
for(i in 1:nrow(reviewers_lut)){
  exclude_out$reviewer_comments <- gsub(paste0(reviewers_lut$Init[i],":"), paste0(reviewers_lut$ID[i],":"), exclude_out$reviewer_comments)
  exclude_out$outsidereviewer_notes <- gsub(paste0(reviewers_lut$Init[i],":"), paste0(reviewers_lut$ID[i],":"), exclude_out$outsidereviewer_notes)
}
# anonymize CTW in outsidereviewer_notes
exclude_out$reviewer_comments <- gsub("CTW", reviewers_lut$ID[reviewers_lut$Init == "CW"], exclude_out$reviewer_comments)
exclude_out$outsidereviewer_notes <- gsub("CTW", reviewers_lut$ID[reviewers_lut$Init == "CW"], exclude_out$outsidereviewer_notes)
# review subs
sort(unique(exclude_out$reviewer_comments)) # some initials present without colon
# look specifically for reviewer initials
sort(unique(exclude_out$reviewer_comments[grepl(str_flatten(reviewers_lut$Init, collapse = "|"), exclude_out$reviewer_comments)])) # LD initials still present, ignore "ND" in NDVI
# outside/third party reviewer notes
sort(unique(exclude_out$outsidereviewer_notes))
# look specifically for reviewer initials
sort(unique(exclude_out$outsidereviewer_notes[grepl(str_flatten(reviewers_lut$Init, collapse = "|"), exclude_out$outsidereviewer_notes)])) # TK, AK, NBD, LD
for(i in c("TK", "AK", "LD", "NBD")){
  exclude_out$reviewer_comments <- gsub(i, reviewers_lut$ID[reviewers_lut$Init == i], exclude_out$reviewer_comments)
  exclude_out$outsidereviewer_notes <- gsub(i, reviewers_lut$ID[reviewers_lut$Init == i], exclude_out$outsidereviewer_notes)
}

# clean up exclusion reasons
distinct(exclude_out, exclusion_id, exclusion_reason, fullquestion)
# reassign exclusion id so corresponds to unique questions (even if questions repeat across review rounds, e.g., biodiv + no direct measure of ef/es)
clean_exclusion_lut <- distinct(exclude_out, fullquestion, exclusion_id) %>%
  #mod question on biodiv only for round 2 (switches order of ES/EF --> EF/ES)
  mutate(clean_question = gsub("EF/ES", "ES/EF", fullquestion)) %>%
  subset(!duplicated(clean_question)) %>%
  #mod question
  mutate(clean_id = 1:length(exclusion_id))
# then clean up reason to reflect
exclusion_lut <- distinct(exclude_out, exclusion_reason, fullquestion) %>%
  #mod question on biodiv only for round 2 (switches order of ES/EF --> EF/ES)
  mutate(clean_question = gsub("EF/ES", "ES/EF", fullquestion)) %>%
  left_join(clean_exclusion_lut[c("clean_question", "clean_id")]) %>%
  mutate(clean_reason = ifelse(grepl("meta", exclusion_reason), "Meta-analysis only",
                               ifelse(grepl("review", exclusion_reason), "Review only",
                                      ifelse(grepl("ef", exclusion_reason, ignore.case = T), "No EF/ES directly measured",
                                             ifelse(grepl("biodi", exclusion_reason, ignore.case = T), "Stops at biodiversity or abundance metrics",
                                                    ifelse(grepl("valr", exclusion_reason), "Valuation or risk assessment only",
                                                           ifelse(grepl("tool", exclusion_reason), "Describes new tool or method only",
                                                                  ifelse(grepl("Social", exclusion_reason), "Social dimensions focused",
                                                                         # else it's the catch all methods/meta/review in round 2
                                                                         "Meta-analysis, review, or methods paper only")
                                                           )
                                                    )
                                             )
                                      )
                               )
  )
  )


# join simple citation cols (correct colnames, no missing pubyears, and reorder columns)
exclude_out_clean <- left_join(exclude_out, simple_citations) %>%
  left_join(exclusion_lut)
# and join clean exclusion reasons
names(exclude_out_clean)
exclude_out_clean <- subset(exclude_out_clean, select = c(title, authors:sourcepub, # paper info
                                                          # reviewer and outside reviewer info
                                                          review_round, reviewer, doublerev, reviewer_comments, ID, outsidereviewer_notes,
                                                          # exclusion reasons
                                                          clean_id, clean_reason, clean_question)) %>%
  # rename some cols for standardization
  rename(reviewer_notes = reviewer_comments, outsidereviewer = ID,
         exclusion_id = clean_id, exclusion_reason = clean_reason, fullquestion = clean_question)

# create records for papers that made it past round 1 but were not selected for r2 review because of random chance (we only reviewed a subset of papers to make it manageable)
random_unselected <- subset(simple_citations, !title %in% c(r2keep_out_clean$title, exclude_out_clean$title)) %>%
  # create columns to match exclude_out_clean df
  data.frame(matrix(nrow = nrow(.), ncol = sum(!names(exclude_out_clean) %in% names(.)),
                    dimnames = list(1:nrow(.), names(exclude_out_clean)[!names(exclude_out_clean) %in% names(.)]))) %>%
  # anonymize dataset and add comments
  # > use final_name instead of Title (Title has some weird characters, final_name is clean title)
  left_join(keep_round1[c("final_name", "EBIOReviewer", "comments")], by = c("title" = "final_name")) %>%
  left_join(reviewers_lut, by = c("EBIOReviewer" = "Init"))
# check for NAs (anything answers or reviewer IDs that didn't join)
# > looking for TRUE in ID, EBIOReviewer, authors, pubyear, sourcepub
summary(!is.na(random_unselected)) # all good


# look specifically for reviewer names or initials in comments field
sort(unique(random_unselected$comments[grepl(str_flatten(reviewers_lut$Init, collapse = "|"), random_unselected$comments)])) # just LD and CTW in comments
# anonymize CTW and LD
random_unselected$comments <- gsub("CTW", reviewers_lut$ID[reviewers_lut$Init == "CW"], random_unselected$comments)
random_unselected$comments <- gsub("LD", reviewers_lut$ID[reviewers_lut$Init == "LD"], random_unselected$comments)
# clean up to rbind to excluded papers
random_unselected <- mutate(random_unselected, reviewer_notes = comments, reviewer = ID,
                            exclusion_reason = "Passed abstract screen, not random-selected for full-text review",
                            exclusion_id = max(exclude_out_clean$exclusion_id)+1,
                            # assign review round as 2 because selected out for that round
                            review_round = 2,
                            # only potentially doublerev papers were LD/CTW splits
                            doublerev = grepl("/", reviewer)) %>%
  dplyr::select(names(exclude_out_clean))
# check for NAs  
summary(is.na(random_unselected)) # all looks good
# rbind to main dataset out
exclude_out_clean <- rbind(exclude_out_clean, random_unselected)
# make sure all distinct rows
exclude_out_clean <- distinct(exclude_out_clean)
# renumber rownames
rownames(exclude_out_clean) <- NULL

# write out final exclused papers dataset
# write.csv(exclude_out_clean, "round2_metareview/publish/ecolofES_excludedpapers.csv", row.names = F)




#### MAKE EML METADATA -----
# set path to publication files
pubdir <- "round2_metareview/publish/"
# read in metadata prep files to list
metaprep <- list()
# grab names of each sheet in metareview prep file
prepsheets <- excel_sheets(paste0(pubdir, "internaluse/ecolofES_EMLmetadata_prep.xlsx"))
# iterate to read in each sheet as data frame
for(i in 1:length(prepsheets)){
  metaprep[[i]] <- read_excel(path = paste0(pubdir, "internaluse/ecolofES_EMLmetadata_prep.xlsx"), 
                              sheet = prepsheets[i], na = na_vals, trim_ws = T)
  names(metaprep)[[i]] <- prepsheets[i]
}

# pull datasets to use
es_factors <- metaprep[[grep("CodeDe", names(metaprep))]]
es_attribs <- metaprep[[grep("Attributes", names(metaprep))]]
es_persons <- metaprep[["Person"]]
es_entities <- metaprep$DataSetEntities


# -- make common items -----
# coverage: only coverage to set is temporal
es_coverage <- set_coverage(beginDate = "2006-01-01", endDate = "2019-11-23")
# responsible party: Laura is point of contact
es_responsibleparty <- with(es_persons[es_persons$surName == "Dee",], set_responsibleParty(givenName, surName, organizationName = organizationName, 
                                                                                           electronicMailAddress = electronicMailAddress, id = id,
                                                                                           onlineUrl = "https://www.colorado.edu/ebio/laura-dee"))
# EBIO/CU address
EBIO_address <- list(deliveryPoint = "EBIO, Ramaley N122", 
                     city = "Boulder", administrativeArea = "Colorado", 
                     postalCode = "80309-0334", country = "U.S.A."  
)

# keywords -- took from manuscript draft
keywordSet <- list(
  keyword = list(
    "Biodiversity", "Ecological drivers", "Ecosystem services", 
    "Ecosystem service providers", "Global change", "Multifunctionality", 
    "Nature’s benefits to people", 
    "ROSES", "Systematic map"
))




# dataset specific items ----

# make attributeLists
es_extracted_attributes <- set_attributes(
  as_tibble(
    subset(es_attribs, grepl("extract", entityName),
           select = c(attributeName, attributeLabel, attributeDefinition, formatString, unit, numberType))),
  # code definitions for es fields
  es_factors[c("attributeName", "code", "definition")],
  # class of field
  col_classes = with(es_attribs, col_classes[grepl("extra", entityName)]))

es_excluded__attributes_excluded <- set_attributes(
  as_tibble(
    subset(es_attribs, grepl("exclud", entityName),
           select = c(attributeName, attributeLabel, attributeDefinition, formatString, unit, numberType))),
  # code definitions for es fields
  es_factors[c("attributeName", "code", "definition")],
  # class of field
  col_classes = with(es_attribs, col_classes[grepl("exclud", entityName)]))


# make dataTables
es_extracted_physical <- set_physical(objectName = with(metaprep$DataSetEntities, entityfilename[grepl("extr", entityfilename)]),
                                      size = file.info(paste0(pubdir, "ecolofES_extracted_data.csv"))$size,
                                      authentication =  unname(tools::md5sum(paste0(pubdir, "ecolofES_extracted_data.csv"))),
                                      authMethod = "MD5"
                                  )
es_excluded_physical <- set_physical(objectName = with(metaprep$DataSetEntities, entityfilename[grepl("exclu", entityfilename)]),
                                      size = file.info(paste0(pubdir, "ecolofES_excludedpapers.csv"))$size,
                                     authentication =  unname(tools::md5sum(paste0(pubdir, "ecolofES_excluded_papers.csv"))),
                                     authMethod = "MD5"
)

es_extracted_dataTable <- list(entityName = with(es_entities, entityName[grepl("extr", entityName)]),
                              entityDescription = with(es_entities, entitydescription[grepl("extr", entityName)]),
                              physical = es_extracted_physical,
                              attributeList = es_extracted_attributes)


# make extracted data eml ----
extracted_dataset <- list(
  title = with(es_entities, title[grepl("extr", title)]),
  creator = ,
  pubDate = 2022,
  intellectualRights = "TBD",
  abstract = "TBD",
  associatedParty = ,
  keywordSet = keywordSet,
  coverage = es_coverage,
  contact = es_responsibleparty,
  methods = "TBD",
  dataTable = es_extracted_dataTable
)

# add to root eml
extracted_eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid",
  dataset = extracted_dataset
)

# write out xml file
write_eml(excluded_eml, paste0(pubdir, "ecolofES_extracted_data_eml.xml"))
# check that xml file validates
eml_validate(paste0(pubdir, "ecolofES_extracted_data_eml.xml"))



# make excluded papers eml ----
excluded_dataset <- list(
  title = with(es_entities, title[grepl("excl", title)]),
  creator = ,
  pubDate = 2022,
  intellectualRights = "TBD",
  abstract = "TBD",
  associatedParty = ,
  keywordSet = keywordSet,
  coverage = es_coverage,
  contact = es_responsibleparty,
  methods = "TBD",
  dataTable = es_extracted_dataTable
)

# add to root eml
excluded_eml <- list(
  packageId = uuid::UUIDgenerate(),
  system = "uuid",
  dataset = excluded_dataset
)

# write out xml file
write_eml(excluded_eml, paste0(pubdir, "ecolofES_excludedpapers_eml.xml"))
# check that xml file validates
eml_validate(paste0(pubdir, "ecolofES_excludedpapers_eml.xml"))

