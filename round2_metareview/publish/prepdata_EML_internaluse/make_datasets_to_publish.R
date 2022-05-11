# internal use R script to prep data and EML metadata for publication
# author(s): CTW
# questions?: caitlin.t.white@colorado.eduy
# date init: 2022-05-07

# script purpose:
# we should publish 2 datasets:
# 1. all abstracts and full text excluded with reasons and citation info
# 2. full text studies retained, with qualtrics data used for analyses, and citation info
# > #2 is our "map database" (anyone interested in particular aspects our our survey could filter studies based on coding responses)

# use GV's Land cover Land Use change code to recode drivers and exclude LULC only
# > add those LULC-excluded studies to the exclusion dataset
# > be sure changed LULC drivers have QA note added

# specific mods for LULC:
# [1] Anything Q12 driver clean_answer with clean_answer_binned “land use and land cover change” should have the driver type changed to “LU_LC”
# > CTW to-do: assign new internal survey order numbers, reassign effect direct answer to LU_LC as needed
# [2] If Q14 ESP_type is “Only land use or habitat proxy”, reassign driver type from biotic to “LU_LC”.
# > CTW to-do: Also reassign effect direct to LU_LC; review what individual driver variables were entered in biotic in this case (were indicated as land use or habitat proxies)
# [3] After LU_LC reassignment, if only driver in study is LU_LC exclude.
# > CTW to-do: Add these excluded papers (GV found 26) to the excluded papers dataset with reason (Only driver in study is land use or land cover as a proxy).

# once datasets prepped with citation info and unecessary columns removed (e.g., anything internal use not critical to publish), write metadata with EML package
# write out all to main level of "publish" subfolder


#### SETUP -----
rm(list = ls()) # start w clean environment (ctw does not care this is not best practice code)
# load needed libraries
library(tidyverse)
library(EML)
options(stringsAsFactors = F)
na_vals = c(NA, "NA", " ", "", ".")

# read in relevant datasets
datpath <- "round2_metareview/data/cleaned/"
# cleaned round 2 data (still need to convert LULC)
r2keep <- read.csv(paste0(datpath, "ESqualtrics_r2keep_cleaned.csv"), na.strings = na_vals)
# excluded papers with reasons
excluded <- read.csv(paste0(datpath, "ESreview_allexcluded_allrounds.csv"), na.strings = na_vals)
# citation info for all papers
citations <- read.csv("round1_exclusion/EcosystemServicesPapersNov2019.csv", na.strings = na_vals)

# how did all read in?
str(r2keep)
str(excluded)
str(citations) #ok


#### TREAT LULC ----
# if driver is land use or land cover related, convert to that type of driver
# > reassign effect direction and create new internal survey order number for LULC to organize
# make sure treat in similar way to how GV and AK use so analysis code runs as is

##### 1. Make new survey question LUT (internal survey order)  -----
# > anything before Q12 will remain the same, but orders change at Q12 because of additional driver group for 1) clean_answer_binned, 2) clean_group, 3) effectdirect
# 16ES x 3 new possible answers = 48 additional survey order IDs to add
# LULC will come after Biotic driver

abbrcodes <- distinct(r2keep[c("id", "qnum", "abbr", "Group", "ES", "survey_order")]) %>%
  arrange(survey_order)

# isolate 16 ES categories and their survey order 1-16
ESlut <- unique(subset(abbrcodes, abbr == "Yclass")) %>%
  # create ES order (1-16)
  mutate(ES_order = as.numeric(gsub("^Q.*[0-9]_", "", id))) %>%
  # drop unneeded rows
  subset(select = c(qnum, ES, ES_order))

# isolate driver groups (Env, Human, Bio) and their survey order, add LULC
grouplut <- unique(subset(abbrcodes, abbr == "Driver", select = c(qnum, id, Group))) %>%
  mutate(group_order = str_extract(id, "(?<=[.])[0-9]+(?=_)")) %>%
  distinct(qnum, Group, group_order) %>%
  mutate(group_order = rank(group_order)) %>%
  # add LULC as 4th group
  rbind(data.frame(qnum = "Q12",Group = "LU_LC", group_order = 4))

# order of all abbreviated questions in the survey
abbrlut <- distinct(abbrcodes[c("qnum", "abbr")]) %>%
  mutate(abbr_globalorder = 1:nrow(.))

# put all together to make LULC questions in Q12 matrix
lulc_Q12order <- left_join(grouplut, abbrlut)
lulc_Q12order[lulc_Q12order$abbr %in% c("Response", "Yclass"), c("Group", "group_order")] <-  NA
lulc_Q12order <- distinct(lulc_Q12order) %>%
  left_join(ESlut)
# make OtherDriver without ES's
lulc_otherdriver <- left_join(grouplut, abbrlut[abbrlut$abbr == "OtherDriver",]) %>%
  mutate(ES = NA, ES_order = NA)
# stack and join existing survey order
lulc_Q12order_all <- rbind(lulc_Q12order, lulc_otherdriver) %>%
  left_join(distinct(subset(abbrcodes, qnum == "Q12", select = -id))) %>%
  left_join(distinct(subset(abbrcodes, qnum == "Q12"))) %>%
# ESes that never got selected for Other answer do not have a survey order or qualtrics id
  mutate(survey_order_simple = trunc(survey_order),
         ES_order = ifelse(is.na(ES), 0, ES_order)) %>%
  arrange(abbr_globalorder, survey_order, group_order, ES_order) %>%
  distinct()

# take out otherdriver to renumber separately
lulc_otherdriver <- subset(lulc_Q12order_all, abbr == "OtherDriver")

# renumber other Q12 questions first
lulc_Q12order_all <- subset(lulc_Q12order_all, !(abbr == "OtherDriver" & ES_order > 0)) %>%
  # order from starting survey_order (minimum order) to min + length of all questions
  mutate(new_order = min(survey_order, na.rm = T):(min(survey_order, na.rm = T) + nrow(.)-1))

# renumber other drivers
lulc_otherdriver <- subset(lulc_otherdriver, select = c(qnum:ES_order)) %>%
  left_join(distinct(subset(lulc_Q12order_all, 
                            abbr == "OtherDriver" & ES_order == 0, 
                            select = -c(survey_order, survey_order_simple)))) %>%
  arrange(group_order, ES_order) %>%
  # fill down qnum and new order
  group_by(Group) %>%
  fill(id, new_order) %>%
  ungroup() %>%
  mutate(new_order = ifelse(ES_order < 10, 
                            as.numeric(paste0(new_order,".0", ES_order)), 
                            as.numeric(paste(new_order, ES_order, sep = "."))))

# stack other driver with other Q12 renumbered questions
newQ12_surveyorder <- subset(lulc_Q12order_all, abbr != "OtherDriver", select = names(lulc_otherdriver)) %>%
  rbind(lulc_otherdriver) %>%
  # be sure in correct order
  arrange(new_order)


# renumber everything after Q12 from max integer of new Q12 internal survey order
new_abbrcodes <- subset(abbrcodes, qnum != "Q12") %>%
  mutate(new_order = ifelse(qnum %in% paste0("Q", 1:11), survey_order, NA)) %>%
  subset(select = -survey_order) %>%
  rbind(newQ12_surveyorder[names(.)]) %>%
  arrange(new_order, qnum)
start_order <- (trunc(max(new_abbrcodes$new_order, na.rm = T))+1)
needs_order <- which(is.na(new_abbrcodes$new_order))
new_abbrcodes$new_order[needs_order] <- seq.int(start_order, length.out = length(needs_order), by =1)
# finally, start order at 1 (begins at 20)
if(min(new_abbrcodes$new_order) != 1){
  new_abbrcodes$new_order <- new_abbrcodes$new_order - (min(new_abbrcodes$new_order)-1)
}




##### 2. Re-classify land use and land cover drivers studies -----
dat <- r2keep

abbrcodes <- distinct(r2keep[!is.na(r2keep$clean_answer), c("qnum", "abbr", "Group", "clean_group", "ES", "survey_order")])

# find the land use land cover studies
dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_answer_binned, Group) %>% 
  pull(clean_answer_binned) %>% 
  unique() 

# 'Land cover' and 'Land use and land cover change' are the two bins that should get their own categories


# Biotic drivers papers that only looked at land cover or habitat type as a proxy
biot_lulc_titles = dat %>%
  filter(qnum=='Q14', abbr=='ESP_type', clean_answer == 'Only land cover or habitat type as proxy') %>%
  pull(Title)


# separate out land use and land cover studies
driv_types_title = dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_answer_binned, clean_group) %>%
  mutate(old_group = clean_group) %>%
  # reassign clean_answer_binned land cover answers to have Group 'LU LC'
  mutate(clean_group = ifelse(clean_answer_binned %in% c('Land cover', 'Land use and land cover change'), 'LU_LC',clean_group)) %>%
  # reassign studies with only land cover biotic drivers (based on Q14) to 'LU LC' Group
  mutate(clean_group = ifelse(Title %in% biot_lulc_titles & clean_group == 'Biotic', 'LU_LC', clean_group)) %>%
  # take only unique rows 
  unique() %>%
  dplyr::select(-old_group, -clean_answer_binned) %>%
  filter(!is.na(clean_group)) %>% # two NAs snuck through somehow, I checked and there were always other drivers
  mutate(pres_holder = TRUE) %>%
  unique() %>%
  pivot_wider(id_cols = 'Title', names_from = 'clean_group', values_from = 'pres_holder') %>%
  replace(is.na(.), FALSE)

# exclude lulc studies
excl_lulc = driv_types_title %>%
  dplyr::select(-LU_LC) %>%
  filter(!(Biotic=='FALSE' & Human =='FALSE' & Environmental =='FALSE'))
# 110 studies used an lulc drivers, 26 studies only used lulc drivers

# save updated driver types
dat2 <- dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_answer_binned, clean_group) %>%
  mutate(old_group = clean_group) %>%
  # reassign clean_answer_binned land cover answers to have Group 'LU LC'
  mutate(clean_group = ifelse(clean_answer_binned %in% c('Land cover', 'Land use and land cover change'), 'LU_LC',clean_group)) %>%
  # reassign studies with only land cover biotic drivers (based on Q14) to 'LU LC' Group
  mutate(clean_group = ifelse(Title %in% biot_lulc_titles & clean_group == 'Biotic', 'LU_LC', clean_group)) %>%
  # take only unique rows 
  unique()

# save driver types and es types for sankey
dat3 <- dat %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_answer_binned, clean_group) %>%
  mutate(old_group = clean_group) %>%
  # reassign clean_answer_binned land cover answers to have Group 'LU LC'
  mutate(clean_group = ifelse(clean_answer_binned %in% c('Land cover', 'Land use and land cover change'), 'LU_LC',clean_group)) %>%
  # reassign studies with only land cover biotic drivers (based on Q14) to 'LU LC' Group
  mutate(clean_group = ifelse(Title %in% biot_lulc_titles & clean_group == 'Biotic', 'LU_LC', clean_group)) %>%
  dplyr::select(-clean_answer_binned, -old_group) %>%
  # take only unique rows 
  unique() %>%
  rename(driv_type = clean_group) %>%
  # join with ES types
  full_join(
    dat %>%
      filter(abbr=='Response') %>% 
      filter(!is.na(clean_answer)) %>% 
      dplyr::select(Title, ES) %>%
      unique(),
    by='Title'
  )


#### MAKE FULL TEXT DATA OUT ----


#### MAKE EXCLUDED PAPERS OUT ----


#### MAKE EML METADATA -----
