# recode land use/land cover drivers in to their own LU_LC driver type
# author(s): CTW
# questions?: caitlin.t.white@colorado.edu
# date init: 2022-05-07

# script purpose:
# 1) use GV's Land cover Land Use change code to recode drivers and exclude LULC only
# > write LULC-excluded studies to round 2 data folder as their own exclusion dataset
# > be sure changed LULC drivers have QA note added

# specific mods for LULC:
# [1] Anything Q12 driver clean_answer with clean_answer_binned “land use and land cover change” should have the driver type changed to “LU_LC”
# > CTW to-do: assign new internal survey order numbers, reassign effect direct answer to LU_LC as needed
# [2] If Q14 ESP_type is “Only land use or habitat proxy”, reassign driver type from biotic to “LU_LC”.
# > CTW to-do: Also reassign effect direct to LU_LC; review what individual driver variables were entered in biotic in this case (were indicated as land use or habitat proxies)
# [3] After LU_LC reassignment, if only driver in study is LU_LC exclude.
# > CTW to-do: Add these excluded papers (GV found 26) to the excluded papers dataset with reason (Only driver in study is land use or land cover as a proxy).

# 2) screen for logical consistencies
# because may want to use original 3 driver types sometimes (?) keep those and put LULC-updated drivers in another column
# GV and AK can switch between which drivers they want to use as needed

# 3) write out 3 datasets:
# 3.1) dataset for analysis with same name (archive old just in case)
# 3.2) ES-driver table for Sankey figure (as GV made for AK)
# 3.3) excluded LULC papers (with full data available -- will deal with that in the make excluded papers script)



#### SETUP -----
rm(list = ls()) # start w clean environment (ctw does not care this is not best practice code)
# load needed libraries
library(tidyverse)
options(stringsAsFactors = F)
na_vals = c(NA, "NA", " ", "", ".")

# read in relevant datasets
datpath <- "round2_metareview/data/cleaned/"
# cleaned round 2 data (still need to convert LULC)
r2keep <- read.csv(paste0(datpath, "ESqualtrics_r2keep_cleaned.csv"), na.strings = na_vals)
# citation info for all papers
citations <- read.csv("round1_exclusion/EcosystemServicesPapersNov2019.csv", na.strings = na_vals)

# how did all read in?
str(r2keep)
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
# add new survey order to questions that appear after q12
start_order <- (trunc(max(new_abbrcodes$new_order, na.rm = T))+1)
needs_order <- which(is.na(new_abbrcodes$new_order))
new_abbrcodes$new_order[needs_order] <- seq.int(start_order, length.out = length(needs_order), by =1)

# > update: since keeping both survey order in the dataset, don't adjust numbering to start at 1 so unaffected numbers are consistent

# # finally, start order at 1 (begins at 20)
# if(min(new_abbrcodes$new_order) != 1){
#   new_abbrcodes$new_order <- new_abbrcodes$new_order - (min(new_abbrcodes$new_order)-1)
# }



##### 2. Review land use and land cover drivers and proxy ESPs -----
# steps:
# 1. assess individual answers that were binned land cover/land use to be sure recoding is consistent (were are similar individual drivers binned consistently)
# > found some inconsistencies in how things were binned/needed further review and updated in cleaning script from Jan 2022 so AK and GV can update their analyses in progress
# > inconsistencies edited are noted in the jan 2022 cleaning script and in the full data methods .docx
# 2. recode bins
# 3. reassign clean group for EffectDirect
# 4. add QA notes
# note: this will apply to individual reviews (for double reviewed papers) and final (single review or converged double review answers)

# 1. assess what individual answers and driver types got binned as land cover or land use related
drivervars <- subset(r2keep, qnum == "Q12" & grepl("Driv", abbr) & clean_answer_binned != "Other",
                     # can ignore variables repeated across ESes
                     select = c(Title, clean_answer, clean_answer_binned, abbr:clean_group)) %>%
  distinct()

# what are the bins?
sort(unique(drivervars$clean_answer_binned))
# check out "Land cover", "Land use and land cover change", "Human disturbance", "Vegetation cover"
# what are the individual "Other" vars for these?
subset(drivervars, abbr == "OtherDriver" & grepl("^Land|disturbance|cover", clean_answer_binned), select = c(clean_answer, clean_answer_binned)) %>%
  arrange(clean_answer_binned, clean_answer) # vars assigned to land cover and land use seem fine, those assigned to disturbance ok to keep disturbance
# proceed with reclass!
# 'Land cover' and 'Land use and land cover change' are the two bins that should get their own categories

# 2. recode Group to LU_LC for any land cover/land use binned driver
# > also recode Effect Direct as needed and add QA note
# BUT if there were multiple vars in the Group that the LULC driver was previously part of, need to copy (duplicate) effect direction for LULC group
# > GV also screened studies that indicated 'Only land cover or habitat type as proxy' for ESP type

# assign rowid for tracking
r2keep$rowid <- as.numeric(rownames(r2keep))
# pull Q12 + Q14 (matrix + ESP type)
reviewlulc <- subset(r2keep, grepl("Driver|ESP|Resp", abbr)) %>%
  # only keep driver rows that have an answer
  subset(!(clean_answer %in% c(NA, "Other") & qnum == "Q12")) %>%
  # create new_group col where all groups = clean_group except in the case of lulc
  mutate(new_group = ifelse(grepl("^Land ", clean_answer_binned), "LU_LC", clean_group)) %>%
  group_by(Title, ResponseId) %>%
  # screen land use in different ways (via ESP answer and driver answers)
  mutate(lulc_ESP = any(grepl("Only land", clean_answer[qnum == "Q14"])), # NOTE!: ppl indicated ESPs for *response variable also*, not just drivers
         lulc_driver = any(grepl("^Land ", clean_answer_binned)),
         lulc_both = lulc_ESP & lulc_driver,
         # screen for papers that only indicate lulc as a driver
         only_lulc_driver  = str_flatten(unique(new_group[grepl("Driv", abbr)])) =="LU_LC",
         count_groups = length(unique(unique(new_group[grepl("Driv", abbr)]))),
         has_biotic = any(grepl("Biotic", new_group)),
         service_provider_id = any(grepl("Service provider", clean_answer_binned)),
         # screen for only lulc_ESP
         only_lulc_ESP = any(grepl("^Only.*proxy$", clean_answer[qnum == "Q14"])),
         count_ESP = str_count(clean_answer[qnum == "Q14"], "],")+1) %>%
  ungroup() %>%
  # if ESP count blank, assign 0 (no answer)
  replace_na(list(count_ESP = 0))

# review who is only lulc_ESP but not only lulc driver
onlylulcesp <- subset(reviewlulc, only_lulc_ESP & !only_lulc_driver & qnum == "Q12" & has_biotic & count_groups <3) %>%
  #further subset to studies that are lulc and biotic only or have just biotic as driver group to review
  # > be sure these shouldn't be counted among lulc only papers
  filter((has_biotic & lulc_driver) | count_groups == 1 | (service_provider_id & !lulc_driver)) %>%
  # join paper info to look up
  left_join(citations[c("Title", "AuthorsFull", "SourcePublication")])
# try to join methods answer
onlylulcesp <- left_join(onlylulcesp, rename(subset(r2keep, Title %in% unique(onlylulcesp$Title) & abbr == "Methods", select = c("ResponseId", "Title", "clean_answer")), Methods = clean_answer))
# how many
distinct(onlylulcesp[grepl("Model|remote", onlylulcesp$Methods), c("Title", "AuthorsFull", "SourcePublication")]) # 6, review manually
# what types of methods?
unique(onlylulcesp$Methods)

# Hiddink et al. 
# "cover" ESP data were trawling frequencies calculated from vessel data over gridded spatial product, they modeled biomass reduction in benthic habitat and eco impact
# > seems more data simulation, field data they used were observational biomass data to validate model output
# > seems like this paper should be excluded too based on methods. Not sure why ESP indicated as driver other than to create logical sense with ESP type question (I reviewed this one)
# *however* have management scenarios (entered as human driver) as an explanatory var, so does include more than land use land cover
# KEEP

# Paudyal et al. 2019
# > this is purely a GIS study, LB indicated methods were model/data simulation. only using satellite imagery to model ES provision. exclude
# > EXCLUDE

# Deguines et al. 2014
# this paper does use regional-aggregated drivers, but goes beyond land cover/geospatial data-derived products (e.g., uses crop identity, other indices, and is actually modeling as ES)
# KEEP -- and maybe should have another ESP type checked (like multiple species bc of the pollinator dependence variable? and also crop ID? kinda of weird to code)
# "pollinator dependence" is in there as an other biotic type along with ESP as a driver.. leave things as they are

# Solins et al. 2019
# this paper was double reviewed, one reviewer excluded (for stopping at abundance and not linking to EF/ES) and the other kept
# looking at paper, they did survey plot-level characteristics but as response variable. Driver was land cover derived from sat imagery. Plot canopy got entered as a driver (that's why paper wasn't flagged), but shouldn't have.
# > EXCLUDE

# Marcilio et al. 2018
# more of a scenario/simulation paper. only using forest cover type as driver.
# > EXCLUDE

# Zhao et al.2009
# entirely a GIS case study based on land cover type.
# > EXCLUDE

# what are the experimental papers that indicated land cover as proxy only?
View(onlylulcesp[grepl("Experi", onlylulcesp$Methods),]) # botbh have management as driver types besides biotic. Both have ESPs in biotic, Glover et al has more than ESP

# Cordingley et al. 2016
# > used plot level vegetation surveys to classify cover/habitat types, so land cover/hab type as proxy is appropriate
# > but also used fauna occurrence surveys to overlay biodiversity values -- however used more as response variable
# > they were looking at tradeoffs between biodiv and various ESes based on cover type
# > this seems like a case where it is appropriate to have ESP and land cover as proxy.. otherwise would need to change to vegetation cover (driver bin = biotic characteristics of the plot)
# > assessed for area, lots of plots surveyed over multiple decades involved. This is a paper that should stay.

# Glover et al.
# > agree veg (cropland) type as proxy
# > follow GV code and assign to LU_LC

# pull out land use land cover only by ESP answer and manual review to exclude
exclude_lulc_esps <- subset(onlylulcesp, grepl("Model|remote", Methods) & !grepl("Deguines|Hiddink", AuthorsFull)) %>%
  mutate(exclude_reason = "LULC ESP only, Biotic Service provider is cover related and doesn't have any other non-LULC driver")
length(unique(exclude_lulc_esps$Title)) # should be 4

# pull out what gets dropped first, and then recode
exclude_lulc_only <- subset(reviewlulc, only_lulc_driver) %>%
  mutate(exclude_reason = "LULC driver only") %>%
  rbind(exclude_lulc_esps[names(.)])
unique(exclude_lulc_only$Title) #29 papers

# note papers to keep after lulc review (this dataset started w everything, so has papers that don't have any lulc)
# use to recode driver type to LULC where needed (land cover/land use driver or only land cover ESP indicated with Service Provider as driver)
keep_fulltext_papers <- subset(reviewlulc, !Title %in% unique(exclude_lulc_only$Title))
# how many papers does this apply to? [has an lulc driver]
with(subset(keep_fulltext_papers, lulc_driver), length(unique(Title))) #79 papers
# lulc indicated as ESP but no lulc_driver present
with(subset(keep_fulltext_papers, !lulc_driver & lulc_ESP), length(unique(Title))) #13 (but GV's shouldn't have land cover so 12)


# what wasn't caught by GV land driver recode method) (so know how affects results in progress)

## GV code:
# Biotic drivers papers that only looked at land cover or habitat type as a proxy
biot_lulc_titles = r2keep %>%
  filter(qnum=='Q14', abbr=='ESP_type', clean_answer == 'Only land cover or habitat type as proxy') %>%
  pull(Title)

# separate out land use and land cover studies
driv_types_title = r2keep %>%
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

# exclude lulc studies <-- CTW note on GV code: this dataset is full text retained
excl_lulc =  driv_types_title %>%
  dplyr::select(-LU_LC) %>%
  filter(!(Biotic=='FALSE' & Human =='FALSE' & Environmental =='FALSE'))
# 110 studies used an lulc drivers, 26 studies only used lulc drivers

# what are the studies not caught in Grant's code?
subset(exclude_lulc_only, Title %in% excl_lulc$Title, select = c(Title, exclude_reason)) %>%
  distinct()
# 5 studies -- 4 only have LULC drivers, 1 had ESP is land cover as proxy and no non-LULC and Biotic driver = cover related (riparian canopy; other driver is urbanization, which is binned as LULC)

# are there any studies in my code not picked up in Grant's?
subset(exclude_lulc_only, Title %in% excl_lulc$Title, select = c(Title, exclude_reason)) %>%
  distinct()

# are there any papers in Grant's excluded, but weren't excluded by my process?
summary(with(driv_types_title, Title[!Title %in% excl_lulc$Title]) %in% unique(exclude_lulc_only$Title))
# > all there.. but says only 24.. GV noted 26.. but there were 2 NAs, so maybe that's why?

# how do driver types per paper differ from GV?
# > CTW note: should remove "Other" variable from this in case the "OtherDriver" driver (actually entered) was reclassed
# > e.g., if paper had "Other" (Environment) and other driver was LULC, the actual driver would be reclass but not "Other"
# save updated driver types
dat2 <- r2keep %>%
  filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
  dplyr::select(Title, clean_answer_binned, clean_group) %>%
  mutate(old_group = clean_group) %>%
  # reassign clean_answer_binned land cover answers to have Group 'LU LC'
  mutate(clean_group = ifelse(clean_answer_binned %in% c('Land cover', 'Land use and land cover change'), 'LU_LC',clean_group)) %>%
  # reassign studies with only land cover biotic drivers (based on Q14) to 'LU LC' Group
  mutate(clean_group = ifelse(Title %in% biot_lulc_titles & clean_group == 'Biotic', 'LU_LC', clean_group)) %>%
  # take only unique rows 
  unique() %>%
  ## CTW code added to GV code below here##
  group_by(Title) %>%
  # count distinct driver groups to check if anything was off
  mutate(types_GV = length(unique(clean_group)),
         types_GV_noOther = length(unique(clean_group[clean_answer_binned != "Other"])),
         conflict = types_GV != types_GV_noOther)
# how many papers affected?
length(unique(dat2$Title[dat2$conflict])) #14 papers -- Sankey should definitely be remade



# clean up environment before moving on
rm(dat2, excl_lulc, driv_types_title, biot_lulc_titles,
   needs_order, start_order, newQ12_surveyorder, grouplut, drivervars, ESlut, abbrlut, abbrcodes)



##### 3. Re-classify LULC drivers -----
# steps:
# 1. recode driver bins -- assign to new_group column for now so GV can stick with 3 main driver types if there is reason for that
# 2. reassign clean group for EffectDirect
# 3. add QA notes
# note: this will apply to individual reviews (for double reviewed papers) and final (single review or converged double review answers)

# figure out which papers need "Other" driver added bc of LULC switch (keep clean_group bc of existing other driver in that group + add LULC other), vs "Other" should be recoded too
# same for effect direct

# rules (2):
# "Other" row (abbr == "Driver):
# If only one other driver for a given group and it got recoded to LULC, "Other" can be recoded to LULC
# If more than one other driver for a given group and one got recoded to LULC, add another "Other" row for LULC

# Effect Direct:
# If only one driver for a given group and that group got switched to LULC, effect direct gets switched to LULC
# If more than one driver for a given group and only one got reclassed to LULC, add another effect direct row and copy effect direct value

# start new data frame with reclassed land cover binned vars to LULC
r2keep_lulc <- r2keep %>%
  # make new driver group as above with LU_LC
  # make qa_note column for lulc adjustments to keep what is done for that separate from other comments
  mutate(lulc_note = NA,
         # make note col character and not logical
         lulc_note = as.character(lulc_note),
         new_group = ifelse(grepl("^Land ", clean_answer_binned), "LU_LC", clean_group))

# pull out just other driver responses (abbr == Driver & clean_answer == Other, or abbr == "OtherDriver)
cleanup_other_lulc <- subset(r2keep_lulc, qnum == "Q12" & !is.na(clean_answer) & (clean_answer_binned == "Other" | abbr == "OtherDriver")) %>% # only keep rows that have an answer
  group_by(Title, ResponseId) %>%
  mutate(has_lulc = any(new_group == "LU_LC")) %>%
  ungroup()
# pull out papers that don't need any other driver adjustment
no_otherdriverchange_papers <- subset(cleanup_other_lulc, !has_lulc)
# proceed with only papers that need driver adjustment
cleanup_other_lulc <- subset(cleanup_other_lulc, has_lulc) %>%
  data.frame() %>%
  group_by(ResponseId, abbr, clean_group) %>%
  # count the number of other driver new groups for the driver type that got changed to lu_lc
  # > if 1 only, can change "Other" new_group to LU_LC
  # > if 2, need to add a new "Other" row
  mutate(numtypes_other_lulc = length(unique(new_group[any(new_group == "LU_LC")]))) %>%
  ungroup() %>%
  group_by(ResponseId, clean_group) %>%
  mutate(add_other = ifelse(any(numtypes_other_lulc==2), "add",
                            ifelse(any(numtypes_other_lulc == 1), "change", "ignore")))

# initiate new data frame for storing amended Other Driver answers
newother <- data.frame()
# iterate by ResponseId to evaluate
for(i in unique(cleanup_other_lulc$ResponseId)){
  tempdat <- subset(cleanup_other_lulc, ResponseId == i & add_other != "ignore")
  # iterate through whatever clean groups are present (e.g., could have lulc reclassed in env and/or human and/or biotic other drivers)
  for(c in unique(tempdat$clean_group)){
    # if it's "add" other row, copy the existing Other row and:
    # 1. assign new_group == "LU_LC"
    # 2. increment rowid by 0.5 (so will order correctly)
    # 3. add QA note (should be fine to overwrite whatever QA note was there since it's added)
    instruction <- unique(tempdat$add_other[tempdat$clean_group ==c])
    stopifnot(length(instruction)==1) # make sure only one instruction present per group
    if(instruction == "add"){
      # copy "Other" row and edit
      newrow <- subset(tempdat, clean_group == c & clean_answer == "Other")
      newrow$new_group <- "LU_LC"
      newrow$rowid <- newrow$rowid+0.5
      newrow$lulc_note <- paste("One of multiple", c, "other drivers entered recoded to 'Land Use Land Cover' driver type. Added 'Other' driver in LULC group for consistency.")
      # add newrow to tempdat
      tempdat <- rbind(tempdat, newrow)
    }else{ 
      # change Other to LULC because only other driver in the group is LULC
      # > note: may pull "Other" in a given group if entered across multiple ESes
      temprow <- with(tempdat, which(clean_answer == "Other" & clean_group == c))
      # update new_group to LU_LC
      tempdat$new_group[temprow] <- "LU_LC"
      # add QA note based on group changed from
      addnote <- paste("Other driver(s) entered in", c, "driver type recoded to Land Use Land Cover type. Updated 'Other' driver to LULC group for consistency.")
      # append (to one or more rows) and clean up
      tempdat$lulc_note[temprow] <- ifelse(is.na(tempdat$lulc_note[temprow]), addnote,
                                           paste(tempdat$lulc_note[temprow], addnote, sep = "; "))
      # clear any NAs or weird punctuation in pasting (".;)
      tempdat$lulc_note[temprow] <- gsub("[.];", ";", tempdat$lulc_note[temprow])
    }
  }
  # once cycle through all possible groups to adjust for a given survey, rbind to newother df
  newother <- rbind(newother, tempdat)
}

# order by rowid to review
View(arrange(newother, rowid)) # looks okay in manual review
# sub out these rowids from main dataset and add in updated rows 
r2keep_lulc <- subset(r2keep_lulc, !rowid %in% newother$rowid) %>%
  rbind(newother[names(.)])

# Grant also reclassed any biotic driver where land cover as proxy was only ESP checked as LU_LC
# > however, some biotic variables that show up are true biotic and not lulc
# only reclass the service providers (fixed answers) from biotic -> lulc when proxy checked in ESP_Type
# > there are 7 papers where only proxy checked for ESP and biotic driver is in "OtherDriver" -- typically some type of veg cover variable.
# > these papers wouldn't have their biotic vars recoded to lulc if only recode fixed service provider, but I think that's okay because they weren't claiming that those vars were service providers (whereas those papers that checked service provider present were)?
# > the issue with recoding those other drivers to lulc would be then binning would be inconsistent (e.g., paper that had perennial veg as a variable and didn't check land proxy esp would have that driver coded as biotic, not lulc)
# > .. to follow what GV did, if paper has service provider that can be lulc, if no service provider, check other driver to see if variable is cover or habitate related and recode to lulc

# what are the variables?
distinct(subset(reviewlulc, has_biotic & only_lulc_ESP & clean_group == "Biotic" & grepl("Driver", abbr), select = c(clean_answer, clean_answer_binned)))
# > pollination should stay as pollination, not lulc
# what are the variables in the papers that don't have a service provider?
subset(reviewlulc, has_biotic & !service_provider_id & only_lulc_ESP & clean_group == "Biotic" & grepl("Driver", abbr), select = c(ResponseId, clean_answer, clean_answer_binned)) %>%
  distinct() %>%
  arrange(ResponseId)
# only one type of other biotic variable per paper/review
# these mostly line up with lulc.. okay to recode so same papers have an lulc driver to match grant's results in progress

# these are service provider rowids to recode as lulc in new_group
serviceprov_lulc_rowids <- with(reviewlulc, rowid[service_provider_id & only_lulc_ESP & grepl("Service Provider", clean_answer)])
# these are the other drivers to recode to lulc when a service provider driver does not exist
otherbiotic_lulc_rowids <- with(reviewlulc, rowid[!service_provider_id & has_biotic & clean_group == "Biotic" & only_lulc_ESP & abbr == "OtherDriver"])

# review to be sure these are the correct rows to recode
r2keep_lulc <- left_join(r2keep_lulc, distinct(reviewlulc[c("ResponseId", "lulc_driver", "has_biotic", "service_provider_id", "only_lulc_ESP")]))
View(subset(r2keep_lulc, rowid %in% serviceprov_lulc_rowids)) # yup
View(subset(r2keep_lulc, rowid %in% otherbiotic_lulc_rowids))

# create qa notes
serviceprov_qanote <- "Reviewer noted ESP type is land cover or habitat proxy. Reclassed Service Provider driver from Biotic to Land Use Land Cover driver type."
otherbiotic_qanote <- "Reviewer noted ESP type is land cover or habitat proxy. Reviewed and reclassed Biotic other driver reported to Land Use Land Cover driver type."

# assign LULC to service providers
r2keep_lulc$new_group[r2keep_lulc$rowid %in% c(serviceprov_lulc_rowids, otherbiotic_lulc_rowids)] <- "LU_LC"
# add qa note to rows..
r2keep_lulc$lulc_note[r2keep_lulc$rowid %in% serviceprov_lulc_rowids] <- paste(r2keep_lulc$lulc_note[r2keep_lulc$rowid %in% serviceprov_lulc_rowids], serviceprov_qanote, sep ="; ")
r2keep_lulc$lulc_note[r2keep_lulc$rowid %in% otherbiotic_lulc_rowids] <- paste(r2keep_lulc$lulc_note[r2keep_lulc$rowid %in% otherbiotic_lulc_rowids], otherbiotic_qanote, sep = "; ")
# clean up qa note
r2keep_lulc$lulc_note[r2keep_lulc$rowid %in% c(serviceprov_lulc_rowids, otherbiotic_lulc_rowids)] <- gsub("NA; ", "", r2keep_lulc$lulc_note[r2keep_lulc$rowid %in% c(serviceprov_lulc_rowids, otherbiotic_lulc_rowids)])


# need to circle back and reassign "Other" or add "Other" row based on esp lulc recoding to other drivers
# rules:
# > if only "other drivers" were all recoded to lulc, clean_answer == other -- > lulc
# > if more than one "other driver" and not all recoded to lulc, then add other row (copy from biotic and increment rowid by 0.5 as above)
# easiest thing is to subset affected other driver records, tweak, then add back in to main dataset

esplulc_otherdriver_check <- subset(r2keep_lulc, grepl("Driver", abbr)) %>%
  group_by(ResponseId, ES) %>%
  # for whatever reason, filter subsets correct but 'subset' function does nothing
  filter(any(rowid %in% otherbiotic_lulc_rowids)) %>% # should be 95 rows
  ungroup() %>%
  # only keep other driver or abbr == Driver & clean_answer == "Other"
  subset(abbr == "OtherDriver" | clean_answer_binned == "Other") %>%
  group_by(ResponseId, ES, clean_group) %>%
  # id any biotic ESes where other driver new group == lulc
  mutate(new_grouplulc = any(new_group == "LU_LC"),
         # to be sure count number of new_groups per clean group (biotic should just have one)
         count_newgroup = length(unique(new_group[abbr == "OtherDriver"]))) %>%
  ungroup() # if subset to new_grouplulc, correctly IDs Others that need lulc reassignment (manual review)
# how many records is this?
nrow(distinct(esplulc_otherdriver_check, ResponseId, ES)) # 14 .. of course bc 14 biotic other drivers treated. good
# how many new groups in biotic other drivers? (should be just 1)
with(esplulc_otherdriver_check, unique(count_newgroup[new_grouplulc])) # good

# pull other rowids that need lulc update
espother_rowids <- with(esplulc_otherdriver_check, rowid[new_grouplulc & clean_answer == "Other"])
# are these the correct rows?
View(subset(r2keep, rowid %in% espother_rowids)) # looks ok

new_esp_otherdrivers <- subset(esplulc_otherdriver_check, rowid %in% espother_rowids) %>%
  # change new_group to LULC
  mutate(new_group = "LU_LC",
         # add QA note
         lulc_note = ifelse(is.na(lulc_note), otherbiotic_qanote, paste(lulc_note, otherbiotic_qanote, sep = "; ")),
         # clean up punctuation
         lulc_note = gsub("[.];", ";", lulc_note))

# sub out and add back in
r2keep_lulc <- subset(r2keep_lulc, !rowid %in% new_esp_otherdrivers$rowid) %>%
  rbind(new_esp_otherdrivers[names(.)]) %>%
  arrange(rowid)

# check that all drivers have a new group
with(r2keep_lulc, summary(!is.na(new_group[grepl("Driver", abbr) & !is.na(clean_answer)]))) # all have a new group
# review unique clean_answers, clean_answer bins, and grouping for new_group == "LU_LC"
View(distinct(subset(r2keep_lulc, new_group == "LU_LC" & grepl("Driver", abbr), 
                     select = c(clean_answer, clean_answer_binned, Group, clean_group, new_group)))) # looks fine
# review inverse
View(distinct(subset(r2keep_lulc, new_group != "LU_LC" & grepl("Driver", abbr), 
                     select = c(clean_answer, clean_answer_binned, Group, clean_group, new_group))))
# there are some inconsistencies in how variables like "canopy cover" are treated, but based on biotic reclass rules so.. that's what it is.
# (e.g., if service provider was reclassed when ESP land proxy checked, no need to reclass biotic other drivers too)
# (e.g., or if reviewer did not signal ESP land proxy, leave other drivers and service providers alone)

# review the rows added -- should be other rows added
View(subset(r2keep_lulc, !rowid %in% r2keep$rowid)) # yep

# clean up environment and move on
rm(c, i, espother_rowids, instruction, otherbiotic_lulc_rowids, serviceprov_lulc_rowids,
   cleanup_other_lulc, esplulc_otherdriver_check, lulc_otherdriver, new_esp_otherdrivers, newother,
   newrow, onlylulcesp, tempdat, temprow)




##### 4. Re-classify LULC effects -----
# update Effect Direct new_group to LU_LC based on presence of only one or more than one LU_LC drivers per clean_group
# > since drivers have been updated, remake logic columns for groups that have lulc
cleanup_effects_lulc <- subset(r2keep_lulc, qnum == "Q12" & grepl("Driver|Effect", abbr) & !is.na(clean_group), select = c(StartDate:new_group)) %>%
  # group by Response, ES, and clean_group to see if any new_groups are LULC
  group_by(ResponseId, ES, clean_group) %>%
  mutate(has_lulc = any(new_group == "LU_LC"),
         # count number of new_groups involved in driver rows
         count_driver_newgroup = length(unique(new_group[grepl("Driver", abbr)]))) %>%
  ungroup() %>%
  # count number of clean_groups per lulc per ES (just to know if multiple effect directions apply)
  group_by(ResponseId, ES, new_group) %>%
  mutate(count_newgroup_cleangroups = length(unique(clean_group[grepl("Driver", abbr)])),
         # count number of distinct effect directions just to see if lulc group will have more than one effect direction coming from different clean_groups
         count_newgroup_effects = length(unique(clean_answer[grepl("Effect", abbr)]))) %>%
  ungroup() %>%
  data.frame()

# how many records have lulc?
nrow(distinct(subset(cleanup_effects_lulc, has_lulc, select = c(ResponseId, clean_group, ES)))) # 318
# how many reviews?
length(unique(cleanup_effects_lulc$ResponseId[cleanup_effects_lulc$has_lulc])) #135

table(cleanup_effects_lulc[cleanup_effects_lulc$has_lulc, c("count_driver_newgroup", "count_newgroup_cleangroups")]) # 99 ES records where lulc drivers comes from multiple clean groups
# how many of those from more than 1 clean group have more than one effects direction type?
table(cleanup_effects_lulc[cleanup_effects_lulc$has_lulc & cleanup_effects_lulc$count_newgroup_cleangroups == 2, c("count_driver_newgroup", "count_newgroup_effects")])

# rule:
# if count_driver_newgroups == 1, can change effect direct new_group for that clean_group to lulc
# if count_driver_newgroups == 2, copy the effect direct row, assign lulc to new_group, and increment rowid by 0.5
# if more than one clean_group in an lulc driver per ES, just copy first clean_group row and note the other effect direct clean_group that applies
# > iterate by response id and ES
cleanup_effects_lulc <- subset(cleanup_effects_lulc, has_lulc) %>%
  mutate(loop_order = paste(ResponseId, ES))


# loop through effect direct updates, subset effect direct only to add back in 
# initialize master for storing updates
neweffectdirect <- data.frame() 
for(i in unique(cleanup_effects_lulc$loop_order)){
  tempdat <- subset(cleanup_effects_lulc, loop_order == i)
  # see how many clean groups are present (if 1 can proceed with whatever the clean_group is, if 2 use first as they appear in survey [by survey order])
  # also, can just grab 1st element in char vector (will be in order of appearance if 2, or single clean group if one)
  temp_cleangroup <- unique(tempdat$clean_group[tempdat$new_group == "LU_LC"])[1]
  temprow <- subset(tempdat, abbr == "EffectDirect" & clean_group == temp_cleangroup)
  # if temprow has more than one row bc lulc from more than one original group, keep the row that has an answer provided
  # OR first row if not
  if(any(!is.na(temprow$clean_answer))){
    temprow <- subset(temprow, !is.na(clean_answer))
  }
  # either way, take the first row (if more than one Group and two have answers, take first row)
  temprow <- temprow[1,]
  stopifnot(nrow(temprow) == 1)
  # if there is more than one driver new_groups per given clean_group, copy + add row, otherwise switch new_group --> LULC
  temprow$new_group <- "LU_LC" # either way, new_group gets recoded
  if(temprow$count_driver_newgroup == 1){
    # add note
    tempnote <- paste("Driver(s) entered in", temp_cleangroup, "driver type recoded to Land Use Land Cover type. Updated driver effect direction to LU_LC group for consistency.")
  }else{
    # add note and increment rowid by 0.5
    tempnote <- paste("One of multiple", temp_cleangroup, "other drivers entered recoded to 'Land Use Land Cover' driver type. Added", temp_cleangroup, "driver effect direction in LULC group for consistency.")
    temprow$rowid <- temprow$rowid + 0.5
  }
  # add whatever the tempnote is
  temprow$lulc_note <- ifelse(is.na(temprow$lulc_note), tempnote ,paste(temprow$lulc_note, tempnote, sep = "; "))
  # clean up any errant punctuation
  temprow$lulc_note <- gsub("[.];", ";", temprow$lulc_note)
  # append to master
  neweffectdirect <- rbind(neweffectdirect, temprow)
  
}

# take adjusted rowids out of main dataset and add back in
r2keep_lulc <- subset(r2keep_lulc, !rowid %in% neweffectdirect$rowid, select = c(StartDate:new_group)) %>%
  rbind(neweffectdirect[names(.)]) %>%
  arrange(rowid) %>%
  data.frame() %>%
  # to be sure no duplicates
  distinct()

# check what's added
View(subset(r2keep_lulc, !rowid %in% r2keep$rowid)) # only driver or effect direct rows added because clean_groups drivers still retained (good)
# check for duplicate rowids
nrow(subset(r2keep_lulc, rowid %in% duplicated(r2keep_lulc$rowid))) # nada. good



#### ADDITIONAL CLEAN UP -----
# 1. remove effect direct from clean_answer binned
r2keep_lulc$clean_answer_binned[r2keep_lulc$abbr == "EffectDirect"] <- NA
# 2. remove land cover as proxy from GV paper where another ESP indicated
# check that it's correct first+
View(subset(reviewlulc, lulc_ESP & count_ESP > 1))
correctESP <- with(reviewlulc, rowid[lulc_ESP & count_ESP > 1 & abbr == "ESP_type"])
r2keep_lulc$clean_answer[r2keep_lulc$rowid == correctESP] # yes
r2keep_lulc$clean_answer[r2keep_lulc$rowid == correctESP] <- gsub(",Only.*$", "", r2keep_lulc$clean_answer[r2keep_lulc$rowid == correctESP])
r2keep_lulc$lulc_note[r2keep_lulc$rowid == correctESP] # check existing note
r2keep_lulc$lulc_note[r2keep_lulc$rowid == correctESP] <- "CTW reviewed. Only Human, Environmental (one recoded to LU_LC) drivers. Response variable is Single ESP. Remove 'Only land cover as proxy' because no biotic driver indicated and land cover reflected in LU_LC driver. Single ESP is response variable."



#### LOGIC CHECK KREMEN TOPIC AND ESP TYPE -----
# screen for logical consistency after LULC recode

# RULES FOR KT3 ENV: (Environmental factors that influence provision)
# 1) has env driver (has_env) but Env not checked in KT Topics (!KT_env)
# 2) does NOT have env driver (!has_env) but Env check in KT Topics (KT_env)

# check KT3 with on LULC (won't necessarily change, but just want to know how it affects results)
has_env <- subset(r2keep_lulc, abbr %in% c("Driver", "OtherDriver") | qnum %in% c("Q13", "Q14", "Q8")) %>%
  #arrange(RecordedDate) %>%
  # check for "Service Provider" in clean_answer_finer by ResponseId
  group_by(ResponseId) %>%
  # look for any answer that is assigned to an environmental group
  mutate(has_env = any(grepl("Env", new_group)),
         KT = clean_answer[abbr == "KremenTopics"],
         # look for Kremen Topic 3 (environmental) in Kremen Topics answer
         KT_env = grepl("3", KT)) %>%
  # subset to either has an env answer and no KT 3 checked *or* the reverse
  filter((has_env & !KT_env) | (KT_env & !has_env)) %>%
  ungroup() %>%
  # keep only ResponseId, Title and flagging
  #dplyr::select(ResponseId, doublerev, Title, Bio_answer:ncol(.)) %>%
  subset(abbr %in%  c("KremenTopics", "KremenNotes", "ESP_type", "Driver", "OtherDriver", "Response", "Nested", "Extent") & !is.na(clean_answer)) %>%
  #filter(is.na(clean_group)) %>%
  distinct()

# how many papers does it affect (in manual look it's because of env --> land use land cover)
length(unique(has_env$Title)) #9 papers .. but these could be those excluded bc of lulc only
# check
summary(unique(has_env$Title) %in% exclude_lulc_only$Title) # only 2..
# maybe should note for Grant which papers would not have env topic if use lulc driver type coding
# check to see if these papers have env as the response variable (since some people used kremen topics question to reflect response vars)
check_env_response <- subset(r2keep_lulc, abbr == "Response" & !is.na(clean_answer) & ResponseId %in% has_env$ResponseId)
with(check_env_response, sapply(split(clean_answer, ResponseId), unique))
# several of these, if interpreting that Q generously, can say deals w environmental vars
# note which ones pass on response var
keepenv <- with(check_env_response, unique(ResponseId[grepl("carbon|environ", clean_answer)]))
# make notes about absence of env driver in lulc_note col
noenvrowids <- with(has_env, rowid[!ResponseId %in% keepenv & grepl("KremenT", abbr)])
envresponse_rowids <- with(has_env, rowid[ResponseId %in% keepenv & grepl("KremenT", abbr)])

r2keep_lulc$lulc_note[r2keep_lulc$rowid %in% noenvrowids] <- "After recode drivers to LU_LC, does not include environmental drivers (no KT3)."
r2keep_lulc$lulc_note[r2keep_lulc$rowid %in% envresponse_rowids] <- "After recode drivers to LU_LC, does not include environmental drivers, only has environmental response variables (no KT3)."




#### RE CHECK EXCLUSION AND FORMAT DATASET OUT -----
# look for papers that only have lulc as driver
r2keep_lulc <- group_by(r2keep_lulc, ResponseId) %>%
  mutate(drivers_present = str_flatten(unique(new_group[!is.na(clean_answer_binned) & grepl("Driver", abbr)]), collapse = ", "),
         only_lulc = drivers_present == "LU_LC") %>%
  ungroup()
# how many papers total have lulc driver?
length(unique(r2keep_lulc$Title[grepl("LU", r2keep_lulc$drivers_present)])) #115 (so about 40% of papers read for full text and retained [pre-lulc] by our coding)
# how many papers excluded? -- looking for 29, but some were esp only and exluded by manual review bc only GIS/RS case studies
length(unique(r2keep_lulc$Title[r2keep_lulc$only_lulc])) # yay
# be sure it's the same titles
summary(unique(r2keep_lulc$Title[r2keep_lulc$only_lulc]) %in% exclude_lulc_only$Title) # woohoo! agrees

# check all non-NA new_groups have a clean_answer
checkgroups <- distinct(subset(r2keep_lulc, grepl("Driver|Effect", abbr) & !is.na(clean_answer), select = c(ResponseId, clean_answer, clean_answer_binned, abbr, Group:clean_group, new_group)))
summary(is.na(checkgroups$new_group)) # everything has a new_group assigned that should. huzzah!

# check for weird punctuation in notes
summary(grepl("[.];", r2keep_lulc$qa_note)) # all okay

# check for any other weird answers/formatting issues
with(subset(r2keep_lulc, !grepl("Notes|GenIn|Uncer", abbr) & ! qnum == "Q12"), sapply(split(clean_answer, abbr), function(x) sort(unique(x))))
# looks okay

# check for ESes with orphan answers in Q12
EScheck <- subset(r2keep_lulc, qnum == "Q12" & !is.na(clean_answer)) %>%
  # only drivers and effect directs have groups, summarise response and yclass first
  group_by(ResponseId, ES) %>%
  mutate(has_response = any(!is.na(clean_answer[abbr == "Response"])),
         has_yclass = any(!is.na(clean_answer[abbr == "Yclass"])), 
         has_driver_inES = any(!is.na(clean_answer[grepl("Driver", abbr)])),
         has_effect_inES = any(!is.na(clean_answer[grepl("Effect", abbr)]))) %>%
  ungroup() %>%
  # drop NA Groups (Response and Yclass)
  subset(!is.na(Group)) %>%
  group_by(ResponseId, ES, Group, has_response, has_yclass, has_driver_inES, has_effect_inES) %>%
  summarise(has_clean_group = all(!is.na(clean_group[is.na(clean_answer)])),
            has_lulc_group = all(!is.na(clean_group[!is.na(clean_answer)])),
            has_driver = any(!is.na(clean_answer[grepl("Driver", abbr)])),
            has_effect = any(!is.na(clean_answer[grepl("Effect", abbr)])),
            has_effectnote = any(!is.na(qa_note[grepl("Effect", abbr)]))
  )

# at minimum, anything with a response should have a driver and vice versa
table(subset(EScheck, select = c(has_response, has_driver))) # 1 case is the same AK paper that has effect direct assigned in Group = Bio bc of clean_group reassignment Env --> Bio and other env vars still present
# i.e., all ok

# do all groups have a clean_group and lulc_group?
table(EScheck[c("Group", "has_clean_group", "has_lulc_group")]) # yes

# do same check with clean_group instead
EScheck <- subset(r2keep_lulc, qnum == "Q12" & !is.na(clean_answer)) %>%
  # only drivers and effect directs have groups, summarise response and yclass first
  group_by(ResponseId, ES) %>%
  mutate(has_response = any(!is.na(clean_answer[abbr == "Response"])),
         has_yclass = any(!is.na(clean_answer[abbr == "Yclass"])), 
         has_driver_inES = any(!is.na(clean_answer[grepl("Driver", abbr)])),
         has_effect_inES = any(!is.na(clean_answer[grepl("Effect", abbr)]))) %>%
  ungroup() %>%
  # drop NA Groups (Response and Yclass)
  subset(!is.na(Group)) %>%
  group_by(ResponseId, ES, clean_group, has_response, has_yclass, has_driver_inES, has_effect_inES) %>%
  summarise(has_clean_group = all(!is.na(clean_group[is.na(clean_answer)])),
            has_lulc_group = all(!is.na(clean_group[!is.na(clean_answer)])),
            has_driver = any(!is.na(clean_answer[grepl("Driver", abbr)])),
            has_effect = any(!is.na(clean_answer[grepl("Effect", abbr)])),
            has_effectnote = any(!is.na(qa_note[grepl("Effect", abbr)])),
            has_lulcnote = any(!is.na(lulc_note[grepl("Effect", abbr)]))
  )

# at minimum, anything with a response should have a driver and vice versa
table(subset(EScheck, select = c(has_response, has_driver))) # all good

# do all groups have a clean_group and lulc_group?
table(EScheck[c("has_clean_group", "has_lulc_group")]) # yes

# do all groups that have effects have and lulc_group?
table(EScheck[c("has_effect", "has_effectnote", "has_lulc_group")]) # has lulc group

# something odd with effects and effects. recrunch just that to check complete answers or notes
# do same check with clean_group instead
EScheck <- subset(r2keep_lulc, grepl("Driver|Effect", abbr)) %>%
  # only drivers and effect directs have groups, summarise response and yclass first
  group_by(ResponseId, ES, clean_group) %>%
  mutate(has_driver = any(!is.na(clean_answer[grepl("Driver", abbr)])),
         has_effect = any(!is.na(clean_answer[grepl("Effect", abbr)])),
         has_effectnote = any(!is.na(qa_note[grepl("Effect", abbr)])),
         has_lulcnote = any(!is.na(lulc_note[grepl("Effect", abbr)]))) %>%
  subset(has_driver) 
# > in manual review, see that effect/effect note are false when abbr != EffectDirect. when look at abbr == EffectDirect effect answers are either present or there is a qa note
table(subset(EScheck, abbr == "EffectDirect", select = c("has_effect", "has_effectnote"))) # what doesn't have an effect direction has a qa note confirming it's missing. huzzah!
# okay to move on

# be sure no duplicated rowids
summary(duplicated(r2keep_lulc$rowid)) # no dups




#### FINISHING -----
# rename new survey order cols to join
new_abbrcodes <- rename(new_abbrcodes, lulc_group = Group, lulc_survey_order = new_order)
# > update: adding in lulc_survey_code not necessary (too much of a pain to add in bc of mismatched id col values)
# > since keeping clean_group col in final dataset, okay to just have original survey order (can come back to this if needed)
# remove unneeded cols, rename new_group to lulc_group, add lulc_survey_order
r2keep_lulc_out <- rename(r2keep_lulc, lulc_group = new_group) %>%
  # make sure arranged by rowid
  arrange(rowid) %>%
  subset(select = c(StartDate:clean_group, lulc_group, qnum:qa_note, lulc_note, only_lulc))


# for exclusion dataset, maybe preserve what drivers they had and their ESP type response? although someone could see that in reading in the main dataset
exclude_lulc_only_out <- subset(r2keep_lulc_out, only_lulc & qnum %in% c("Q12", "Q14")) %>%
  # further subset to driver only for Q12 and clean answers that aren't NA
  subset((qnum == "Q12" & grepl("Driver", abbr) & !is.na(clean_answer)) | qnum == "Q14") %>%
  left_join(distinct(exclude_lulc_only[c("ResponseId", "exclude_reason")]))



# MAKE ES DAT FOR SANKEY
# save updated driver types
# (from GV final_analses/results.R script -- tweak below for new lulc structure of dataset)
# r2keep_lulc_out %>%
#   filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
#   dplyr::select(Title, clean_answer_binned, clean_group) %>%
#   mutate(old_group = clean_group) %>%
#   # reassign clean_answer_binned land cover answers to have Group 'LU LC'
#   mutate(clean_group = ifelse(clean_answer_binned %in% c('Land cover', 'Land use and land cover change'), 'LU_LC',clean_group)) %>%
#   # reassign studies with only land cover biotic drivers (based on Q14) to 'LU LC' Group
#   mutate(clean_group = ifelse(Title %in% biot_lulc_titles & clean_group == 'Biotic', 'LU_LC', clean_group)) %>%
#   # take only unique rows 
#   unique() %>%
#   write.csv(file='round2_metareview/analyze_data/final_analyses/lulc_reclass_bytitle.csv', row.names=F)

update_driver_types <- r2keep_lulc_out %>%
  # pull final answers for analysis only
  subset(version == "final") %>%
  # pull driver answers that are not NA
  filter(grepl("Driver", abbr), !is.na(clean_answer)) %>%
  # leaving in studies that are lulc_only
  dplyr::select(Title, clean_answer_binned, lulc_group) %>%
  # rename to clean_group so sankey code runs
  rename(clean_group = lulc_group) %>%
  # take only unique rows
  distinct() %>%
  # ctw -- drop 'Other' from binned answers since it's just a stand in
  subset(!clean_answer_binned == "Other") #%>%
# write.csv(file='round2_metareview/analyze_data/final_analyses/lulc_reclass_bytitle.csv', row.names=F)


# save driver types and es types for sankey
# preserve GV code to see goal of output
# dat %>%
#   filter(abbr %in% c('Driver', 'OtherDriver'), !is.na(clean_answer)) %>%
#   dplyr::select(Title, clean_answer_binned, clean_group) %>%
#   mutate(old_group = clean_group) %>%
#   # reassign clean_answer_binned land cover answers to have Group 'LU LC'
#   mutate(clean_group = ifelse(clean_answer_binned %in% c('Land cover', 'Land use and land cover change'), 'LU_LC',clean_group)) %>%
#   # reassign studies with only land cover biotic drivers (based on Q14) to 'LU LC' Group
#   mutate(clean_group = ifelse(Title %in% biot_lulc_titles & clean_group == 'Biotic', 'LU_LC', clean_group)) %>%
#   dplyr::select(-clean_answer_binned, -old_group) %>%
#   # take only unique rows 
#   unique() %>%
#   rename(driv_type = clean_group) %>%
#   # join with ES types
#   full_join(
#     dat %>%
#       filter(abbr=='Response') %>% 
#       filter(!is.na(clean_answer)) %>% 
#       dplyr::select(Title, ES) %>%
#       unique(),
#     by='Title'
#   ) %>%
#   write.csv(file='round2_metareview/analyze_data/final_analyses/lulcreclass_driv_ES_bytitle.csv', row.names=F)

update_es_driver_types <- r2keep_lulc_out  %>%
  # pull final answers for analysis only
  subset(version == "final") %>%
  # pull driver answers that are not NA
  filter(grepl("Driver", abbr), !is.na(clean_answer), !clean_answer == "Other") %>%
  dplyr::select(Title, ES, lulc_group) %>%
  # take unique rows
  distinct() %>%
  rename(driv_type = lulc_group) #%>%
# write.csv(file='round2_metareview/analyze_data/final_analyses/lulcreclass_driv_ES_bytitle.csv', row.names=F)

biot_lulc_titles <- subset(r2keep_lulc_out, version == "final" & qnum == "Q14" & grepl("Only land cover", clean_answer))


#### WRITE OUT -----
# write out ES-driver table for Sankey
write.csv(update_driver_types, file='round2_metareview/analyze_data/final_analyses/lulc_reclass_bytitle.csv', row.names=F)
write.csv(update_es_driver_types, file='round2_metareview/analyze_data/final_analyses/lulcreclass_driv_ES_bytitle.csv', row.names=F)

# write out lulc-adjusted analysis clean full text dataset
# > different name than previous main dataset to not mess up any analyses code developed (it shouldn't, but just in case)
write_csv(r2keep_lulc_out, "round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned_lulc.csv")

# write out lulc-excluded dataset
write_csv(exclude_lulc_only_out, "round2_metareview/data/cleaned/ESqualtrics_r2excludelulc_cleaned.csv")

