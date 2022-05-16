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
start_order <- (trunc(max(new_abbrcodes$new_order, na.rm = T))+1)
needs_order <- which(is.na(new_abbrcodes$new_order))
new_abbrcodes$new_order[needs_order] <- seq.int(start_order, length.out = length(needs_order), by =1)
# finally, start order at 1 (begins at 20)
if(min(new_abbrcodes$new_order) != 1){
  new_abbrcodes$new_order <- new_abbrcodes$new_order - (min(new_abbrcodes$new_order)-1)
}




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
length(unique(dat2$Title[dat2$conflict])) #15 papers -- Sankey should definitely be remade


# clean up environment before moving on
rm(dat2, excl_lulc, driv_types_title, biot_lulc_titles,
   needs_order, start_order, newQ12_surveyorder, grouplut, drivervars, ESlut, abbrlut, abbrcodes)



##### 3. Re-classify land use and land cover drivers and effects -----
# steps:
# 1. recode driver bins -- assign to new_group column for now so GV can stick with 3 main driver types if there is reason for that
# 2. reassign clean group for EffectDirect
# 3. add QA notes
# note: this will apply to individual reviews (for double reviewed papers) and final (single review or converged double review answers)

# preserve r2keep as is
r2keep_lulc <- r2keep %>%
  # since lulc-adjusted drivers will be in their own column, easiest thing might be to join that column
  left_join(reviewlulc)

# figure out which papers need "Other" driver added bc of LULC switch (keep clean_group bc of existing other driver in that group + add LULC other), vs "Other" should be recoded too
# same for effect direct

# rules:
# "Other" row (abbr == "Driver):
# If only one other driver for a given group and it got recoded to LULC, "Other" can be recoded to LULC
# If more than one other driver for a given group and one got recoded to LULC, add another "Other" row for LULC

# Effect Direct:
# If only one driver for a given group and that group got switched to LULC, effect direct gets switched to LULC
# If more than one driver for a given group and only one got reclassed to LULC, add another effect direct row and copy effect direct value

cleanup_other_lulc <- subset(r2keep, qnum == "Q12" & !is.na(clean_answer) & (clean_answer_binned == "Other" | abbr == "OtherDriver")) %>% # only keep rows that have an answer
  # make new driver group as above with LU_LC
  mutate(new_group = ifelse(grepl("^Land ", clean_answer_binned), "LU_LC", clean_group)) %>%
  group_by(Title, ResponseId) %>%
  mutate(has_lulc = any(new_group == "LU_LC")) %>%
  ungroup()
# pull out papers that don't need any driver adjustment
no_driverchange_papers <- subset(cleanup_other_lulc, !has_lulc)
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
      newrow$qa_note <- paste("One of multiple", c, "other drivers entered recoded to 'Land Use Land Cover' driver type. Added 'Other' driver in LULC group for consistency.")
      # add newrow to tempdat
      tempdat <- rbind(tempdat, newrow)
    }else{ 
      # change Other to LULC because only other driver in the group is LULC
      temprow <- with(tempdat, which(clean_answer == "Other" & clean_group == c))
      stopifnot(length(temprow) != 1)
      tempdat$new_group[temprow] <- "LU_LC"
      # add QA note
      addnote <- paste("Other driver(s) entered in", c, "driver type recoded to Land Use Land Cover type. Updated 'Other' driver to LULC group for consistency.")
      if(is.na(tempdat$qa_note[temprow])){
        # if blank, assign new note
        tempdat$qa_note[temprow] <- addnote
      }else
        # else append
        tempdat$qa_note[temprow] <- paste(tempdat$qa_note[temprow], addnote, sep = "; ")
    }
  }
  # once cycle through all possible groups to adjust for a given survey, rbind to newother df
  newother <- rbind(newother, tempdat)
}




## remove effect direction from binned answers (shouldn't be in there -- thought removed before)
# mutate(clean_answer_binned = ifelse(abbr == "EffectDirect", NA, clean_answer_binned),


#### LOGIC CHECK KREMEN TOPIC AND ESP TYPE -----
# screen for logical consistency after LULC recode


#### MAKE ES DAT FOR SANKEY -----

# save driver types and es types for sankey
dat3 <- r2keep %>%
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
    r2keep %>%
      filter(abbr=='Response') %>% 
      filter(!is.na(clean_answer)) %>% 
      dplyr::select(Title, ES) %>%
      unique(),
    by='Title'
  )


#### FINISHING -----
# write out ES-driver table for Sankey

# write out lulc-adjusted analysis clean full text dataset


# write out lulc-excluded dataset


