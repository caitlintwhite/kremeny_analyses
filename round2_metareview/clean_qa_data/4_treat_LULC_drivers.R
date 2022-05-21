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
length(unique(dat2$Title[dat2$conflict])) #14 papers -- Sankey should definitely be remade



# clean up environment before moving on
rm(dat2, excl_lulc, driv_types_title, biot_lulc_titles,
   needs_order, start_order, newQ12_surveyorder, grouplut, drivervars, ESlut, abbrlut, abbrcodes)



##### 3. Treat missing effect directions -----
# caught an error -- make sure any effectdirect that has a driver entered has a clean_group assigned, even if reviewer didn't provide an answer
# having all clean_groups infilled that should be infilled (whether reviewer provided answer or not helps keep lulc reassignment cleaner)
hasdriver <- subset(r2keep, qnum == "Q12" & grepl("Driver|Effect", abbr)) %>%
  group_by(ResponseId, ES, Group) %>%
  mutate(has_driver = any(!is.na(clean_answer_binned)),
         effectmissing = any(is.na(clean_answer[abbr == "EffectDirect"]))) %>%
  ungroup() %>%
  # subset to just those papers that have a driver entered but no answer to effect direct
  subset(has_driver & effectmissing)
# > not sure what to do where clean_group is different than original Group... 
# how many reviews is this?
unique(hasdriver$ResponseId) # 104..
# decide to assign clean_group just based on original Group:
# if there is only one clean_group and it's different than the original Group, enter clean_group
# else enter the original Group (even when another clean_group is present within a given original Group -- because ultimately that answer would have been copied over to the Group row corresponding to clean_group)
# > ultimately this is just for documenting no answer was provided

noeffectdirect <- subset(hasdriver, select = c(ResponseId, Group, clean_group, ES, rowid, has_driver, effectmissing)) %>%
  distinct()
# iterate through to add clean_groups and note to QA column
for(i in unique(hasdriver$ResponseId)){
  tempdrivers <- subset(hasdriver, ResponseId ==i & grepl("Driver", abbr)) %>%
    # subset just to Group, clean_group, and ES
    distinct(ResponseId, Group, clean_group, ES)
  tempeffect <- subset(hasdriver, ResponseId == i & abbr == "EffectDirect")
  # iterate through each ES
  for(e in unique(tempdrivers$ES)){
    tempES <- subset(tempdrivers, ES == e)
    for(g in unique(tempES$Group)){
      # find relevant rowid in the main dataset
      temprow <- with(tempeffect, rowid[ES == e & Group == g])
      # assign clean_group based on clean_group present (if more than one, default to original)
      if(any(tempES$clean_group == g)){
        # assign original group
        r2keep$clean_group[r2keep$rowid == temprow] <- g
      }else{
        # if original group not there assign whatever the clean_group is (i.e., when only driver entered was assigned to a different clean group)
        r2keep$clean_group[r2keep$rowid == temprow] <- unique(tempES$clean_group)
      }
      # add QA note
      tempnote <- paste0("No effect direction provided for driver(s) entered in ", g, ". 'Clean_group' driver types assigned within ", g, " are: ", str_flatten(unique(tempES$clean_group[tempES$Group == g]), collapse = ", "), ".")
      if(!is.na(r2keep$qa_note[r2keep$rowid == temprow])){
        # if note present, append
        r2keep$qa_note[r2keep$rowid == temprow] <- paste0(r2keep$qa_note[r2keep$rowid == temprow], "; ", tempnote)
      }else{
        # assign tempnote
        r2keep$qa_note[r2keep$rowid == temprow] <- tempnote
      }
      # cycle to next group
    }
  }
}

# review what was ammended
View(subset(r2keep, rowid %in% noeffectdirect$rowid)) # looks okay


##### 4. Re-classify LULC drivers -----
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
  mutate(new_group = ifelse(grepl("^Land ", clean_answer_binned), "LU_LC", clean_group))

# pull out just other driver responses (abbr == Driver & clean_answer == Other, or abbr == "OtherDriver)
cleanup_other_lulc <- subset(r2keep_lulc, qnum == "Q12" & !is.na(clean_answer) & (clean_answer_binned == "Other" | abbr == "OtherDriver")) %>% # only keep rows that have an answer
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
      # > note: may pull "Other" in a given group if entered across multiple ESes
      temprow <- with(tempdat, which(clean_answer == "Other" & clean_group == c))
      # update new_group to LU_LC
      tempdat$new_group[temprow] <- "LU_LC"
      # add QA note based on group changed from
      addnote <- paste("Other driver(s) entered in", c, "driver type recoded to Land Use Land Cover type. Updated 'Other' driver to LULC group for consistency.")
      # append (to one or more rows) and clean up
      tempdat$qa_note[temprow] <- ifelse(is.na(tempdat$qa_note[temprow]), addnote,
                                         paste(tempdat$qa_note[temprow], addnote, sep = "; "))
      # clear any NAs or weird punctuation in pasting (".;)
      tempdat$qa_note[temprow] <- gsub("[.];", ";", tempdat$qa_note[temprow])
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
r2keep_lulc$qa_note[r2keep_lulc$rowid %in% serviceprov_lulc_rowids] <- paste(r2keep_lulc$qa_note[r2keep_lulc$rowid %in% serviceprov_lulc_rowids], serviceprov_qanote, sep ="; ")
r2keep_lulc$qa_note[r2keep_lulc$rowid %in% otherbiotic_lulc_rowids] <- paste(r2keep_lulc$qa_note[r2keep_lulc$rowid %in% otherbiotic_lulc_rowids], otherbiotic_qanote, sep = "; ")
# clean up qa note
r2keep_lulc$qa_note[r2keep_lulc$rowid %in% c(serviceprov_lulc_rowids, otherbiotic_lulc_rowids)] <- gsub("NA; ", "", r2keep_lulc$qa_note[r2keep_lulc$rowid %in% c(serviceprov_lulc_rowids, otherbiotic_lulc_rowids)])












# update Effect Direct answer to LULC based on presence of only one or more than one drivers per group
# might need to iterate through ES on this one (Effect Direct may not be entered similarly across all ESes?)
# use newother in conjuction with fixed driver answers to recode effect direction as needed


# proceed with effect direction adjustment for lulc -- these are effect directs adjusted when clean_answer_binned is land cover related (has land cover in binned name)
cleanup_effects_lulc <- subset(r2keep, qnum == "Q12" & abbr %in% c("Driver", "EffectDirect") & !grepl("^Other$", clean_answer_binned)) %>%
  # make new driver group as above with LU_LC
  mutate(new_group = ifelse(grepl("^Land ", clean_answer_binned), "LU_LC", clean_group)) %>%
  group_by(Title, ResponseId) %>%
  mutate(has_lulc = any(new_group == "LU_LC")) %>%
  ungroup() %>%
  data.frame() %>%
  # add "Other" driver answers that were adjusted (whether type changed to LULC or added because of multiple drivers)
  mutate(add_other = NA) %>%
  rbind(subset(newother, clean_answer_binned == "Other", select = names(.))) %>%
  group_by(Title, ResponseId, clean_group, ES) %>%
  mutate(has_lulc2 = any(new_group == "LU_LC")) %>%
  ungroup() %>%
  group_by(ResponseId, clean_group, ES) %>%
  # count the number of other driver new groups for the driver type that got changed to lu_lc
  # > if 1 only, can change "Other" new_group to LU_LC
  # > if 2, need to add a new "Other" row
  mutate(numtypes_other_lulc =length(unique(new_group[any(new_group == "LU_LC") & abbr == "Driver" & !is.na(new_group)])),
         add_other2= ifelse(any(numtypes_other_lulc==2), "add",
                            ifelse(any(numtypes_other_lulc == 1), "change", "ignore")),
         # check if effect direct answer present
         effect_present = any(!is.na(clean_answer[abbr == "EffectDirect"]))) %>%
  ungroup() %>%
  # only treat those who have an effect direct answer to update
  subset(add_other2 != "ignore" & effect_present) %>%
  # add iteration order for updating
  mutate(loop_order = paste(ResponseId, clean_group, ES, sep = "_"))


# loop through effect direct updates, subset effect direct only to add back in 
# initialize master for storing updates
neweffectdirect <- data.frame() 
for(i in unique(cleanup_effects_lulc$loop_order)){
  tempdat <- subset(cleanup_effects_lulc, loop_order == i)
  # iterate through whatever clean groups are present (e.g., could have lulc reclassed in env and/or human and/or biotic other drivers)
  # if it's "add" other row, copy the existing Other row and:
  # 1. assign new_group == "LU_LC"
  # 2. increment rowid by 0.5 (so will order correctly)
  # 3. add QA note (should be fine to overwrite whatever QA note was there since it's added)
  instruction <- unique(tempdat$add_other2)
  stopifnot(length(instruction)==1) # make sure only one instruction present per group
  if(instruction == "add"){
    # copy "Other" row and edit
    newrow <- subset(tempdat, abbr == "EffectDirect")
    newrow$new_group <- "LU_LC"
    newrow$rowid <- newrow$rowid+0.5
    newrow$qa_note <- paste("One of multiple", unique(tempdat$clean_group), "other drivers entered recoded to 'Land Use Land Cover' driver type. Added", unique(tempdat$clean_group), "driver effect direction in LULC group for consistency.")
    # add newrow to tempdat
    tempdat <- rbind(tempdat, newrow)
  }else{ 
    # change Other to LULC because only other driver in the group is LULC
    # > note: may pull "Other" in a given group if entered across multiple ESes
    temprow <- with(tempdat, which(abbr == "EffectDirect"))
    # update new_group to LU_LC
    tempdat$new_group[temprow] <- "LU_LC"
    # add QA note based on group changed from
    addnote <- paste("Other driver(s) entered in", unique(tempdat$clean_group), "driver type recoded to Land Use Land Cover type. Updated driver effect direction to LULC group for consistency.")
    # append (to one or more rows) and clean up
    tempdat$qa_note[temprow] <- ifelse(is.na(tempdat$qa_note[temprow]), addnote,
                                       paste(tempdat$qa_note[temprow], addnote, sep = "; "))
    # clear any NAs or weird punctuation in pasting (".;)
    tempdat$qa_note[temprow] <- gsub("[.];", ";", tempdat$qa_note[temprow])
  }
  # once cycle through all possible groups to adjust for a given survey, rbind to newother df
  neweffectdirect <- rbind(neweffectdirect, subset(tempdat, abbr == "EffectDirect"))
}

# add updated other driver and effect direct rows back into main dataset
# > need to add in: recoded other drivers, recoded effect direct + LULC group
# take out rowids in updated sets, then add those in, check for all rowids present after

# preserve r2keep as is
r2keep_lulc <- r2keep %>%
  # since lulc-adjusted drivers will be in their own column, easiest thing might be to join that column
  left_join(reviewlulc) %>%
  # remove rowids to swap
  subset(!rowid %in% c(neweffectdirect$rowid, newother$rowid), select = c(StartDate:new_group)) %>%
  rbind(neweffectdirect[names(.)], newother[names(.)]) %>%
  arrange(rowid)

# check what's added
View(subset(r2keep_lulc, !rowid %in% r2keep$rowid)) # only driver or effect direct rows added because clean_groups drivers still retained (good)
# check for duplicate rowids
View(subset(r2keep_lulc, rowid %in% duplicated(r2keep_lulc$rowid))) # nada. good
# screen for lulc driver consistency
r2keep_lulc <- group_by(r2keep_lulc, ResponseId, qnum, clean_group, ES) %>%
  mutate(has_lulc = any(new_group == "LU_LC")) %>%
  ungroup() # I think it's okay (manual review)

# make sure missing effect directions have their new_groups assigned when new_group is not lulc
subset(r2keep_lulc, has_lulc & grepl("no effect direction", qa_note, ignore.case = T) & is.na(new_group), select = c(Group, clean_group, qa_note)) %>%
  distinct() %>%
  data.frame() # okay to assign clean_group for these. only one clean_group per Group in these cases
# id relevant rows
infillrows <- with(subset(r2keep_lulc, has_lulc & grepl("no effect direction", qa_note, ignore.case = T) & is.na(new_group)), rowid)
# verify these are correct
View(r2keep_lulc[r2keep_lulc$rowid %in% infillrows,]) #yup
r2keep_lulc$new_group[r2keep_lulc$rowid %in% infillrows] <- r2keep_lulc$clean_group[r2keep_lulc$rowid %in% infillrows]



# these would also potentially need effect directs reclassed from biotic to lulc... :(
# > or effect direct rows added when additional biotic variables remain
# rules:
# > if was only biotic variables/all biotic reclass to lulc, biotic effect direct new_group --> lulc
# if was one of many biotic variables reclassed, see if effect direct for new group already exists:
# if so, do nothing (could change to mixed if answers conflict, but that would change answer for other original group [e.g., lulc in environmental])
# if not, copy biotic effect row, make new_group lulc and increment rowid by 0.5

# > start fresh
r2keep_lulc <- distinct(subset(r2keep_lulc, select = c(StartDate:new_group)))
# be sure no duplicate rowids so far (shouldn't be)
summary(duplicated(r2keep_lulc$rowid)) # no dups
# note which ESes affected by ESP reclass to lulc
revieweffects_esplulc <- subset(r2keep_lulc, grepl("Effect|Driver", abbr)) %>%
  group_by(ResponseId, ES) %>%
  mutate(has_driver = any(!is.na(clean_answer)),
         esp_rowid = rowid %in% c(serviceprov_lulc_rowids, otherbiotic_lulc_rowids)) %>%
  subset(has_driver) %>%
  ungroup() %>%
  group_by(ResponseId, ES, Group) %>%
  mutate(has_esp_rowid = any(esp_rowid)) %>%
  subset(has_esp_rowid) %>%
  ungroup()
# check it's number of records expected
nrow(distinct(revieweffects_esplulc[c("ResponseId", "ES")])) #34 [records flagged]
length(c(serviceprov_lulc_rowids, otherbiotic_lulc_rowids)) # 34 [biotic drivers reclassed due to esp; agrees]

#%>%
  # group_by(ResponseId, Group, ES) %>%
  # mutate(missing_newgroup = ifelse(grepl("Effect|Driver", abbr) & !is.na(clean_answer), !is.na(clean_group) & is.na(new_group), NA),
  #   newgroup_drivers = ifelse(!grepl("Effect|Driver", abbr), NA, str_flatten(sort(unique(new_group[grepl("Driver", abbr) & !is.na(new_group)])))),
  #        newgroup_effects = ifelse(!grepl("Effect|Driver", abbr), NA, str_flatten(sort(unique(new_group[grepl("Effect", abbr)])))),
  #        has_lulc_drivers = ifelse(!grepl("Effect|Driver", abbr), NA, any(grepl("LU", newgroup_drivers))),
  #        has_lulc_effects = ifelse(!grepl("Effect|Driver", abbr), NA, any(grepl("LU", newgroup_effects))),
  #   esp_adjusted = rowid %in% c(serviceprov_lulc_rowids, otherbiotic_lulc_rowids)) %>%
  # ungroup()

# review consistency
lulc_effects_review <- subset(r2keep_lulc, has_lulc_drivers & ((newgroup_drivers != newgroup_effects) | missing_newgroup))
length(unique(lulc_effects_review$ResponseId))



##### 5. Re-classify LULC effects -----



##### 6. Assign new_group for unaffected drivers and effect directs -----
# if driver and effect direct stayed biotic/env/human, assign those to new groups (only affected answers treated above)




#### ADDITIONAL CLEAN UP -----
# 1. remove effect direct from clean_answer binned
r2keep_lulc$clean_answer_binned[r2keep_lulc$abbr == "EffectDirect"] <- NA
# 2. remove land cover as proxy from GV paper where another ESP indicated
# check that it's correct first+
View(subset(reviewlulc, lulc_ESP & count_ESP > 1))
correctESP <- with(reviewlulc, rowid[lulc_ESP & count_ESP > 1 & abbr == "ESP_type"])
r2keep_lulc$clean_answer[r2keep_lulc$rowid == correctESP] # yes
r2keep_lulc$clean_answer[r2keep_lulc$rowid == correctESP] <- gsub(",Only.*$", "", r2keep_lulc$clean_answer[r2keep_lulc$rowid == correctESP])
r2keep_lulc$qa_note[r2keep_lulc$rowid == correctESP] # check existing note
r2keep_lulc$qa_note[r2keep_lulc$rowid == correctESP] <- "CTW reviewed. Only Human, Environmental (one recoded to LU_LC) drivers. Response variable is Single ESP. Remove 'Only land cover as proxy' because no biotic driver indicated and land cover reflected in LU_LC driver. Single ESP is response variable."



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
  mutate(env_answer = str_flatten(unique(clean_answer_binned[grepl("Env", new_group) & !is.na(clean_answer_binned)])), # flatten all env driver bin labels that aren't NA
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

# how many papers does it affect (in manual look it's because of env --> land use land cover)
length(unique(has_env$Title)) #10 papers


#### CLEAN UP AND RE CHECK EXCLUSION -----
r2keep_out <- ungroup(r2keep_lulc) %>%
  data.frame() %>%
  group_by(ResponseId) %>%
  mutate(drivers_present = str_flatten(unique(new_group[!is.na(clean_answer_binned) & grepl("Driver", abbr) & !is.na(new_group)]), collapse = ", "),
         only_lulc = drivers_present == "LU_LC")
# how many papers total have lulc driver?
length(unique(r2keep_out$Title[grepl("LU", r2keep_out$drivers_present)])) #115 (so about 40% of papers read for full text and retained [pre-lulc] by our coding)
# how many papers excluded? -- looking for 29, but some were esp only and exluded by manual review bc only GIS/RS case studies
length(unique(r2keep_out$Title[r2keep_out$only_lulc])) # yay
# be sure it's the same titles
summary(unique(r2keep_out$Title[r2keep_out$only_lulc]) %in% exclude_lulc_only$Title) # woohoo! agrees

# check all non-NA new_groups have a clean_answer
checkgroups <- distinct(subset(r2keep_out, grepl("Driver|Effect", abbr) & !is.na(clean_answer), select = c(ResponseId, clean_answer, clean_answer_binned, abbr, Group:clean_group, new_group)))
View(subset(checkgroups, is.na(new_group)))
# forgot to assign clean_group as new_group for driver and effect direction responses unaffected by LULC recode
assigngroup_rowids <- with(r2keep_out, rowid[is.na(new_group) & !is.na(clean_group) & !is.na(clean_answer) & grepl("Effect|Driver", abbr)])
View(r2keep_out[r2keep_out$rowid %in% assigngroup_rowids,])
# check all non-NA new_groups that are driver rows have clean_answer_binned and clean_group

# are these in no_driver
summary(no_driverchange_papers$rowid %in% assigngroup_rowids) # some
View(subset(no_driverchange_papers, rowid %in% assigngroup_rowids)) # all abbr == Driver, makes sense
View(subset(no_driverchange_papers, !rowid %in% assigngroup_rowids))
View(subset(r2keep_out, Title ))







#### MAKE ES DAT FOR SANKEY -----

# save driver types and es types for sankey
dat3 <- r2keep_lulc %>%
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


