# make study summary figs

# script purpose:
# read in current prepped dataset [will change as incorp more answers and clean]
# subset by question and widen for LD
# make following study summary figs:
# 1) Study location summary
# 2) Ecosystem type studied (q4)
# 3) Study type
# 4) Temporal component
# 5) Feedbacks studied? counts (q21)
# 6) Threshold/non-linearity discussed? summary (q22)
# write out to round2_metareview/figs
## > use consistent, intuitive file nomenclature

# notes:
# helpful online resources for making study location map figures
# using rnaturalearth data: https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# dissolve geometry for sf: https://philmikejones.me/tutorials/2015-09-03-dissolve-polygons-in-r/



# -- SETUP -----
library(tidyverse)
library(forcats)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
options(stringsAsFactors = F)
na_vals <- c("NA", "NaN", "", " ", ".", NA, NaN)
theme_set(theme_classic())

# read in latest metareview data
rawdat <- read.csv("round2_metareview/data/cleaned/prelim_singlereview.csv", na.strings = na_vals)
# check read in as expected
glimpse(rawdat)


# function for breaking out comma separated answers
splitcom <- function(df, keepcols = c("Title", "answer")){
  df <- df[keepcols] %>%
    #dplyr::select_vars(keepcols) %>%
    distinct() %>%
    # break out answers by comma
    separate(answer, paste0("v",1:20), ",") %>% # <- this will throw warning, fine. pad with extra cols for up to 20 comma-separated answers just in case
    # remove cols that are all na
    dplyr::select(names(.)[sapply(., function(x) !(all(is.na(x))))]) %>%
    # gather to tidy
    gather(num, answer, v1:ncol(.)) %>%
    mutate(answer = trimws(answer)) %>%
    # make number of answers numeric
    mutate(num = parse_number(num)) %>%
    # remove any NAs in answer
    filter(!(is.na(answer)|answer %in% c("", " "))) # dat still tidy at this point
  return(df)
}



# -- PREP DATA -----
# remove any titles that were excluded (yes in q3)
excludetitles <- with(rawdat, Title[(qnum == "Q3" & grepl("yes", answer, ignore.case = T))]) 
checkexcluded <- subset(rawdat, Title %in% excludetitles) # looks good
rm(checkexcluded)

# remove excluded papers
dat <- subset(rawdat, !Title %in% excludetitles)

# for data susbetting reference, here are the Q#s and their abbreviations:
distinct(dat[c("abbr", "qnum")])

# prep map data
continents <- ne_countries(type = "map_units", returnclass = "sf")
oceans <- ne_download(type = "ocean", category = "physical")




# -- SUMMARIZE REVIEW DATA BY QUESTION -----
# 1) Plot Location Type -----
loc <- subset(dat, qnum == "Q5")
unique(loc$answer) # deal with commas
loc <- splitcom(loc, )  

# simple bar plot
## add col for terrestrial or ocean to fill location bars
loc %>%
  mutate(loctype = ifelse(grepl("Ocean$|Marine", answer), "Ocean", 
                          ifelse(answer == "Oceania", "Oceania", "Terrestrial"))) %>%
  ggplot(aes(forcats::fct_infreq(answer), fill = loctype)) +
  geom_bar() +
  labs(y = "Number of studies", x = "Study location") +
  scale_y_continuous(expand = c(0,0)) +
  # make oceania color between terrestrial and ocean
  scale_fill_manual(values = c("Ocean" = "royalblue4", "Oceania" = "lightseagreen", "Terrestrial" = "tan3"), guide =F) 
ggsave("round2_metareview/figs/q5_location_barplot.png",
       width = 3, height = 1.75, units = "in", scale = 3.2)
table(loc$answer)
table(loc$num)

# map plot <- # come back to this. ctw needs to remember how to use sf (has been a minute)
# group_by(loc, answer) 
# ggplot(data = continents) +
#   geom_sf()
#   scale_fill_viridis_c()



# 2) Plot Ecosystem Type -----
#table(krem$Q4)
ecotype <- subset(dat, qnum == "Q4")
View(ecotype)
unique(ecotype$answer[ecotype$abbr == "Ecosytem"]) #ctw will fix typo later
# for simplicity, keep only ecosytem checked (ignore notes for now)
ecotype <- subset(ecotype, abbr == "Ecosytem") %>%
  # apply splitcom
  splitcom() # end result is still tidy data

# simple bar plot
ggplot(ecotype, aes(forcats::fct_infreq(answer))) +
  geom_bar() +
  labs(x = "Ecosystem", y = "Number of studies") +
  scale_y_continuous(breaks = seq(0,400, 20), expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("round2_metareview/figs/q4_ecosystem_barplot.png",
       width = 0.8, height = 1.47, units = "in", scale = 4)
table(ecotype$answer)
table(ecotype$num) # most papers study only one type of system, but multiple systems entered for +100 papers (about 30% of papers reviewed)






# 2) Plot Study Type -----
studytype <- subset(dat, qnum == "Q6")
head(studytype) # for simplicity, keep only Methods for now (deal with other and notes later)
unique(studytype$answer[studytype$abbr == "Methods"]) #remove "(Includes...)", then un-comma
studytype <- subset(studytype, abbr == "Methods") %>%
  # remove "(Includes...)" text
  mutate(answer = gsub(" [(]Includes.*[a-z][)]", "", answer)) %>%
  # split com-sep answers
  splitcom()

# simple bar plot
ggplot(studytype, aes(forcats::fct_infreq(answer))) +
  geom_bar() +
  labs(x = "Study type", y = "Number of studies") +
  scale_y_continuous(breaks = seq(0,400, 20), expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("round2_metareview/figs/q6_studytype_barplot.png",
       width = 0.8, height = 1.47, units = "in", scale = 4)
table(studytype$answer)
table(studytype$num)


# 3) Consideration of multiple years & # of years -----
temptype <- subset(dat, qnum == "Q7")
unique(temptype$answer) # no comma splitting needed, but there are two questions to Q7 (time? y/n, and if yes, what?)
# can separate by abbr if wish
unique(temptype$abbr)
# does each title have only 1 temporal interval selected?
sapply(split(temptype$answer, temptype$abbr), function(x) summary(as.factor(x))) #hm..

# widen temptype
temptype <- dplyr::select(temptype, Title, abbr, answer) %>%
  spread(abbr, answer) %>%
  # abbreviate TimeTrends answers
  mutate(TimeTrends = gsub("^Yes.*", "Yes", TimeTrends),
         TimeTrends = gsub("^Space.*", "Space for time", TimeTrends),
         YrsData = gsub("–", "-", YrsData)) # apparently ggsave can't output an en dash?? (turns it to "...")

# 2-part bar plot
# y/n to multi time periods
ggplot(temptype, aes(forcats::fct_infreq(TimeTrends))) +
  geom_bar() +
  labs(subtitle = "Considered Multiple Time Periods?", x = NULL, y = "Number of studies") +
  scale_y_continuous(breaks = seq(0,400, 20), expand = c(0,0)) 
ggsave("round2_metareview/figs/q7_timetrends_barplot.png",
       width = 3.5, height = 1, units = "in", scale = 3)

# if yes, what temporal scales?
## > some space for time studies have yrs entered, and some don't so need to filter NAs in YrsData (but am including YrsData if entered for space for time studies)
ggplot(subset(temptype, TimeTrends != "No" & !is.na(YrsData)), aes(forcats::fct_infreq(YrsData))) +
  geom_bar() +
  labs(subtitle = "If yes, what temporal scales?", x = NULL, y = "Number of studies") +
  scale_y_continuous(expand = c(0,0)) #breaks = seq(0,400, 20), 
ggsave("round2_metareview/figs/q7_yrsdata_barplot.png",
       width = 3.5, height = 1, units = "in", scale = 3)

# also wants drivers for multiple time period studies
# extract paper titles that have time windows entered
timetitles <- unique(temptype$Title[!is.na(temptype$YrsData)])

# note: placeholder figure for now. this should be made with qc'd driver dataset (not ready yet)
# pull drivers info from q12
drivers <-  subset(dat, qnum == "Q12" & grepl("driver", abbr, ignore.case = T)) %>%
  # for now, ignore ES and "Other" entered in answer for abbr == "Driver" (should be entered in abbr "Other driver")
  filter(!(answer == "Other" | is.na(answer))) %>%
  # remove "Other" checked for abbr == "Driver"
  mutate(answer = gsub("Other,", "", answer),
         answer = gsub(",Other[,]?", "", answer),
         # also edit canned "Exploitation (hunting, fishing)" since it has a comma and will mess up counts
         answer = gsub("hunting, fishing)", "hunting or fishing)", answer),
         answer = trimws(answer)) #nailed it
  # remove whitespace in answers and remove any blanks
  
# now trim cols and un-comma respnoses
drivers_summary <- splitcom(drivers, keepcols = c("Title", "Group", "answer")) %>%
  # rename Group factors
  mutate(Group = recode(Group, "Anthro" = "Human", "Bio" = "Biotic", "Env" = "Environmental"))
# which drivers?
ggplot(subset(drivers_summary, Title %in% timetitles), aes(forcats::fct_infreq(Group))) +
  geom_bar() +
  labs(subtitle = "Which drivers?", x = NULL, y = "Number of studies") +
  scale_y_continuous(expand = c(0,0))
ggsave("round2_metareview/figs/q7_temporal_drivertype_barplot.png",
       height = 1, width = 1.25, units = "in", scale = 3)

# how many drivers?
ggplot(subset(drivers_summary, Title %in% timetitles), aes(as.factor(num), fill = Group)) +
  geom_bar(color = "grey30") +
  labs(subtitle = "How many drivers?", x = NULL, y = "Number of studies") +
  scale_y_continuous(breaks = seq(0,400, 20), expand = c(0,0)) +
  scale_fill_grey(name = "Driver", start = 0.7, end = 0.1) +
  #scale_fill_viridis_d(name = "Driver", option = "E", direction = -1) +
  theme(legend.position = c(1,1),
        legend.justification = c(1,1))
ggsave("round2_metareview/figs/q7_temporal_drivercount_barplot.png",
       height = 1, width = 1.25, units = "in", scale = 3)


# 4) Plot Feedbacks counts -----
#table(krem$Q21)
feedbacks <- subset(dat, abbr == "Feedbacks")
unique(feedbacks2$answer) # needs comma splitting
feedbacks <- splitcom(feedbacks) %>%
  # abbreviate text in answer to match LD's mock up fig
  mutate(answer = gsub("Ecosystem function / service", "EF/ES", answer),
         answer = gsub("Ecosystem function", "EF", answer, ignore.case = T),
         answer = gsub("Ecosystem services", "ES", answer),
         answer = gsub("service providers", "ESP", answer, ignore.case = T),
         answer = gsub("^No .*", "None", answer),
         answer = gsub("abi.*s$", "Abiotic", answer))
  

# simple bar plot
ggplot(feedbacks, aes(forcats::fct_infreq(answer))) +
  geom_bar() +
  labs(subtitle = "Considered feedbacks?", y = "Number of studies", x = NULL) +
  scale_y_continuous(breaks = seq(0,400, 20), expand = c(0,0))
ggsave("round2_metareview/figs/q15_feedbacks_barplot.png",
       width = 3.5, height = 1, units = "in", scale = 3)

# follow up barplot for those studies that did consider feedbacks
ggplot(subset(feedbacks, !grepl("^No", answer)), aes(forcats::fct_infreq(answer))) +
  geom_bar() +
  labs(subtitle = "If yes, which feedbacks?", y = "Number of studies", x = NULL) +
  scale_y_continuous(breaks = seq(0,50, 4), expand = c(0,0))

table(feedbacks$answer)
table(feedbacks$num)



# 5) Plot Threshold/Non-linearity counts -----
#table(krem$Q22)
tholds <- subset(dat, abbr == "Thresholds")
unique(tholds$answer) # no un-comma needed
unique(tholds$abbr) # no subquestions or optional text comments to this question

# simple bar plot
ggplot(tholds, aes(forcats::fct_infreq(answer))) +
  geom_bar() +
  labs(subtitle = "Considered thresholds?", y = "Number of studies", x = NULL) +
  scale_y_continuous(breaks = seq(0,400, 20), expand = c(0,0))
ggsave("round2_metareview/figs/q16_thresholds_barplot.png",
       width = 3.5, height = 1, units = "in", scale = 3)

table(tholds$answer)
  

# 6) Kremen Framework summary ----
kremen <- subset(dat, qnum == "Q13") %>%
  # clean up text
  mutate(answer = gsub(" that influence.*$| influencing function", "", answer),
         answer = gsub("Kremen Topic [1-4] :", "", answer),
         answer = trimws(answer)) %>%
  splitcom()

# simple bar plot
ggplot(kremen, aes(forcats::fct_infreq(answer))) +
  geom_bar() +
  labs(subtitle = "Which Kremen Topics?", y = "Number of studies", x = NULL) +
  scale_y_continuous(breaks = seq(0,400, 20), expand = c(0,0))
ggsave("round2_metareview/figs/q13_krementopics_barplot.png",
       width = 3.5, height = 1, units = "in", scale = 3)

