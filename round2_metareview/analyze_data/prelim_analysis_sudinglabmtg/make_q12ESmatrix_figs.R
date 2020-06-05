# make q12 ES response/driver matrix figures
# author(s): ctw
# initiated: apr 2020


# script purpose:
# make following figs:
## 1) word cloud response (or driver) variables
## 2) alluvial diagram ES driver to ES response to ES


# notes
# R package options for wordcloud:
# 1) base R-based: wordcloud, wordcloud2
# https://cran.r-project.org/web/packages/wordcloud/wordcloud.pdf
# tutorial: https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a

# 2) ggplot-based: ggwordcloud, ggwordcloud2
# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html
## > if use gg version, borrow ctw climate QA code to panel ggplots in same graphing window
## > ctw thinks ggwordcloud = more annoying (e.g. need to create col to angle words, but ctw also doesn't like base R plot functionality)

# Rpackage options for alluvial diagram:
# 1) base R-based: network3D, flipPlots, alluvial (see link in #2 below)
# network3D: https://www.displayr.com/sankey-diagrams-r/
# flipPlots: https://www.r-bloggers.com/how-to-create-sankey-diagrams-from-tables-data-frames-using-r/

# 2) ggplot-based: ggalluvial
# ak has used this and recommends
# package vignette: https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
# https://www.r-bloggers.com/data-flow-visuals-alluvial-vs-ggalluvial-in-r-2/

# other:




# -- SETUP -----
rm(list=ls()) # clean up environment
library(tidyverse)
library(wordcloud)
#library(ggwordcloud)
library(ggalluvial)
options(stringsAsFactors = F)
na_vals <- c("NA", "NaN", ".", "", " ", NA, NaN)
theme_set(theme_classic())


# read in latest metareview data
rawdat <- read.csv("round2_metareview/data/intermediate/round2_prelim_singlereview.csv", na.strings = na_vals)
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
# pull out q12 only
q12dat <- subset(dat, qnum == "Q12")



# -- FIGURES -----
# 1) WORD CLOUDS ----
# -- driver clouds ----
# pull drivers info from q12
drivers <-  subset(q12dat, grepl("driver", abbr, ignore.case = T)) %>%
  # for now, ignore ES and "Other" entered in answer for abbr == "Driver" (should be entered in abbr "Other driver")
  filter(!(answer == "Other" | is.na(answer))) %>%
  # remove "Other" checked for abbr == "Driver"
  mutate(answer = gsub("Other,", "", answer),
         answer = gsub(",Other[,]?", "", answer),
         # also edit canned "Exploitation (hunting, fishing)" since it has a comma and will mess up counts
         answer = gsub("hunting, fishing)", "hunting or fishing)", answer),
         answer = trimws(casefold(answer))) #nailed it
# remove whitespace in answers and remove any blanks

# now trim cols and un-comma respnoses
drivers_summary <- splitcom(drivers, keepcols = c("Title", "ES", "Group", "answer")) %>%
  # rename Group factors
  #mutate(Group = recode(Group, "Anthro" = "Human", "Bio" = "Biotic", "Env" = "Environmental")) %>%
  arrange(answer)# ES for "OtherDriver" not supplied..
 
otherdrivers <- subset(q12dat, grepl("Other", answer)) %>%
  dplyr::select(Title, ES, Group, answer) %>% distinct() %>%
  rename_at(vars(ES, answer), function(x)paste0("other",x)) %>%
  left_join(subset(drivers_summary, Title %in% unique(otherdrivers$Title) & is.na(ES))) %>%
  # get rid of things that shouldn't have been split
  filter(!answer %in% (c("city 2])", "not esp)"))) %>%
  # filter any ES that didn't join for simplicity sake
  filter(!is.na(otherES)) # <--this doesn't work as expected, leaves some vars out (e.g. see Conservation tillage mitigates the negative effect of landscape simplification on biological control)
# i think that might happen when only one ES? may need to fill down.. revisit


# lump unique drivers by group, ignore ES for now..
drivers_grouped <- drivers_summary %>%
  mutate(Group = recode(Group, "Anthro" = "Human", "Bio" = "Biotic", "Env" = "Environmental")) %>%
  group_by(Group, answer) %>%
  summarise(count = length(Title)) %>%
  ungroup() %>%
  # clean up ESP text in answer
  mutate(answer = gsub(" [(]q17[)]", "", answer))


# make single word cloud
sort(sapply(split(response_summary3$yresponse, response_summary3$ES), function(x) length(unique(x))))
ES <- "SoilProtect"
response_summary_simple <- group_by(response_summary3, ES, yresponse) %>% summarise(count = sum(count)) %>% ungroup() %>%
  filter(yresponse != "")

# environmental drivers
png("round2_metareview/analyze_data/prelim_analysis_sudinglabmtg/prelim_figs/envdrivers_wordcloud.png",width = 5, height = 5, units = "in", res = 300)
wordcloud(words = drivers_grouped$answer[drivers_grouped$Group == "Environmental"], freq = drivers_grouped$count[drivers_grouped$Group == "Environmental"], 
          min.freq = 1, max.words=200, random.order=FALSE,
          colors=brewer.pal(8, "Dark2")) # scale = c(2, 0.4),
title(sub = "Environmental Drivers")
dev.off()

# human drivers
png("round2_metareview/analyze_data/prelim_analysis_sudinglabmtg/prelim_figs/humandrivers_wordcloud.png",width = 5, height = 5, units = "in", res = 300)
wordcloud(words = drivers_grouped$answer[drivers_grouped$Group == "Human"], freq = drivers_grouped$count[drivers_grouped$Group == "Human"], 
          min.freq = 1, max.words=200, random.order=FALSE,
          colors=brewer.pal(8, "Dark2")) # scale = c(2, 0.4),
title(sub = "Human Drivers")
dev.off()

# biotic drivers
png("round2_metareview/analyze_data/prelim_analysis_sudinglabmtg/prelim_figs/bioticdrivers_wordcloud.png",width = 5, height = 5, units = "in", res = 300)
wordcloud(words = drivers_grouped$answer[drivers_grouped$Group == "Biotic"], freq = drivers_grouped$count[drivers_grouped$Group == "Biotic"], 
          min.freq = 1, max.words=200, scale = c(3, 0.5), random.order=FALSE,
          colors=brewer.pal(8, "Dark2")) # scale = c(2, 0.4),
title(sub = "Biotic Drivers")
dev.off()


# -- response clouds -----
# just make for one ES as example (e.g. soil formation since has the most)

# copied ugly code -- improve later. just for plotting for apr 8 meeting
responses <- subset(q12dat, qnum == "Q12" & abbr =="Response")
yclass <- subset(q12dat, qnum == "Q12" & abbr == "Yclass") %>%
  dplyr::select(StartDate, Title, Init, ES, answer) %>%
  separate(answer, c("type1", "type2", "type3", "type4"), sep = ",") %>%
  # remove anything that's all NA
  dplyr::select(names(.)[sapply(., function(x) !all(is.na(x)))])
# join EFs and Es
ESresponses <- left_join(responses, yclass)

response_summary <- responses %>%
  mutate(answer =  casefold(answer)) %>%
  group_by(ES, answer) %>% #ES, 
  # get freq of each answer
  summarise(count = length(qnum))

response_summary2 <- ESresponses %>%
  mutate(answer =  casefold(answer)) %>%
  gather(yclass_num, yclass, type1, type2) %>%
  mutate(yclass_num = parse_number(yclass_num)) %>%
  filter(!(yclass_num == 2 & is.na(yclass))) 

response_summary3 <- response_summary2 %>%
  group_by(ES, yclass, answer) %>% #ES, 
  # get freq of each answer
  summarise(count = length(qnum)) %>%
  ungroup() %>%
  rename(yresponse = answer) %>%
  separate(yresponse, paste0("answer", 1:50), ",") %>%
  # remove anything that's all NA
  dplyr::select(names(.)[sapply(., function(x) !all(is.na(x)))]) %>%
  dplyr::select(-count) %>%
  gather(response_num, yresponse, answer1:ncol(.)) %>%
  filter(!is.na(yresponse)) %>%
  mutate(yresponse = trimws(yresponse)) %>%
  group_by(ES, yclass, yresponse) %>%
  summarise(count = length(response_num)) %>%
  ungroup() %>%
  arrange(yresponse, yclass)

response_summary4 <- subset(response_summary, !is.na(answer)) %>%
  splitcom(keepcols = c("ES", "answer")) %>%
  group_by(ES, answer) %>%
  summarise(count = length(num)) %>%
  ungroup()

# soil protection responses
png("round2_metareview/analyze_data/prelim_analysis_sudinglabmtg/prelim_figs/soilESresponses_wordcloud.png",width = 5, height = 5, units = "in", res = 300)
wordcloud(words = response_summary4$answer[response_summary4$ES == "SoilProtect"], freq = response_summary4$count[response_summary4$ES == "SoilProtect"], 
          min.freq = 1, max.words=200, scale = c(2, 0.4),random.order=FALSE, rot.per = 0.35,
          colors=brewer.pal(8, "Dark2")) # 
title(sub = "Soil Protection ES/EF")
dev.off()





# 2) ES DRIVERS FIG -------



# 3) ALLUVIAL DIAGRAM -----
testalluvial <- subset(q12dat, !is.na(answer) & abbr %in% c("Response", "Yclass", "Driver", "OtherDriver")) %>%
  # for now, just to test how ggalluvial works, fill down ES
  group_by(Title) %>%
  fill(ES) %>%
  ungroup() %>%
  # clean up "Other" entries
  filter(!(answer == "Other" & abbr == "Driver")) %>%
  mutate(answer = gsub("Other,", "", answer),
         answer = gsub(",Other[,]?", "", answer),
         # also edit canned "Exploitation (hunting, fishing)" since it has a comma and will mess up counts
         answer = gsub("hunting, fishing)", "hunting or fishing)", answer),
         answer = trimws(casefold(answer))) %>%
  #simplify
  dplyr::select(Title, abbr, Group, ES, answer) %>%
  distinct()

alluvialresponses <- subset(testalluvial, abbr == "Response") %>%
  splitcom(keepcols = c("Title", "ES", "answer")) %>%
  # sum # driver variables per ES per Group per title
  group_by(Title, ES) %>%
  summarise(count_yvars = length(answer))
alluvialEFES <- subset(testalluvial, abbr == "Yclass") %>%
  splitcom(keepcols = c("Title", "ES", "answer")) %>%
  rename("ytype"="answer")
  

# need to bring in # drivers as well
alluvialdrivers <- subset(testalluvial, grepl("Driver", abbr, ignore.case = T)) %>%
  splitcom(keepcols = c("Title", "ES", "Group", "answer")) %>%
  # sum # driver variables per ES per Group per title
  group_by(Title, ES, Group) %>%
  summarise(count_drivers = length(answer)) %>%
  ungroup() 
  # group_by(ES, Group) %>%
  # summarise(count_drivers = sum(count_drivers)) %>%
  # ungroup()
  spread(Group, count_drivers)

test <- left_join(alluvialresponses, subset(alluvialEFES, num == 1)) %>%
  #drop any paper that had more than 1 ES type selected
  filter(!is.na(ytype)) %>%
  dplyr::select(-num) %>%
  left_join(alluvialdrivers) %>% #! some drivers not entered :(
  filter(!is.na(count_drivers))

test2 <- alluvialdrivers %>%
  group_by(ES, Group, count_drivers) %>%
  summarise(count_papers = length(Title))

ggplot(test2, aes(y = count_papers, axis1 = Group, axis2 = ES)) +
    geom_alluvium(aes(fill = count_drivers), width = 1/12) +
    geom_stratum(width = 1/12, fill = "black", color = "grey") +
    geom_label(stat = "stratum", infer.label = TRUE) +
    scale_x_discrete(limits = c("Group", "ES"), expand = c(.05, .05)) +
    scale_fill_brewer(type = "qual", palette = "Set1")
    ggtitle("UC Berkeley admissions and rejections, by sex and department")
  
ggplot(alluvialdrivers,
       aes(x = met, stratum = val, alluvium = countpapers,
           y = count_drivers, label = val)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_alluvium(aes(fill = count_drivers)) +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none")
  #ggtitle("vaccination survey responses at three points in time")

