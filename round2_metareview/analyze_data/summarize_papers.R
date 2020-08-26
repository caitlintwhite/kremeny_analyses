# short script to read in and lookup summary stats on abstracts/papers reviewed for ES
# author(s): CTW
# > feel free to add on as needed (e.g. more summary stats, figs, whatever needed)
library(dplyr)
options(stringsAsFactors = F)
na_vals <- c("NA", "NaN", ".", "", " ", NA, NaN)


# initial starting pool of ES abstracts
round1 <- read.csv("round1_exclusion/EcosystemServicesPapersNov2019.csv", na.strings = na_vals)
# note 1 abstract title appears twice
summary(duplicated(round1$Title)); length(unique(round1$Title)) # actually 1932 abstracts, not 1933
# was the duplicated title assigned to same person?
with(round1, EBIOReviewer[Title == Title[duplicated(Title)]]) # yes (Aislyn)
# what is the paper?
subset(round1, Title %in% Title[duplicated(Title)])
# > difference = one record has pubdate of Dec 2018, the other Apr 2019; otherwise title, authors, abstract, etc. identical
# how many assigned per person?
with(subset(round1, !duplicated(Title)), sapply(split(Title, EBIOReviewer), length)) # Aislyn reviewed 148 unique titles


# abstracts excluded in round 1
r1exclude <- read.csv("round1_exclusion/output/exclude_round1.csv", na.strings = na_vals)
# percent excluded?
length(unique(r1exclude$Title))/length(unique(round1$Title))

# abstracts kept after round1
r1kept <- read.csv("round1_exclusion/output/keep_round1.csv", na.strings = na_vals)
# percent kept
length(unique(r1kept$Title))/nrow(round1)
# of abstracts kept, papers assigned for round 2 (half of round 1 kept with the intent for each paper to have 2 reviewers)
r2assigned <- read.csv("round1_exclusion/output/review_assignments_round2_grpdsubset.csv", na.strings = na_vals)

# papers excluded in round 2
# note: this has multiple rows per paper for double reviewed papers (use version = "final" if only want 1 record)
r2exclude <- read.csv("round2_metareview/data/cleaned/ESqualtrics_r2exclude_cleaned.csv", na.strings = na_vals) 
# percent excluded
length(unique(r2exclude$Title))/length(unique(r2assigned$Title))


# compiled round1 and round 2 exclusion dataset to summarize reasons for exclusion in each round
# > note: this dataset only allows 1 reason for exclusion (exclusion datasets above give responses to all exclusion questions that reviewer answers)
allexclude <- read.csv("round2_metareview/data/cleaned/ESreview_allexcluded_allrounds.csv")
# how many papers excluded per round?
sapply(split(allexclude$title, allexclude$review_round), length)
# reason excluded?
group_by(allexclude, review_round, exclusion_reason) %>%
  summarise(nobs = length(title),
            grand_pct = (nobs/nrow(.))*100) %>%
  ungroup() %>%
  mutate(round_pct = ifelse(review_round == 1, (nobs/nrow(r1exclude))*100, (nobs/length(unique(r2exclude$Title)))*100)) %>%
  arrange(review_round, -nobs)

# lump reasons per round 2 and re-summarize
mutate(allexclude, grp_reason = ifelse(grepl("revi|meta|tool", exclusion_reason, ignore.case = T), "ReviewOnly",
                                       ifelse(grepl("^no", exclusion_reason, ignore.case = T), "no_efes",
                                              ifelse(grepl("Biod", exclusion_reason, ignore.case = T), "biodiv", "SocialOnly")))) %>%
  group_by(grp_reason) %>%
  summarise(nobs = length(title),
            grand_pct = (nobs/nrow(.))*100) %>%
  arrange(-nobs)


qdat <- read.csv("round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv", na.strings = na_vals) 
# stack and write out all varnames for all datasets for metadata
varnames <- rbind(data.frame(source = "allexcluded", varnames = names(allexclude), type = sapply(allexclude, class)),
                  data.frame(source = "r1exclude", varnames = names(r1exclude), type = sapply(r1exclude, class))) %>%
  rbind(data.frame(source = "r2exclude", varnames = names(r2exclude),  type = sapply(r2exclude, class))) %>%
  rbind(data.frame(source = "r1keep", varnames = names(r1kept),  type = sapply(r1kept, class))) %>%
  rbind(data.frame(source = "r2keep", varnames = names(qdat),  type = sapply(qdat, class))) %>%
  rbind(data.frame(source = "r2_assigned", varnames = names(r2assigned),  type = sapply(r2assigned, class))) %>%
  rbind(data.frame(source = "r1_assigned", varnames = names(round1),  type = sapply(round1, class)))
write.csv(varnames, "round2_metareview/data/intermediate/varnames4metadata.csv", row.names = F)


