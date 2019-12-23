# assess review scoring for es quasi-metaanalysis (beta version)
# ctw


# -- SETUP ----
# load needed libraries
library(tidyverse)
library(googledrive) #https://googledrive.tidyverse.org/
library(googlesheets4) #https://googlesheets4.tidyverse.org/
library(cowplot)
na_vals <- c(NA, NaN, ".", "NA")
options(stringsAsFactors = F)
theme_set(theme_bw())
# read in gsheet with reviewer scoring
# > for 2019-11-05 class TK, IS, LB, CK and CW eval'd 55 total papers: 5 commonly review (for review QC) and 10 individual papers
# > papers gathered from WOS search (see LB search methods)
# find gsheet
gsmeta <- drive_find(pattern = "test_meta_analysis", type = "spreadsheet", n_max = 30) #for me there's only 1 return (meta_analysis_bin)
# get tabs in sheets
tabnames <- sheets_sheets(gsmeta)
# initiate list for storing data
dat <- list()
# iterate through each tab and read in dats
for(i in 1:length(tabnames)){
  dat[[i]] <- read_sheet(gsmeta, tabnames[i], na = na_vals, trim_ws = T)
  names(dat)[i] <- tabnames[i]
}



# -- PREP DATA FOR ANALYSIS -----
# extract all paper review
allrev <- dat[["papers"]]
# just keep cols of interest for summarizing
allrev <- allrev[,1:grep("comment", names(allrev), ignore.case = T)]
# exclude anything that hasn't been reviewed
allrev <- allrev[apply(select(allrev, Kremen1:Reviewer_Comments), 1, function(x) any(!is.na(x))),]
# adjust TK answer for paper that's not ES focus but has structure noted in bin2 (think maybe copied down from cell above?)
allrev$Kremen2[allrev$Reviewer == "Tim" & grepl("ES not", allrev$Reviewer_Comments)] <- NA
# add col to indicate exclude if reviewer decided paper not ecological-focused
allrev$exclude <- !is.na(allrev$Reviewer_Comments) & apply(select(allrev, Kremen1:Julie_fig), 1, function(x) all(is.na(x)))
# all col to indicate doesn't apply to julie's fig when can be scored for kreme
allrev$kremenonly <- is.na(allrev$Julie_fig) & apply(select(allrev, Kremen1:Julie_fig), 1, function(x) any(!is.na(x)))
# lowcase and standardize ESP
# allrev[,grep("Kremen|Julie", names(allrev))] <- sapply(allrev[,grep("Kremen|Julie", names(allrev))], casefold)
# allrev$Kremen1 <- gsub("esps|esp", "ESP", allrev$Kremen1)
# collapse words
phrases <- select(allrev, Kremen1:Kremen4) %>%
  gather(bin, phrase) %>%
  filter(!is.na(phrase)) %>%
  # add extra cols to separate words by comma
  separate(phrase, paste0("word", rep(1:10)), sep = ",") %>%
  gather(cat, word, word1:word10) %>%
  filter(!is.na(word)) %>%
  # lowcase and remove white space
  mutate(word = trimws(casefold(word)),
         # standardize words that are the same (biodiversity/diversity, temporal/time, space/spatial)
         word = recode(word, temporal = "time", biodiversity = "diversity", spatial = "space"),
         # standardize trade-off, trade off, tradeoff
         word = ifelse(grepl("trade", word), "trade-off", word),
         # uppercase and standardize ESP
         word = gsub("esps|esp", "ESP", word)) %>%
  group_by(bin, word) %>%
  summarise(freq = length(word))

ggplot(phrases, aes(word, bin, size = freq)) +
  geom_text(aes(label = word), position = position_jitter(width = 1))
labeldf <- group_by(phrases, bin) %>%
  filter(word == max(word)) %>% ungroup()
kremenfig <- ggplot(phrases, aes(word, freq)) +
  geom_col(show.legend = F, fill ="grey50") +
  geom_text(data = labeldf, aes(word,max(phrases$freq), label = bin), hjust = 1, fontface = "bold", size = 3) +
  labs(y = NULL, x = NULL, subtitle = "Keyword binning by Kremen topic") +
  #scale_fill_grey() +
  scale_y_continuous(breaks = seq(0,50,2), expand = c(0,0.1)) +
  #ggtitle(paste0("Test review (", Sys.Date(),  "): keyword frequency by Kremen topic")) +
  coord_flip() +
  facet_grid(bin~., scales = "free_y", space = "free") +
  theme(strip.background = element_blank(),
        strip.text = element_blank(),
        axis.text.y = element_text(face = "bold", color = "black"),
        #plot.title = element_text(hjust = 0))
        plot.subtitle = element_text(face = "bold"),
        axis.text.x = element_blank())

# repeat for julie
jphrases <- select(allrev, Record, Julie_fig) %>%
  mutate(Julie_fig = trimws(casefold(Julie_fig))) %>%
  distinct() %>%
  separate(Julie_fig, paste0("word", 1:10), sep = ", ") %>%
  gather(phrase, word, word1:ncol(.)) %>%
  filter(!is.na(word)) %>%
  mutate(word = gsub("drivers", "driver", word)) %>%
  select(Record, word) %>%
  distinct() %>%
  group_by(word) %>%
  summarise(freq = length(Record)) %>% ungroup() %>%
  mutate(word = gsub("driver", "drivers", word))

jlfig <- ggplot(jphrases, aes(word, freq)) +
  geom_col(fill = "grey50") +
  scale_y_continuous(breaks = seq(0,50,2), expand = c(0,0.1)) +
  labs(x= NULL, y = "Frequency",
       subtitle = "Conceptual figure binning") +
  coord_flip() +
  theme(axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_text(face = "bold", color = "black"),
        #plot.title = element_text(hjust = 0),
        plot.subtitle = element_text(face = "bold"))


# plot both
plot_grid(kremenfig, jlfig, nrow = 2,
          rel_heights = c(1.5,1))
#ggsave("/Volumes/GoogleDrive/Mi\ unidad/EBIO\:\ Ecology\ of\ Ecosystem\ Services\ Seminar/Class\ Notes\ on\ Readings\ \&\ Discussion/Week\ 12/meta-analysis/prelim_")
# stack commonly reviewed for QC
qcrev <- data.frame()
## iterate through datlist to append person who reviewed with their scoring
for(t in tabnames[2:length(tabnames)]){
  qcrev <- rbind(qcrev, data.frame(QC = gsub("1_5", "", t), dat[[t]][names(allrev)[!names(allrev) %in% c("exclude", "kremenonly")]]))
}
# add col to indicate exclude if reviewer decided paper not ecological-focused
qcrev$exclude <- !is.na(qcrev$Reviewer_Comments) & apply(select(qcrev, Kremen1:Julie_fig), 1, function(x) all(is.na(x)))
# all col to indicate doesn't apply to julie's fig when can be scored for kreme
qcrev$kremenonly <- is.na(qcrev$Julie_fig) & apply(select(qcrev, Kremen1:Julie_fig), 1, function(x) any(!is.na(x)))


# collapse words
commonphrases <- select(qcrev,QC, Record, Kremen1:Kremen4, exclude) %>%
  gather(bin, phrase, Kremen1:ncol(.)) %>%
  filter(!is.na(phrase)) %>%
  mutate(phrase = casefold(phrase)) %>%
  # add extra cols to separate words by comma
  separate(phrase, paste0("word", rep(1:10)), sep = ",") %>%
  gather(cat, word, word1:word10) %>%
  filter(!is.na(word)) %>%
  # lowcase and remove white space
  mutate(word = trimws(casefold(word)),
         # standardize words that are the same (biodiversity/diversity, temporal/time, space/spatial)
         word = recode(word, temporal = "time", biodiversity = "diversity", spatial = "space"),
         # standardize trade-off, trade off, tradeoff
         word = ifelse(grepl("trade", word), "trade-off", word),
         # uppercase and standardize ESP
         word = gsub("esps|esp|species|ecosystem service provi[*]", "ESP", word),
         word = ifelse(grepl("maybe sp", word), "space", word)) %>%
  group_by(Record, bin, word) %>%
  summarise(freq = length(QC)) %>%
  ungroup()


common_kfig <- mutate(commonphrases, Record = paste("Paper", Record)) %>%
  ggplot(aes(word, freq)) +
  geom_col(show.legend = F, fill ="grey50") +
  #geom_text(data = commonlabeldf, aes(word,max(phrases$freq), label = bin), hjust = 1, fontface = "bold", size = 3) +
  labs(y = "Count (of 5 reviewers)", x = NULL, subtitle = "Common review: Keyword binning by Kremen topic and paper (1-5)") +
  #scale_fill_grey() +
  scale_y_continuous(breaks = seq(0,50,2), expand = c(0,0.1)) +
  #ggtitle(paste0("Test review (", Sys.Date(),  "): keyword frequency by Kremen topic")) +
  coord_flip() +
  facet_grid(bin~Record, scales = "free_y", space = "free") +
  theme(axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_text(face = "bold", color = "black"),
        #plot.title = element_text(hjust = 0))
        plot.subtitle = element_text(face = "bold"))
       

# repeat for julie
commonjphrases <- select(qcrev, QC, Record, Julie_fig) %>%
  mutate(Julie_fig = trimws(casefold(Julie_fig))) %>%
  distinct() %>%
  separate(Julie_fig, paste0("word", 1:10), sep = ", ") %>%
  gather(phrase, word, word1:ncol(.)) %>%
  filter(!is.na(word)) %>%
  mutate(word = gsub("drivers", "driver", word),
         word = gsub("dirrect", "direct", word),
         word = ifelse(grepl("environmental", word), "environmental driver", word)) %>%
  select(QC, Record, word) %>%
  distinct() %>%
  group_by(Record, word) %>%
  summarise(freq = length(QC)) %>% ungroup() %>%
  mutate(word = gsub("driver", "drivers", word))

commonj_fig <- mutate(commonjphrases, Record = paste("Paper", Record)) %>%
  ggplot(aes(word, freq)) +
  geom_col(show.legend = F, fill ="grey50") +
  #geom_text(data = commonlabeldf, aes(word,max(phrases$freq), label = bin), hjust = 1, fontface = "bold", size = 3) +
  labs(y = "Count (of 5 reviewers)", x = NULL, subtitle = "Common review: Conceptual figure binning by paper (1-5)") +
  #scale_fill_grey() +
  scale_y_continuous(breaks = seq(0,50,2), expand = c(0,0.1)) +
  #ggtitle(paste0("Test review (", Sys.Date(),  "): keyword frequency by Kremen topic")) +
  coord_flip() +
  facet_grid(.~Record, scales = "free_y", space = "free") +
  theme(axis.text.y = element_text(face = "bold", color = "black"),
        axis.title.x = element_text(face = "bold", color = "black"),
        #plot.title = element_text(hjust = 0))
        plot.subtitle = element_text(face = "bold"))

common_rev_fig <- plot_grid(common_kfig, commonj_fig, nrow = 2,
          align = "v", axis = "l")
# save to repo
ggsave("figs/common_review_prelim.pdf", common_rev_fig)
# write out to gdrive
drive_put(common_rev_fig)