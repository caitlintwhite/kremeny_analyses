# space and time alone basic summary bar plots
# author: Grant

library(tidyverse)
library(reshape2)


dat = read.csv("round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv")


### first the basic summary plots for each separately ###

# spatial extent summary
dat %>% 
  filter(abbr=='Extent') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>% 
  mutate(clean_answer = factor(clean_answer, levels = c('Local', 'Macro-scale','Global','Undefined/no scale'))) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  xlab('Scale') +
  ylab('Number of papers')


# nested/multiple scales
dat %>%
  filter(abbr=='Nested') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  xlab('Nested/multiple scales?') +
  ylab('Number of papers')


# connectivity
dat %>%
  filter(abbr=='Connect') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  xlab('Connectivity?') +
  ylab('Number of papers')

dat %>%
  filter(abbr=='ConnectDist') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  filter(!is.na(clean_answer)) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  xlab('Connectivity distance') +
  ylab('Number of papers') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# I don't think the distance for connectivity is useful


# temporal trends
dat %>%
  filter(abbr=='TimeTrends') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label() +
  theme_bw() +
  xlab('Time considered?') +
  ylab('Number of papers')


dat %>%
  filter(abbr=='YrsData') %>%
  group_by(clean_answer) %>%
  summarise(count = n()) %>%
  filter(!is.na(clean_answer)) %>%
  ggplot(aes(x = clean_answer, y = count, label = count)) +
  geom_col() +
  geom_label()

 


# now getting to the time-space interactions somehow

# Is there a bias for local scale in the long-term studies?
# Is there a bias for not considering time in the macro and global scale studies?
# 


# eventually...study type considerations (simulations?)






