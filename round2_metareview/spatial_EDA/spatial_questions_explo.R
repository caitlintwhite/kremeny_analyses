library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(igraph)
library(plotly)

df = read.csv("prelim_singlereview.csv")

# get the titles that made it through the first few exclusion questions
non_excl_titles = df %>% 
  filter(qnum =='Q3', !answer=='Yes') %>%
  select(Title)



spat = df %>% 
  filter(Title %in% non_excl_titles$Title) %>%
  filter(qnum %in% c("Q8","Q9","Q10","Q11"))

### Q8 ###
#counts of yes and no for multiple spatial scales
spat %>% 
  filter(qnum == "Q8") %>%
  group_by(fullquestion) %>%
  summarise(Yes = sum(answer=='Yes', na.rm = T),
            No = sum(answer=='No', na.rm = T),
            'NA' = sum(is.na(answer))) %>%
  melt() %>%
  ggplot(aes(x = variable, y = value)) +
  geom_col() +
  geom_label(aes(label = value)) +
  ylab("Number of papers") +
  xlab("Q8: Does the paper consider or compare multiple spatial scales?") +
  theme_bw()


### Q9 ###
#plots
spat %>% 
  filter(qnum == "Q9", abbr == "Plots", !is.na(answer), !answer==0) %>% 
  group_by(fullquestion, Group, answer) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(Group) %>%
  mutate(Group = factor(Group, levels = c('25m','50m','100m','500m','1km','10km', '100km','1000km','100Mgm','100Gm','101Gm','unk'))) %>%
  ggplot() +
  geom_col(aes(x = Group, y = count, group = answer, fill = answer)) +
  labs(x = "Size of plots", y = "Number of papers", fill = "Number of plots") +
  theme_bw()

#sites
spat %>% 
  filter(qnum == "Q9", abbr == "Sites", !is.na(answer), !answer==0) %>% 
  group_by(fullquestion, Group, answer) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(Group) %>%
  mutate(Group = factor(Group, levels = c('25m','50m','100m','500m','1km','10km', '100km','1000km','100Mgm','100Gm','101Gm','unk'))) %>%
  ggplot() +
  geom_col(aes(x = Group, y = count, group = answer, fill = answer)) +
  labs(x = "Size of sites", y = "Number of papers", fill = "Number of sites") +
  theme_bw()

#plot nums
spat %>% 
  filter(qnum == "Q9", abbr == "Plots", !is.na(answer), !answer==0) %>% 
  group_by(fullquestion, Group, answer) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(Group) %>%
  mutate(Group = factor(Group, levels = c('25m','50m','100m','500m','1km','10km', '100km','1000km','100Mgm','100Gm','101Gm','unk'))) %>%
  ggplot() +
  geom_col(aes(x = answer, y = count)) +
  labs(x = "Number of plots", y = "Number of papers") +
  theme_bw()

#site nums
spat %>% 
  filter(qnum == "Q9", abbr == "Sites", !is.na(answer), !answer==0) %>% 
  group_by(fullquestion, Group, answer) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(Group) %>%
  mutate(Group = factor(Group, levels = c('25m','50m','100m','500m','1km','10km', '100km','1000km','100Mgm','100Gm','101Gm','unk'))) %>%
  ggplot() +
  geom_col(aes(x = answer, y = count)) +
  labs(x = "Number of sites", y = "Number of papers") +
  theme_bw()





#just those that looked across scales
crossscale_titles = spat %>% filter(qnum == "Q8") %>% filter(answer=='Yes') %>% select(Title)

plot_cross = spat %>% 
  filter(Title %in% crossscale_titles$Title) %>% 
  filter(qnum=="Q9", abbr == "Plots", !is.na(answer), !answer==0) %>% 
  select(Title, answer, Group) %>%
  rename(plot_answer = answer, plot_group = Group)

site_cross = spat %>% 
  filter(Title %in% crossscale_titles$Title) %>% 
  filter(qnum=="Q9", abbr == "Sites", !is.na(answer), !answer==0) %>% 
  select(Title, answer, Group) %>%
  rename(site_answer = answer, site_group = Group)

cross_sc_sum = plot_cross %>% full_join(site_cross, by = 'Title') 



#### Network for all data points ####
# not just the cross-scale ones
#network making (https://www.jessesadler.com/post/network-analysis-with-r/)

plot_site = spat %>% 
  filter(qnum=="Q9", abbr == "Plots", !is.na(answer), !answer==0) %>% 
  select(Title, answer, Group) %>%
  rename(plot_answer = answer, plot_group = Group) %>%
  full_join(spat  %>% 
              filter(qnum=="Q9", abbr == "Sites", !is.na(answer), !answer==0) %>% 
              select(Title, answer, Group) %>%
              rename(site_answer = answer, site_group = Group),
            by = 'Title') 


#node list
plots = spat %>%
  filter(qnum=="Q9", abbr=='Plots') %>%
  distinct(Group) %>%
  rename(label = Group)

sites = spat %>% 
  filter(qnum=="Q9", abbr=='Sites') %>%
  distinct(Group) %>%
  rename(label = Group)


nodes = plots %>% 
  full_join(sites, by = 'label') 
nodes$label = as.character(nodes$label)
nodes = nodes %>% 
  rbind(list("none")) %>% 
  rowid_to_column(var = "id")

plot_site$plot_group = plot_site$plot_group %>% as.character()
plot_site$site_group = plot_site$site_group %>% as.character()

edges = plot_site %>%
  replace_na(replace = list("plot_answer" = NA, "plot_group" = "none", "site_answer" = NA, "site_group" = "none")) %>% 
  group_by(plot_group, site_group) %>%
  summarize(weight = n()) %>% 
  ungroup() %>%
  left_join(nodes, by = c('plot_group'='label')) %>%
  rename(from = id) %>%
  left_join(nodes, by = c('site_group'='label')) %>%
  rename(to = id) %>%
  select(from, to, weight) %>%
  replace_na(replace = list("from" = nodes %>% 
                              filter(label=='none') %>% 
                              select(id) %>% 
                              as.numeric(), 
                            "to" = nodes %>% 
                              filter(label=='none') %>% 
                              select(id) %>% 
                              as.numeric()))




net = graph.data.frame(d = edges, vertices = nodes, directed = F)
# this could work without doing all of the site and plot specific stuff
netm <- get.adjacency(net, attr="weight", sparse=F) 
colnames(netm) <- V(net)$label
rownames(netm) <- V(net)$label
palf <- colorRampPalette(c("gold", "red")) 

c_sums = colSums(netm)
r_sums = rowSums(netm)
netm = rbind(netm, 'Total' = c_sums)
netm = cbind(netm, 'Total' = c(r_sums, NA)) #makes Total,Total entry 0 so it doesn't overwhelm the heatmap




heatmap(t(netm[,ncol(netm):1]), Rowv = NA, Colv = NA, col = palf(100),scale="none", ylab = 'Site size', xlab = 'Plot size')



# for use with ggplot
p = ggplot(data=melt(netm)) + #[-nrow(netm),-ncol(netm)] # to exclude the totals
  geom_raster(aes(x = Var1, y = Var2, fill = value, text = value)) +
  xlab('Plot size') +
  ylab('Site size') +
  theme(axis.text.x = element_text(angle = 45))

ggplotly(p, tooltip = 'text')



# just checking that the number of unk unk (site, plot) matches up with the
# plot, it does
spat %>% 
  filter(qnum=="Q9", !is.na(answer), !answer==0) %>%
  filter(Group=='unk') %>%
  group_by(Title) %>% 
  summarize(x = sum(Group=='unk')) %>%
  select(x) %>%
  filter(x==2) %>% nrow()



# Q10: Does the paper consider connectivity between spatial replicates (sites, plots)?	
spat %>%
  filter(qnum=='Q10') %>%
  group_by(fullquestion) %>%
  summarise(Yes = sum(answer=='Yes', na.rm = T),
            No = sum(answer=='No', na.rm = T),
            'NA' = sum(is.na(answer))) %>%
  melt() %>%
  ggplot(aes(x = variable, y = value)) +
  geom_col() +
  geom_label(aes(label = value)) +
  ylab("Number of papers") +
  xlab("Q10: Does the paper consider connectivity between spatial replicates (sites, plots)?") +
  theme_bw()

connect_titles = spat %>%
  filter(qnum=='Q10', answer=='Yes') %>%
  select(Title)

spat %>% 
  filter(qnum=='Q9', Title %in% connect_titles$Title)

#plots with connectivity
spat %>% 
  filter(qnum == "Q9", abbr == "Plots", !is.na(answer), !answer==0, Title %in% connect_titles$Title) %>% 
  group_by(fullquestion, Group, answer) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(Group) %>%
  mutate(Group = factor(Group, levels = c('25m','50m','100m','500m','1km','10km', '100km','1000km','100Mgm','100Gm','101Gm','unk'))) %>%
  ggplot() +
  geom_col(aes(x = Group, y = count, group = answer, fill = answer)) +
  labs(x = "Size of plots", y = "Number of papers", fill = "Number of plots", title = 'If paper examined connectivity..') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45))

#sites with connectivity
spat %>% 
  filter(qnum == "Q9", abbr == "Sites", !is.na(answer), !answer==0, Title %in% connect_titles$Title) %>% 
  group_by(fullquestion, Group, answer) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(Group) %>%
  mutate(Group = factor(Group, levels = c('25m','50m','100m','500m','1km','10km', '100km','1000km','100Mgm','100Gm','101Gm','unk'))) %>%
  ggplot() +
  geom_col(aes(x = Group, y = count, group = answer, fill = answer)) +
  labs(x = "Size of sites", y = "Number of papers", fill = "Number of sites", title = 'If paper examined connectivity..') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45))




# Q11: What is the distance between replicates?

#have to deal with the ability to check multiple boxes
answers = spat %>%
  filter(qnum=='Q11', Title %in% connect_titles$Title) %>%
  group_by(answer) %>%
  summarise(count = n()) %>% 
  select(answer) 

# collapse and separate to get all unique possibilities
vect_ans = paste(answers$answer, collapse= ',') 
uniq_answers = unlist(strsplit(vect_ans, split=',')) %>% unique()
count_df = spat %>%
  filter(qnum=='Q11', Title %in% connect_titles$Title) %>%
  group_by(answer) %>%
  summarise(count = n())

# grep (search) for each unique answer in the count_df, return the sum of the
# counts where that value was selected
ans_df = data.frame('answer' = uniq_answers, 'count' = NA)
for (i in 1:nrow(ans_df)) {
  rows_found = grep(as.character(ans_df$answer[i]), count_df$answer)
  ans_df$count[i] = sum(count_df$count[rows_found])
}

ans_df %>%
  ggplot() +
  geom_col(aes(x = answer, y = count)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust = 1, vjust = 0)) +
  xlab('Distance between replicates') +
  ylab('Number of papers (of those that measured connectivity)')


