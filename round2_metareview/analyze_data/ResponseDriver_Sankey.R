######################################################
#### Alluvial - Sankey Figure ########################
## Written by: Aislyn Keyes 
## Last edited: June 23, 2020

# Load packages 
#library(ggalluvial)
install.packages("htmlwidgets")
library(tidyverse)
library(dplyr)
library(ggfittext)
library(networkD3)
library(htmlwidgets)

# Load data, process data -------------------------------------------------------------------
data <- read.csv("round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv")

data <- data[data$version == "final",] # final data
df <- data[,c(8,11:13,15:20)] # pull relevant columns
df <- df[df$qnum == "Q12",] # subset to only Q12



response.df <- df[df$abbr=="Response",] # pull just response rows
response.df <- response.df[,c(1,4,7,8,10)] # pull important columns
response.df <- response.df[!is.na(response.df$clean_answer_finer),] # get rid of NA response bin for now
response.df <- response.df[!duplicated(response.df),]

driver.df <- df[df$abbr=="Driver",] # pull just driver rows
driver.df <- driver.df[,c(1,4,7,10)] # pull important columns 
driver.df <- driver.df[!duplicated(driver.df),] # remove duplicate rows 

#### SMALL SANKEY: Drive type-ES type ---------------------------------------------------------
## Node date frame for sankey
nodes.driver.bins <- data.frame(name = unique(driver.df$clean_group), NodeType="driver.type")
nodes.ES <- data.frame(name = unique(response.df$ES), NodeType = "eco.serv")

nodes <- rbind(nodes.driver.bins, nodes.ES)
nodes <- nodes[!is.na(nodes$name),] # get rid of NA values

x <- nrow(nodes) - 1
nodes$ID <- 0:x

nodes$NodeType <- c("Env","Anthro","Bio", "Provisioning", "Regulating","Supporting",
                    "Supporting","Cultural","Supporting","Regulating","Regulating",
                    "Provisioning","Provisioning","Regulating","Regulating","Regulating",
                    "Cultural","Provisioning","Regulating")
nodes$FullName <- c("Environmental", "Anthropogenic", "Biotic",
                    "Food and Feed", "Regulation of pests/pathogens",
                    "Pollination", "Habitat Creation","Physical and Psychological exp.",
                    "Soil Formation/Protection", "Climate Regulation",
                    "Coastal Water Quality", "Materials","Medicinal",
                    "Fresh Water Quality", "Hazard Regulation",
                    "Air Quality", "Maintenance of Options",
                    "Energy", "Regulation of Ocean Acidification")

#attach(nodes)
#nodes <- nodes[order(NodeType),]
#detach(nodes)

## Link data frame for sankey
# link VALUES
values <- driver.df[,c(2,3,4)]
values$num.papers <- 1 # add column to sum
values <- aggregate(formula = num.papers ~ ES + clean_group , data = values, FUN = sum)
col <- c("TargetName","SourceName","value")
colnames(values) <- col


# create data frame to merge later
links <- data.frame(SourceName = rep(c("Bio","Anthro","Env"), each=nrow(nodes[nodes$NodeType=="eco.serv",])),
                    Source = rep(c(2,1,0), each=nrow(nodes[nodes$NodeType=="eco.serv",])),
                    TargetName = rep(nodes.ES$name, times = 3),
                    Target = rep(nodes$ID[4:nrow(nodes)], times=3)
) 

s.links <- merge(x=links, y=values, by = c("SourceName","TargetName"), all.x = T)
s.links <- s.links[!is.na(s.links$value),] # get rid of NA values
s.links <- s.links[,-5]


names(s.links)[names(s.links)=="x"] <- "value"


colors <-  'd3.scaleOrdinal() .domain(["Anthro", "Env","Bio", "Supporting","Cultural","Regulating","Provisioning"]) .range(["#69b3a2", "steelblue" , "purple", "red", "orange", "yellow", "FF3399"])'

# BASE SANKEY CODE
sn <- sankeyNetwork(Links = s.links, Nodes = nodes, Source = "Source",
              Target = "Target", Value = "value", NodeID = "FullName",
              units = "papers", fontSize = 18, nodeWidth = 40,
              fontFamily = "Arial",
              LinkGroup = "SourceName",
              NodeGroup = "NodeType",
              colourScale = colors,
              margin = list(left=300, right=50))


right <- c("Food and Feed", "Regulation of pests/pathogens",
           "Habitat Creation","Pollination","Physical and Psychological exp.",
           "Soil Formation/Protection","Climate Regulation","Fresh Water Quality",
           "Materials", "Medicinal","Coastal Water Quality", "Hazard Regulation",
           "Maintenance of Options", "Air Quality","Energy","Regulation of Ocean Acidification")

onRender(
  sn,
  paste0('
        function(el,x){
        d3.select(el)
        .selectAll(".node text")
        .filter(function(d) { return (["',paste0(right,collapse = '","'),'"].indexOf(d.name) > -1);})
        .attr("x", 6 + x.options.nodeWidth)
        .attr("text-anchor", "begin");
        }
        ')
)







#### LARGE SANKEY: Driver type-response bin-ES type ----------------------------------------------------
## Node data frame for sankey
nodes.response <- data.frame(name = unique(response.df$clean_answer_finer),NodeType="response")
nodes.driver.bins <- data.frame(name = unique(driver.df$clean_group), NodeType="driver.type")
nodes.ES <- data.frame(name = unique(response.df$ES), NodeType = "eco.serv")

nodes <- rbind(nodes.response, nodes.driver.bins, nodes.ES)
x <- nrow(nodes) - 1
nodes$ID <- 0:x


## Link data frame for sankey


# make two link data frames: 1 for driver type to response bin and 2 for response bin to ES type

# left side link VALUES
val.df2 <- response.df[,c(2,3)]
val.df2$num.papers <- 1 # add column to sum
val.df2 <- aggregate(formula = num.papers ~ clean_answer_finer + clean_group, data = val.df2, FUN = sum)

# left side links
link.drivers <- data.frame(SourceName = rep(c("Bio","Anthro","Env",NA), 
                                         each=nrow(nodes[nodes$NodeType=="response",])),
                            Source = rep(c(42,41,40,39), each=nrow(nodes[nodes$NodeType=="response",])),
                            TargetName = rep(nodes.response$name, times = 4),
                            Target = rep(nodes$ID[1:nrow(nodes.response)], times=4),
                            value = 5
                            ) 

# right side link VALUES
val.df <- response.df[,c(2,5)]
val.df$num.papers <- 1 # add a column to sum
val.df <- aggregate(formula = num.papers ~ clean_answer_finer + ES, data = val.df, FUN = sum)
col <- c("SourceName","TargetName","value")
colnames(val.df) <- col

# right side links
link.response <- data.frame(SourceName = rep(nodes.response$name, 
                                             each = nrow(nodes[nodes$NodeType=="eco.serv",])),
                            Source = rep(nodes$ID[1:nrow(nodes.response)], each = nrow(nodes[nodes$NodeType=="eco.serv",])),
                            TargetName = rep(nodes.ES$name, times = nrow(nodes[nodes$NodeType=="response",])),
                            Target = rep(nodes$ID[nodes$NodeType=="eco.serv"], times = nrow(nodes[nodes$NodeType=="response",])),
                            )

right.links <- merge(x=link.response, y=val.df, by = c("SourceName","TargetName"), all.x = T)
right.links <- right.links[!is.na(right.links$value),] # get rid of NA values

#right.links[["value"]][is.na(right.links[["value"]])] <- 0


links <- rbind(link.drivers,right.links)



# BASE SANKEY CODE
sankeyNetwork(Links = links, Nodes = nodes, Source = "Source",
                   Target = "Target", Value = "value", NodeID = "name",
                   units = "papers", fontSize = 12, nodeWidth = 30)







