######################################################
#### Alluvial - Sankey Figure ########################
## Written by: Aislyn Keyes 
## Last edited: June 23, 2020

# Load packages 
#library(ggalluvial)
#install.packages("htmlwidgets")
library(tidyverse)
library(dplyr)
library(ggfittext)
library(networkD3)
library(htmlwidgets)

# Load data, process data -------------------------------------------------------------------
data <- read.csv("round2_metareview/data/cleaned/ESqualtrics_r2keep_cleaned.csv")
new.bins <- read.csv("round2_metareview/analyze_data/final_analyses/lulc_reclass_bytitle.csv")
es.bins <- read.csv("round2_metareview/analyze_data/final_analyses/lulcreclass_driv_ES_bytitle.csv")


data <- data[data$version == "final",] # final data
df <- data[,c(8,15:19)] # pull relevant columns
df <- df[df$qnum == "Q12",] # subset to only Q12
df <- df[,-5]

response.df <- df[df$abbr=="Response",] # pull just response rows
#response.df <- response.df[!is.na(response.df$clean_answer_finer),] # get rid of NA response bin for now
response.df <- response.df[!duplicated(response.df),]


driver.df <- es.bins
driver.df <- driver.df[!is.na(driver.df$driv_type),] # get rid of NA values
driver.df[driver.df == "LU_LC"] <- "Land Use Land Change"


#### SMALL SANKEY: Drive type-ES type ---------------------------------------------------------
## Node date frame for sankey
nodes.driver.bins <- data.frame(name = unique(driver.df$driv_type), NodeType="driver.type")
nodes.ES <- data.frame(name = unique(response.df$ES), NodeType = "eco.serv")

nodes <- rbind(nodes.driver.bins, nodes.ES)
nodes <- nodes[!is.na(nodes$name),] # get rid of NA values

attach(nodes)
nodes <- nodes[order(NodeType,name),]
detach(nodes)

nodes <- nodes[c(3,1,2,4,5:20),]

nodes$NodeType <- c("Human","Biotic","Environmental","Land Use Land Change", "Regulating", "Regulating","Regulating",
                    "Cultural","Provisioning","Provisioning","Regulating","Supporting",
                    "Regulating","Cultural","Provisioning","Provisioning","Regulating",
                    "Regulating","Supporting","Supporting")


nodes$Type <- c(rep("Driver", times = 4), rep("eco.serv", times=16))

attach(nodes)
nodes <- nodes[order(Type, NodeType),]
detach(nodes)

nodes$FullName <- c("Biotic", "Environmental", "Human","Land Use Land Change",
                    "Physical and Psychological exp.","Maintenance of Options",
                    "Energy","Food and Feed","Materials", "Medical",
                    "Air Quality","Climate Regulation","Coastal Water Quality",
                    "Fresh Water Quality","Hazard Regulation","Regulation of Ocean Acidification",
                    "Regulation of pests/pathogens","Habitat Creation","Pollination",
                    "Soil Formation/Protection")


nodes <- nodes[c(3,4,2,1,12,17,13,14,15,11,16,20,18,19,8,9,7,10,5,6),] # reorder based on number of papers...this is just manual at this point

x <- nrow(nodes) - 1
nodes$ID <- 0:x

colnames(nodes)[colnames(nodes)=="name"] <- "TargetName"

nodes[1,c(1,2)] <- "Human"
nodes[4,c(1,2)] <- "Biotic"
nodes[3,c(1,2)] <- "Environmental"
nodes[2,c(1,2)] <- "Land Use Land Change"

nodes$FullName[1:4] <- ""

## Link data frame for sankey
# link VALUES
values <- driver.df[,c(2,3)]
values$num.papers <- 1 # add column to sum
values <- aggregate(formula = num.papers ~ ES + driv_type , data = values, FUN = sum)
col <- c("TargetName","SourceName","value")
colnames(values) <- col


# create data frame to merge later
links <- data.frame(SourceName = rep(c("Human","Land Use Land Change","Environmental", "Biotic"), each=nrow(nodes[nodes$Type=="eco.serv",])),
                    Source = rep(c(0,1,2,3), each=nrow(nodes[nodes$Type=="eco.serv",])),
                    TargetName = rep(nodes$TargetName[c(5:20)], times = 4),
                    Target = rep(nodes$ID[5:nrow(nodes)], times=4)
) 

s.links <- merge(x=links, y=values, by = c("SourceName","TargetName"), all.x = T)
s.links <- s.links[!is.na(s.links$value),] # get rid of NA values



names(s.links)[names(s.links)=="x"] <- "value"

s.links <- merge(s.links,nodes,by="TargetName", all.x=T)

attach(s.links)
s.links <- s.links[order(NodeType, value),]
detach(s.links)

# Add group labels for coloring in sankey plot
s.links$LinkType <- s.links$SourceName
nodes$Group <- c("A","B","C","D",rep(c("E","F","G","H"), times=c(7,3,4,2)))
s.links["LinkType"][s.links["LinkType"] == "Human"] <- "A"
s.links["LinkType"][s.links["LinkType"] == "Land Use Land Change"] <- "B"
s.links["LinkType"][s.links["LinkType"] == "Environmental"] <- "C"
s.links["LinkType"][s.links["LinkType"] == "Biotic"] <- "D"


my_colors <-  'd3.scaleOrdinal() .domain(["A","B","C","D","E","F","G","H"]) 
.range(["#a6cee3", "#00008B","#b2df8a", "#33a02c", "#000000","#808080","#D3D3D3","#FFFFFF"])'



# BASE SANKEY CODE


# build sankey
sn <- sankeyNetwork(Links = s.links, Nodes = nodes, Source = "Source",
              Target = "Target", Value = "value", NodeID = "FullName",
              units = "papers", fontSize = 17, nodeWidth = 30,
              fontFamily = "Arial",
              LinkGroup = "LinkType",
              NodeGroup = "Group",
              colourScale = my_colors,
              margin = list(left=300, right=50),
              iterations=0)

# ecosystem service labels
right <- c("Climate Regulation","Regulation of pests/pathogens","Coastal Water Quality",
           "Fresh Water Quality","Hazard Regulation","Air Quality","Regulation of Ocean Acidification",
           "Soil Formation/Protection","Habitat Creation","Pollination","Food and Feed",
           "Materials","Energy","Medical","Physical and Psychological exp.","Maintenance of Options")

# Plot sankey  
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
        '))

























