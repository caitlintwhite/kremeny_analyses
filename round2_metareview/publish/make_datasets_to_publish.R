# internal use R script to prep data and EML metadata for publication
# author(s): CTW
# questions?: caitlin.t.white@colorado.edu
# date init: 2022-05-15

# script purpose:
# we should publish 2 datasets:
# 1. all abstracts and full text excluded with reasons and citation info
# 2. full text studies retained, with qualtrics data used for analyses, and citation info
# > #2 is our "map database" (anyone interested in particular aspects our our survey could filter studies based on coding responses)


# once datasets prepped with citation info and unecessary columns removed (e.g., anything internal use not critical to publish), write metadata with EML package
# write out all to main level of "publish" subfolder


#### SETUP -----
rm(list = ls()) # start w clean environment (ctw does not care this is not best practice code)
# load needed libraries
library(tidyverse)
library(EML)
options(stringsAsFactors = F)
na_vals = c(NA, "NA", " ", "", ".")

# read in relevant datasets
datpath <- "round2_metareview/data/cleaned/"





#### MAKE EXCLUDED PAPERS DATA OUT----




#### MAKE FULL TEXT DATA OUT ----



#### MAKE EML METADATA -----