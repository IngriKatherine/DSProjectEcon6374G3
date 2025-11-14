###### Preamble
## Description:
# This code opens the filtered FIECS dataset TX, LA

#Clean space
rm(list = ls())

#Libraries
library(dplyr)
library(tidyverse)
library(arrow)

#Directory
getwd()
setwd("..")

######
#Open DB
g3lar <- read_parquet("proc_data/g3lar.parquet")

str(g3lar)
summary(g3lar)