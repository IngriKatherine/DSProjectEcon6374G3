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
TXLA <- read_parquet("proc_data/TXLA_2021_2023.parquet")

str(TXLA)

table(TXLA$state_code, useNA = "ifany")
