###### Preamble
## Description:
# This code opens the filtered FIECS dataset TX, LA and collapse at the county level

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

# activity_year county_code loan_amount interest_rate
 
#Free unused memory and R.Data
rm(list = ls())
gc()

#End of R-script