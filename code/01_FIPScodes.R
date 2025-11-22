###### Preamble
## Description:
# This code creates a dictionary of the FIPS to use for input to download the 
# FRED data

#Clean space
rm(list = ls())

#Libraries
library(dplyr)
library(tidyr)
library(arrow)

#Directory
getwd()
setwd("..")

###### BRING COUNTY CODES LA AND TX  ######
fips = read.table("raw_data/fips.txt", header = TRUE, sep = "\t", skip = 14, col.names=c("raw"))
#Clean FIPS
fips_c=fips %>%
  mutate(raw = trimws(raw, which = "left")) %>%
  separate(raw, into = c("code", "state"),
           sep = "\\s+", extra = "merge", fill = "right")
fips_c <- fips_c %>%
  mutate(code_2 = suppressWarnings(as.numeric(code))) %>%
  filter(!is.na(code_2))

fips_f=fips_c[c("code", "state")]

fips_f <- fips_f %>%
  rename(state_county = state)

#Clean space
rm(fips,fips_c)

#Create State Code
fips_f$state = substr(fips_f$code, start = 1, stop = 2)

##SAVE FIPS DICTIONARY
write_parquet(fips_f, "proc_data/FIPS_dictionary.parquet")

#Free unused memory and R.Data
rm(list = ls())
gc()

#End of R-script