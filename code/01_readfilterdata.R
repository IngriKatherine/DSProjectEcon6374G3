###### Preamble
## Description:
# This code opens the raw dataset downloaded from Snapshot National Loan Level Dataset
# 2021 and 2023
# It filters the files to the states TX, LA
# Exports the dataset as a parquet file to save space

#Clean space
rm(list = ls())

#Libraries
library(dplyr)
library(tidyverse)
library(arrow)

#Directory
getwd()
setwd("..")

###### Read 2021
lar2021 <- read.csv("raw_data/2021_public_lar.csv", header = TRUE)
TXLA2021 <- lar2021[lar2021$state_code == "TX" | lar2021$state_code == "LA", ]
#Remove large DB
rm(lar2021)
# Dimension
print(dim(TXLA2021))

###### Read 2023
lar2023 <- read.csv("raw_data/2023_public_lar_csv.csv", header = TRUE)
TXLA2023 <- lar2023[lar2023$state_code == "TX" | lar2023$state_code == "LA", ]
#Remove large DB
rm(lar2023)
# Dimension
print(dim(TXLA2023))

#### APPEND 2021 & 2023
TXLA <- rbind(TXLA2021, TXLA2023)
#Clean up
rm(lar2021)
rm(lar2023)

###### Export Final DB
write_parquet(TXLA, "proc_data/TXLA_2021_2023.parquet")
