###### Preamble
## Description:
# This code opens the unemployment data at the county level from BEA
# It only uses the states TX, LA 
# It filters to keep only Counties and only 2021 and 2023 
# Finally it exports the dataset as a parquet file to save space

#Clean space
rm(list = ls())

#Libraries
library(dplyr)
library(tidyr)
library(arrow)

#Directory
getwd()
setwd("..")

###### LOUISIANA  ######
## Read txt downloaded from https://download.bls.gov/pub/time.series/la/la.data.25.Louisiana
# It contains the historic information for unemployment of Louisiana at all the aggregations
LA = read.table("raw_data/la.data.25.Louisiana.txt", header = TRUE, sep = "\t")
## It has information since 1976 but I am only keeping 2021 and 2023
LA_2123 <- LA%>%
  filter(year == 2021 | year == 2023)
## It has monthly and Annual Averages, I am keeping monthly data, as Annual Averages be easily recalculated
LA_2123 <- LA_2123%>%
  filter(period != "M13")
## Create Date Variable with year and month
LA_2123$date <- as.Date(
  paste0(LA_2123$year, "-", substring(LA_2123$period, 2), "-01")
)
### I am only keeping the Counties and equivalents
#area_type_code	areatype_text
#A	Statewide
#B	Metropolitan areas
#C	Metropolitan divisions
#D	Micropolitan areas
#E	Combined areas
#CN IS THIS ONE!!!! #F	Counties and equivalents
#G	Cities and towns above 25,000 population
#H	Cities and towns below 25,000 population in New England
#I	Parts of cities that cross county boundaries
#J	Multi-entity small labor market areas
#K	Intrastate parts of interstate areas
#L	Balance of state areas
#M	Census regions
#N	Census divisions

LA_2123$agg = substr(LA_2123$series_id, start = 4, stop = 5)
table(LA_2123$agg)

LA_2123_CN = LA_2123%>%
  filter(agg == "CN")

## Get the county_code
LA_2123_CN$county_code = substr(LA_2123_CN$series_id, start = 6, stop = 10)

## Get the variable code
LA_2123_CN$agg = substr(LA_2123_CN$series_id, start = 19, stop = 20)
table(LA_2123_CN$agg)

LA_2123_CN = LA_2123_CN %>%
  mutate(
    agg = recode(agg,
                 "03" = "unemployment_rate",
                 "04" = "unemployment",
                 "05" = "employment",
                 "06" = "labor_force",
                 "07" = "employment_population_ratio",
                 "08" = "labor_force_participation_rate",
                 "09" = "civilian_noninstitutional_population"
    )
  )

#Identify duplicates
duplicates = duplicated(LA_2123_CN[, c("county_code","date","agg")])
print(which(duplicates))

## Filter to only what we need
LA_2123_CN_f=LA_2123_CN[c("county_code","date","value","agg")]

#Clean space
rm(LA,LA_2123,LA_2123_CN)

## Reshape wide to get variables in columns
LA_2123_CN_f = LA_2123_CN_f %>%
  pivot_wider(
    id_cols = c(date, county_code),
    names_from = agg,
    values_from = value
  )

###### SAVE FINAL DATASET ######
write_parquet(LA_2123_CN_f, "proc_data/BEA_unemployment_LA2123CN.parquet")

#Free unused memory and R.Data
rm(list = ls())
gc()



###### TEXAS  ######
## Read txt downloaded from https://download.bls.gov/pub/time.series/la/la.data.51.Texas
# It contains the historic information for unemployment of Texas at all the aggregations
TX = read.table("raw_data/la.data.51.Texas.txt", header = TRUE, sep = "\t")
## It has information since 1976 but I am only keeping 2021 and 2023
TX_2123 <- TX%>%
  filter(year == 2021 | year == 2023)
## It has monthly and Annual Averages, I am keeping monthly data, as Annual Averages be easily recalculated
TX_2123 <- TX_2123%>%
  filter(period != "M13")
## Create Date Variable with year and month
TX_2123$date <- as.Date(
  paste0(TX_2123$year, "-", substring(TX_2123$period, 2), "-01")
)
### I am only keeping the Counties and equivalents
#area_type_code	areatype_text
#A	Statewide
#B	Metropolitan areas
#C	Metropolitan divisions
#D	Micropolitan areas
#E	Combined areas
#CN IS THIS ONE!!!! #F	Counties and equivalents
#G	Cities and towns above 25,000 population
#H	Cities and towns below 25,000 population in New England
#I	Parts of cities that cross county boundaries
#J	Multi-entity small labor market areas
#K	Intrastate parts of interstate areas
#L	Balance of state areas
#M	Census regions
#N	Census divisions

TX_2123$agg = substr(TX_2123$series_id, start = 4, stop = 5)
table(TX_2123$agg)

TX_2123_CN = TX_2123%>%
  filter(agg == "CN")

## Get the county_code
TX_2123_CN$county_code = substr(TX_2123_CN$series_id, start = 6, stop = 10)

## Get the variable code
TX_2123_CN$agg = substr(TX_2123_CN$series_id, start = 19, stop = 20)
table(TX_2123_CN$agg)

TX_2123_CN = TX_2123_CN %>%
  mutate(
    agg = recode(agg,
                 "03" = "unemployment_rate",
                 "04" = "unemployment",
                 "05" = "employment",
                 "06" = "labor_force",
                 "07" = "employment_population_ratio",
                 "08" = "labor_force_participation_rate",
                 "09" = "civilian_noninstitutional_population"
    )
  )

#Identify duplicates
duplicates = duplicated(TX_2123_CN[, c("county_code","date","agg")])
print(which(duplicates))

## Filter to only what we need
TX_2123_CN_f=TX_2123_CN[c("county_code","date","value","agg")]

#Clean space
rm(TX,TX_2123,TX_2123_CN)

## Reshape wide to get variables in columns
TX_2123_CN_f = TX_2123_CN_f %>%
  pivot_wider(
    id_cols = c(date, county_code),
    names_from = agg,
    values_from = value
  )

###### SAVE FINAL DATASET ######
write_parquet(TX_2123_CN_f, "proc_data/BEA_unemployment_TX2123CN.parquet")

#Free unused memory and R.Data
rm(list = ls())
gc()

#End of R-script

