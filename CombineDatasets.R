library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

###############################################################
## Secchi Depth
###############################################################

## Read in both datasets and take a look at them
Jordan_Secchi_STORET <- read_xlsx('Jordan Secchi STORET.xlsx')
glimpse(Jordan_Secchi_STORET)

Jordan_Secchi_NWIS <- read_xlsx('Jordan Secchi NWIS.xlsx')
glimpse(Jordan_Secchi_NWIS)

# double check all the column names are the same
names(Jordan_Secchi_NWIS) == names(Jordan_Secchi_STORET)

##combine datasets
Jordan_Secchi_Combined <- rbind(Jordan_Secchi_NWIS, Jordan_Secchi_STORET)

##order by collection method, location, date
Jordan_Secchi_Combined <- Jordan_Secchi_Combined %>%
  arrange(`SampleCollectionMethod/MethodName`, MonitoringLocationIdentifier, ActivityStartDate, `ActivityStartTime/Time`)

##Add latitude and longitude for each locationID
Jordan_LatLong <- read_csv('Jordan In Situ LatLong.csv')
glimpse(Jordan_LatLong)


##Match the MonitoringLocationIdentifier names from the LatLong dataset and the combined dataset.
LatLong_JL_Secchi <- merge(Jordan_Secchi_Combined, Jordan_LatLong, by='MonitoringLocationIdentifier')
View(LatLong_JL_Secchi)


##Add reflectance values
Jordan_REFL <- read_csv('JordanLakeREFL2.csv')
glimpse(Jordan_REFL)
colnames(Jordan_REFL)

## Separate the date from the system index column

##I want to attach the time column to the dataset, but the times were read in with an incorrect
##date in front of them. I want to eliminate that date and just keep the time. I figure the code
##will be similar to the next section of code.


## Here, str_split breaks the input value on the set pattern '_0000", then [[1]][1]
# takes the first result of that.  The map functions just tell it go row by row and
# repeat the same process.
Jordan_REFL <- Jordan_REFL %>%
  mutate(LandsatID = map_chr(`system:index`, ~str_split(.,'_0000')[[1]][1]),
       date =  map_chr(LandsatID, ~tail(strsplit(., "_")[[1]], n =1)),
       date = ymd(date)) %>%
  select(-`system:index`)

## The Monitoring Location Identifier column is very weird, we'll rename it with a 
## workaround
colnames(Jordan_REFL)[16] <-  'locationID'


## Ok I moved this up a little to work with a cleaners data set.  I also don't have
## the lat long csv you use so you'll want to swap that back in here and add the lat/long
## columns back

## Get rid of variables you don't want
## In the code below the format is select(X = Y), X is your new name, Y is the old Name

jsFiltered <- LatLong_JL_Secchi %>%
  select(date = ActivityStartDate, 
         time = `ActivityStartTime/Time`,
         locationID = MonitoringLocationIdentifier,
         latitude = LatitudeMeasure,
         longitude = LongitudeMeasure,
         method = `SampleCollectionMethod/MethodName`,
         equipment = SampleCollectionEquipmentName,
         depth = `ActivityDepthHeightMeasure/MeasureValue`,
         depthunits = `ActivityDepthHeightMeasure/MeasureUnitCode`,
         secchidepth = ResultMeasureValue,
         sddunits = `ResultMeasure/MeasureUnitCode`,
         source = ProviderName)

##Change all values measured in inches to meters.
jsFiltered$secchidepth[jsFiltered$sddunits=="in"] = jsFiltered$secchidepth[jsFiltered$sddunits=="in"]/39.37
jsFiltered$sddunits[jsFiltered$sddunits=="in"] = "m"


View(jsFiltered)

write.table(jsFiltered, file = "JLSDtotal.csv", sep = "\t")

## This is a clunky way to do it, but I think it's the easiest to understand
## Let's assume you want +/- 1 day, that means it's a three day window you want to
## match dates to, so we'll do it in three stemps

#1) Same day matches (inner_join here means only take dates that exist in both
# datasets) (Also, I don't have the lat long csv you use, so I'm just going to use 
# Jordan_Secchi_Combined, you might want to swap this for your LatLog_JL_Secchi)

## First we need the date formats to be the same
jsFiltered <- jsFiltered %>%
  mutate(date = ymd(date))

same_day <- Jordan_REFL %>%
  inner_join(jsFiltered)

# Sample taken previous day
previous_day <- Jordan_REFL %>%
  inner_join(jsFiltered %>% mutate(date = date + 1))

# Sample taken following day
following_day <- Jordan_REFL %>%
  inner_join(jsFiltered %>% mutate(date = date - 1))

## Combine them all together
matchups <- same_day %>% mutate(MatchType = 'same day') %>%
  bind_rows(previous_day %>% mutate(MatchType = 'previous day')) %>%
  bind_rows(following_day %>% mutate(MatchType = 'following day'))

## We have 201 +/- 1 day matchups!
# It's good to get rid of intermediate variables to keep things clean
rm(previous_day, same_day, following_day)


#Histogram of Secchi Depth
hist(matchups$secchidepth)
### Weird value at 6.60m secchi depth
plot(matchups$date, matchups$secchidepth)


hist(matchups$Red)

## Using ggplots (very much worth learning)
ggplot(matchups,aes(x = Red, y = secchidepth)) + geom_point() +
  ylim(0,2) + # get rid on one outlier
  geom_smooth(method = 'lm') +
  ggpubr::stat_cor(label.y = 2) +
  ggpubr::stat_regline_equation(label.y = 1.75)

write.table(matchups, file = "JLSDmatchups.csv", sep = "\t")


###############################################################
###############################################################
###############################################################
##Chlorophyll-a
###############################################################
###############################################################
###############################################################


## Read in both datasets and take a look at them
Jordan_ChlA_STORET <- read_xlsx('Jordan_ChlA_STORET.xlsx')
glimpse(Jordan_ChlA_STORET)

Jordan_ChlA_NWIS <- read_xlsx('Jordan_ChlA_NWIS.xlsx')
glimpse(Jordan_ChlA_NWIS)

# double check all the column names are the same
names(Jordan_ChlA_NWIS) == names(Jordan_ChlA_STORET)

##combine datasets
Jordan_ChlA_Combined <- rbind(Jordan_ChlA_NWIS, Jordan_ChlA_STORET)

##order by collection method, location, date
Jordan_ChlA_Combined <- Jordan_ChlA_Combined %>%
  arrange(`SampleCollectionMethod/MethodName`, MonitoringLocationIdentifier, ActivityStartDate)

##Add latitude and longitude for each locationID
Jordan_LatLong <- read_csv('Jordan In Situ LatLong.csv')
glimpse(Jordan_LatLong)


##Match the MonitoringLocationIdentifier names from the LatLong dataset and the combined dataset.
LatLong_JL_ChlA <- merge(Jordan_ChlA_Combined, Jordan_LatLong, by='MonitoringLocationIdentifier')
View(LatLong_JL_ChlA)


##Add reflectance values
Jordan_REFL <- read_csv('JordanLakeREFL2.csv')
glimpse(Jordan_REFL)
colnames(Jordan_REFL)

## Separate the date from the system index column

## Here, str_split breaks the input value on the set pattern '_0000", then [[1]][1]
# takes the first result of that.  The map functions just tell it go row by row and
# repeat the same process.
Jordan_REFL <- Jordan_REFL %>%
  mutate(LandsatID = map_chr(`system:index`, ~str_split(.,'_0000')[[1]][1]),
         date =  map_chr(LandsatID, ~tail(strsplit(., "_")[[1]], n =1)),
         date = ymd(date)) %>%
  select(-`system:index`)

## The Monitoring Location Identifier column is very weird, we'll rename it with a 
## workaround
colnames(Jordan_REFL)[16] <-  'locationID'



## Get rid of variables you don't want
## In the code below the format is select(X = Y), X is your new name, Y is the old Name

jsFiltered <- LatLong_JL_ChlA %>%
  select(date = ActivityStartDate, 
         locationID = MonitoringLocationIdentifier,
         latitude = LatitudeMeasure,
         longitude = LongitudeMeasure,
         method = `SampleCollectionMethod/MethodName`,
         equipment = SampleCollectionEquipmentName,
         depth = `ActivityDepthHeightMeasure/MeasureValue`,
         depthunits = `ActivityDepthHeightMeasure/MeasureUnitCode`,
         chla = ResultMeasureValue,
         chlaunits = `ResultMeasure/MeasureUnitCode`,
         source = ProviderName) 

View(jsFiltered)

write.table(jsFiltered, file = "JLChlAtotal.csv", sep = "\t")

## This is a clunky way to do it, but I think it's the easiest to understand
## Let's assume you want +/- 1 day, that means it's a three day window you want to
## match dates to, so we'll do it in three stemps

#1) Same day matches (inner_join here means only take dates that exist in both
# datasets) (Also, I don't have the lat long csv you use, so I'm just going to use 
# Jordan_Secchi_Combined, you might want to swap this for your LatLog_JL_Secchi)

## First we need the date formats to be the same
jsFiltered <- jsFiltered %>%
  mutate(date = ymd(date))

same_day <- Jordan_REFL %>%
  inner_join(jsFiltered)

# Sample taken previous day
previous_day <- Jordan_REFL %>%
  inner_join(jsFiltered %>% mutate(date = date + 1))

# Sample taken following day
following_day <- Jordan_REFL %>%
  inner_join(jsFiltered %>% mutate(date = date - 1))

## Combine them all together
matchups <- same_day %>% mutate(MatchType = 'same day') %>%
  bind_rows(previous_day %>% mutate(MatchType = 'previous day')) %>%
  bind_rows(following_day %>% mutate(MatchType = 'following day'))

## We have 961 +/- 1 day matchups!
# It's good to get rid of intermediate variables to keep things clean
rm(previous_day, same_day, following_day)


write.table(matchups, file = "JLChlAmatchups.csv", sep = "\t")
