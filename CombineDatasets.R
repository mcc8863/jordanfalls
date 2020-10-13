library(readxl)
library(tidyverse)
library(lubridate)

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
  arrange(`SampleCollectionMethod/MethodName`, MonitoringLocationIdentifier, ActivityStartDate)

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

##How do I match the MonitoringLocationIdentifier and the Date?
totalJL_Secchi <- merge(LatLong_JL_Secchi, Jordan_REFL, by='MonitoringLocationIdentifier')
##Error above: "Error in fix.by(by.y, y) : 'by' must specify a uniquely valid column"



## Get rid of variables you don't want
## In the code below the format is select(X = Y), X is your new name, Y is the old Name
jsFiltered <- totalJL_Secchi %>%
  select(date = ActivityStartDate, 
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
View(jsFiltered)
