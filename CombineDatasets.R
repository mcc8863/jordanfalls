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

## I would suggest as a next step you get rid of variables you don't want
## In the code below the format is select(X = Y), X is your new name, Y is the old Name
jsFiltered <- Jordan_Secchi_Combined %>%
  select(method = `SampleCollectionMethod/MethodName`,
         equipment = SampleCollectionEquipmentName,
         date = ActivityStartDate, 
         locationID = MonitoringLocationIdentifier,
         depth = `ActivityDepthHeightMeasure/MeasureValue`,
         depthunits = `ActivityDepthHeightMeasure/MeasureUnitCode`,
         secchidepth = ResultMeasureValue,
         sddunits = `ResultMeasure/MeasureUnitCode`,
         source = ProviderName) 
View(jsFiltered)
##.... Keep going to include the rest of the variables you want!





