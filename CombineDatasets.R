library(readxl)
library(dplyr)
library(lubridate)
##combine datasets
Jordan_Secchi_Combined <- rbind(Jordan_Secchi_NWIS, Jordan_Secchi_STORET)
##order by collection method, location, date
Jordan_Secchi_Combined[with(Jordan_Secchi_Combined, order('SampleCollectionMethod/MethodName', 'MonitoringLocationIdentifier')), ]
