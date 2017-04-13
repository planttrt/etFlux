library(data.table)
library(lubridate)

ameriflux <- list()
sites <- read.csv('data/sites.csv')
sites$Code <- gsub('US-','',sites$Code)

# ameriflux$Akn <- read.csv('data/ameriflux/csv/AMF_US-Akn_BASE_HH_4-1.csv', skip = 2)
ameriflux$ChR <- read.csv('data/ameriflux/csv/AMF_US-ChR_BASE_HH_2-1.csv', skip = 2)
ameriflux$Dk1 <- read.csv('data/ameriflux/csv/AMF_US-Dk1_BASE_HH_3-1.csv', skip = 2)
ameriflux$Dk2 <- read.csv('data/ameriflux/csv/AMF_US-Dk2_BASE_HH_3-1.csv', skip = 2)
ameriflux$Dk3 <- read.csv('data/ameriflux/csv/AMF_US-Dk3_BASE_HH_3-1.csv', skip = 2)
ameriflux$KS1 <- read.csv('data/ameriflux/csv/AMF_US-KS1_BASE_HH_2-1.csv', skip = 2)
ameriflux$KS2 <- read.csv('data/ameriflux/csv/AMF_US-KS2_BASE_HH_2-1.csv', skip = 2)
ameriflux$NC1 <- read.csv('data/ameriflux/csv/AMF_US-NC1_BASE_HH_2-1.csv', skip = 2)
ameriflux$NC2 <- read.csv('data/ameriflux/csv/AMF_US-NC2_BASE_HH_3-1.csv', skip = 2)

ameriflux.NA <- lapply(ameriflux, function(x){x[x==-9999] <- NA; as.data.table(x)})
ameriflux.NA <- lapply(ameriflux.NA, function(x){x[,.(Time=as.POSIXct(as.character(TIMESTAMP_START),format='%Y%m%d%H%M'),
                                                      TA, 
                                                      RS=SW_IN, 
                                                      WS, 
                                                      ET= LE/(2502-2.308*TA)/1000*3600*24)]})
ameri <- rbind(data.table(ameriflux.NA$ChR, Site='ChR'),
               data.table(ameriflux.NA$Dk1, Site='Dk1'),
               data.table(ameriflux.NA$Dk2, Site='Dk2'),
               data.table(ameriflux.NA$Dk3, Site='Dk3'),
               data.table(ameriflux.NA$KS1, Site='KS1'),
               data.table(ameriflux.NA$KS2, Site='KS2'),
               data.table(ameriflux.NA$NC1, Site='NC1'),
               data.table(ameriflux.NA$NC2, Site='NC2'))
ameri[ET<0, ET:=0]
ameri[SW_IN<0, SW_IN:=0]
ameri[,Year:=year(Time)]
ameri[,DOY:=yday(Time)]
ameri[,Hour:=hour(Time)+minute(Time)/60]
ameri




