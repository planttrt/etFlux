library(data.table)
library(lubridate)

etFlux.LoadAmeriData <- function (){
  ameriflux <- list()
  
  # ameriflux$Akn <- read.csv('data/ameriflux/csv/AMF_US-Akn_BASE_HH_4-1.csv', skip = 2)
  ameriflux$ChR <- read.csv('data/ameriflux/csv/AMF_US-ChR_BASE_HH_2-1.csv', skip = 2)
  ameriflux$Dk1 <- read.csv('data/ameriflux/csv/AMF_US-Dk1_BASE_HH_3-1.csv', skip = 2)
  ameriflux$Dk2 <- read.csv('data/ameriflux/csv/AMF_US-Dk2_BASE_HH_3-1.csv', skip = 2)
  ameriflux$Dk3 <- read.csv('data/ameriflux/csv/AMF_US-Dk3_BASE_HH_3-1.csv', skip = 2)
  ameriflux$KS1 <- read.csv('data/ameriflux/csv/AMF_US-KS1_BASE_HH_2-1.csv', skip = 2)
  ameriflux$KS2 <- read.csv('data/ameriflux/csv/AMF_US-KS2_BASE_HH_2-1.csv', skip = 2)
  ameriflux$NC1 <- read.csv('data/ameriflux/csv/AMF_US-NC1_BASE_HH_2-1.csv', skip = 2)
  ameriflux$NC2 <- read.csv('data/ameriflux/csv/AMF_US-NC2_BASE_HH_3-1.csv', skip = 2)
  
  ameriflux$Me1 <- read.csv('data/ameriflux/csv/AMF_US-Me1_BASE_HH_2-1.csv', skip = 2)
  ameriflux$Me2 <- read.csv('data/ameriflux/csv/AMF_US-Me2_BASE_HH_8-1.csv', skip = 2)
  ameriflux$Me3 <- read.csv('data/ameriflux/csv/AMF_US-Me3_BASE_HH_3-1.csv', skip = 2)
  ameriflux$Me4 <- read.csv('data/ameriflux/csv/AMF_US-Me4_BASE_HH_4-1.csv', skip = 2)
  ameriflux$Me5 <- read.csv('data/ameriflux/csv/AMF_US-Me5_BASE_HH_2-1.csv', skip = 2)
  ameriflux$Me6 <- read.csv('data/ameriflux/csv/AMF_US-Me6_BASE_HH_6-1.csv', skip = 2)
  ameriflux$Wrc <- read.csv('data/ameriflux/csv/AMF_US-Wrc_BASE_HH_8-1.csv', skip = 2)
  ameriflux$MRf <- read.csv('data/ameriflux/csv/AMF_US-MRf_BASE_HH_4-1.csv', skip = 2)
  
  ameriflux$Ho3 <- read.csv('data/ameriflux/csv/AMF_US-Ho3_BASE_HH_2-1.csv', skip = 2)
  ameriflux$GMF <- read.csv('data/ameriflux/csv/AMF_US-GMF_BASE_HH_2-1.csv', skip = 2)
  ameriflux$LPH <- read.csv('data/ameriflux/csv/AMF_US-LPH_BASE_HH_1-1.csv', skip = 2)
  ameriflux$Blo <- read.csv('data/ameriflux/csv/AMF_US-Blo_BASE_HH_3-1.csv', skip = 2)
  
  ameriflux.NA <- lapply(ameriflux, function(x){x[x==-9999] <- NA; as.data.table(x)})
  ameriflux.NA <- lapply(ameriflux.NA, function(x){x[,.(Time=as.POSIXct(as.character(TIMESTAMP_START),format='%Y%m%d%H%M'),
                                                        TA, 
                                                        RS=SW_IN, 
                                                        WS, 
                                                        ET= LE/(2502-2.308*TA)/1000*3600*24)]})
  ameri <- Reduce(rbind, ameriflux.NA)
  ameri$Site <- rep( names(ameriflux.NA), sapply(ameriflux.NA,nrow))
  
  ameri[ET<0, ET:=0]
  ameri[RS<0, RS:=0]
  ameri[,Year:=year(Time)]
  ameri[,DOY:=yday(Time)]
  ameri[,Hour:=hour(Time)+minute(Time)/60]
  
  ameri
}

