library(data.table)

source('data/etFlux.LoadAmeriData.R')
source('data/etFlux.LoadLSTData.R')

sites <- as.data.table(read.csv('data/sites.csv'))
sites$Code <- gsub('US-','',sites$Code)


etFlux.LoadData <- function(){
  
  ameri <- etFlux.LoadAmeriData()
  
  LST <- etFlux.LoadLSTData()
  
  
  LST[,UniCode:=paste(Site, Year,
                      sprintf(DOY,fmt = '%03d'), 
                      sprintf(floor(Hour*2),fmt = '%02d'), sep = '.' )]
  
  ameri[,UniCode:=paste(Site, Year,
                        sprintf(DOY,fmt = '%03d'), 
                        sprintf(floor(Hour*2)+1,fmt = '%02d'), sep = '.' )]
  
  ameriLST <- merge(LST[,.(Site, Year, DOY, LST=LST_Day_1km, UniCode)], 
                    ameri[,.(TA, RS, WS, ET, UniCode)], by = 'UniCode')
  ameriLST <- merge(ameriLST, sites, by.x = 'Site', by.y = 'Code')
  # ameriLST[,plot(Year+DOY/365, ET, col=Site)]
  ameriLST <- na.omit(ameriLST)
  
  ameriLST
}