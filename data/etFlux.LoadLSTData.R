library(data.table)
library(lubridate)

etFlux.LoadLSTData <- function(){
  LST <- list()
  LST$Day_view_time <- merge(read.csv('data/LST/LST1/Day_view_time.csv', header = F), 
                             read.csv('data/LST/LST23/Day_view_time.csv', header = F),
                             by = c(1:2))
  LST$LST_Day_1km <- merge(read.csv('data/LST/LST1/LST_Day_1km.csv', header = F), 
                           read.csv('data/LST/LST23/LST_Day_1km.csv', header = F),
                           by = c(1:2))
  LST$QC_Day <- merge(read.csv('data/LST/LST1/QC_Day.csv', header = F), 
                      read.csv('data/LST/LST23/QC_Day.csv', header = F),
                      by = c(1:2))
  # LST$Clear_day_cov <- merge(read.csv('data/LST/LST1/Clear_day_cov.csv', header = F), 
  #                            read.csv('data/LST/LST2/Clear_day_cov.csv', header = F),
  #                            by = c(1:2))
  # LST$Emis_31 <- merge(read.csv('data/LST/LST1/Emis_31.csv', header = F), 
  #                      read.csv('data/LST/LST2/Emis_31.csv', header = F),
  #                      by = c(1:2))
  # LST$Emis_32 <- merge(read.csv('data/LST/LST1/Emis_32.csv', header = F), 
  #                      read.csv('data/LST/LST2/Emis_32.csv', header = F),
  #                      by = c(1:2))
  
  LST <- lapply(LST, function(x){colnames(x) <- c('Year','DOY',sites$Code) ; as.data.table(x)})
  LST <- lapply(LST, melt, id.vars = 1:2, variable.name = 'Site')
  LST <- data.table(Year = LST$LST_Day_1km$Year, 
                    DOY = LST$LST_Day_1km$DOY, 
                    Site = LST$LST_Day_1km$Site,
                    LST_Day_1km = -273.15 + 0.02*LST$LST_Day_1km$value, 
                    Day_view_time = 0.1*LST$Day_view_time$value, 
                    QC_Day = LST$QC_Day$value, 
                    # Clear_day_cov = 0.0005 * LST$Clear_day_cov$value,
                    # Emis_31 = 0.49 + 0.002*LST$Emis_31$value,
                    # Emis_32 = 0.49 + 0.002*LST$Emis_32$value,
                    GoodQuality = NA)
  # LST[,Emissivity := (Emis_31+Emis_32)/2]
  LST[QC_Day%%4==0, GoodQuality:=T]
  LST[,Hour:=Day_view_time]
  LST <- na.omit(LST)
  
  LST
}