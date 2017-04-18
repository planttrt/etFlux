source('etFlux.Func.R')

source('etFlux.LoadData.R')

# ameri <- etFlux.LoadAmeriData()
load('ameri.RData')
LST <- etFlux.LoadLSTData()

ameriLST <- etFlux.LoadData(ameri, LST)

# sitesList <- c('ChR', 'Dk2', 'Dk3', 'NC2')
sitesList <- c('ChR',  'Dk2',  'Dk3',  'NC2',  'Me2', 'Me5',  'Me6',  'MRf', 'Ho3','LPH','GMF','Blo')

table(ameriLST[,Site])
genModel <- etFlux.Model(ameriLST, 
                         useWindData = F,
                         perSite = F,
                         SitesList = sitesList)
genModel$DT <- JagsOutput2list(genModel)

genModel.Wind <- etFlux.Model(ameriLST, 
                              useWindData = T,
                              perSite = F,
                              SitesList = sitesList)
genModel.Wind$DT <- JagsOutput2list(genModel.Wind)

perSiteModel <- etFlux.Model(ameriLST, 
                               useWindData = F,
                               perSite = T,
                               SitesList = sitesList)
perSiteModel$DT <- JagsOutput2list(perSiteModel)

perSiteModel.Wind <- etFlux.Model(ameriLST,
                                    useWindData = T, 
                                    perSite = T,
                                    SitesList = sitesList)
perSiteModel.Wind$DT <- JagsOutput2list(perSiteModel.Wind)

sampleOutModel <- etFlux.Model(ameriLST, 
                                      useWindData = F,
                                      perSite = F,
                                      SitesList = c('ChR', 'Dk2', 'NC2'))
sampleOutModel$DT <- JagsOutput2list(sampleOutModel)

sampleOutModel.Wind <- etFlux.Model(ameriLST, 
                               useWindData = T,
                               perSite = F,
                               SitesList = c('ChR', 'Dk2', 'NC2'))
sampleOutModel.Wind$DT <- JagsOutput2list(sampleOutModel.Wind)

# save.image(file='etFlux.RData')
