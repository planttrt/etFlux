source('etFlux.Func.R')

source('etFlux.LoadData.R')

# ameri <- etFlux.LoadAmeriData()
load('ameri.RData')
LST <- etFlux.LoadLSTData()

ameriLST <- etFlux.MergeData(ameri, LST)

# sitesList <- c('ChR', 'Dk2', 'Dk3', 'NC2')
sitesList <- c('ChR',  'Dk2',  'Dk3', 'NC2', 'MRf')

table(ameriLST[,Site])

perSiteModel <- etFlux.Model(ameriLST, 
                             useWindData = F,
                             perSite = T,
                             sitesList = sitesList)
perSiteModel$DT <- JagsOutput2list(perSiteModel)

perSiteModel.Wind <- etFlux.Model(ameriLST,
                                  useWindData = T, 
                                  perSite = T,
                                  SitesList = sitesList)
perSiteModel.Wind$DT <- JagsOutput2list(perSiteModel.Wind)


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




sampleOutModel <- etFlux.Model(ameriLST, 
                                      useWindData = F,
                                      perSite = F,
                                      SitesList = c('ChR', 'Dk2', 'NC2'))
sampleOutModel$DT <- JagsOutput2list(sampleOutModel)

sampleOutModel.Wind <- etFlux.Model(ameriLST, 
                               useWindData = T,
                               perSite = F,
                               sitesList = sitesList
                               # SitesList = c('ChR', 'Dk2', 'NC2')
                               )
sampleOutModel.Wind$DT <- JagsOutput2list(sampleOutModel.Wind)

save.image(file='etFlux.RData')
