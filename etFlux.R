source('etFlux.Func.R')

source('etFlux.LoadData.R')

ameriLST <- etFlux.LoadData()

genModel <- etFlux.Model(ameriLST, 
                         useWindData = F,
                         perSite = F,
                         SitesList = c('ChR', 'Dk2', 'Dk3', 'NC2'))

genModel.Wind <- etFlux.Model(ameriLST, 
                              useWindData = T,
                              perSite = F,
                              SitesList = c('ChR', 'Dk2', 'Dk3', 'NC2'))

fourSitesModel <- etFlux.Model(ameriLST, 
                               useWindData = F,
                               perSite = T,
                               SitesList = c('ChR', 'Dk2', 'Dk3', 'NC2'))

fourSitesModel.Wind <- etFlux.Model(ameriLST,
                                    useWindData = T, 
                                    perSite = T,
                                    SitesList = c('ChR', 'Dk2', 'Dk3', 'NC2'))

genModel.Wind.DukeOut <- etFlux.Model(ameriLST, 
                                      useWindData = T,
                                      perSite = F,
                                      SitesList = c('ChR', 'Dk2', 'NC2'))

save.image(file='etFlux.RData')