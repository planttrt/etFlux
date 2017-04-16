source('etFlux.Func.R')

source('etFlux.LoadData.R')

ameriLST <- etFlux.LoadData()

genModel <- etFlux.Model(ameriLST, 
                         noWindData = T,
                         perSite = F,
                         SitesList = c('ChR', 'Dk2', 'Dk3', 'NC2'))
genModel.Wind <- etFlux.Model(ameriLST, 
                         noWindData = F,
                         perSite = F,
                         SitesList = c('ChR', 'Dk2', 'Dk3', 'NC2'))
fourSitesModel <- etFlux.Model(ameriLST, 
                         noWindData = T,
                         perSite = T,
                         SitesList = c('ChR', 'Dk2', 'Dk3', 'NC2'))
fourSitesModel.Wind <- etFlux.Model(ameriLST,
                            noWindData = F, 
                            perSite = T,
                            SitesList = c('ChR', 'Dk2', 'Dk3', 'NC2'))
genModel.Wind.DukeOut <- etFlux.Model(ameriLST, 
                              noWindData = F,
                              perSite = F,
                              SitesList = c('ChR', 'Dk2', 'NC2'))
