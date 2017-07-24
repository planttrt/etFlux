source('~/Projects/procVisData/dataViz.R')
source('etFlux.Func.R')
source('etFlux.LoadData.R')

# ameri <- etFlux.LoadAmeriData()
load('ameri.RData')
LST <- etFlux.LoadLSTData()
ameriLST <- etFlux.MergeData(ameri, LST)
table(ameriLST[,Site])

sitesList = c('ChR',  'Dk2',  'Dk3', 'NC2')
siteNames <- c('Chestnut Ridge',
                   'Duke Hardwoods',
                   'Duke Loblolly',
                   'Loblolly Plantation',
                   'Average Model')

cc <- c(1, .9, .85, .8)

ameriLST[Site%in% sitesList , .(unique(Species), unique(Latitude), unique(Longitude)), Site]

outWind <- list()
outNoWind <- list()

for(i in 1:length(sitesList)){
  message(paste('Site', i,' wind: ...'))
  outWind[[i]] <- oneSiteOut(sitesList = sitesList, siteOut = i, useWindData = T, CC = cc)
  message(paste('Site', i,' no wind: ...'))
  outNoWind[[i]] <- oneSiteOut(sitesList = sitesList, siteOut = i, useWindData = F, CC = cc)
  
}

names(outWind) <- names(outNoWind) <- sitesList

perSiteModel <- etFlux.Model(ameriLST, 
                             useWindData = F,
                             perSite = T,
                             sitesList = sitesList)
perSiteModel$DT <- JagsOutput2list(perSiteModel)

genModel <- etFlux.Model(ameriLST, 
                         useWindData = F,
                         perSite = F,
                         sitesList = sitesList)
genModel$DT <- JagsOutput2list(genModel)

save(list = c('outNoWind', 'outWind', 'perSiteModel', 'genModel'), file = 'out.RData')


