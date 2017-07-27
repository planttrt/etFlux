source('~/Projects/procVisData/dataViz.R')
source('etFlux.Func.R')
source('etFlux.LoadData.R')

# ameri <- etFlux.LoadAmeriData()
load('ameri.RData')
LST <- etFlux.LoadLSTData()
ameriLST <- etFlux.MergeData(ameri, LST)
table(ameriLST[,Site])

# sitesList = c('ChR',  'Dk2',  'Dk3', 'NC2')
# sitesList = c('ChR',  'Dk2',  'Dk3', 'NC2', 'MRf')

siteNames <- c('Chestnut Ridge',
               'Duke Hardwoods',
               'Duke Loblolly',
               "Mary's River Fir",
               'Loblolly Plantation',
               'Average Model')
# cc <- c(1, .9, .85, .8)
# cc <- c(1, .9, .85, .75, 1)
# cc <- rep(1, length(sitesList))
# sitesList <- 'MRf'
# siteNames <- "Mary's River Fir"
# cc <- .4
sitesList <- names(table(ameriLST[,Site]))
tblSite <- ameriLST[Site%in% sitesList , .(unique(Species), unique(Latitude), unique(Longitude)), Site]

        # Site "ChR"  "Dk2"  "Dk3"  "Ho3"  "KS2" "MRf"  "Me2"  "Me3"  "Me4"  "Me5"  "Me6"  "NC2"  "Wrc" 
        # CC   "1.00" "0.90" "0.95" "0.75" "NA"  "0.40" "0.90" "0.10" "0.40" "0.80" "0.30" "0.80" "0.80"
tblSite$CC <- c(1.0,   0.90,  0.95,  1.00,  NA,   0.70,  0.90,  0.10,  0.60,  0.80,  0.30,  0.85,  0.80)
tblSite$CC <- c(1.0,   0.90,  0.95,  1.00,  NA,   0.70,  0.90,  0.10,  0.60,  0.80,  0.30,  0.75,  0.80)
tblSite$CC <- c(1.0,   1.0,   1.00,  1.00,  NA,   0.70,  0.90,  0.10,  0.60,  0.80,  0.30,  .80,  0.80)
tblSite$CC <- c(1.0,   1.0,   0.95,  1.20,  NA,   0.80,  0.90,  0.10,  0.60,  0.80,  0.30,  .85,  0.80)
tblSite$CC <- c(1.0,   1.0,   1.00,  1.00,  NA,   1.00,  0.90,  0.10,  0.60,  0.80,  0.30,  1.0,  0.80)
tblSite$CC <- c(1.0,   1.0,   1.00,  0.90,  NA,   0.90,  0.90,  0.10,  0.60,  0.80,  0.30,  0.9,  0.80)
tblSite$CC <- c(0.85,  0.9,   0.90,  0.85,  NA,   0.7,  0.90,  0.10,  0.60,  0.80,  0.30,  1.0,  0.80)
tblSite$CC <- 1
tblSite$CC <- NA

sitesList <- c("ChR", "Dk2", "Dk3", "MRf", "NC2")
siteNames <- c('Chestnut Ridge',
               'Duke Hardwoods',
               'Duke Loblolly',
               "Mary's River Fir",
               'Loblolly Plantation',
               'Average Model')
sitesList <- tblSite[Site%in%sitesList, Site]
# siteNames <- sitesList

tblSite[Site%in%sitesList, ]$CC <- c(1, 1, 1, 1.1, .7)
cc <- tblSite[Site%in%sitesList, CC]
tblSite
sitesList
cc

outWind <- list()
# outNoWind <- list()

for(i in 1:length(sitesList)){
  message(paste('Site', i, sitesList[i], ' wind: ...'))
  outWind[[i]] <- oneSiteOut(sitesList = sitesList, siteOut = i, useWindData = T, CC = cc)
  # message(paste('Site', i,' no wind: ...'))
  # outNoWind[[i]] <- oneSiteOut(sitesList = sitesList, siteOut = i, useWindData = F, CC = cc)
  
}

names(outWind) <- sitesList
# names(outNoWind) <- sitesList

perSiteModel <- etFlux.Model(ameriLST,
                             useWindData = T,
                             perSite = T,
                             sitesList = sitesList, 
                             CC = cc
                              )
perSiteModel$DT <- JagsOutput2list(perSiteModel)

genModel <- etFlux.Model(ameriLST,
                         useWindData = F,
                         perSite = F,
                         sitesList = sitesList)
genModel$DT <- JagsOutput2list(genModel)

# save(list = c('outNoWind', 'outWind', 'perSiteModel', 'genModel'), file = 'out2.RData')
save.image('figure/5sites.RData')

