source('~/Projects/procVisData/dataViz.R')
source('etFlux.Func.R')

source('etFlux.LoadData.R')

# ameri <- etFlux.LoadAmeriData()
load('ameri.RData')
LST <- etFlux.LoadLSTData()

ameriLST <- etFlux.LoadData(ameri, LST)

# sitesList <- c('ChR', 'Dk2', 'Dk3', 'NC2')
sitesList <- c('ChR',  'Dk2',  'Dk3', 'NC2', 'MRf')
table(ameriLST[,Site])
ameriLST[Site%in% sitesList , .(unique(Latitude), unique(Longitude)), Site]
ameriLST[Site%in% sitesList,]

sampleOutModel <- etFlux.Model(ameriLST, 
                               useWindData = F,
                               perSite = T,
                               SitesList = c('ChR',  'Dk2',  'Dk3', 'NC2', 'MRf'),
                               vegCover = c(1, .9, .9, 0.8, 0.5))

sampleOutModel$DT <- JagsOutput2list(sampleOutModel)

sampleOutModel2 <- etFlux.Model(ameriLST, 
                               useWindData = F,
                               perSite = T,
                               SitesList = c('ChR',  'Dk2',  'Dk3', 'NC2', 'MRf'),
                               vegCover = 1/c(1, .9, .9, 0.8, 0.4))

sampleOutModel2$DT <- JagsOutput2list(sampleOutModel2)

plotObsPred(sampleOutModel$DT$ETobs$`50%`,
     sampleOutModel$DT$ETpred$`50%`, ptcol = ameriLST[Site%in% sitesList,as.factor(Site)])
abline(0,1 , col='red')

w <-  which(ameriLST[Site%in% sitesList,.(Site)][,Site=='MRf'])
plotObsPred(sampleOutModel2$DT$ETobs$`50%`[w],
            sampleOutModel2$DT$ETpred$`50%`[w])
abline(0,1 , col='red')
