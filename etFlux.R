library('rjags')
library(data.table)
source('~/Projects/procVisData/dataViz.R')
source('heatCanFunc.R')

load('~/Projects/transFlux/bothSites.Rdata')
#pk <- na.omit(transData[Site=='PK', .(TA, RgOpen, Rg, LST, WS, Tr= ET, Site=1)])
pk <- na.omit(transData[Site=='PK', .(YearCon, Year, DOY, TA, SOL = RgOpen, Rg, LST, WS, TR = ET)])
#bw <- na.omit(transData[Site=='BW', .(YearCon, Year, DOY, TA, SOL = RgOpen, Rg, LST, WS, TR = TR)])
df <- pk[Year%in%c(2009)]
#2009, 2007


dim(df)

jags <- jags.model('heatCan.bugs',
                   data = list('Si' = df$SOL,
                               'Tair' = df$TA,
                               'Tsur' = df$LST,
                               'WS' = df$WS,
                               'ET' = df$TR,
                               'n' = nrow(df)),
                   n.chains = 1,
                   quiet = T)

update(jags, 1000)
samples <- jags.samples(jags,n.iter = 1000,
                        c('abs',
                          'eSur',
                          'eSky',
                          'Cconv',
                          'Aconv',  
                          'sigma',
                          'ETpred',  
                          'ETobs',
                          'Snet','THi','THo','H','LE'))
 
print(signif(sapply(samples, mean),2))
samples[c('abs',
          'eSur',
          'eSky',
          'Cconv',
          'Aconv',
          'sigma')]

#albedo = .2, eSky = .70, eSur = .95, Cconv = 10, Aconv = .8, eta = .50, sigma = 1

pred <- apply(samples$ETpred, 1, mean)
obs <- apply(samples$ETobs, 1, mean)

#ETpred <- predictET(df, samples)

plotObsPred(obs, pred, nbin = 15)
abline(0,1,col='red')
lm(pred~obs-1)
print(cor(obs, pred))

# plotObsPred(obs, pred, nbin = 10, breaks = seq(0,9,1.5), xlim = c(0.5,9), ylim=c(0.5,9))
# abline(0,1,col='red')
# 
# plotObsPred(obs, pred, nbin = 10, breaks = seq(0,14,1.5), xlim = c(2,14), ylim=c(2,14))
# abline(0,1,col='red')



