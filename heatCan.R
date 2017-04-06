library('rjags')
library(data.table)
source('~/Projects/procVisData/dataViz.R')

load('~/Projects/transFlux/bothSites.Rdata')
df <- na.omit(transData[Site=='PK', .(TA, RgOpen, Rg, LST, WS, Tr= ET)])
df <- na.omit(transData[Site=='PK', .(TA, RgOpen, Rg, LST, WS, Tr= TR)])
# df <- na.omit(transData[Site=='PK',])
# df <- na.omit(transData[Site=='BW', .(TA, RgOpen, Rg, LST, WS, Tr= TR)])
dim(df)
# df[,plot(Rg, Sdifopen)]
# abline(0,1)
jags <- jags.model('heatCan.bugs',
                   data = list('Si' = df$Rg,
                               'Tair' = df$TA,
                               'Tsur' = df$LST,
                               'WS' = df$WS,
                               'ET' = df$Tr,
                               'n' = nrow(df)),
                   # n.chains = 1,
                   # n.adapt = 50, 
                   quiet = T)

update(jags, 1)

samples <- jags.samples(jags,n.iter = 5000,
                        c('albedo',
                          'eta',
                          'eSur',
                          'eSky',
                           # 'Cgrnd',
                          'Cconv',
                           'Aconv',
                          'sigma',
                          'ETpred',  
                          'ETobs'))
 
plot(samples$albedo)
#plot(samples$Cconv)
print(signif(sapply(samples, mean)),2)

pred <- apply(samples$ETpred, 1, mean)
obs <- apply(samples$ETobs, 1, mean)

plotObsPred(obs, pred, nbin = 10, breaks = seq(0,9,1), xlim = c(0.5,9), ylim=c(0.5,9))
abline(0,1,col='red')
plotObsPred(obs, pred, nbin = 10, breaks = seq(0,14,1.5), xlim = c(2,14), ylim=c(2,14))
abline(0,1,col='red')
print(cor(obs, pred)^2)
#boxplot(samples[-2], outline = F)

