library('rjags')
library(data.table)

load('~/Projects/transFlux/bothSites.Rdata')
df <- na.omit(transData[Site=='PK', .(TA, RgOpen, Rg, LST, WS, ET, TR)])
dim(df)
# df[,plot(Rg, Sdifopen)]
# abline(0,1)
jags <- jags.model('heatCan.bugs',
                   data = list('Si' = df$Rg,
                               'Tair' = df$TA,
                               'Tsur' = df$LST,
                               'WS' = df$WS,
                               'ET' = df$ET,
                               'n' = nrow(df)),
                   n.chains = 1,
                   n.adapt = 50, quiet = T)

update(jags, 1000)

samples <- jags.samples(jags,n.iter = 1000,
                        c('albedo','eSur','eSky',
                          'Cgrnd',
                          'Cconv','Aconv',
                          'sigma'))



preds <- jags.samples(jags,n.iter = 1000, 'ETpred')

pred <- apply(preds$ETpred, 1, mean)
plot(df$ET, pred)
abline(0,1,col='red')
lm(df$ET~ pred-1)

print(signif(sapply(samples, mean)),2)
boxplot(samples[-2], outline = F)

