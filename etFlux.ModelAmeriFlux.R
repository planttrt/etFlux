library('rjags')
library(data.table)
source('~/Projects/procVisData/dataViz.R')
source('heatCanFunc.R')


table(ameriLST$Site)
# ChR Dk2 Dk3 NC2

#df <- ameriLST[Site=='KS2', ]
df <- ameriLST[Site%in%c('ChR', 'Dk2', 'Dk3', 'NC2')]
df[,SitesID:=as.numeric(as.factor(as.character(Site)))]
# df <- ameriLST[Site%in%c('ChR', 'Dk2')]
dim(df)

dfSites <- df$SitesID

ns <- length(unique(dfSites))

jags <- jags.model('heatCanSites.bugs',
                   data = list('Si' = df$RS,
                               'Tair' = df$TA,
                               'Tsur' = df$LST,
                               'WS' = df$WS,
                               'ET' = df$ET,
                               'Sites' = dfSites,
                               'ns' = ns,
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


samples[c("Aconv","Cconv", "abs", "eSky","eSur","sigma")]

pred <- apply(samples$ETpred, 1, mean)
obs <- apply(samples$ETobs, 1, mean)

#ETpred <- predictET(df, samples)

plotObsPred(obs, pred, breaks = 0:16, 
            xlab = 'Observed ET (mm/day)',
            ylab = 'Predicted ET (mm/day)',
            ylim = c(0,16), xlim=c(0,16))
mtext('in-sample predicitons for all sites', font = 2, cex=1.7, line = 1)
abline(0,1,col='red')
mtext(paste0('R²=',signif(cor(obs, pred)^2,2)),
      adj = 0.1, line = -2, font=2, cex=1.7)

lm(pred~obs-1)
