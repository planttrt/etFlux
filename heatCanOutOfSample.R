
table(ameriLST$Site)
# ChR Dk2 Dk3 NC2

#df <- ameriLST[Site=='KS2', ]
df <- ameriLST[Site%in%c('ChR', 'Dk2', 'NC2')]
df[,SitesID:=as.numeric(as.factor(as.character(Site)))]
# df <- ameriLST[Site%in%c('ChR', 'Dk2')]
dim(df)

dfSites <- df$SitesID
dfSites <- rep(1, length(dfSites))
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


boxplot(samples[c("Aconv","Cconv", "abs", "eSky","eSur","sigma")])






### out of sample
Dk3 <- ameriLST[Site=='Dk3']

pred <- apply(predictET(Dk3, samples), 2, mean)
obs <- Dk3$ET

plotObsPred(obs, pred, nbin = 15)
abline(0,1,col='red')
lm(pred~obs-1)
print(cor(obs, pred))

Dk3[,plot(Year+DOY/365, ET, type = 'l', lty=2)]
Dk3[,lines(Year+DOY/365, pred, col='darkgreen', lwd=2)]

