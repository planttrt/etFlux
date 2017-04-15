
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

jagsOut <- jags.model('heatCanSites.bugs',
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

update(jagsOut, 1000)
samplesOut <- jags.samples(jagsOut,n.iter = 1000,
                        c('abs',
                          'eSur',
                          'eSky',
                          'Cconv',
                          'Aconv',  
                          'sigma',
                          'ETpred',  
                          'ETobs',
                          'Snet','THi','THo','H','LE'))

print(signif(sapply(samplesOut, mean),2))
samplesOut[c('abs',
          'eSur',
          'eSky',
          'Cconv',
          'Aconv',
          'sigma')]


boxplot(samplesOut[c("Aconv","Cconv", "abs", "eSky","eSur","sigma")])






### out of sample
Dk3 <- ameriLST[Site=='Dk3']

pred <- apply(predictET(Dk3, samplesOut), 2, mean)
obs <- Dk3$ET


Dk3[,plot(Year+DOY/365, ET, type = 'l', lty=2)]
Dk3[,lines(Year+DOY/365, pred, col='darkgreen', lwd=2)]




plotObsPred(obs, pred, breaks = 0:16, 
            xlab = 'Observed ET (mm/day)',
            ylab = 'Predicted ET (mm/day)',
            ylim = c(0,16), xlim=c(0,16))
mtext('out-of-sample (Duke pine forest)', font = 2, cex=1.7, line = 1)
abline(0,1,col='red')
mtext(paste0('R²=',signif(cor(obs, pred)^2,2)),
      adj = 0.1, line = -2, font=2, cex=1.7)

lm(pred~obs-1)