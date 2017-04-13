
table(ameriLST$Site)
# ChR Dk2 Dk3 NC2

#df <- ameriLST[Site=='KS2', ]
df <- ameriLST[Site%in%c('ChR', 'Dk2', 'Dk3', 'NC2')]
# df <- ameriLST[Site%in%c('ChR', 'Dk2')]
dim(df)



jags <- jags.model('heatCanSites.bugs',
                   data = list('Si' = df$RS,
                               'Tair' = df$TA,
                               'Tsur' = df$LST,
                               'WS' = df$WS,
                               'ET' = df$ET,
                               'Sites' = as.numeric(as.factor(as.character(df$Site))),
                               'ns' = length(unique(df$Site)),
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

plotObsPred(obs, pred, nbin = 15, ptcol = as.numeric(as.factor(as.character(df$Site))))
abline(0,1,col='red')
lm(pred~obs-1)
print(cor(obs, pred))
