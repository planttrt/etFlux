LST[,UniCode:=paste(Site, Year,
                    sprintf(DOY,fmt = '%03d'), 
                    sprintf(floor(Hour*2),fmt = '%02d'), sep = '.' )]
ameri[,UniCode:=paste(Site, Year,
                      sprintf(DOY,fmt = '%03d'), 
                      sprintf(floor(Hour*2)+1,fmt = '%02d'), sep = '.' )]

LST
ameri

ameriLST <- merge(LST[,.(Site, Year, DOY, LST=LST_Day_1km, Emissivity, UniCode)], 
                  ameri[,.(TA, RS, WS, ET, UniCode)], by = 'UniCode')
# ameriLST[,plot(Year+DOY/365, ET, col=Site)]
ameriLST <- na.omit(ameriLST)


table(ameriLST$Site)
# ChR Dk2 Dk3 NC2

df <- ameriLST[Site=='KS2', ]
dim(df)


jags <- jags.model('heatCan.bugs',
                   data = list('Si' = df$RS,
                               'Tair' = df$TA,
                               'Tsur' = df$LST,
                               'WS' = df$WS,
                               'ET' = df$ET,
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
