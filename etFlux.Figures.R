library(maps)

map('state', xlim=c(-85,-75), ylim=c(32,38))
points(sites[Code%in%c('ChR', 'Dk2', 'NC2'),
             .(Longitude, Latitude)], pch=19, cex=2)
mtext('Flux sites locations', font=2, cex=2, line = 2)

par(mfrow=c(2,2))
boxplot(data.frame(ChR=samples$Aconv[1,,],
                   Dk2=samples$Aconv[2,,],
                   Dk3=samples$Aconv[3,,],
                   NC2=samples$Aconv[4,,]), 
        ylim = c(0,1),
        outline = F)
mtext('Aconv', font=2, cex=1.5)

boxplot(data.frame(ChR=samples$Cconv[1,,],
                   Dk2=samples$Cconv[2,,],
                   Dk3=samples$Cconv[3,,],
                   NC2=samples$Cconv[4,,]), 
         ylim = c(10,25),
        outline = F)
mtext('Cconv', font=2, cex=1.5)




boxplot(data.frame(ChR=samples$eSur[1,,],
                   Dk2=samples$eSur[2,,],
                   Dk3=samples$eSur[3,,],
                   NC2=samples$eSur[4,,]), 
        ylim = c(0.9,1),
        outline = F)
mtext('eSur', font=2, cex=1.5)


boxplot(data.frame(ChR=samples$eSky[1,,],
                   Dk2=samples$eSky[2,,],
                   Dk3=samples$eSky[3,,],
                   NC2=samples$eSky[4,,]), 
        ylim = c(0.6,.9),
        outline = F)
mtext('eSky', font=2, cex=1.5)



par(mfrow=c(2,2))

boxplot(data.frame(ChR=-samples$H[1,,],
                   Dk2=-samples$H[2,,],
                   Dk3=-samples$H[3,,],
                   NC2=-samples$H[4,,]), 
        ylim = c(0,500),
        outline = F)
mtext('Sensible heat (W/m2)', font=2, cex=1.5)


boxplot(data.frame(ChR=-samples$LE[1,,],
                   Dk2=-samples$LE[2,,],
                   Dk3=-samples$LE[3,,],
                   NC2=-samples$LE[4,,]), 
        ylim = c(0,500),
        outline = F)
mtext('Latent heat (W/m2)', font=2, cex=1.5)


boxplot(data.frame(ChR=samples$THi[1,,],
                   Dk2=samples$THi[2,,],
                   Dk3=samples$THi[3,,],
                   NC2=samples$THi[4,,]), 
        ylim = c(0,500),
        outline = F)
mtext('Incoming thermal (W/m2)', font=2, cex=1.5)

boxplot(data.frame(ChR=-samples$THo[1,,],
                   Dk2=-samples$THo[2,,],
                   Dk3=-samples$THo[3,,],
                   NC2=-samples$THo[4,,]), 
        ylim = c(0,500),
        outline = F)
mtext('Outgoing thermal (W/m2)', font=2, cex=1.5)




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

plotObsPred(obs, pred, nbin = 15)
abline(0,1,col='red')
lm(pred~obs-1)
print(cor(obs, pred))
