source('etFlux.Func.R')

out <- genModel

# ameri[Site=='Dk3', .(TA=mean(TA, na.rm=T)), DOY][,plot(DOY, TA, type='l')]
# ameri[Site=='NC2', .(TA=mean(TA, na.rm=T)), DOY][,lines(DOY, TA, col='grey')]

site <- 'ChR'

addSensPlots <- function(site, cols=c('#000000', '#808080', '#80808080',
                                      '#000000', '#808080', '#80808080')){
  annualMean <- ameriLST[Site==site, .(TA=mean(TA, na.rm=T), TS=mean(LST, na.rm=T)), DOY]
  TA <- annualMean[order(DOY), TA]
  TS <- annualMean[order(DOY), TS]
  DOY <- annualMean[order(DOY), DOY]
  
  tempSens <- getTemporalSens(TA, TS, out)
  tempSens <- lapply(tempSens, function(x)(t(apply(x, 2, quantile, probs = c(0.025, .5, .975), na.rm=T))))
  
  polygon(c(DOY, rev(DOY)), c(tempSens$dDT[,1], rev(tempSens$dDT[,3])),border = cols[2], col = cols[3])
  lines(DOY, tempSens$dDT[,2], lwd=2, col=cols[1])
  
  polygon(c(DOY, rev(DOY)), c(tempSens$dTS[,1], rev(tempSens$dTS[,3])),border = cols[5], col = cols[6])
  lines(DOY, tempSens$dTS[,2], lwd=2, col=cols[4])
}


cols <- c('#023442','#114756','#11475680',
         '#6A0E00', '#8A2717', '#8A271780')

png('figure/etFlux.Fig.DT.SensTime.png', res = 300, 
    height = 6, width = 6, units = 'in')
par(mfrow=c(2,2), mar=c(0,0,0,0), oma=c(4,4,1,1))

plot(NA, xlim=c(1,365), ylim=c(-1.3,-.85), xlab='', ylab='', xaxt='n')
mtext("(a) Chestnut Ridge (ChR)", adj = .1, font = 2, line = -1.5)
addSensPlots('ChR', cols = cols)

plot(NA, xlim=c(1,365), ylim=c(-1.3,-.85), xlab='', ylab='', xaxt='n', yaxt='n')
mtext("(b) Duke Hardwoods (Dk2)", adj = .1, font = 2, line = -1.5)
addSensPlots('Dk2', cols = cols)

plot(NA, xlim=c(1,365), ylim=c(-1.3,-.85), xlab='', ylab='')
mtext("(c) Mary's River Fir (MRf)", adj = .1, font = 2, line = -1.5)
addSensPlots('MRf', cols = cols)

plot(NA, xlim=c(1,365), ylim=c(-1.3,-.85), xlab='', ylab='', yaxt='n')
mtext("(d) Loblolly Plantation (NC2)", adj = .1, font = 2, line = -1.5)
addSensPlots('NC2', cols = cols)

mtext('Day of year', font=2, side = 1, line = 2.5, cex = 1.5, outer = T)
mtext('Sensitivity of ET (mm/day/°C)', font=2, side = 2, 
      line = 2.5, cex = 1.5, outer = T)

legend(legend = c('w.r.t. TS', 'w.r.t. ∆T'), fill = cols[c(2,5)],
       'bottom', bty='n', cex=1.4, horiz = T, text.font = 2)
dev.off()
