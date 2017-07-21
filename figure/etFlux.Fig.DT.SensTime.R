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
par(mfrow=c(2,2), mar=c(0,0,0,0), oma=c(4,4,1,1), xaxt='n', yaxt='n')

for(i in 1:length(sitesList)){
  plot(NA, xlim=c(1,365), ylim=c(-1.35,-.86), xlab='', ylab='', xaxt='n')
  mtext(paste0('(',letters[i], ') ', siteNames[i], '(',sitesList[i], ')'), adj = 0.05, line = -1.5, font=2, cex=1)
  addSensPlots(sitesList[i], cols = cols)
  if(i%in%c(3,4)) axis(1, xaxt='s', font=2)
  if(i%in%c(1,3)) axis(2, yaxt='s', font=2)
  
}
mtext('Day of year', side = 1, line = 2.5, cex = 1.3,  font=2, outer = T)
mtext('Sensitivty (mm/day/°C)', side = 2, line = 2.5,  cex = 1.3, font=2, outer = T)

legend(legend = c('w.r.t. S', 'w.r.t. ∆T'), fill = cols[c(5,2)],
       'bottom', bty='n', cex=1.4, horiz = T, text.font = 2)
dev.off()
