source('etFlux.Func.R')

out <- genModel

# ameri[Site=='Dk3', .(TA=mean(TA, na.rm=T)), DOY][,plot(DOY, TA, type='l')]
# ameri[Site=='NC2', .(TA=mean(TA, na.rm=T)), DOY][,lines(DOY, TA, col='grey')]

site <- 'ChR'

addSensPlots <- function(site, cols=c('#000000', '#808080', '#80808080',
                                      '#000000', '#808080', '#80808080')){
  plot(NA, xlim=c(1,365), ylim=rev(c(-1.3,-.75)), xlab='', ylab='', xaxt='n')
  annualMean <- ameriLST[Site==site, .(TA=mean(TA, na.rm=T), TS=mean(LST, na.rm=T)), DOY]
  TA <- annualMean[order(DOY), TA]
  TS <- annualMean[order(DOY), TS]
  DOY <- annualMean[order(DOY), DOY]
  
  tempSens <- getTemporalSens(TA, TS, out)
  tempSens <- lapply(tempSens, function(x)(t(apply(x, 2, quantile, probs = c(0.025, .5, .975), na.rm=T))))
  plg <- data.frame(x=c(DOY, rev(DOY)), y1=c(tempSens$dDT[,1], rev(tempSens$dDT[,3])), y2= c(tempSens$dTS[,1], rev(tempSens$dTS[,3])))
  plg <- na.omit(plg)
  
  polygon(plg$x, plg$y1, border = cols[2], col = cols[3])
  lines(DOY, tempSens$dDT[,2], lwd=2, col=cols[1])
  
  polygon(plg$x, plg$y2 ,border = cols[5], col = cols[6])
  lines(DOY, tempSens$dTS[,2], lwd=2, col=cols[4])
}


cols <- c('#023442','#114756','#11475680',
         '#6A0E00', '#8A2717', '#8A271780')

png('figure/etFlux.Fig.DT.SensTime.png', res = 300, 
    height = 5, width = 8, units = 'in')
par(mfrow=c(2,3), mar=c(0,0,0,0), oma=c(4,4,1,1), xaxt='n', yaxt='n')

for(i in 1:length(sitesList)){
  if(i==3)
  {
    plot(1, type="n", axes=F, xlab="", ylab="")
    legend(legend = c('w.r.t. ∆T', 'w.r.t. TS'),
           fill = cols[c(2,5)],
               'center', bty='n', cex=3, horiz = F, text.font = 2)
    
    }
  addSensPlots(sitesList[i], cols = cols)
  mtext(paste0('(',letters[i], ') ', siteNames[i], ' (',sitesList[i], ')'), adj = .1,
        line = -2, font=2, cex=1)
  
  if(i%in%c(3,4,5)) axis(1, xaxt='s', font=2)
  if(i%in%c(1,3)) axis(2, yaxt='s', font=2)
  
}
mtext('Day of year', side = 1, line = 2.5, cex = 1.3,  font=2, outer = T)
mtext('Sensitivty (mm/day/°C)', side = 2, line = 2.5,  cex = 1.3, font=2, outer = T)

dev.off()
