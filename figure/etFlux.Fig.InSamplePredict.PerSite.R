source('~/Projects/procVisData/dataViz.R')
out <- perSiteModel.Wind

siteNames.tmp <- c('Chestnut Ridge, TN',
                   'Duke Forest Hardwoods, NC',
                   'Duke Forest Loblolly Pine, NC',
                   'Loblolly Plantation, NC',
                   "Mary's River Fir Site, OR")

png('figure/etFlux.Fig.InSamplePredict.PerSite.png', width = 8, height = 8.5, units = 'in', res = 300)

par(mfrow=c(2,2), mar=c(0,0,0,0), oma=c(5,5,4,1), pty = "s")
for(i in 1:length(sitesList)){
  par(xaxt='n', yaxt='n')
  if(i%in%c(1,4))par(yaxt='s')
  if(i%in%c(4,5))par(xaxt='s')
  
  if(sitesList[i]=='Dk3') next
  w <- which(out$Data$Site==sitesList[i])
  obs <- out$DT$ETobs$`50%`[w]
  pred <- out$DT$ETpred$`50%`[w]
  cols <- c('#e56c5f','grey','#354358')
  xylim <- quantile(obs, probs = c(.05,.95))
  plotObsPred(obs, pred, 
              breaks = 0:16,
              # nbin = 20,
              xlab ='', ylab = '',ptcol = cols[2], box.col = cols[3],
              ylim = c(0,16), xlim=c(0,16)
              # xlim=xylim, ylim= xylim
  )
  abline(0,1,col=cols[1], lwd=2)
  mtext(siteNames.tmp[i], adj = 0.1, line = -2, font=2, cex=1)
  # mtext(paste0('R²=', signif(cor(obs, pred)^2,2)), adj = 0.9, line = -2, font=2, cex=1, side = 1)
}

mtext('Observed ET (mm/day)', side = 1, line = 3, cex = 1.3,  font=2, outer = T)
mtext('Predicted ET (mm/day)', side = 2, line = 3,  cex = 1.3, font=2, outer = T)
mtext('in-sample comparison', font = 2, cex=1.7, line = 1, outer = T)

dev.off()