source('~/Projects/procVisData/dataViz.R')

obs <- ameriLST[Site=='Dk3', ET]
pred <- apply(predictET(ameriLST[Site=='Dk3'], samples = genModel.Wind.DukeOut$Samples), 2, mean)
cols <- c('#e56c5f','grey','#354358')

png('figure/etFlux.Fig.OutSamplePredict.png', width = 6, height = 6, units = 'in', res = 300)
par(mar=c(4,4,3,2),  pty = "s")
plotObsPred(obs, pred, breaks = 0:16, 
            xlab ='', ylab = '',ptcol = cols[2], box.col = cols[3],
            ylim = c(0,16), xlim=c(0,16))
mtext('Observed ET (mm/day)', side = 1, line = 2.5, cex = 1.3,  font=2)
mtext('Predicted ET (mm/day)', side = 2, line = 2.5,  cex = 1.3, font=2)
mtext('out-of--sample comparison', font = 2, cex=1.7, line = 1)
abline(0,1,col=cols[1], lwd=2)
mtext(paste0('R²=',signif(cor(obs, pred)^2,2)),
      adj = 0.1, line = -2, font=2, cex=1.7)
dev.off()

