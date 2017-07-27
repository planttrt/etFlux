
oneOutPrediction <- list()


for(i in 1:length(sitesList)){
  s <- sitesList[i]
  out <- outWind[[s]]
  
  obs <- ameriLST[Site==s, ET]
  
  predMat <- predictET(ameriLST[Site==s], 
                       samples = out$Samples, 
                       useWindData = T, 
                       CC = cc[i])
  
  pred <- apply(predMat, 2, mean)
  
  oneOutPrediction[[i]] <- data.table(obs=obs, pred= pred)
  names(oneOutPrediction)[length(oneOutPrediction)] <- s
}


cols <- c('#e56c5f','grey','#354358')




png('figure/etFlux.Fig.oneSampleOut.png', width = 8, height = 5.6, units = 'in', res = 300)
par(mfrow = c(2,3))
# layout(matrix(c(1, 1, 2, 3, 4, 5), nrow = 3, byrow = T))
par(oma=c(5,5,1,1),  mar=c(0,0,0,0), pty = "s", xaxt='n', yaxt='n')

for(i in 1:length(sitesList)){
  if(i==3) plot(1, type="n", axes=F, xlab="", ylab="")
  s <- sitesList[i]
  obs <- oneOutPrediction[[s]]$obs/24
  pred <- oneOutPrediction[[s]]$pred/24
  
  plotObsPred(obs, pred, breaks = (0:16)/24, 
              xlab ='', ylab = '',ptcol = cols[2], box.col = cols[3],
              ylim = c(0,16/24), xlim=c(0,16/24))
  if(i%in%c(3,4,5)) axis(1, xaxt='s', font=2)
  if(i%in%c(1,3)) axis(2, yaxt='s', font=2)
  # mtext('out-of-sample comparison', font = 2, cex=1.7, line = 1)
  abline(0,1,col=cols[1], lwd=2)
  R2 <- signif(cor(obs, pred)^2,2)
  RMSE <- signif(rmse(obs, pred), 2)
  # mtext(paste0(letters[i], ') ', siteNames[i], '(',sitesList[i], ')'), adj = 0.05, line = -1.5, font=2, cex=1)
  mtext(paste0('(',letters[i], ') ', siteNames[i]), adj = 0.05, line = -1.5, font=2, cex=1)
  # mtext(paste0('R²=', R2), side = 1, adj = 0.95, line = -1.5, font=1, cex=1)
  mtext(paste0('RMSE=', RMSE), side = 1, adj = .95, line = -1.5, font=1, cex=1)
}
mtext('Observed ET (mm/hour)', side = 1, line = 2.5, cex = 1.3,  font=2, outer = T)
mtext('Predicted ET (mm/hour)', side = 2, line = 2.5,  cex = 1.3, font=2, outer = T)
dev.off()
