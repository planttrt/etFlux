source('~/Projects/procVisData/dataViz.R')

fluxes <- c('Net solar (W/m²)',
            'Incoming thermal (W/m²)',
            'Outgoing thermal (W/m²)',
            'Sensible heat (W/m²)')
fluxes <- c('R (W/m²)',
            'LI (W/m²)',
            'LO (W/m²)',
            'H (W/m²)')

DT <- cbind(perSiteModel$Data[, .(Site, Year, DOY, LST, TA, RS, ET)],
            as.data.table(perSiteModel$DT[c("H", "LE", "Snet", "THi", "THo")]))
DT[, Time:=Year+DOY/365]
DT[, table(Site)]


cols <- c('#e56c5f','grey','#354378', 'black')

png('figure/etFlux.Fig.Fluxes.png', width = 4, height = 6, units = 'in', res = 300)
par(mfrow=c(4,1), mar=c(.5,0,0,0), oma=c(4,5,1,1), xaxt='n')
DT[Site=='NC2', .(Snet = mean(`Snet.50%`)), DOY][order(DOY),plot(DOY, Snet, type = 'l',col=cols[1])]
mtext(fluxes[1], side = 2, line = 3, cex = 1, font = 2, adj = 0.5)
DT[Site=='NC2', .(THin = mean(`THi.50%`)), DOY][order(DOY),plot(DOY, THin, type = 'l',col=cols[2])]
mtext(fluxes[2], side = 2, line = 3, cex = 1, font = 2, adj = 0.5)
DT[Site=='NC2', .(THout = -mean(`THo.50%`)), DOY][order(DOY),plot(DOY, THout, type = 'l',col=cols[3])]
mtext(fluxes[3], side = 2, line = 3, cex = 1, font = 2, adj = 0.5)
par(xaxt='s')
DT[Site=='NC2', .(H = -mean(`H.50%`)), DOY][order(DOY),plot(DOY, H, type = 'l',col=cols[4])]
mtext(fluxes[4], side = 2, line = 3, cex = 1, font = 2, adj = 0.5)
mtext('Day of year', side = 1, line = 3, cex = 1.2, font = 2)
dev.off()

png('figure/etFlux.Fig.Fluxes.Box.png', width = 6, height = 5, units = 'in', res = 300)
par(mar=c(0,0,0,0), oma=c(4,4,1,1))
bp <- boxplot(abs(sapply(genModel$Samples[c("H", "Snet", "THi", "THo")], rowMeans)), outline=F, horizontal=F, col='#bbbbbb', names=NA)
#c('Sensible heat (H)','Net solar (Snet)','Incoming thermal (THi)','Outgoing thermal (THo)'
# mtext(text =  c("H", "R", "LI", "LO"), side = 2, at = -70, xpd=T, line = -2.7-seq(0,10.5, length.out = 4), adj=0, font=2, cex=1.7)
mtext(text =  c("H", "R", "LI", "LO"), side = 1, at = 1:4, xpd=T, adj=.5, font=2, cex=1.7, line = 2)
mtext('Energy flux (W/m²)', font = 2, cex=1.4, line = 2.5, side = 2)
dev.off()


