source('~/Projects/procVisData/dataViz.R')
siteNames <- c('Chestnut Ridge',
               'Duke Hardwoods',
               'Duke Loblolly Pine',
               'NC Loblolly Pine',
               'General Model')
cols <- c(rep('#707070', 4), 'black') 

png('figure/etFlux.Fig.Params.Wind.png', width = 3, height = 8, units = 'in', res = 300)
par(mfrow=c(4,1), mar=c(0,0,2,0), oma=c(8,5,0,2), xaxt='n')
boxplot(data.table(perSiteModel.Wind$DT$sigma, genModel.Wind$DT$sigma),outline = F, ylim = c(2,4), col = cols, border = cols)
mtext('σ (mm/day)', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(perSiteModel.Wind$DT$abs, genModel.Wind$DT$abs), outline = F, ylim = c(0.45,.60), col = cols, border = cols)
mtext('αCan', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(perSiteModel.Wind$DT$eSky, genModel.Wind$DT$eSky), outline = F, ylim = c(.6,.9), col = cols, border = cols)
mtext('ϵSky', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(perSiteModel.Wind$DT$eSur, genModel.Wind$DT$eSur), outline = F, ylim = c(.93,1), col = cols, border = cols)
mtext('ϵSur', side = 2, line = 3, cex = 1, font = 2)
text(labels = siteNames, x = 1:5, y = .925, srt = 65, xpd = NA, adj=1, font=2, col = cols)
dev.off()

png('figure/etFlux.Fig.Params.NoWind.png', width = 3, height = 8, units = 'in', res = 300)
par(mfrow=c(4,1), mar=c(0,0,2,0), oma=c(8,5,0,2), xaxt='n')
boxplot(data.table(fourSitesModel$DT$sigma, genModel$DT$sigma),outline = F, ylim = c(2,4), col = cols, border = cols)
mtext('σ (mm/day)', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(fourSitesModel$DT$abs, genModel$DT$abs), outline = F, ylim = c(0.40,.60), col = cols, border = cols)
mtext('αCan', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(fourSitesModel$DT$eSky, genModel$DT$eSky), outline = F, ylim = c(.6,.9), col = cols, border = cols)
mtext('ϵSky', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(fourSitesModel$DT$eSur, genModel$DT$eSur), outline = F, ylim = c(.93,1), col = cols, border = cols)
mtext('ϵSur', side = 2, line = 3, cex = 1, font = 2)
text(labels = siteNames, x = 1:5, y = .925, srt = 65, xpd = NA, adj=1, font=2, col = cols)
dev.off()

png('figure/etFlux.Fig.Params.Conv.png', width = 3, height = 6, units = 'in', res = 300)
par(mfrow=c(3,1), mar=c(0,0,2,0), oma=c(8,5,0,2), xaxt='n')
boxplot(data.table(perSiteModel.Wind$DT$Aconv, genModel.Wind$DT$Aconv), outline = F, ylim = c(.5, .8), col = cols, border = cols)
mtext('Aconv', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(perSiteModel.Wind$DT$Cconv, genModel.Wind$DT$Cconv), outline = F, ylim = c(12, 22), col = cols, border = cols)
mtext('Cconv', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(fourSitesModel$DT$Hconv, genModel$DT$Hconv), outline = F, ylim = c(15,40), col = cols, border = cols)
mtext('Hconv (W/m²/K)', side = 2, line = 3, cex = 1, font = 2)
text(labels = siteNames, x = 1:5, y = 13, srt = 65, xpd = NA, adj=1, font=2, col = cols)
dev.off()


