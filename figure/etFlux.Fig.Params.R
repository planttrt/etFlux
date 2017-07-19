source('~/Projects/procVisData/dataViz.R')

siteNames.tmp <- c('Chestnut Ridge',
                   'Duke Hardwoods',
                   'Duke Loblolly',
                   "Mary's River Fir",
               'Loblolly Plantation',
               'Average Model')
cols <- c(rep('#707070', 5), 'black') 

png('figure/etFlux.Fig.Params.Wind.png', width = 4, height = 6, units = 'in', res = 300)
par(mfrow=c(4,1), mar=c(0,0,1,0), oma=c(8,5,0,2), xaxt='n')
boxplot(data.table(perSiteModel.Wind$DT$sigma, genModel.Wind$DT$sigma),outline = F, ylim = c(1.2,4), col = cols, border = cols)
mtext('σ (mm/day)', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(perSiteModel.Wind$DT$abs, genModel.Wind$DT$abs), outline = F, ylim = c(0.2,.60), col = cols, border = cols)
mtext('α', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(perSiteModel.Wind$DT$eSky, genModel.Wind$DT$eSky), outline = F, ylim = c(.6,.9), col = cols, border = cols)
mtext('ϵsky', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(perSiteModel.Wind$DT$eSur, genModel.Wind$DT$eSur), outline = F, ylim = c(.9,1), col = cols, border = cols)
mtext('ϵsur', side = 2, line = 3, cex = 1, font = 2)
text(labels = siteNames.tmp, x = 1:6, y = .89, srt = 65, xpd = NA, adj=1, font=2, col = cols)
dev.off()

png('figure/etFlux.Fig.Params.NoWind.png', width = 4, height = 6, units = 'in', res = 300)
par(mfrow=c(4,1), mar=c(0,0,1,0), oma=c(8,5,0,2), xaxt='n')
boxplot(data.table(perSiteModel$DT$sigma, genModel$DT$sigma),outline = F, ylim = c(1.2,4), col = cols, border = cols)
mtext('σ (mm/day)', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(perSiteModel$DT$abs, genModel$DT$abs), outline = F, ylim = c(0.20,.60), col = cols, border = cols)
mtext('α', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(perSiteModel$DT$eSky, genModel$DT$eSky), outline = F, ylim = c(.6,.9), col = cols, border = cols)
mtext('ϵsky', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(perSiteModel$DT$eSur, genModel$DT$eSur), outline = F, ylim = c(.9,1), col = cols, border = cols)
mtext('ϵsur', side = 2, line = 3, cex = 1, font = 2)
text(labels = siteNames.tmp, x = 1:6, y = .89, srt = 65, xpd = NA, adj=1, font=2, col = cols)
dev.off()

png('figure/etFlux.Fig.Params.NoWind2.png', width = 4, height = 5, units = 'in', res = 300)
par(mfrow=c(3,1), mar=c(0,0,1,0), oma=c(8,5,0,2), xaxt='n')
boxplot(data.table(perSiteModel$DT$abs, genModel$DT$abs), outline = F, ylim = c(0.20,.60), col = cols, border = cols)
mtext('α', side = 2, line = 3, cex = 1, font = 2)
mtext('(a)', adj = .999, font = 2, line=-1.3)
boxplot(data.table(perSiteModel$DT$eSky, genModel$DT$eSky), outline = F, ylim = c(.6,.9), col = cols, border = cols)
mtext('ϵsky', side = 2, line = 3, cex = 1, font = 2)
mtext('(b)', adj = .999, font = 2, line=-1.3)
boxplot(data.table(perSiteModel$DT$eSur, genModel$DT$eSur), outline = F, ylim = c(.9,1), col = cols, border = cols)
mtext('ϵsur', side = 2, line = 3, cex = 1, font = 2)
mtext('(c)', adj = .999, font = 2, line=-1.3)

text(labels = siteNames.tmp, x = 1:6, y = .89, srt = 65, xpd = NA, adj=1, font=2, col = cols)
dev.off()

png('figure/etFlux.Fig.Params.Conv.png', width = 3, height = 6, units = 'in', res = 300)
par(mfrow=c(3,1), mar=c(0,0,2,0), oma=c(8,5,0,2), xaxt='n')
boxplot(data.table(perSiteModel.Wind$DT$Aconv, genModel.Wind$DT$Aconv), outline = F, ylim = c(.5, .8), col = cols, border = cols)
mtext('a', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(perSiteModel.Wind$DT$Cconv, genModel.Wind$DT$Cconv), outline = F, ylim = c(0, 25), col = cols, border = cols)
mtext('c', side = 2, line = 3, cex = 1, font = 2)
boxplot(data.table(perSiteModel$DT$Hconv, genModel$DT$Hconv), outline = F, ylim = c(0,40), col = cols, border = cols)
mtext('h (W/m²/K)', side = 2, line = 3, cex = 1, font = 2)
text(labels = siteNames.tmp, x = 1:6, y = 13, srt = 65, xpd = NA, adj=1, font=2, col = cols)
dev.off()


