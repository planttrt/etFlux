

cols <- c(rep('#707070', 4), 'black') 

png('figure/etFlux.Fig.MainParam.png', width = 3.5, height = 6, units = 'in', res = 300)
par(mfrow=c(3,1), mar=c(0,0,1,0), oma=c(8,5,0,2), xaxt='n')
boxplot(data.table(perSiteModel$DT$abs, genModel$DT$abs), outline = F, ylim = c(0.40,.65), col = NULL, border = cols)
mtext('α', side = 2, line = 3, cex = 1, font = 2)
mtext('(a)', adj = .99, font = 2, line=-1.3)
boxplot(data.table(perSiteModel$DT$eSky, genModel$DT$eSky), outline = F, ylim = c(.6,.9), col = NULL, border = cols)
mtext('ϵsky', side = 2, line = 3, cex = 1, font = 2)
mtext('(b)', adj = .99, font = 2, line=-1.3)
boxplot(data.table(perSiteModel$DT$eSur, genModel$DT$eSur), outline = F, ylim = c(.9,1), col = NULL, border = cols)
mtext('ϵsur', side = 2, line = 3, cex = 1, font = 2)
mtext('(c)', adj = .99, font = 2, line=-1.3)

text(labels = siteNames, x = 1:5, y = .89, srt = 65, xpd = NA, adj=1, font=2, col = cols)
dev.off()
