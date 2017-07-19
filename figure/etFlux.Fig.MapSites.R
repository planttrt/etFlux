library(maps)

source('~/Projects/procVisData/dataViz.R')
siteNames.tmp <- c('Chestnut Ridge',
                   'Duke Forest Hardwoods',
                   'Duke Forest Loblolly',
                   'Loblolly Plantation',
                   "Mary's River Fir")

png('figure/etFlux.Fig.MapSites.png', width = 7, height = 4, units = 'in', res = 300)
par(mar=c(0,0,0,0))
map('state')#, xlim=c(-85,-75), ylim=c(32,38))
pts <- sites[Code%in%c('ChR', 'Dk2', 'NC2', 'Me2'), .(Name, Code, Longitude, Latitude)]
points(pts[,.(Longitude, Latitude)], pch=c(15,17:19),col='#1555D2', cex=2)
legend('bottomleft',legend = siteNames.tmp[-3], col = '#1555D2', pch= c(15, 17:19), bty='n')
mtext('Study sites across the USA', font=2, cex=1.5, line = 0)
dev.off()