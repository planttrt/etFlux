library(maps)

source('~/Projects/procVisData/dataViz.R')
siteNames <- c('Chestnut Ridge',
               'Duke Hardwoods',
               'Duke Loblolly Pine',
               'NC Loblolly Pine',
               'General Model')
cols <- c(rep('#707070', 4), 'black') 

png('figure/etFlux.Fig.MapSites.png', width = 6, height = 4, units = 'in', res = 300)
map('state', xlim=c(-85,-75), ylim=c(32,38))
points(sites[Code%in%c('ChR', 'Dk2', 'NC2'),
             .(Longitude, Latitude)], pch=19, cex=2)
mtext('Flux sites locations', font=2, cex=2, line = 2)
dev.off()