physio <- shapefile('data/physioProvinceLatLon/physioProvinceLatLon.shp')
# source('transAuxFuncs.R')
r <- sapply(dir('~/Box Sync/Home Folder/Private/DATA/DT/4K/NORM//', recursive = T, full.names = T), raster)
rng <- c(5,20)
# colList <- rev(colList.FunAndTropical)
# colList <- colList.Ideas4Homes
# colList <- colList.WarmAndRustic[c(2,4,1)]
colList <- c('#4897D8', '#ffffff','#C60000')



r <- rMean(r[6:9])
r [r< rng[1]] <- rng[1]
r [r> rng[2]] <- rng[2]

png('figure/etFlux.Fig.dT.Summer.USA.png', width = 6.5, height = 3.5, res = 300,  units = 'in')
par(mar=c(0,0,2,0), bty='n')
plot(r, axes=F,
     # xlim=c(-90.5,-74.5), ylim=c(25,40), 
     zlim=rng, #breaks=bks,
     col=colorRampPalette(colList)(100))
map('usa', add = T)
plot(physio, add=T)
mtext('Thermal stress (∆T)', cex=2, font=2, line = 0)
mtext('(b)', cex=2, font=2, line = 0, adj=0)

dev.off()