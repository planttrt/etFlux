source('~/Projects/procVisData/colorSet.R')
load('data/usaRaster.RData')
library(maps)
library(raster)
physio <- shapefile('data/physioProvinceLatLon/physioProvinceLatLon.shp')

ndvi <- raster('data/ndvi/MOD13A2_M_NDVI_2015-07-01_rgb_3600x1800.FLOAT.TIFF')
prism.sampl <- raster('data/PRISM_tmean_stable_4kmM2_2015_bil/PRISM_tmean_stable_4kmM2_2015_bil.bil')
rsmp <- raster(nrow=nrow(prism.sampl), ncol=ncol(prism.sampl))
rsmp@extent <- prism.sampl@extent
ndvi <- resample(ndvi, rsmp)
ndvi[is.na(resample(usaRaster, ndvi, method='ngb'))] <- NA


png('figure/etFlux.Fig.VI.Map.png', width = 6.5, height = 3.5, res = 300,  units = 'in')
par(mar=c(0,0,2,0), bty='n')
plot(ndvi, zlim=c(0,1), axes=F,
     col=colorRampPalette(colList.brownGreen)(1000))
map('usa',add=T)    
plot(physio, add=T)
mtext('NDVI', cex=2, font=2, line = 0)
mtext('(a)', cex=2, font=2, line = 0, adj=0)
dev.off()
