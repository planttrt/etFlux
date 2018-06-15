library(shape)
library(fields)
library(raster)
source('etFlux.Func.R')
source('~/Projects/procVisData/colorSet.R')
physio <- shapefile('data/physioProvinceLatLon/physioProvinceLatLon.shp')

out <- genModel

dET_dDT <- list()
# dET_dTS <- list()
# dET_dTA <- list()

monthRange <- 7:9

for(i in 1:length(monthRange)){
  dET_dDT[[i]] <- sensPerMonth(monthRange[i], out, 1)
  # dET_dTS[[i]] <- sensPerMonth(monthRange[i], out, 2)
  # dET_dTA[[i]] <- sensPerMonth(monthRange[i], out, 3)
}

r1 <- setRange(rMean(dET_dDT))
# r2 <- setRange(rMean(dET_dTS))
# r3 <- setRange(rMean(dET_dTA))

col <- colorRampPalette(colList.Contad)(100)

png('figure/etFlux.Fig.GraphicalAbstract.png', width = 7, height = 4.5, res = 300,  units = 'in')
par(mar=c(1,1,1,1), oma=c(2,2,2,1), bty='n')
plot(r1, col = col, legend = F, xaxt='n', yaxt='n')
map('usa', add = T)
plot(physio, add=T)
scalebar(d = 1000, xy = c(-122, 25),type = 'bar', below = 'kilometers', divs = 4)
northArrow(xb = -75, yb = 25, len=1.5, lab="N", tcol = 'black', font.lab = 2, col='black')  

mtext('Sensitivity of evapotranspiration to ∆T across the USA', font=2, line = 0, cex=1)
insertLegend(quantile(r1, probs=c(.01,.99)), col)

Arrows(-65.5, 38, -65.5, 44.5, xpd=T, lwd=2)
Arrows(-65.5, 33, -65.5, 26.5, xpd=T, lwd=2)

# mtext('Longitude (°C)', side = 1, line = 2)
# mtext('Latitude (°C)', side = 2, line = 2)

mtext('Low', side = 2, line = -26.5, at = 41.5, font=2)
mtext('High', side = 2, line = -26.5, at = 30, font=2)


dev.off()
