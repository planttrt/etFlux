library(fields)
library(raster)
source('etFlux.Func.R')
source('~/Projects/procVisData/colorSet.R')
physio <- shapefile('~/Projects/traitsModel/data/maps/physioProvinceLatLon/physioProvinceLatLon.shp')

out <- genModel

dET_dDT <- list()
dET_dTS <- list()
# dET_dTA <- list()

monthRange <- 7:9

for(i in 1:length(monthRange)){
  dET_dDT[[i]] <- sensPerMonth(monthRange[i], out, 1)
  dET_dTS[[i]] <- sensPerMonth(monthRange[i], out, 2)
  # dET_dTA[[i]] <- sensPerMonth(monthRange[i], out, 3)
}

r1 <- setRange(rMean(dET_dDT))
r2 <- setRange(rMean(dET_dTS))
# r3 <- setRange(rMean(dET_dTA))

col <- colorRampPalette(colList.Contad)(100)

png('figure/etFlux.Fig.DT.SensMaps.png', width = 6, height = 8, res = 300,  units = 'in')
par(mar=c(1,1,1,1), mfrow=c(2,1), oma=c(2,2,2,1), bty='n')
plot(r1, col = col, legend = F, xaxt='n')
map('usa', add = T)
plot(physio, add=T)
mtext('(a) Sensitivity to ∆T, Jul-Aug-Sept', font=2, line = 0, cex=1)
insertLegend(quantile(r1, probs=c(.01,.99)), col)

plot(r2, col = col, legend = F, xaxt='n')
map('usa', add = T)
plot(physio, add=T)
mtext('(b) Sensitivity to surface temperature, Jul-Aug-Sept', font=2, line = 0, cex=1)
insertLegend(quantile(r2, probs=c(.01,.99)), col)

# plot(r3, col = col, legend = F)
# map('usa', add = T)
# plot(physio, add=T)
# mtext('(c) Sensitivity to air temperature, Jul-Aug-Sept', font=2, line = 0, cex=1)
# insertLegend(quantile(r3, probs=c(.01,.99)), col)

dev.off()
