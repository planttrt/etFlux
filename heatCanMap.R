library(maps)

map('state', xlim=c(-85,-75), ylim=c(32,38))
points(sites[Code%in%c('US-ChR', 'US-Dk2', 'US-NC2'),.(Longitude, Latitude)], pch=19)
