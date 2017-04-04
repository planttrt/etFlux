library('rjags')
library(data.table)
source('loaddata.R')

jags <- jags.model('heatCan.bugs',
                   data = list('Si' = data$Si,
                               'Tair' = data$Tair,
                               'Tsir' = data$Tsur,
                               'WS' = data$WS,
                               'ET' = data$ET,
                               'n' = n),
                   n.chains = 1,
                   n.adapt = 50)

update(jags, 1000)

samples <- jags.samples(jags,n.iter = 1000,
                        c('albedo','albedo','eSur',
                          'Cgrnd','Cconv','Aconv',
                          'tau','sigma'))

boxplot(samples, outline = F)

