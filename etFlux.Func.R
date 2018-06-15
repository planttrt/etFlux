library(data.table)
library(truncnorm)
library(rjags)

simulateET <- function(df, abs, eSky, eSur, Cconv, Aconv, sigma, CC=1){
  Sigma <- 5.670367 * 10^-8
  Snet <- abs * df$SOL
  THi <- Sigma*eSky * (df$TA + 273.15)^4
  THo <- - Sigma * eSur * (df$LST + 273.15)^4
  H <- -(Cconv * df$WS^Aconv) * (df$LST - df$TA)
  LE <- -((Snet + THi + THo)/CC + H)
  Lambda <- 2502 - 2.308*df$TA
  mu <- -LE*24*3.6/Lambda
  ET <- rtruncnorm(n = length(mu), a = 0, sd = sigma, mean = mu)
  ET
}

predictET <- function(df, samples=NULL, useWindData = T, CC = rep(1, nrow(df))){
  Sigma <- 5.670367 * 10^-8
  
  if(length(CC)==1) CC <- rep(CC, nrow(df))
  
  abs <- t(apply(samples$abs, 1:2, mean))
  eSky <- t(apply(samples$eSky, 1:2, mean))
  eSur <- t(apply(samples$eSur, 1:2, mean))
  sigma <- t(apply(samples$sigma, 1:2, mean))
  
  if(!useWindData){
    Hconv <- t(apply(samples$Hconv, 1:2, mean))
  }else{
    Cconv <- t(apply(samples$Cconv, 1:2, mean))
    Aconv <- t(apply(samples$Aconv, 1:2, mean))
  }
  
  Snet <- abs %*% df$RS
  THi <- Sigma*eSky %*% (df$TA + 273.15)^4
  THo <- - Sigma * eSur %*% (df$LST + 273.15)^4
  if(!useWindData) {
    H <- - Hconv%*%(df$LST - df$TA)
  }else{
    H <- - sweep(sweep( exp(Aconv%*%log(df$WS)), 1, c(Cconv), '*'), 2, (df$LST - df$TA),'*')
  }
  LE <- -((Snet + THi + THo)/CC + H)
  Lambda <- 2502 - 2.308*df$TA
  mu <- sweep(-LE, 2, c(Lambda), '/')*24*3.6
  ET <- apply(mu, 2, rtruncnorm, n=length(sigma), a = 0, b = Inf, sd = (sigma))
  ET
}



etFlux.Model <- function(ameriLST, 
                         nburnin = 200,
                         ngibbs = 300,
                         n.chains = 1,
                         quiet = F,
                         useWindData = T,
                         CC = NULL,
                         perSite = T,
                         sitesList){
  bugsCode <- switch(useWindData+1, 'etFluxNoWind.bugs', 'etFluxWind.bugs')
  
  df <- ameriLST[Site%in%sitesList]
  sitesList <- unique(df$Site)
  df[,SitesID:=as.numeric(as.factor(as.character(Site)))]
  
  dfSites <- df$SitesID
  if(is.null(CC)) CC <- rep(1, length(dfSites))
  
  if(!perSite) dfSites <- rep(1, length(dfSites))
  ns <- length(unique(dfSites))
  
  if(!useWindData){
    bugsCode <- 'etFluxNoWind.bugs'
    dataList <- list('Si' = df$RS,
                     'Tair' = df$TA,
                     'Tsur' = df$LST,
                     # 'WS' = df$WS,
                     'ET' = df$ET,
                     'Sites' = dfSites,
                     'CC' = CC,
                     'ns' = ns,
                     'n' = nrow(df))
    variableNames <- c('abs',
                       'eSur', 'eSky',
                       'Hconv',
                       'sigma',
                       'ETpred', 'ETobs',
                       'Snet','THi','THo','H','LE')
    
  }else{
    bugsCode <- 'etFluxWind.bugs'
    dataList <- list('Si' = df$RS,
                     'Tair' = df$TA,
                     'Tsur' = df$LST,
                     'WS' = df$WS,
                     'ET' = df$ET,
                     'Sites' = dfSites,
                     'CC' = CC,
                     'ns' = ns,
                     'n' = nrow(df))
    
    variableNames <- c('abs',
                       'eSur', 'eSky',
                       'Cconv', 'Aconv',
                       'sigma',
                       'ETpred', 'ETobs',
                       'Snet','THi','THo','H','LE')
  }
  
  jags <- jags.model(bugsCode,
                     data = dataList,
                     n.chains = n.chains,
                     quiet = quiet)
  
  update(jags, nburnin)
  
  samples <- jags.samples(jags, n.iter = ngibbs, variable.names = variableNames)
  if(!perSite)sitesList <- 'general'
  
  output <- list(Samples = samples, 
                 Sites = sitesList, 
                 Data = df, 
                 useWindData = useWindData, 
                 perSite = perSite)
  # output$DT <- JagsOutput2list(output)
  output
}


JagsOutput2list <- function(output){
  DT <- list()
  nameList <- names(output$Samples)
  for(i in 1:length(nameList)){
    mc <- output$Samples[[nameList[i]]]
    if(dim(mc)[3]>1){
      mc <- t(apply(mc, 1:2, mean))
    }else{
      dim(mc) <- dim(mc)[-3]
      mc <- t(apply(mc, 1:2, function(x)x))
    }
    
    if(ncol(mc)==length(output$Sites)){
      mc <- as.data.table(mc)
      colnames(mc) <- output$Sites
    }else if(ncol(mc)==1){
      mc <- as.data.table(mc)
      colnames(mc) <- nameList[i]
    }else{
      mc <- as.data.table(t(apply(mc, 2, quantile, probs=c(.5, .025, .975))))
    }
    DT[[nameList[i]]] <- mc
  }
  DT
}



insertLegend <- function(rng, col){
  bty <- par()$bty
  par(bty='o')
  image.plot(legend.only=TRUE, zlim= rng, 
             smallplot= c(.89, .93, .10, .80),
             axis.args = list(cex.axis = 1, font=2),
             legend.args = list(text= 'mm/day/°C', side=3,xpd=T, adj=0, line=.7, font=2), 
             col = col, horizontal = F, yaxt='s') 
  par(bty=bty)
}
setRange <- function(r, rng=quantile(r, probs=c(.01,.99))){
  r[r<rng[1]] <- rng[1]
  r[r>rng[2]] <- rng[2]
  r
}
rMean <- function(rList){
  m <- rList[[1]]
  for(i in 2:length(rList)) m <- m + rList[[i]]
  m/length(rList)
}

sensPerMonth <- function(m=8, out, flag=1){
  TA <- raster(sprintf('~/Box Sync/Home Folder/Private/DATA/TA/4K/NORM/TA.4K.NORM.%02d.tif', m))
  TS <- raster(sprintf('~/Box Sync/Home Folder/Private/DATA/TS/4K/NORM/TS.4K.NORM.%02d.tif', m))
  
  σ <- 5.670367 * 10^-8
  λ <- (2502 - 2.308*TA)/24/3.6
  ϵsky <- mean(out$DT$eSky$general)
  ϵsur <- mean(out$DT$eSur$general)
  Hconv <- mean(out$DT$Hconv$general)
  
  sens <- switch(flag,
                 -1/λ * ( 4*σ*ϵsky*(TA+273.15)^3 + 4*σ*ϵsur*TS^3 + Hconv),
                 -1/λ * ( 4*σ*ϵsur*(TS+273.15)^3 + Hconv),
                 1/λ * ( 4*σ*ϵsky*(TA+273.15)^3+ Hconv))
  sens
}



getTemporalSens <- function(TA, TS, out){
  TA <- matrix(TA, nrow = 1)
  TS <- matrix(TS, nrow = 1)
  σ <- 5.670367 * 10^-8
  λ <- (2502 - 2.308*TA)/24/3.6
  one1 <- TA/TA
  
  ϵsky <- matrix(out$DT$eSky$general, ncol = 1)
  ϵsur <- matrix(out$DT$eSur$general, ncol = 1)
  Hconv <- matrix(out$DT$Hconv$general, ncol = 1)
  one2 <- ϵsur/ϵsur
  
  dDT <- (one2%*%(-1/λ)) * ( 4*σ*ϵsky%*%(TA+273.15)^3 + 4*σ*ϵsur%*%(TS^3) + Hconv%*%one1)
  dTS <- (one2%*%(-1/λ)) * ( 4*σ*ϵsur%*%(TS^3) + Hconv%*%one1)
  dTA <- (one2%*%(1/λ)) * ( 4*σ*ϵsky%*%(TA+273.15)^3 + Hconv%*%one1)
  list(dDT=dDT, dTS=dTS, dTA=dTA)
}



oneSiteOut <- function(
  sitesList = c('ChR',  'Dk2',  'Dk3', 'NC2'),
  siteOut = length(sitesList)+1,
  useWindData = T, 
  perSite = F,
  CC = rep(1, length(sitesList))){
  
  out <- etFlux.Model(ameriLST, 
                      useWindData = useWindData,
                      perSite = perSite,
                      sitesList = sitesList[-siteOut],
                      CC = CC[-siteOut])
  
  out$DT <- JagsOutput2list(out)
  
  out
}