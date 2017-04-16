library(data.table)
library(truncnorm)
library(rjags)

simulateET <- function(df, abs, eSky, eSur, Cconv, Aconv, sigma){
  Sigma <- 5.670367 * 10^-8
  Snet <- abs * df$SOL
  THi <- Sigma*eSky * (df$TA + 273.15)^4
  THo <- - Sigma * eSur * (df$LST + 273.15)^4
  H <- -(Cconv * df$WS^Aconv) * (df$LST - df$TA)
  LE <- -(Snet + THi + THo + H)
  Lambda <- 2502 - 2.308*df$TA
  mu <- -LE*24*3.6/Lambda
  ET <- rtruncnorm(n = length(mu), a = 0, sd = sigma, mean = mu)
  ET
}

predictET <- function(df, samples=NULL, useWindData = T){
  Sigma <- 5.670367 * 10^-8
  
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
  LE <- -(Snet + THi + THo + H)
  Lambda <- 2502 - 2.308*df$TA
  mu <- sweep(-LE, 2, c(Lambda), '/')*24*3.6
  ET <- apply(mu, 2, rtruncnorm, n=length(sigma), a = 0, b = Inf, sd = (sigma))
  ET
}



etFlux.Model <- function(ameriLST, 
                         nburnin = 1000,
                         ngibbs = 1000,
                         n.chains = 1,
                         quiet = F,
                         useWindData = T,
                         perSite = T,
                         SitesList){
  bugsCode <- switch(useWindData+1, 'etFluxNoWind.bugs', 'etFlux.bugs')
  
  df <- ameriLST[Site%in%SitesList]
  SitesList <- unique(df$Site)
  df[,SitesID:=as.numeric(as.factor(as.character(Site)))]
  
  dfSites <- df$SitesID
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
                     'ns' = ns,
                     'n' = nrow(df))
    variableNames <- c('abs',
                       'eSur', 'eSky',
                       'Hconv',
                       'sigma',
                       'ETpred', 'ETobs',
                       'Snet','THi','THo','H','LE')
    
  }else{
    bugsCode <- 'etFlux.bugs'
    dataList <- list('Si' = df$RS,
                     'Tair' = df$TA,
                     'Tsur' = df$LST,
                     'WS' = df$WS,
                     'ET' = df$ET,
                     'Sites' = dfSites,
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
  if(!useWindData)SitesList <- 'general'
  
  output <- list(samples = samples, sitesList = SitesList)
  output$DT <- JagsOutput2list(output)
  output
}


JagsOutput2list <- function(output){
  DT <- list()
  nameList <- names(output$samples)
  for(i in 1:length(nameList)){
    mc <- output$samples[[nameList[i]]]
    if(dim(mc)[3]>1){
      mc <- t(apply(mc, 1:2, mean))
    }else{
      dim(mc) <- dim(mc)[-3]
      mc <- apply(t(mc), 1:2, function(x)x)
    }
    
    if(ncol(mc)==length(output$sitesList)){
      mc <- as.data.table(mc)
      colnames(mc) <- output$sitesList
    }else{
      mc <- as.data.table(t(apply(mc, 2, quantile, probs=c(.5, .025, .975))))
    }
    DT[[nameList[i]]] <- mc
  }
  DT
}