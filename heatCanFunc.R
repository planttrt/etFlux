library(truncnorm)
simulateET <- function(df, albedo, eSky, eSur, Cconv, Aconv, eta, sigma){
  Sigma <- 5.670367 * 10^-8
  Snet <- (1 - albedo) * df$SOL
  THi <- Sigma*eSky * (df$TA + 273.15)^4
  THo <- - Sigma * eSur * (df$LST + 273.15)^4
  H <- -(Cconv * df$WS^Aconv) * (df$LST - df$TA)
  LE <- -(Snet + THi + THo + H)
  Lambda <- 2502 - 2.308*df$TA
  mu <- -LE*eta*24*3.6/Lambda
  ET <- rtruncnorm(n = length(mu), a = 0, sd = sigma, mean = mu)
  ET
}
predictET <- function(df, 
                      samples=NULL,
                      albedo, eSky, eSur, Cconv, Aconv, eta, sigma){
  Sigma <- 5.670367 * 10^-8
  if(!is.null(samples)) {
    albedo <- samples$albedo
    eSky <- samples$eSky
    eSur <- samples$eSur
    Cconv <- samples$Cconv
    Aconv <- samples$Aconv
    eta <- samples$eta
    sigma <- samples$sigma
    
  }
  albedo <- t(apply(albedo, 1:2, mean))
  eSky <- t(apply(eSky, 1:2, mean))
  eSur <- t(apply(eSur, 1:2, mean))
  Cconv <- t(apply(Cconv, 1:2, mean))
  Aconv <- t(apply(Aconv, 1:2, mean))
  #Aconv <- mean(Aconv)
  eta <- apply(eta, 1:2, mean)
  sigma <- t(apply(sigma, 1:2, mean))
    
  Snet <- (1 - albedo) %*% df$SOL
  THi <- Sigma*eSky %*% (df$TA + 273.15)^4
  THo <- - Sigma * eSur %*% (df$LST + 273.15)^4
  H <- - sweep(sweep( exp(Aconv%*%log(df$WS)), 1, c(Cconv), '*'), 2, (df$LST - df$TA),'*')
  LE <- -(Snet + THi + THo + H)
  Lambda <- 2502 - 2.308*df$TA
  mu <- sweep(-LE, 1, c(eta), '*')*24*3.6
  mu <- sweep(mu, 2, c(Lambda), '/')
  ET <- apply(mu, 2, rtruncnorm, n=length(sigma), a = 0, b = Inf, sd = (sigma))
  
  ET
}


