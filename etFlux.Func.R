library(truncnorm)
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
predictET <- function(df, 
                      samples=NULL,
                      abs, eSky, eSur, Cconv, Aconv, sigma){
  Sigma <- 5.670367 * 10^-8
  if(!is.null(samples)) {
    abs <- samples$abs
    eSky <- samples$eSky
    eSur <- samples$eSur
    Cconv <- samples$Cconv
    Aconv <- samples$Aconv
    sigma <- samples$sigma
    
  }
  abs <- t(apply(abs, 1:2, mean))
  eSky <- t(apply(eSky, 1:2, mean))
  eSur <- t(apply(eSur, 1:2, mean))
  Cconv <- t(apply(Cconv, 1:2, mean))
  Aconv <- t(apply(Aconv, 1:2, mean))
  sigma <- t(apply(sigma, 1:2, mean))
    
  Snet <- abs %*% df$RS
  THi <- Sigma*eSky %*% (df$TA + 273.15)^4
  THo <- - Sigma * eSur %*% (df$LST + 273.15)^4
  H <- - sweep(sweep( exp(Aconv%*%log(df$WS)), 1, c(Cconv), '*'), 2, (df$LST - df$TA),'*')
  LE <- -(Snet + THi + THo + H)
  Lambda <- 2502 - 2.308*df$TA
  mu <- sweep(-LE, 2, c(Lambda), '/')*24*3.6
  ET <- apply(mu, 2, rtruncnorm, n=length(sigma), a = 0, b = Inf, sd = (sigma))
  ET
}


