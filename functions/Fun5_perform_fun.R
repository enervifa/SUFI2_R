
Pbias <- function(x,y) {
  100*sum(x - y, na.rm=T)/(sum(x,na.rm=T))
}

NSE <- function(x,y) {
  1 - sum((x - y)^2, na.rm = T)/(sum((x - mean(x, na.rm = T))^2,
                                     na.rm = T))
}

Rsquared <- function(x,y) {
  cor(x,y, use = "complete.obs")^2
}

KGE <- function(x,y) {
  1 - sqrt((cor(x,y, use = "complete.obs") - 1)^2 +
             (sd(x, na.rm = T)/sd(y, na.rm = T) - 1)^2 +
             (mean(x, na.rm = T)/mean(y, na.rm = T) - 1)^2)
}

Vol_frac <- function(x,y) {
  sum(x,na.rm = T)/sum(y,na.rm = T)
} 

MSE <- function(x,y) {
  mean(sum(x - y, na.rm = T))
}

SSQR <- function(x,y) {
  1/(length(x))*sum((x - y)^2, na.rm = T)
}

bR2 <- function(x, y) {
  pb <- Pbias(x,y)
  if (abs(pb) <= 1) {
    out <- abs(pb)*Rsquared(x,y)
  } else {
    out <- abs(pb)^(-1)*Rsquared(x,y)
  }
}

