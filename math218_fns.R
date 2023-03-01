.dist <- function(x0, x){
  n <- nrow(x)
  dists <- rep(NA, n)
  for(i in 1:n){
    dists[i] <- sqrt(sum((x0 - x[i,])^2))
  }
  dists
}

get_rmse <- function(y_pred, true){
  sqrt(mean((y_pred-true)^2))
}

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}