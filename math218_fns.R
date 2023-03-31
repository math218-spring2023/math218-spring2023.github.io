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

my_scale <- function(df, mean_vec, sd_vec){
  ret <- df
  for(i in 1:ncol(df)){
    ret[,i] <- (df[,i] - mean_vec[i])/sd_vec[i]
  }
  return(ret)
}

.get_nbs <- function(K, d_vec){
  nb_ids <- data.frame(d = d_vec) %>%
    mutate(id = row_number()) %>%
    arrange(d) %>%
    slice(1:K) %>%
    pull(id)
  return(nb_ids)
}