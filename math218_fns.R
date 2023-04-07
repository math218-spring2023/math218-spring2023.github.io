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

.knn_class <- function(K, train_x, train_y, test_x){
  n_test <- nrow(test_x)
  levs <- levels(train_y)
  if(length(n_test) == 0){
    n_test <- 1
  }
  y_pred <- rep(NA, n_test)
  for(i in 1:n_test){
    # get distances
    d_vec <- .dist(test_x[i,], train_x)
    
    # get neighbors
    nb_ids <- .get_nbs(K, d_vec)
    
    # create data frame that contains the proportion of each label in neighbor set
    label_df <- data.frame(label = train_y[nb_ids]) %>%
      group_by(label) %>% 
      summarise(n = n()) %>%
      mutate(prop = n / sum(n)) # not necessary, but maybe useful
    
    ### OPTION 1 (probably more confusing, but doesn't use any new functions)
    
    ## find index (i.e. row) of label with highest proportion 
    # max_label <- which(label_df$n == max(label_df$n))
    # 
    ## if there is only one index --> no ties
    # if(length(max_label) == 1){
    #   y_pred[i] <- label_df$label[max_label]
    # } else{ ## otherwise, we will need to randomly choose one of the tied labels
    #   rand_id <- sample(max_label, 1)
    #   y_pred[i] <- label_df$label[rand_id]
    # }
    
    ### OPTION 2 (cleaner)
    max_df <- label_df %>%
      mutate(max_prop = max(prop)) %>%
      filter(prop == max_prop) 
    rand_id <- sample(1:nrow(max_df), 1)
    y_pred[i] <- max_df$label[rand_id]
    
  }
  return(levs[y_pred])
}