library(tidyverse)
library(vegan)
library(tree)
data(mite)
data(mite.env)
mite_dat <- mite.env %>%
  add_column(abundance = mite$LRUG)

## BAGGING BY HAND: test/train split
set.seed(123) # 18
train_ids <- sample(1:n, 0.8*n)
test_ids <- (1:n)[-train_ids]
n_train <- length(train_ids)
n_test <- length(test_ids)
pred_test <- rep(0, n_test)
B <- 10
for (b in 1:B){
  samp_ids <- sample(train_ids, n_train, replace = T)
  boot_samp <- mite_dat[samp_ids,]
  bag_mod <- tree(abundance ~ ., data = boot_samp)
  preds <- predict(bag_mod, newdata = mite_dat[test_ids,])
  pred_test <- pred_test + preds
}
my_bag_preds_test <- pred_test/B
sqrt(mean((my_bag_preds_test - mite_dat[test_ids,]$abundance)^2))

## BAGGING BY HAND: oob
pred_oob <- rep(0, n)
n_oob <- rep(0, n)
for (b in 1:B){
  samp_ids <- sample(1:n, n, replace = T)
  oob_ids <- which(!1:n %in% samp_ids)
  boot_samp <- mite_dat[samp_ids,]
  oob_samp <- mite_dat[oob_ids,]
  bag_mod <- tree(abundance ~ ., data = boot_samp)
  preds <- predict(bag_mod, newdata = oob_samp)
  pred_oob[oob_ids] <- pred_oob[oob_ids] + preds
  n_oob[oob_ids] <- n_oob[oob_ids] + 1
}

my_bag_preds_oob <- pred_oob/n_oob
sqrt(mean((my_bag_preds_oob - mite_dat$abundance)^2))

sqrt(mean(( na.omit(my_bag_preds_oob- mite_dat$abundance))^2))

## BAGGING USING randomForest()
library(randomForest)
bag_mod <- randomForest(abundance ~. , data = mite_dat,
                        mtry = 5,
                        ntree = B)
bag_preds <- bag_mod$predicted
sqrt(mean((bag_preds - mite_dat$abundance)^2))
sqrt(mean((na.omit(bag_preds - mite_dat$abundance))^2))

## question: why is it not simple to implement a random forest by hand 
## using only the tree() function?





