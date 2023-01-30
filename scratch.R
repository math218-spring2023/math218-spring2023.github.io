library(class)
mite_dat
set.seed(1)
test_ids <- sample(1:nrow(mite_dat), 20)

lm_mod <- lm(abundance ~ .  - Substrate,mite_dat[-test_ids,])
summary(lm_mod)
lm_preds <- predict(lm_mod, mite_dat[test_ids,])
lm_mse <- mean((mite_dat$abundance[test_ids] - lm_preds)^2)



mite_dat2 <- mite_dat %>%
  mutate(present = ifelse(abundance > 0, 1, 0)) %>%
  dplyr::select(-abundance)
glm_mod <- glm(present ~ .  - Substrate,mite_dat2, family = "binomial")
summary(glm_mod)

