library(vegan)
cols <- c("#F9C51F", "#FC766AFF", "#9B4A97FF")
data(mite)
data(mite.env)
mite_dat <- mite.env %>%
  add_column(abundance = mite$LRUG) %>%
  mutate(present = ifelse(abundance > 0 , T, F))

mite_dat %>%
  mutate(present = factor(present, levels = c(T, F))) %>%
  pivot_longer(cols = 1:2, names_to = "variable", values_to = "value") %>%
  ggplot(., aes(x = present, y = value)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  # plot_theme +
  ylab("LRUG abundance")

mite_dat %>%
  mutate(present = factor(present, levels = c(T, F))) %>%
  mutate(Substrate = as.character(Substrate),
         Shrub = as.character(Shrub)) %>%
  pivot_longer(cols = 3:5, names_to = "variable", values_to = "value") %>%
  ggplot(., aes(fill = present, x = value)) +
  geom_bar(position = "fill") +
  facet_wrap(~variable, scales = "free_x", nrow = 1,  strip.position = "bottom") +
  theme(panel.spacing = unit(0, "lines"),
        panel.border = element_rect(fill = NA),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        strip.placement = "outside",
        axis.text.x = element_text(angle = 90))  +
  scale_fill_manual(values = cols[1:2])

mite_dat2 <- mite_dat %>%
  dplyr::select(-abundance)
mod1 <- glm(present ~ ., data = mite_dat2, family = "binomial")

mod2 <- glm(present ~ . - Substrate, data = mite_dat2, family = "binomial")

summary(mod1)
summary(mod2)
