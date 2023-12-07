
library(Matrix)
library(readr)
library(tidyverse)
library(lubridate)
library(janitor)
library(readxl)
library(ggplot2)
library(ggbreak)
library(brms)
library(tidybayes)


#### Movement Models ####

# flow change over 24 hours
flow_model_binom <- brm(move_no_move ~ change_flow_24_s,
                        family = bernoulli(link="logit"),
                        prior = c(prior(normal(-1, 1), class = "Intercept"),
                                  prior(normal(0, 1), class = "b")),
                        data = max_movement,
                        file="models/binom1.rds",
                        file_refit = "on_change",
                        chains=4, iter=2000)

plot(conditional_effects(flow_model_binom),points = T)
pp_check(flow_model_binom)
bayes_R2(flow_model_binom)
summary(flow_model_binom)

# flow change over 24 hours plus flow
flow_model_binom2 <- brm(move_no_move ~ change_flow_24_s + Flow,
                         family = bernoulli(link="logit"),
                         prior = c(prior(normal(-1, 1), class = "Intercept"),
                                   prior(normal(0, 1), class = "b")),
                         data = max_movement,
                         file="models/binom2.rds",
                         file_refit = "on_change",
                         chains=4, iter=2000)

plot(conditional_effects(flow_model_binom2),points = T)
pp_check(flow_model_binom2)
bayes_R2(flow_model_binom2)
summary(flow_model_binom2)

# flow change over 24 hours plus flow plus mean weekly temp
flow_model_binom3 <- brm(move_no_move ~ change_flow_24_s + Flow + mean_temp,
                         family = bernoulli(link="logit"),
                         prior = c(prior(normal(-1, 1), class = "Intercept"),
                                   prior(normal(0, 1), class = "b")),
                         data = max_movement,
                         file="models/binom3.rds",
                         file_refit = "on_change",
                         chains=4, iter=2000)

plot(conditional_effects(flow_model_binom3),points = T)
pp_check(flow_model_binom3)
bayes_R2(flow_model_binom3)
summary(flow_model_binom3)

# flow change over 24 hours plus flow plus mean weekly DO
flow_model_binom4 <- brm(move_no_move ~ change_flow_24_s + Flow + mean_do,
                         family = bernoulli(link="logit"),
                         prior = c(prior(normal(-1, 1), class = "Intercept"),
                                   prior(normal(0, 1), class = "b")),
                         data = max_movement,
                         file="models/binom4.rds",
                         file_refit = "on_change",
                         chains=4, iter=2000)

plot(conditional_effects(flow_model_binom4),points = T)
pp_check(flow_model_binom4)
bayes_R2(flow_model_binom4)
summary(flow_model_binom4)

# flow change over 24 hours plus flow plus mean weekly temp plus mean weekly DO
get_prior(data= max_movement, move_no_move ~ change_flow_24_s + Flow + mean_temp + mean_do)
flow_model_binom5 <- brm(move_no_move ~ change_flow_24_s + Flow + mean_temp + mean_do,
                         family = bernoulli(link="logit"),
                         prior = c(prior(normal(-1, 1), class = "Intercept"),
                                   prior(normal(0, 1), class = "b")),
                         data = max_movement,
                         file="models/binom5.rds",
                         file_refit = "on_change",
                         chains=4, iter=2000)

plot(conditional_effects(flow_model_binom5),points = T)
pp_check(flow_model_binom5)
bayes_R2(flow_model_binom5)
summary(flow_model_binom5)

# flow change over 48 hours
flow_model_binom6 <- brm(move_no_move ~ change_flow_48_s,
                         family = bernoulli(link="logit"),
                         prior = c(prior(normal(-1, 1), class = "Intercept"),
                                   prior(normal(0, 1), class = "b")),
                         data = max_movement,
                         file="models/binom6.rds",
                         file_refit = "on_change",
                         chains=4, iter=2000)

plot(conditional_effects(flow_model_binom6),points = T)
pp_check(flow_model_binom6)
bayes_R2(flow_model_binom6)
summary(flow_model_binom6)

# flow change over 48 hours plus flow
flow_model_binom7 <- brm(move_no_move ~ change_flow_48_s + Flow,
                         family = bernoulli(link="logit"),
                         prior = c(prior(normal(-1, 1), class = "Intercept"),
                                   prior(normal(0, 1), class = "b")),
                         data = max_movement,
                         file="models/binom7.rds",
                         file_refit = "on_change",
                         chains=4, iter=2000)

plot(conditional_effects(flow_model_binom7),points = T)
pp_check(flow_model_binom7)
bayes_R2(flow_model_binom7)
summary(flow_model_binom7)

# flow change over 48 hours plus flow plus mean weekly temp
flow_model_binom8 <- brm(move_no_move ~ change_flow_48_s + Flow + mean_temp,
                         family = bernoulli(link="logit"),
                         prior = c(prior(normal(-1, 1), class = "Intercept"),
                                   prior(normal(0, 1), class = "b")),
                         data = max_movement,
                         file="models/binom8.rds",
                         file_refit = "on_change",
                         chains=4, iter=2000)

plot(conditional_effects(flow_model_binom8),points = T)
pp_check(flow_model_binom8)
bayes_R2(flow_model_binom8)
summary(flow_model_binom8)

# flow change over 48 hours plus flow plus mean weekly DO
flow_model_binom9 <- brm(move_no_move ~ change_flow_48_s + Flow + mean_do,
                         family = bernoulli(link="logit"),
                         prior = c(prior(normal(-1, 1), class = "Intercept"),
                                   prior(normal(0, 1), class = "b")),
                         data = max_movement,
                         file="models/binom9.rds",
                         file_refit = "on_change",
                         chains=4, iter=2000)

plot(conditional_effects(flow_model_binom9),points = T)
pp_check(flow_model_binom9)
bayes_R2(flow_model_binom9)
summary(flow_model_binom9)

# flow change over 48 hours plus flow plus mean weekly temp plus mean weekly DO
flow_model_binom10 <- brm(move_no_move ~ change_flow_48_s + Flow + mean_temp + mean_do,
                          family = bernoulli(link="logit"),
                          prior = c(prior(normal(-1, 1), class = "Intercept"),
                                    prior(normal(0, 1), class = "b")),
                          data = max_movement,
                          file="models/binom10.rds",
                          file_refit = "on_change",
                          chains=4, iter=2000)

plot(conditional_effects(flow_model_binom10),points = T)
pp_check(flow_model_binom10)
bayes_R2(flow_model_binom10)
summary(flow_model_binom10)

# just flow
flow_model_binom11 <- brm(move_no_move ~ Flow,
                          family = bernoulli(link="logit"),
                          prior = c(prior(normal(-1, 1), class = "Intercept"),
                                    prior(normal(0, 1), class = "b")),
                          data = max_movement,
                          file="models/binom11.rds",
                          file_refit = "on_change",
                          chains=4, iter=2000)

plot(conditional_effects(flow_model_binom11),points = T)
pp_check(flow_model_binom11)
bayes_R2(flow_model_binom11)
summary(flow_model_binom11)

# flow plus average weekly temp
flow_model_binom12 <- brm(move_no_move ~ Flow + mean_temp,
                          family = bernoulli(link="logit"),
                          prior = c(prior(normal(-1, 1), class = "Intercept"),
                                    prior(normal(0, 1), class = "b")),
                          data = max_movement,
                          file="models/binom12.rds",
                          file_refit = "on_change",
                          chains=4, iter=2000)

plot(conditional_effects(flow_model_binom12),points = T)
pp_check(flow_model_binom12)
bayes_R2(flow_model_binom12)
summary(flow_model_binom12)

# flow plus average weekly DO
flow_model_binom13 <- brm(move_no_move ~ Flow + mean_do,
                          family = bernoulli(link="logit"),
                          prior = c(prior(normal(-1, 1), class = "Intercept"),
                                    prior(normal(0, 1), class = "b")),
                          data = max_movement,
                          file="models/binom13.rds",
                          file_refit = "on_change",
                          chains=4, iter=2000)

plot(conditional_effects(flow_model_binom13),points = T)
pp_check(flow_model_binom13)
bayes_R2(flow_model_binom13)
summary(flow_model_binom13)

# flow plus average weekly DO and average weekly temperature
flow_model_binom14 <- brm(move_no_move ~ Flow + mean_do + mean_temp,
                          family = bernoulli(link="logit"),
                          prior = c(prior(normal(-1, 1), class = "Intercept"),
                                    prior(normal(0, 1), class = "b")),
                          data = max_movement,
                          file="models/binom14.rds",
                          file_refit = "on_change",
                          chains=4, iter=2000)

plot(conditional_effects(flow_model_binom14),points = T)
pp_check(flow_model_binom14)
bayes_R2(flow_model_binom14)
summary(flow_model_binom14)

# just mean weekly temperature
flow_model_binom15 <- brm(move_no_move ~ mean_temp,
                          family = bernoulli(link="logit"),
                          prior = c(prior(normal(-1, 1), class = "Intercept"),
                                    prior(normal(0, 1), class = "b")),
                          data = max_movement,
                          file="models/binom15.rds",
                          file_refit = "on_change",
                          chains=4, iter=2000)

plot(conditional_effects(flow_model_binom15),points = T)
pp_check(flow_model_binom15)
bayes_R2(flow_model_binom15)
summary(flow_model_binom15)

# just mean weekly DO
flow_model_binom16 <- brm(move_no_move ~ mean_do,
                          family = bernoulli(link="logit"),
                          prior = c(prior(normal(-1, 1), class = "Intercept"),
                                    prior(normal(0, 1), class = "b")),
                          data = max_movement,
                          file="models/binom16.rds",
                          file_refit = "on_change",
                          chains=4, iter=2000)

plot(conditional_effects(flow_model_binom16),points = T)
pp_check(flow_model_binom16)
bayes_R2(flow_model_binom16)
summary(flow_model_binom16)

# mean weekly temperature and DO
flow_model_binom17 <- brm(move_no_move ~ mean_temp + mean_do,
                          family = bernoulli(link="logit"),
                          prior = c(prior(normal(-1, 1), class = "Intercept"),
                                    prior(normal(0, 1), class = "b")),
                          data = max_movement,
                          file="models/binom17.rds",
                          file_refit = "on_change",
                          chains=4, iter=2000)

plot(conditional_effects(flow_model_binom17),points = T)
pp_check(flow_model_binom17)
bayes_R2(flow_model_binom17)
summary(flow_model_binom17)

waic(flow_model_binom,
     flow_model_binom2,
     flow_model_binom3,
     flow_model_binom4,
     flow_model_binom5,
     flow_model_binom6,
     flow_model_binom7,
     flow_model_binom8,
     flow_model_binom9,
     flow_model_binom10,
     flow_model_binom11,
     flow_model_binom12,
     flow_model_binom13,
     flow_model_binom14,
     flow_model_binom15,
     flow_model_binom16,
     flow_model_binom17)

### UPDATE: Model 11 is the best ###
# model 11 ONLY instantaneous discharge (flow)

max_movement <- read_csv("data/max_movement.csv") %>% 
  mutate(m3s=Flow*0.028316832)

binom11_conds <- tibble(Flow = seq(min(flow_model_binom11$data$Flow), max(flow_model_binom11$data$Flow))) %>% add_epred_draws(flow_model_binom11)

sum(max_movement$total_movement>50)


binom11_medians <- binom11_conds %>% 
  group_by(Flow) %>% 
  median_qi(.epred)

binom11_medians_metric <- binom11_medians %>% 
  mutate(m3s=binom11_medians$Flow*0.028316832)

Mean_output <- binom11_medians_metric %>% 
  ggplot(aes(x = m3s, y = .epred)) + 
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) + 
  geom_point(data = max_movement, aes(y = move_no_move))+
  labs(y="Predicted Probability of Movement")

ggsave(Mean_output,file="plots/MeanPosteriors.jpg", dpi = 750, width = 3, height = 3,
       units = "in")


mean_posterior_with_distances <- binom11_medians_metric %>% 
  ggplot(aes(x = m3s, y = .epred)) + 
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) + 
  geom_point(data = max_movement, aes(y = move_no_move, size = total_movement))+
  labs(y="Probability of Movement",
       x="Flow in m^3/s")+
  scale_x_log10()


ggsave(mean_posterior_with_distances,file="plots/MeanPosteriorsDistanceBubbles.jpg", dpi = 750, width = 7, height = 4,
       units = "in")

# model 11 sensitivity analysis: 
flow_model_binom11s <- brm(move_no_move ~ Flow,
                           family = bernoulli(link="logit"),
                           prior = c(prior(normal(-1, 2), class = "Intercept"),
                                     prior(normal(0, 2), class = "b")),
                           data = max_movement,
                           file="models/binom11sensitivity.rds",
                           file_refit = "on_change",
                           chains=4, iter=2000)

plot(conditional_effects(flow_model_binom11s),points = T)
pp_check(flow_model_binom11s)
bayes_R2(flow_model_binom11s)
summary(flow_model_binom11s)
