# Família de modelos que expresa el patrón / relación a estudiar
# y = a_1 * x + a_0 -> relación lineal
# y = a_2 * x^2 + a_1 * x + a_0 -> relación cuadrática

# Ajustar el modelo: buscar los parámetros del modelo que hayamos decidido
# y = 6x-8
# y = 2x^2-5x+8

library(tidyverse)
library(modelr)
options(na.action = na.warn)

# Linear models -------------------------------------------------------

sim1 %>% View()
sim1 %>% 
  ggplot(aes(x,y)) +
  geom_point()
# y = a_0  + a_1 * x 
models <- tibble(
  a0 = runif(300, -20, 40),
  a1 = runif(300, -5,   5)
)

sim1 %>% 
  ggplot(aes(x,y)) +
  geom_abline(aes(intercept = a0, slope = a1), data = models, alpha = 0.2) +
  geom_point()

model1 <- function(a0, a1, data){
  a0 + data$x * a1
}

model1(3, 1.2, sim1)

rmse <- function(mod, data) {
  diff <- data$y - model1(mod[1], mod[2], data)
  sqrt(mean(diff^2))
}

rmse(c(3,1.2), sim1)

sim1_rmse <- function(a0, a1){
  rmse(c(a0, a1), sim1)
}

models <- models %>%
  mutate(rmse = purrr::map2_dbl(a0, a1, sim1_rmse))

models %>% View()

sim1 %>%
  ggplot(aes(x,y)) + 
  geom_point(size = 2, color = "grey30") + 
  geom_abline(aes(intercept = a0, slope = a1, color = -rmse),
              data = filter(models, rank(rmse) <= 10))

models %>%
  filter(rank(rmse) <= 10)

models %>%
  ggplot(aes(a0, a1))+
  geom_point(data = filter(models, rank(rmse) <= 10), size = 4, color = "red")+
  geom_point(aes(color = -rmse))

grid <- expand.grid(
  a0 = seq(3,5, length = 25),
  a1 = seq(1.9, 2.2, length = 25)
) %>%
  mutate(rmse = purrr::map2_dbl(a0, a1, sim1_rmse))

grid %>%
  ggplot(aes(a0, a1)) +
  geom_point(data = filter(grid, rank(rmse)<=10), size = 4, color = "red")+
  geom_point(aes(color = -rmse))

sim1 %>%
  ggplot(aes(x,y)) + 
  geom_point(size = 2, color = "grey30")+
  geom_abline(
    aes(intercept = a0, slope = a1, color = -rmse),
    data = filter(grid, rank(rmse)<=10)
  )

best <- optim(c(0,0), rmse, data = sim1)
best$par

sim1 %>%
  ggplot(aes(x,y)) + 
  geom_point(size = 2, color = "grey30") + 
  geom_abline(intercept = best$par[1], slope = best$par[2])

# y = a0 + a1*x1 + a2*x2 + a3*x3 + ... + an*xn
lm(y~x, data = sim1) -> sim1_mod
coef(sim1_mod)
summary(sim1_mod)

grid <- sim1 %>%
  data_grid(x)
grid

grid <- grid %>%
  add_predictions(sim1_mod)
grid

sim1 %>%
  ggplot(aes(x)) + 
  geom_point(aes(y = y)) + 
  geom_line(aes(y = pred), data = grid, color = "red", size = 1)


sim1 <- sim1 %>%
  add_residuals(sim1_mod)
sim1


sim1 %>%
  ggplot(aes(resid)) + 
  geom_freqpoly(binwidth = 0.5)

sim1 %>%
  ggplot(aes(x, resid)) + 
  geom_ref_line(h = 0)+
  geom_point()
  
# y ~ x <-> y = a0 + a1 * x <-> y = a0 * out0 + a1 * out1

df <- tribble(
  ~y, ~x1, ~x2,
   4,   2,   5,
   5,   1,   6
)
model_matrix(df, y ~ x1)
model_matrix(df, y ~ x1 - 1)
model_matrix(df, y ~ x1 + x2)

# Categorical models -------------------------------------------------------

# y ~ sex <-> y = a0 + a1*sexmale (sexmale = 0, 1)
df <- tribble(
  ~sex, ~value,
  "male",    1,
  "female",  5, 
  "male",    1
)
model_matrix(df, value ~sex)
# sexfemale = 1 - sexmale

sim2 %>%
  ggplot(aes(x,y)) + 
  geom_point()

mod2 <- lm(y~x, data = sim2)

grid <- sim2 %>%
  data_grid(x) %>%
  add_predictions(mod2)
grid

sim2 %>%
  ggplot(aes(x)) + 
  geom_point(aes(y = y)) + 
  geom_point(data = grid, aes(y = pred), color = "red", size = 4)

tibble(x = "e") %>%
  add_predictions(mod2)

# C-D models -------------------------------------------------------

sim3 %>%
  ggplot(aes(x1, y)) + 
  geom_point(aes(color = x2))

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)
# y = a0 + (a1 * x1 + a2 * x2) + [a12 * x1 * x2]

grid <- sim3 %>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2)
grid%>%View()

sim3 %>%
  data_grid(x1, x2) %>%
  spread_predictions(mod1, mod2)


sim3 %>%
  ggplot(aes(x1, y, color = x2)) + 
  geom_point() + 
  geom_line(data = grid, aes(y = pred)) + 
  facet_wrap(~model)


sim3 %>% 
  gather_residuals(mod1, mod2) %>%
  ggplot(aes(x1, resid, color = x2))+ 
  geom_point() +
  facet_grid(model ~ x2)

# C-C models -------------------------------------------------------

mod1 <- lm(y ~ x1 + x2, data = sim4)
mod2 <- lm(y ~ x1 * x2, data = sim4)

grid <- sim4 %>%
  data_grid(
    x1 = seq_range(x1, 5),
    x2 = seq_range(x2, 5)
  ) %>% 
  gather_predictions(mod1, mod2)

grid

seq_range(c(0.23675, 0.98765), n = 6, pretty = TRUE)

x1 <- rcauchy(1000)
seq_range(x1, n = 10)
seq_range(x1, n = 10, trim = 0.1)
seq_range(x1, n = 10, trim = 0.25)
seq_range(x1, n = 10, trim = 0.50)

x2 <- c(0,1)
seq_range(x2, n = 10)
seq_range(x2, n = 10, expand = 0.1)
seq_range(x2, n = 10, expand = 0.25)
seq_range(x2, n = 10, expand = 0.5)


grid %>%
  ggplot(aes(x1,x2))+
  geom_tile(aes(fill = pred)) + 
  facet_wrap(~model)

grid %>%
  ggplot(aes(x1, pred, color = x2, group = x2)) + 
  geom_line()+
  facet_wrap(~model)

grid %>%
  ggplot(aes(x2, pred, color = x1, group = x1)) + 
  geom_line()+
  facet_wrap(~model)

# Transform models -------------------------------------------------------


# log(y) ~sqrt(x1) + x2 <-> log(y) = a0 + a1 * sqrt(x1) + a2 * x2
# y ~ x + I(x ^ 2) <-> y = a0 + a1 * x + a2 * x^2
# y ~ x + x ^ 2 <-> y ~ x + x * x = x <-> y = a0 + a1*x

df <- tribble(
  ~y, ~x, 
   1,  1,
   2,  2, 
   3,  3
)
model_matrix(df, y ~ x + x^2)
model_matrix(df, y ~ x + I(x^2))
# y = a0 + a1*x + a2*x^2 + a3*x^3 + ...
model_matrix(df, y ~ poly(x,2))

library(splines)
model_matrix(df, y ~ ns(x,2))

set.seed(2018)
sim5 <- tibble(
  x = seq(0, 3.5 *pi, length = 50),
  y = 4*sin(x) + rnorm(length(x))
)

sim5 %>%
  ggplot(aes(x,y)) +
  geom_point()

# Splines models -------------------------------------------------------

mod1 <- lm(y ~ ns(x,1), data = sim5)
mod2 <- lm(y ~ ns(x,2), data = sim5)
mod3 <- lm(y ~ ns(x,3), data = sim5)
mod4 <- lm(y ~ ns(x,4), data = sim5)
mod5 <- lm(y ~ ns(x,5), data = sim5)
mod6 <- lm(y ~ ns(x,6), data = sim5)

grid <- sim5 %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.1)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, mod6, .pred = "y")

sim5 %>%
  ggplot(aes(x,y)) + 
  geom_point() + 
  geom_line(data = grid, color = "red") + 
  facet_wrap(~model)


# Polinomial models -------------------------------------------------------

mod1 <- lm(y ~ poly(x,1), data = sim5)
mod2 <- lm(y ~ poly(x,2), data = sim5)
mod3 <- lm(y ~ poly(x,3), data = sim5)
mod4 <- lm(y ~ poly(x,4), data = sim5)
mod5 <- lm(y ~ poly(x,5), data = sim5)
mod6 <- lm(y ~ poly(x,6), data = sim5)

grid <- sim5 %>%
  data_grid(x = seq_range(x, n = 50, expand = 0.5)) %>%
  gather_predictions(mod1, mod2, mod3, mod4, mod5, mod6, .pred = "y")

sim5 %>%
  ggplot(aes(x,y)) + 
  geom_point() + 
  geom_line(data = grid, color = "red") + 
  facet_wrap(~model)

# Other models -------------------------------------------------------

df <- tribble(
  ~x, ~y,
   1, 1.5,
   2,  NA, 
   3, 3.5,
   4, 7.5,
  NA,  15
)
mod <- lm(y~x, data = df, na.action = na.exclude)

nobs(mod)
# Modelo lineal
# y = a0 + a1*x1 + a2*x2 + ... + an*xn
# Modelo lineal generalizado
stats::glm()
# Modelo generalizado additivo
mgcv::gam() # y~s(x) <-> y = f(x)
# Modelo lineal penalizado
glmnet::glmnet()
# Modelo lineal robusto
MASS::rlm()
# Árboles y bosques aleatorios
rpart::rpart()
randomForest::randomForest()
xgboost::xgboost()

