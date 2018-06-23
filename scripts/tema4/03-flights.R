library(tidyverse)
library(modelr)
library(nycflights13)
library(lubridate)

# Qué afecta al número de vuelos de un día? -------------------------------

daily <- flights %>%
  mutate(date = make_date(year, month, day))%>%
  group_by(date) %>%
  summarise(n = n())
daily

daily %>%
  ggplot(aes(date, n)) + geom_line()

daily %>%
  mutate(wday = wday(date, label = TRUE)) -> daily

daily %>%
  ggplot(aes(wday, n)) + 
  geom_boxplot()

mod <- lm(n ~ wday, data = daily)

grid <- daily %>%
  data_grid(wday) %>%
  add_predictions(mod, "n")

daily %>% 
  ggplot(aes(wday, n)) + 
  geom_boxplot() + 
  geom_point(data = grid, color = "red", size = 4)

daily <- daily %>%
  add_residuals(mod)

daily %>%
  ggplot(aes(date, resid, color = wday)) + 
  geom_ref_line(h = 0) + 
  geom_line()

daily %>%
  filter(abs(resid)>100)

daily %>%
  ggplot(aes(date, resid)) + 
  geom_ref_line(h = 0) + 
  geom_line(color = "grey30") + 
  geom_smooth(se = TRUE, span = 0.2)


daily %>%
  filter(wday == "sáb") %>%
  ggplot(aes(date, n)) + 
  geom_point() + 
  geom_line() + 
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

# 26 Junio - 9 Septiembre
# 5 Junio - 25 Agosto

term <- function(date) {
  cut(date,
      breaks = ymd(20130101, 20130605, 20130825, 20140101),
      labels = c("spring", "summer", "fall")
  )
}

daily <- daily %>%
  mutate(term = term(date))

daily %>%
  filter(wday == "sáb") %>%
  ggplot(aes(date, n, color = term))+
  geom_point(alpha = 0.25) + 
  geom_line()+
  scale_x_date(NULL, date_breaks = "1 month", date_labels = "%b")

daily %>%
  ggplot(aes(wday, n, color = term)) + 
  geom_boxplot()


mod1 <- lm(n ~ wday, data = daily)
mod2 <- lm(n ~ wday*term, data = daily)

daily %>%
  gather_residuals(without_term = mod1, with_term = mod2) %>%
  ggplot(aes(date, resid, color = model)) + 
  geom_line(alpha = 0.7)

grid <- daily %>%
  data_grid(wday, term) %>%
  add_predictions(mod2, "n")

daily %>%
  ggplot(aes(wday, n)) + 
  geom_boxplot()+
  geom_point(data = grid, color = "red")+
  facet_wrap(~term)


mod3 <- MASS::rlm(n ~ wday * term, data = daily)

daily %>%
  add_residuals(mod3, "resid") %>%
  ggplot(aes(date, resid)) +
  geom_hline(yintercept = 0, size = 2, color = "white") + 
  geom_line()
  

compute_vars <- function(data){
  data %>%
    mutate(
      term = term(date), 
      wday = wday(date, label = TRUE)
    )
}

wday2 <- function(x) wday(x, label = TRUE)  
mod4 <- lm(n ~ wday2(date) * term(date), data = daily)

library(splines)
mod <- MASS::rlm(n ~ wday * ns(date, 5), data = daily)

daily %>%
  data_grid(wday, date = seq_range(date, n = 13)) %>%
  add_predictions(mod) %>%
  ggplot(aes(date, pred, color = wday)) + 
  geom_line()+
  geom_point()


