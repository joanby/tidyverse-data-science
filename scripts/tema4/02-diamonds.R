library(tidyverse)
library(modelr)
options(na.action = na.warn)

# Por qué los diamantes de baja calidad son más caros ---------------------

diamonds %>% ggplot(aes(cut, price)) + geom_boxplot()
diamonds %>% ggplot(aes(color, price)) + geom_boxplot()
diamonds %>% ggplot(aes(clarity, price)) + geom_boxplot()

diamonds %>% ggplot(aes(carat, price)) + 
  geom_hex(bins = 50)

diamonds2 <- diamonds %>%
  filter(carat <= 2.5) %>%
  mutate(lprice = log2(price), lcarat = log2(carat))

diamonds2 %>% 
  ggplot(aes(lcarat, lprice)) + 
  geom_hex(bins = 50)


mod_diamonds <- lm(lprice ~ lcarat, data = diamonds2)

grid <- diamonds2 %>%
  data_grid(carat = seq_range(carat, 30)) %>%
  mutate(lcarat = log2(carat)) %>%
  add_predictions(mod_diamonds, "lprice") %>%
  mutate(price = 2 ^ lprice)
grid

diamonds2 %>%
  ggplot(aes(carat, price)) +
  geom_hex(bins = 50) + 
  geom_line(data = grid, color = "red", size = 1)


diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamonds, "lresid")

diamonds2 %>%
  ggplot(aes(lcarat, lresid)) +
  geom_hex(bins = 50)



diamonds2 %>% ggplot(aes(cut, lresid)) + geom_boxplot()
diamonds2 %>% ggplot(aes(color, lresid)) + geom_boxplot()
diamonds2 %>% ggplot(aes(clarity, lresid)) + geom_boxplot()


mod_diamonds2 <- lm(lprice ~ lcarat + color + cut + clarity, 
                    data = diamonds2)

diamonds2 %>%
  data_grid(cut, .model = mod_diamonds2) %>%
  add_predictions(mod_diamonds2)%>%
  ggplot(aes(cut, pred)) +
  geom_point()

diamonds2 %>%
  data_grid(color, .model = mod_diamonds2) %>%
  add_predictions(mod_diamonds2)%>%
  ggplot(aes(color, pred)) +
  geom_point()

diamonds2 %>%
  data_grid(clarity, .model = mod_diamonds2) %>%
  add_predictions(mod_diamonds2)%>%
  ggplot(aes(clarity, pred)) +
  geom_point()



diamonds2 <- diamonds2 %>%
  add_residuals(mod_diamonds2, "lresid2")

ggplot(diamonds2, aes(lcarat, lresid2))+
  geom_hex(bins = 50)

diamonds2 %>%
  filter(abs(lresid2)>1) %>%
  add_predictions(mod_diamonds2) %>%
  mutate(pred = round(2^pred)) %>%
  select(price, pred, carat:table, x:z) %>%
  arrange(price)
