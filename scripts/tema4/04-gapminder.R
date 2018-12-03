library(tidyverse)
library(modelr)
install.packages("gapminder")
library(gapminder)

?gapminder

gapminder %>%
  ggplot(aes(year, lifeExp, group = country))+
  geom_line(alpha = 0.2)


es <- filter(gapminder, country == "Spain")

es %>%
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data") +
  theme(plot.title = element_text(hjust = 0.5))

es_mod <- lm(lifeExp ~ year, data = es)
es %>%
  add_predictions(es_mod) %>%
  ggplot(aes(year, pred)) +
  geom_line() + 
  ggtitle("Linear Trend") +
  theme(plot.title = element_text(hjust = 0.5))

es %>%
  add_residuals(es_mod) %>%
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, color = "white", size = 3) + 
  geom_line() + 
  ggtitle("Residual pattern") +
  theme(plot.title = element_text(hjust = 0.5))


by_country <- gapminder %>%
  group_by(country, continent) %>%
  nest()
by_country %>% View()

#by_country %>% str()

by_country $data[[1]]


country_model <- function(df){
  lm(lifeExp ~ year, data = df)
}

models <- map(by_country$data, country_model)

by_country <- by_country %>%
  mutate(model = map(data, country_model))

by_country %>%
   filter(continent == "Europe")

by_country %>%
  arrange(continent, country)

by_country <- by_country %>%
  mutate(resids = map2(data, model, add_residuals))

resids <- unnest(by_country, resids)

resids %>%
  ggplot(aes(year, resid)) + 
  geom_line(aes(group = country), alpha = 0.2) +
  geom_smooth(se = FALSE) +
  facet_wrap(~continent)

library(broom)
glance(es_mod)

glance <- by_country %>%
  mutate(glance = map(model, glance)) %>%
  unnest(glance, .drop = TRUE)

glance %>%
  arrange(r.squared)

glance %>%
  ggplot(aes(continent, r.squared))+
  geom_jitter(width = 0.5)

bad_fit <- filter(glance, r.squared < 0.25)
bad_fit

gapminder %>%
  semi_join(bad_fit, by = "country") %>%
  ggplot(aes(year, lifeExp, color = country)) + 
  geom_line()

glance(es_mod)
tidy(es_mod)
augment(es_mod, es)

