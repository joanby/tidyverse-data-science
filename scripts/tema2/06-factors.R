library(tidyverse)
library(forcats)

day_levels <- c("Lun", "Mar", "Mie", "Jue", "Vie", "Sab", "Dom")

x1 <- c("Vie", "Lun", "Mar", "Dom")
sort(x1)

y1 <- factor(x1, levels = day_levels)
sort(y1)

x2 <- c("Vim", "Lun", "Mar", "Dom")
y2 <- factor(x2, levels = day_levels)
y2
y2 <- parse_factor(x2, levels = day_levels)

factor(x1)

f1 <- factor(x1, levels = unique(x1))

f2 <- x1 %>% factor() %>% fct_inorder()

levels(f2)

gss_cat %>% View()

gss_cat %>% count(marital)

gss_cat %>% ggplot(aes(marital)) + 
  geom_bar()


gss_cat %>% ggplot(aes(race)) + 
  geom_bar() + 
  scale_x_discrete(drop = FALSE)

religion_summary <- gss_cat %>%
  group_by(relig) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

religion_summary %>% View()

ggplot(religion_summary, aes(tvhours, relig)) + geom_point()

ggplot(religion_summary, aes(tvhours, fct_reorder(relig, tvhours))) + 
  geom_point()

religion_summary %>%
  mutate(relig = fct_reorder(relig, tvhours)) %>%
  ggplot(aes(tvhours, relig)) + 
  geom_point()

gss_cat %>%
  group_by(rincome) %>%
  summarise(
    age = mean(age, na.rm = TRUE),
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  ) %>%
  #mutate(rincome = fct_reorder(rincome, age)) %>%
  mutate(rincome = fct_relevel(rincome, "Not applicable")) %>%
  ggplot(aes(age, rincome)) +
  geom_point()

by_age <- gss_cat %>%
  filter(!is.na(age)) %>%
  group_by(age, marital) %>%
  count()

ggplot(by_age, aes(age, n, color = marital)) + 
  geom_line(na.rm = TRUE)

ggplot(by_age, aes(age, n, color = fct_reorder2(marital, age, n)))+
  geom_line(na.rm = TRUE)+
  labs(color = "Marital")

gss_cat %>%
  mutate(marital = marital %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(marital)) + 
  geom_bar()

gss_cat %>% count(partyid)

gss_cat %>%
  mutate(partyid = fct_recode(partyid,
    "Republican, strong"      = "Strong republican",
    "Republican, weak"        = "Not str republican",
    "Independent, near rep"   = "Ind,near rep",
    "Independent, near dem"   = "Ind,near dem",
    "Democrat, weak"          = "Not str democrat",
    "Democrat, strong"        = "Strong democrat",
    "Other"                   = "No answer",
    "Other"                   = "Don't know",
    "Other"                   = "Other party"
  )
  ) %>% count(partyid)


gss_cat %>%
  mutate(partyid = fct_collapse(partyid,
          other = c("No answer", "Don't know", "Other party"),
          rep   = c("Strong republican", "Not str republican"),
          ind   = c("Ind,near rep", "Independent", "Ind,near dem"),
          dem   = c("Not str democrat", "Strong democrat")
        )
  ) %>%
  count(partyid)

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 5)) %>%
  count(relig, sort = TRUE) %>%
  print(n = 3)

