data.frame(x = list(1:3, 4:6))

data.frame(x = I(list(1:3, 3:5)),
           y = c("1,2,3", "3,4,5"))

tibble(x = list(1:3, 3:5),
           y = c("1,2,3", "3,4,5"))

tribble(
  ~x, ~y,
  1:3, "1,2,3",
  4:6, "4,5,6"
)

# Crear columna de listas con nest(), summarise() + list()
# mutate() + map() o map2() o pmap()
# ................
# unnest()
# tibble::enframe()

gapminder %>%
  group_by(country, continent) %>%
  nest()

gapminder %>%
  nest(year:gdpPercap)

df <- tribble(
  ~x,
  "a,b,c",
  "f,g,h",
  "w,x,y,z"
  )
df %>% 
  mutate(y = str_split(x, ",")) %>%
  unnest()

#tidyr::separate_rows()

sim <- tribble(
  ~f,    ~params,
  "runif", list(min = -1, max = 1),
  "rnorm", list(sd = 3),
  "rpois", list(lambda = 5)
)

sim %>%
  mutate(sims = invoke_map(f, params, n = 10))

probs <- c(0.01, 0.25, 0.5, 0.75, 0.99)

mtcars %>%
  group_by(cyl) %>%
  summarise(p = list(probs), 
            q = list(quantile(mpg, probs))) %>%
  unnest()

x <- list(
  a = 1:6,
  b = 3:4,
  c = 5:8
)
df <- enframe(x)
df

df %>%
  mutate(
    smry = map2_chr(name, value, 
                    ~ stringr::str_c(.x, ": ", .y[1]))
  )

df <- tribble(
  ~x,
  letters[1:8],
  2:9,
  runif(6)
)

df %>%
  mutate(
    type = map_chr(x, typeof),
    length = map_int(x, length)
  )


df <- tribble(
  ~x, 
  list(a=1, b=2),
  list(a=2, c=4),
  list(a=3, c=8)
)
df %>%
  mutate(
    a = map_dbl(x, "a"),
    b = map_dbl(x, "b", .null = NA_real_),
    c = map_dbl(x, "c", .null = NA_real_)
  )

tibble(x = 1:2,
       y = list(1:8, 1)) %>% unnest(y)

df1 <- tribble(
  ~x,   ~y,       ~z,
  1, c("x", "y"),  1:2,
  2, "m",           3
)
df1 %>% unnest(y)
df1 %>% unnest(z)

df1 %>% unnest(y,z)

df2 <- tribble(
  ~x,   ~y,       ~z,
  1, c("x", "y", "z"),  1:2,
  2, "m",               3:5
)
df2 %>% unnest(y)
df2 %>% unnest(z)
df2 %>% unnest(y, z)

# BROOM
# glance(model)
# tidy(model)
# augment(model, data)

