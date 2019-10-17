vignette("tibble")
library(tidyverse)

View(iris)
class(iris)

iris_tibble <- as_tibble(iris)
class(iris_tibble)

t <- tibble(
  x = 1:10,
  y = pi,
  z = y * x ^ 2
)

View(t)
t[2,3]

t2 <- tibble(
  `:)` =  "smilie",
  ` ` = "space",
  `1988` = "number"
)

t2$`:)`

tribble(
  ~x, ~y, ~z,
#|---|--|------|
  "a", 4,  3.14,
  "b", 8,  6.28,
  "c", 9, -1.25
)


tibble(
  a = lubridate::now() + runif(1e3)*24*60*60,
  b = 1:1e3,
  c = lubridate::today() + runif(1e3)*30,
  d = runif(1e3), 
  e = sample(letters, 1e3, replace = T)
)

lubridate::today()

nycflights13::flights %>%
  print(n = 12, width = Inf)
  

options(tibble.print_max = 12, tibble.print_min = 12)
options(dplyr.print_min = Inf)
options(tibble.width = Inf)
  
nycflights13::flights %>% #....
  View()
  

# [['nombre_variable']]
# [[posicion_variable]]
# $nombre_variable

df <- tibble(
  x = rnorm(10),
  y = runif(10)
)

df$x
df$y

df %>% .$x
df %>% .$y

sapply(df, FUN = function(x) {
  x+1
  })

?sapply

df[["x"]]
df[["y"]]

df %>% .[["x"]]

df[[1]]
df[[2]]

df %>% .[[1]]


class(as.data.frame(df))

#[[]]
dplyr::filter()
dplyr::select()
#[[]] sobre un data.frame, puede devolver un data.frame o un vector
#[[]] sobre una tibble, siempre devuelve una tibble


# Ejercicio 1
mtcars
nycflights13::flights 

# Ejercicio 2
df <- data.frame(abc = 1, xyz = "a")
df$x
df[,"xyz"]
df[,c("abc","xyz")]

df2 <- tibble(abc = 1, xyz = "a")
df2$x
df2[,"xyz"]
df2[,c("abc","xyz")]

# Ejercicio 3
var <- "mpg"
mtcars[,var]
as_tibble(mtcars)[[var]]

#Ejercicio 4 
df <- tibble(
  `1` = 1:12,
  `2` = `1` * 2 + `1`*runif(length(`1`))
)
df$`1`

df %>% ggplot(mapping = aes(x = `1`, y = `2`)) + 
  geom_point()

df <- df%>%
  mutate(`3` = `2`/`1`)

df %>%
  rename(x = `1`, 
         y = `2`,
         z = `3`) 


#Ejercicio 5
enframe(1:10)
deframe(enframe(1:10))
enframe(c(x = 3, y = 5))

#Ejercicio 6
?print.tbl_df

