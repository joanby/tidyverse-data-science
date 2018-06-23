library(tidyverse)

t <- tibble(
  a = rnorm(100),
  b = rnorm(100),
  c = rnorm(100),
  d = rnorm(100),
  e = rnorm(100)
)
t %>% View()

median(t$a)
median(t$b)
median(t$c)
median(t$d)
median(t$e)

output <- vector("double", ncol(t))
output       # 1 - output
seq_along(t) # 1:ncol(t) 
for(i in seq_along(t)){  # 2 - sequence
  output[[i]] <- median(t[[i]]) # 3 - body
}
output

y <- vector("double",0)
seq_along(y)
1:length(y)



# Bucle for advanced ------------------------------------------------------

# Modificación

df <- tibble::tibble(
  a = rnorm(20),
  b = rnorm(20), 
  c = rnorm(20),
  d = rnorm(20),
  e = rnorm(20)
)

rescale_0_1 <- function(x){
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1])/(rng[2] - rng[1])
}

for (i in seq_along(df)) {
  df[[i]] <- rescale_0_1(df[[i]])
}

df

# Patrones de bucle
# por posición
# for (i in seq_along(df)) -> df[[i]]

# por elementos
# for(x in df) -> x

# por nombre
# for(name in names(df)) -> df[[name]]
df
results <- vector("list", length(df))
names(results) <- names(df)
results

for(i in seq_along(df)){
  name <- names(df)[[i]]
  value <- df[[i]]
  print(paste0(name, " - ", value))
}

# desconocimiento de la longitud de output

means <- 0:100000
#output <- double()
output <- vector("list", length(means))
for(i in seq_along(means)){
  n <- sample(100, 1)
  #output <- c(output, rnorm(n, means[[i]]))
  output[[i]] <- rnorm(n, means[[i]])
}  

str(output)
  
str(unlist(output))

str(purrr::flatten_dbl(output))

paste(output, collapse = "")

#rbind()
#cbind()
dplyr::bind_rows()
dplyr::bind_cols()


# Desconocimiento de la longitud de la iteración
while(condition){ # condition es una condición booleana
  #aquí va el cuerpo del bucle
  #dentro del bucle, en algun momento condition = FALSE
}

for (i in seq_along(df)){
  #body
  print(names(df)[[i]])
}

i <- 1
while(i <= length(df)){
  #body
  print(names(df)[[i]])
  i <- i+1
}


flip_coin <- function() sample(c("C", "X"), 1)

count_heads <- function(total_heads = 5) {
  flips <- 0
  n_heads <- 0 
  
  while (n_heads < total_heads) {
    if(flip_coin() == "C"){
      n_heads <- n_heads + 1
    } else {
      n_heads <- 0
    }
    flips <- flips + 1
  }
  
  flips
}

count_heads(8)



# Programación funcional --------------------------------------------------

df <- tibble::tibble(
  a = rnorm(20),
  b = rnorm(20), 
  c = rnorm(20),
  d = rnorm(20),
  e = rnorm(20)
)

col_means <- function(df){
  output <- vector("double", length(df))
  for(i in seq_along(df)){
    output[[i]] <- mean(df[[i]])
  }
  output
}

col_medians <- function(df){
  output <- vector("double", length(df))
  for(i in seq_along(df)){
    output[[i]] <- median(df[[i]])
  }
  output
}

col_sd <- function(df){
  output <- vector("double", length(df))
  for(i in seq_along(df)){
    output[[i]] <- sd(df[[i]])
  }
  output
}

col_means(df)


f1 <- function(df) abs(df - mean(df)) ^ 1
f2 <- function(df) abs(df - mean(df)) ^ 2
f3 <- function(df) abs(df - mean(df)) ^ 3

f <- function(df, i = 1) abs(df - mean(df)) ^ i

col_summary <- function(df, fun){
  output <- vector("double", length(df))
  for(i in seq_along(df)){
    output[[i]] <- fun(df[[i]])
  }
  output
}

col_summary(df, mean)
col_summary(df, sd)
col_summary(df, min)
col_summary(df, max)
col_summary(df, median)

# paquete base de R
apply()
lapply()
sapply()
tapply()

# un elemento de la lista -> (purrr) -> todos los elementos de la lista
# resolver pequeños problemas que se unan en conjunto con una pipe %>%


# purrr -------------------------------------------------------------------

?map() # crea una lista
map_lgl() # crea un vector lógico
map_int() # crea un vector de enteros
map_dbl() # crea un vector de doubles
map_chr() # crea un vector de caracteres

map_dbl(df, mean)
map_dbl(df, median)
map_dbl(df, sd)

df %>% map_dbl(mean)
df %>% map_dbl(median)
df %>% map_dbl(sd)

df %>% map_dbl(mean, trim = 0.5)

z <- list(x = 1:5, y = 6:10)
map_int(z, length)


models <- mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg~wt, data = .))

models %>%
  map(summary) %>%
  map_dbl(~.$r.squared)

models %>%
  map(summary) %>%
  map_dbl("r.squared")

x <- list(list(1,2,3), list(4,5,6), list(7,8,9))
x %>% map_dbl(2)

# map() <-> lapply()
# sapply()

x1 <- list(
  runif(5),
  runif(5),
  runif(5)
)

x2 <- list(
  runif(5)/2,
  runif(5)/2,
  runif(5)
)

x3 <- list(
  0.8,
  0.9,
  0.85
)

x1
x2
x3

threshold <- function(x, cutoff = 0.75) x[x>cutoff]
x1 %>% sapply(threshold) %>% str()
x2 %>% sapply(threshold) %>% str()
x3 %>% sapply(threshold) %>% str()

vapply(df, is.numeric, logical(1))
map_lgl(df, is.numeric)

safe_log <- safely(log)
str(safe_log(12))

str(safe_log("antonio"))

x <- list(1, 10, "z", -8)
x %>% map(safe_log) %>% transpose() %>% str()
x %>% map(safe_log) %>% transpose() -> y
y$error %>% map_lgl(is_null) -> is_ok
x[!is_ok]
y$result[is_ok] %>%flatten_dbl()

x %>%map_dbl(possibly(log, NA_real_))

list(5,-5) %>% map(quietly(log)) %>% str()

# Multiple mappings -------------------------------------------------------

mu <- list(2, 17, -5)

mu %>%
  map(rnorm, n = 10) %>%
  str()

sigma <- list(1, 5, 25)

seq_along(mu) %>% # . valdrá 1, 2 y 3 respectivamente
  map(~rnorm(10, mu[[.]], sigma[[.]])) %>%
  str()

map2(mu, sigma, rnorm, n = 10) %>% str()

map2 <- function(x, y, f, ...){
  out <- vector("list", length(x))
  for(i in seq_along(x)){
    out[[i]] <- f(x[[i]], y[[i]], ...)
  }
}


n <- list(10, 15, 22)

args <- list(n, mu, sigma)

args %>%
  pmap(rnorm) %>% 
  str()

?rnorm

args_ok <- list(mean = mu, sd = sigma, n = n) 

args_ok %>% pmap(rnorm) %>% str()

param <- tribble(
  ~mean, ~sd, ~n,
  2,     1,   10,
  17,    5,   15,
  -5,    25,  22
)

param %>% pmap(rnorm)


f <- c("runif", "rnorm", "rpois")
param <- list(
  list(min = -5, max = 5),
  list(sd = 3),
  list(lambda = 12)
)

invoke_map(f, param, n = 15) %>% str()

matchs <- tribble(
  ~f,      ~params,
  "runif", list(min = -5, max = 5),
  "rnorm", list(sd = 5), 
  "rpois", list(lambda = 12)
)
matchs %>% mutate(sim = invoke_map(f, params, n = 15)) %>% View()


x <- list("hola", 123, -pi)
x %>% walk(print)

plots <- mtcars %>%
  split(.$cyl) %>%
  map(~ggplot(., aes(mpg, wt)) + geom_point())
paths <- str_c(names(plots), ".pdf") 

pwalk(list(paths, plots), ggsave, path = "data/")

iris %>%
  keep(is.factor) %>%
  str()

iris %>%
  discard(is.factor) %>%
  str()

x <- list(1:5, letters, list(16))
x %>% some(is_character)
x %>% every(is_vector)

x <- sample(12)
x %>% detect(~.<7)
x %>% detect_index(~.<7)

x %>% head_while(~.>7)
x %>% tail_while(~.<7)

dfs <- list(
  age = tibble(name = "Juan Gabriel", age = 30),
  sex = tibble(name = c("Juan Gabriel", "María"), sex = c("M", "F")),
  trt = tibble(name = "María", treatment = "Mrs")
) 

dfs %>% reduce(full_join)

vs <- list(
  1:5,
  c(1,3,5,6,8,10),
  c(1,2,3,7,8,10),
  c(1,2,4,6,9,10)
)

vs %>% reduce(intersect)

x <- sample(12)
x %>% accumulate(`+`)
