library(tidyverse)
?magrittr

#Little bunny Foo Foo
#Hopping through the forest
#Scooping up the field mice
#And bopping them on the head

foo_foo <- little_bunny()

hop()
scoop()
bop()

# Variables intermedias
foo_foo1 <- hop(foo_foo, through = forest)
foo_foo2 <- scoop(foo_foo1, up = field_mice)
foo_foo3 <- bop(foo_foo2, on = head)

dd <- ggplot2::diamonds
dd1 <- dd %>%
  dplyr::mutate(price_per_carat = price / carat)

install.packages("pryr")
pryr::object_size(dd)
pryr::object_size(dd1)
pryr::object_size(dd,dd1)

dd$carat[1] <- NA
pryr::object_size(dd)
pryr::object_size(dd1)
pryr::object_size(dd,dd1)

# Sobreescribir la variable original
foo_foo <- hop(foo_foo, through = forest)
foo_foo <- scoop(foo_foo, up = field_mice)
foo_foo <- bop(foo_foo, on = head)

# Componer funciones
bop(
  scoop(
    hop(
      foo_foo, 
      through = forest
      ),
    up = field_mice),
  on = head
  )

# Usar una pipe
foo_foo %>%
  hop(through = forest) %>%
  scoop(up = field_mice) %>%
  bop(on = head)

my_own_pipe <- function(.){
  . <- hop(., through = forest)
  . <- scoop(., through = field_mice)
  . <-bop(., on = head)
  return(.)
}

#assign, get, load
assign("x", 3)
"x" %>% assign(6)

env <- environment()
"x" %>% assign(6, envir = env)
x

#tryCatch, try, supressMessages, supressWarnings
tryCatch(stop("!"), 
         error = function(e) "Me he encontrado un error")

stop("!") %>%
  tryCatch(error = function(e) "Me he encontrado un error")

rnorm(1000) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()

mtcars %$%
  cor(disp, mpg)

mtcars <- mtcars %>%
  filter(cyl == 6)

mtcars %<>%
  filter(cyl == 6)
