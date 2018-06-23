library(tidyverse)
#vector atómico: logical, integer, double (numeric), character, complex, raw
#listas: vectores recursivos y heterogéneos
# NA -> ausencia de un valor dentro de un vector
# NULL -> ausencia del vector, es como un vector de longitud 0
typeof(letters)
typeof(1:10)

length(letters)
x <- list("a", "b", 1:10)
typeof(x)
length(x)

# vectores aumentados
# factor: vector aumentado sobre vectore enteros
# date y date-time: vector aumentado sobre vectores numéricos
# data frame y tibble: vector aumentado sobre listas



# Vectores atómicos -------------------------------------------------------
# Lógicos: TRUE; FALSE; NA
1:10 %% 3 == 0

c(TRUE, TRUE, FALSE, NA, FALSE)
# Numérico: 
typeof(1)
typeof(1L)

typeof(1.5L)

x <- sqrt(2)^2
x-2
dplyr::near(x, 2)

x <- c(c(-1,0,1)/0, NA, 2)
x
is.finite(x)
is.infinite(x)
is.na(x)
is.nan(x)

# character
x <- "Dábale arroz a la zorra el abad"
pryr::object_size(x)
y <- rep(x, 10000)
pryr::object_size(y) 
# pointer -> 8 bytes
(8 * 10000 + 152)/1000
152*10000/1000/1024

# complex
1 + 5i

NA            #lógico
NA_character_ #character
NA_complex_   #complejo
NA_integer_   #entero
NA_real_      #double


# Castings ----------------------------------------------------------------

as.logical(c(1,0,0,0,1))
as.integer(c(T,F,F,F,T))
as.double(c(1,2,3))
as.character(c(1,2,3))

x <- sample(20, size = 100, replace = TRUE)
y <- x > 10
sum(y) # cuantos elementos >10 hay en el array?
mean(y) # qué proporción de elementos son > 10?

if(length(x)){ # = 0 -> F, > 0 -> T // length(x)>0
  #hacer algo con el vector... 
}

typeof(c(TRUE, 1L))
typeof(c(1L, 1.6))
typeof(c(1.6, "a"))


is_logical(c(T,T,T))            #lgl
is_integer(c(1L,2L,3L,4L))      #int
is_double(c(1,2,3,4))           #dbl
is_numeric(c(1,2,3,4L))         #int, dbl
is_character(c("a","b", "c"))   #chr
is_atomic(c(T,T, 1,2L, "a"))    #lgl, int, dbl, chr

is_list(list(1,2,3))            #list

is_vector(c(T,T))               #lgl, int, dbl, chr, list

is_scalar_integer(c(4L))


# Recycling rule ----------------------------------------------------------

sample(10) + 12
rep(12,10)

runif(10) > 0.5

1:10 + 1:2
1:10 + 1:3

tibble(
  x = 1:4,
  y = 1:2
)

tibble(
  x = 1:4,
  y = 1
)

tibble(
  x = 1:4,
  y = rep(1:2,2)
)

tibble(
  x = 1:4,
  y = rep(1:2, each = 2)
)

c(x = 4, y = 3, z = -1)

set_names(1:3, c("x", "y", "z"))


# Subsetting --------------------------------------------------------------

x <- letters
x[c(3,2,6)]
x[c(1,1,7,7,7,3,3,3)]
x[c(-3,-5,-7)]
x[-c(6:15)]
x[c(4,-3)]
x[0]

x <- c(4,5,8,NA,2,1,3,NA)
x[!is.na(x)]
x[x %% 2 == 0]

x <- c(abc = 1, def = 2, ghi = 3)
x[c("def", "abc", "abc")]
x["abc"]
x[["abc"]]

x <- matrix(1:9, byrow = T, ncol = 3)
x[1,]
x[,1]
x[-2,]
x[,-1]


# Lists -------------------------------------------------------------------

x <- list(1,2,3)
x
str(x)

x <- list(x = 1, y = 2, z = 3)
str(x)

x <- list("x", 1L, pi, TRUE)
str(x)

x <- list(list(1,2,3), list(pi,sqrt(2)), list("a", "b", 0))
str(x)

x1 <- list(c(1,2),c(3,4))
x1


x2 <- list(list(1,2), list(3,4))
x2

x3 <- list(1, list(2, list(3)))
x3

x <- list(
  a = 1:3, 
  b = "soy un string",
  c = pi, 
  d = list(-2,-5)
)
x

x[1:2]
str(x[1:2])
x[4]
str(x[4])

x[c("a", "c")]

x[[1]]
str(x[[1]])
str(x[[4]])

x[["a"]]
x$a

x[[1]][1]
x[[4]][[1]]


# Attributes --------------------------------------------------------------

x <- 1:12
attr(x, "desc")
attr(x, "desc") <- "Vector de las horas del día"
attr(x, "desc")
attr(x, "created") <- "Juan Gabriel Gomila"
attr(x, "source") <- "Curso Tidyverse"
attributes(x)

as.Date
methods("as.Date")
getS3method("as.Date", "character")
getS3method("as.Date", "numeric")

methods("print")


# Augmented Vectors -------------------------------------------------------

#factor
x <- factor(c("L", "M", "J", "S", "D"), 
            levels = c("L", "M", "X", "J", "V", "S", "D") )
typeof(x)
attributes(x)

# date & date-time
x <- as.Date("1988-05-19")
typeof(x)
attributes(x)
unclass(x)

#POSIXct
x <- lubridate::ymd_hm("1988-05-19 16:30")
x
typeof(x)
attributes(x)
unclass(x)
attr(x, "tzone") <- "US/Pacific"
x
attr(x, "tzone") <- "US/Eastern"
x

#POSIXlt
y <- as.POSIXlt(x)
typeof(y)
attributes(y)

attr(y, "names")

z <- lubridate::as_datetime(y)
typeof(z)

#Tibble
tb <- tibble(x = 1:5, y = 6:10)
typeof(tb)
attributes(tb)

df <- data.frame(x = 1:5, y = 6:10)
typeof(df)
attributes(df)
