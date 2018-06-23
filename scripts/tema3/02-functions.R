#nombre a la función 
#cambios en un único lugar
#eliminais probabilidad de error del C&P

df <- tibble::tibble(
  a = rnorm(20),
  b = rnorm(20), 
  c = rnorm(20),
  d = rnorm(20),
  e = rnorm(20)
)

df %>% View()

df$a <- (df$a - min(df$a, na.rm = TRUE))/(max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$b <- (df$b - min(df$b, na.rm = TRUE))/(max(df$b, na.rm = TRUE) - min(df$b, na.rm = TRUE))
df$c <- (df$c - min(df$c, na.rm = TRUE))/(max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE))
df$d <- (df$d - min(df$d, na.rm = TRUE))/(max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE))
df$e <- (df$e - min(df$e, na.rm = TRUE))/(max(df$e, na.rm = TRUE) - min(df$e, na.rm = TRUE))

View(df)

x <- df$a
rng <- range(x, na.rm = TRUE)
(x - rng[1])/(rng[2] - rng[1])

rescale_0_1 <- function(x){

  # Calcular rango de lo datos excluyendo NAs e Infinitos -------------------
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  (x - rng[1])/(rng[2] - rng[1])
}

rescale_0_1(c(0,5,10))
rescale_0_1(c(-1,0,1))
rescale_0_1(c(1,2,NA,4,5))

df$a <- rescale_0_1(df$a)
df$b <- rescale_0_1(df$b)
df$c <- rescale_0_1(df$c)
df$d <- rescale_0_1(df$d)
df$e <- rescale_0_1(df$e)

x <- c(1:10, Inf)
rescale_0_1(x)

l()
my_fucking_awesome_function()

impute_missing()
count_days()
collapse_hours()

imputeMissing()
countDays()
collapseHours()

col_number()
colMeans()

input_select()
input_checkbox()
input_text()

select_input()
checkbox_input()
text_input()

stringr::str_

#T <- FALSE
#c <- 5
#mean <- function(x) median(x)

# Load data --------------------------------------------- 

read_csv("data/cars.csv")

# Plot data =============================================


# Nueva sección -----------------------------------------------------------



# Condicionales -----------------------------------------------------------

if(condicion){
  # código a ejecutar si la condición es TRUE
} else {
  # código a ejecutar si la condición es FALSE
}

?`if`

has_name <- function(x){
  nms <- names(x)
  if(is.null(nms)){
    #no existe el objeto en cuestión
    rep(FALSE, length(x))
  } else {
    # ausencia de NAs y de ""
    !is.na(nms) & nms != ""
  }
}

has_name(c(1,2,3))
has_name(mtcars)
has_name(tribble(
  ~x, ~y, ~` `,
   1,  2,   3
))

if(c(T,F)){}

if(NA){}

# c1 && c2 -> AND
# c1 || c2 -> OR

if(any(c(T,F))){
  "tenemos almenos un verdadero"
} else{
  "no hay ningún verdadero"
}
if(all(c(T,F))){
  "tenemos todas las condiciones verdaderas"
} else{
  "tenemos alguna condición falsa"
}

identical(0, 0L)

2 == sqrt(2)^2

sqrt(2)^2-2

dplyr::near(2, sqrt(2)^2)

2 == NA

if(condicion) {
  # resultado 1
} else if(condicion2) {
  # resultado 2
} else if(condicion3) {
  # resultado 3
} else {
  # resultado por defecto
}

calculate <- function(x,y,op) {
  switch (op,
    suma = x+y,
    resta = x-y,
    multiplicacion = x*y,
    division = x/y,
    stop("ERROR: no se puede ejecutar la operación indicada")
  )
}

calculate(2,3,"suma")
calculate(2,3,"resta")
calculate(2,3,"multiplicacion")
calculate(2,3,"division")
calculate(2,3,"antonio")

x <- 5
y <- -7

if(y < 0 && length("Hace sol")>0) {
  message("y es negativo, con este sol que hace")
}

#if(y < 0 && length("Hace sol")>0) 
#message("y es negativo, con este sol que hace")



if(y <= 0) {
  log(x)
} else {
  y^x
}

#if(y <= 0) {
#  log(x)
#} 
#else {
#  y^x
#}

y <- 10
x <- if(y<20) "Número pequeño" else "Número grande"

if(y<20) {
  x <- "Número pequeño"
} else {
  x <- "Número grande"
}


# Argumentos --------------------------------------------------------------
# Dato
# Detalle del cálculo
log(x = 8, base = 2)
mean(x = c(1,2,3,NA,4,5,10), trim = 2, na.rm = T)
t.test(rnorm(100, mean = 0, sd = 1), rnorm(100, mean = 1, sd = 1),
       alternative = "greater", paired = T, var.equal = T, conf.level = 0.99)
stringr::str_c(c("banana", "Manzana", "pera"),  collapse = ", ")

standar_ci <- function(x, conf = 0.95){
  se <- sd(x)/sqrt(length(x))
  alpha <- (1-conf)
  mean(x) + se * qnorm(c(alpha/2, 1-alpha/2))
}


x <- runif(1000)
standar_ci(x)
standar_ci(x, conf = 0.99)
standar_ci(x, conf = 0.999)

mean(1:10, na.rm = TRUE)

mean(x = 1:10, , TRUE)
mean(,TRUE, x = c(1:10,NA))
mean(1:10, n=T)

avg <- mean(120 / 12 + 32, na.rm = TRUE)
avg<-mean(120/12+32,na.rm=TRUE)

# x, y, z: vectores
# w: vector de pesos
# df, data, d : data frame
# i, j, k : subíndices numéricos (filas y columnas)
# n : longitud de un vector, o número de filas
# m : número de columnas
# p : probabilidades


wt_mean <- function(x, w, na.rm = TRUE){
  
  stopifnot(is.logical(na.rm), 
            length(na.rm) == 1,
            length(x) == length(w)
            ) 
  
  if(na.rm) {
    missing <- is.na(x) | is.na(w)
    x <- x[!missing]
    w <- w[!missing]
  }
  
  sum(x*w) / sum(w)
}

wt_var <- function(x, w) {
  mu <- wt_mean(x, w)
  sum(w*(x-mu)^2)/sum(w)
}

wt_sd <- function(x, w){
  sqrt(wt_var(x, w))
}

wt_mean(1:6, 6:1, na.rm = T)

#...
sum(1:10)
str_c(c("a", "b", "c", "d", "e"))

commas <- function(...) stringr::str_c(..., collapse = ", ")
commas(letters[1:10])

rule <- function(..., pad = "=") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(stringr::str_dup(pad, width/2), " ", title, " ", stringr::str_dup(pad, width/2), "\n", sep = "")
}

rule("Sección número 1 ", "Informe del jefe")


sum(c(1,2), na.mr = T)
list(...)

my_function <- function(x,y,z) {
  if(length(x) == 0 || length(y) == 0){
    return(0)
  }
  ## el código sigue más adelante con otras funciones...
}

my_function <- function(x){
  if(x){
    #Aquí
    #va
    #un
    #código
    #muy
    #largo
    #y 
    #complejo
  }else {
    return(0)
  }
}


my_function <- function(x){
  if(!x){
    return(0)
  }
  
    #Aquí
    #va
    #un
    #código
    #muy
    #largo
    #y 
    #complejo
}

#transformación: el objeto de entrada es modificado antes de ser devuelto
#efecto secundario: el objeto de entrada no es modificado (plot, write...)

show_nas <- function(df){
  n <- sum(is.na(df))
  cat("Número de NAs: ", n, "\n", sep = "")
  
  invisible(df)
}

x <- show_nas(diamonds)

class(x)

dim(x)

mtcars %>%
  show_nas() %>%
  mutate(mpg = ifelse(mpg>20, NA, mpg)) %>%
  show_nas()

f <- function(x){
  x + y
}

y <- 5
f(3)

y <- 30
f(3)


`+` <- function(x,y){
  if(runif(1) < 0.1){
    sum(x,y)
  } else{
    sum(x,y)*1.5
  }
}

table(replicate(1000, 2+3))

rm(`+`)