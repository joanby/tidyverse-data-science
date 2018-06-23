library(tidyverse)
library(stringr)

s1 <- "Esto es un string"
s2 <- 'Esto es un string que contiene otro "string" dentro'
s3 <- "Esto es un string sin cerrar"

double_quote <- "\"" # '"'
single_quote <- '\'' # "'"
backslash <- "\\"

x <- c(single_quote, double_quote, backslash)
writeLines(x)
#\n -> intro, salto de línea
#\t -> tabulador

mu <- "\u00b5"

?'"'

c("uno", "dos", "tres")


str_length(c("x", "Juan Gabriel mola como profe", NA))

str_c("a", "b","c", sep = ", ")

x <- c("abc", NA)
str_c("hola", str_replace_na(x), "adios", sep = " ")

str_c("prefix-", c("a","b","c"),"-suffix")

name <- "Ricardo"
momento_del_dia <- "mañana"
birthday <- F

str_c(
  "Buena ", momento_del_dia," ", name,
  if(birthday) " y FELIZ CUMPLEAÑOS!!! =D",
  "."
)

str_c(c("a", "b", "c"), collapse = ",")

x <- c("Manzanas", "Peras", "Limones", "Plátanos")
str_sub(x, 1,3)
str_sub(x, -3,-1)
str_sub("x", 1,8)

str_sub(x,1,1) <- str_to_lower(str_sub(x,1,1))
str_to_upper(x)
str_to_title(x)

str_to_upper("i", locale = "tr")

str_sort(x, locale = "es")
str_sort(c("apple", "banana",  "eggplant"), locale = "haw")

str_order(x, locale = "haw")

# REGEXP
# - str_view()
# - str_view_all()

library(htmlwidgets)
x <- c("manzana", "banana", "pera", "pomelo")
str_view(x, "an")

#. -> cualquier caracter (pero solo uno)
str_view(x, ".a.")

#\. -> localizar un punto
dot <- "\\."
writeLines(dot)
str_view(c("abc","a.c","bc."), "a\\.c")

#\\ -> localizar un backslash
backslash <- "\\\\"
writeLines(backslash)
str_view("a\\b", "\\\\")


#^ -> inicio del string
#$ -> final del string
str_view(x, "^p")
str_view(x, "a$")

y <- c("tarta de manzana", "manzana", "manzana al horno", "pastel de manzana")
str_view(y, "^manzana$")

#\b -> localizar la frontera de una palabra
sum()
summarise()

#\d -> localizar cualquier dígito
#\s -> cualquier espacio en blanco (espacio, tabulador, salto de línea)
#[abc] -> localizar la a, la b o la c indistintamente
#[^abc] -> localizar cualquier cosa excepto la a, b o c

#abc|d..m, abc|xyz
str_view(c("grey", "gray"), "gr(e|a)y")

#? -> 0 o 1
#+ -> 1 o + veces
#* -> 0 o más veces

x <- "El año 1888 es el más largo en números romanos: MDCCCLXXXVIII"
str_view(x, "CC?")
str_view(x, "CC+")
str_view(x, "C[LX]+")

#"colou?r"
#"ba(na)+"

#{n} -> exactamente n repeticiones 
#{n,} -> n o más repeticiones
#{,m} -> como máximo m repeticiones
#{n,m} -> entre n y m repeticiones
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}?")
str_view(x, "C[LX]+?")

fruits = c("banana", "coco", "papaya", "manzana", "pera", "pepino")
str_view(fruit, "(..)\\1", match = TRUE)
str_view("abc-def-", "(...)(-)\\1\\2")

# Otras herramientas
str_detect(fruits, "a")

sum(str_detect(words, "^j"))
mean(str_detect(words, "^[aeiou]"))
mean(str_detect(words, "[aeiou]$"))

f1 <- !str_detect(words, "[aeiou]")
f2 <- str_detect(words, "^[^aeiou]+$")
identical(f1, f2)

words[!str_detect(words, "[aeiou]")]
str_subset(words, "[aeiou]$")

df <- tibble(
  word = words, 
  i = seq_along(word)
)
df %>% View()

df %>% filter(str_detect(words, "x$"))

str_count(fruits, "a")

mean(str_count(words, "[aeiou]"))

df %>%
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

str_count("abababababa", "aba")
str_view("abababababa", "aba")
str_view_all("abababababa", "aba")

head(sentences)
length(sentences)

colors <- c("red", "orange", "yellow", "green", "blue", "purple")
color_match <- str_c(colors, collapse = "|")
color_match
has_color <- str_subset(sentences, color_match)
has_color
matches <- str_extract(has_color, color_match)
matches
more_than_one <- sentences[str_count(sentences, color_match)>1]
str_view_all(more_than_one, color_match)
str_extract(more_than_one, color_match)

str_extract_all(more_than_one, color_match)
str_extract_all(more_than_one, color_match, simplify = TRUE)

x <- c("x", "x y", "x y z")
str_extract_all(x, "[a-z]", simplify = TRUE)

noun <- "(a|the) ([^ ]+)"

sentences %>% 
  str_subset(noun) %>%
  str_extract(noun) %>%
  head(20)

sentences %>% 
  str_subset(noun) %>%
  str_match_all(noun) %>%
  head(20)


tibble(sentence = sentences) %>%
  tidyr::extract(
    sentence, 
    c("article", "noun"), 
    "(a|the) ([^ ]+)",
    remove = FALSE
  )

#str_replace()
#str_replace_all()

str_replace(fruits, "[aeiou]", "_")
str_replace_all(fruits, "[aeiou]", "_")

str_replace_all(c("1 coche", "2 teléfonos", "3 amigos"),
                c("1" = "un", "2" = "dos", "3" = "tres")
                )

sentences %>%
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>%
  head(20)

sentences %>%
  head(10) %>%
  str_split(" ") %>%
  .[[1]]

"a,b,c,d,e,f" %>%
  str_split(",") %>%
  .[[1]]


sentences %>%
  head(10) %>%
  str_split(" ", simplify = TRUE) 

fields <- c("Name: Juan Gabriel", "Country: España", "Age: 30")
fields %>% str_split(": ", n = 2, simplify = TRUE)

#c("character", "line_break", "sentence", "word")

sent <- "El perro de San Roque no tiene rabo. Y esto es todo."
str_view_all(sent, boundary("word"))
str_split(sent, " ")
str_split(sent, boundary("word"))[[1]]

str_locate_all(sent, "[aeiou] ")
str_sub(sent, 8, 9)


str_view(fruit, "nana")
str_view(fruit, regex("nana"))

apples <- c("manzana", "Manzana", "MANZANA")
str_view(apples, "manzana")
str_view(apples, regex("manzana", ignore_case = TRUE))

x <- "Linea 1\nLinea 2\nLinea 3\nLinea 4"
str_extract_all(x, "^Linea")[[1]]
str_extract_all(x, regex("^Linea", multiline = TRUE))[[1]]

phone <- regex("
               \\(?      #paréntesis de apertura opcionales
               (\\d{3})  #código de área
               [)- ]?    #cierre de paréntesis, guión o espacio opcionales
               (\\d{3})  #tres dígitos de teléfono
               [ -]?     #espacio o guión opcional
               (\\d{3})  #tres dígitos finales
               ", comments = TRUE)

str_match("971-123-456", phone)

#dotall = TRUE <-> permite que el punto '.' reemplace cualquier cosa (incluido \n)
microbenchmark::microbenchmark(
  regex = str_detect(sentences, "the"),
  fixed = str_detect(sentences, fixed("the")),
  times = 30
)

"\u00e1" == "a\u0301"

str_detect("\u00e1", fixed("a\u0301"))

str_detect("\u00e1", coll("a\u0301"))

turkish_i = c("I", "İ", "ı",	"i")
turkish_i
str_subset(turkish_i, coll("i", ignore_case = TRUE))
str_subset(turkish_i, coll("i", ignore_case = TRUE, locale = "tr"))

stringi::stri_locale_info()$Name

str_view_all("Esto es una frase.", boundary("word"))
str_extract_all("Esto es una frase.", boundary("word"))[[1]]


apropos("replace")
apropos("fruit")
getwd()

dir(pattern = "^.*\\.Rmd$")
glob2rx("*.Rmd")

?stringi
