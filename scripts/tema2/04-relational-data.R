library(tidyverse)
library(nycflights13)

nycflights13::airlines
airports
planes
weather
colnames(flights)

View(flights)

planes %>%
  count(tailnum) %>%
  filter(n>1)

weather %>%
  count(year, month, day, hour, origin) %>%
  filter(n>1)


flights %>%
  count(year, month, day, hour,flight) %>%
  filter(n>1)

flights %>%
  count(year, month, day, hour,tailnum) %>%
  filter(n>1)


flights_new <- flights %>%
  select(year:day, hour, origin, dest, tailnum, carrier)

flights_new %>% 
  left_join(airlines, by = "carrier")

flights_new %>%
  mutate(name = airlines$name[match(carrier, airlines$carrier)])



x <- tribble(
  ~key, ~value_x,
     1, "x1",
     2, "x2",
     3, "x3"
)

y <- tribble(
  ~key, ~value_y,
     1, "y1", 
     2, "y2", 
     4, "y3"
)

x %>% inner_join(y, by = "key")

x %>% left_join(y, by = "key")

x %>% right_join(y, by = "key")

x %>% full_join(y, by = "key")


x <- tribble(
  ~key, ~value_x,
     1, "x1", 
     2, "x2",
     2, "x3",
     1, "x4"
)

y <- tribble(
  ~key, ~value_y,
     1, "y1",
     2, "y2"
)

x %>% left_join(y, by = "key")

x <- tribble(
  ~key, ~value_x,
     1, "x1",
     2, "x2",
     2, "x3",
     3, "x4"
)

y <- tribble(
  ~key, ~value,
     1, "y1",
     2, "y2",
     2, "y3",
     3, "y4"
)

left_join(x,y, by = "key")


flights_new %>% left_join(weather)

flights_new %>% left_join(planes, by = "tailnum")

flights_new %>% left_join(airports, by = c("dest" = "faa"))

flights_new %>% left_join(airports, by = c("origin" = "faa"))

?base::merge
#     dplyr        <->      base 
# inner_join(x, y) <-> merge(x, y)
# left_join(x, y)  <-> merge(x, y, all.x = TRUE)
# right_join(x, y) <-> merge(x, y, all.y = TRUE)
# full_join(x, y)  <-> merge(x, y, all.x = TRUE, all.y = TRUE)

#     dplyr                  <->           SQL
# inner_join(x, y, by = "z") <-> SELECT * FROM x [INNER] JOIN y USING (z)
#                                SELECT * FROM x [INNER] JOIN y ON x.key = y.key
# left_join(x, y, by = "z")  <-> SELECT * FROM x LEFT [OUTER] JOIN y USING (z)
#                                SELECT * FROM x LEFT [OUTER] JOIN y ON x.key = y.key
# right_join(x, y, by = "z") <-> SELECT * FROM x RIGHT [OUTER] JOIN y USING (z)
#                                SELECT * FROM x RIGHT [OUTER] JOIN y ON x.key = y.key
# full_join(x, y, by = "z")  <-> SELECT * FROM x FULL [OUTER] JOIN y USING (z)
#                                SELECT * FROM x FULL [OUTER] JOIN y ON x.key = y.key


# FILTERING JOINS
# semi_join(x,y) -> se queda con las observaciones de x que tienen correspondencia en y
# anti_join(x,y) -> elimina todas las observaciones de x que tienen correspondencia en y

flights %>%
  count(dest, sort = TRUE) %>%
  head(10) -> top_dest

flights %>%
  filter(dest %in% top_dest$dest)

flights %>% semi_join(top_dest)

flights %>% anti_join(planes, by = "tailnum") %>%
  count(tailnum, sort = TRUE)


# OPERACIONES DE CONJUNTOS
# intersect(x,y): observaciones comunes a x e y (x intersección y)
# union(x,y): observaciones únicas en x e y     (x unión y)
# setdiff(x,y): observaciones en x pero no en y (x - y = x - (x intersección y))

x <- tribble(
 ~a, ~b,
  1,  1,
  2,  1
)

y <- tribble(
  ~a, ~b,
   1,  1,
   1,  2
)

intersect(x,y)
union(x,y)
setdiff(x,y)
setdiff(y,x)


#Ejercicio 5 - Mapa de aeropuertos de USA
airports %>% 
  semi_join(flights, c("faa" = "dest")) %>%
  ggplot(aes(lon,lat)) + 
  borders("state")+
  geom_point()+
  coord_quickmap()

airports %>% count(alt, lon) %>% filter(n>1)


