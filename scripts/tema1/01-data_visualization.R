#Data Visualization - 11 de Mayo de 2018
library(tidyverse)

#tidyverse 1.2.1 ──
#✔ ggplot2 2.2.1     ✔ purrr   0.2.4
#✔ tibble  1.4.2     ✔ dplyr   0.7.4
#✔ tidyr   0.8.0     ✔ stringr 1.3.1
#✔ readr   1.1.1     ✔ forcats 0.3.0

#Los coches con motor más grande consumen más combustible 
#que los coches con motor más pequeño.
#La relación consumo / tamaño es lineal? Es no lineal? Es exponencial?
#Es positiva? Es negativa?

View(mpg)
?mpg #help(mpg)
# displ: tamaño del motor del coche en litros
# hwy: número de millas recorridas en autopista por galón de combustible (3.785411784 litros)

ggplot(data = mpg)

mpg %>% ggplot()


ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

#PLANTILLA PARA HACER UNA REPRESENTACIÓN GRÁFICA CON GGPLOT
#ggplot(data = <DATA_FRAME>) +
#  <GEOM_FUNCTION>(mapping = aes(<MAPPINGS>))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = class, y = drv))



#Color de los puntos
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

#Tamaño de los puntos (conviene que sea numérico)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, size = class))

#Transparencia de los puntos
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

#Forma de los puntos (solo permite 6 formas a la vez)
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

#Elección manual de estéticas
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "red")
# color = nombre del color en formato string
# size = tamaño del punto en mm
# shape = forma del punto con números desde el 0 al 25
# 0 - 14: son formas huecas y por tanto solo se le puede cambiar el color
# 15- 20: son formas rellenas de color, por tanto se le puede cambiar el color
# 21 - 25: son formas con borde y relleno, y se les puede cambiar el color (borde) y el fill (relleno)

d=data.frame(p=c(0:25))
ggplot() +
  scale_y_continuous(name="") +
  scale_x_continuous(name="") +
  scale_shape_identity() +
  geom_point(data=d, mapping=aes(x=p%%16, y=p%/%16, shape=p), size=5, fill="yellow") +
  geom_text(data=d, mapping=aes(x=p%%16, y=p%/%16+0.25, label=p), size=3)



ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), 
             shape = 23, size = 10, color = "red", 
             fill = 'yellow')

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y = hwy, color = displ<5))


##FACETS
# facet_wrap(~<FORMULA_VARIABLE>): la variable debe ser discreta
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~class, nrow = 3)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy)) +
  facet_wrap(~displ, nrow = 3)

# facet_grid(<FORMULA_VARIABLE1>~<FORMULA_VARIABLE2>)
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy)) +
  facet_grid(drv~cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy)) +
  facet_grid(.~cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y = hwy)) +
  facet_grid(drv~.)

#Diferentes geometrías
ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y =hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x=displ, y = hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x=displ, y = hwy, linetype = drv))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y=hwy, color = drv)) +
  geom_smooth(mapping = aes(x=displ, y = hwy, linetype = drv, color = drv))

?geom_smooth


ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x=displ, y=hwy))

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x=displ, y=hwy, group = drv, color = drv),
              show.legend = T)

ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point(mapping = aes(shape = class)) + 
  geom_smooth(mapping = aes(color = drv))


ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "suv"), se = F)

ggplot(data = mpg, mapping = aes(x=displ, y = hwy,color = drv)) + 
  geom_point() + 
  geom_smooth( se = F)


#Ejercicio 4
ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = mpg) + 
  geom_point(mapping = aes(x=displ, y = hwy)) + 
  geom_smooth(mapping = aes(x=displ, y = hwy))

#Ejercicio 5
ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(se=F)

#Ejercicio 6
ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) + 
  geom_point() + 
  geom_smooth(mapping = aes(group=drv), se=F)

#Ejercicio 7
ggplot(data = mpg, mapping = aes(x=displ, y = hwy, col=drv)) + 
  geom_point() + 
  geom_smooth( se=F)

#Ejercicio 8
ggplot(data = mpg, mapping = aes(x=displ, y = hwy)) + 
  geom_point(mapping = aes(col=drv, shape = drv)) + 
  geom_smooth( se=F)

#Ejercicio 9
ggplot(data = mpg, mapping = aes(x=displ, y = hwy) ) + 
  geom_point(mapping = aes(col=drv, shape = drv)) + 
  geom_smooth(mapping = aes(linetype = drv), se=F)

#Ejercicio 10
ggplot(data = mpg, mapping = aes(x=displ, y = hwy) ) + 
  geom_point(mapping = aes(fill = drv), size = 4, 
             shape = 23, col = "white", stroke = 2) 


## Ejemplo del dataset de diamantes
View(diamonds)

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))

?geom_bar

ggplot(data = diamonds)+
  stat_count(mapping = aes(x=cut))


demo_diamonds <- tribble(
  ~cut,       ~freqs,
  "Fair",       1610,
  "Good",       4906,
  "Very Good", 12082,
  "Premium",   13791,
  "Ideal",     21551
)

ggplot(data = demo_diamonds) + 
  geom_bar(mapping = aes(x=cut, y = freqs), 
           stat = "identity")

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, y = ..prop.., group = 1))

ggplot(data = diamonds) + 
  stat_summary(
    mapping = aes(x = cut, y = depth),
    fun.ymin = min,
    fun.ymax = max, 
    fun.y = median
  )

#Colores y formas de los gráficos


ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, colour = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = cut))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color))

## position = "identity"
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(alpha = 0.2, position = "identity")

ggplot(data = diamonds, mapping = aes(x = cut, colour = clarity)) + 
  geom_bar(fill = NA, position = "identity")


## position = "fill"
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(position = "fill")

## position = "dodge"
ggplot(data = diamonds, mapping = aes(x = cut, fill = clarity)) + 
  geom_bar(position = "dodge")


## Volvemos al scatterplot
## position = "jitter"
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point( position = "jitter" )

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_jitter()

?position_stack
?position_identity
?position_fill
?position_dodge
?position_jitter


# Sistemas de Coordenadas

#coord_flip() -> cambia los papeles de x e y
ggplot(data = mpg, mapping = aes(x=class, y = hwy)) + 
  geom_boxplot()

ggplot(data = mpg, mapping = aes(x=class, y = hwy)) + 
  geom_boxplot() + 
  coord_flip()

#coord_quickmap() -> configura el aspect ratio para mapas

usa <- map_data("usa")

ggplot(usa, aes(long, lat, group = group)) + 
  geom_polygon(fill = "blue", color = "white") + 
  coord_quickmap()

italy <- map_data("italy")

ggplot(italy, aes(long, lat, group = group)) + 
  geom_polygon(fill = "blue", color = "white") + 
  coord_quickmap()

#coord_polar()

ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut),
    show.legend = F,
    width = 1
  ) +
  theme(aspect.ratio = 1) + 
  labs(x = NULL, y = NULL) + 
  coord_polar()


ggplot(data = mpg, mapping = aes(x = cty, y = hwy )) + 
  geom_point() + 
  geom_abline() + 
  coord_fixed()

?geom_jitter
?geom_boxplot
?labs
?geom_abline

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = color))+ 
  theme(aspect.ratio = 1) + 
  labs(x = NULL, y = NULL) + 
  coord_polar()


# Gramática por capas de ggplot2

#ggplot(data = <DATA_FRAME>) +
#  <GEOM_FUNCTION>(
#                  mapping = aes(<MAPPINGS>),
#                  stat = <STAT>,
#                  position = <POSITION>
#                 ) + 
#   <COORDINATE_FUNCTION>() + 
#   <FACET_FUNCTION>()

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = clarity, fill = clarity, y = ..count..)) +
  coord_polar() +
  facet_wrap(~cut) +
  labs(x=NULL, y = NULL, title = "Ejemplo final de ggplot con JB",
       caption = "Dos variables cruzadas de diamonds",
       subtitle="Aprender ggplot puede ser hasta divertido ;)")

