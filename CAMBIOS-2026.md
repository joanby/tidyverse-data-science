# Cambios de la rama `update-2026`

> Esta rama es el mismo curso —Curso completo de R para Data Science con Tidyverse—, con el código adaptado
> a R y los paquetes de hoy (R 4.5.3, tidyverse 2.0.0, readr 2.2.0, purrr 1.2.2, dplyr 1.2.1, ggplot2 4.0.3;
> octubre de 2026). La rama principal sigue exactamente como en el vídeo.

> **Cómo se ha comprobado.** Los scripts del curso están pensados para ejecutarse línea a línea en RStudio:
> llevan pseudocódigo (`little_bunny %>% hop()`), ejemplos de ayuda sin argumentos y líneas que fallan a
> propósito. Por eso cada script se ha ejecutado **línea a línea**, como en RStudio, dos veces: con R y paquetes
> de finales de 2019 (como en el vídeo) y con los de hoy. Lo que falla en las dos es parte del curso; lo que
> solo falla hoy es lo que se ha arreglado. Resultado: **no queda ninguna línea que funcionara entonces y
> falle hoy**, salvo el ejemplo de `if(c(T, F))`, que se explica abajo.
> No se ha podido comprobar `geom_lv()` (tema 1, EDA): el paquete `lvplot` no estaba disponible en el
> entorno de prueba.

## Qué ha cambiado y por qué

- **readr 2:** `read_csv(path = …)` → `read_csv(file = …)` (tema 2, importación de datos).
- **purrr 1.0** retiró `is_numeric()`: `is.numeric()` (tema 3, vectores), que hace lo mismo.
- **R 4.2:** `if (c(T, F)) {}` ya no da un aviso, **da un error** (la condición de un `if` tiene que ser un
  único `TRUE`/`FALSE`). Es el ejemplo de la lección, así que se ha dejado tal cual, con un comentario.
- **Caché de knitr subida al repositorio** (`tema5/01-first-markdown_cache`): se ha borrado. Cargaba paquetes
  de entonces (`bindrcpp`, archivado) y hacía fallar el render; RStudio la vuelve a crear.

Si te sale el aviso de que una función del tidyverse está *deprecated* (por ejemplo `gather()`/`spread()`,
hoy `pivot_longer()`/`pivot_wider()`), el código sigue funcionando: son avisos amarillos.
