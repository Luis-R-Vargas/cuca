datos <- DBDiD

#### PREGUNTA 1 ####

# Filtrar los datos para los años 2010 y 2012 y los subsidios otorgados
datos_2010 <- subset(datos, año == 2010 & subsidio == 1)
datos_2012 <- subset(datos, año == 2012 & subsidio == 1)
datos_2010_sin <- subset(datos, año == 2010 & subsidio == 0)
datos_2012_sin <- subset(datos, año == 2012 & subsidio == 0)

# Calcular las medias condicionales
media_condicional_2010 <- mean(datos_2010$homicidios)
media_condicional_2012 <- mean(datos_2012$homicidios)
media_condicional_2010_sin <- mean(datos_2010_sin$homicidios)
media_condicional_2012_sin <- mean(datos_2012_sin$homicidios)

# Crear la tabla
tabla_esperanza <- matrix(c(media_condicional_2010, media_condicional_2012, media_condicional_2010_sin, media_condicional_2012_sin), nrow = 2, byrow = TRUE)
colnames(tabla_esperanza) <- c("2010", "2012")
rownames(tabla_esperanza) <- c("Subsidio otorgado", "Subsidio no otorgado")

# Imprimir la tabla
print(tabla_esperanza)

#### Pregunta 2 ####

# Calcular el estimador de diferencia en diferencias
estimadorDiD <- ((media_condicional_2012 - media_condicional_2010)-(media_condicional_2012_sin - media_condicional_2010_sin))

# Imprimir el estimador de diferencia en diferencias
print(estimadorDiD)


#### Pregunta 3 ####

#Creamos la base del antes y el despues
did2 <- rbind(datos[datos$año==2010, ], datos[datos$año==2012, ])
#Las dumificamos
did2$después <- ifelse(did2$año==2012, 1, 0)
#Creamos la interacción
did2$después.sub <- did2$después * did2$subsidio

#Sacamos la regresión
summary(lm(homicidios ~ después + subsidio + después.sub, data = did2))


####PRegunta 4#####

# Agregar la variable después a la base de datos
datos$después <- ifelse(datos$año >= 2012, 1, 0)

# Filtrar los datos para los dos períodos de interés
datos_tendencias1 <- subset(datos, año %in% c(2008:2010))
datos_tendencias2 <- subset(datos, año %in% c(2010:2012))

# Crear la primera gráfica para el período de 2008 a 2010
grafica1 <- ggplot(datos_tendencias1, aes(x = año, y = homicidios, color = factor(subsidio), shape = factor(subsidio))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  scale_color_manual(name = "Subsidio", values = c("blue", "red")) +
  scale_shape_manual(name = "Subsidio", values = c(19, 3)) +
  labs(title = "Tendencias de la variable de resultado antes de la asignación del programa",
       x = "Año",
       y = "Número de homicidios") +
  facet_wrap(~ factor(subsidio))

# Crear la segunda gráfica para el período de 2010 a 2012
grafica2 <- ggplot(datos_tendencias2, aes(x = año, y = homicidios, color = factor(subsidio), shape = factor(subsidio))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  scale_color_manual(name = "Subsidio", values = c("blue", "red")) +
  scale_shape_manual(name = "Subsidio", values = c(19, 3)) +
  labs(title = "Tendencias de la variable de resultado después de la asignación del programa",
       x = "Año",
       y = "Número de homicidios") +
  facet_wrap(~ factor(subsidio))

# Combinar las dos gráficas en una sola
install.packages("patchwork")
library(patchwork)
grafica1 + grafica2 + plot_layout(ncol = 2)
