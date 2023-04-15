###### Ejercicio 2: Asignación Aleatoria ######
###### Código por: Luis Roberto Vargas Pineda y Jessica Daniela Monroy #######

library(haven)
base <- read_dta("C:/Users/danmo/Downloads/Asignación aleatoria.dta")
View(base)

# Cargamos las librerías necesarias
library(dplyr)


######## 1. Transformar la base a nivel municipal #######
datos_mun <- base %>%
  group_by(mun, nom_mun) %>%
  summarise(
    pobtot = sum(pobtot),
    pobfem = sum(pobfem),
    p_18a24 = sum(p_18a24),
    p_18a24_m = sum(p_18a24_m),
    p_18a24_f = sum(p_18a24_f),
    pnacoe = sum(pnacoe),
    p18ym_pb = sum(p18ym_pb),
    pea = sum(pea),
    pocupada = sum(pocupada),
    tvivhab = sum(tvivhab),
    vph_c_serv = sum(vph_c_serv),
    vph_inter = sum(vph_inter)
  )

########## 2. Ordenar alfabéticamente y mostrar primeros y últimos cinco municipios ########
datos_mun <- datos_mun %>%
  arrange(nom_mun)

cat("Primeros cinco municipios:")
head(datos_mun, 5)

cat("Últimos cinco municipios:")
tail(datos_mun, 5)

########## 3. Transformar variables a valores relativos y mostrar estadísticas descriptivas #########
datos_mun <- datos_mun %>%
  mutate(
    porc_pobfem = pobfem / pobtot * 100,
    porc_p_18a24 = p_18a24 / pobtot * 100,
    porc_p_18a24_m = p_18a24_m / pobtot * 100,
    porc_p_18a24_f = p_18a24_f / pobtot * 100,
    porc_pnacoe = pnacoe / pobtot * 100,
    porc_p18ym_pb = p18ym_pb / pobtot * 100,
    porc_pea = pea / pobtot * 100,
    porc_pocupada = pocupada / pobtot * 100,
    porc_vph_c_serv = vph_c_serv / tvivhab * 100,
    porc_vph_inter = vph_inter / tvivhab * 100
  )

# Estadísticas descriptivas

cat("Estadísticas descriptivas de las variables porc_")
select(datos_mun, starts_with("porc_")) %>%
  summarise_all(
    list(
      media = mean,
      desviacion_estandar = sd,
      minimo = min,
      maximo = max
    )
  )


########## 4. Asignación aleatoria del programa ###########
set.seed(12345)

# Crear un vector de asignaciones con 53 tratamientos y 53 controles
asignaciones <- c(rep("tratamiento", 53), rep("control", 53))

# Mezclar aleatoriamente las asignaciones
asignaciones_aleatorias <- sample(asignaciones, length(asignaciones))

# Unir las asignaciones aleatorias a la base de datos como una columna nueva
datos_mun$asignacion <- asignaciones_aleatorias

cat("Tabulación de la asignación:")
table(datos_mun$asignacion)

####### 5. Pruebas t para verificar el balance #########
ttest_pobfem <- t.test(porc_pobfem ~ asignacion, data = datos_mun)
ttest_p_18a24 <- t.test(porc_p_18a24 ~ asignacion, data = datos_mun)
ttest_vph_inter <- t.test(porc_vph_inter ~ asignacion, data = datos_mun)

cat("Resultados de las pruebas t:")
ttest_pobfem
ttest_p_18a24
ttest_vph_inter

# Crear la tabla resumen (Tabla que viene en la tarea para el ejercicio 4)
tabla_ttest <- data.frame(
  variable = c("porc_pobfem", "porc_p_18a24", "porc_vph_inter"),
  media_tratamiento = c(ttest_pobfem$estimate[1], ttest_p_18a24$estimate[1], ttest_vph_inter$estimate[1]),
  media_control = c(ttest_pobfem$estimate[2], ttest_p_18a24$estimate[2], ttest_vph_inter$estimate[2]),
  diff_medias = c(ttest_pobfem$estimate[1] - ttest_pobfem$estimate[2],
                  ttest_p_18a24$estimate[1] - ttest_p_18a24$estimate[2],
                  ttest_vph_inter$estimate[1] - ttest_vph_inter$estimate[2]),
  estadistica_t = c(ttest_pobfem$statistic, ttest_p_18a24$statistic, ttest_vph_inter$statistic),
  p_value = c(ttest_pobfem$p.value, ttest_p_18a24$p.value, ttest_vph_inter$p.value),
  rechazo_hipotesis = c(ttest_pobfem$p.value < 0.05, ttest_p_18a24$p.value < 0.05, ttest_vph_inter$p.value < 0.05)
)

cat("Tabla resumen de las pruebas t:")
tabla_ttest

# 6. Pruebas de balance usando regresiones bivariadas
install.packages("lmtest")
install.packages("sandwich")
library(lmtest)
library(sandwich)

reg_pobfem <- lm(porc_pobfem ~ asignacion, data = datos_mun)
reg_p_18a24 <- lm(porc_p_18a24 ~ asignacion, data = datos_mun)
reg_vph_inter <- lm(porc_vph_inter ~ asignacion, data = datos_mun)

cat("Resultados de las regresiones bivariadas:")
summary(reg_pobfem)
summary(reg_p_18a24)
summary(reg_vph_inter)

######### 7. Comparar las distribuciones usando diagramas de caja #########
install.packages("ggplot2")
library(ggplot2)
ggplot(datos_mun, aes(x = asignacion, y = porc_p_18a24_f)) +
  geom_boxplot() +
  labs(title = "Distribución de porc_p_18a24_f", x = "Asignación", y = "Porcentaje")

ggplot(datos_mun, aes(x = asignacion, y = porc_pnacoe)) +
  geom_boxplot() +
  labs(title = "Distribución de porc_pnacoe", x = "Asignación", y = "Porcentaje")

########## 8. Pruebas de balance para las diez observables poblacionales y de vivienda usando stargazer #########

# Asegúrate de haber instalado y cargado el paquete stargazer ###

install.packages("stargazer")
library(stargazer)

modelos <- list(
  lm(porc_pobfem ~ asignacion, data = datos_mun),
  lm(porc_p_18a24 ~ asignacion, data = datos_mun),
  lm(porc_p_18a24_m ~ asignacion, data = datos_mun),
  lm(porc_p_18a24_f ~ asignacion, data = datos_mun),
  lm(porc_pnacoe ~ asignacion, data = datos_mun),
  lm(porc_p18ym_pb ~ asignacion, data = datos_mun),
  lm(porc_pea ~ asignacion, data = datos_mun),
  lm(porc_pocupada ~ asignacion, data = datos_mun),
  lm(porc_vph_c_serv ~ asignacion, data = datos_mun),
  lm(porc_vph_inter ~ asignacion, data = datos_mun)
)

stargazer(modelos, type = "text", title = "Resultados de las regresiones bivariadas",
          model.names = FALSE, column.labels = c("porc_pobfem", "porc_p_18a24", "porc_p_18a24_m", "porc_p_18a24_f", "porc_pnacoe", "porc_p18ym_pb", "porc_pea", "porc_pocupada", "porc_vph_c_serv", "porc_vph_inter"),
          omit.stat = "f", single.row = TRUE)

############ Oprción 2 #############
# install.packages(c("broom", "dplyr", "tidy"))
library(broom)
library(dplyr)
library(tidyr)

######## Verificar las salidas de las regresiones #############
for (i in 1:length(modelos)) {
  cat("\nModelo", i, ":", model_names[i], "\n")
  print(summary(modelos[[i]]))
}



# Función para extraer resultados de los modelos
library(broom)
extract_results <- function(model, variable_name) {
  coefs <- summary(model)$coefficients
  tibble(
    variable = variable_name,
    term = rownames(coefs),
    estimate = coefs[, "Estimate"],
    std.error = coefs[, "Std. Error"],
    statistic = coefs[, "t value"],
    p.value = coefs[, "Pr(>|t|)"]
  )
}

# Extraer resultados de cada modelo y combinarlos en una tabla
resultados_modelos <- bind_rows(lapply(1:length(modelos), function(i) extract_results(modelos[[i]], model_names[i])))

# Verificar la estructura de la tabla "resultados_modelos"
print(resultados_modelos)

