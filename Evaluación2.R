
library(tidyverse)
library(readxl)
install.packages("psych")
library(psych)
library(haven)

EPT <- read_dta("./Efecto promedio del tratamiento.dta")
View(EPT)

#Diferencia en medias variables observables #

#EPT <- Efecto_promedio_del_tratamiento

EPT$ofjobs.tratamiento <- ifelse(EPT$black_sounding==1, EPT$ofjobs, NA)

EPT$ofjobs.control <- ifelse(EPT$black_sounding==0, EPT$ofjobs, NA)

EPT$yearsexp.tratamiento <- ifelse(EPT$black_sounding==1, EPT$yearsexp, NA)

EPT$yearsexp.control <- ifelse(EPT$black_sounding==0, EPT$yearsexp, NA)

EPT$fracblack.tratamiento <- ifelse(EPT$black_sounding==1, EPT$fracblack, NA)

EPT$fracblack.control <- ifelse(EPT$black_sounding==0, EPT$fracblack, NA)

#1. Pruebas balanceadas con pruebas t

t.test(EPT$ofjobs ~ EPT$black_sounding, var.equal=TRUE)

t.test(EPT$yearsexp ~ EPT$black_sounding, var.equal=TRUE)

t.test(EPT$fracblack ~ EPT$black_sounding, var.equal=TRUE)



#2.  Pruebas balanceadas con regresiones Vibariadas# 

PB1 <- lm(college ~ black_sounding, data = EPT)
PB2 <- lm(female ~ black_sounding, data = EPT)
PB3 <- lm(ofjobs ~ black_sounding, data = EPT)
PB4 <- lm(yearsexp ~ black_sounding, data = EPT)
PB5 <- lm(honors ~ black_sounding, data = EPT)
PB6 <- lm(volunteer ~ black_sounding, data = EPT)
PB7 <- lm(military ~ black_sounding, data = EPT)
PB8 <- lm(empholes ~ black_sounding, data = EPT)
PB9 <- lm(workinschool ~ black_sounding, data = EPT)
PB10 <- lm(email ~ black_sounding, data = EPT)
PB11 <- lm(computerskills ~ black_sounding, data = EPT)
PB12 <- lm(specialskills ~ black_sounding, data = EPT)
PB13 <- lm(h ~ black_sounding, data = EPT)
PB14 <- lm(chicago ~ black_sounding, data = EPT)
PB15 <- lm(fracwhite ~ black_sounding, data = EPT)
PB16 <- lm(fracblack ~ black_sounding, data = EPT)
PB17 <- lm(lmedhhinc ~ black_sounding, data = EPT)
PB18 <- lm(fracdropout ~ black_sounding, data = EPT)
PB19 <- lm(fraccolp ~ black_sounding, data = EPT)
PB20 <- lm(linc ~ black_sounding, data = EPT)

install.packages("stargazer")
library(stargazer)

modelos1 <- list(
  PB1 <- lm(college ~ black_sounding, data = EPT),
  PB2 <- lm(female ~ black_sounding, data = EPT),
  PB3 <- lm(ofjobs ~ black_sounding, data = EPT),
  PB4 <- lm(yearsexp ~ black_sounding, data = EPT),
  PB5 <- lm(honors ~ black_sounding, data = EPT)
  
)

stargazer(modelos1, type = "text", title = "Resultados de las regresiones bivariadas",
          model.names = FALSE, column.labels = c("college", "female", "ofjobs", "yearsexp", "honors"),
          omit.stat = "f", single.row = TRUE)

modelos2 <- list(
  PB6 <- lm(volunteer ~ black_sounding, data = EPT),
  PB7 <- lm(military ~ black_sounding, data = EPT),
  PB8 <- lm(empholes ~ black_sounding, data = EPT),
  PB9 <- lm(workinschool ~ black_sounding, data = EPT),
  PB10 <- lm(email ~ black_sounding, data = EPT)
  
)

stargazer(modelos2, type = "text", title = "Resultados de las regresiones bivariadas",
          model.names = FALSE, column.labels = c("volunteer", "military", "empholes", "workinschool", "email"),
          omit.stat = "f", single.row = TRUE)


modelos3 <- list(
  PB11 <- lm(computerskills ~ black_sounding, data = EPT),
  PB12 <- lm(specialskills ~ black_sounding, data = EPT),
  PB13 <- lm(h ~ black_sounding, data = EPT),
  PB14 <- lm(chicago ~ black_sounding, data = EPT),
  PB15 <- lm(fracwhite ~ black_sounding, data = EPT)
)

stargazer(modelos3, type = "text", title = "Resultados de las regresiones bivariadas",
          model.names = FALSE, column.labels = c("computerskills", "specialskills", "h", "chicago", "fracwhite"),
          omit.stat = "f", single.row = TRUE)


modelos4 <- list(
  PB16 <- lm(fracblack ~ black_sounding, data = EPT),
  PB17 <- lm(lmedhhinc ~ black_sounding, data = EPT),
  PB18 <- lm(fracdropout ~ black_sounding, data = EPT),
  PB19 <- lm(fraccolp ~ black_sounding, data = EPT),
  PB20 <- lm(linc ~ black_sounding, data = EPT)
)

stargazer(modelos4, type = "text", title = "Resultados de las regresiones bivariadas",
          model.names = FALSE, column.labels = c("fracblack", "lmedhhinc", "fracdropout", "fraccolp", "linc"),
          omit.stat = "f", single.row = TRUE)


#4. Porcentajes y diferencias en puntos porcentuales

# porcentaje de los CV con nombre blanco y negro que recibieron una llamada para entrevista

tabla <- table(EPT$black_sounding, EPT$call)
rownames(tabla) <- c("Whitesounding", "Blacksounding")
colnames(tabla) <- c("No recibieron llamada", "Sí recibieron llamada")
addmargins(tabla)


# porcentaje de los CV con nombre blanco que recibieron una llamada para entrevista

percentagewhitesounding_call <- (sum(EPT$black_sounding == 0 & EPT$call == 1) / sum(EPT$black_sounding == 0)) * 100
percentagewhitesounding_call

# porcentaje de los CV con nombre negro recibieron una llamada para entrevista
percentageblacksounding_call <- (sum(EPT$black_sounding == 1 & EPT$call == 1) / sum(EPT$black_sounding == 1)) * 100
percentageblacksounding_call


#Diferencia en puntos porcentuales

diferencia_en_pp <- percentagewhitesounding_call - percentageblacksounding_call

cat("La diferencia en puntos porcentuales es:", diferencia_en_pp, "\n")


#5. Regresión Vibariada - Impacto

ImpactoSimple <- lm(call ~ black_sounding, data = EPT)
summary(ImpactoSimple)

#7. Regresión Multivariada - Impacto

ImpactoMultivariado <- lm(call ~ black_sounding + college + female + ofjobs + yearsexp + honors + volunteer + military + empholes + workinschool+
                        email + computerskills + specialskills + h + chicago + fracwhite + fracblack + lmedhhinc + fracdropout +
                          fraccolp + linc, data = EPT)

summary(ImpactoMultivariado)

