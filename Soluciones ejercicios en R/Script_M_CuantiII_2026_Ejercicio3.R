########################################################################/
# Script Solución Ejercicio 3 Métodos Cuantitativos II --------------------
# M. Constanza Ayala (maria.ayala@uv.cl)
# 22-04-2026
########################################################################/

# Preparación -------------------------------------------------------------

# Ajustes previos

options(scipen = 999)

# Paquetes

library(tidyverse)
library(sjPlot)

# Base de datos

load("data/Casen 2022_filter.Rdata")
data %>% glimpse()

# Filtro y ajuste variable
data_osig <- data %>%
  filter(
    edad >= 25 & edad <= 65,
    !genero %in% c(-88, -99),
    !is.na(yautcor),
    yautcor > 0
  ) %>%
  mutate(genero = case_when(
    genero == 1         ~ "Masculino",
    genero == 2         ~ "Femenino",
    genero %in% c(3:6)  ~ "Otro",
    TRUE                ~ NA_character_
  ))

dim(data_osig)

# Estadísticos descriptivos
data_osig %>%
  summarise(
    Media_yautcor   = mean(yautcor, na.rm = TRUE),
    DE_yautcor      = sd(yautcor, na.rm = TRUE),
    Media_tot_per_h = mean(tot_per_h, na.rm = TRUE),
    DE_tot_per_h    = sd(tot_per_h, na.rm = TRUE)
  )

# La muestra quedó con 82.663 personas luego del filtro.
# El ingreso autónomo promedio es de $711.202,2 pesos.


# Modelo 1 — género -------------------------------------------------------

# Pregunta de investigación: ¿En qué medida el género predice el ingreso
# autónomo de las personas en edad laboral activa en Chile (CASEN 2022)?

m1 <- lm(yautcor ~ genero, data = data_osig)

# Bondad de ajuste
summary(m1)

# El estadístico F(2, 49037) = 637.4, p < .001, indica que el modelo es
# estadísticamente significativo: el género capta varianza en el ingreso
# autónomo más allá del azar.
# El R² = 0.025 indica que el género explica aproximadamente el 2.5% de
# la varianza en el ingreso autónomo.
########################################################################/
