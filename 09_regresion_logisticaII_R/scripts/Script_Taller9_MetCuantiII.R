########################################################################/
# Script Taller 9 Métodos Cuantitativos II --------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 17-06-2026
########################################################################/

rm(list = ls()) # Borramos todos los objetos cargados


# Ajustes previos ---------------------------------------------------------

options(scipen = 999)


# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(sjPlot)
library(DescTools)


# Base de datos -----------------------------------------------------------

data <- readRDS("data/base_92_20112024.Rds")

data %>% glimpse()


# Preparación de variables ------------------------------------------------

# Recodificamos valores perdidos en VD y escolaridad
data <- data %>%
  mutate(across(c(mtf_45_h, esc_nivel_1_c), ~ case_when(
    . %in% c(-8, -9) ~ NA_real_,
    TRUE ~ .
  )))

# Exploramos la distribución original de mtf_45_h
data %>%
  group_by(mtf_45_h) %>%
  summarise(N = n(),
            Porcentaje = round(100 * n() / nrow(data), 1))

# Binarizamos la VD
# "Muy de acuerdo / De acuerdo": valores 1 y 2
# "Otro": valores 3, 4 y 5
data <- data %>%
  mutate(mtf_45_h = case_when(
    mtf_45_h %in% c(1, 2) ~ "Muy de acuerdo / De acuerdo",
    mtf_45_h %in% c(3:5)  ~ "Otro",
    TRUE                   ~ NA_character_
  ))

data %>%
  group_by(mtf_45_h) %>%
  summarise(N = n(),
            Porcentaje = round(100 * n() / nrow(data), 1))

# Etiquetas en sexo
data$sexo <- factor(data$sexo,
                    levels = c(1:2),
                    labels = c("Hombre", "Mujer"))

# Verificamos NA en las tres variables
data %>%
  summarise(across(c(mtf_45_h, sexo, esc_nivel_1_c),
                   ~ sum(is.na(.))))

# Eliminamos casos con NA en VD y escolaridad
data <- data %>%
  drop_na(mtf_45_h, esc_nivel_1_c)

dim(data)


# Estadísticos descriptivos -----------------------------------------------

# Distribución por sexo
data %>%
  group_by(Sexo = sexo) %>%
  summarise(N = n(),
            Porcentaje = round(100 * n() / nrow(data), 1))

# Distribución de la VD
data %>%
  group_by(Homoparentalidad = mtf_45_h) %>%
  summarise(N = n(),
            Porcentaje = round(100 * n() / nrow(data), 1))

# Escolaridad
summary(data$esc_nivel_1_c)


# Estimación de modelos ---------------------------------------------------

class(data$mtf_45_h)
table(data$mtf_45_h)

# Definimos la categoría de referencia de la VD
data$mtf_45_h <- as.factor(data$mtf_45_h)
levels(data$mtf_45_h)
data$mtf_45_h <- fct_relevel(data$mtf_45_h,"Otro")
  
# Modelo nulo (solo intercepto)
m0 <- glm(mtf_45_h ~ 1,data = data, family = "binomial")

# Modelo 1: sexo
m1 <- glm(mtf_45_h ~ sexo, data = data, family = "binomial")

# Modelo 2: escolaridad
m2 <- glm(mtf_45_h ~ esc_nivel_1_c,data = data, family = "binomial")

# Modelo 3: sexo + escolaridad
m3 <- glm(mtf_45_h ~ sexo + esc_nivel_1_c, data = data, family = "binomial")


# Output de summary() -----------------------------------------------------

table(data$sexo)

summary(m3)


# Tabla comparativa de modelos (tab_model muestra OR por defecto en glm binomial) ----

tab_model(m1, m2, m3,
          show.ci  = FALSE,
          show.se  = TRUE,
          show.aic = TRUE,
          dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"))

# Cálculo manual del OR
exp(coef(m3))

# Aumento porcentual en los odds: (OR - 1) * 100
(exp(coef(m3)["sexoMujer"]) - 1) * 100
########################################################################/
