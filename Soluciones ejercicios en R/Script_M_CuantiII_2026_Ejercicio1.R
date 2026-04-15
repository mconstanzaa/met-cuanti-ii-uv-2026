########################################################################/
# Script Solución Ejercicio 1 Métodos Cuantitativos II --------------------
# M. Constanza Ayala (maria.ayala@uv.cl)
# 25-03-2026
########################################################################/

rm(list = ls())


# Paquetes ----------------------------------------------------------------

library(dplyr)
library(haven)


# Base de datos -----------------------------------------------------------

base_93 <- readRDS("data/base_93.Rds")


# Exploración base de datos -----------------------------------------------

dim(base_93)     # Número de observaciones y variables
names(base_93)   # Nombre de las variables
str(base_93)     # Estructura y tipo de cada variable
head(base_93)    # Primeras 6 observaciones
tail(base_93)    # Últimas 6 observaciones

# La base contiene 1493 observaciones y 183 variables.
# Las variables id_bu e id_bu_encuesta son identificadores únicos de cada caso

# Lógica fila, columna
base_93[, 2]     # Valores de la variable en la columna 2
base_93[2, ]     # Valores del caso 2 (fila 2)
base_93[2, 2]    # Valor del caso 2 en la columna 2

########################################################################/
