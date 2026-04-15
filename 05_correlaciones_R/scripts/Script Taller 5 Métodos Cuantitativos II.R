########################################################################/
# Script Taller 5 Métodos Cuantitativos II --------------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 15-04-2026
########################################################################/

rm(list = ls()) # Borramos todos los objetos cargados


# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(haven)
library(sjPlot)
library(labelled)


# Base de datos -----------------------------------------------------------

data <- readRDS("data/base_93.Rds")


# Etiquetar variables -----------------------------------------------------

data <- data %>%
  set_variable_labels(
    bienestar_2   = "Satisfacción con la vida",
    iden_pol_2    = "Autoidentificación ideológica",
    pobreza_62    = "Igualdad vs. esfuerzo individual",
    religion_63   = "Trabajo duro vs. suerte",
    democracia_38 = "Libertades vs. control delincuencia"
  )


# Recodificación de valores perdidos --------------------------------------

# En la CEP 93, los valores -8 (No sabe) y -9 (No contesta)
# corresponden a datos perdidos y deben reemplazarse por NA.
# Usamos across() para aplicar la recodificación a múltiples variables a la vez.

data <- data %>%
  mutate(across(
    c(bienestar_2, iden_pol_2, pobreza_62, religion_63, democracia_38),
    ~ case_when(
      . %in% c(-8, -9) ~ NA_real_,
      TRUE ~ .
    )
  ))

# Verificamos que los valores -8 y -9 ya no aparezcan
data %>%
  select(bienestar_2, iden_pol_2, pobreza_62, religion_63, democracia_38) %>%
  summary()


# Gráfico de dispersión ---------------------------------------------------

ggplot(data, aes(x = iden_pol_2, y = pobreza_62)) +  # variables en los ejes
  geom_jitter(alpha = 0.2,                            # puntos semitransparentes
              width = 0.2, height = 0.2) +            # separa puntos superpuestos en escalas discretas
  geom_smooth(method = "lm",                          # línea de tendencia lineal
              color = "steelblue") +                  # color de la línea
  labs(
    title = "Gráfico de dispersión: autoidentificación ideológica y preferencia por el esfuerzo individual",
    x = "Autoidentificación ideológica (1 = izquierda, 10 = derecha)",
    y = "Ingresos iguales vs. esfuerzo individual (1–10)",
    caption = "Fuente: Encuesta CEP N° 93"
  ) +
  theme_minimal()                                     # tema sin fondo gris


# Correlación de Pearson --------------------------------------------------

# cor() entrega el coeficiente r
# Sin especificar el tratamiento de NA → devuelve NA si hay valores perdidos
cor(data$iden_pol_2, data$pobreza_62)

# Con use = "complete.obs" → usa solo los casos con datos en ambas variables
cor(data$iden_pol_2, data$pobreza_62, use = "complete.obs")

# cor.test() agrega inferencia estadística (t, df, p-valor, IC 95%)
options(scipen = 999) # evita notación científica
cor.test(data$iden_pol_2, data$pobreza_62)


# Matriz de correlaciones: tab_corr() -------------------------------------

# Seleccionamos las variables de trabajo
vars_corr <- data %>%
  select(bienestar_2, iden_pol_2, pobreza_62, religion_63, democracia_38)

# Matriz completa
tab_corr(vars_corr, na.deletion = "pairwise")

# Triángulo inferior (más legible)
tab_corr(vars_corr, na.deletion = "pairwise", triangle = "lower")


# Mapa de calor -----------------------------------------------------------

# Calculamos la matriz de correlaciones (solo casos completos)
mat_corr <- cor(vars_corr, use = "complete.obs")

# Convertimos la matriz a formato largo: una fila por par de variables
mat_long <- as.data.frame(as.table(mat_corr))

# Renombramos las columnas para facilitar el trabajo con ggplot2
names(mat_long) <- c("Variable1", "Variable2", "r")

ggplot(mat_long, aes(x = Variable1, y = Variable2, fill = r)) +  # ejes y color según r
  geom_tile(color = "white") +                                    # celdas con borde blanco
  geom_text(aes(label = round(r, 2)), size = 3.5) +              # valor de r en cada celda
  scale_fill_gradient2(
    low = "#d73027",                              # rojo: correlación negativa
    mid = "white",                                # blanco: correlación cercana a 0
    high = "#4575b4",                             # azul: correlación positiva
    midpoint = 0, limits = c(-1, 1), name = "r"  # escala centrada en 0
  ) +
  labs(
    title = "Matriz de correlaciones",
    caption = "Fuente: Encuesta CEP N° 93"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))       # etiquetas eje x inclinadas

########################################################################
