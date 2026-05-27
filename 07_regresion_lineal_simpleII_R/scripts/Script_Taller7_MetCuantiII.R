########################################################################/
# Script Taller 7 Métodos Cuantitativos II ------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 27-05-2026
########################################################################/

rm(list = ls()) # Borramos todos los objetos cargados


# Ajustes previos ---------------------------------------------------------

options(scipen = 999) # evita notación científica


# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(sjPlot)
library(lmtest)


# Base de datos -----------------------------------------------------------

load("data/Casen 2022_filter.Rdata")

data %>% glimpse()


# Preparación de variables ------------------------------------------------

data_trabajo <- data %>%
  filter(edad >= 25 & edad <= 65,
         !is.na(ytrabajocor),
         ytrabajocor > 0,
         !is.na(esc)) %>%
  mutate(sexo = factor(sexo,
                       levels = c(1, 2),
                       labels = c("Hombre", "Mujer")))

dim(data_trabajo)


# Estimación de modelos ---------------------------------------------------

# Modelo 1: escolaridad → ingreso del trabajo
m1 <- lm(ytrabajocor ~ esc, data = data_trabajo)

# Modelo 3: sexo → ingreso del trabajo (variable categórica)
m3 <- lm(ytrabajocor ~ sexo, data = data_trabajo)


# Tabla de coeficientes ---------------------------------------------------

# Modelo 1: coeficientes no estandarizados y estandarizados
tab_model(m1,
          show.se  = TRUE,
          show.std = TRUE,
          show.ci  = FALSE)

# Modelo 3: variable categórica como dummy
tab_model(m3,
          show.se  = TRUE,
          show.std = TRUE,
          show.ci  = FALSE)


# Supuesto 1: Linealidad --------------------------------------------------

# Diagrama de dispersión con línea de tendencia
ggplot(data_trabajo, aes(x = esc, y = ytrabajocor)) +
  geom_jitter(alpha = 0.05, width = 0.2, height = 0) +
  geom_smooth(method = "lm", color = "steelblue", se = TRUE) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    title   = "Escolaridad e ingreso del trabajo",
    x       = "Años de escolaridad",
    y       = "Ingreso del trabajo (CLP)",
    caption = "Fuente: Encuesta CASEN 2022"
  ) +
  theme_minimal()

# Gráfico de residuos vs. valores predichos
# Línea roja cerca del cero y sin patrón curvo → linealidad satisfecha
# Dispersión creciente → anticipa heterocedasticidad
plot(m1, which = 1)


# Supuesto 2: Independencia de los errores --------------------------------

# Test de Durbin-Watson
# H0: no hay autocorrelación en los residuos
# Estadístico cercano a 2 → independencia
# < 1.5 o > 2.5 → posible autocorrelación

dwtest(m1)

# Resultado: DW = 1.65, p < .001
# Autocorrelación positiva leve, atribuible al diseño multietápico por conglomerados de CASEN


# Supuesto 3: Homocedasticidad --------------------------------------------

# Test de Breusch-Pagan
# H0: varianza de los errores es constante (homocedasticidad)
# p < 0.05 → rechazamos H0 → heterocedasticidad

bptest(m1)

# Resultado: BP = 134.24, p < .001
# Se rechaza H0: hay heterocedasticidad
# La variabilidad del ingreso crece con la escolaridad (patrón de embudo)
# Los errores estándar no son confiables → debe reconocerse al reportar


# Supuesto 4: Normalidad de los residuos ----------------------------------

# Gráfico Q-Q de los residuos
# Puntos sobre la diagonal → normalidad
# Desviaciones en colas → no normalidad

plot(m1, which = 2)

# Resultado: desviación grave en cola derecha
# Residuos estandarizados hasta ~70 (casos 43615, 41422, 46564)
# Supuesto no se cumple, pero con n = 75.080 el TCL protege la inferencia


# Supuesto 5: Ausencia de valores influyentes -----------------------------

# Distancia de Cook
# D_i > 1 → caso potencialmente influyente

plot(m1, which = 4)

# Resultado: ningún caso supera D_i > 1
# Máximo: D_i ≈ 0.07 (caso 41422)
# Supuesto se cumple; casos 41422, 43615 y 41421 son los más influyentes

########################################################################
