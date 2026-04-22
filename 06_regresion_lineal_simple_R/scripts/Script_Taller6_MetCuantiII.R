########################################################################/
# Script Taller 6 Métodos Cuantitativos II ------------------------------------
# Escuela de Sociología UV
# M. Constanza Ayala (maria.ayala@uv.cl)
# 22-04-2026
########################################################################/

rm(list = ls()) # Borramos todos los objetos cargados


# Ajustes previos ---------------------------------------------------------

options(scipen = 999) # evita notación científica


# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(sjPlot)


# Base de datos -----------------------------------------------------------

load("data/Casen 2022_filter.Rdata")

# Vista previa
data %>% glimpse()


# Preparación de variables ------------------------------------------------

# Filtramos personas en edad laboral activa (25-65 años)
# con ingreso del trabajo declarado y positivo, y con datos de escolaridad
data_trabajo <- data %>%
  filter(edad >= 25 & edad <= 65,
         !is.na(ytrabajocor),
         ytrabajocor > 0,
         !is.na(esc))

# Recodificamos sexo como factor con etiquetas
data_trabajo <- data_trabajo %>%
  mutate(sexo = factor(sexo,
                       levels = c(1, 2),
                       labels = c("Hombre", "Mujer")))

dim(data_trabajo)


# Exploración descriptiva -------------------------------------------------

# Estadísticos descriptivos de variables continuas
data_trabajo %>%
  select(Ingreso = ytrabajocor, Escolaridad = esc, Edad = edad) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
  summarise(
    Media = mean(valor, na.rm = TRUE),
    DE    = sd(valor, na.rm = TRUE),
    Min   = min(valor, na.rm = TRUE),
    Max   = max(valor, na.rm = TRUE),
    .by = variable
  )

# Distribución por sexo
data_trabajo %>%
  group_by(Sexo = sexo) %>%
  summarise(
    N          = n(),
    Porcentaje = round(100 * n() / nrow(data_trabajo), 1)
  )


# Visualización previa ----------------------------------------------------

# Diagrama de dispersión: escolaridad e ingreso del trabajo
ggplot(data_trabajo, aes(x = esc, y = ytrabajocor)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "lm", color = "steelblue", se = TRUE) +
  scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
  labs(
    title = "Escolaridad e ingreso del trabajo",
    x = "Años de escolaridad",
    y = "Ingreso del trabajo (CLP)",
    caption = "Fuente: Encuesta CASEN 2022"
  ) +
  theme_minimal()


# Estimación de modelos ---------------------------------------------------

# Modelo 1: escolaridad → ingreso del trabajo
m1 <- lm(ytrabajocor ~ esc, data = data_trabajo)

# Modelo 2: edad → ingreso del trabajo
m2 <- lm(ytrabajocor ~ edad, data = data_trabajo)

# Modelo 3: sexo → ingreso del trabajo (variable categórica)
m3 <- lm(ytrabajocor ~ sexo, data = data_trabajo)


# Bondad de ajuste --------------------------------------------------------

summary(m1)


# Tabla de coeficientes ---------------------------------------------------

# Comparación de los tres modelos con coeficientes estandarizados
tab_model(m1, m2, m3,
          show.se  = TRUE,
          show.std = TRUE,
          show.ci  = FALSE,
          dv.labels = c("Modelo 1", "Modelo 2", "Modelo 3"))

# Modelo 3 por separado: variable categórica sexo
tab_model(m3,
          show.se  = TRUE,
          show.std = TRUE,
          show.ci  = FALSE)

########################################################################
