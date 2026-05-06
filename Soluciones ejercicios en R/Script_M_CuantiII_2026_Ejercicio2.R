########################################################################/
# Script Solución Ejercicio 2 Métodos Cuantitativos II --------------------
# M. Constanza Ayala (maria.ayala@uv.cl)
# 08-04-2026
########################################################################/

rm(list = ls())


# Paquetes ----------------------------------------------------------------

library(dplyr)
library(haven)


# Base de datos -----------------------------------------------------------

base_93 <- readRDS("data/base_93.Rds")


# Exploración base de datos -----------------------------------------------

base_93 %>% glimpse()


# Mutate 1 ----------------------------------------------------------------
# Recodificación de valores perdidos en ítems de bienestar en el barrio

base_93 <- base_93 %>%
  mutate(
    bienestar_23_a = case_when(
      bienestar_23_a %in% c(-7, -8, -9) ~ NA_real_,
      TRUE ~ bienestar_23_a
    ),
    bienestar_23_b = case_when(
      bienestar_23_b %in% c(-7, -8, -9) ~ NA_real_,
      TRUE ~ bienestar_23_b
    ),
    bienestar_23_c = case_when(
      bienestar_23_c %in% c(-7, -8, -9) ~ NA_real_,
      TRUE ~ bienestar_23_c
    ),
    bienestar_23_d = case_when(
      bienestar_23_d %in% c(-7, -8, -9) ~ NA_real_,
      TRUE ~ bienestar_23_d
    )
  )

# Verificamos que los valores -8 y -9 ya no aparezcan
table(base_93$bienestar_23_a, exclude = FALSE)


# Mutate 2 ----------------------------------------------------------------
# Índice de preocupación por el barrio

# Opción A: índice sumatorio
base_93 <- base_93 %>%
  mutate(indice_preocupacion_barrio = bienestar_23_a + bienestar_23_b +
           bienestar_23_c + bienestar_23_d)

# Opción B: índice promedio
# base_93 <- base_93 %>%
#   mutate(indice_preocupacion_barrio = (bienestar_23_a + bienestar_23_b +
#                                          bienestar_23_c + bienestar_23_d) / 4)

summary(base_93$indice_preocupacion_barrio)


# Mutate 3 ----------------------------------------------------------------
# Recodificación de valores perdidos y categorización ideológica

base_93 <- base_93 %>%
  mutate(iden_pol_2 = case_when(
    iden_pol_2 %in% c(-8, -9) ~ NA_real_,
    TRUE ~ iden_pol_2
  )) %>%
  mutate(posicion_ideologica = case_when(
    iden_pol_2 %in% 1:4  ~ "Izquierda",
    iden_pol_2 == 5       ~ "Centro",
    iden_pol_2 %in% 6:10  ~ "Derecha",
    TRUE                  ~ NA_character_
  ))

table(base_93$posicion_ideologica, exclude = FALSE)

########################################################################/
