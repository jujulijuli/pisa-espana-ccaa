# =============================================================================
# 02_prepare_variables.R
# Construcción de cuantiles ESCS, educación parental, estratos MAIHDA, y
# formato largo multivariante -- igual que en pisa-maihda-trends, pero con
# `ccaa` como nivel jerárquico real en vez de país.
#
# Asume que 01_load_data.R ya produjo data/student_esp_pooled.rds con
# columnas confirmadas contra el fichero real de 2022 (ver 01_load_data.R):
# year, ccaa, school_id, student_id, global_school_id, gender, immig,
# parent_educ (ya categorizada baja/media/alta), escs, stu_wgt, math, read,
# science.
#
# Si además has ejecutado 01b_incorporate_historic_ccaa.R (añade 2009,
# 2012, 2015 y opcionalmente 2006 -- ver ese script para las limitaciones
# de cada edición histórica), se usa automáticamente
# data/student_esp_pooled_extended.rds en su lugar, que ya incluye 2022
# más esas ediciones -- mismo patrón que `student_pooled_9cycles.rds` en
# pisa-maihda-trends. No hace falta tocar nada más para que el resto del
# pipeline (03/04/05) abarque más ediciones: ya iteran sobre
# `unique(student_prep$year)`.
# =============================================================================

library(dplyr)
library(tidyr)
library(forcats)

path_extended <- "data/student_esp_pooled_extended.rds"
path_2022_only <- "data/student_esp_pooled.rds"
if (file.exists(path_extended)) {
  message("Usando ", path_extended, " (2022 + ediciones históricas con CCAA)")
  student <- readRDS(path_extended)
} else {
  message(
    "Usando solo ", path_2022_only, " -- ejecuta 01b_incorporate_historic_ccaa.R ",
    "si quieres añadir 2009/2012/2015 (y opcionalmente 2006)."
  )
  student <- readRDS(path_2022_only)
}

student_prep <- student |>
  mutate(gender = factor(gender, levels = c("female", "male"))) |>
  group_by(year) |>
  mutate(escs_q = ntile(escs, 4) |> factor(labels = c("Q1_bajo", "Q2", "Q3", "Q4_alto"))) |>
  ungroup() |>
  mutate(
    # Estratos MAIHDA: género x educación parental x cuartil ESCS x origen
    # inmigrante. Ahora incluye `immig` POR DEFECTO (2 x 3 x 4 x 3 = 72
    # estratos posibles), igual que en pisa-maihda-trends -- confirmaste que
    # es importante incluirla en toda la serie, en los dos proyectos. Aquí
    # es más sencillo que en el internacional porque `immig` ya viene en el
    # fichero del INEE, sin necesidad de EdSurvey ni fusión externa. Si
    # prefieres volver a 3 dimensiones (24 estratos, celdas más grandes,
    # comparable 1:1 con una versión anterior del proyecto internacional),
    # quita `immig` de la línea de abajo.
    strata = interaction(gender, parent_educ, escs_q, immig, drop = TRUE) |> fct_drop()
  )

n_strata <- n_distinct(student_prep$strata)
message("Número de estratos interseccionales: ", n_strata,
        " (2 género x 3 educ. parental x 4 ESCS x 3 immig = 72 posibles)")
message("CCAA distintas por edición:")
print(student_prep |> count(year, ccaa) |> count(year, name = "n_ccaa"))

# --- Diagnóstico de NA por edición -------------------------------------------
# `strata` es NA si falta género, educación parental, ESCS o immig para esa
# fila. Si una edición entera se queda sin alguna de esas variables, el
# ajuste MAIHDA de esa edición fallará en INLA con "only NA values in
# strata_id" (03_model_maihda_ccaa_inla.R ya lo detecta y salta esa edición
# en vez de romper todo el pipeline, pero conviene revisar aquí la causa).
na_diag <- student_prep |>
  group_by(year) |>
  summarise(
    n = n(),
    pct_na_gender = scales::percent(mean(is.na(gender)), accuracy = 0.1),
    pct_na_parent_educ = scales::percent(mean(is.na(parent_educ)), accuracy = 0.1),
    pct_na_escs_q = scales::percent(mean(is.na(escs_q)), accuracy = 0.1),
    pct_na_immig = scales::percent(mean(is.na(immig)), accuracy = 0.1),
    pct_na_strata = scales::percent(mean(is.na(strata)), accuracy = 0.1),
    .groups = "drop"
  )
message("Diagnóstico de valores ausentes por edición (revisar si alguna fila da ~100% en pct_na_strata):")
print(na_diag, n = Inf)

# `public_private` (tipo de colegio, de 01_load_data.R): igual que en
# pisa-maihda-trends, NO entra en `strata` (es del colegio, no del alumno),
# se usa como covariable de nivel colegio en los modelos.
has_public_private <- "public_private" %in% names(student_prep)
if (has_public_private) {
  message(
    "public_private disponible -- cobertura: ",
    scales::percent(mean(!is.na(student_prep$public_private))), " de alumnos con colegio clasificado."
  )
}

student_long <- student_prep |>
  select(year, ccaa, school_id, student_id, global_school_id,
         gender, immig, parent_educ, escs_q, strata, stu_wgt,
         any_of("public_private"),
         math, read, science) |>
  pivot_longer(cols = c(math, read, science), names_to = "domain", values_to = "score") |>
  mutate(domain = factor(domain, levels = c("math", "read", "science")))

saveRDS(student_prep, "data/student_esp_prep.rds")
saveRDS(student_long, "data/student_esp_long.rds")

message("student_long: ", nrow(student_long), " filas (alumno x dominio)")
