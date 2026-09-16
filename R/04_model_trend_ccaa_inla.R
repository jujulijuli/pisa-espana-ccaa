# =============================================================================
# 04_model_trend_ccaa_inla.R
# Tendencia pooled con pendiente aleatoria POR CCAA (no por país, como en el
# proyecto internacional) -- aquí es donde este proyecto aporta algo que el
# internacional no puede: ¿qué comunidades autónomas mejoran/empeoran, con
# qué certeza, y cuánta varianza territorial hay dentro de España?
#
# CAVEAT: cobertura desbalanceada antes de 2015 (ver README.md) -- si se
# incluyen ediciones pre-2015, algunas CCAA tendrán menos puntos temporales
# que otras; el shrinkage de INLA lo maneja razonablemente pero hay que
# advertirlo al interpretar (una CCAA con solo 2 ediciones tendrá un
# intervalo de credibilidad para su pendiente mucho más ancho).
#
# ACTUALIZADO (2026-09-14): ver la nota equivalente en
# ../pisa-maihda-trends/R/04_model_trend_inla.R -- el efecto de ESTRATO es
# ahora trivariante correlacionado (`iid3d`, igual que en
# 03_model_maihda_ccaa_inla.R). Colegio se deja como `iid` simple a
# propósito (barato en estrato -- 72 grupos como mucho, no escala con los
# datos -- caro en colegio, decenas de miles de grupos con las ediciones
# pooled). CCAA se deja igual que antes.
# =============================================================================

library(dplyr)
library(INLA)

# Ver la nota en ../pisa-maihda-trends/R/03_model_maihda_inla.R --
# num.threads="4:1" ya confirmado por Julián que funciona sin crash.
inla.setOption(num.threads = "4:1")

# `domain_labels`/`n_strata_global`/`score_mean`/`score_sd`: mismo motivo y
# mismo patrón que en ../pisa-maihda-trends/R/04_model_trend_inla.R --
# computados sobre el fichero completo, independientemente de si
# 03_model_maihda_ccaa_inla.R ya se ejecutó.
student_long_full <- readRDS("data/student_esp_long.rds")
domain_labels <- levels(student_long_full$domain)
n_strata_global <- n_distinct(student_long_full$strata)
score_mean <- mean(student_long_full$score, na.rm = TRUE)
score_sd <- sd(student_long_full$score, na.rm = TRUE)
rm(student_long_full)

student_long_raw <- readRDS("data/student_esp_long.rds") |>
  filter(!is.na(score)) |>
  mutate(
    year_c = (year - 2018) / 3,   # centrado en 2018 (punto medio del rango con cobertura completa 2015-2022)
    domain_id = as.integer(domain),
    strata_id = as.integer(strata),
    school_id_num = as.integer(factor(global_school_id)),
    ccaa_id = as.integer(factor(ccaa)),
    ccaa_id2 = ccaa_id   # INLA exige índice distinto para cada f()
  )

# Guarda defensiva -- mismo problema que en 03_model_maihda_ccaa_inla.R: si
# falta género/educación parental/ESCS/immig, `strata_id` es NA para esa
# fila, y también lo serían los términos fijos derivados de esas mismas
# variables. Se descartan esas filas en vez de dejar que INLA falle a mitad
# del ajuste pooled.
n_before <- nrow(student_long_raw)
student_long <- student_long_raw |>
  filter(
    !is.na(strata_id), !is.na(school_id_num), !is.na(ccaa_id),
    !is.na(stu_wgt), stu_wgt > 0
  )
n_after <- nrow(student_long)
if (n_after < n_before) {
  message(
    "Modelo de tendencia: ", n_before - n_after, " de ", n_before,
    " filas descartadas por strata/colegio/CCAA/peso muestral ausente o no positivo (",
    scales::percent((n_before - n_after) / n_before, accuracy = 0.1), ")."
  )
}
stopifnot("No queda ninguna fila tras eliminar NA en strata/colegio/CCAA -- revisa el diagnóstico de 02_prepare_variables.R" = n_after > 0)

covars <- c("gender", "parent_educ", "escs_q")
if ("immig" %in% names(student_long)) covars <- c(covars, "immig")
if ("public_private" %in% names(student_long)) covars <- c(covars, "public_private")

# Respuesta estandarizada (`score_z`) -- mismo motivo que en
# ../pisa-maihda-trends/R/04_model_trend_inla.R. `slopes_by_ccaa` queda en
# unidades de `score_z`, se multiplica por `score_sd` en
# R/05_communicate_uncertainty.R para volver a puntos PISA.
formula_trend <- reformulate(
  c("0", "domain", "domain:year_c", covars,
    sprintf("f(strata_domain_id, model = \"iid3d\", n = %d)", 3L * n_strata_global),
    "f(school_id_num, model = \"iid\")",
    "f(ccaa_id, model = \"iid\")",
    "f(ccaa_id2, year_c, model = \"iid\")"),   # pendiente temporal aleatoria POR CCAA
  response = "score_z"
)

student_long <- student_long |>
  mutate(
    strata_domain_id = strata_id + (domain_id - 1L) * n_strata_global,
    score_z = (score - score_mean) / score_sd
  )

# `control.inla`/`dic`/`waic`/`config`: mismas razones que en
# ../pisa-maihda-trends/R/04_model_trend_inla.R -- `config = FALSE`
# (CORRECCIÓN 2026-09-14): no hace falta para inla.rmarginal()/
# inla.tmarginal(), y su coste de memoria es muy probablemente la causa de
# "vector memory limit... reached" al cargar los fits guardados.
# `control.predictor = list(compute = FALSE)` (CORRECCIÓN 2026-09-15, ver
# el comentario largo equivalente en
# ../pisa-maihda-trends/R/04_model_trend_inla.R): nada lee la marginal del
# predictor lineal por fila que guardaba `compute = TRUE`.
fit_trend <- inla(
  formula_trend, data = student_long, family = "gaussian",
  weights = student_long$stu_wgt,
  control.compute = list(dic = FALSE, waic = FALSE, config = FALSE),
  control.predictor = list(compute = FALSE),
  control.inla = list(int.strategy = "eb", cmin = 0)
)

saveRDS(fit_trend, "data/fit_trend_ccaa.rds")
summary(fit_trend)

# --- Extraer pendiente por CCAA con intervalo de credibilidad --------------
# (para el forest plot de qmd/04-resultados.qmd: qué CCAA mejoran/empeoran
# de forma clara -- intervalo que no cruza el cero -- frente a las que no
# hay evidencia suficiente para afirmar nada)
ccaa_lookup <- student_long |> distinct(ccaa, ccaa_id2)

slopes_by_ccaa <- fit_trend$summary.random$ccaa_id2 |>
  as_tibble() |>
  rename(ccaa_id2 = ID) |>
  left_join(ccaa_lookup, by = "ccaa_id2") |>
  select(ccaa, mean, `0.025quant`, `0.975quant`) |>
  rename(slope_mean = mean, slope_lower = `0.025quant`, slope_upper = `0.975quant`) |>
  mutate(significant = slope_lower > 0 | slope_upper < 0) |>
  arrange(slope_mean)

saveRDS(slopes_by_ccaa, "data/slopes_by_ccaa.rds")

message(
  "Pendientes por CCAA: ", nrow(slopes_by_ccaa), " comunidades. ",
  sum(slopes_by_ccaa$significant), " con intervalo de credibilidad que NO cruza el cero ",
  "(", scales::percent(mean(slopes_by_ccaa$significant)), ")."
)
