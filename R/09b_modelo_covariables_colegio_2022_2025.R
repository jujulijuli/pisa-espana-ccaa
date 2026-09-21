# =============================================================================
# 09b_modelo_covariables_colegio_2022_2025.R
# Respuesta a la pregunta de Julián (2026-09-21): "la distracción agregada de
# colegio no existe en 2025, pero ¿no podría incorporarse con los colegios
# del fichero internacional de 2025?" -- SÍ, y de hecho no hace falta ningún
# fichero de CENTROS nuevo: el agregado de colegio de 06_exposicion_digital.R
# ya sale de PROMEDIAR el ítem de alumno (`ST097Q06DA`/`distr_propia`) por
# colegio -- y ese ítem de alumno para 2025 ya está extraído y guardado en
# `data/student_distraccion_2022_2025.rds` (pieza C de 06). No hace falta
# tocar el fichero de centros (`CY09_MS_SCH_PUF`) para esto -- ese fichero
# solo hacía falta para `public_private`, que 2025 ya trae desde
# 01d_incorporate_pisa2025_ccaa.R.
#
# EL LÍMITE REAL, y por qué esto es un script NUEVO en vez de una edición de
# 09_modelo_covariables_colegio.R: `distr_pares` (distracción por
# COMPAÑEROS, ST273Q07JA en 2022) NO tiene equivalente en la batería de 2025
# -- verificado en 06_exposicion_digital.R, no ha cambiado. El M3/M4 original
# de 09 usa `distr_propia_agg_c` Y `distr_pares_agg_c` a la vez. Pooling
# 2022+2025 con esa especificación dejaría a TODOS los alumnos de 2025 con
# `distr_pares_agg_c = NA` y los filtraría fuera silenciosamente (mismo tipo
# de bug que ya nos pasó una vez con el género de 2025, ver
# 01d_incorporate_pisa2025_ccaa.R) -- así que no se puede simplemente "correr
# 09 con más años". La solución, siguiendo el mismo criterio no destructivo
# del resto del proyecto: 09_modelo_covariables_colegio.R se deja EXACTAMENTE
# igual (2022, con distr_propia + distr_pares, sigue siendo la referencia
# para esa pregunta completa), y aquí se ajusta un M3/M4 REDEFINIDO -- solo
# con distr_propia_agg_c, sin distr_pares_agg_c -- por separado en 2022 y en
# 2025, para poder comparar los dos años con la misma especificación exacta
# (si comparásemos el M3/M4 original de 2022, con pares, contra uno sin
# pares en 2025, la diferencia observada podría deberse solo a quitar esa
# covariable, no a un cambio real entre ediciones).
#
# Requiere haber ejecutado antes: 01_load_data.R, 01b/01c/01d, 02, y
# 06_exposicion_digital.R (para student_distraccion_2022_2025.rds).
#
# COSTE COMPUTACIONAL: el doble de 09 (2 ediciones x 3 dominios x 6 modelos
# = 36 ajustes INLA, cada uno más ligero que los de 03/04) -- 09 tardaba
# "minutos, no horas"; espera algo más aquí, pero del mismo orden. Ejecuta
# aparte de un render de Quarto, como 09.
#
# AÑADIDO 2026-09-21 a petición de Julián: público/privado se separa de M1
# ("individual") en su propio bloque M2, con el resto de modelos
# renumerados M3/M4/M5 -- ver la nota completa junto a `fit_domain_year()`.
# =============================================================================

library(dplyr)
library(tidyr)
library(INLA)

inla.setOption(num.threads = "4:1")

stopifnot(
  "Necesito data/student_esp_prep.rds -- ejecuta antes 02_prepare_variables.R" =
    file.exists("data/student_esp_prep.rds"),
  "Necesito data/student_distraccion_2022_2025.rds -- ejecuta antes 06_exposicion_digital.R (con datos de 2025 ya incorporados)" =
    file.exists("data/student_distraccion_2022_2025.rds")
)

student_all      <- readRDS("data/student_esp_prep.rds") |> filter(year %in% c(2022L, 2025L))
distraccion_all  <- readRDS("data/student_distraccion_2022_2025.rds")  # year, student_id, school_id, distr_propia, [horas_redes], distr_pares (solo 2022, NA en 2025)

stopifnot(
  "Faltan alumnos de 2025 en student_esp_prep.rds -- ejecuta 01d_incorporate_pisa2025_ccaa.R" =
    2025L %in% unique(student_all$year),
  "Faltan alumnos de 2025 en student_distraccion_2022_2025.rds -- revisa la pieza (C) de 06_exposicion_digital.R" =
    2025L %in% unique(distraccion_all$year)
)

distr_to_num <- function(x) {
  as.numeric(factor(x, levels = c("Nunca/casi nunca", "Algunas clases", "La mayoría de clases", "Cada clase"))) - 1
}

# --- Covariables de nivel colegio, por año (a diferencia de 09, aquí SOLO
# distr_propia -- ver aviso arriba sobre por qué se deja fuera distr_pares) --
build_school_covars <- function(yr) {
  student <- student_all |> filter(year == yr)

  school_pct_migrante <- student |>
    filter(!is.na(immig), !is.na(global_school_id)) |>
    group_by(global_school_id) |>
    summarise(pct_migrante_colegio = mean(immig %in% c("primera_gen", "segunda_gen")), .groups = "drop")
  media_pct_migrante <- mean(school_pct_migrante$pct_migrante_colegio)
  school_pct_migrante <- school_pct_migrante |>
    mutate(pct_migrante_colegio_c = pct_migrante_colegio - media_pct_migrante)

  school_distraccion <- distraccion_all |>
    filter(year == yr, !is.na(distr_propia)) |>
    mutate(distr_propia_num = distr_to_num(distr_propia)) |>
    left_join(student |> select(school_id, global_school_id) |> distinct(), by = "school_id") |>
    filter(!is.na(global_school_id)) |>
    group_by(global_school_id) |>
    summarise(distr_propia_agg = mean(distr_propia_num, na.rm = TRUE), .groups = "drop") |>
    mutate(distr_propia_agg_c = distr_propia_agg - mean(distr_propia_agg, na.rm = TRUE))

  dat <- student |>
    left_join(school_pct_migrante, by = "global_school_id") |>
    left_join(school_distraccion, by = "global_school_id") |>
    mutate(ccaa_id = as.integer(factor(ccaa)))

  message(
    "[", yr, "] Colegios con % migrante: ", nrow(school_pct_migrante),
    " | colegios con distracción propia agregada: ", nrow(school_distraccion)
  )
  dat
}

extract_vpc_simple <- function(fit, effect_name) {
  hp <- fit$marginals.hyperpar[[paste0("Precision for ", effect_name)]]
  if (is.null(hp)) return(tibble(vpc_mean = NA_real_, vpc_lower = NA_real_, vpc_upper = NA_real_))
  var_effect <- inla.tmarginal(function(x) 1 / x, hp)
  var_resid  <- inla.tmarginal(function(x) 1 / x, fit$marginals.hyperpar$`Precision for the Gaussian observations`)
  samp_effect <- inla.rmarginal(4000, var_effect)
  samp_resid  <- inla.rmarginal(4000, var_resid)
  vpc_samples <- samp_effect / (samp_effect + samp_resid)
  tibble(vpc_mean = mean(vpc_samples), vpc_lower = quantile(vpc_samples, 0.025), vpc_upper = quantile(vpc_samples, 0.975))
}

fit_stage <- function(dat, formula_rhs) {
  dat <- dat |> filter(!is.na(stu_wgt), stu_wgt > 0)
  dat <- dat |> mutate(school_id_num = as.integer(factor(global_school_id)))
  f <- as.formula(paste("score_z ~", formula_rhs))
  inla(
    f, data = dat, family = "gaussian", weights = dat$stu_wgt,
    control.compute = list(dic = FALSE, waic = FALSE, config = FALSE),
    control.predictor = list(compute = FALSE),
    control.inla = list(int.strategy = "eb", cmin = 0)
  )
}

# M4/M5 REDEFINIDOS -- sin distr_pares_agg_c (ver aviso). Igual que en 09,
# pero con este único cambio, para que 2022 y 2025 sean comparables entre sí
# con la MISMA especificación exacta (el 09 original, con pares incluido,
# sigue siendo la referencia completa de 2022, sin tocar). AÑADIDO
# 2026-09-21 a petición de Julián: público/privado sale de M1 y pasa a su
# propio bloque M2 (mismo motivo que en 09 -- es una covariable de nivel
# colegio, no individual), renumerando el resto.
fit_domain_year <- function(domain, yr, dat_full) {
  message("\n=== [", yr, "] Dominio: ", domain, " ===")

  d0 <- dat_full |>
    filter(!is.na(.data[[domain]]), !is.na(global_school_id), !is.na(ccaa_id), !is.na(stu_wgt), stu_wgt > 0) |>
    mutate(score_z_full = as.numeric(scale(.data[[domain]])))
  sd_domain <- sd(d0[[domain]], na.rm = TRUE)

  common_re <- "f(school_id_num, model = \"iid\") + f(ccaa_id, model = \"iid\")"

  d_m0 <- d0 |> mutate(score_z = score_z_full)
  fit_m0 <- fit_stage(d_m0, common_re)

  # M1: + individual (SIN público/privado -- ver M2)
  d_m1 <- d0 |>
    filter(!is.na(gender), !is.na(parent_educ), !is.na(escs_q), !is.na(immig)) |>
    mutate(score_z = score_z_full)
  fit_m1 <- fit_stage(d_m1, paste("0 + gender + parent_educ + escs_q + immig +", common_re))

  # M2 (NUEVO): + público/privado
  d_m2 <- d_m1 |> filter(!is.na(public_private))
  message("[", yr, "] ", domain, " -- M2 (+público/privado): ", nrow(d_m2), " alumnos, ", n_distinct(d_m2$global_school_id), " colegios")
  fit_m2 <- fit_stage(d_m2, paste("0 + gender + parent_educ + escs_q + immig + public_private +", common_re))

  d_m3 <- d_m2 |> filter(!is.na(pct_migrante_colegio_c))
  fit_m3 <- fit_stage(d_m3, paste("0 + gender + parent_educ + escs_q + immig + public_private + pct_migrante_colegio_c +", common_re))

  # M4 (redefinido): + distracción PROPIA agregada del colegio, sin pares.
  d_m4 <- d_m3 |> filter(!is.na(distr_propia_agg_c))
  message("[", yr, "] ", domain, " -- M4 (+distracción propia colegio): ", nrow(d_m4), " alumnos, ", n_distinct(d_m4$global_school_id), " colegios")
  fit_m4 <- fit_stage(d_m4, paste("0 + gender + parent_educ + escs_q + immig + public_private + pct_migrante_colegio_c + distr_propia_agg_c +", common_re))

  fit_m5 <- fit_stage(
    d_m4,
    paste("0 + gender + parent_educ + escs_q + immig + public_private + pct_migrante_colegio_c + distr_propia_agg_c",
          "+ immig:pct_migrante_colegio_c +", common_re)
  )

  vpc_stages <- bind_rows(
    extract_vpc_simple(fit_m0, "school_id_num") |> mutate(modelo = "M0_nulo", n = nrow(d_m0), .before = 1),
    extract_vpc_simple(fit_m1, "school_id_num") |> mutate(modelo = "M1_individual", n = nrow(d_m1), .before = 1),
    extract_vpc_simple(fit_m2, "school_id_num") |> mutate(modelo = "M2_pubpriv", n = nrow(d_m2), .before = 1),
    extract_vpc_simple(fit_m3, "school_id_num") |> mutate(modelo = "M3_segregacion", n = nrow(d_m3), .before = 1),
    extract_vpc_simple(fit_m4, "school_id_num") |> mutate(modelo = "M4_distraccion_propia", n = nrow(d_m4), .before = 1),
    extract_vpc_simple(fit_m5, "school_id_num") |> mutate(modelo = "M5_interaccion", n = nrow(d_m4), .before = 1)
  ) |> mutate(domain = domain, year = yr, .before = 1)

  interaccion <- fit_m5$summary.fixed |>
    tibble::as_tibble(rownames = "term") |>
    filter(grepl("^immig.*:pct_migrante_colegio_c$", term)) |>
    transmute(
      domain = domain, year = yr, term,
      estimate = mean, ci_low = `0.025quant`, ci_high = `0.975quant`,
      estimate_puntos = mean * sd_domain, ci_low_puntos = `0.025quant` * sd_domain, ci_high_puntos = `0.975quant` * sd_domain
    )

  distr_propia_efecto <- fit_m4$summary.fixed |>
    tibble::as_tibble(rownames = "term") |>
    filter(term == "distr_propia_agg_c") |>
    transmute(
      domain = domain, year = yr, term,
      estimate = mean, ci_low = `0.025quant`, ci_high = `0.975quant`,
      estimate_puntos = mean * sd_domain, ci_low_puntos = `0.025quant` * sd_domain, ci_high_puntos = `0.975quant` * sd_domain
    )

  # Efecto de público/privado (M2), por edición -- mismo patrón que en 09.
  pubpriv_efecto <- fit_m2$summary.fixed |>
    tibble::as_tibble(rownames = "term") |>
    filter(grepl("^public_private", term)) |>
    transmute(
      domain = domain, year = yr, term,
      estimate = mean, ci_low = `0.025quant`, ci_high = `0.975quant`,
      estimate_puntos = mean * sd_domain, ci_low_puntos = `0.025quant` * sd_domain, ci_high_puntos = `0.975quant` * sd_domain
    )

  list(domain = domain, year = yr, sd_domain_puntos = sd_domain, vpc_stages = vpc_stages,
       interaccion = interaccion, distr_propia_efecto = distr_propia_efecto, pubpriv_efecto = pubpriv_efecto,
       fits = list(m0 = fit_m0, m1 = fit_m1, m2 = fit_m2, m3 = fit_m3, m4 = fit_m4, m5 = fit_m5))
}

domains <- c("math", "read", "science")
years   <- c(2022L, 2025L)

resultados <- list()
for (yr in years) {
  dat_full_yr <- build_school_covars(yr)
  for (dom in domains) {
    key <- paste(dom, yr, sep = "_")
    resultados[[key]] <- fit_domain_year(dom, yr, dat_full_yr)
  }
}

tbl_vpc_escuela_covariables_22_25 <- purrr::map_dfr(resultados, "vpc_stages")
tbl_interaccion_immig_segregacion_22_25 <- purrr::map_dfr(resultados, "interaccion")
tbl_distr_propia_colegio_22_25 <- purrr::map_dfr(resultados, "distr_propia_efecto")
tbl_pubpriv_efecto_colegio_22_25 <- purrr::map_dfr(resultados, "pubpriv_efecto")

message("\n=== VPC de colegio por dominio, modelo anidado y edición (M4/M5 redefinidos, solo distr_propia) ===")
print(tbl_vpc_escuela_covariables_22_25, n = Inf)

message("\n=== Efecto de la distracción propia agregada del colegio (M4), 2022 vs 2025 ===")
print(tbl_distr_propia_colegio_22_25, n = Inf)

message("\n=== Efecto de público/privado (M2), 2022 vs 2025 -- privado frente a público ===")
print(tbl_pubpriv_efecto_colegio_22_25, n = Inf)

message("\n=== Interacción origen migrante x % migrante del colegio (M5), 2022 vs 2025 ===")
print(tbl_interaccion_immig_segregacion_22_25, n = Inf)

saveRDS(tbl_vpc_escuela_covariables_22_25, "data/tbl_vpc_escuela_covariables_2022_2025.rds")
saveRDS(tbl_interaccion_immig_segregacion_22_25, "data/tbl_interaccion_immig_segregacion_2022_2025.rds")
saveRDS(tbl_distr_propia_colegio_22_25, "data/tbl_distr_propia_colegio_2022_2025.rds")
saveRDS(tbl_pubpriv_efecto_colegio_22_25, "data/tbl_pubpriv_efecto_colegio_2022_2025.rds")
saveRDS(resultados, "data/modelo_covariables_colegio_fits_2022_2025.rds")

# --- Figura de comparación: VPC de colegio a través de los modelos, por
# dominio y edición ------------------------------------------------------
library(ggplot2)
fig_vpc_stages_22_25 <- tbl_vpc_escuela_covariables_22_25 |>
  mutate(
    modelo_lab = recode(modelo,
      M0_nulo = "M0: nulo", M1_individual = "M1: +individual",
      M2_pubpriv = "M2: +público/privado",
      M3_segregacion = "M3: +% migrante colegio", M4_distraccion_propia = "M4: +distracción propia colegio",
      M5_interaccion = "M5: +interacción"
    ) |> factor(levels = c("M0: nulo", "M1: +individual", "M2: +público/privado", "M3: +% migrante colegio", "M4: +distracción propia colegio", "M5: +interacción")),
    domain_lab = recode(domain, math = "Matemáticas", read = "Lectura", science = "Ciencias"),
    year_lab = factor(year, levels = c(2022, 2025))
  ) |>
  ggplot(aes(x = modelo_lab, y = vpc_mean, ymin = vpc_lower, ymax = vpc_upper, color = year_lab)) +
  geom_pointrange(position = position_dodge(width = 0.4)) +
  facet_wrap(~domain_lab) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "VPC de colegio según qué covariables se van añadiendo -- 2022 vs 2025",
    subtitle = "M4/M5 redefinidos (solo distracción propia, sin distracción por compañeros -- no existe en 2025)",
    x = NULL, y = "VPC de colegio (IC 95% credibilidad)", color = "Edición"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1), legend.position = "bottom")
ggsave("data/fig_vpc_escuela_covariables_2022_2025.png", fig_vpc_stages_22_25, width = 10, height = 6, dpi = 150)

message(
  "\nGuardado: data/tbl_vpc_escuela_covariables_2022_2025.rds, ",
  "data/tbl_interaccion_immig_segregacion_2022_2025.rds, data/tbl_distr_propia_colegio_2022_2025.rds, ",
  "data/tbl_pubpriv_efecto_colegio_2022_2025.rds, ",
  "data/modelo_covariables_colegio_fits_2022_2025.rds, data/fig_vpc_escuela_covariables_2022_2025.png"
)
