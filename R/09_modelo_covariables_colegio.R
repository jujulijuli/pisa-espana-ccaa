# =============================================================================
# 09_modelo_covariables_colegio.R
# Pieza pedida por Julián 2026-09-16: modelo mixto con variables de NIVEL
# COLEGIO (concentración de alumnado migrante, distracción agregada) e
# interacción con el origen migrante individual, para ver cómo influyen
# sobre la PARTICIÓN DE VARIANZA (VPC de colegio) -- análisis separado por
# resultado (matemáticas/lectura/ciencia), como pidió expresamente.
#
# DISEÑO Y DECISIONES, para que quede documentado por qué es así y no de
# otra forma:
#
#  - Solo 2022. La distracción agregada (H2) solo existe esa edición (ver
#    R/06_exposicion_digital.R) -- no tiene sentido un modelo con variables
#    de nivel colegio que solo están completas en una de las cuatro
#    ediciones, así que TODO este script es transversal 2022. Si en el
#    futuro se consigue un proxy de distracción comparable en ediciones
#    anteriores, este script se podría extender a serie temporal como
#    03/04_model_*_inla.R.
#  - Modelos SEPARADOS por dominio (3 ajustes independientes univariantes),
#    no el efecto trivariante iid3d de 03_model_maihda_ccaa_inla.R -- es lo
#    que Julián pidió explícitamente ("análisis separado por resultado"), y
#    además evita triplicar el coste computacional de un problema que ya de
#    por sí tiene más covariables que el modelo principal.
#  - El efecto de ESTRATO INTERSECCIONAL (género x educ. parental x ESCS x
#    immig, ver 02_prepare_variables.R) NO se incluye aquí -- esa pregunta
#    (desigualdad social) ya está respondida en 03/04/qmd/04-resultados.qmd.
#    Aquí la pregunta es otra: cuánta VARIANZA ENTRE COLEGIOS explican la
#    composición migrante del colegio y la distracción agregada del colegio,
#    así que el efecto aleatorio de interés es el de COLEGIO (`iid` simple,
#    mismo criterio de velocidad que 03), con CCAA como aleatorio adicional
#    de control (no es el foco, pero conviene no atribuirle al colegio
#    varianza que en realidad es territorial).
#  - SEIS modelos anidados por dominio (revisado 2026-09-21 a petición de
#    Julián: público/privado tenía su propio bloque, no compartido con lo
#    individual), cada uno añadiendo un bloque, para poder leer cuánto BAJA
#    el VPC de colegio (o el PCV respecto al modelo anterior) al añadir cada
#    bloque:
#      M0 (nulo):         solo aleatorios (colegio, CCAA), sin covariables
#      M1 (individual):   M0 + género + educ. parental + ESCS (cuartil) +
#                          origen migrante -- covariables que varían DENTRO
#                          de un mismo colegio (ningún alumno de un centro
#                          comparte necesariamente género/ESCS con otro)
#      M2 (+pub./priv.):  M1 + titularidad del colegio (público/privado).
#                          Antes iba mezclado dentro de M1 -- se separa
#                          porque, a diferencia de género/ESCS/origen
#                          migrante, es una covariable de NIVEL COLEGIO
#                          (mismo valor para todo el alumnado de un centro,
#                          igual que la segregación de M3 o la distracción
#                          de M4), así que fusionarla con lo individual
#                          escondía cuánta VPC de colegio explica por sí
#                          sola la titularidad.
#      M3 (+segregación): M2 + % alumnado migrante DEL COLEGIO (centrado)
#      M4 (+distracción): M3 + distracción propia/pares agregada DEL COLEGIO
#                          (medias del colegio, centradas)
#      M5 (+interacción): M4 + origen migrante x % migrante del colegio
#                          (¿el alumnado migrante rinde peor específicamente
#                          en colegios muy concentrados, más allá del efecto
#                          de cada cosa por separado?)
#  - Respuesta estandarizada (`score_z`) por dominio, DENTRO de la muestra
#    analítica de ESTE script (2022, tras filtrar NA de todas las
#    covariables) -- se guarda su DE en puntos (`sd_domain_puntos`, mismo
#    patrn que 06/07) para poder traducir los coeficientes a puntos PISA.
#
# COSTE COMPUTACIONAL: 3 dominios x 5 modelos = 15 ajustes INLA, pero cada
# uno es mucho más ligero que los de 03/04 (un único año, ~30.000 filas,
# efecto de colegio `iid` simple, sin trivariante) -- debería tardar
# minutos, no horas, y no debería necesitar el mem.maxVSize ampliado que
# hizo falta para 03/04. Aun así, ejecuta esto aparte (no dentro de un
# render de Quarto) y ven con el resultado -- si tarda mucho más de lo
# esperado, dímelo con el mensaje de progreso que imprime cada ajuste.
# =============================================================================

library(dplyr)
library(tidyr)
library(INLA)

inla.setOption(num.threads = "4:1")

stopifnot(
  "Necesito data/student_esp_prep.rds -- ejecuta antes 02_prepare_variables.R" =
    file.exists("data/student_esp_prep.rds"),
  "Necesito data/student_distraccion_2022.rds -- ejecuta antes 06_exposicion_digital.R" =
    file.exists("data/student_distraccion_2022.rds")
)

student <- readRDS("data/student_esp_prep.rds") |> filter(year == 2022)
distraccion <- readRDS("data/student_distraccion_2022.rds")

# --- Preparación de covariables de nivel colegio ------------------------------
# % de alumnado migrante (1ª+2ª gen) del colegio, en 2022 -- mismo criterio
# "amplio" que 05_segregacion_heterogeneidad.R. Centrado sobre la media DE
# COLEGIOS (no de alumnos, para no ponderar implícitamente por tamaño).
school_pct_migrante <- student |>
  filter(!is.na(immig), !is.na(global_school_id)) |>
  group_by(global_school_id) |>
  summarise(pct_migrante_colegio = mean(immig %in% c("primera_gen", "segunda_gen")), .groups = "drop")
media_pct_migrante_colegios <- mean(school_pct_migrante$pct_migrante_colegio)
school_pct_migrante <- school_pct_migrante |>
  mutate(pct_migrante_colegio_c = pct_migrante_colegio - media_pct_migrante_colegios)

# Distracción agregada del colegio: recodifica el factor de
# 06_exposicion_digital.R (Nunca/casi nunca=0 ... Cada clase=3) y promedia
# por colegio (sin ponderar -- es un agregado descriptivo del clima de aula,
# no una estimación poblacional).
distr_to_num <- function(x) {
  as.numeric(factor(x, levels = c("Nunca/casi nunca", "Algunas clases", "La mayoría de clases", "Cada clase"))) - 1
}
school_distraccion <- distraccion |>
  filter(!is.na(distr_propia) | !is.na(distr_pares)) |>
  mutate(distr_propia_num = distr_to_num(distr_propia), distr_pares_num = distr_to_num(distr_pares)) |>
  left_join(student |> select(school_id, global_school_id) |> distinct(), by = "school_id") |>
  filter(!is.na(global_school_id)) |>
  group_by(global_school_id) |>
  summarise(
    distr_propia_agg = mean(distr_propia_num, na.rm = TRUE),
    distr_pares_agg = mean(distr_pares_num, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    distr_propia_agg_c = distr_propia_agg - mean(distr_propia_agg, na.rm = TRUE),
    distr_pares_agg_c = distr_pares_agg - mean(distr_pares_agg, na.rm = TRUE)
  )

dat_full <- student |>
  left_join(school_pct_migrante, by = "global_school_id") |>
  left_join(school_distraccion, by = "global_school_id") |>
  mutate(ccaa_id = as.integer(factor(ccaa)))

message(
  "Colegios con % migrante calculado: ", nrow(school_pct_migrante),
  " | colegios con distracción agregada: ", nrow(school_distraccion),
  " (la distracción agregada reduce algo la muestra analítica de M3/M4 frente a M0-M2)"
)

# --- Ajuste anidado por dominio -----------------------------------------------
domains <- c("math", "read", "science")

# Ver el comentario largo sobre robustez de nombres de hiperparámetros en
# 03_model_maihda_ccaa_inla.R -- misma lógica, reutilizada aquí tal cual
# porque el efecto de colegio también es `iid` simple en este script.
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

fit_stage <- function(dat, formula_rhs, dat_desc) {
  n_before <- nrow(dat)
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

fit_domain <- function(domain) {
  message("\n=== Dominio: ", domain, " ===")

  d0 <- dat_full |>
    filter(
      !is.na(.data[[domain]]), !is.na(global_school_id), !is.na(ccaa_id),
      !is.na(stu_wgt), stu_wgt > 0
    ) |>
    mutate(score_z_full = as.numeric(scale(.data[[domain]])))
  # `score_z_full` se calcula UNA VEZ aquí y se reutiliza sin recalcular en
  # M0-M4 (cada modelo es un subconjunto de filas de `d0`, nunca una
  # reestandarización) -- así que `sd_domain` es la escala real de los 5
  # modelos, no solo de los primeros.
  sd_domain <- sd(d0[[domain]], na.rm = TRUE)

  common_re <- "f(school_id_num, model = \"iid\") + f(ccaa_id, model = \"iid\")"

  # M0: nulo
  d_m0 <- d0 |> mutate(score_z = score_z_full) |> filter(!is.na(stu_wgt), stu_wgt > 0)
  message("M0 (nulo): ", nrow(d_m0), " alumnos, ", n_distinct(d_m0$global_school_id), " colegios")
  fit_m0 <- fit_stage(d_m0, common_re, "M0")

  # M1: + individual (SIN público/privado -- ver M2)
  d_m1 <- d0 |>
    filter(!is.na(gender), !is.na(parent_educ), !is.na(escs_q), !is.na(immig)) |>
    mutate(score_z = score_z_full)
  message("M1 (+individual): ", nrow(d_m1), " alumnos")
  fit_m1 <- fit_stage(d_m1, paste("0 + gender + parent_educ + escs_q + immig +", common_re), "M1")

  # M2: + público/privado -- AÑADIDO 2026-09-21 (antes iba dentro de M1; ver
  # nota en el bloque de documentación del principio del script sobre por
  # qué se separa como covariable de nivel colegio).
  d_m2 <- d_m1 |> filter(!is.na(public_private))
  message("M2 (+público/privado): ", nrow(d_m2), " alumnos, ", n_distinct(d_m2$global_school_id), " colegios")
  fit_m2 <- fit_stage(d_m2, paste("0 + gender + parent_educ + escs_q + immig + public_private +", common_re), "M2")

  # M3: + % migrante colegio
  d_m3 <- d_m2 |> filter(!is.na(pct_migrante_colegio_c))
  message("M3 (+segregación colegio): ", nrow(d_m3), " alumnos, ", n_distinct(d_m3$global_school_id), " colegios")
  fit_m3 <- fit_stage(d_m3, paste("0 + gender + parent_educ + escs_q + immig + public_private + pct_migrante_colegio_c +", common_re), "M3")

  # M4: + distracción agregada colegio (reduce la N -- solo colegios con ítem 2022 de distracción)
  d_m4 <- d_m3 |> filter(!is.na(distr_propia_agg_c), !is.na(distr_pares_agg_c))
  message("M4 (+distracción colegio): ", nrow(d_m4), " alumnos, ", n_distinct(d_m4$global_school_id), " colegios")
  fit_m4 <- fit_stage(
    d_m4,
    paste("0 + gender + parent_educ + escs_q + immig + public_private + pct_migrante_colegio_c + distr_propia_agg_c + distr_pares_agg_c +", common_re),
    "M4"
  )

  # M5: + interacción immig x % migrante colegio
  message("M5 (+interacción immig x segregación): ", nrow(d_m4), " alumnos")
  fit_m5 <- fit_stage(
    d_m4,
    paste(
      "0 + gender + parent_educ + escs_q + immig + public_private + pct_migrante_colegio_c + distr_propia_agg_c + distr_pares_agg_c",
      "+ immig:pct_migrante_colegio_c +", common_re
    ),
    "M5"
  )

  vpc_stages <- bind_rows(
    extract_vpc_simple(fit_m0, "school_id_num") |> mutate(modelo = "M0_nulo", n = nrow(d_m0), .before = 1),
    extract_vpc_simple(fit_m1, "school_id_num") |> mutate(modelo = "M1_individual", n = nrow(d_m1), .before = 1),
    extract_vpc_simple(fit_m2, "school_id_num") |> mutate(modelo = "M2_pubpriv", n = nrow(d_m2), .before = 1),
    extract_vpc_simple(fit_m3, "school_id_num") |> mutate(modelo = "M3_segregacion", n = nrow(d_m3), .before = 1),
    extract_vpc_simple(fit_m4, "school_id_num") |> mutate(modelo = "M4_distraccion", n = nrow(d_m4), .before = 1),
    extract_vpc_simple(fit_m5, "school_id_num") |> mutate(modelo = "M5_interaccion", n = nrow(d_m4), .before = 1)
  ) |> mutate(domain = domain, .before = 1)

  interaccion <- fit_m5$summary.fixed |>
    tibble::as_tibble(rownames = "term") |>
    filter(grepl("^immig.*:pct_migrante_colegio_c$", term)) |>
    transmute(
      domain = domain, term,
      estimate = mean, ci_low = `0.025quant`, ci_high = `0.975quant`,
      estimate_puntos = mean * sd_domain, ci_low_puntos = `0.025quant` * sd_domain, ci_high_puntos = `0.975quant` * sd_domain
    )

  # Efecto de público/privado (M2), en puntos -- AÑADIDO 2026-09-21, mismo
  # patrón que `interaccion` arriba. `public_private` (niveles "publico" /
  # "privado") no es el primer factor de la fórmula, así que con "0 +" R le
  # aplica codificación de tratamiento estándar (k-1 columnas): solo
  # aparece un término, ya interpretable directamente como "privado frente
  # a público" (referencia). Se usa `grepl` en vez de un nombre exacto por
  # si el sufijo de nivel cambia de versión de R/INLA.
  pubpriv_efecto <- fit_m2$summary.fixed |>
    tibble::as_tibble(rownames = "term") |>
    filter(grepl("^public_private", term)) |>
    transmute(
      domain = domain, term,
      estimate = mean, ci_low = `0.025quant`, ci_high = `0.975quant`,
      estimate_puntos = mean * sd_domain, ci_low_puntos = `0.025quant` * sd_domain, ci_high_puntos = `0.975quant` * sd_domain
    )

  list(domain = domain, sd_domain_puntos = sd_domain, vpc_stages = vpc_stages, interaccion = interaccion,
       pubpriv_efecto = pubpriv_efecto,
       fits = list(m0 = fit_m0, m1 = fit_m1, m2 = fit_m2, m3 = fit_m3, m4 = fit_m4, m5 = fit_m5))
}

resultados <- lapply(domains, fit_domain)
names(resultados) <- domains

tbl_vpc_escuela_covariables <- purrr::map_dfr(resultados, "vpc_stages")
tbl_interaccion_immig_segregacion <- purrr::map_dfr(resultados, "interaccion")
tbl_pubpriv_efecto_colegio <- purrr::map_dfr(resultados, "pubpriv_efecto")
tbl_sd_domain_2022_modelo9 <- purrr::map_dfr(resultados, ~ tibble(domain = .x$domain, sd_domain_puntos = .x$sd_domain_puntos))

message("\n=== VPC de colegio por dominio y modelo anidado ===")
print(tbl_vpc_escuela_covariables, n = Inf)

message("\n=== Interacción origen migrante x % migrante del colegio (M5) ===")
print(tbl_interaccion_immig_segregacion, n = Inf)

message("\n=== Efecto de público/privado (M2), en puntos -- privado frente a público ===")
print(tbl_pubpriv_efecto_colegio, n = Inf)

saveRDS(tbl_vpc_escuela_covariables, "data/tbl_vpc_escuela_covariables.rds")
saveRDS(tbl_interaccion_immig_segregacion, "data/tbl_interaccion_immig_segregacion.rds")
saveRDS(tbl_pubpriv_efecto_colegio, "data/tbl_pubpriv_efecto_colegio.rds")
saveRDS(tbl_sd_domain_2022_modelo9, "data/tbl_sd_domain_2022_modelo9.rds")
saveRDS(resultados, "data/modelo_covariables_colegio_fits.rds")

# --- Gráfico: VPC de colegio a través de los modelos anidados, por dominio --
library(ggplot2)
fig_vpc_stages <- tbl_vpc_escuela_covariables |>
  mutate(
    modelo_lab = recode(modelo,
      M0_nulo = "M0: nulo", M1_individual = "M1: +individual",
      M2_pubpriv = "M2: +público/privado",
      M3_segregacion = "M3: +% migrante colegio", M4_distraccion = "M4: +distracción colegio",
      M5_interaccion = "M5: +interacción"
    ) |> factor(levels = c("M0: nulo", "M1: +individual", "M2: +público/privado", "M3: +% migrante colegio", "M4: +distracción colegio", "M5: +interacción")),
    domain_lab = recode(domain, math = "Matemáticas", read = "Lectura", science = "Ciencias")
  ) |>
  ggplot(aes(x = modelo_lab, y = vpc_mean, ymin = vpc_lower, ymax = vpc_upper, color = domain_lab)) +
  geom_pointrange(position = position_dodge(width = 0.4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "VPC de colegio (2022) según qué covariables se van añadiendo",
    subtitle = "Modelos anidados M0->M5, por dominio -- cuánta varianza entre colegios queda tras cada bloque",
    x = NULL, y = "VPC de colegio (IC 95% credibilidad)", color = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "bottom")
ggsave("data/fig_vpc_escuela_covariables.png", fig_vpc_stages, width = 9, height = 6, dpi = 150)

message("\nGuardado: data/tbl_vpc_escuela_covariables.rds, data/tbl_interaccion_immig_segregacion.rds, ",
        "data/tbl_pubpriv_efecto_colegio.rds, data/tbl_sd_domain_2022_modelo9.rds, ",
        "data/modelo_covariables_colegio_fits.rds, data/fig_vpc_escuela_covariables.png")
