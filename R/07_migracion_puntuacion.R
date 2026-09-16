# =============================================================================
# 07_migracion_puntuacion.R
# Descriptivo 2026-09-16, dos piezas:
#
#  (A) Univariante de migración: puntuación media por origen migrante
#      (nativo / 1ª generación / 2ª generación, separados -- no agrupados
#      como en 05_segregacion_heterogeneidad.R, donde se juntan para el
#      índice de segregación) por dominio y año, serie 2009-2022. Es el
#      equivalente para H1 de lo que R/06_exposicion_digital.R ya hace para
#      H2 (allí SÍ había un modelo ajustado; aquí Julián pidió expresamente
#      el univariante -- sin ajustar por ESCS/género/etc. -- como paso
#      previo al modelo multivariante de R/09_modelo_covariables_colegio.R).
#  (B) Escala de "DE -> puntos": todas las tablas de asociación del proyecto
#      (06 y 09) expresan el efecto en desviaciones estándar (score_z) para
#      poder comparar entre dominios/ediciones -- aquí se traduce esa unidad
#      a la escala real de puntuación PISA, y se documentan las distintas
#      "DE" que aparecen en el proyecto (no son la misma constante en todos
#      los scripts, ver aviso más abajo).
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

path_extended <- "data/student_esp_prep.rds"
stopifnot(
  "Necesito data/student_esp_prep.rds -- ejecuta antes 02_prepare_variables.R" =
    file.exists(path_extended)
)
student <- readRDS(path_extended)

# --- (A) Univariante: puntuación media por origen migrante -------------------
weighted_mean_ci <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  n_eff <- (sum(w))^2 / sum(w^2)
  m <- weighted.mean(x, w)
  v <- sum(w * (x - m)^2) / sum(w)
  se <- sqrt(v / n_eff)
  tibble(media = m, se = se, ci_low = m - 1.96 * se, ci_high = m + 1.96 * se, n = length(x), n_eff = n_eff)
}

puntuacion_migracion <- student |>
  filter(!is.na(immig), !is.na(stu_wgt)) |>
  select(year, ccaa, immig, stu_wgt, math, read, science) |>
  pivot_longer(cols = c(math, read, science), names_to = "domain", values_to = "score") |>
  filter(!is.na(score)) |>
  group_by(year, domain, immig) |>
  group_modify(~ weighted_mean_ci(.x$score, .x$stu_wgt)) |>
  ungroup()

message("=== Puntuación media por origen migrante (nacional, sin ajustar) ===")
print(puntuacion_migracion |> filter(domain == "math"), n = Inf)
message("(dominios lectura y ciencia en el objeto guardado, no impresos aquí por espacio)")

saveRDS(puntuacion_migracion, "data/tbl_puntuacion_migracion.rds")

# --- Brecha nativo vs. cada generación migrante, con IC -----------------------
brecha_migracion <- puntuacion_migracion |>
  select(year, domain, immig, media, se) |>
  pivot_wider(names_from = immig, values_from = c(media, se)) |>
  mutate(
    brecha_1gen = media_primera_gen - media_nativo,
    se_brecha_1gen = sqrt(se_primera_gen^2 + se_nativo^2),
    ci_low_1gen = brecha_1gen - 1.96 * se_brecha_1gen,
    ci_high_1gen = brecha_1gen + 1.96 * se_brecha_1gen,
    brecha_2gen = media_segunda_gen - media_nativo,
    se_brecha_2gen = sqrt(se_segunda_gen^2 + se_nativo^2),
    ci_low_2gen = brecha_2gen - 1.96 * se_brecha_2gen,
    ci_high_2gen = brecha_2gen + 1.96 * se_brecha_2gen
  ) |>
  select(year, domain, starts_with("brecha_"), starts_with("ci_low_"), starts_with("ci_high_"))

message("\n=== Brecha de puntuación, nativo como referencia (puntos PISA) ===")
print(brecha_migracion |> filter(domain == "math"), n = Inf)

saveRDS(brecha_migracion, "data/tbl_brecha_migracion_puntuacion.rds")

fig_brecha_migracion <- brecha_migracion |>
  select(year, domain, brecha_1gen, ci_low_1gen, ci_high_1gen, brecha_2gen, ci_low_2gen, ci_high_2gen) |>
  pivot_longer(
    cols = -c(year, domain),
    names_to = c(".value", "generacion"),
    names_pattern = "(brecha|ci_low|ci_high)_(.*)"
  ) |>
  mutate(
    generacion = recode(generacion, `1gen` = "1ª generación", `2gen` = "2ª generación"),
    domain_lab = recode(domain, math = "Matemáticas", read = "Lectura", science = "Ciencias")
  ) |>
  ggplot(aes(x = year, y = brecha, ymin = ci_low, ymax = ci_high, color = generacion)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.6)) +
  facet_wrap(~domain_lab) +
  labs(
    title = "Brecha de puntuación por origen migrante (referencia: nativo)",
    subtitle = "1ª y 2ª generación por separado, sin ajustar por ESCS/género/educación parental",
    x = NULL, y = "Diferencia en puntos PISA", color = NULL
  ) +
  theme_minimal(base_size = 11)
ggsave("data/fig_brecha_migracion_trend.png", fig_brecha_migracion, width = 9, height = 5, dpi = 150)

# --- Distribución real de puntuación por origen migrante, última edición ----
fig_distribucion_migracion <- student |>
  filter(year == max(year), !is.na(immig)) |>
  select(immig, math, read, science) |>
  pivot_longer(cols = c(math, read, science), names_to = "domain", values_to = "score") |>
  mutate(
    domain_lab = recode(domain, math = "Matemáticas", read = "Lectura", science = "Ciencias"),
    immig_lab = recode(immig, nativo = "Nativo", primera_gen = "1ª generación", segunda_gen = "2ª generación") |>
      factor(levels = c("Nativo", "1ª generación", "2ª generación"))
  ) |>
  ggplot(aes(x = immig_lab, y = score, fill = immig_lab)) +
  geom_violin(alpha = 0.6, show.legend = FALSE) +
  geom_boxplot(width = 0.15, outlier.shape = NA, show.legend = FALSE) +
  facet_wrap(~domain_lab) +
  labs(
    title = paste("Distribución de puntuación por origen migrante,", max(student$year)),
    subtitle = "Sin ponderar (descriptivo) -- la tabla de brecha de arriba sí está ponderada",
    x = NULL, y = "Puntuación"
  ) +
  theme_minimal(base_size = 11)
ggsave("data/fig_distribucion_migracion.png", fig_distribucion_migracion, width = 9, height = 5, dpi = 150)

# --- (B) Escala DE -> puntos --------------------------------------------------
# AVISO IMPORTANTE -- hay tres "desviaciones estándar" distintas en el
# proyecto, y no son intercambiables:
#  1. `data/score_scale.rds` (de 03_model_maihda_ccaa_inla.R): media/DE
#     GLOBAL, calculada sin ponderar sobre TODO el pool (todas las ediciones
#     y los tres dominios juntos). Es la que usa el modelo MAIHDA principal
#     (03/04) para estandarizar `score_z`.
#  2. La DE que usa cada fila de `tbl_asociacion_distraccion_2022.rds` (de
#     06_exposicion_digital.R): específica de un dominio, año 2022, y de la
#     submuestra analítica de ESE predictor concreto (cambia ligeramente
#     entre distr_propia/distr_pares/horas_redes por su patrón de missing
#     distinto) -- ya viene incorporada en ese fichero como
#     `sd_domain_puntos` (columna añadida en esta misma revisión), así que
#     sus columnas `estimate_puntos`/`ci_low_puntos`/`ci_high_puntos` ya
#     están en puntos, no hace falta recalcular nada aquí.
#  3. La de R/09_modelo_covariables_colegio.R (mismo patrón que 2, otra
#     submuestra analítica): trae su propia `sd_domain_puntos`.
# Aquí se calcula una CUARTA referencia, la más simple e intuitiva para el
# texto del informe: la DE bruta (ponderada) de cada dominio, por año,
# usando TODA la muestra de esa edición -- útil para dar una regla general
# tipo "una DE en matemáticas 2022 son unos X puntos", sin atarla a ningún
# modelo concreto.
weighted_sd <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  m <- weighted.mean(x, w)
  sqrt(sum(w * (x - m)^2) / sum(w))
}

escala_puntos <- student |>
  select(year, stu_wgt, math, read, science) |>
  pivot_longer(cols = c(math, read, science), names_to = "domain", values_to = "score") |>
  filter(!is.na(score), !is.na(stu_wgt)) |>
  group_by(year, domain) |>
  summarise(media_puntos = weighted.mean(score, stu_wgt), de_puntos = weighted_sd(score, stu_wgt), .groups = "drop")

score_scale_global <- tryCatch(readRDS("data/score_scale.rds"), error = function(e) NULL)

message("\n=== Escala de puntos por dominio y año (DE bruta, ponderada) ===")
print(escala_puntos, n = Inf)
if (!is.null(score_scale_global)) {
  message(
    "\nPara referencia, la DE GLOBAL usada en el modelo MAIHDA principal (03/04, ",
    "pool completo, sin ponderar, los 3 dominios y todas las ediciones juntos) es: ",
    round(score_scale_global$sd, 1), " puntos (media ", round(score_scale_global$mean, 1), ")."
  )
}

saveRDS(escala_puntos, "data/tbl_escala_de_puntos.rds")

message("\nGuardado: data/tbl_puntuacion_migracion.rds, data/tbl_brecha_migracion_puntuacion.rds, ",
        "data/fig_brecha_migracion_trend.png, data/fig_distribucion_migracion.png, ",
        "data/tbl_escala_de_puntos.rds")
