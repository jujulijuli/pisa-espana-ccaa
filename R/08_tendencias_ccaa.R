# =============================================================================
# 08_tendencias_ccaa.R
# Descriptivo pedido por Julián 2026-09-16: evolución por CCAA, tanto de la
# puntuación como de los índices de segregación de 05_segregacion_
# heterogeneidad.R -- para ver a ojo si las CCAA con más caída de puntuación
# son también las que más han subido en segregación (correlación ecológica,
# no causal ni individual: son promedios por CCAA-año, no el dato de cada
# alumno -- una CCAA puede "correlacionar" a nivel agregado sin que exista
# ninguna relación a nivel individual, y viceversa. Documentado así en el
# propio gráfico).
#
# Requiere haber ejecutado antes 02_prepare_variables.R (usa
# data/student_esp_prep.rds) y 05_segregacion_heterogeneidad.R (usa
# data/tbl_segregacion_ccaa.rds).
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)

stopifnot(
  "Necesito data/student_esp_prep.rds -- ejecuta antes 02_prepare_variables.R" =
    file.exists("data/student_esp_prep.rds"),
  "Necesito data/tbl_segregacion_ccaa.rds -- ejecuta antes 05_segregacion_heterogeneidad.R" =
    file.exists("data/tbl_segregacion_ccaa.rds")
)

student <- readRDS("data/student_esp_prep.rds")
seg_ccaa <- readRDS("data/tbl_segregacion_ccaa.rds")

weighted_mean_ci <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  n_eff <- (sum(w))^2 / sum(w^2)
  m <- weighted.mean(x, w)
  v <- sum(w * (x - m)^2) / sum(w)
  se <- sqrt(v / n_eff)
  tibble(media = m, ci_low = m - 1.96 * se, ci_high = m + 1.96 * se, n = length(x))
}

# --- 1. Puntuación por CCAA-año -----------------------------------------------
# Puntuación "global" = media de los tres dominios por alumno, para tener una
# sola serie por CCAA en vez de tres (más legible en un grid de 19 paneles);
# los tres dominios completos quedan guardados en `puntuacion_ccaa_dominio`
# por si hace falta desagregar.
student_score_avg <- student |>
  filter(!is.na(math), !is.na(read), !is.na(science)) |>
  mutate(score_global = (math + read + science) / 3)

puntuacion_ccaa_global <- student_score_avg |>
  filter(!is.na(stu_wgt)) |>
  group_by(year, ccaa) |>
  group_modify(~ weighted_mean_ci(.x$score_global, .x$stu_wgt)) |>
  ungroup()

puntuacion_ccaa_dominio <- student |>
  select(year, ccaa, stu_wgt, math, read, science) |>
  pivot_longer(cols = c(math, read, science), names_to = "domain", values_to = "score") |>
  filter(!is.na(score), !is.na(stu_wgt)) |>
  group_by(year, ccaa, domain) |>
  group_modify(~ weighted_mean_ci(.x$score, .x$stu_wgt)) |>
  ungroup()

saveRDS(puntuacion_ccaa_global, "data/tbl_puntuacion_ccaa_global.rds")
saveRDS(puntuacion_ccaa_dominio, "data/tbl_puntuacion_ccaa_dominio.rds")

fig_puntuacion_ccaa <- puntuacion_ccaa_global |>
  ggplot(aes(x = year, y = media, ymin = ci_low, ymax = ci_high)) +
  geom_ribbon(alpha = 0.15) +
  geom_line(linewidth = 0.8, color = "steelblue4") +
  geom_point(size = 1.3, color = "steelblue4") +
  facet_wrap(~ccaa, ncol = 5) +
  labs(
    title = "Evolución de la puntuación PISA por CCAA (media de los 3 dominios)",
    subtitle = "Serie 2009-2022, IC 95%",
    x = NULL, y = "Puntuación media"
  ) +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(size = 7.5))
ggsave("data/fig_puntuacion_ccaa_trend.png", fig_puntuacion_ccaa, width = 11, height = 9, dpi = 150)

# --- 2. Segregación por CCAA-año (reutiliza 05_segregacion_heterogeneidad.R) -
fig_segregacion_ccaa <- seg_ccaa |>
  filter(!is.na(dissim_index)) |>
  select(year, ccaa, dissim_index, isolation_adj) |>
  pivot_longer(cols = c(dissim_index, isolation_adj), names_to = "indice", values_to = "valor") |>
  mutate(indice = recode(indice, dissim_index = "Disimilitud", isolation_adj = "Aislamiento adj.")) |>
  ggplot(aes(x = year, y = valor, color = indice)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  facet_wrap(~ccaa, ncol = 5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Evolución de la segregación escolar del alumnado migrante por CCAA",
    subtitle = "Serie 2009-2022 (paneles sin datos: <2 colegios con alumnado migrante esa edición)",
    x = NULL, y = "Índice", color = NULL
  ) +
  theme_minimal(base_size = 9) +
  theme(strip.text = element_text(size = 7.5), legend.position = "bottom")
ggsave("data/fig_segregacion_ccaa_trend.png", fig_segregacion_ccaa, width = 11, height = 9, dpi = 150)

# --- 3. ¿Correlación ecológica entre cambio de puntuación y cambio de segregación? ----
# Un punto por CCAA: cambio en puntuación global (última edición - primera
# edición disponible en esa CCAA) vs. cambio en el índice de disimilitud.
# PURAMENTE DESCRIPTIVO/ECOLÓGICO -- con 17-19 puntos (CCAA) no hay potencia
# para nada más que una foto de conjunto, y una correlación a este nivel
# agregado NO implica ninguna relación a nivel de alumno individual (falacia
# ecológica) -- se advierte también en el propio gráfico.
cambio_puntuacion <- puntuacion_ccaa_global |>
  group_by(ccaa) |>
  filter(n_distinct(year) >= 2) |>
  summarise(
    year_ini = min(year), year_fin = max(year),
    cambio_puntuacion = media[year == max(year)] - media[year == min(year)],
    .groups = "drop"
  )

cambio_segregacion <- seg_ccaa |>
  filter(!is.na(dissim_index)) |>
  group_by(ccaa) |>
  filter(n_distinct(year) >= 2) |>
  summarise(
    cambio_dissim = dissim_index[year == max(year)] - dissim_index[year == min(year)],
    cambio_isolation = isolation_adj[year == max(year)] - isolation_adj[year == min(year)],
    .groups = "drop"
  )

cambio_ccaa <- inner_join(cambio_puntuacion, cambio_segregacion, by = "ccaa")

cor_test_dissim <- suppressWarnings(cor.test(cambio_ccaa$cambio_puntuacion, cambio_ccaa$cambio_dissim))
message(
  "\nCorrelación ecológica (CCAA como unidad, N=", nrow(cambio_ccaa), "): cambio en puntuación vs. ",
  "cambio en disimilitud: r=", round(cor_test_dissim$estimate, 2),
  " (IC95% ", round(cor_test_dissim$conf.int[1], 2), " a ", round(cor_test_dissim$conf.int[2], 2), ", p=",
  round(cor_test_dissim$p.value, 3), "). PURAMENTE DESCRIPTIVO -- ver aviso en el script."
)

saveRDS(cambio_ccaa, "data/tbl_cambio_ccaa.rds")

# `ggrepel` no está en 00_packages.R en tu instalación si no lo has vuelto a
# ejecutar tras esta revisión -- se usa si está disponible (etiquetas sin
# solaparse), si no cae a `geom_text` normal (puede solaparse algo con 17-19
# etiquetas, pero no rompe el script).
label_layer <- if (requireNamespace("ggrepel", quietly = TRUE)) {
  ggrepel::geom_text_repel(size = 3, max.overlaps = 20)
} else {
  geom_text(size = 3, vjust = -0.6)
}

fig_cor_cambio <- cambio_ccaa |>
  ggplot(aes(x = cambio_dissim, y = cambio_puntuacion, label = ccaa)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(color = "steelblue4", size = 2) +
  label_layer +
  labs(
    title = "Cambio en segregación vs. cambio en puntuación, por CCAA",
    subtitle = paste0(
      "Correlación ecológica entre CCAA (N=", nrow(cambio_ccaa), "), r=", round(cor_test_dissim$estimate, 2),
      " -- NO implica relación a nivel individual (falacia ecológica)"
    ),
    x = "Cambio en índice de disimilitud (edición inicial -> final)",
    y = "Cambio en puntuación media (edición inicial -> final)"
  ) +
  theme_minimal(base_size = 11)
ggsave("data/fig_correlacion_cambio_ccaa.png", fig_cor_cambio, width = 8, height = 6.5, dpi = 150)

message("\nGuardado: data/tbl_puntuacion_ccaa_global.rds, data/tbl_puntuacion_ccaa_dominio.rds, ",
        "data/fig_puntuacion_ccaa_trend.png, data/fig_segregacion_ccaa_trend.png, ",
        "data/tbl_cambio_ccaa.rds, data/fig_correlacion_cambio_ccaa.png")
