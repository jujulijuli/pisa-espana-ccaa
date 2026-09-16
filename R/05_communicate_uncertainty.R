# =============================================================================
# 05_communicate_uncertainty.R
# Igual que en pisa-maihda-trends: nunca solo el punto medio. Aquí el gráfico
# estrella es el forest plot de pendientes por CCAA -- el que normalmente NO
# se ve en la cobertura mediática de "qué comunidad ha subido/bajado más".
# Además: PCV del estrato, forest plot de estratos interseccionales (incluye
# `immig`, a diferencia del proyecto internacional), y brechas ajustadas de
# público-privado y de origen inmigrante.
#
# Solo prepara datos (data/*.rds) y figuras (data/*.png); las tablas `gt` se
# construyen en los .qmd al renderizar (informe HTML, ver _quarto.yml).
# =============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(patchwork)
library(ggrepel)  # etiquetas numéricas en el extremo de las líneas (evita
                   # que el lector tenga que leer el valor exacto a ojo)
library(INLA)   # necesario para inla.rmarginal() en extract_gap()

student_long <- readRDS("data/student_esp_long.rds")
domain_labels <- levels(student_long$domain)

# 03_model_maihda_ccaa_inla.R ajusta sobre `score_z` (estandarizada), no
# `score` en bruto -- ver el comentario largo equivalente en
# ../pisa-maihda-trends/R/03_model_maihda_inla.R sobre el error "matrix is
# not positive definite". VPC/PCV/correlación son invariantes a ese
# reescalado; el forest plot de estratos (sección 3) y las brechas
# público-privado/immig (sección 4) SÍ están en unidades de `score_z` y se
# multiplican por `score_sd` más abajo para volver a puntos PISA.
score_scale <- readRDS("data/score_scale.rds")
score_sd <- score_scale$sd

# =============================================================================
# 1. Tendencia por CCAA: pendientes con intervalo de credibilidad
# =============================================================================
# `significant` ya viene calculado en 04_model_trend_ccaa_inla.R (IC no cruza
# el cero) -- se reusa tal cual en vez de recalcular un criterio paralelo.
# 04_model_trend_ccaa_inla.R ajusta ahora sobre `score_z` también (estrato
# pasó a `iid3d` ahí) -- `slope_mean`/`slope_lower`/`slope_upper` salen en
# unidades de `score_z` y se multiplican aquí por `score_sd` (mismo factor
# leído arriba) para volver a puntos PISA por ciclo.

# AVISO IMPORTANTE (2026-09-16): Ceuta y Melilla no tienen cobertura
# continua en los microdatos del INEE -- 2009 las reporta como categoría
# COMBINADA "Ceuta y Melilla" (2012 y 2015 no incluyen ninguna de las dos),
# y solo en 2022 aparecen como "Ceuta"/"Melilla" por separado (ver
# R/01b_incorporate_historic_ccaa.R). `04_model_trend_ccaa_inla.R` trata
# `ccaa` como una única variable categórica, así que estas TRES etiquetas
# se ajustan como si fueran tres territorios distintos, cada uno con datos
# de UNA SOLA edición -- una pendiente temporal no es estimable con un solo
# punto en el tiempo, así que lo que sale ahí es un artefacto del modelo
# (arrastra intercept hacia pendiente por la correlación entre ambos en el
# efecto aleatorio), no una tendencia real. Se marcan aquí como
# `has_trend = FALSE`, se excluyen del forest plot principal y de las
# cifras "mejor/peor" que calcula qmd/04-resultados.qmd, y se documentan
# aparte con su puntuación bruta de la única edición disponible (ver
# `tbl_puntuacion_ccaa_global.rds`, de R/08_tendencias_ccaa.R).
n_editions_by_ccaa <- student_long |>
  filter(!is.na(score)) |>
  distinct(ccaa, year) |>
  count(ccaa, name = "n_editions")

slopes_by_ccaa <- readRDS("data/slopes_by_ccaa.rds") |>
  left_join(n_editions_by_ccaa, by = "ccaa") |>
  mutate(
    slope_mean = slope_mean * score_sd,
    slope_lower = slope_lower * score_sd,
    slope_upper = slope_upper * score_sd,
    has_trend = n_editions >= 2,
    ccaa = fct_reorder(ccaa, slope_mean)
  )

saveRDS(slopes_by_ccaa, "data/tbl_slopes_ccaa_full.rds")

p_slopes <- ggplot(slopes_by_ccaa |> filter(has_trend),
                    aes(y = ccaa, x = slope_mean,
                        xmin = slope_lower, xmax = slope_upper,
                        color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange() +
  scale_color_manual(
    values = c(`TRUE` = "#2b6a3e", `FALSE` = "grey60"),
    labels = c(`TRUE` = "IC no cruza el cero", `FALSE` = "sin evidencia suficiente"),
    name = NULL
  ) +
  labs(
    title = "Tendencia por comunidad autónoma, con intervalo de credibilidad al 95%",
    subtitle = "Gris = no se puede afirmar que haya cambiado; verde = cambio con evidencia clara.\nExcluye Ceuta, Melilla y \"Ceuta y Melilla\" (una sola edición cada una; ver nota en el texto)",
    x = "Pendiente (puntos PISA por ciclo)", y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("data/fig_slopes_ccaa.png", p_slopes, width = 8, height = 7, dpi = 150)

message(
  "Pendientes por CCAA: ", sum(slopes_by_ccaa$has_trend), " de ", nrow(slopes_by_ccaa),
  " comunidades con >=2 ediciones (tendencia estimable), ",
  sum(slopes_by_ccaa$significant & slopes_by_ccaa$has_trend), " con IC que no cruza el cero ",
  "(", scales::percent(mean(slopes_by_ccaa$significant[slopes_by_ccaa$has_trend])), "). ",
  "Excluidas por edición única: ",
  paste(slopes_by_ccaa$ccaa[!slopes_by_ccaa$has_trend], collapse = ", "), "."
)

# =============================================================================
# 2. VPC: desigualdad social (estrato) vs. desigualdad territorial (CCAA)
# =============================================================================
# Desde que 03_model_maihda_ccaa_inla.R pasó a un efecto trivariante
# ("iid3d") para estrato y colegio, el VPC/PCV del estrato y la correlación
# entre dominios son POR DOMINIO -- ver el comentario largo equivalente en
# ../pisa-maihda-trends/R/05_communicate_uncertainty.R. El VPC de la CCAA
# sigue siendo univariante simple (una sola línea, no por dominio).

vpc_strata <- readRDS("data/vpc_strata_by_year.rds") |> mutate(year = as.integer(year))
vpc_ccaa   <- readRDS("data/vpc_ccaa_by_year.rds")   |> mutate(year = as.integer(year), nivel = "Comunidad autónoma")
pcv_strata <- readRDS("data/pcv_strata_by_year.rds") |> mutate(year = as.integer(year))
cor_strata <- readRDS("data/cor_strata_by_year.rds") |> mutate(year = as.integer(year))
# PRUEBA DE VELOCIDAD (2026-09-14, ver 03_model_maihda_ccaa_inla.R): colegio
# es ahora `iid` simple, no `iid3d` -- su VPC ya NO tiene columna `domain`
# (univariante, como el de CCAA) y ya no existe correlación entre dominios a
# nivel de colegio (no se lee/genera data/cor_school_by_year.rds).
vpc_school <- readRDS("data/vpc_school_by_year.rds") |> mutate(year = as.integer(year))

saveRDS(
  vpc_strata |> left_join(pcv_strata, by = c("year", "domain")),
  "data/tbl_vpc_pcv_strata.rds"
)
saveRDS(cor_strata, "data/tbl_cor_strata.rds")
saveRDS(vpc_school, "data/tbl_vpc_school.rds")

# Comparación territorio vs. estrato: el VPC de la CCAA (una línea) frente al
# de cada dominio del estrato (3 líneas) en el mismo panel.
vpc_combined <- bind_rows(
  vpc_strata |> mutate(nivel = paste0("Estrato (", domain, ")")),
  vpc_ccaa |> select(-any_of("domain"))
)
saveRDS(vpc_combined, "data/tbl_vpc_compare.rds")

p_vpc_compare <- ggplot(vpc_combined, aes(x = year, y = vpc_mean, color = nivel, fill = nivel)) +
  geom_ribbon(aes(ymin = vpc_lower, ymax = vpc_upper), alpha = 0.15, color = NA) +
  geom_line() + geom_point() +
  # Etiqueta numérica solo en la última edición: con 4 líneas y sus bandas de
  # IC superpuestas, el valor exacto en el extremo derecho (justo el que se
  # cita en el texto) es difícil de leer a ojo sin ella.
  ggrepel::geom_text_repel(
    data = vpc_combined |> filter(year == max(year)),
    aes(label = scales::percent(vpc_mean, accuracy = 0.1)),
    show.legend = FALSE, fontface = "bold", size = 3.3,
    direction = "y", nudge_x = 1.3, hjust = 0, segment.size = 0.3,
    min.segment.length = 0
  ) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.14))) +
  labs(
    title = "¿Qué pesa más, la desigualdad social o la territorial?",
    subtitle = "VPC del estrato interseccional (por dominio) vs. VPC de la comunidad autónoma, con IC 95%",
    x = NULL, y = "Varianza explicada", color = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("data/fig_vpc_compare.png", p_vpc_compare, width = 8, height = 5, dpi = 150)

p_pcv_strata <- ggplot(pcv_strata, aes(x = year, y = pcv_mean, colour = domain, fill = domain)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_ribbon(aes(ymin = pcv_lower, ymax = pcv_upper), alpha = 0.15, colour = NA) +
  geom_line() + geom_point(size = 2) +
  ggrepel::geom_text_repel(
    data = pcv_strata |> filter(year == max(year)),
    aes(label = scales::percent(pcv_mean, accuracy = 0.1)),
    show.legend = FALSE, fontface = "bold", size = 3.3,
    direction = "y", nudge_x = 1.3, hjust = 0, segment.size = 0.3,
    min.segment.length = 0
  ) +
  scale_x_continuous(breaks = unique(pcv_strata$year), expand = expansion(mult = c(0.02, 0.16))) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "PCV por dominio: cuánta varianza entre estratos se explica de forma aditiva",
    subtitle = "Intervalo probablemente conservador; los valores negativos se explican en el texto",
    x = NULL, y = "PCV", colour = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("data/fig_pcv_strata.png", p_pcv_strata, width = 8, height = 5, dpi = 150)

p_vpc_pcv_combo <- p_vpc_compare + p_pcv_strata +
  plot_annotation(title = "Desigualdad social y territorial: cuánta varianza hay (VPC) y cuánta es aditiva (PCV)")
ggsave("data/fig_vpc_pcv_combo.png", p_vpc_pcv_combo, width = 13, height = 5, dpi = 150)

p_cor_strata <- ggplot(cor_strata, aes(x = year, y = cor_mean, colour = par)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = cor_lower, ymax = cor_upper), position = position_dodge(width = 0.4)) +
  scale_x_continuous(breaks = unique(cor_strata$year)) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(
    title = "Correlación entre dominios a nivel de estrato interseccional",
    subtitle = "¿Un estrato que puntúa alto en un dominio también lo hace en los otros? IC 95%",
    x = NULL, y = "Correlación", colour = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave("data/fig_cor_strata.png", p_cor_strata, width = 8, height = 5, dpi = 150)

# PRUEBA DE VELOCIDAD (2026-09-14): ya no hay correlación entre dominios a
# nivel de colegio que graficar (colegio es `iid` simple -- ver comentario
# arriba junto a `vpc_school`). Si se revierte a `iid3d`, este gráfico vuelve
# a construirse igual que `p_cor_strata`.

# =============================================================================
# 3. Estratos interseccionales: forest plot para la edición más reciente
# =============================================================================
# Modelo NULO (sin efectos principales) de la edición más reciente. Ahora hay
# una desviación POR DOMINIO para cada estrato (efecto trivariante) -- el
# gráfico se separa en 3 paneles. Aquí los estratos incluyen `immig` (72
# posibles), así que dentro de cada panel se muestran los 15 más altos y los
# 15 más bajos para que siga siendo legible; la tabla completa se guarda
# igual (con las 3 desviaciones por estrato).

maihda_fits <- readRDS("data/maihda_fits_ccaa_by_year.rds")
latest_year <- names(maihda_fits)[length(maihda_fits)]
fit_latest <- maihda_fits[[latest_year]]
fit_latest_null <- fit_latest$null
n_strata_latest <- fit_latest$n_strata

strata_lookup <- student_long |>
  distinct(strata) |>
  mutate(strata_id = as.integer(strata))

strata_forest_full <- fit_latest_null$summary.random$strata_domain_id |>
  as_tibble() |>
  rename(strata_domain_id = ID) |>
  mutate(
    strata_id = ((strata_domain_id - 1L) %% n_strata_latest) + 1L,
    domain_idx = ((strata_domain_id - 1L) %/% n_strata_latest) + 1L,
    domain = domain_labels[domain_idx]
  ) |>
  left_join(strata_lookup, by = "strata_id") |>
  transmute(
    strata = as.character(strata),
    domain,
    # *score_sd deshace la estandarización de 03_model_maihda_ccaa_inla.R
    # para volver a puntos PISA (ver comentario junto a `score_scale` arriba).
    estimate = mean * score_sd,
    lower = `0.025quant` * score_sd,
    upper = `0.975quant` * score_sd
  ) |>
  arrange(domain, estimate)

saveRDS(strata_forest_full, "data/tbl_strata_forest_full.rds")

n_show <- 15
strata_forest_extremes <- strata_forest_full |>
  group_by(domain) |>
  group_modify(~ bind_rows(slice_min(.x, estimate, n = n_show), slice_max(.x, estimate, n = n_show))) |>
  ungroup() |>
  distinct(strata, domain, .keep_all = TRUE) |>
  arrange(domain, estimate)

saveRDS(strata_forest_extremes, "data/tbl_strata_forest.rds")

# Mismo truco de etiqueta "estrato___dominio" que en el proyecto
# internacional para poder reordenar el eje Y de forma independiente dentro
# de cada panel sin depender de `tidytext::reorder_within`.
strata_forest_plot_data <- strata_forest_extremes |>
  arrange(domain, estimate) |>
  mutate(strata_label = factor(paste0(strata, "___", domain), levels = paste0(strata, "___", domain)))

p_strata <- ggplot(
  strata_forest_plot_data,
  aes(y = strata_label, x = estimate, xmin = lower, xmax = upper)
) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(size = 0.3) +
  facet_wrap(~domain, scales = "free_y") +
  scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
  labs(
    title = paste("Puntuación por estrato interseccional y dominio --", latest_year, "(15 más bajos / 15 más altos por panel)"),
    subtitle = "Desviación respecto a la media general (escala PISA), con IC 95%. La posición de un estrato puede cambiar entre paneles.",
    x = "Desviación respecto a la media general", y = NULL
  ) +
  theme_minimal(base_size = 8)

ggsave("data/fig_strata_forest.png", p_strata, width = 13,
       height = max(6, 0.18 * n_show * 2), dpi = 150)

# =============================================================================
# 4. Brechas ajustadas: público-privado y origen inmigrante
# =============================================================================
# Coeficientes extraídos del modelo de EFECTOS PRINCIPALES de cada edición
# (fit$main en 03_model_maihda_ccaa_inla.R), ya ajustados por dominio, estrato
# interseccional y la jerarquía colegio/CCAA. Búsqueda por grep del nombre
# exacto del término (en vez de asumir la codificación) porque tanto
# `public_private` como `immig` pueden tener más de un nivel no-referencia
# (immig especialmente: "native"/"first_gen"/"second_gen" o similar según el
# fichero del INEE), así que puede devolver varias filas por año.

extract_gap <- function(fit, var_prefix) {
  nms <- grep(var_prefix, names(fit$marginals.fixed), value = TRUE)
  if (length(nms) == 0) return(NULL)
  purrr::map_dfr(nms, function(nm) {
    # *score_sd: el coeficiente sale en unidades de `score_z` (ver
    # comentario junto a `score_scale` arriba) -- se deshace aquí, antes de
    # resumir, para que la media/IC ya salgan directamente en puntos PISA.
    samp <- inla.rmarginal(4000, fit$marginals.fixed[[nm]]) * score_sd
    tibble(
      term = nm,
      gap_mean = mean(samp),
      gap_lower = quantile(samp, 0.025),
      gap_upper = quantile(samp, 0.975)
    )
  })
}

pubpriv_by_year <- purrr::map_dfr(maihda_fits, ~ extract_gap(.x$main, "public_private"), .id = "year")
immig_by_year   <- purrr::map_dfr(maihda_fits, ~ extract_gap(.x$main, "^immig"), .id = "year")

if (nrow(pubpriv_by_year) > 0) {
  pubpriv_by_year <- pubpriv_by_year |> mutate(year = as.integer(year))
  saveRDS(pubpriv_by_year, "data/tbl_pubpriv_gap.rds")

  p_pubpriv <- ggplot(pubpriv_by_year, aes(x = year, y = gap_mean)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_ribbon(aes(ymin = gap_lower, ymax = gap_upper), alpha = 0.2) +
    geom_line() + geom_point(size = 2) +
    scale_x_continuous(breaks = unique(pubpriv_by_year$year)) +
    labs(
      title = "Brecha colegio privado vs. público, ajustada",
      subtitle = "Puntos PISA (referencia = público); ajustado por dominio, estrato interseccional y jerarquía colegio/CCAA",
      x = NULL, y = "Diferencia (puntos PISA)"
    ) +
    theme_minimal(base_size = 12)

  ggsave("data/fig_pubpriv_gap.png", p_pubpriv, width = 8, height = 5, dpi = 150)
} else {
  message("Nota: no se encontró el coeficiente de public_private en los modelos.")
}

if (nrow(immig_by_year) > 0) {
  immig_by_year <- immig_by_year |> mutate(year = as.integer(year))
  saveRDS(immig_by_year, "data/tbl_immig_gap.rds")

  p_immig <- ggplot(immig_by_year, aes(x = year, y = gap_mean, color = term, fill = term)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    geom_ribbon(aes(ymin = gap_lower, ymax = gap_upper), alpha = 0.15, color = NA) +
    geom_line() + geom_point(size = 2) +
    scale_x_continuous(breaks = unique(immig_by_year$year)) +
    labs(
      title = "Brecha por origen inmigrante, ajustada",
      subtitle = "Puntos PISA (referencia = nativo); ajustado por dominio, estrato interseccional y jerarquía colegio/CCAA",
      x = NULL, y = "Diferencia (puntos PISA)", color = NULL, fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")

  ggsave("data/fig_immig_gap.png", p_immig, width = 8, height = 5, dpi = 150)
} else {
  message("Nota: no se encontró el coeficiente de immig en los modelos.")
}

message(
  "Guardado en data/: fig_slopes_ccaa.png, fig_vpc_compare.png, fig_pcv_strata.png, ",
  "fig_cor_strata.png, fig_vpc_pcv_combo.png, fig_strata_forest.png",
  if (nrow(pubpriv_by_year) > 0) ", fig_pubpriv_gap.png" else "",
  if (nrow(immig_by_year) > 0) ", fig_immig_gap.png" else "",
  " + tablas tbl_*.rds para los chunks `gt` de qmd/04-resultados.qmd"
)
