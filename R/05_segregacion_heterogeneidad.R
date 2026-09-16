# =============================================================================
# 05_segregacion_heterogeneidad.R
# Descriptivo pormenorizado (H1 de la pregunta de investigación de Julián):
# ¿en qué medida ha aumentado la heterogeneidad del aula -- en particular la
# concentración/segregación de alumnado de origen migrante en determinados
# colegios -- como posible explicación de la caída observada en PISA?
#
# Serie histórica 2009-2022 (usa data/student_esp_pooled_extended.rds, que ya
# incluye 2022 + 2009/2012/2015 gracias a 01b_incorporate_historic_ccaa.R --
# si solo existe data/student_esp_pooled.rds, se usa solo 2022 y se avisa).
#
# No es una pieza del pipeline MAIHDA/INLA (03/04/05_communicate_uncertainty.R)
# -- es autocontenida y alimenta un qmd de descriptivo aparte
# (qmd/05-descriptivo-heterogeneidad.qmd), no qmd/04-resultados.qmd.
#
# Índices de segregación calculados (estándar en la literatura de segregación
# escolar, ambos entre 0 y 1, mayor = más segregado):
#  - Índice de disimilitud (Duncan & Duncan, 1955): proporción de alumnado
#    migrante (o nativo) que tendría que cambiar de colegio para que la
#    composición fuera idéntica en todos los colegios de esa CCAA/año.
#  - Índice de aislamiento/isolamiento (Bell, 1954), en su versión ajustada
#    (resta la composición migrante media de la CCAA, para no confundir
#    "hay más migrantes en general" con "están más concentrados en los
#    mismos colegios"): 0 = mezcla aleatoria dado el peso migrante de esa
#    CCAA/año, 1 = aislamiento completo.
# Se calculan a dos niveles: nacional por año, y CCAA x año (para ver si la
# concentración varía geográficamente, que es justo el valor añadido de
# este proyecto frente al internacional).
#
# LIMITACIÓN documentada (coherente con 01b_incorporate_historic_ccaa.R):
# `public_private` no está disponible para 2009/2012/2015 -- no se puede
# cruzar segregación migrante x tipo de colegio en esos años, solo en 2022.
# =============================================================================

library(dplyr)
library(tidyr)

path_extended <- "data/student_esp_pooled_extended.rds"
path_2022_only <- "data/student_esp_pooled.rds"
if (file.exists(path_extended)) {
  message("Usando ", path_extended, " (serie 2009-2022 con CCAA)")
  student <- readRDS(path_extended)
} else {
  message(
    "Solo encuentro ", path_2022_only, " -- ejecuta antes 01b_incorporate_historic_ccaa.R ",
    "si quieres la serie histórica de segregación; por ahora solo se calculará 2022."
  )
  student <- readRDS(path_2022_only)
}

# --- 1. Composición migrante por colegio-año ---------------------------------
# `immig` = nativo / segunda_gen / primera_gen (ver 01_load_data.R y
# 01b_incorporate_historic_ccaa.R). Para el índice de segregación se agrupa
# 1ª+2ª generación como "origen migrante" (uso estándar en la literatura),
# y se guarda también la versión estricta (solo 1ª generación) como análisis
# de sensibilidad -- puede ser relevante porque la 2ª generación está más
# asentada y podría segregarse menos que la 1ª.
school_comp <- student |>
  filter(!is.na(immig), !is.na(global_school_id)) |>
  mutate(
    es_migrante_amplio  = immig %in% c("primera_gen", "segunda_gen"),
    es_migrante_estricto = immig == "primera_gen"
  ) |>
  group_by(year, ccaa, global_school_id) |>
  summarise(
    n_alumnos           = n(),
    n_migrante_amplio   = sum(es_migrante_amplio),
    n_migrante_estricto = sum(es_migrante_estricto),
    .groups = "drop"
  )

# --- 2. Índices de disimilitud y aislamiento ---------------------------------
# `group_col` recibe los datos ya agrupados al nivel deseado (nacional: solo
# year; CCAA: year + ccaa) con una fila por colegio dentro de ese grupo.
calc_segregation <- function(df, n_migrant_col) {
  n_migrant <- df[[n_migrant_col]]
  n_total   <- df$n_alumnos
  n_native  <- n_total - n_migrant

  T_migrant <- sum(n_migrant)
  T_native  <- sum(n_native)
  T_total   <- T_migrant + T_native
  p_migrant <- T_migrant / T_total  # peso migrante global de ese grupo (año o CCAA-año)

  n_colegios <- nrow(df)

  if (T_migrant == 0 || T_native == 0 || n_colegios < 2) {
    return(tibble(
      n_colegios = n_colegios, n_alumnos = T_total,
      n_migrante = T_migrant, pct_migrante = p_migrant,
      dissim_index = NA_real_, isolation_adj = NA_real_,
      aviso = "Muestra insuficiente (sin variación entre colegios, o sin migrantes/nativos)"
    ))
  }

  # Índice de disimilitud (Duncan & Duncan 1955)
  dissim <- 0.5 * sum(abs(n_migrant / T_migrant - n_native / T_native))

  # Índice de aislamiento (Bell 1954), ajustado restando el peso migrante
  # medio: isolation_raw = sum_s (n_migrant_s/T_migrant * n_migrant_s/n_total_s)
  iso_raw <- sum((n_migrant / T_migrant) * (n_migrant / n_total), na.rm = TRUE)
  iso_adj <- (iso_raw - p_migrant) / (1 - p_migrant)

  tibble(
    n_colegios = n_colegios, n_alumnos = T_total,
    n_migrante = T_migrant, pct_migrante = p_migrant,
    dissim_index = dissim, isolation_adj = iso_adj,
    aviso = if (T_migrant < 30) "Aviso: <30 alumnos migrantes -- índice poco estable" else NA_character_
  )
}

run_segregation_by <- function(data, group_vars, n_migrant_col) {
  data |>
    group_by(across(all_of(group_vars))) |>
    group_modify(~ calc_segregation(.x, n_migrant_col)) |>
    ungroup()
}

seg_nacional <- run_segregation_by(school_comp, "year", "n_migrante_amplio") |>
  mutate(nivel = "nacional", .before = 1)

seg_ccaa <- run_segregation_by(school_comp, c("year", "ccaa"), "n_migrante_amplio") |>
  mutate(nivel = "ccaa", .before = 1)

# Análisis de sensibilidad: solo 1ª generación (definición estricta)
seg_nacional_estricto <- run_segregation_by(school_comp, "year", "n_migrante_estricto") |>
  mutate(nivel = "nacional_1gen", .before = 1)

message("=== Segregación nacional (origen migrante amplio: 1ª+2ª gen) ===")
print(seg_nacional, n = Inf)
message("\n=== Segregación por CCAA (última edición disponible) ===")
print(
  seg_ccaa |> filter(year == max(year)) |> arrange(desc(dissim_index)),
  n = Inf
)

saveRDS(seg_nacional, "data/tbl_segregacion_nacional.rds")
saveRDS(seg_ccaa, "data/tbl_segregacion_ccaa.rds")
saveRDS(seg_nacional_estricto, "data/tbl_segregacion_nacional_1gen.rds")
saveRDS(school_comp, "data/school_composicion_migrante.rds")

# --- 3. Gráfico de tendencia (nacional, disimilitud + aislamiento) ----------
library(ggplot2)

fig_seg_trend <- seg_nacional |>
  filter(!is.na(dissim_index)) |>
  select(year, dissim_index, isolation_adj) |>
  pivot_longer(cols = c(dissim_index, isolation_adj), names_to = "indice", values_to = "valor") |>
  mutate(indice = recode(indice,
    dissim_index = "Disimilitud (Duncan)",
    isolation_adj = "Aislamiento ajustado (Bell)"
  )) |>
  ggplot(aes(x = year, y = valor, color = indice)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, NA), labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Evolución de la segregación escolar del alumnado de origen migrante en España",
    subtitle = "Índices de disimilitud y aislamiento ajustado, serie nacional",
    x = NULL, y = "Índice (0 = sin segregación, 1 = segregación completa)",
    color = NULL,
    caption = "Fuente: microdatos INEE/OCDE PISA. 2ª generación incluida junto con 1ª (\"origen migrante amplio\")."
  ) +
  theme_minimal(base_size = 12)

ggsave("data/fig_segregacion_trend.png", fig_seg_trend, width = 8, height = 5, dpi = 150)

message("\nGuardado: data/tbl_segregacion_nacional.rds, data/tbl_segregacion_ccaa.rds, ",
        "data/tbl_segregacion_nacional_1gen.rds, data/school_composicion_migrante.rds, ",
        "data/fig_segregacion_trend.png")

# --- 4. Segregación por tipo de colegio (público/privado) -------------------
# Esto es lo que Julián preguntó específicamente: ¿el alumnado migrante está
# desproporcionadamente concentrado en colegios públicos? Requiere
# `public_private`, cuya cobertura varía por edición (ver
# 01c_incorporate_public_private_historic.R): prácticamente censal en
# 2012/2015/2022, parcial (24%, solo submuestra) en 2009 -- por eso el
# resultado de 2009 lleva una advertencia explícita en vez de presentarse
# igual que el resto.
#
# Indicador: entre el alumnado de origen migrante (amplio, 1ª+2ª gen), ¿qué
# % está escolarizado en centro público vs. privado, comparado con el mismo
# % entre el alumnado nativo? La diferencia de esos dos porcentajes es la
# "brecha de escolarización" -- positiva significa que el alumnado migrante
# está sobrerrepresentado en la pública respecto al nativo.
weighted_prop_ci <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  n_eff <- (sum(w))^2 / sum(w^2)
  p <- weighted.mean(x, w)
  se <- sqrt(p * (1 - p) / n_eff)
  tibble(prop = p, se = se, n = length(x), n_eff = n_eff)
}

pubpriv_by_immig <- student |>
  filter(!is.na(public_private), !is.na(immig), !is.na(stu_wgt)) |>
  mutate(es_migrante_amplio = immig %in% c("primera_gen", "segunda_gen")) |>
  group_by(year, es_migrante_amplio) |>
  group_modify(~ weighted_prop_ci(.x$public_private == "publico", .x$stu_wgt)) |>
  ungroup() |>
  rename(pct_en_publico = prop)

# Cobertura de public_private por año, para poder avisar si es parcial
# (mismo cálculo que imprime 01c, repetido aquí para que la tabla final lo
# lleve incorporado y no dependa de que se recuerde la nota por separado).
cobertura_pubpriv <- student |>
  group_by(year) |>
  summarise(pct_cobertura_pubpriv = mean(!is.na(public_private)), .groups = "drop")

brecha_pubpriv_migrante <- pubpriv_by_immig |>
  select(year, es_migrante_amplio, pct_en_publico, se, n) |>
  pivot_wider(
    id_cols = year, names_from = es_migrante_amplio,
    values_from = c(pct_en_publico, se, n),
    names_glue = "{.value}_{ifelse(es_migrante_amplio, 'migrante', 'nativo')}"
  ) |>
  mutate(
    brecha = pct_en_publico_migrante - pct_en_publico_nativo,
    se_brecha = sqrt(se_migrante^2 + se_nativo^2),
    ci_low = brecha - 1.96 * se_brecha,
    ci_high = brecha + 1.96 * se_brecha
  ) |>
  left_join(cobertura_pubpriv, by = "year") |>
  mutate(
    aviso = if_else(
      pct_cobertura_pubpriv < 0.5,
      "Cobertura de public_private <50% esta edición -- interpretar con mucha cautela (submuestra, no censo)",
      NA_character_
    )
  ) |>
  select(year, pct_en_publico_nativo, pct_en_publico_migrante, brecha, ci_low, ci_high,
         pct_cobertura_pubpriv, aviso)

message("\n=== Brecha de escolarización pública/privada: % en centro público, migrante vs. nativo ===")
print(brecha_pubpriv_migrante, n = Inf)

saveRDS(pubpriv_by_immig, "data/tbl_composicion_pubpriv_migrante.rds")
saveRDS(brecha_pubpriv_migrante, "data/tbl_brecha_pubpriv_migrante.rds")

fig_brecha_pubpriv <- brecha_pubpriv_migrante |>
  filter(!is.na(brecha)) |>
  ggplot(aes(x = year, y = brecha, ymin = ci_low, ymax = ci_high, alpha = pct_cobertura_pubpriv >= 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35), guide = "none") +
  labs(
    title = "Brecha de escolarización pública/privada: alumnado migrante vs. nativo",
    subtitle = "Diferencia en puntos porcentuales del % escolarizado en centro público (positivo = migrante más en pública)",
    x = NULL, y = "Diferencia (p.p.)",
    caption = "Puntos más tenues (2009): cobertura de public_private <50% (submuestra del panel, no censo) -- interpretar con cautela."
  ) +
  theme_minimal(base_size = 12)
ggsave("data/fig_brecha_pubpriv_migrante.png", fig_brecha_pubpriv, width = 8, height = 5, dpi = 150)

message("Guardado: data/tbl_composicion_pubpriv_migrante.rds, data/tbl_brecha_pubpriv_migrante.rds, ",
        "data/fig_brecha_pubpriv_migrante.png")
