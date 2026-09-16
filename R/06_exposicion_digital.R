# =============================================================================
# 06_exposicion_digital.R
# Descriptivo pormenorizado (H2 de la pregunta de investigación de Julián):
# ¿en qué medida el uso de pantallas/redes sociales ("distractores digitales")
# ayuda a explicar la caída observada en PISA? Dos piezas, confirmadas con él
# 2026-09-16 tras revisar qué variables existen realmente en cada edición:
#
#  (A) SERIE del proxy de exposición 2009-2022 (nacional y por CCAA) -- de
#      contexto/tendencia, no de asociación con la nota.
#  (B) ASOCIACIÓN TRANSVERSAL en 2022 entre distracción declarada en clase
#      (el ítem "fuerte", nuevo de 2022, sin equivalente en ediciones
#      anteriores) y la puntuación, ajustada por ESCS/género/educación
#      parental, con SE robustas agrupadas por colegio.
#
# ADVERTENCIA METODOLÓGICA IMPORTANTE sobre (A), verificada variable a
# variable en los ficheros reales (no asumida) -- documentar en cualquier
# informe/manuscrito:
#   - Antes de 2022 NO existe ningún ítem de "me distraigo en clase por
#     dispositivos digitales" -- la serie (A) usa un PROXY distinto:
#     frecuencia de uso recreativo de internet/redes fuera del colegio.
#   - Y ese proxy no se mide igual en todas las ediciones. 2009 (P09_ST26Q02,
#     frecuencia de chat online), 2012 (P12_IC08Q06, frecuencia de navegar
#     por internet "por diversión") y 2015 (P15_IC008Q05TA, frecuencia de
#     uso de redes sociales) SÍ comparten una escala de FRECUENCIA de 5
#     niveles (nunca -> varias veces al día), recodificada aquí a una escala
#     común 0-4. PERO 2022 (IC178Q02JA) pregunta HORAS AL DÍA en redes
#     sociales, no frecuencia -- es un constructo distinto (intensidad, no
#     frecuencia; para 2022 casi el 100% ya usa redes a diario, así que la
#     pregunta relevante pasó a ser "cuánto", no "si"). Se recodifica aquí a
#     la misma escala ordinal 0-4 PARA PODER DIBUJAR UNA SOLA SERIE, pero el
#     salto 2015->2022 mezcla un cambio real (más uso) con un cambio de
#     instrumento -- no interpretar la magnitud del salto como puramente
#     sustantivo. Queda documentado en el propio gráfico (línea discontinua
#     entre 2015 y 2022) y aquí.
#   - 2009 código 1 ("Don't know what it is") se trata como NA (desconocer
#     la tecnología no es lo mismo que "nunca la uso"), no como nivel 0.
#
# La pieza (B) es la que de verdad responde a la pregunta de investigación
# de Julián con el instrumento correcto -- (A) es contexto/tendencia, no
# evidencia causal ni siquiera asociativa con la nota.
# =============================================================================

library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

path_extended  <- "data/student_esp_pooled_extended.rds"
path_2022_only <- "data/student_esp_pooled.rds"
path_raw_2022  <- "data/PISA2022_Estudiantes_Esp.sav"

if (file.exists(path_extended)) {
  student <- readRDS(path_extended)
} else {
  message("Solo tengo ", path_2022_only, " -- la serie (A) se limitará a 2022.")
  student <- readRDS(path_2022_only)
}
stopifnot(
  "Necesito data_raw/PISA2022_Estudiantes_Esp.sav para los ítems de distracción/exposición de 2022 (no están en 01_load_data.R)" =
    file.exists(path_raw_2022)
)

# Códigos especiales SPSS (Valid Skip/Not Applicable/Invalid/No Response) que
# haven NO convierte a NA automáticamente -- hay que limpiarlos a mano, mismo
# patrón defensivo que el resto del proyecto.
clean_special <- function(x, thresh = 90) {
  x <- as.numeric(x)
  x[x >= thresh] <- NA_real_
  x
}

# --- (A) Serie histórica del proxy de exposición -----------------------------
raw_hist_path <- "/Volumes/discojuli/PISA/pisa-espana-ccaa/data/ESP_PISA00_03_06_09_12_15.sav"

exposicion_historica <- NULL
if (file.exists(raw_hist_path)) {
  raw_hist <- haven::read_sav(
    raw_hist_path,
    col_select = any_of(c(
      "P09_StIDStd", "P09_SCHOOLID_PISA", "P09_ST26Q02",
      "P12_StIDStd", "P12_SCHOOLID_PISA", "P12_IC08Q06",
      "P15_CNTSTUID", "P15_CNTSCHID", "P15_CNT", "P15_IC008Q05TA"
    ))
  )

  # Frecuencia 1..5 (nunca..varias veces al día) -> escala común 0..4.
  # 2009 nivel 1 ("no sé qué es") -> NA (ver nota arriba).
  freq5_to_common <- function(x, na_code_1 = FALSE) {
    x <- clean_special(x, thresh = 7)  # 2009/2012 usan 7/8/9 como especiales
    if (na_code_1) x[x == 1] <- NA_real_
    x - 1  # 1..5 (o 2..5 tras marcar NA) -> 0..4 aproximado
  }

  exp_2009 <- tibble(
    year = 2009L,
    student_id = as.character(raw_hist$P09_StIDStd),
    school_id  = as.character(raw_hist$P09_SCHOOLID_PISA),
    exposicion_num = freq5_to_common(raw_hist$P09_ST26Q02, na_code_1 = TRUE)
  )
  exp_2012 <- tibble(
    year = 2012L,
    student_id = as.character(raw_hist$P12_StIDStd),
    school_id  = as.character(raw_hist$P12_SCHOOLID_PISA),
    exposicion_num = freq5_to_common(raw_hist$P12_IC08Q06)
  )
  exp_2015 <- tibble(
    year = 2015L,
    student_id = as.character(raw_hist$P15_CNTSTUID),
    school_id  = as.character(raw_hist$P15_CNTSCHID),
    exposicion_num = freq5_to_common(clean_special(raw_hist$P15_IC008Q05TA, thresh = 95))
  ) |>
    filter(raw_hist$P15_CNT == "QES")  # misma submuestra con CCAA que 01b

  exposicion_historica <- bind_rows(exp_2009, exp_2012, exp_2015) |>
    filter(!is.na(exposicion_num))
  rm(raw_hist)
} else {
  message(
    "No encuentro ", raw_hist_path, " -- la serie (A) se limitará a 2022. ",
    "Ajusta `raw_hist_path` si lo tienes en otra ruta."
  )
}

# 2022: horas/día en redes sociales (IC178Q02JA, 1..6) -> misma escala 0..4
# (1 no tiempo -> 0; 2 <1h -> 1; 3 1-3h -> 2; 4 3-5h -> 3; 5-6 >5h -> 4).
raw_2022 <- haven::read_sav(
  path_raw_2022,
  col_select = any_of(c("CNTSTUID", "CNTSCHID", "IC178Q02JA", "ST273Q06JA", "ST273Q07JA"))
)
hours6_to_common <- function(x) {
  x <- clean_special(x)
  case_when(
    x == 1 ~ 0, x == 2 ~ 1, x == 3 ~ 2, x == 4 ~ 3, x %in% c(5, 6) ~ 4,
    TRUE ~ NA_real_
  )
}
exp_2022 <- tibble(
  year = 2022L,
  student_id = as.character(raw_2022$CNTSTUID),
  school_id  = as.character(raw_2022$CNTSCHID),
  exposicion_num = hours6_to_common(raw_2022$IC178Q02JA)
)

exposicion_all <- bind_rows(exposicion_historica, exp_2022) |>
  left_join(
    student |> select(year, school_id, student_id, ccaa, stu_wgt) |> mutate(school_id = as.character(school_id), student_id = as.character(student_id)),
    by = c("year", "school_id", "student_id")
  ) |>
  filter(!is.na(ccaa), !is.na(stu_wgt))

weighted_ci <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  n_eff <- (sum(w))^2 / sum(w^2)  # tamaño muestral efectivo, ajuste simple por ponderación
  m <- weighted.mean(x, w)
  v <- sum(w * (x - m)^2) / sum(w)
  se <- sqrt(v / n_eff)
  tibble(media = m, ci_low = m - 1.96 * se, ci_high = m + 1.96 * se, n = length(x), n_eff = n_eff)
}

exposicion_nacional <- exposicion_all |>
  group_by(year) |>
  group_modify(~ weighted_ci(.x$exposicion_num, .x$stu_wgt)) |>
  ungroup()

exposicion_ccaa <- exposicion_all |>
  group_by(year, ccaa) |>
  group_modify(~ weighted_ci(.x$exposicion_num, .x$stu_wgt)) |>
  ungroup()

message("=== Proxy de exposición digital -- serie nacional (escala 0-4) ===")
print(exposicion_nacional, n = Inf)

saveRDS(exposicion_nacional, "data/tbl_exposicion_digital_nacional.rds")
saveRDS(exposicion_ccaa, "data/tbl_exposicion_digital_ccaa.rds")

fig_exposicion_trend <- exposicion_nacional |>
  ggplot(aes(x = year, y = media)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15) +
  geom_line(aes(linetype = year > 2015), linewidth = 1, show.legend = FALSE) +
  geom_point(size = 2) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  labs(
    title = "Proxy de exposición a pantallas/redes sociales (escala 0-4)",
    subtitle = "Línea discontinua 2015->2022: cambia el instrumento de medida (frecuencia -> horas/día), ver notas del script",
    x = NULL, y = "Exposición media (IC 95%)",
    caption = "2009/2012/2015: frecuencia de uso recreativo de internet/redes. 2022: horas/día en redes sociales, recodificado a la misma escala."
  ) +
  theme_minimal(base_size = 12)
ggsave("data/fig_exposicion_digital_trend.png", fig_exposicion_trend, width = 8, height = 5, dpi = 150)

# --- (B) Asociación transversal 2022: distracción declarada en clase --------
# ST273Q06JA (distracción propia) / ST273Q07JA (distracción por otros), 1-4
# (1=cada clase ... 4=nunca/casi nunca) -- se invierte a 0-3 con "nunca/casi
# nunca" = 0 como referencia, para que un coeficiente positivo signifique
# "más distracción se asocia con..." de forma intuitiva.
recode_distraccion <- function(x) {
  x <- clean_special(x)
  factor(
    case_when(
      x == 4 ~ "Nunca/casi nunca", x == 3 ~ "Algunas clases",
      x == 2 ~ "La mayoría de clases", x == 1 ~ "Cada clase",
      TRUE ~ NA_character_
    ),
    levels = c("Nunca/casi nunca", "Algunas clases", "La mayoría de clases", "Cada clase")
  )
}

distraccion_2022 <- tibble(
  year = 2022L,
  student_id = as.character(raw_2022$CNTSTUID),
  school_id  = as.character(raw_2022$CNTSCHID),
  distr_propia = recode_distraccion(raw_2022$ST273Q06JA),
  distr_pares  = recode_distraccion(raw_2022$ST273Q07JA),
  horas_redes  = hours6_to_common(raw_2022$IC178Q02JA)
)

analisis_2022 <- student |>
  filter(year == 2022) |>
  mutate(school_id = as.character(school_id), student_id = as.character(student_id)) |>
  inner_join(distraccion_2022, by = c("year", "school_id", "student_id"))

message("\nCobertura 2022 tras unir distracción/exposición: ", nrow(analisis_2022),
        " alumnos (de ", sum(student$year == 2022), " en el fichero preparado)")

saveRDS(distraccion_2022, "data/student_distraccion_2022.rds")

# Error estándar robusto agrupado por colegio (sandwich CR1), sin depender de
# paquetes nuevos (el proyecto ya usa INLA para el modelado confirmatorio;
# esto es la pieza descriptiva/exploratoria, con intervalos DE CONFIANZA
# frecuentistas, no credibilidad bayesiana -- distinción indicada a
# propósito en las tablas para no confundirlo con 03/04_model_*_inla.R).
cluster_robust_ci <- function(fit, cluster) {
  # `data` ya se filtró antes de llamar a lm() para que todas las variables
  # del modelo estén completas, así que lm() no descarta filas adicionales
  # -- model.matrix(fit) mantiene el mismo orden/longitud que `cluster`.
  X <- model.matrix(fit)
  stopifnot(nrow(X) == length(cluster))
  w <- weights(fit)
  score <- (residuals(fit) * w) * X  # contribución i: w_i * u_i * x_i (WLS "sandwich")
  meat <- rowsum(score, cluster)     # suma por colegio antes de elevar al cuadrado
  bread <- solve(t(X * w) %*% X)     # (X'WX)^-1
  n_clust <- length(unique(cluster))
  k <- ncol(X)
  n <- nrow(X)
  adj <- (n_clust / (n_clust - 1)) * ((n - 1) / (n - k))
  vcov_cr <- adj * bread %*% (t(meat) %*% meat) %*% bread
  se <- sqrt(diag(vcov_cr))
  tibble(
    term = names(coef(fit)),
    estimate = coef(fit),
    se_cluster = se,
    ci_low = estimate - 1.96 * se,
    ci_high = estimate + 1.96 * se,
    n_clusters = n_clust
  )
}

fit_association <- function(data, domain, predictor) {
  d <- data |>
    filter(!is.na(.data[[domain]]), !is.na(.data[[predictor]]), !is.na(escs),
           !is.na(gender), !is.na(parent_educ), !is.na(stu_wgt), stu_wgt > 0) |>
    mutate(score_z = as.numeric(scale(.data[[domain]])))
  if (nrow(d) < 100) return(NULL)

  # DE -> puntos (pedido por Julián 2026-09-16): la DE de `score_z` es la
  # desviación típica de la muestra analítica EFECTIVAMENTE usada en este
  # ajuste concreto (tras el filtrado de NA de arriba, distinto para cada
  # combinación dominio x predictor porque cada predictor tiene su propio
  # patrón de missing) -- se guarda aquí, en el momento exacto en que
  # `scale()` la usa, para no arriesgarse a recalcularla luego con un
  # filtrado ligeramente distinto y que no cuadre.
  sd_domain <- sd(d[[domain]], na.rm = TRUE)

  f <- as.formula(paste("score_z ~", predictor, "+ escs + gender + parent_educ"))
  fit <- lm(f, data = d, weights = stu_wgt)
  cluster_robust_ci(fit, d$school_id) |>
    filter(grepl(paste0("^", predictor), term)) |>
    mutate(
      domain = domain, predictor_var = predictor, sd_domain_puntos = sd_domain,
      estimate_puntos = estimate * sd_domain,
      ci_low_puntos = ci_low * sd_domain,
      ci_high_puntos = ci_high * sd_domain,
      .before = 1
    )
}

domains <- c("math", "read", "science")
predictors <- c("distr_propia", "distr_pares", "horas_redes")

tbl_asociacion_2022 <- expand.grid(domain = domains, predictor = predictors, stringsAsFactors = FALSE) |>
  pmap_dfr(~ fit_association(analisis_2022, ..1, ..2))

message("\n=== Asociación 2022 distracción/exposición <-> puntuación (score_z, ajustado por ESCS/género/educ. parental) ===")
print(tbl_asociacion_2022, n = Inf)

saveRDS(tbl_asociacion_2022, "data/tbl_asociacion_distraccion_2022.rds")

# --- Gráfico tipo forest plot de la asociación (categorías de distracción) -
fig_asociacion <- tbl_asociacion_2022 |>
  filter(predictor_var %in% c("distr_propia", "distr_pares")) |>
  mutate(
    categoria = sub("^distr_(propia|pares)", "", term),
    predictor_lab = recode(predictor_var,
      distr_propia = "Distracción propia (dispositivo propio)",
      distr_pares  = "Distracción por otros (dispositivo de compañeros)"
    ),
    domain_lab = recode(domain, math = "Matemáticas", read = "Lectura", science = "Ciencias")
  ) |>
  ggplot(aes(x = estimate, y = categoria, xmin = ci_low, xmax = ci_high, color = domain_lab)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  facet_wrap(~predictor_lab, ncol = 1) +
  labs(
    title = "Distracción en clase por dispositivos digitales y puntuación PISA 2022",
    subtitle = "Diferencia en DE de puntuación vs. \"nunca/casi nunca\" se distrae, ajustado por ESCS/género/educación parental",
    x = "Diferencia (desviaciones estándar de puntuación, IC 95% robusto por colegio)",
    y = NULL, color = "Dominio"
  ) +
  theme_minimal(base_size = 11)
ggsave("data/fig_asociacion_distraccion_2022.png", fig_asociacion, width = 9, height = 7, dpi = 150)

# --- Distribución real de puntuaciones por categoría de distracción --------
# Complementa la tabla de coeficientes ajustados (que resume la asociación en
# un único número por categoría) con la distribución completa de puntuación
# -- pedido por Julián para poder valorar la magnitud "a ojo", no solo por el
# coeficiente. Sin ponderar (es descriptivo/visual, la tabla de asociación de
# arriba sí está correctamente ponderada) y sin ajustar por covariables --
# por eso las medianas pueden diferir algo de lo que sugiere la tabla
# ajustada, que sí controla por ESCS/género/educación parental.
fig_distribucion <- analisis_2022 |>
  filter(!is.na(distr_propia)) |>
  select(math, read, science, distr_propia) |>
  tidyr::pivot_longer(cols = c(math, read, science), names_to = "domain", values_to = "score") |>
  mutate(domain_lab = recode(domain, math = "Matemáticas", read = "Lectura", science = "Ciencias")) |>
  ggplot(aes(x = distr_propia, y = score, fill = distr_propia)) +
  geom_boxplot(outlier.alpha = 0.15, show.legend = FALSE) +
  facet_wrap(~domain_lab) +
  labs(
    title = "Distribución de puntuación PISA 2022 según distracción propia en clase",
    subtitle = "Sin ponderar ni ajustar (descriptivo) -- ver tabla ajustada arriba para la asociación con covariables",
    x = NULL, y = "Puntuación"
  ) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave("data/fig_distribucion_distraccion_2022.png", fig_distribucion, width = 9, height = 5, dpi = 150)

message("\nGuardado: data/tbl_exposicion_digital_nacional.rds, data/tbl_exposicion_digital_ccaa.rds, ",
        "data/fig_exposicion_digital_trend.png, data/student_distraccion_2022.rds, ",
        "data/tbl_asociacion_distraccion_2022.rds (ahora con estimate_puntos/ci_low_puntos/ci_high_puntos), ",
        "data/fig_asociacion_distraccion_2022.png, data/fig_distribucion_distraccion_2022.png")
