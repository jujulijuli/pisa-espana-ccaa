# =============================================================================
# 06_exposicion_digital.R
# Descriptivo pormenorizado (H2 de la pregunta de investigación de Julián):
# ¿en qué medida el uso de pantallas/redes sociales ("distractores digitales")
# ayuda a explicar la caída observada en PISA? Dos piezas, confirmadas con él
# 2026-09-16 tras revisar qué variables existen realmente en cada edición:
#
#  (A) SERIE del proxy de exposición 2009-2022 (nacional y por CCAA) -- de
#      contexto/tendencia, no de asociación con la nota.
#  (B) ASOCIACIÓN TRANSVERSAL en 2022 (y ahora también 2025, ver más abajo)
#      entre distracción declarada en clase (el ítem "fuerte", nuevo de
#      2022, sin equivalente en ediciones anteriores a esa) y la
#      puntuación, ajustada por ESCS/género/educación parental, con SE
#      robustas agrupadas por colegio.
#
# AÑADIDO (2026-09-21), a petición de Julián -- ¿se puede comparar 2022 con
# 2025 en distractores digitales? Comprobado variable a variable contra el
# fichero internacional de 2025 (el mismo que usa `01d_incorporate_pisa2025_
# ccaa.R`), no asumido por analogía:
#   - `ST273Q06JA` (distracción PROPIA, 2022) y `ST097Q06DA` (2025) tienen
#     el mismo enunciado literal en inglés ("Students get distracted by
#     using digital resources, e.g. smartphones, websites, apps") y la
#     misma escala 1-4 (cada clase...nunca/casi nunca) -- SÍ comparable.
#     Solo cambia el número de ítem entre ediciones (PISA renumera cada
#     ciclo), no el instrumento.
#   - `IC178Q02JA` (horas/día en redes sociales) existe en 2025 con el
#     mismo código y la misma escala 1-6 -- SÍ comparable.
#   - `ST273Q07JA` (distracción por OTROS compañeros, 2022) NO tiene
#     equivalente en la batería de 2025 (que solo repite el ítem de
#     distracción propia) -- se queda 2022-only, no se puede comparar.
# Las tablas/figuras 2022-only de más abajo se dejan EXACTAMENTE IGUAL que
# antes (mismos nombres de fichero, para no romper qmd/05-descriptivo-
# heterogeneidad.qmd si ya las usa) -- la comparación 2022 vs 2025 se añade
# aparte, al final del script, sin tocarlas.
#
# AÑADIDO (2026-09-21), a petición de Julián -- dos piezas más:
#   - La serie (A) se EXTIENDE a 2025 (a diferencia de la comparación 2022-
#     2025, esto sí sobrescribe `tbl_exposicion_digital_nacional.rds`/
#     `_ccaa.rds`/`fig_exposicion_digital_trend.png`, porque es la MISMA
#     serie, no una comparación nueva -- igual que 01b/01c/01d van
#     extendiendo `student_esp_pooled_extended.rds` en vez de bifurcarlo).
#     2022 y 2025 usan el mismo instrumento (`IC178Q02JA`, horas/día en
#     redes), así que el tramo 2022->2025 es directamente comparable -- la
#     única discontinuidad de instrumento sigue siendo 2015->2022 (marcada
#     en la figura con una línea vertical, ya no con el propio trazo
#     discontinuo, para no confundir el segmento 2022-2025, que SÍ es
#     homogéneo).
#   - Pieza (D), nueva: un "balance" 2022->2025 -- Julián pidió una
#     aproximación a cuánto del cambio se explica por CUÁNTA GENTE está en
#     cada categoría de distracción (prevalencia) frente a CUÁNTO PESA cada
#     categoría en la nota (la asociación/coeficiente). Es una
#     descomposición contable tipo Oaxaca-Blinder simplificada, NO un
#     modelo causal -- ver el aviso largo delante de esa sección.
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

# =============================================================================
# --- (C) AÑADIDO 2026-09-21: comparación 2022 vs 2025 en los dos predictores
# que SÍ son comparables (distr_propia, horas_redes) -- ver aviso al
# principio del script sobre qué se verificó y por qué distr_pares se queda
# fuera. No modifica nada de lo de arriba: ficheros y objetos nuevos, con
# sufijo `_2025` o `_2022_2025`.
# =============================================================================

path_2025_student_zip <- "/Volumes/discojuli/PISA/dataintwww/CY09_MS_STU_PUF.zip"
path_2025_student_sav <- "/Volumes/discojuli/PISA/dataintwww/CY09_MS_STU_PUF.sav"

if (!file.exists(path_2025_student_sav) && file.exists(path_2025_student_zip)) {
  tmp_stu_dir <- file.path(tempdir(), "stu2025_expo")
  dir.create(tmp_stu_dir, showWarnings = FALSE)
  utils::unzip(path_2025_student_zip, exdir = tmp_stu_dir)
  path_2025_student_sav <- list.files(tmp_stu_dir, pattern = "\\.sav$", full.names = TRUE, ignore.case = TRUE)[1]
}

if (!is.na(path_2025_student_sav) && file.exists(path_2025_student_sav) && 2025L %in% unique(student$year)) {

  raw_2025_expo <- haven::read_sav(
    path_2025_student_sav,
    col_select = c("CNT", "CNTSCHID", "CNTSTUID", "ST097Q06DA", "IC178Q02JA")
  ) |>
    filter(as.character(CNT) == "ESP")

  distraccion_2025 <- tibble(
    year = 2025L,
    student_id = as.character(raw_2025_expo$CNTSTUID),
    school_id  = as.character(raw_2025_expo$CNTSCHID),
    # Mismo recode_distraccion()/hours6_to_common() que 2022 -- misma
    # escala verificada, ver aviso al principio del script.
    distr_propia = recode_distraccion(raw_2025_expo$ST097Q06DA),
    horas_redes  = hours6_to_common(raw_2025_expo$IC178Q02JA)
    # distr_pares: sin equivalente en 2025, se omite (no NA a propósito --
    # bind_rows() más abajo la rellena de NA sola, mismo patrón que
    # public_private en 01b).
  )

  analisis_2025 <- student |>
    filter(year == 2025) |>
    mutate(school_id = as.character(school_id), student_id = as.character(student_id)) |>
    inner_join(distraccion_2025, by = c("year", "school_id", "student_id"))

  message(
    "\n2025 -- cobertura tras unir distracción/exposición: ", nrow(analisis_2025),
    " alumnos (de ", sum(student$year == 2025), " en el fichero preparado)"
  )

  saveRDS(bind_rows(distraccion_2022, distraccion_2025), "data/student_distraccion_2022_2025.rds")

  # --- Extiende la serie (A) a 2025 -------------------------------------------
  # Mismo `horas6_to_common(IC178Q02JA)` que ya se usó arriba para
  # `distraccion_2025$horas_redes` -- se reutiliza directamente, no se vuelve
  # a leer el fichero. A diferencia de 2015->2022 (proxy distinto,
  # frecuencia vs. horas), 2022 y 2025 usan EXACTAMENTE el mismo ítem
  # (`IC178Q02JA`), así que este tramo sí es homogéneo.
  exp_2025 <- tibble(
    year = 2025L,
    student_id = as.character(raw_2025_expo$CNTSTUID),
    school_id  = as.character(raw_2025_expo$CNTSCHID),
    exposicion_num = distraccion_2025$horas_redes
  )

  exposicion_all_ext <- bind_rows(exposicion_historica, exp_2022, exp_2025) |>
    left_join(
      student |> select(year, school_id, student_id, ccaa, stu_wgt) |> mutate(school_id = as.character(school_id), student_id = as.character(student_id)),
      by = c("year", "school_id", "student_id")
    ) |>
    filter(!is.na(ccaa), !is.na(stu_wgt))

  exposicion_nacional <- exposicion_all_ext |>
    group_by(year) |>
    group_modify(~ weighted_ci(.x$exposicion_num, .x$stu_wgt)) |>
    ungroup()

  exposicion_ccaa <- exposicion_all_ext |>
    group_by(year, ccaa) |>
    group_modify(~ weighted_ci(.x$exposicion_num, .x$stu_wgt)) |>
    ungroup()

  message("\n=== Proxy de exposición digital -- serie nacional AMPLIADA a 2025 (escala 0-4) ===")
  print(exposicion_nacional, n = Inf)

  # Sobrescribe -- es la misma serie, ahora con un punto más (ver aviso al
  # principio del script sobre por qué esto sí sobrescribe y la comparación
  # 2022-2025 de la pieza (C) no).
  saveRDS(exposicion_nacional, "data/tbl_exposicion_digital_nacional.rds")
  saveRDS(exposicion_ccaa, "data/tbl_exposicion_digital_ccaa.rds")

  fig_exposicion_trend <- exposicion_nacional |>
    ggplot(aes(x = year, y = media)) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.15) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    geom_vline(xintercept = 2018.5, linetype = "dotted", color = "grey50") +
    labs(
      title = "Proxy de exposición a pantallas/redes sociales (escala 0-4)",
      subtitle = "Línea vertical: cambio de instrumento (frecuencia de uso -> horas/día). El tramo 2022-2025 usa el mismo ítem y sí es directamente comparable.",
      x = NULL, y = "Exposición media (IC 95%)",
      caption = "2009/2012/2015: frecuencia de uso recreativo de internet/redes. 2022 y 2025: horas/día en redes sociales (mismo ítem), recodificado a la misma escala."
    ) +
    theme_minimal(base_size = 12)
  ggsave("data/fig_exposicion_digital_trend.png", fig_exposicion_trend, width = 8, height = 5, dpi = 150)

  message("\nSerie (A) ampliada a 2025: data/tbl_exposicion_digital_nacional.rds, data/tbl_exposicion_digital_ccaa.rds, data/fig_exposicion_digital_trend.png (sobrescritos)")

  # Mismos fit_association()/cluster_robust_ci() de la pieza (B) -- solo
  # los dos predictores comparables.
  tbl_asociacion_2025 <- expand.grid(
    domain = domains, predictor = c("distr_propia", "horas_redes"), stringsAsFactors = FALSE
  ) |>
    pmap_dfr(~ fit_association(analisis_2025, ..1, ..2))

  message("\n=== Asociación 2025 distracción/exposición <-> puntuación (score_z, ajustado por ESCS/género/educ. parental) ===")
  print(tbl_asociacion_2025, n = Inf)

  tbl_asociacion_2022_2025 <- bind_rows(
    tbl_asociacion_2022 |> filter(predictor_var %in% c("distr_propia", "horas_redes")) |> mutate(year = 2022L),
    tbl_asociacion_2025 |> mutate(year = 2025L)
  )
  saveRDS(tbl_asociacion_2022_2025, "data/tbl_asociacion_distraccion_2022_2025.rds")

  # --- Figura de comparación: solo distr_propia (el ítem con el enunciado
  # idéntico en las dos ediciones -- el más defendible de los dos para
  # comparar visualmente año a año) ------------------------------------------
  fig_comparacion_2022_2025 <- tbl_asociacion_2022_2025 |>
    filter(predictor_var == "distr_propia") |>
    mutate(
      categoria = sub("^distr_propia", "", term),
      domain_lab = recode(domain, math = "Matemáticas", read = "Lectura", science = "Ciencias"),
      year_lab = factor(year, levels = c(2022, 2025))
    ) |>
    ggplot(aes(x = estimate, y = categoria, xmin = ci_low, xmax = ci_high, color = year_lab)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_pointrange(position = position_dodge(width = 0.5)) +
    facet_wrap(~domain_lab) +
    labs(
      title = "Distracción propia por dispositivos digitales y puntuación -- 2022 vs. 2025",
      subtitle = "Diferencia en DE de puntuación vs. \"nunca/casi nunca\" se distrae, ajustado por ESCS/género/educación parental",
      x = "Diferencia (desviaciones estándar de puntuación, IC 95% robusto por colegio)",
      y = NULL, color = "Edición"
    ) +
    theme_minimal(base_size = 11)
  ggsave("data/fig_asociacion_distr_propia_2022_2025.png", fig_comparacion_2022_2025, width = 9, height = 5, dpi = 150)

  # --- Prevalencia (no la asociación con la nota, sino "cuánta distracción
  # hay") ponderada por año -- complementa la tabla de coeficientes: si la
  # asociación con la nota no cambia mucho pero la prevalencia sí, es una
  # historia distinta (más alumnos afectados) a si cambia la asociación en
  # sí. Un alumno puede aparecer varias veces si se unió con más de un
  # dominio en `analisis_*`, así que se recalcula sobre distraccion_2022/
  # distraccion_2025 (un registro por alumno), no sobre analisis_*.
  prevalencia_distr_propia <- bind_rows(
    distraccion_2022 |> select(year, student_id, distr_propia) |>
      inner_join(student |> filter(year == 2022) |> mutate(student_id = as.character(student_id)) |> select(year, student_id, stu_wgt), by = c("year", "student_id")),
    distraccion_2025 |> select(year, student_id, distr_propia) |>
      inner_join(student |> filter(year == 2025) |> mutate(student_id = as.character(student_id)) |> select(year, student_id, stu_wgt), by = c("year", "student_id"))
  ) |>
    filter(!is.na(distr_propia), !is.na(stu_wgt), stu_wgt > 0) |>
    group_by(year, distr_propia) |>
    summarise(peso = sum(stu_wgt), .groups = "drop") |>
    group_by(year) |>
    mutate(pct = peso / sum(peso)) |>
    ungroup()

  message("\n=== Prevalencia ponderada de distracción propia, 2022 vs 2025 ===")
  print(prevalencia_distr_propia |> select(year, distr_propia, pct), n = Inf)

  saveRDS(prevalencia_distr_propia, "data/tbl_prevalencia_distraccion_2022_2025.rds")

  message(
    "\nAñadido (comparación 2022 vs 2025): data/student_distraccion_2022_2025.rds, ",
    "data/tbl_asociacion_distraccion_2022_2025.rds, data/fig_asociacion_distr_propia_2022_2025.png, ",
    "data/tbl_prevalencia_distraccion_2022_2025.rds"
  )

} else if (!(2025L %in% unique(student$year))) {
  message(
    "\n2025 no está todavía en ", path_extended, " -- ejecuta antes ",
    "01d_incorporate_pisa2025_ccaa.R si quieres la comparación 2022 vs 2025 ",
    "en distractores."
  )
} else {
  message(
    "\nNo encuentro el fichero internacional de 2025 (", path_2025_student_sav,
    ") -- se omite la comparación 2022 vs 2025 en distractores."
  )
}

# =============================================================================
# --- (D) AÑADIDO 2026-09-21: "balance" 2022->2025 -- descomposición tipo
# shift-share (Oaxaca-Blinder simplificado) para separar CUÁNTO del cambio
# en el "coste" medio (en puntos de puntuación) asociado a la distracción se
# debe a que CAMBIÓ CUÁNTA GENTE está en cada categoría (composición /
# prevalencia) frente a CUÁNTO CAMBIÓ LA ASOCIACIÓN de cada categoría con la
# nota (coeficiente). Julián pidió explícitamente mostrar los signos de
# mejora Y de empeoramiento y una aproximación al balance neto, no solo la
# comparación categoría a categoría de la pieza (C).
#
# "Coste medio" de un año x dominio = suma, sobre las categorías de
# distracción, de (prevalencia de esa categoría) x (coeficiente de esa
# categoría, en puntos, frente a "nunca/casi nunca" = referencia = coste 0).
# Es literalmente el mismo número que se obtendría prediciendo la nota media
# atribuible a distracción con la composición y los coeficientes observados
# de ese año -- no es un estadístico nuevo, es una forma de resumir en un
# solo número lo que las tablas de arriba ya muestran categoría a categoría.
#
# Descomposición (convención: composición pesada con coeficientes de 2022,
# coeficiente pesado con composición de 2025 -- la más habitual en la
# literatura Oaxaca-Blinder cuando se quiere leer "cuánto explicaría el
# cambio de composición si la asociación no hubiera cambiado"):
#   efecto_composicion = sum_c (prevalencia_c,2025 - prevalencia_c,2022) * coef_c,2022
#   efecto_coeficiente = sum_c prevalencia_c,2025 * (coef_c,2025 - coef_c,2022)
#   efecto_composicion + efecto_coeficiente == delta_total  (por construcción)
#
# ADVERTENCIA IMPORTANTE (documentar en cualquier informe/manuscrito):
#   - Esto es CONTABILIDAD ARITMÉTICA de un cambio observado, NO un modelo
#     causal ni una prueba de qué "causó" el cambio. Un `efecto_coeficiente`
#     grande no dice POR QUÉ cambió la asociación (podría ser composicional
#     en variables no incluidas, un cambio real en el fenómeno, o ruido
#     muestral -- no se puede distinguir con esto).
#   - La descomposición depende de qué año se usa como base para cada
#     término (aquí: coeficientes de 2022 para el efecto de composición,
#     composición de 2025 para el efecto de coeficiente) -- la convención
#     inversa daría números algo distintos (el llamado "index number
#     problem" de Oaxaca-Blinder). No es un error, es inherente al método.
#   - Los coeficientes de origen ya están ajustados por ESCS/género/
#     educación parental (ver `fit_association()`), pero eso NO se traslada
#     a esta descomposición: aquí no se separa cuánto del cambio en el
#     coeficiente se debe a su vez a cambios en esas covariables.
#   - `horas_redes` es continua (0-4); su "coste" usa el nivel medio
#     ponderado en vez de prevalencia por categoría, mismo principio.
# =============================================================================

if (exists("tbl_asociacion_2022_2025") && exists("prevalencia_distr_propia")) {

  # --- distr_propia: categórico, referencia "Nunca/casi nunca" (coef 0) ------
  # NOTA (corregido 2026-09-21, bug de signo detectado por Julián): `coef`
  # aquí es un COSTE (positivo = puntos perdidos por distracción, frente a la
  # referencia), no el coeficiente crudo de `fit_association()`. Como la
  # distracción se asocia a NOTA MÁS BAJA, `estimate_puntos` es negativo para
  # las categorías de distracción -- hay que invertir el signo (`-estimate_
  # puntos`) para que "coste" suba cuando la nota baja y baje cuando la nota
  # sube, que es la convención que usan el subtítulo del gráfico de abajo
  # ("positivo = peor nota") y el texto del informe ("el coste ha bajado").
  # Con el signo sin invertir, `efecto_coeficiente` salía positivo pese a que
  # la asociación se debilitó (mejoró) de 2022 a 2025 -- justo al revés de
  # lo que decía el subtítulo.
  coef_distr_propia <- tbl_asociacion_2022_2025 |>
    filter(predictor_var == "distr_propia") |>
    mutate(categoria = sub("^distr_propia", "", term)) |>
    select(year, domain, categoria, coef = estimate_puntos) |>
    mutate(coef = -coef) |>
    bind_rows(
      expand.grid(year = c(2022L, 2025L), domain = domains, categoria = "Nunca/casi nunca", stringsAsFactors = FALSE) |>
        mutate(coef = 0)
    )

  prev_wide <- prevalencia_distr_propia |>
    transmute(year, categoria = as.character(distr_propia), pct)

  balance_distr_propia <- coef_distr_propia |>
    inner_join(prev_wide, by = c("year", "categoria")) |>
    select(domain, categoria, year, pct, coef) |>
    tidyr::pivot_wider(names_from = year, values_from = c(pct, coef)) |>
    group_by(domain) |>
    summarise(
      coste_2022 = sum(pct_2022 * coef_2022),
      coste_2025 = sum(pct_2025 * coef_2025),
      efecto_composicion = sum((pct_2025 - pct_2022) * coef_2022),
      efecto_coeficiente = sum(pct_2025 * (coef_2025 - coef_2022)),
      .groups = "drop"
    ) |>
    mutate(
      predictor_var = "distr_propia",
      delta_total = coste_2025 - coste_2022,
      check_suma = round(efecto_composicion + efecto_coeficiente - delta_total, 3)
    ) |>
    select(predictor_var, domain, coste_2022, coste_2025, delta_total,
           efecto_composicion, efecto_coeficiente, check_suma)

  # --- horas_redes: continua, nivel medio ponderado x coeficiente por paso ---
  nivel_horas_redes <- bind_rows(
    distraccion_2022 |> select(year, student_id, horas_redes) |>
      inner_join(student |> filter(year == 2022) |> mutate(student_id = as.character(student_id)) |> select(year, student_id, stu_wgt), by = c("year", "student_id")),
    distraccion_2025 |> select(year, student_id, horas_redes) |>
      inner_join(student |> filter(year == 2025) |> mutate(student_id = as.character(student_id)) |> select(year, student_id, stu_wgt), by = c("year", "student_id"))
  ) |>
    filter(!is.na(horas_redes), !is.na(stu_wgt), stu_wgt > 0) |>
    group_by(year) |>
    summarise(nivel_medio = weighted.mean(horas_redes, stu_wgt), .groups = "drop")

  # Mismo signo invertido que arriba: coef = -estimate_puntos, para que
  # "coste" sea positivo cuando más horas de redes se asocian a peor nota.
  balance_horas_redes <- tbl_asociacion_2022_2025 |>
    filter(predictor_var == "horas_redes") |>
    select(year, domain, coef = estimate_puntos) |>
    mutate(coef = -coef) |>
    inner_join(nivel_horas_redes, by = "year") |>
    select(domain, year, nivel_medio, coef) |>
    tidyr::pivot_wider(names_from = year, values_from = c(nivel_medio, coef)) |>
    mutate(
      predictor_var = "horas_redes",
      coste_2022 = nivel_medio_2022 * coef_2022,
      coste_2025 = nivel_medio_2025 * coef_2025,
      delta_total = coste_2025 - coste_2022,
      efecto_composicion = (nivel_medio_2025 - nivel_medio_2022) * coef_2022,
      efecto_coeficiente = nivel_medio_2025 * (coef_2025 - coef_2022),
      check_suma = round(efecto_composicion + efecto_coeficiente - delta_total, 3)
    ) |>
    select(predictor_var, domain, coste_2022, coste_2025, delta_total,
           efecto_composicion, efecto_coeficiente, check_suma)

  tbl_balance_distraccion_2022_2025 <- bind_rows(balance_distr_propia, balance_horas_redes)

  message("\n=== Balance 2022->2025: descomposición composición vs. coeficiente (puntos de puntuación; check_suma debe ser ~0) ===")
  print(tbl_balance_distraccion_2022_2025, n = Inf)

  saveRDS(tbl_balance_distraccion_2022_2025, "data/tbl_balance_distraccion_2022_2025.rds")

  fig_balance <- tbl_balance_distraccion_2022_2025 |>
    filter(predictor_var == "distr_propia") |>  # el ítem con enunciado idéntico en las dos ediciones -- el más defendible para el resumen visual
    select(domain, efecto_composicion, efecto_coeficiente) |>
    tidyr::pivot_longer(cols = c(efecto_composicion, efecto_coeficiente), names_to = "componente", values_to = "puntos") |>
    mutate(
      componente = recode(componente,
        efecto_composicion = "Cambio en prevalencia\n(cuánta gente distraída)",
        efecto_coeficiente = "Cambio en asociación\n(cuánto pesa estar distraído)"
      ),
      domain_lab = recode(domain, math = "Matemáticas", read = "Lectura", science = "Ciencias")
    ) |>
    ggplot(aes(x = domain_lab, y = puntos, fill = componente)) +
    geom_col(position = "stack") +
    geom_hline(yintercept = 0, color = "grey30") +
    labs(
      title = "Descomposición del cambio 2022->2025 en el coste medio de la distracción propia",
      subtitle = "Positivo = el componente empujó el coste medio hacia peor nota; negativo = lo empujó hacia mejor nota",
      x = NULL, y = "Puntos de puntuación", fill = NULL,
      caption = "Descomposición contable (Oaxaca-Blinder simplificado), no causal -- ver aviso en el script. Referencia = 'nunca/casi nunca' (coste 0)."
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave("data/fig_balance_distraccion_2022_2025.png", fig_balance, width = 8, height = 5, dpi = 150)

  message(
    "\nAñadido (balance 2022 vs 2025): data/tbl_balance_distraccion_2022_2025.rds, ",
    "data/fig_balance_distraccion_2022_2025.png"
  )

} else {
  message(
    "\nBalance 2022 vs 2025 (pieza D) omitido -- necesita que la pieza (C) de ",
    "arriba se haya ejecutado con éxito (con datos de 2025 disponibles)."
  )
}
