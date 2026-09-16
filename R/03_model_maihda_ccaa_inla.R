# =============================================================================
# 03_model_maihda_ccaa_inla.R
# MAIHDA multivariante y multinivel (alumno < colegio < CCAA), por edición.
# Mismo patrón que ../pisa-maihda-trends/R/03_model_maihda_inla.R, pero con
# `ccaa` sustituyendo a `country` como nivel superior -- aquí SÍ es un nivel
# real de jerarquía territorial (no una aproximación), que es justo lo que
# motivó abrir este proyecto aparte.
#
# GENUINAMENTE multivariante (como en el proyecto internacional): los
# efectos aleatorios de estrato y de colegio son trivariantes correlacionados
# (`model = "iid3d"`), uno por dominio, con su matriz de covarianza estimada
# entre los tres. El nivel CCAA se deja como `iid` simple, igual que país en
# el proyecto internacional. Ver el comentario largo al principio de
# ../pisa-maihda-trends/R/03_model_maihda_inla.R para el detalle de cómo
# funciona el índice trivariante (strata_domain_id/school_domain_id con
# n = 3 * m) y el aviso sobre robustez de los nombres de hiperparámetros.
# =============================================================================

library(dplyr)
library(INLA)

# PRUEBA DE VELOCIDAD (2026-09-14): ver el comentario largo equivalente en
# ../pisa-maihda-trends/R/03_model_maihda_inla.R -- subido de "1:1" a "4:1"
# ahora que se sabe que la causa real del crash nativo era `stu_wgt` sin
# filtrar (corregido más abajo), no el threading en sí.
inla.setOption(num.threads = "4:1")

student_long <- readRDS("data/student_esp_long.rds")

domain_labels <- levels(student_long$domain)
stopifnot(length(domain_labels) == 3)

# `strata` tiene niveles fijados globalmente sobre todo el pool (72 posibles,
# ver 02_prepare_variables.R) -- n_strata_global es constante entre años.
n_strata_global <- n_distinct(student_long$strata)

# Ver el comentario largo equivalente en
# ../pisa-maihda-trends/R/03_model_maihda_inla.R: estandarizar `score` antes
# de ajustar evita el error numérico "matrix is not positive definite" (GSL
# Cholesky) visto en la ejecución real del proyecto internacional para el
# efecto trivariante "iid3d". Constante GLOBAL (no por año/dominio) para
# poder deshacer el reescalado de forma exacta; VPC/PCV/correlaciones no
# necesitan deshacerse (son razones/correlaciones, invariantes a un
# reescalado lineal), pero la brecha público-privado/immig y el forest plot
# de estratos sí -- por eso se guarda `score_sd` en data/score_scale.rds
# para que 05_communicate_uncertainty.R los multiplique de vuelta.
score_mean <- mean(student_long$score, na.rm = TRUE)
score_sd <- sd(student_long$score, na.rm = TRUE)
saveRDS(list(mean = score_mean, sd = score_sd), "data/score_scale.rds")

fit_maihda_year <- function(yr) {

  dat <- student_long |>
    filter(year == yr, !is.na(score)) |>
    mutate(
      domain_id = as.integer(domain),
      strata_id = as.integer(strata),
      ccaa_id = as.integer(factor(ccaa))
    )

  # Guarda defensiva (mismo problema visto en pisa-maihda-trends): `strata`
  # es NA si falta género, educación parental, ESCS O immig para esa fila
  # (interaction() propaga NA). Si una edición entera se queda sin alguna de
  # esas variables, `strata_id` sería NA para TODAS las filas y INLA falla
  # con "only NA values in strata_id" en vez de un aviso claro sobre qué año
  # y qué variable. Se descartan las filas con NA y, si no queda suficiente,
  # se salta la edición.
  n_before <- nrow(dat)
  # También se descartan pesos muestrales ausentes o no positivos -- ver el
  # comentario largo en ../pisa-maihda-trends/R/03_model_maihda_inla.R: un NA
  # o un cero en `stu_wgt` que llegue a `weights = ...` puede crashear el
  # binario nativo `inla.run` en vez de dar un error legible de R.
  dat <- dat |>
    filter(
      !is.na(strata_id), !is.na(global_school_id), !is.na(ccaa_id),
      !is.na(stu_wgt), stu_wgt > 0
    )
  n_after <- nrow(dat)
  if (n_after < n_before) {
    message(
      "Año ", yr, ": ", n_before - n_after, " de ", n_before,
      " filas descartadas por strata/colegio/CCAA/peso muestral ausente o no positivo (",
      scales::percent((n_before - n_after) / n_before, accuracy = 0.1), ")."
    )
  }
  if (n_after == 0 || n_distinct(dat$strata_id) < 2) {
    message(
      "Año ", yr, ": SALTADO -- datos insuficientes tras eliminar NA (n=", n_after,
      ", estratos distintos=", n_distinct(dat$strata_id), "). ",
      "Revisa si ESCS/educación parental/immig están disponibles para esta edición."
    )
    return(NULL)
  }

  # `school_id_num` se calcula AQUÍ, DESPUÉS de filtrar -- ver el comentario
  # largo en ../pisa-maihda-trends/R/03_model_maihda_inla.R sobre por qué
  # calcularlo antes del filtro deja "huecos" en la numeración que rompen el
  # supuesto de INLA de índices densos 1..n para un efecto con `n=` explícito
  # (produce el error "Covariate does not match 'values'").
  dat <- dat |> mutate(school_id_num = as.integer(factor(global_school_id)))
  n_school_year <- n_distinct(dat$school_id_num)

  # Diagnóstico de tamaño del problema -- ver el comentario largo
  # equivalente en ../pisa-maihda-trends/R/03_model_maihda_inla.R: el coste
  # computacional del efecto de colegio escala con `n_school_year`
  # (dimensión latente = 3 * n_school_year), y puede tardar bastante incluso
  # con `int.strategy = "eb"` -- no es un cuelgue, es factorizar una matriz
  # dispersa grande.
  message(
    "Año ", yr, ": ajustando con ", n_after, " filas, ", n_strata_global,
    " estratos y ", n_school_year, " colegios (dimensión latente del efecto ",
    "de colegio = 3 x ", n_school_year, " = ", 3L * n_school_year, ")..."
  )

  # PRUEBA DE VELOCIDAD (2026-09-14): `school_domain_id` ya no se usa -- ver
  # el comentario junto a `formula_null`.
  dat <- dat |>
    mutate(
      strata_domain_id = strata_id + (domain_id - 1L) * n_strata_global,
      score_z = (score - score_mean) / score_sd
    )

  # Respuesta = `score_z` (estandarizada, ver comentario arriba), no `score`.
  #
  # PRUEBA DE VELOCIDAD (2026-09-14): ver el comentario largo equivalente en
  # ../pisa-maihda-trends/R/03_model_maihda_inla.R -- el efecto de COLEGIO se
  # simplifica temporalmente de `iid3d` a `iid` simple (compartido entre los
  # 3 dominios) para probar si esto resuelve la lentitud. Estrato se deja
  # trivariante (no se toca). Con este cambio, un colegio ya NO puede
  # cambiar de posición según la habilidad -- regresión deliberada y
  # temporal para la prueba. Para revertir: `f(school_id_num, model =
  # "iid")` -> `f(school_domain_id, model = "iid3d", n = 3L *
  # n_school_year)` (y restaurar `school_domain_id` arriba y en
  # extract_vpc_by_domain/extract_domain_correlations más abajo).
  formula_null <- score_z ~ 0 + domain +
    f(strata_domain_id, model = "iid3d", n = 3L * n_strata_global) +
    f(school_id_num, model = "iid") +
    f(ccaa_id, model = "iid")

  # `control.inla`: ver el comentario largo equivalente en
  # ../pisa-maihda-trends/R/03_model_maihda_inla.R -- `int.strategy = "eb"`
  # (empirical Bayes) evita que el ajuste tarde horas (con el colegio ya
  # simplificado a `iid`, quedan ~9 hiperparámetros, no ~14); sigue dando
  # intervalos de credibilidad reales, pero sin
  # integrar la incertidumbre ENTRE hiperparámetros (pueden salir
  # ligeramente más estrechos que con la rejilla completa -- cambia a
  # `int.strategy = "ccd"` si prefieres la integración completa). `cmin = 0`
  # es el ajuste específico de INLA para el error "matrix is not positive
  # definite" con efectos multivariantes correlacionados.
  # `dic`/`waic` DESACTIVADOS: ver el comentario equivalente en
  # ../pisa-maihda-trends/R/03_model_maihda_inla.R -- nada del pipeline los
  # lee, y calcularlos añade coste evitable con N tan grande. `config =
  # FALSE` (CORRECCIÓN 2026-09-14, ver el comentario largo equivalente en
  # ../pisa-maihda-trends/R/03_model_maihda_inla.R): no hace falta para las
  # funciones de extracción de abajo (usan `$marginals.*`, siempre
  # calculados) -- `config = TRUE` solo hace falta para
  # `inla.posterior.sample()`, que no se usa aquí, y es la causa más
  # probable de "vector memory limit... reached" al cargar los fits
  # guardados en 05_communicate_uncertainty.R.
  # `control.predictor = list(compute = FALSE)` (CORRECCIÓN 2026-09-15, ver
  # el comentario largo equivalente en
  # ../pisa-maihda-trends/R/03_model_maihda_inla.R): `compute = TRUE`
  # guardaba la marginal del predictor lineal por fila, sin usarla nadie
  # después -- probablemente el peso más grande que quedaba tras desactivar
  # `config`.
  fit_null <- inla(
    formula_null, data = dat, family = "gaussian",
    weights = dat$stu_wgt,
    control.compute = list(dic = FALSE, waic = FALSE, config = FALSE),
    control.predictor = list(compute = FALSE),
    control.inla = list(int.strategy = "eb", cmin = 0)
  )

  # `immig` como efecto principal (además de formar parte del estrato desde
  # 02_prepare_variables.R) -- necesario para que el PCV no atribuya de forma
  # espuria a interacción interseccional la varianza que en realidad explica
  # el origen inmigrante por sí solo. `public_private` se añade por una razón
  # distinta: no forma parte de `strata` (es del colegio, no del alumno), así
  # que no afecta al PCV -- se incluye para obtener directamente la brecha
  # público-privado con su intervalo de credibilidad. Ambas condicionales por
  # si se usa una versión de los datos sin esas columnas.
  covars <- c("gender", "parent_educ", "escs_q")
  if ("immig" %in% names(dat)) covars <- c(covars, "immig")
  if ("public_private" %in% names(dat)) covars <- c(covars, "public_private")

  formula_main <- reformulate(
    c("0", "domain", covars,
      sprintf("f(strata_domain_id, model = \"iid3d\", n = %d)", 3L * n_strata_global),
      "f(school_id_num, model = \"iid\")",   # PRUEBA DE VELOCIDAD: ver comentario junto a formula_null
      "f(ccaa_id, model = \"iid\")"),
    response = "score_z"
  )

  # `config = FALSE` / `control.predictor(compute = FALSE)`: ver la
  # corrección junto a `fit_null` arriba.
  fit_main <- inla(
    formula_main, data = dat, family = "gaussian",
    weights = dat$stu_wgt,
    control.compute = list(dic = FALSE, waic = FALSE, config = FALSE),
    control.predictor = list(compute = FALSE),
    control.inla = list(int.strategy = "eb", cmin = 0)
  )

  list(year = yr, null = fit_null, main = fit_main, n_strata = n_strata_global, n_school = n_school_year)
}

# Ver el comentario largo sobre robustez de nombres de hiperparámetros en
# ../pisa-maihda-trends/R/03_model_maihda_inla.R -- misma lógica aquí.
extract_domain_variance_samples <- function(fit, effect_name, domain_labels) {
  hp_names <- names(fit$marginals.hyperpar)
  effect_hp <- grep(effect_name, hp_names, fixed = TRUE, value = TRUE)
  prec_names <- grep("recision", effect_hp, value = TRUE)

  out <- list()
  for (nm in prec_names) {
    digits <- as.integer(regmatches(nm, gregexpr("[1-3]", nm))[[1]])
    dom_idx <- if (length(digits) >= 1) digits[1] else NA_integer_
    lab <- if (!is.na(dom_idx) && dom_idx <= length(domain_labels)) domain_labels[dom_idx] else nm
    var_marg <- inla.tmarginal(function(x) 1 / x, fit$marginals.hyperpar[[nm]])
    out[[lab]] <- inla.rmarginal(4000, var_marg)
  }
  attr(out, "hp_names_found") <- effect_hp
  out
}

extract_vpc_by_domain <- function(fit, effect_name, domain_labels) {
  var_domain_samples <- extract_domain_variance_samples(fit, effect_name, domain_labels)
  var_resid <- inla.tmarginal(function(x) 1 / x, fit$marginals.hyperpar$`Precision for the Gaussian observations`)
  samp_resid <- inla.rmarginal(4000, var_resid)

  if (length(var_domain_samples) != length(domain_labels)) {
    message(
      "AVISO VPC (", effect_name, "): se esperaban ", length(domain_labels),
      " precisiones de dominio y se encontraron ", length(var_domain_samples), ". ",
      "Hiperparámetros vistos para este efecto: ",
      paste(attr(var_domain_samples, "hp_names_found"), collapse = ", ")
    )
  }

  purrr::imap_dfr(var_domain_samples, function(samp_domain, lab) {
    vpc_samples <- samp_domain / (samp_domain + samp_resid)
    tibble::tibble(
      domain = lab,
      vpc_mean = mean(vpc_samples),
      vpc_lower = quantile(vpc_samples, 0.025),
      vpc_upper = quantile(vpc_samples, 0.975)
    )
  })
}

# VPC de la CCAA sigue siendo univariante simple (ccaa_id es `iid`, no
# trivariante) -- mismo cálculo de siempre, solo que ahora como caso
# particular con un único "dominio" ficticio.
extract_vpc_simple <- function(fit, effect_name) {
  var_effect <- inla.tmarginal(function(x) 1 / x, fit$marginals.hyperpar[[paste0("Precision for ", effect_name)]])
  var_resid  <- inla.tmarginal(function(x) 1 / x, fit$marginals.hyperpar$`Precision for the Gaussian observations`)
  samp_effect <- inla.rmarginal(4000, var_effect)
  samp_resid  <- inla.rmarginal(4000, var_resid)
  vpc_samples <- samp_effect / (samp_effect + samp_resid)
  tibble::tibble(
    vpc_mean = mean(vpc_samples),
    vpc_lower = quantile(vpc_samples, 0.025),
    vpc_upper = quantile(vpc_samples, 0.975)
  )
}

extract_pcv_by_domain <- function(fit_null, fit_main, effect_name, domain_labels) {
  var_null <- extract_domain_variance_samples(fit_null, effect_name, domain_labels)
  var_main <- extract_domain_variance_samples(fit_main, effect_name, domain_labels)
  common <- intersect(names(var_null), names(var_main))

  purrr::map_dfr(common, function(lab) {
    pcv_samples <- (var_null[[lab]] - var_main[[lab]]) / var_null[[lab]]
    tibble::tibble(
      domain = lab,
      pcv_mean = mean(pcv_samples),
      pcv_lower = quantile(pcv_samples, 0.025),
      pcv_upper = quantile(pcv_samples, 0.975)
    )
  })
}

extract_domain_correlations <- function(fit, effect_name, domain_labels) {
  hp_names <- rownames(fit$summary.hyperpar)
  effect_hp <- grep(effect_name, hp_names, fixed = TRUE, value = TRUE)
  cor_names <- grep("ho", effect_hp, value = TRUE, ignore.case = TRUE)

  if (length(cor_names) == 0) {
    message(
      "AVISO: no se encontraron hiperparámetros de correlación para '", effect_name,
      "'. Hiperparámetros vistos para este efecto: ", paste(effect_hp, collapse = ", ")
    )
    return(tibble::tibble())
  }

  purrr::map_dfr(cor_names, function(nm) {
    digits <- as.integer(regmatches(nm, gregexpr("[1-3]", nm))[[1]])
    lab <- if (length(digits) >= 2) {
      paste0(domain_labels[digits[1]], "-", domain_labels[digits[2]])
    } else {
      nm
    }
    row <- fit$summary.hyperpar[nm, ]
    tibble::tibble(
      par = lab,
      cor_mean = row[["mean"]],
      cor_lower = row[["0.025quant"]],
      cor_upper = row[["0.975quant"]]
    )
  })
}

years <- sort(unique(student_long$year))
maihda_fits <- lapply(years, fit_maihda_year)
names(maihda_fits) <- years

skipped_years <- names(maihda_fits)[sapply(maihda_fits, is.null)]
if (length(skipped_years) > 0) {
  message(
    "AVISO: ", length(skipped_years), " edición(es) saltada(s) por datos insuficientes: ",
    paste(skipped_years, collapse = ", "),
    ". No aparecerán en vpc_strata_by_year.rds / vpc_ccaa_by_year.rds / pcv_strata_by_year.rds."
  )
}
maihda_fits <- maihda_fits[!sapply(maihda_fits, is.null)]

# VPC del estrato interseccional (desigualdad social, por dominio) Y de la
# CCAA (desigualdad territorial, univariante) por separado -- interesante
# comparar cuál pesa más y cómo evoluciona, y si el peso de la desigualdad
# social difiere entre mates/lectura/ciencia.
vpc_strata_by_year <- purrr::map_dfr(maihda_fits, ~ extract_vpc_by_domain(.x$null, "strata_domain_id", domain_labels), .id = "year")
vpc_ccaa_by_year    <- purrr::map_dfr(maihda_fits, ~ extract_vpc_simple(.x$null, "ccaa_id"), .id = "year")
pcv_strata_by_year  <- purrr::map_dfr(maihda_fits, ~ extract_pcv_by_domain(.x$null, .x$main, "strata_domain_id", domain_labels), .id = "year")
cor_strata_by_year  <- purrr::map_dfr(maihda_fits, ~ extract_domain_correlations(.x$null, "strata_domain_id", domain_labels), .id = "year")

# PRUEBA DE VELOCIDAD (2026-09-14): colegio es ahora `iid` simple (ver
# comentario en fit_maihda_year), así que su VPC es univariante (reusa
# extract_vpc_simple, la misma función que ya se usa para ccaa_id) y ya NO
# hay correlación entre dominios a nivel de colegio que extraer -- se quita
# cor_school_by_year. Si se revierte a `iid3d`, volver a usar
# extract_vpc_by_domain()/extract_domain_correlations() aquí como para
# "strata_domain_id" arriba.
vpc_school_by_year <- purrr::map_dfr(maihda_fits, ~ extract_vpc_simple(.x$null, "school_id_num"), .id = "year")

saveRDS(maihda_fits, "data/maihda_fits_ccaa_by_year.rds")
saveRDS(vpc_strata_by_year, "data/vpc_strata_by_year.rds")
saveRDS(vpc_ccaa_by_year, "data/vpc_ccaa_by_year.rds")
saveRDS(pcv_strata_by_year, "data/pcv_strata_by_year.rds")
saveRDS(cor_strata_by_year, "data/cor_strata_by_year.rds")
saveRDS(vpc_school_by_year, "data/vpc_school_by_year.rds")

message(
  "Correlaciones entre dominios (estrato), última edición ajustada:\n",
  paste(capture.output(print(cor_strata_by_year |> filter(year == max(year)))), collapse = "\n")
)
