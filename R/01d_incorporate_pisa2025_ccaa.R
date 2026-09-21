# =============================================================================
# 01d_incorporate_pisa2025_ccaa.R
# Añade 2025 al proyecto usando el fichero INTERNACIONAL de la OCDE
# (CY09_MS_STU_PUF.sav) filtrado a España, en vez de un fichero del INEE --
# a diferencia de 2009/2012/2015/2022, el INEE remite directamente al
# fichero internacional para 2025 (confirmado por Julián: no hay microdatos
# nacionales separados esta vez). La variable que da comunidad autónoma es
# `REGION` (NO `SUBNATIO`, que para España solo da el código de país
# completo) -- mismo hallazgo que ya se documentó en
# `../pisa-maihda-trends/R/06_incorporate_pisa2025.R`, que usa el mismo
# fichero fuente y del que se reutiliza aquí la lógica de PVs/pesos/IMMIG/
# HISCED (verificada allí contra el codebook oficial).
#
# *** AVISO IMPORTANTE, verificado dos veces (contra `attributes(ESP$REGION)`
# que nos pasó Julián Y de forma independiente aquí con pyreadstat) ***:
# los CÓDIGOS numéricos de `REGION` en este fichero internacional NO
# coinciden con los del fichero nacional de 2022 (`region_lookup` en
# `01_load_data.R`). P.ej. el código 72405 es "Canarias" en el fichero de
# 2022 del INEE, pero es "Basque Country" (País Vasco) en este fichero de
# la OCDE; 72411 es "Galicia" en 2022 pero "Ceuta" aquí. Son DOS
# variables distintas que por casualidad comparten nombre (`REGION`) y
# rango de códigos (72400-72419), pero NO el mismo mapeo -- así que este
# script tiene su PROPIA tabla de equivalencia (`region_lookup_2025` más
# abajo), no reutiliza `region_lookup` de `01_load_data.R`. Si algún día
# fusionas o refactorizas esto, NO asumas que los códigos son
# intercambiables entre ficheros -- vuelve a verificar contra las
# etiquetas de valor reales de cada fichero (`attr(x, "labels")`).
#
# Otra diferencia relevante frente a `01b`: en 2025 Ceuta y Melilla
# aparecen SEPARADAS (como en 2022; a diferencia de 2009, donde solo
# existe "Ceuta y Melilla" combinada, y de 2012, donde no aparecen en
# absoluto). Esto significa que, tras incorporar 2025, Ceuta y Melilla
# pasan a tener 2 ediciones (2022 y 2025) en vez de 1 -- con lo que
# `has_trend` (ver `05_communicate_uncertainty.R`) las incluirá por
# primera vez en el forest plot de tendencias. Sigue siendo una
# "tendencia" de solo dos puntos (intervalo de credibilidad
# necesariamente muy ancho), así que el texto de
# `qmd/04-resultados.qmd` que las excluía por completo hay que
# actualizarlo, no solo dejar que el filtro `has_trend` las cuele sin
# comentario.
# =============================================================================

library(haven)
library(dplyr)

# --- 0. Ruta al fichero fuente -----------------------------------------------
# Mismo fichero que usa `../pisa-maihda-trends/R/06_incorporate_pisa2025.R`
# -- se apunta aquí directamente a donde ya está descargado (igual que
# `01c_incorporate_public_private_historic.R` apunta a rutas de
# `/Volumes/discojuli/PISA/dataintwww/` en vez de copiarlo a `data_raw/`)
# para no duplicar ~2 GB. Ajusta si lo tienes en otro sitio.
path_2025_student_zip <- "/Volumes/discojuli/PISA/dataintwww/CY09_MS_STU_PUF.zip"
path_2025_student_sav <- "/Volumes/discojuli/PISA/dataintwww/CY09_MS_STU_PUF.sav"
path_2025_school_zip  <- "/Volumes/discojuli/PISA/dataintwww/CY09_MS_SCH_PUF.zip"
path_extended         <- "data/student_esp_pooled_extended.rds"

stopifnot(
  "Ejecuta antes 01_load_data.R, 01b_incorporate_historic_ccaa.R y 01c_incorporate_public_private_historic.R (necesito data/student_esp_pooled_extended.rds)" =
    file.exists(path_extended)
)

# El .sav de alumno puede estar ya descomprimido (como aquí) o solo el
# .zip -- admite los dos casos, igual que 01c hace para sus ficheros de
# centro.
if (!file.exists(path_2025_student_sav)) {
  stopifnot(
    "No encuentro ni el .sav ni el .zip del fichero de alumno de 2025 -- ajusta las rutas arriba" =
      file.exists(path_2025_student_zip)
  )
  message("Descomprimiendo fichero de alumno de 2025 (pesa ~1 GB comprimido)...")
  tmp_stu_dir <- file.path(tempdir(), "stu2025")
  dir.create(tmp_stu_dir, showWarnings = FALSE)
  utils::unzip(path_2025_student_zip, exdir = tmp_stu_dir)
  path_2025_student_sav <- list.files(tmp_stu_dir, pattern = "\\.sav$", full.names = TRUE, ignore.case = TRUE)[1]
  stopifnot("No encuentro el .sav dentro del zip de alumno de 2025" = !is.na(path_2025_student_sav))
}

# --- 1. Leer solo las columnas necesarias ------------------------------------
# El fichero completo trae 885 columnas / ~2,2 GB -- leerlo entero sería
# lento y usaría mucha RAM para nada, así que se seleccionan solo las ~40
# columnas que hacen falta (mismo patrón que el `col_select` que ya usa
# `01c` para sus ficheros de centro). Nombres verificados contra el
# fichero real (pyreadstat), no asumidos por analogía.
message("Leyendo fichero internacional 2025 (solo columnas necesarias)...")
raw2025_all <- haven::read_sav(
  path_2025_student_sav,
  col_select = c(
    "CNT", "CNTSCHID", "CNTSTUID", "REGION", "ST004D01T", "MALE", "IMMIG", "HISCED",
    "ESCS", "W_FSTUWT",
    tidyselect::matches("^PV\\d+(MATH|READ|SCIE)$")
  )
)

raw2025 <- raw2025_all |> filter(as.character(CNT) == "ESP")
message("Alumnos ESP en el fichero internacional 2025: ", nrow(raw2025))
stopifnot("No hay filas con CNT == 'ESP' -- revisa el nombre/valor de la columna de país" = nrow(raw2025) > 0)
rm(raw2025_all)

# --- 2. Tabla de equivalencia REGION -> CCAA, PROPIA de este fichero --------
# Verificada dos veces (ver aviso arriba): contra `attributes(ESP$REGION)`
# que Julián compartió directamente, y de forma independiente aquí con
# pyreadstat sobre el fichero real. NO es la misma tabla que
# `region_lookup` en `01_load_data.R`.
region_lookup_2025 <- tibble::tribble(
  ~region_code, ~ccaa,
  "72401", "Andalucía",
  "72402", "Aragón",
  "72403", "Asturias",
  "72404", "Illes Balears",
  "72405", "País Vasco",
  "72406", "Canarias",
  "72407", "Cantabria",
  "72408", "Castilla y León",
  "72409", "Castilla-La Mancha",
  "72410", "Cataluña",
  "72411", "Ceuta",
  "72412", "Comunidad Valenciana",
  "72413", "Extremadura",
  "72414", "Galicia",
  "72415", "La Rioja",
  "72416", "Madrid",
  "72417", "Melilla",
  "72418", "Murcia",
  "72419", "Navarra"
  # 72400 "Spain: Rest of Country" -- residual sin CCAA asignada, se deja
  # fuera a propósito (queda como ccaa = NA y se descarta más abajo, igual
  # que los códigos "not adjudicated" de 2012 en 01b).
)

# --- 3. Público/privado, desde el fichero de CENTRO --------------------------
# Mismo patrón que `06_incorporate_pisa2025.R` (PRIVATESCH preferida,
# SC013Q01TA de respaldo), pero devolviendo "publico"/"privado" (los
# niveles que usa ESTE proyecto en toda la serie, no el inglés que usa el
# proyecto internacional).
harmonize_public_private <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  case_when(
    x_chr %in% c("1", "public", "publico", "público")  ~ "publico",
    x_chr %in% c("2", "private", "privado")             ~ "privado",
    TRUE ~ NA_character_
  )
}

path_2025_school_sav <- "/Volumes/discojuli/PISA/dataintwww/CY09_MS_SCH_PUF.sav"
if (!file.exists(path_2025_school_sav)) {
  if (file.exists(path_2025_school_zip)) {
    tmp_sch_dir <- file.path(tempdir(), "sch2025")
    dir.create(tmp_sch_dir, showWarnings = FALSE)
    utils::unzip(path_2025_school_zip, exdir = tmp_sch_dir)
    path_2025_school_sav <- list.files(tmp_sch_dir, pattern = "\\.sav$", full.names = TRUE, ignore.case = TRUE)[1]
  } else {
    path_2025_school_sav <- NA_character_
  }
}

if (!is.na(path_2025_school_sav) && file.exists(path_2025_school_sav)) {
  raw2025_school <- haven::read_sav(
    path_2025_school_sav,
    col_select = tidyselect::any_of(c("CNT", "CNTSCHID", "PRIVATESCH", "SC013Q01TA"))
  )
  school_type_2025 <- raw2025_school |>
    filter(as.character(CNT) == "ESP") |>
    transmute(
      school_id = as.character(CNTSCHID),
      public_private = coalesce(
        if ("PRIVATESCH" %in% names(raw2025_school)) harmonize_public_private(PRIVATESCH) else NA_character_,
        if ("SC013Q01TA" %in% names(raw2025_school)) harmonize_public_private(SC013Q01TA) else NA_character_
      ) |> factor(levels = c("publico", "privado"))
    )
  message(
    "2025 -- colegios ESP con público/privado clasificado: ",
    sum(!is.na(school_type_2025$public_private)), " de ", nrow(school_type_2025)
  )
} else {
  message("2025: no encuentro el fichero de centro -- public_private quedará NA para esta edición.")
  school_type_2025 <- NULL
}

# --- 4. PVs, IMMIG, HISCED, género -------------------------------------------
# HISCED 1-9 (no 1-10 como en el fichero de 2022 del INEE -- ver el aviso
# de escala distinta en 01_load_data.R; escala verificada aquí contra las
# etiquetas de valor reales del fichero, coincide con la que ya documentó
# `06_incorporate_pisa2025.R`).
#
# *** HALLAZGO EMPÍRICO (2026-09-21), no documentado en el codebook que
# tenías: `ST004D01T` (género Female/Male tradicional) está AUSENTE al
# 100% para España en este fichero -- verificado contra el fichero real,
# no es un fallo de este script. No es solo España: 10 países del fichero
# (entre ellos Alemania, Países Bajos, Irlanda, Dinamarca, Bélgica,
# Canadá, Chile, Colombia, Islandia) tienen 0% de cobertura en
# `ST004D01T`, mientras que la mayoría de los demás países tienen 0% de
# NA -- es decir, no es un patrón aleatorio de no-respuesta, sino que ese
# grupo de países sencillamente no publica esa variable en 2025 (lectura
# más plausible: restricciones nacionales sobre recogida/publicación de
# género binario en estadística educativa oficial). En su lugar, la OCDE
# incluye `MALE`, una variable MÁS TOSCA pensada para estos casos:
# 1 = Male, 0 = "Female / Other" (ver etiquetas de valor reales del
# fichero) -- confirmado con cobertura del 100% para España. Se usa como
# alternativa SOLO cuando falta `ST004D01T` (por si una actualización
# futura del fichero la incluyera). Consecuencia real, no cosmética: para
# España en 2025 la dimensión "género" del estrato MAIHDA distingue
# Male vs. "Female / Other" (agrega cualquier alumno no binario con
# "female"), no Female vs. Male como en el resto de ediciones -- una
# diferencia de medición que conviene mencionar en qmd/03-metodos.qmd si
# no está ya.
pv_mean <- function(df, domain_suffix) {
  cols <- grep(paste0("^PV\\d+", domain_suffix, "$"), names(df), value = TRUE)
  stopifnot(length(cols) == 10)
  rowMeans(df[cols], na.rm = TRUE)
}

hisced_to_3cat_2025 <- function(x) {
  case_when(
    x %in% c(1, 2)       ~ "baja",
    x %in% c(3, 4, 5)    ~ "media",
    x %in% c(6, 7, 8, 9) ~ "alta",
    TRUE ~ NA_character_
  )
}

student_2025 <- raw2025 |>
  transmute(
    year = 2025L,
    region_code = as.character(REGION),
    school_id = as.character(CNTSCHID),
    student_id = as.character(CNTSTUID),
    gender = coalesce(
      case_when(
        ST004D01T == 1 ~ "female",
        ST004D01T == 2 ~ "male",
        TRUE ~ NA_character_
      ),
      case_when(
        MALE == 1 ~ "male",
        MALE == 0 ~ "female",  # etiqueta real del fichero: "Female / Other"
        TRUE ~ NA_character_
      )
    ),
    immig = case_when(
      IMMIG == 1 ~ "nativo",
      IMMIG == 2 ~ "segunda_gen",
      IMMIG == 3 ~ "primera_gen",
      TRUE ~ NA_character_
    ),
    parent_educ = factor(hisced_to_3cat_2025(HISCED), levels = c("baja", "media", "alta")),
    escs = ESCS,
    stu_wgt = W_FSTUWT,
    math = pv_mean(raw2025, "MATH"),
    read = pv_mean(raw2025, "READ"),
    science = pv_mean(raw2025, "SCIE")
  ) |>
  left_join(region_lookup_2025, by = "region_code")

if (!is.null(school_type_2025)) {
  student_2025 <- student_2025 |> left_join(school_type_2025, by = "school_id")
}

message(
  "2025 -- género: ", sum(!is.na(student_2025$gender)), " de ", nrow(student_2025),
  " alumnos clasificados (", scales::percent(mean(!is.na(student_2025$gender))), "). ",
  "Si ST004D01T está ausente (caso esperado para España, ver aviso arriba), esto viene ",
  "íntegramente de MALE -- comprueba que no quede en ~0% clasificado, lo que indicaría ",
  "que TAMPOCO MALE está disponible y haría falta otra alternativa."
)

# --- 5. Comprobaciones antes de unir ------------------------------------------
# Igual que 01_load_data.R: todas las filas deben mapear a una CCAA
# conocida (si esto falla, algo ha cambiado en el fichero de la OCDE y hay
# que revisar `region_lookup_2025` contra las etiquetas reales, no
# ignorarlo).
n_sin_ccaa <- sum(is.na(student_2025$ccaa))
if (n_sin_ccaa > 0) {
  message(
    n_sin_ccaa, " alumnos de 2025 sin CCAA asignada (código 72400 = ",
    "\"Rest of Country\", residual sin adjudicar -- se descartan, igual ",
    "que los códigos \"not adjudicated\" de 2012 en 01b)."
  )
}
student_2025 <- student_2025 |>
  filter(!is.na(ccaa)) |>
  select(-region_code) |>
  mutate(global_school_id = paste(ccaa, year, school_id, sep = "_"))

n_ccaa_2025 <- n_distinct(student_2025$ccaa)
message("2025 -- alumnos con CCAA asignada: ", nrow(student_2025), " | CCAA distintas: ", n_ccaa_2025)
print(student_2025 |> count(ccaa, sort = TRUE))
stopifnot(
  "Se esperaban 19 CCAA (17 + Ceuta + Melilla) en 2025 -- revisa region_lookup_2025" =
    n_ccaa_2025 == 19
)

# --- 6. Unir con el fichero extendido ya existente ---------------------------
student_esp_extended <- readRDS(path_extended)

# Si ya habías ejecutado antes una versión de este script (p.ej. antes de
# la corrección de `gender`/`MALE`), 2025 ya estará en el fichero --se
# quita esa versión anterior en vez de fallar, así puedes re-ejecutar
# 01d sin tener que repetir 01/01b/01c desde cero.
if (2025L %in% unique(student_esp_extended$year)) {
  message(
    "2025 ya estaba en ", path_extended, " -- se sustituye por esta versión ",
    "nueva (no hace falta rehacer 01_load_data.R/01b/01c)."
  )
  student_esp_extended <- student_esp_extended |> filter(year != 2025L)
}

student_esp_extended <- bind_rows(student_esp_extended, student_2025)

saveRDS(student_esp_extended, path_extended)

editions_now <- sort(unique(student_esp_extended$year))
ceuta_melilla_editions <- student_esp_extended |>
  filter(ccaa %in% c("Ceuta", "Melilla")) |>
  count(ccaa, year) |>
  count(ccaa, name = "n_editions")

message(
  "\nGuardado (sobrescrito) ", path_extended, " -- ediciones disponibles ahora: ",
  paste(editions_now, collapse = ", "), "\n",
  "Alumnos totales: ", nrow(student_esp_extended), "\n\n",
  "Ceuta/Melilla -- nº de ediciones con datos tras añadir 2025 (antes tenían 1 cada una, ",
  "solo 2022 -- comprueba si esto ya les da 'has_trend = TRUE' en 05_communicate_uncertainty.R ",
  "y actualiza el aviso correspondiente en qmd/04-resultados.qmd):"
)
print(ceuta_melilla_editions)

# -----------------------------------------------------------------------------
# 02_prepare_variables.R no necesita ningún cambio: ya usa
# data/student_esp_pooled_extended.rds automáticamente en cuanto existe, y
# ya itera sobre unique(student_prep$year) para el resto del pipeline
# (03/04/05). Sí conviene revisar a mano, tras ejecutar esto:
#   - qmd/03-metodos.qmd: documentar que 2025 viene del fichero
#     internacional de la OCDE (vía REGION), no de microdatos del INEE
#     como el resto de ediciones -- es una fuente distinta, aunque el
#     resultado (comunidad autónoma real) sea comparable.
#   - qmd/04-resultados.qmd: el aviso sobre Ceuta/Melilla sin tendencia
#     estimable deja de ser cierto tal cual estaba (pasan a tener 2
#     ediciones) -- hay que matizarlo, no borrarlo sin más (siguen siendo
#     solo 2 puntos, intervalo muy ancho).
#   - README.md: la sección "Pendiente" sigue diciendo "Solo tenemos 2022
#     cargado", ya desactualizada por 01b/01c antes incluso de este script.
# -----------------------------------------------------------------------------
