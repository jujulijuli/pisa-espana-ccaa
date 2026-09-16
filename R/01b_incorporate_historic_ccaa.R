# =============================================================================
# 01b_incorporate_historic_ccaa.R
# Añade 2009, 2012 y 2015 (y opcionalmente 2006) al proyecto usando un
# fichero consolidado que SÍ trae comunidad autónoma para esas ediciones
# concretas -- hallazgo verificado variable a variable (no una suposición)
# inspeccionando directamente `ESP_PISA00_03_06_09_12_15.sav`: `SUBNATIO`
# (2006/2009/2012) y `Region` (2015, vía la submuestra de país "QES" en vez
# de la muestra estándar "ESP" -- ver más abajo) SÍ tienen desglose real por
# CCAA, a diferencia del fichero internacional estándar (donde España
# siempre colapsa a un único código, confirmado también para 2025).
#
# El fichero fuente es ANCHO (una columna por variable-y-edición, prefijos
# P06_/P09_/P12_/P15_, ~3.500 columnas en total) -- muy distinto del "un
# fichero por año" que asume 01_load_data.R. Por eso va en un script
# aparte: reshapea cada bloque a filas y lo une al `student_esp_pooled.rds`
# que ya genera 01_load_data.R (2022), guardando el resultado en un fichero
# NUEVO (`data/student_esp_pooled_extended.rds`) para no pisar el de
# solo-2022 -- 02_prepare_variables.R detecta automáticamente cuál usar,
# igual que hace el proyecto internacional con `student_pooled_9cycles.rds`
# (ver ../pisa-maihda-trends/R/06_incorporate_pisa2025.R).
#
# LIMITACIONES verificadas contra el fichero real, no supuestas -- documentar
# si esto llega a un manuscrito:
#  - 2000/2003/2018 quedan FUERA de este script: 2000 no tiene desglose
#    usable; 2003 solo identifica 3 CCAA (Castilla y León, Cataluña, País
#    Vasco) -- demasiado parcial para comparar entre comunidades; el
#    `ESP_06_Diciembre_PISA2018.sav` que tenemos no trae NINGÚN desglose
#    (confirmado: un único código SUBNATIO para las 35.943 filas).
#  - 2006 queda fuera POR DEFECTO (cobertura parcial, ~10 de 19 categorías,
#    con un cajón grande de "resto de España, no adjudicado") -- cambia
#    `incluir_2006 <- TRUE` más abajo si lo quieres de todos modos.
#  - 2009: 15 CCAA + una categoría COMBINADA "Ceuta y Melilla" (ese año no
#    se pueden separar) -- DISTINTO de 2022, donde sí están separadas.
#    Cuidado al comparar Ceuta/Melilla específicamente entre 2009 y 2022.
#  - 2012: ~14 CCAA, SIN Ceuta ni Melilla en absoluto ese año (no
#    adjudicadas, no aparece ningún código para ellas).
#  - `public_private` (tipo de colegio) NO está disponible para estas
#    ediciones históricas (el fichero fuente es solo de alumno, no incluye
#    el fichero de centros) -- queda NA. 02_prepare_variables.R /
#    03_model_maihda_ccaa_inla.R ya tratan esa columna como opcional (vía
#    `if ("public_private" %in% names(...))`), así que no hace falta tocar
#    nada ahí -- solo saber que esos años no aportarán la brecha
#    público-privado.
#  - HISCED en estos años usa la escala INTERNACIONAL 0-6 (ISCED 0 a
#    5A/6), DISTINTA de la escala 1-10 verificada para el fichero de 2022
#    -- se mapea aquí con su propio corte a baja/media/alta
#    (`hisced_to_3cat_intl()`); NO reutiliza el corte de 01_load_data.R
#    porque la escala de origen es distinta (misma lección que ya nos
#    enseñó HISCED en 2022: revalidar, no asumir).
#  - Solo hay 1 valor plausible por dominio en estos años (PV1MATH/READ/SCIE),
#    no los 10 completos que sí trae 2022 -- mismo patrón que
#    `learningtower` en el proyecto internacional, así que no rompe nada
#    aguas abajo (03_model_maihda_ccaa_inla.R ya promedia si hay 10, y usa
#    el único valor si solo hay 1... revisar `pv_mean()` si hiciera falta,
#    pero aquí ya entregamos math/read/science listos, no columnas PV1..10).
# =============================================================================

library(haven)
library(dplyr)

# --- 0. Ruta al fichero fuente -----------------------------------------------
# AJUSTAR a donde lo tengas. No hace falta copiarlo a data_raw/ -- puedes
# apuntar directamente a la ruta donde ya esté (puede ser pesado, ~530 MB).
path_historic <- "/Volumes/discojuli/PISA/pisa-espana-ccaa/data/ESP_PISA00_03_06_09_12_15.sav"
stopifnot(
  "No encuentro el fichero histórico -- ajusta `path_historic` arriba" =
    file.exists(path_historic)
)

incluir_2006 <- FALSE  # cambia a TRUE si quieres 2006 pese a su cobertura parcial

message("Leyendo fichero histórico (puede tardar un poco, son ~3.500 columnas)...")
raw <- haven::read_sav(path_historic)

# CORRECCIÓN 2026-09-16: el propio fichero NO usa mayúsculas/minúsculas de
# forma consistente entre ediciones para el mismo concepto -- confirmado
# tras un error real: `P12_hisced` está en minúsculas (el resto de
# columnas de P12 van en mayúsculas) y `P06_STIDSTD` está TODO en
# mayúsculas (a diferencia de `P09_StIDStd`/`P12_StIDStd`, en
# mayúsculas/minúsculas mixtas). Para no depender de adivinar la
# capitalización exacta de cada columna en cada edición, `col()` resuelve
# el nombre real por comparación insensible a mayúsculas/minúsculas contra
# las ~3.500 columnas del fichero, y avisa con un error claro (en vez de
# un NULL silencioso) si de verdad no encuentra ninguna variante.
col_lookup_upper <- setNames(names(raw), toupper(names(raw)))
resolve_col <- function(prefixed_name) {
  actual <- col_lookup_upper[[toupper(prefixed_name)]]
  if (is.null(actual)) {
    stop("No encuentro la columna '", prefixed_name, "' (ni ninguna variante de mayúsculas) en el fichero histórico")
  }
  actual
}

# --- 1. Traducción inglés -> nombre de CCAA ----------------------------------
# Mismos nombres/acentos que `region_lookup` en 01_load_data.R, para que la
# columna `ccaa` case exactamente al unir con 2022 (mismos niveles de factor).
english_to_ccaa <- c(
  "Andalusia"            = "Andalucía",
  "Aragon"                = "Aragón",
  "Asturias"              = "Asturias",
  "Balearic Islands"      = "Illes Balears",
  "Canary Islands"        = "Canarias",
  "Cantabria"             = "Cantabria",
  "Castile and Leon"      = "Castilla y León",
  "Castile-La Mancha"     = "Castilla-La Mancha",
  "Catalonia"             = "Cataluña",
  "Extremadura"           = "Extremadura",
  "Galicia"               = "Galicia",
  "La Rioja"              = "La Rioja",
  "Madrid"                = "Madrid",
  "Murcia"                = "Murcia",
  "Navarre"               = "Navarra",
  "Basque Country"        = "País Vasco",
  "Comunidad Valenciana"  = "Comunidad Valenciana",
  "Ceuta and Melilla"     = "Ceuta y Melilla"  # combinada -- solo aparece así en 2009
)

# HISCED en escala internacional 0-6 (ISCED 0 a 5A/6) -- ver nota arriba.
hisced_to_3cat_intl <- function(x) {
  case_when(
    x %in% c(0, 1, 2) ~ "baja",   # sin cualificación, ISCED1, ISCED2
    x %in% c(3, 4)    ~ "media",  # ISCED 3B/C, ISCED 3A/4
    x %in% c(5, 6)    ~ "alta",   # ISCED 5B, ISCED 5A/6
    TRUE ~ NA_character_
  )
}

# --- 2. Decodificar la región A PARTIR DE LAS ETIQUETAS DE VALOR REALES -----
# del propio fichero (no de una tabla de códigos escrita a mano) -- así se
# adapta automáticamente a que 2006 usa 4-5 dígitos, 2009/2015 usan 5, y
# 2012 usa 7, sin tener que reconciliar los códigos numéricos a mano. Se
# comprobó que los tres formatos de etiqueta que usa la OCDE en estos años
# son "Spain: X", "Spain (X)" y "Spain - X".
decode_region <- function(region_vec) {
  labels_map <- attr(region_vec, "labels")  # named num: "Spain: Andalusia" = 72401
  if (is.null(labels_map)) {
    warning("La variable de región no trae etiquetas de valor -- revisa a mano.")
    return(rep(NA_character_, length(region_vec)))
  }
  # CORRECCIÓN 2026-09-16: region_vec viene almacenado como TEXTO en este
  # fichero para P06/P09/P12_SUBNATIO (confirmado: as.numeric() sobre esas
  # columnas da "Can't convert vec_data(x) <character> to <double>"), pero
  # como numérico para P15_Region -- comparamos siempre como texto (tras
  # quitar la clase haven_labelled) para que funcione en los dos casos.
  region_chr <- trimws(as.character(unclass(region_vec)))
  labels_chr <- trimws(as.character(unclass(labels_map)))
  label_text <- names(labels_map)[match(region_chr, labels_chr)]

  is_spain_detail <- !is.na(label_text) &
    grepl("^Spain", label_text) &
    !grepl("not adjudicated|rest of", label_text, ignore.case = TRUE) &
    label_text != "Spain"

  # CORRECCIÓN 2026-09-16 (confirmado contra el fichero real con Julián):
  # 2012 usa "Spain - Andalusia" -- espacio ANTES *y* después del guion, no
  # solo después. El patrón anterior (`^Spain[:\\-]?\\s*\\(?`) solo permitía
  # espacio tras el separador, así que para 2012 solo consumía "Spain " y
  # dejaba colgando el guion ("- Andalusia"), que no coincidía con ninguna
  # CCAA de `english_to_ccaa` -- todas las filas de 2012 quedaban en NA en
  # silencio. Se añade `\\s*` también ANTES de la clase del separador.
  region_name_en <- trimws(sub("\\)\\s*$", "", sub("^Spain\\s*[:\\-]?\\s*\\(?", "", label_text)))
  ccaa <- unname(english_to_ccaa[region_name_en])
  ccaa[!is_spain_detail] <- NA_character_
  ccaa
}

# --- 3. Extracción por edición -----------------------------------------------
edition_specs <- list(
  list(prefix = "P06", year = 2006L, gender = "ST04Q01",   schoolid = "SCHOOLID_PISA", studid = "STIDSTD",  region = "SUBNATIO", cnt = NULL),
  list(prefix = "P09", year = 2009L, gender = "ST04Q01",   schoolid = "SCHOOLID_PISA", studid = "StIDStd",  region = "SUBNATIO", cnt = NULL),
  list(prefix = "P12", year = 2012L, gender = "ST04Q01",   schoolid = "SCHOOLID_PISA", studid = "StIDStd",  region = "SUBNATIO", cnt = NULL),
  list(prefix = "P15", year = 2015L, gender = "ST004D01T", schoolid = "CNTSCHID",      studid = "CNTSTUID", region = "Region",   cnt = "CNT")
)
if (!incluir_2006) {
  edition_specs <- Filter(function(s) s$year != 2006L, edition_specs)
}

extract_edition <- function(spec) {
  col <- function(name) resolve_col(paste0(spec$prefix, "_", name))
  d <- tibble::tibble(.rows = nrow(raw))

  # Para 2015, la CCAA solo está poblada en la submuestra de país "QES"
  # (la "ESP" estándar trae el código genérico sin desglose) -- confirmado
  # empíricamente: 6.736 filas ESP (todas con región genérica) frente a
  # 32.330 filas QES (todas con una de las 17 CCAA).
  row_ok <- if (!is.null(spec$cnt)) {
    raw[[col(spec$cnt)]] == "QES" & !is.na(raw[[col(spec$cnt)]])
  } else {
    !is.na(raw[[col(spec$studid)]])
  }

  out <- tibble::tibble(
    year        = spec$year,
    school_id   = as.character(raw[[col(spec$schoolid)]]),
    student_id  = as.character(raw[[col(spec$studid)]]),
    gender      = case_when(
      raw[[col(spec$gender)]] == 1 ~ "female",
      raw[[col(spec$gender)]] == 2 ~ "male",
      TRUE ~ NA_character_
    ),
    immig       = case_when(
      raw[[col("IMMIG")]] == 1 ~ "nativo",
      raw[[col("IMMIG")]] == 2 ~ "segunda_gen",
      raw[[col("IMMIG")]] == 3 ~ "primera_gen",
      TRUE ~ NA_character_
    ),
    parent_educ = factor(hisced_to_3cat_intl(as.numeric(raw[[col("HISCED")]])), levels = c("baja", "media", "alta")),
    escs        = as.numeric(raw[[col("ESCS")]]),
    stu_wgt     = as.numeric(raw[[col("W_FSTUWT")]]),
    math        = as.numeric(raw[[col("PV1MATH")]]),
    read        = as.numeric(raw[[col("PV1READ")]]),
    science     = as.numeric(raw[[col("PV1SCIE")]]),
    ccaa        = decode_region(raw[[col(spec$region)]])
  )
  out[row_ok & !is.na(out$ccaa), ]
}

historic <- purrr::map_dfr(edition_specs, extract_edition) |>
  mutate(global_school_id = paste(ccaa, year, school_id, sep = "_"))

message(
  "Histórico -- alumnos por edición y nº de CCAA distintas:\n",
  paste(capture.output(print(
    historic |> group_by(year) |> summarise(alumnos = n(), ccaa_distintas = n_distinct(ccaa), .groups = "drop")
  )), collapse = "\n")
)

# --- 4. Unir con el `student_esp_pooled.rds` ya generado por 01_load_data.R -
stopifnot(
  "Ejecuta primero 01_load_data.R (necesito data/student_esp_pooled.rds con 2022)" =
    file.exists("data/student_esp_pooled.rds")
)
student_esp_2022 <- readRDS("data/student_esp_pooled.rds")

# `public_private` y `region_code` no existen en `historic` (ver
# limitaciones arriba) -- bind_rows() los rellena con NA automáticamente,
# no hace falta añadirlos a mano.
student_esp_extended <- bind_rows(student_esp_2022, historic)

saveRDS(student_esp_extended, "data/student_esp_pooled_extended.rds")

message(
  "Guardado data/student_esp_pooled_extended.rds -- ediciones disponibles: ",
  paste(sort(unique(student_esp_extended$year)), collapse = ", "), "\n",
  "Alumnos totales: ", nrow(student_esp_extended)
)

# -----------------------------------------------------------------------------
# 02_prepare_variables.R ya detecta automáticamente
# `data/student_esp_pooled_extended.rds` en cuanto exista (mismo patrón que
# `student_pooled_9cycles.rds` en el proyecto internacional) y lo usa en vez
# del de solo-2022, sin tener que tocar nada más a mano.
# -----------------------------------------------------------------------------
