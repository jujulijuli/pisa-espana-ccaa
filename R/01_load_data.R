# =============================================================================
# 01_load_data.R
# CONFIRMADO 2026-09 contra el fichero real que nos pasaste
# (PISA2022_Estudiantes_Esp.sav / PISA2022_CentrosEducativos_Esp.sav,
# descomprimidos del .rar del INEE): 30.800 alumnos, 966 colegios, y --lo
# importante-- **19 valores distintos de `REGION`**, correspondientes a las
# 17 comunidades autónomas + Ceuta + Melilla. Esto confirma que el fichero
# NACIONAL del INEE sí trae comunidad autónoma, a diferencia del fichero
# internacional de la OCDE (donde `SUBNATIO` colapsa España a un único
# código -- de hecho `SUBNATIO` también está en este fichero y SIGUE sin
# desglosar España; es `REGION`, no `SUBNATIO`, la variable que hay que
# usar aquí).
#
# Columnas confirmadas (mismo esquema que PISA 2025 -- ver
# ../pisa-maihda-trends/R/06_incorporate_pisa2025.R):
#   CNT, CNTSCHID, CNTSTUID, REGION, ST004D01T, IMMIG, LANGN, HISCED, ESCS,
#   W_FSTUWT, PV1..10 MATH/READ/SCIE (los 10 completos, no solo 1 -- mejor
#   que learningtower para el proyecto internacional, que solo da un valor
#   "simulado" por dominio).
#
# BONUS: IMMIG (origen inmigrante) SÍ está aquí, a diferencia de
# learningtower para 2000-2022 en el proyecto internacional. Puedes usarla
# como dimensión interseccional en MAIHDA sin trabajo adicional.
# =============================================================================

library(haven)
library(dplyr)
library(purrr)

if (!dir.exists("data")) dir.create("data", recursive = TRUE)

# --- Tabla de equivalencia REGION -> nombre de CCAA -------------------------
# Verificada contra las etiquetas de valor reales del fichero.
region_lookup <- tibble::tribble(
  ~region_code, ~ccaa,
  72401, "Andalucía",
  72402, "Aragón",
  72403, "Asturias",
  72404, "Illes Balears",
  72405, "Canarias",
  72406, "Cantabria",
  72407, "Castilla y León",
  72408, "Castilla-La Mancha",
  72409, "Cataluña",
  72410, "Extremadura",
  72411, "Galicia",
  72412, "La Rioja",
  72413, "Madrid",
  72414, "Murcia",
  72415, "Navarra",
  72416, "País Vasco",
  72417, "Comunidad Valenciana",
  72418, "Ceuta",
  72419, "Melilla"
)

# --- Ficheros por edición ---------------------------------------------------
# Añade aquí más ediciones a medida que las consigas (mismo patrón: fichero
# de alumno del INEE, formato .sav). El fichero de 2022 ya está confirmado;
# para 2015/2018 lo más probable es que el esquema de columnas sea el mismo
# (es estable en toda la serie reciente de la OCDE) pero verifícalo con el
# mismo bloque de diagnóstico de abajo antes de asumirlo.
files_by_year <- c(
  "2022" = "data/PISA2022_Estudiantes_Esp.sav"
  # "2018" = "data_raw/PISA2018_Estudiantes_Esp.sav",
  # "2015" = "data_raw/PISA2015_Estudiantes_Esp.sav",
)

# Fichero de CENTROS (colegios) -- necesario para público/privado, que es
# una variable de nivel colegio y por tanto no está en el fichero de
# alumno. Mismo patrón año->ruta que arriba.
files_by_year_sch <- c(
  "2022" = "data/PISA2022_CentrosEducativos_Esp.sav"
)

missing_files <- files_by_year[!file.exists(files_by_year)]
if (length(missing_files) > 0) {
  stop(
    "No encuentro estos ficheros (colócalos en data_raw/, descomprimidos ",
    "del .rar del INEE):\n", paste(" -", missing_files, collapse = "\n")
  )
}

pv_mean <- function(df, domain_suffix) {
  cols <- grep(paste0("^PV\\d+", domain_suffix, "$"), names(df), value = TRUE)
  if (length(cols) == 0) stop("No encuentro columnas PV para el dominio ", domain_suffix)
  rowMeans(df[cols], na.rm = TRUE)
}

# OJO -- verificado empíricamente con el fichero real de 2022: la escala de
# HISCED aquí NO es la misma que documenté para el codebook de PISA 2025
# (esa iba de 1 a 9; en el fichero de 2022 del INEE va de 1 a 10, con un
# nivel adicional "Less than ISCED Level 1" separado de "ISCED level 1").
# Con el corte anterior (1-9) el código 10 = "ISCED level 8" (doctorado)
# habría caído en NA para 3.381 alumnos (~11% de la muestra) -- lo detecté
# al validar las cuentas antes de dar el script por bueno. Etiquetas reales
# confirmadas contra el fichero de 2022:
#   1 Less than ISCED1 | 2 ISCED1 | 3 ISCED2 | 4 ISCED3.3 | 5 ISCED3.4 |
#   6 ISCED4 | 7 ISCED5 | 8 ISCED6 | 9 ISCED7 | 10 ISCED8
# Si añades otra edición, vuelve a comprobar esto (no asumas que la escala
# es la misma -- claramente cambia entre ediciones).
hisced_to_3cat <- function(x) {
  case_when(
    x %in% c(1, 2, 3)        ~ "baja",   # menos que ISCED1, ISCED1, ISCED2
    x %in% c(4, 5, 6)        ~ "media",  # ISCED 3.3/3.4/4 (secundaria sup./postsecundaria)
    x %in% c(7, 8, 9, 10)    ~ "alta",   # ISCED 5-8 (terciaria corta a doctorado)
    TRUE ~ NA_character_
  )
}

load_one_year <- function(path, yr) {
  raw <- haven::read_sav(path)

  # --- Diagnóstico: avisa si faltase alguna columna esperada, en vez de
  # fallar en silencio con NAs ------------------------------------------------
  expected <- c("CNT", "CNTSCHID", "CNTSTUID", "REGION", "ST004D01T",
                "IMMIG", "HISCED", "ESCS", "W_FSTUWT")
  missing_cols <- setdiff(expected, names(raw))
  if (length(missing_cols) > 0) {
    warning(yr, ": faltan columnas esperadas: ", paste(missing_cols, collapse = ", "))
  }

  raw |>
    transmute(
      year = as.integer(yr),
      region_code = as.integer(REGION),
      school_id = as.character(CNTSCHID),
      student_id = as.character(CNTSTUID),
      gender = case_when(
        ST004D01T == 1 ~ "female",
        ST004D01T == 2 ~ "male",
        TRUE ~ NA_character_
      ),
      immig = case_when(
        IMMIG == 1 ~ "nativo",
        IMMIG == 2 ~ "segunda_gen",
        IMMIG == 3 ~ "primera_gen",
        TRUE ~ NA_character_
      ),
      parent_educ = factor(hisced_to_3cat(HISCED), levels = c("baja", "media", "alta")),
      escs = ESCS,
      stu_wgt = W_FSTUWT,
      math = pv_mean(raw, "MATH"),
      read = pv_mean(raw, "READ"),
      science = pv_mean(raw, "SCIE")
    ) |>
    left_join(region_lookup, by = "region_code")
}

student_esp <- map2_dfr(files_by_year, names(files_by_year), load_one_year) |>
  mutate(global_school_id = paste(ccaa, year, school_id, sep = "_"))

# --- Público/privado, desde el fichero de CENTROS ---------------------------
# Verificado directamente contra PISA2022_CentrosEducativos_Esp.sav: trae
# `PRIVATESCH` (derivada del marco muestral, sin missing en 2022: 603
# públicos / 363 privados de 966 colegios) codificada como texto
# "public"/"private" -- OJO, distinto de como la documenta el codebook de
# PISA 2025 (ahí es numérica, 1=público/2=privado). También está
# `SC013Q01TA` (autodeclarada, numérica 1/2, con algo de missing: 54 de
# 966). La función de abajo admite ambas formas para no repetir la sorpresa
# de HISCED si una edición futura cambia de convención otra vez.
harmonize_public_private <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  case_when(
    x_chr %in% c("1", "public", "publico", "público")  ~ "publico",
    x_chr %in% c("2", "private", "privado")             ~ "privado",
    TRUE ~ NA_character_
  )
}

load_school_type_one_year <- function(path, yr) {
  raw_sch <- haven::read_sav(path)
  stopifnot("CNTSCHID" %in% names(raw_sch))
  raw_sch |>
    transmute(
      year = as.integer(yr),
      school_id = as.character(CNTSCHID),
      public_private = coalesce(
        if ("PRIVATESCH" %in% names(raw_sch)) harmonize_public_private(PRIVATESCH) else NA_character_,
        if ("SC013Q01TA" %in% names(raw_sch)) harmonize_public_private(SC013Q01TA) else NA_character_
      ) |> factor(levels = c("publico", "privado"))
    )
}

missing_sch_files <- files_by_year_sch[!file.exists(files_by_year_sch)]
if (length(missing_sch_files) > 0) {
  message(
    "No encuentro estos ficheros de centro (público/privado quedará NA para ",
    "esos años):\n", paste(" -", missing_sch_files, collapse = "\n")
  )
}
available_sch <- files_by_year_sch[file.exists(files_by_year_sch)]

if (length(available_sch) > 0) {
  school_type_esp <- map2_dfr(available_sch, names(available_sch), load_school_type_one_year)
  message(
    "Colegios con público/privado clasificado: ",
    sum(!is.na(school_type_esp$public_private)), " de ", nrow(school_type_esp),
    " (", scales::percent(mean(!is.na(school_type_esp$public_private))), ")"
  )
  student_esp <- student_esp |> left_join(school_type_esp, by = c("year", "school_id"))
} else {
  message("Sin ningún fichero de centro disponible -- public_private no estará en los datos.")
}

# --- Comprobaciones antes de guardar -----------------------------------------
stopifnot(all(!is.na(student_esp$ccaa)))  # todas las filas deben mapear a una CCAA conocida
message("Alumnos: ", nrow(student_esp), " | colegios: ", n_distinct(student_esp$global_school_id))
message("Alumnos por CCAA:")
print(student_esp |> count(ccaa, sort = TRUE))

saveRDS(student_esp, "data/student_esp_pooled.rds")
