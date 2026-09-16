# =============================================================================
# 01c_incorporate_public_private_historic.R
# Añade `public_private` a las ediciones históricas (2009/2012/2015) que
# 01b_incorporate_historic_ccaa.R dejó en NA -- el fichero consolidado que usa
# 01b es solo de alumnos, sin nada de centro. Verificado edición a edición
# contra los ficheros reales de la OCDE/INEE que Julián localizó, sin asumir
# nada del nombre de los ficheros (ver conversación 2026-09-16 -- un zip que
# parecía ser 2015 resultó ser en realidad 2022):
#
#  - 2012: `INT_SCQ12_DEC03.zip` (fichero internacional de centro, formato
#    texto de ancho fijo) + `SPSS syntax to read in school questionnaire
#    data file.txt` (el layout de columnas, confirmado que es el de 2012
#    pese a no llevar el año en el nombre -- lo confirma la propia página
#    de descargas de la OCDE para el dataset 2012). Variable `SC01Q01`
#    ("Public or private"), columna 32, 1=Public/2=Private. 902 colegios
#    españoles -- prácticamente el censo completo de 2012.
#  - 2015: `PUF_SPSS_COMBINED_CMB_SCH_QQQ.zip` (SPSS directo, `CY6_MS_CMB_
#    SCH_QQQ.sav` = Ciclo 6 = 2015). Variable `SC013Q01TA` (mismo nombre y
#    codificación que ya usamos para 2022), 1=Public/2=Private. Incluye
#    tanto la muestra estándar (CNT=="ESP", 201 colegios) como la ampliada
#    con detalle de CCAA (CNT=="QES", 976 colegios) -- las dos hacen falta,
#    porque 01b usa las filas QES de alumnos para la serie de CCAA.
#  - 2009: NO se encontró el layout de columnas correcto (el fichero
#    internacional de centro de 2009 también es texto de ancho fijo, pero
#    con una sintaxis distinta a la de 2012 que no se ha localizado). En su
#    lugar se usa el panel de seguimiento que Julián encontró ("Centros
#    SPSS_Base de datos PfS 2013-2014"): 225 de los ~900 colegios de PISA
#    2009 (una submuestra, no el censo), re-evaluados en 2013 -- variable
#    `Titularidad_3` (Público/Privado-Concertado/Privado), enlazada por
#    `SCHOOLIDINT_09`. Se asume que la titularidad no cambió entre 2009 y
#    2013 (supuesto razonable, pero es un supuesto -- documentarlo si esto
#    llega a un manuscrito). El resto de colegios de 2009 (~675) quedan NA.
#
# Requiere haber ejecutado antes 01_load_data.R y 01b_incorporate_historic_
# ccaa.R (relee y sobrescribe data/student_esp_pooled_extended.rds, añadiendo
# `public_private` donde antes había NA para 2009/2012/2015; no toca 2022,
# que ya lo trae de 01_load_data.R).
# =============================================================================

library(haven)
library(dplyr)
library(readr)

# --- Rutas -- AJUSTAR si tus ficheros están en otro sitio --------------------
path_2012_zip   <- "/Volumes/discojuli/PISA/dataintwww/INT_SCQ12_DEC03.zip"
path_2012_syntax <- "/Volumes/discojuli/PISA/dataintwww/SPSS syntax to read in school questionnaire data file.txt"
path_2015_zip   <- "/Volumes/discojuli/PISA/dataintwww/PUF_SPSS_COMBINED_CMB_SCH_QQQ.zip"
path_2009_panel <- "/Volumes/discojuli/PISA/dataesp/Centros SPSS_Base de datos PfS 2013-2014_20150430.sav"
path_extended   <- "data/student_esp_pooled_extended.rds"

stopifnot(
  "Ejecuta antes 01_load_data.R y 01b_incorporate_historic_ccaa.R (necesito data/student_esp_pooled_extended.rds)" =
    file.exists(path_extended)
)

# Mismo harmonizador que 01_load_data.R (redefinido aquí para que este script
# sea autosuficiente y no dependa de que 01_load_data.R siga en el entorno).
harmonize_public_private <- function(x) {
  x_chr <- tolower(trimws(as.character(x)))
  case_when(
    x_chr %in% c("1", "public", "publico", "público")  ~ "publico",
    x_chr %in% c("2", "private", "privado")             ~ "privado",
    TRUE ~ NA_character_
  )
}

# --- 2012: texto de ancho fijo -----------------------------------------------
message("2012: leyendo fichero internacional de centro (texto de ancho fijo)...")
tmp_2012_dir <- file.path(tempdir(), "sch2012")
dir.create(tmp_2012_dir, showWarnings = FALSE)
utils::unzip(path_2012_zip, exdir = tmp_2012_dir)
txt_2012 <- list.files(tmp_2012_dir, pattern = "\\.txt$", full.names = TRUE, ignore.case = TRUE)[1]
stopifnot("No encuentro el .txt dentro de INT_SCQ12_DEC03.zip" = !is.na(txt_2012))

# Posiciones confirmadas contra la sintaxis SPSS real (1-indexadas, ambas
# inclusive, igual que las especifica la propia sintaxis DATA LIST):
# CNT 1-3, SCHOOLID 25-31, SC01Q01 32-32 ("Public or private": 1=Public,
# 2=Private, 7/8/9=N.A./Invalid/Missing).
sch_2012 <- read_fwf(
  txt_2012,
  fwf_positions(start = c(1, 25, 32), end = c(3, 31, 32), col_names = c("cnt", "school_id", "sc01q01")),
  col_types = cols(.default = col_character())
) |>
  filter(cnt == "ESP") |>
  transmute(
    year = 2012L,
    school_id = trimws(school_id),
    public_private = harmonize_public_private(sc01q01) |> factor(levels = c("publico", "privado"))
  )
message("2012: ", nrow(sch_2012), " colegios españoles (", sum(!is.na(sch_2012$public_private)), " con titularidad clasificada)")

# --- 2015: SPSS directo, incluye muestra QES ---------------------------------
message("2015: leyendo fichero internacional de centro (SPSS)...")
tmp_2015_dir <- file.path(tempdir(), "sch2015")
dir.create(tmp_2015_dir, showWarnings = FALSE)
utils::unzip(path_2015_zip, exdir = tmp_2015_dir)
sav_2015 <- list.files(tmp_2015_dir, pattern = "\\.sav$", full.names = TRUE, ignore.case = TRUE)[1]
stopifnot("No encuentro el .sav dentro de PUF_SPSS_COMBINED_CMB_SCH_QQQ.zip" = !is.na(sav_2015))

raw_sch_2015 <- haven::read_sav(sav_2015, col_select = any_of(c("CNT", "CNTSCHID", "SC013Q01TA")))
sch_2015 <- raw_sch_2015 |>
  filter(CNT %in% c("ESP", "QES")) |>  # las dos muestras -- 01b usa las filas QES para la serie de CCAA
  transmute(
    year = 2015L,
    school_id = as.character(as.integer(CNTSCHID)),
    public_private = harmonize_public_private(SC013Q01TA) |> factor(levels = c("publico", "privado"))
  )
message("2015: ", nrow(sch_2015), " colegios españoles (ESP+QES) (", sum(!is.na(sch_2015$public_private)), " con titularidad clasificada)")

# --- 2009: panel PfS 2013 (cobertura parcial, ~225 de ~900 colegios) --------
sch_2009 <- tibble()
if (file.exists(path_2009_panel)) {
  message("2009: leyendo panel de seguimiento PfS 2013 (cobertura parcial)...")
  raw_panel <- haven::read_sav(path_2009_panel, col_select = any_of(c("SCHOOLIDINT_09", "Titularidad_3")))
  sch_2009 <- raw_panel |>
    filter(!is.na(SCHOOLIDINT_09)) |>
    transmute(
      year = 2009L,
      school_id = trimws(as.character(SCHOOLIDINT_09)),
      public_private = case_when(
        Titularidad_3 == 1 ~ "publico",
        Titularidad_3 %in% c(2, 3) ~ "privado",  # concertado + privado puro, mismo criterio 2-categorías que el resto de ediciones
        TRUE ~ NA_character_
      ) |> factor(levels = c("publico", "privado"))
    )
  message("2009: ", nrow(sch_2009), " colegios con titularidad conocida de ~900 en la edición completa (submuestra del panel, no censo)")
} else {
  message("2009: no encuentro el fichero del panel PfS -- public_private quedará NA para toda la edición 2009.")
}

# --- Unir todo al fichero extendido -------------------------------------------
lookup_pubpriv <- bind_rows(sch_2012, sch_2015, sch_2009)

student_esp_extended <- readRDS(path_extended)

# Si `public_private` ya existe (de 2022 y como columna NA para el resto,
# generada por 01b), la sustituimos solo donde el lookup aporta un valor
# nuevo -- 2022 no se toca porque no está en `lookup_pubpriv`.
student_esp_extended <- student_esp_extended |>
  select(-any_of("public_private_old")) |>
  rename(public_private_old = any_of("public_private")) |>
  left_join(lookup_pubpriv, by = c("year", "school_id")) |>
  mutate(
    public_private = factor(
      coalesce(as.character(public_private), as.character(public_private_old)),
      levels = c("publico", "privado")
    )
  ) |>
  select(-public_private_old)

message("\nCobertura de public_private tras la incorporación, por edición:")
print(
  student_esp_extended |>
    group_by(year) |>
    summarise(
      n = n(),
      pct_clasificado = scales::percent(mean(!is.na(public_private)), accuracy = 0.1),
      .groups = "drop"
    ),
  n = Inf
)

saveRDS(student_esp_extended, path_extended)
message("\nGuardado (sobrescrito) ", path_extended, " con public_private ampliado.")
