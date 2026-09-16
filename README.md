# PISA España: comunidades autónomas, jerarquía real y tendencia

Proyecto Quarto/R hermano de [`pisa-maihda-trends`](../pisa-maihda-trends),
centrado en España usando los microdatos del INEE (Instituto Nacional de
Evaluación Educativa), que sí incluyen la comunidad autónoma que el fichero
internacional de la OCDE no trae (confirmado en el proyecto anterior:
`SUBNATIO` no desglosa España). Aquí la jerarquía real por fin es **alumno <
colegio < comunidad autónoma**, y se puede hacer MAIHDA e INLA con ese nivel
de verdad, no como aproximación.

## Estado: confirmado con datos reales de 2022

Nos pasaste `PISA2022_Estudiantes_Esp.sav` + `PISA2022_CentrosEducativos_Esp.sav`
(descomprimidos de un `.rar` del INEE). Verificado directamente contra el
fichero, no por analogía:

- **30.800 alumnos, 966 colegios, 19 valores de `REGION`** -- las 17 CCAA +
  Ceuta + Melilla. Importante: la variable útil es `REGION`, NO `SUBNATIO`
  (`SUBNATIO` está también en este fichero nacional y sigue sin desglosar
  España, igual que en el internacional -- es `REGION` la que trae el
  desglose real).
- Mismo esquema de columnas que el codebook de PISA 2025 que ya conocíamos:
  `CNT`, `CNTSCHID`, `CNTSTUID`, `ST004D01T`, `ESCS`, `W_FSTUWT`,
  `PV1..10MATH/READ/SCIE` (los 10 completos, no solo 1 como en
  `learningtower`), `IMMIG` (sí disponible, a diferencia del proyecto
  internacional para 2000-2022), `HISCED`.
- **Aviso real que detecté al validar, no algo hipotético**: `HISCED` en
  este fichero va de 1 a 10 (con "Less than ISCED1" e "ISCED1" como
  categorías separadas), NO de 1 a 9 como en el codebook de PISA 2025 que
  usé de referencia para el proyecto internacional. Con el corte que copié
  al principio, el código 10 (doctorado) se habría quedado sin clasificar
  para ~3.400 alumnos (11% de la muestra). Ya está corregido en
  `R/01_load_data.R`, con las etiquetas de valor reales documentadas ahí
  mismo -- pero es un recordatorio de que la escala de HISCED cambia entre
  ediciones y hay que revalidarla si añades más años.
- Tamaños de muestra por CCAA: la mayoría entre 1.400 y 3.250 alumnos;
  Ceuta (345) y Melilla (259) bastante más pequeñas -- normal dado su
  tamaño poblacional, pero con intervalos de credibilidad más anchos para
  esas dos en cualquier resultado.

## Solución de problemas: INLA en macOS Apple Silicon

Si al ejecutar `R/03_model_maihda_ccaa_inla.R` o `R/04_model_trend_ccaa_inla.R`
ves un `Segmentation fault` del binario `inla.run`, es el mismo crash nativo
(no de R) que se documenta con más detalle en
`../pisa-maihda-trends/README.md`. Ambos scripts ya incluyen
`inla.setOption(num.threads = "1:1")`, el primer arreglo habitual; si
persiste, sigue los mismos pasos de ese README (actualizar a la versión
"testing" de INLA, reinstalar desde terminal, o volver a lanzar con
`verbose = TRUE` para diagnosticar). Si `02_prepare_variables.R` avisa de
que una edición se ha saltado por datos insuficientes, revisa la tabla de
diagnóstico de NA que imprime ese mismo script (aquí incluye también
`immig`, otra vía por la que `strata` puede quedar sin datos para un año
concreto).

Si en cambio ves `gsl: cholesky.c:645: ERROR: matrix is not positive
definite` (un error limpio de R, no un crash silencioso) o el ajuste tarda
horas sin terminar, es el mismo problema numérico del efecto trivariante
`iid3d` documentado en la sección **"INLA: 'matrix is not positive
definite' / ajuste extremadamente lento"** de `../pisa-maihda-trends/README.md`
-- ya corregido aquí también (`score` se estandariza antes de ajustar, y
`control.inla = list(int.strategy = "eb", cmin = 0)` en ambas llamadas a
`inla()` de `R/03_model_maihda_ccaa_inla.R`).

Si ves `Error: vector memory limit of 32.0 Gb reached` al ejecutar
`R/05_communicate_uncertainty.R` (no al ajustar el modelo), es la misma
causa documentada en la sección **"Error: vector memory limit..."** de
`../pisa-maihda-trends/README.md` -- dos causas, ambas ya corregidas aquí
también: `config = TRUE` innecesario en `control.compute` (corregido a
`FALSE`), y `control.predictor = list(compute = TRUE)` guardando la
marginal del predictor lineal por fila sin que nadie la use después
(corregido a `compute = FALSE`) -- esta segunda es probablemente la más
grande de las dos. **Importante**: hace falta volver a ejecutar
`R/03_model_maihda_ccaa_inla.R` (y `R/04_model_trend_ccaa_inla.R` si ya lo
habías corrido) para regenerar los ficheros `.rds` más pequeños antes de
relanzar `R/05_communicate_uncertainty.R`. Puedes comprobar el tamaño de lo
que tienes guardado con
`file.info(list.files("data", pattern = "\\.rds$", full.names = TRUE))[, "size"] / 1e9`.

`R/01_load_data.R` ya está terminado (no es plantilla) para 2022. Para
añadir más ediciones (2015, 2018, quizá 2025 si el INEE ya lo ha publicado),
descarga los ficheros de alumno equivalentes, colócalos en `data_raw/`, y
añade la ruta a `files_by_year` -- pero **revalida las columnas categóricas
(sobre todo HISCED)** con el mismo tipo de comprobación que hice aquí antes
de asumir que el esquema es idéntico.

## Estructura

```
_quarto.yml
R/
  00_packages.R          # dependencias (compartidas con pisa-maihda-trends)
  01_load_data.R          # carga PISA 2022 España -- confirmado con datos reales
  02_prepare_variables.R  # estratos MAIHDA con CCAA real
  03_model_maihda_ccaa_inla.R  # VPC/PCV con CCAA como nivel jerárquico real
  04_model_trend_ccaa_inla.R   # tendencia por CCAA (necesita >1 edición para tener sentido)
  05_communicate_uncertainty.R
qmd/
references.bib
data/
data_raw/               # pon aquí los .sav descomprimidos (no versionar)
```

## `immig` (origen inmigrante) ya incluida en los estratos

Confirmaste que es importante incluir `IMMIG` en toda la serie, en los dos
proyectos. Aquí es sencillo: el fichero del INEE ya la trae, así que no
hace falta ninguna fusión externa (a diferencia del proyecto internacional,
que necesitaba un paso aparte para 2000-2022, ver `pisa-maihda-trends/R/07_add_immig.R`).
`R/02_prepare_variables.R` ahora usa por defecto género × educación
parental × cuartil ESCS × `immig` (72 estratos posibles, antes 24), y
`R/03_model_maihda_ccaa_inla.R` / `R/04_model_trend_ccaa_inla.R` incluyen
`immig` como efecto principal para que el PCV no le atribuya de forma
espuria su varianza a la interacción interseccional.

## Tipo de colegio: público/privado

Otra distinción que pediste incorporar al análisis jerárquico. Verificado
directamente contra `PISA2022_CentrosEducativos_Esp.sav` (el fichero de
centros que también nos pasaste): trae `PRIVATESCH` (derivada del marco
muestral, sin missing en 2022: 603 públicos / 363 privados de 966
colegios) codificada como texto `"public"`/`"private"` -- **distinto** de
como la documenta el codebook de PISA 2025 (ahí es numérica, 1=público/
2=privado). También está `SC013Q01TA` (autodeclarada, numérica 1/2, con
algo de missing: 54 de 966), que se usa como respaldo. `R/01_load_data.R`
ahora también lee este fichero de centros y fusiona `public_private` con
los datos de alumno por colegio -- misma función tolerante a ambos
formatos (texto o numérico) que en el proyecto internacional, para no
repetir la sorpresa de HISCED si el INEE cambia de convención en otra
edición.

Al igual que `immig`, `public_private` es una variable de colegio, no de
alumno: no entra en `strata` (no encajaría como dimensión interseccional
de MAIHDA), sino que se usa como covariable de nivel colegio en
`03_model_maihda_ccaa_inla.R` y `04_model_trend_ccaa_inla.R`, dando
directamente la brecha público-privado media con su propio intervalo de
credibilidad.

## Pendiente

- Solo tenemos 2022 cargado. El script de tendencia
  (`04_model_trend_ccaa_inla.R`) necesita al menos 2-3 ediciones para decir
  algo con sentido -- de momento con un solo año podemos hacer el MAIHDA
  transversal (`03_model_maihda_ccaa_inla.R`) pero no la tendencia.
- Decidir el rango temporal si se añaden más ediciones: 2015-2022(-2025)
  da cobertura completa de las 17 CCAA; ediciones anteriores, cobertura
  parcial (panel desbalanceado).

## Relación con `pisa-maihda-trends`

Mismo enfoque conceptual (modelo multivariante de los 3 dominios, MAIHDA
interseccional, INLA bayesiano, comunicar siempre con intervalos), pero
aquí el "país" desaparece como nivel (es un único país, España) y su lugar
lo ocupa la comunidad autónoma real -- y, a diferencia de `learningtower`,
aquí sí tenemos los 10 valores plausibles completos por dominio y la
variable de origen inmigrante. Igual que en el proyecto internacional, los
efectos de estrato y colegio son ahora trivariantes correlacionados (los
tres dominios pueden covariar, no comparten un único efecto) -- VPC/PCV por
dominio y correlación entre dominios, ver `qmd/03-metodos.qmd`.
