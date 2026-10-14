# =====================================================================
#  PROYECTO FINAL - MINERIA DE DATOS | UMSA - Informatica
#  Empleo vulnerable en Bolivia | Encuesta de Hogares 2024 (INE)
#  00_config.R : rutas, parametros, paquetes, tema, bitacora y utilidades
#  Metodologia: CRISP-DM. Este archivo lo cargan todas las fases.
# =====================================================================

options(warn = 1, scipen = 8, dplyr.summarise.inform = FALSE)

# Al abrir ProyectoEH.Rproj el directorio de trabajo es la carpeta del proyecto; y # en otro caso se usa la ruta fija.
# ---------------------------------------------------------------------
RAIZ <- getwd()
if (!file.exists(file.path(RAIZ, "persona.csv")) && file.exists(file.path(RAIZ, "..", "persona.csv")))
  RAIZ <- normalizePath(file.path(RAIZ, ".."))
if (!file.exists(file.path(RAIZ, "persona.csv")))
  RAIZ <- "C:/Users/HP/Desktop/EH2024_Vulnerabilidad"
RUTA_CSV <- file.path(RAIZ, "persona.csv")
if (!file.exists(RUTA_CSV))
  stop("No se encontro persona.csv en ", RAIZ, ". Abrir EH2024_Vulnerabilidad.Rproj o corregir RAIZ en 00_config.R")
RUTA_GEO <- file.path(RAIZ, "datos_aux", "bolivia_deptos.geojson")

DIR <- list(
  bit  = file.path(RAIZ, "salidas", "01_bitacora"),
  graf = file.path(RAIZ, "salidas", "02_graficos"),
  tab  = file.path(RAIZ, "salidas", "03_tablas"),
  dat  = file.path(RAIZ, "salidas", "04_datos"),
  mod  = file.path(RAIZ, "salidas", "05_modelos"),
  web  = file.path(RAIZ, "docs"),
  chk  = file.path(RAIZ, "salidas", "99_checkpoints"))
invisible(lapply(DIR, dir.create, recursive = TRUE, showWarnings = FALSE))
RUTA_BIT <- file.path(DIR$bit, "bitacora_maestra.csv")

# ---------------------------------------------------------------------
# 0.2 PARAMETROS (unico lugar a editar)
# ---------------------------------------------------------------------
CFG <- list(
  version     = "3.1",
  titulo      = "Datos sucios a patrones reales: Empleo vulnerable en Bolivia",
  responsable = "Equipo del seminario - UMSA Informatica",
  pct_prueba_formato = 0.40, # hogares usados en la prueba de resistencia al formato regional
  dataset     = "persona.csv (EH 2024, INE)",
  anio_eh     = 2024,
  fecha_ref   = as.Date("2024-11-15"),  # referencia aproximada del levantamiento
  smn         = 2500,    # Salario Minimo Nacional 2024, DS 5154 (2750 rige recien en 2025)
  edad_min    = 19,      # poblacion de estudio
  edad_pet    = 14,      # poblacion en edad de trabajar
  horas_lim   = 40,      # subempleo por horas
  umbral_col  = 0.80,    # % NA a partir del cual una columna seria candidata a eliminarse
  umbral_num  = 0.95,    # % de valores parseables para convertir una columna a numerica
  k_iqr       = 1.5,     # IQR clasico
  k_iqr_log   = 1.5,     # IQR de Tukey sobre log para ingresos (asimetricos)
  m_imp       = 5,       # imputaciones multiples
  k_knn       = 5,
  pct_dup_sim = 0.05,
  max_cnn     = 2000,
  max_dist    = 3000,    # tope de filas por clase para metodos por distancia
  max_iter    = 5,       # vueltas maximas del ciclo de limpieza
  semilla     = 2026,
  k_cv        = 5,       # pliegues de validacion cruzada
  cat_sin_pago = c(6, 7),# s04b_12: familiar sin remuneracion y aprendiz sin remuneracion
  cat_oit     = c(3, 6), # OIT: cuenta propia + trabajador familiar auxiliar
  dpi         = 200,
  mostrar     = FALSE,   # TRUE para ver cada grafico en RStudio (mas lento)
  limpiar_chk = TRUE)    # TRUE: guarda solo los 2 ultimos checkpoints (ahorra disco)
set.seed(CFG$semilla)

# ---------------------------------------------------------------------
# 0.3 PAQUETES
# data.table, rpart, openxlsx y cluster se usan con :: para no tapar
# funciones de dplyr.
# ---------------------------------------------------------------------
PKG_REQ <- c("data.table", "dplyr", "tidyr", "stringr", "stringi", "forcats", "ggplot2",
             "scales", "ggridges", "patchwork", "rpart", "openxlsx", "cluster",
             "ranger", "gbm", "glmnet", "e1071", "jsonlite", "MASS")
falt <- PKG_REQ[!vapply(PKG_REQ, requireNamespace, logical(1), quietly = TRUE)]
if (length(falt)) install.packages(falt)
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(stringi); library(forcats)
  library(ggplot2); library(scales); library(ggridges); library(patchwork)
})
# Opcionales: mapa real (sf, geodata, terra), 3D interactivo (plotly, htmlwidgets),
# fuente moderna (ragg, systemfonts). Si faltan, el script usa alternativas.
TIENE <- function(p) requireNamespace(p, quietly = TRUE)

# ---------------------------------------------------------------------
# 0.4 TEMA VISUAL (aprobado: oscuro corporativo, sin emojis)
# ---------------------------------------------------------------------
PAL <- list(bg = "#0B1026", panel = "#121838", grid = "#232B57", txt = "#E6E8F5", sub = "#9AA3C7",
            violet = "#8B5CF6", cyan = "#22D3EE", coral = "#FB7185", amber = "#FBBF24",
            mint = "#34D399", indigo = "#6366F1", pink = "#EC4899")
GRAD  <- c("#1E1B4B", "#3730A3", "#6366F1", "#8B5CF6", "#EC4899", "#FB7185")
ESCAL <- c("#3730A3", "#6366F1", "#8B5CF6", "#EC4899", "#FB7185", "#FBBF24")
DISC  <- c(PAL$violet, PAL$cyan, PAL$coral, PAL$amber, PAL$mint, PAL$indigo, PAL$pink, "#A3E635", "#F97316",
           "#14B8A6", "#E879F9", "#94A3B8", "#FDE68A", "#60A5FA")
CAP   <- "Elaboracion propia con datos de la EH - INE Bolivia"
COL_SEXO <- c(Hombre = PAL$cyan, Mujer = PAL$pink)
COL_ZONA <- c(Urbana = PAL$cyan, Rural = PAL$amber)
COL_VUL  <- c(Vulnerable = PAL$coral, `No vulnerable` = PAL$mint)

# Fuente: si ragg y systemfonts estan instalados se usa una fuente moderna de Windows
FUENTE <- ""; DISP_PNG <- "png"
if (TIENE("ragg") && TIENE("systemfonts")) {
  fams <- unique(systemfonts::system_fonts()$family)
  pref <- c("Bahnschrift", "Segoe UI", "Montserrat", "Roboto", "Calibri")
  f <- pref[pref %in% fams][1]
  if (!is.na(f)) { FUENTE <- f; DISP_PNG <- ragg::agg_png }
}

theme_eh <- function(base = 12) {
  theme_minimal(base_size = base, base_family = FUENTE) +
    theme(plot.background   = element_rect(fill = PAL$bg, color = NA),
          panel.background  = element_rect(fill = PAL$bg, color = NA),
          panel.grid.major  = element_line(color = PAL$grid, linewidth = 0.3),
          panel.grid.minor  = element_blank(),
          text              = element_text(color = PAL$txt),
          axis.text         = element_text(color = PAL$sub),
          axis.title        = element_text(color = PAL$sub),
          plot.title        = element_text(face = "bold", size = 17),
          plot.title.position = "plot",
          plot.subtitle     = element_text(color = PAL$sub, margin = margin(b = 10)),
          plot.caption      = element_text(color = PAL$sub, size = base - 3),
          legend.position   = "top",
          legend.background = element_rect(fill = PAL$bg, color = NA),
          legend.key        = element_rect(fill = PAL$bg, color = NA),
          strip.text        = element_text(color = PAL$txt, face = "bold"),
          plot.margin       = margin(18, 18, 18, 18))
}
theme_set(theme_eh())
update_geom_defaults("text", list(family = FUENTE))
sin_ejes <- theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid.major = element_blank())

# ---------------------------------------------------------------------
# 0.5 FASES, GRAFICOS E INDICE DE GRAFICOS
# Cada grafico se guarda como Fxx_nn_nombre.png dentro de la carpeta de
# su fase, en el mismo orden en que aparece en el informe.
# ---------------------------------------------------------------------
GRAF <- new.env()
GRAF$fase <- "F00"; GRAF$nombre <- "config"; GRAF$n <- 0L; GRAF$dir <- DIR$graf
RUTA_IDX <- file.path(DIR$graf, "indice_graficos.csv")
GRAF$indice <- if (file.exists(RUTA_IDX)) as.data.frame(data.table::fread(RUTA_IDX)) else
  data.frame(fase = character(), nro = integer(), archivo = character(), titulo = character(), estado = character())

fase_inicio <- function(cod, nombre, titulo) {
  GRAF$fase <- cod; GRAF$nombre <- nombre; GRAF$n <- 0L
  GRAF$dir <- file.path(DIR$graf, paste0(cod, "_", nombre))
  dir.create(GRAF$dir, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(GRAF$dir, pattern = "\\.png$", full.names = TRUE))      # una corrida nueva reemplaza los graficos
  unlink(list.files(DIR$tab, pattern = paste0("^", cod, "_"), full.names = TRUE))
  GRAF$indice <- GRAF$indice[GRAF$indice$fase != cod, , drop = FALSE]
  if (exists("BITACORA", envir = globalenv(), inherits = FALSE)) {
    b <- get("BITACORA", envir = globalenv())
    b <- b[!startsWith(b$fase, cod), , drop = FALSE]
    if (nrow(b)) b$id <- seq_len(nrow(b))
    assign("BITACORA", b, envir = globalenv())
  }
  cat("\n", strrep("=", 70), "\n ", cod, " | ", titulo, "\n", strrep("=", 70), "\n", sep = "")
}

guardar <- function(p, nombre, w = 11, h = 7) {
  GRAF$n <- GRAF$n + 1L
  arch <- sprintf("%s_%02d_%s.png", GRAF$fase, GRAF$n, nombre)
  ok <- tryCatch({
    ggsave(file.path(GRAF$dir, arch), p, width = w, height = h, dpi = CFG$dpi, bg = PAL$bg, device = DISP_PNG)
    if (isTRUE(CFG$mostrar)) print(p)
    "ok"
  }, error = function(e) { message("  [grafico omitido] ", arch, ": ", conditionMessage(e)); "error" })
  tit <- tryCatch(p$labels$title, error = function(e) NULL)
  if (is.null(tit)) tit <- tryCatch(p$patches$annotation$title, error = function(e) NULL)
  if (is.null(tit)) tit <- nombre
  GRAF$indice <- rbind(GRAF$indice, data.frame(fase = GRAF$fase, nro = GRAF$n, archivo = arch,
                                               titulo = as.character(tit), estado = ok))
  GRAF$indice <- GRAF$indice[order(GRAF$indice$fase, GRAF$indice$nro), ]
  data.table::fwrite(GRAF$indice, RUTA_IDX, bom = TRUE)
  if (ok == "ok") cat("  [grafico]", arch, "\n")
  invisible(ok)
}

guardar_tabla <- function(x, nombre) {
  ruta <- file.path(DIR$tab, sprintf("%s_%s.csv", GRAF$fase, nombre))
  data.table::fwrite(as.data.frame(x), ruta, bom = TRUE)
  invisible(ruta)
}

# ---------------------------------------------------------------------
# 0.6 BITACORA MAESTRA
# Un registro por accion. Se reescribe en disco en CADA llamada: si el
# script se corta, lo registrado hasta ese momento queda guardado.
# Columnas exigidas: ID, fecha-hora, responsable, dataset, atributo,
# tipo de problema, metodologia, accion, criterio, filas afectadas (N),
# impacto en el dataset y codigo; mas filas, columnas y NA del estado.
# ---------------------------------------------------------------------
BIT_VACIA <- data.frame(id = integer(), fecha_hora = character(), responsable = character(),
                        dataset = character(), fase = character(), atributo = character(),
                        tipo_problema = character(), metodologia = character(), accion = character(),
                        criterio = character(), filas_afectadas = numeric(), impacto = character(),
                        filas = numeric(), columnas = numeric(), na_total = numeric(),
                        codigo = character(), archivo = character())
if (!exists("BITACORA", inherits = FALSE)) {
  BITACORA <- if (file.exists(RUTA_BIT)) as.data.frame(data.table::fread(RUTA_BIT, colClasses = "character")) else BIT_VACIA
  if (nrow(BITACORA)) {
    BITACORA$id <- as.integer(BITACORA$id)
    for (v in c("filas_afectadas", "filas", "columnas", "na_total")) BITACORA[[v]] <- as.numeric(BITACORA[[v]])
  }
}

bitacora <- function(atributo, tipo_problema, metodologia, accion, criterio = "",
                     filas_afectadas = NA, impacto = "", datos = NULL, codigo = "", archivo = "") {
  b <- get("BITACORA", envir = globalenv())
  reg <- data.frame(
    id = nrow(b) + 1L, fecha_hora = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    responsable = CFG$responsable, dataset = CFG$dataset,
    fase = paste(GRAF$fase, GRAF$nombre), atributo = paste(atributo, collapse = ", "),
    tipo_problema = tipo_problema, metodologia = metodologia, accion = accion, criterio = criterio,
    filas_afectadas = as.numeric(filas_afectadas), impacto = impacto,
    filas = if (is.null(datos)) NA else nrow(datos), columnas = if (is.null(datos)) NA else ncol(datos),
    na_total = if (is.null(datos)) NA else sum(is.na(datos)),
    codigo = codigo, archivo = archivo, stringsAsFactors = FALSE)
  b <- rbind(b, reg)
  assign("BITACORA", b, envir = globalenv())
  data.table::fwrite(b, RUTA_BIT, bom = TRUE)
  cat(sprintf("  [bitacora #%03d] %s | %s\n", reg$id, reg$atributo, reg$accion))
  invisible(reg)
}

# ---------------------------------------------------------------------
# 0.7 CHECKPOINTS: cada fase guarda su estado; una fase se puede volver
# a correr sola en una sesion nueva sin repetir las anteriores.
# ---------------------------------------------------------------------
cargar_raw <- function() {
  r <- as.data.frame(data.table::fread(RUTA_CSV, colClasses = "character", na.strings = c("", "NA"),
                                       encoding = "UTF-8", showProgress = FALSE))
  names(r) <- tolower(str_trim(names(r))); r
}
checkpoint_guardar <- function() {
  objs <- setdiff(ls(globalenv()), c(OBJ_CONFIG, "raw"))
  objs <- objs[!vapply(objs, function(o) is.function(get(o, envir = globalenv())), logical(1))]
  saveRDS(mget(objs, envir = globalenv()), file.path(DIR$chk, paste0(GRAF$fase, ".rds")))
  # Solo se conservan el checkpoint actual y el anterior (ahorra espacio en disco)
  num <- as.integer(sub("F", "", GRAF$fase))
  if (isTRUE(CFG$limpiar_chk) && num > 2) unlink(file.path(DIR$chk, sprintf("F%02d.rds", seq_len(num - 2))))
  cat("  [checkpoint]", GRAF$fase, "guardado (", length(objs), "objetos )\n")
}
requiere <- function(objetos, cod) {
  if (!all(vapply(objetos, exists, logical(1), envir = globalenv(), inherits = FALSE))) {
    f <- file.path(DIR$chk, paste0(cod, ".rds"))
    if (file.exists(f)) {
      list2env(readRDS(f), envir = globalenv())
      cat("  [checkpoint]", cod, "cargado\n")
    } else {
      # Si no existe el resultado de la fase anterior, se ejecuta esa fase
      # (y en cadena las que le falten). Cualquier fase corre sola siempre.
      script <- list.files(file.path(RAIZ, "scripts"), pattern = paste0("^", cod, "_.*\\.R$"), full.names = TRUE)
      if (!length(script)) stop("No se encontro el script de la fase ", cod)
      cat("  [aviso] Falta el resultado de", cod, "-> se ejecuta", basename(script), "\n")
      source(script, local = globalenv())
    }
  }
  if (!exists("raw", envir = globalenv(), inherits = FALSE)) assign("raw", cargar_raw(), envir = globalenv())
  invisible(TRUE)
}

# ---------------------------------------------------------------------
# 0.8 UTILIDADES
# ---------------------------------------------------------------------
DEPTOS <- c("Chuquisaca", "La Paz", "Cochabamba", "Oruro", "Potosi", "Tarija", "Santa Cruz", "Beni", "Pando")
VARS_TEXTO   <- c("folio", "upm", "estrato", "cob_op", "caeb_op", "caeb_os",
                  "s04b_09a_cod", "s04b_10a_cod", "s04e_26_cod")
VARS_INGRESO <- c("yprilab", "yseclab", "ylab", "ynolab", "yper", "yhog", "yhogpc")
VARS_CUANTI  <- c("edad", "aestudio", "phrs", "shrs", "tothrs", "ylab", "ynolab", "yper", "yhog", "yhogpc", "factor")

hay     <- function(d, ...) all(c(...) %in% names(d))
cod_num <- function(x) {
  if (is.numeric(x)) return(x)
  xs <- str_trim(as.character(x))
  n <- suppressWarnings(as.numeric(xs))
  f <- is.na(n) & !is.na(xs)
  if (any(f)) n[f] <- suppressWarnings(as.numeric(str_replace(xs[f], ",", ".")))
  f <- is.na(n) & !is.na(xs)
  if (any(f)) n[f] <- suppressWarnings(as.numeric(str_extract(xs[f], "^-?\\d+(\\.\\d+)?")))
  n
}
pct     <- function(x, d = 1) percent(x, accuracy = 10^-d)
abrev   <- function(x) ifelse(is.na(x), NA, ifelse(abs(x) >= 1e6, paste0(round(x / 1e6, 1), "M"),
                          ifelse(abs(x) >= 1e3, paste0(round(x / 1e3, 1), "K"), as.character(round(x, 1)))))
lab_bs  <- function(x) ifelse(is.na(x), NA, paste("Bs", abrev(x)))
fmt     <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
wmean   <- function(x, w) { ok <- !is.na(x) & !is.na(w); if (!any(ok)) NA_real_ else sum(as.numeric(x[ok]) * w[ok]) / sum(w[ok]) }
wquant  <- function(x, w, p) {
  ok <- !is.na(x) & !is.na(w); x <- x[ok]; w <- w[ok]
  if (!length(x)) return(rep(NA_real_, length(p)))
  o <- order(x); cw <- cumsum(w[o]) / sum(w)
  vapply(p, function(q) x[o][which(cw >= q)[1]], numeric(1))
}
esc01 <- function(x) { r <- range(x, na.rm = TRUE); if (!all(is.finite(r)) || diff(r) == 0) rep(0, length(x)) else (x - r[1]) / diff(r) }
moda  <- function(x) { x <- x[!is.na(x)]; if (!length(x)) return(NA); u <- unique(x); u[which.max(tabulate(match(x, u)))] }
asimetria <- function(x) { x <- x[!is.na(x)]; n <- length(x); if (n < 3 || sd(x) == 0) return(NA_real_); m <- mean(x); (sum((x - m)^3) / n) / (sum((x - m)^2) / n)^1.5 }
curtosis  <- function(x) { x <- x[!is.na(x)]; n <- length(x); if (n < 4 || sd(x) == 0) return(NA_real_); m <- mean(x); (sum((x - m)^4) / n) / (sum((x - m)^2) / n)^2 - 3 }
n_ef  <- function(w) sum(w)^2 / sum(w^2)   # tamanio efectivo de Kish

bin01 <- function(x) {
  n <- cod_num(x); xs <- tolower(stri_trans_general(as.character(x), "Latin-ASCII"))
  n[is.na(n) & str_detect(xs, "^no\\b|no pobre") %in% TRUE] <- 0
  n[is.na(n) & str_detect(xs, "pobre|^si\\b") %in% TRUE] <- 1
  u <- sort(unique(n[!is.na(n)]))
  if (identical(u, c(1, 2))) n[n == 2] <- 0       # codificacion 1 = si, 2 = no
  n
}

seccion_de <- function(v) case_when(
  v %in% c("folio", "area", "depto", "nro", "upm", "estrato", "factor") ~ "0 Identificacion",
  str_starts(v, "s01") ~ "1 Sociodemografica",
  str_starts(v, "s02") | v %in% c("cobersalud", "hnv_ult_a", "quienatenparto") ~ "2 Salud",
  str_starts(v, "s03") | v %in% c("niv_ed", "niv_ed_g", "cmasi", "educ_prev", "aestudio") ~ "3 Educacion",
  str_starts(v, "s04") | v %in% c("cob_op", "caeb_op", "caeb_os", "pet", "pea", "pei", "condact", "phrs", "shrs", "tothrs") ~ "4 Empleo",
  str_starts(v, "s05") ~ "5 Ingresos no laborales",
  TRUE ~ "6 Calculadas (ingreso/pobreza)")

# Identificacion por sus valores de las columnas de edad, sexo y
# parentesco (no se asume el orden de las columnas) y del esquema de condact.
detectar_mapa <- function(d) {
  cand <- intersect(c("s01a_02", "s01a_03", "s01a_05"), names(d))
  anac <- if (hay(d, "s01a_04c")) cod_num(d$s01a_04c) else rep(NA, nrow(d))
  anac[!is.na(anac) & (anac < 1900 | anac > CFG$anio_eh)] <- NA
  perfil <- bind_rows(lapply(cand, function(v) {
    x <- cod_num(d[[v]]); u <- unique(x[!is.na(x)])
    data.frame(columna = v, min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE), n_valores = length(u),
               solo_1_2 = all(u %in% c(1, 2)),
               cor_edad_nacimiento = suppressWarnings(cor(x, CFG$anio_eh - anac, use = "complete.obs")))
  }))
  col_edad  <- perfil$columna[which.max(ifelse(is.na(perfil$cor_edad_nacimiento), -1, perfil$cor_edad_nacimiento) +
                                          (perfil$n_valores > 30))]
  col_sexo  <- perfil$columna[perfil$solo_1_2 & perfil$columna != col_edad][1]
  col_paren <- setdiff(perfil$columna, c(col_edad, col_sexo))[1]
  ca <- if (hay(d, "condact")) cod_num(d$condact) else NA
  esquema <- if (isTRUE(max(ca, na.rm = TRUE) >= 4)) "5 categorias (1 Ocupado, 2 Cesante, 3 Aspirante, 4-5 Inactivo)" else
    "3 categorias (1 Ocupado, 2 Desocupado, 3 Inactivo)"
  perfil$rol_detectado <- case_when(perfil$columna == col_edad ~ "EDAD", perfil$columna == col_sexo ~ "SEXO",
                                    perfil$columna == col_paren ~ "PARENTESCO", TRUE ~ "-")
  list(edad = col_edad, sexo = col_sexo, paren = col_paren, condact = esquema,
       condact5 = startsWith(esquema, "5"), perfil = perfil)
}

# Columnas etiquetadas que se AGREGAN al dataset (nunca reemplazan)
etiquetar <- function(d, M) {
  d$w <- cod_num(d$factor)
  d$edad <- cod_num(d[[M$edad]])
  sx <- cod_num(d[[M$sexo]]); sxt <- tolower(as.character(d[[M$sexo]]))
  sx[is.na(sx) & str_detect(sxt, "hombre|masc")] <- 1; sx[is.na(sx) & str_detect(sxt, "mujer|fem")] <- 2
  d$sexo <- factor(c("Hombre", "Mujer")[sx], levels = c("Hombre", "Mujer"))
  d$parentesco <- cod_num(d[[M$paren]])
  d$jefe <- d$parentesco %in% 1
  ar <- cod_num(d$area); art <- tolower(as.character(d$area))
  ar[is.na(ar) & str_detect(art, "urban")] <- 1; ar[is.na(ar) & str_detect(art, "rural")] <- 2
  d$zona <- factor(c("Urbana", "Rural")[ar], levels = c("Urbana", "Rural"))
  dp <- cod_num(d$depto)
  if (all(is.na(dp))) dp <- match(tolower(stri_trans_general(as.character(d$depto), "Latin-ASCII")), tolower(DEPTOS))
  d$departamento <- factor(DEPTOS[dp], levels = DEPTOS)
  ca <- cod_num(d$condact); pei <- bin01(d$pei); pet <- bin01(d$pet)
  cond <- rep(NA_character_, nrow(d))
  if (M$condact5) { cond[ca %in% 1] <- "Ocupado"; cond[ca %in% 2:3] <- "Desocupado"; cond[ca %in% 4:5] <- "Inactivo" } else {
    cond[ca %in% 1] <- "Ocupado"; cond[ca %in% 2] <- "Desocupado"; cond[ca %in% 3] <- "Inactivo" }
  cat_txt <- tolower(stri_trans_general(as.character(d$condact), "Latin-ASCII"))   # por si condact viene como texto
  cond[is.na(cond) & str_detect(cat_txt, "desocup|cesante|aspirante") %in% TRUE] <- "Desocupado"
  cond[is.na(cond) & str_detect(cat_txt, "inactiv") %in% TRUE] <- "Inactivo"
  cond[is.na(cond) & str_detect(cat_txt, "ocupad") %in% TRUE] <- "Ocupado"
  cond[is.na(cond) & pei %in% 1] <- "Inactivo"
  cond[is.na(cond) & (pet %in% 0 | (!is.na(d$edad) & d$edad < CFG$edad_pet))] <- "Fuera de PET"
  d$condicion <- factor(cond, levels = c("Ocupado", "Desocupado", "Inactivo", "Fuera de PET"))
  d$pobre     <- if (hay(d, "p0")) bin01(d$p0) else NA
  d$pobre_ext <- if (hay(d, "pext0")) bin01(d$pext0) else NA
  for (v in intersect(c("aestudio", "phrs", "shrs", "tothrs", "totper", VARS_INGRESO, "z", "zext", "p1", "p2", "pext1", "pext2", "factor"), names(d)))
    d[[v]] <- cod_num(d[[v]])
  d
}

nivel_educ <- function(a) cut(a, c(-1, 0, 5, 8, 12, 30), ordered_result = TRUE,
                              labels = c("Sin instruccion", "Primaria incompleta", "Primaria completa", "Secundaria", "Superior"))

# ---------------------------------------------------------------------
# Variables objetivo: TRES lentes sobre el mismo ocupado de 19+ anios
#  A Persona : su propio ingreso laboral no alcanza el SMN 2024 (Bs 2,500).
#              Los ocupados sin remuneracion (familiar y aprendiz) tienen
#              ingreso 0 por definicion y quedan en A.
#  B Hogar   : vive en un hogar pobre (p0 = 1): ingreso per capita del
#              hogar menor a la linea de pobreza moderada oficial (z).
#  C OIT     : empleo vulnerable segun la OIT (ODS 8.3): trabajador por
#              cuenta propia o familiar sin remuneracion (s04b_12 = 3, 6).
#  Compuesto (v1): horas < 40 o A o B; se conserva solo como referencia.
#  'vulnerable' (factor) es el enfoque A, usado por fases que piden un
#  solo objetivo; todas las salidas nuevas reportan A, B y C.
# ---------------------------------------------------------------------
marcar_vulnerable <- function(d) {
  no_dup <- if ("dup_repetido" %in% names(d)) !d$dup_repetido else TRUE
  d$pob_estudio  <- d$condicion %in% "Ocupado" & !is.na(d$edad) & d$edad >= CFG$edad_min & no_dup
  d$crit_horas   <- d$tothrs < CFG$horas_lim
  d$crit_ingreso <- d$ylab < CFG$smn
  d$crit_pobreza <- d$pobre == 1
  cat_oc <- if ("s04b_12" %in% names(d)) cod_num(d$s04b_12) else NA
  d$crit_oit     <- cat_oc %in% CFG$cat_oit
  lab <- function(x) factor(ifelse(!d$pob_estudio, NA, ifelse(is.na(x), NA, ifelse(x, "Vulnerable", "No vulnerable"))),
                            levels = c("Vulnerable", "No vulnerable"))
  d$vul_persona <- lab(d$crit_ingreso)
  d$vul_hogar   <- lab(d$crit_pobreza)
  d$vul_oit     <- lab(ifelse(is.na(cat_oc), NA, d$crit_oit))
  m <- cbind(d$crit_horas, d$crit_ingreso, d$crit_pobreza)
  alguno <- rowSums(m, na.rm = TRUE) > 0
  todos  <- rowSums(!is.na(m)) == 3
  d$n_criterios  <- ifelse(d$pob_estudio, rowSums(m, na.rm = TRUE), NA)
  d$vul_compuesto <- factor(ifelse(!d$pob_estudio, NA, ifelse(alguno, "Vulnerable", ifelse(todos, "No vulnerable", NA))),
                            levels = c("Vulnerable", "No vulnerable"))
  d$cuadrante <- factor(ifelse(!d$pob_estudio | is.na(d$crit_ingreso) | is.na(d$crit_pobreza), NA,
                        ifelse(d$crit_ingreso & d$crit_pobreza, "Ambos enfoques",
                        ifelse(d$crit_ingreso, "Solo persona", ifelse(d$crit_pobreza, "Solo hogar", "Ninguno")))),
                        levels = c("Ambos enfoques", "Solo persona", "Solo hogar", "Ninguno"))
  d$vulnerable <- d$vul_persona
  d
}
ENFOQUES <- c(persona = "vul_persona", hogar = "vul_hogar", oit = "vul_oit")
NOMBRE_ENF <- c(persona = "A Persona (ingreso propio < SMN)", hogar = "B Hogar (ingreso per capita < linea de pobreza)",
                oit = "C OIT (cuenta propia o familiar sin pago)")
tasa_w <- function(x, w) { ok <- !is.na(x) & !is.na(w); if (!any(ok)) NA_real_ else sum(w[ok] * (x[ok] == "Vulnerable")) / sum(w[ok]) }

# Indice de Vulnerabilidad Laboral (0-100) por grupo, segun el enfoque
# Componentes (min-max entre grupos): desempleo, subempleo por horas,
# informalidad (no aporta a la Gestora/AFP), baja educacion y la tasa del
# enfoque elegido (A persona o B hogar). Mayor IVL = mas vulnerable.
calc_ivl <- function(d, ..., enfoque = c("persona", "hogar")) {
  enfoque <- match.arg(enfoque); vcol <- ENFOQUES[[enfoque]]
  d$.vul <- d[[vcol]]
  if (!"informal" %in% names(d)) d$informal <- NA
  d |> filter(!is.na(departamento)) |> group_by(...) |>
    summarise(
      desempleo   = wmean(condicion == "Desocupado", ifelse(condicion %in% c("Ocupado", "Desocupado") & edad >= CFG$edad_min, w, NA)),
      subempleo   = wmean(tothrs < CFG$horas_lim, ifelse(pob_estudio, w, NA)),
      informalidad = wmean(informal, ifelse(pob_estudio, w, NA)),
      baja_educ   = wmean(aestudio <= 8, ifelse(pob_estudio, w, NA)),
      tasa_enfoque = tasa_w(.vul, ifelse(pob_estudio, w, NA)),
      ocup_pobre  = wmean(pobre == 1, ifelse(pob_estudio, w, NA)),
      tasa_persona = tasa_w(vul_persona, ifelse(pob_estudio, w, NA)),
      tasa_hogar  = tasa_w(vul_hogar, ifelse(pob_estudio, w, NA)),
      tasa_oit    = tasa_w(vul_oit, ifelse(pob_estudio, w, NA)),
      poblacion   = sum(w[pob_estudio], na.rm = TRUE),
      n           = sum(pob_estudio),
      .groups = "drop") |>
    mutate(informalidad = ifelse(is.na(informalidad), 0, informalidad),
           ivl = 100 * (esc01(desempleo) + esc01(subempleo) + esc01(informalidad) + esc01(baja_educ) + esc01(tasa_enfoque)) / 5,
           enfoque = enfoque, rango = rank(-ivl, ties.method = "first"),
           nivel = cut(ivl, c(-Inf, 33.3, 66.6, Inf), labels = c("Baja", "Media", "Alta")))
}

# Hot deck aleatorio dentro de clase; los donantes deben estar en rango valido
hot_deck <- function(x, clase, min_val = -Inf, max_val = Inf) {
  x <- as.numeric(x); clase <- as.character(clase); clase[is.na(clase)] <- "sin_clase"
  donante_ok <- !is.na(x) & x >= min_val & x <= max_val
  for (g in unique(clase[is.na(x)])) {
    i <- which(is.na(x) & clase == g); pool <- x[donante_ok & clase == g]
    if (!length(pool)) pool <- x[donante_ok]
    if (length(pool)) x[i] <- pool[sample.int(length(pool), length(i), replace = TRUE)]
  }
  x
}

# Seleccion segura de predictores (evita el error de contrasts) y alineacion de niveles
predictores_ok <- function(d, y, cand, max_na = 0.5) {
  cand <- intersect(cand, names(d))
  cand <- cand[vapply(cand, function(v) mean(is.na(d[[v]])) <= max_na, logical(1))]
  while (length(cand)) {
    cc <- complete.cases(d[c(y, cand)])
    ok <- vapply(cand, function(v) { x <- d[[v]][cc]
      if (is.numeric(x)) length(x) > 1 && isTRUE(var(x) > 0) else length(unique(x)) >= 2 }, logical(1))
    if (all(ok)) break
    cand <- cand[ok]
  }
  cand
}
alinear <- function(nd, tr, vars) {
  for (v in vars) if (is.factor(tr[[v]])) nd[[v]] <- factor(as.character(nd[[v]]), levels = levels(tr[[v]]))
  nd
}

# Limites de atipicos por metodo y base de calculo (ingresos > 0, sin copias duplicadas)
limites <- function(x, metodo) {
  y <- x[!is.na(x)]
  switch(metodo,
         "IQR k=1.5"   = { q <- quantile(y, c(.25, .75)); q + c(-1, 1) * CFG$k_iqr * diff(q) },
         "IQR log" = { ly <- log1p(y[y > 0]); q <- quantile(ly, c(.25, .75)); expm1(q + c(-1, 1) * CFG$k_iqr_log * diff(q)) },
         "z > 3"       = mean(y) + c(-3, 3) * sd(y),
         "MAD z > 3.5" = median(y) + c(-3.5, 3.5) * mad(y))
}
base_out <- function(d, v) {
  x <- d[[v]]
  if (v %in% VARS_INGRESO) x[!is.na(x) & x <= 0] <- NA
  if ("dup_repetido" %in% names(d)) x[d$dup_repetido] <- NA
  x
}

# Etiquetas de rama (caeb_op: secciones CAEB codificadas 0 a 20, en orden A a U)
# y de gran grupo ocupacional (primer digito de cob_op, COB 2009)
RAMAS_CAEB <- c(`0` = "Agricultura, ganaderia y pesca", `1` = "Mineria e hidrocarburos", `2` = "Industria manufacturera",
                `3` = "Electricidad y gas", `4` = "Agua y saneamiento", `5` = "Construccion", `6` = "Comercio",
                `7` = "Transporte y almacenamiento", `8` = "Alojamiento y comidas", `9` = "Informacion y comunicaciones",
                `10` = "Financieras y seguros", `11` = "Inmobiliarias", `12` = "Profesionales y tecnicas",
                `13` = "Servicios administrativos", `14` = "Administracion publica", `15` = "Educacion",
                `16` = "Salud y asistencia social", `17` = "Artes y entretenimiento", `18` = "Otros servicios",
                `19` = "Hogares empleadores", `20` = "Organismos extraterritoriales")
GRUPOS_COB <- c(`0` = "Fuerzas armadas", `1` = "Directivos", `2` = "Profesionales", `3` = "Tecnicos de nivel medio",
                `4` = "Empleados de oficina", `5` = "Servicios y vendedores", `6` = "Agricultores y pescadores",
                `7` = "Oficios de construccion e industria", `8` = "Operadores de maquinaria", `9` = "Ocupaciones elementales")
etq_rama <- function(x) { x <- str_trim(as.character(x)); unname(ifelse(x %in% names(RAMAS_CAEB), RAMAS_CAEB[x], NA)) }
etq_cob  <- function(x) { x <- substr(str_trim(as.character(x)), 1, 1); unname(ifelse(x %in% names(GRUPOS_COB), GRUPOS_COB[x], NA)) }

# Mapa de Bolivia: real si estan sf + geodata, si no mapa de mosaicos
GRID_BOL <- data.frame(
  departamento = c("Pando", "La Paz", "Beni", "Oruro", "Cochabamba", "Santa Cruz", "Potosi", "Chuquisaca", "Tarija"),
  col = c(1, 1, 2, 1, 2, 3, 1, 2, 2), fila = c(1, 2, 2, 3, 3, 3, 4, 4, 5))
SHP_BOL <- NULL
cargar_shp <- function() {
  if (!is.null(SHP_BOL) || !TIENE("sf") || !file.exists(RUTA_GEO)) return(SHP_BOL)
  SHP_BOL <<- tryCatch({
    s <- suppressMessages(sf::st_read(RUTA_GEO, quiet = TRUE))
    s$departamento <- stri_trans_general(str_remove(s$name, "^El "), "Latin-ASCII"); s
  }, error = function(e) NULL)
  SHP_BOL
}
mapa_bolivia <- function(tab, var, etiqueta, escala = scale_fill_gradientn(colors = GRAD)) {
  shp <- cargar_shp()
  tab$lab <- paste0(tab$departamento, "\n", etiqueta)
  if (!is.null(shp)) {
    m <- left_join(shp, tab, by = "departamento")
    ggplot(m) + geom_sf(aes(fill = .data[[var]]), color = PAL$bg, linewidth = 0.6) +
      geom_sf_text(aes(label = lab), color = "white", size = 3.1, fontface = "bold") + escala + sin_ejes
  } else {
    m <- left_join(GRID_BOL, tab, by = "departamento")
    ggplot(m, aes(col, -fila, fill = .data[[var]])) +
      geom_tile(width = 0.94, height = 0.94, color = PAL$bg, linewidth = 1.5) +
      geom_text(aes(label = lab), color = "white", fontface = "bold", size = 3.8) +
      escala + coord_equal() + sin_ejes
  }
}

# ---------------------------------------------------------------------
# 0.9 FUNCIONES ESTADISTICAS REUTILIZADAS (F02 antes y F09 despues)
# ---------------------------------------------------------------------
medidas <- function(d, vars, momento) bind_rows(lapply(vars, function(v) {
  x <- d[[v]]; w <- d$w
  if (all(is.na(x))) return(NULL)
  q <- quantile(x, c(.10, .25, .50, .75, .90), na.rm = TRUE, names = FALSE)
  a <- asimetria(x); k <- curtosis(x)
  data.frame(momento = momento, variable = v, n = sum(!is.na(x)), n_na = sum(is.na(x)),
             media = mean(x, na.rm = TRUE), media_ponderada = wmean(x, w), mediana = q[3], moda = moda(round(x)),
             minimo = min(x, na.rm = TRUE), maximo = max(x, na.rm = TRUE), rango = diff(range(x, na.rm = TRUE)),
             varianza = var(x, na.rm = TRUE), desv_est = sd(x, na.rm = TRUE),
             coef_variacion = sd(x, na.rm = TRUE) / abs(mean(x, na.rm = TRUE)),
             p10 = q[1], q1 = q[2], q3 = q[4], p90 = q[5], iqr = q[4] - q[2], asimetria = a, curtosis = k,
             forma = paste(ifelse(is.na(a), "-", ifelse(a > 0.5, "asimetrica a la derecha", ifelse(a < -0.5, "asimetrica a la izquierda", "aprox. simetrica"))),
                           ifelse(is.na(k), "", ifelse(k > 1, "| leptocurtica", ifelse(k < -1, "| platicurtica", "| mesocurtica")))))
}))

ic_media <- function(x, w) { ok <- !is.na(x) & !is.na(w); x <- x[ok]; w <- w[ok]; m <- wmean(x, w)
  se <- sqrt(wmean((x - m)^2, w)) / sqrt(n_ef(w)); c(est = m, li = m - 1.96 * se, ls = m + 1.96 * se, n = length(x)) }
ic_prop <- function(x, w) { ok <- !is.na(x) & !is.na(w); x <- x[ok]; w <- w[ok]; p <- wmean(x, w)
  se <- sqrt(p * (1 - p) / n_ef(w)); c(est = p, li = p - 1.96 * se, ls = p + 1.96 * se, n = length(x)) }

pruebas_hipotesis <- function(d, momento) {
  oc <- d |> filter(condicion %in% "Ocupado", ylab > 0, !is.na(sexo), !is.na(departamento))
  oc$ly <- log(oc$ylab)
  fila <- function(prueba, h0, est, gl, p, interp) data.frame(momento = momento, prueba = prueba, hipotesis_nula = h0,
    estadistico = round(as.numeric(est), 3), gl = as.character(gl), p_valor = signif(p, 3),
    decision = ifelse(p < 0.05, "Se rechaza H0", "No se rechaza H0"), interpretacion = interp)
  tt <- t.test(ly ~ sexo, data = oc)
  brecha <- exp(tt$estimate[1] - tt$estimate[2]) - 1
  an <- summary(aov(ly ~ departamento, data = oc))[[1]]
  kw <- kruskal.test(ylab ~ departamento, data = oc)
  pz <- d |> filter(!is.na(zona), !is.na(pobre)); cz <- suppressWarnings(chisq.test(table(pz$zona, pz$pobre)))
  ps <- d |> filter(!is.na(sexo), condicion %in% c("Ocupado", "Desocupado", "Inactivo")) |> droplevels()
  cs <- suppressWarnings(chisq.test(table(ps$sexo, ps$condicion)))
  oa <- oc |> filter(!is.na(aestudio)); cp <- cor.test(oa$aestudio, oa$ly)
  cs2 <- suppressWarnings(cor.test(oa$aestudio, oa$ly, method = "spearman", exact = FALSE))
  bind_rows(
    fila("t de Student (Welch)", "El log del ingreso medio es igual entre hombres y mujeres", tt$statistic, round(tt$parameter, 1), tt$p.value,
         sprintf("Los hombres ocupados ganan %.1f%% mas que las mujeres (razon de medias geometricas)", 100 * brecha)),
    fila("ANOVA de un factor", "El log del ingreso medio es igual en los 9 departamentos", an$`F value`[1], paste(an$Df, collapse = ", "), an$`Pr(>F)`[1],
         "El departamento de residencia se asocia con el nivel de ingreso"),
    fila("Kruskal-Wallis", "La distribucion del ingreso es igual en los 9 departamentos", kw$statistic, kw$parameter, kw$p.value,
         "Confirmacion no parametrica del ANOVA (no exige normalidad)"),
    fila("Chi-cuadrado de independencia", "La pobreza es independiente del area", cz$statistic, cz$parameter, cz$p.value,
         "Pobreza y area urbana/rural estan asociadas"),
    fila("Chi-cuadrado de independencia", "La condicion de actividad es independiente del sexo", cs$statistic, cs$parameter, cs$p.value,
         "La participacion laboral difiere entre hombres y mujeres"),
    fila("Correlacion de Pearson", "rho = 0 entre anios de estudio y log del ingreso", cp$estimate, cp$parameter, cp$p.value,
         sprintf("Relacion lineal r = %.2f", cp$estimate)),
    fila("Correlacion de Spearman", "rho = 0 entre anios de estudio e ingreso", cs2$estimate, "-", cs2$p.value,
         sprintf("Relacion monotona rho = %.2f", cs2$estimate)))
}

ajustar_mincer <- function(d) {
  oc <- d |> filter(condicion %in% "Ocupado", ylab > 0, tothrs > 0) |> mutate(ly = log(ylab), edad2 = edad^2)
  pr <- predictores_ok(oc, "ly", c("aestudio", "edad", "edad2", "sexo", "zona", "tothrs"))
  m <- lm(reformulate(pr, "ly"), data = oc |> mutate(across(where(is.factor), droplevels)))
  coefs <- as.data.frame(summary(m)$coefficients) |> tibble::rownames_to_column("termino") |>
    setNames(c("termino", "b", "ee", "t", "p")) |>
    mutate(efecto_pct = 100 * (exp(b) - 1), li = 100 * (exp(b - 1.96 * ee) - 1), ls = 100 * (exp(b + 1.96 * ee) - 1),
           r2_aj = summary(m)$adj.r.squared, n = nobs(m))
  list(modelo = m, coef = coefs, r2 = summary(m)$adj.r.squared)
}

source(file.path(RAIZ, "scripts", "00_reconstruccion.R"), local = TRUE)
source(file.path(RAIZ, "scripts", "00_modelos.R"), local = TRUE)
OBJ_CONFIG <- c(ls(), "OBJ_CONFIG", "CFG_OK")
CFG_OK <- TRUE
cat("Configuracion lista | raiz:", RAIZ, "| fuente:", ifelse(FUENTE == "", "sans", FUENTE), "\n")
