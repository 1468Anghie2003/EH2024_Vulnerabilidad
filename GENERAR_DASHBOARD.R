# =====================================================================
#  GENERAR_DASHBOARD.R  (version 3.1)
#  Construye la pagina web (docs/) a partir de las salidas del pipeline.
#  Lee:   salidas/03_tablas/*.csv, salidas/01_bitacora/bitacora_maestra.csv,
#         salidas/01_bitacora/log_consola.txt, salidas/hallazgos.txt,
#         salidas/02_graficos/**/*.png, salidas/05_modelos/simulador_logit.json,
#         scripts/*.R, datos_aux/bolivia_deptos.geojson,
#         datos_aux/equipo.csv (nombres del equipo)
#  Crea:  docs/data/datos.js  y  docs/img/*.png
#  Ninguna cifra de la pagina se escribe a mano: todo sale de aqui.
#  Uso: source("GENERAR_DASHBOARD.R") despues de EJECUTAR_TODO.R
# =====================================================================
for (p in c("jsonlite", "data.table")) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(jsonlite)

raiz <- if (file.exists("salidas")) "." else dirname(sys.frame(1)$ofile)
SAL  <- file.path(raiz, "salidas")
WEB  <- file.path(raiz, "docs")
stopifnot(dir.exists(SAL), dir.exists(WEB))
dir.create(file.path(WEB, "data"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(WEB, "img"),  showWarnings = FALSE, recursive = TRUE)
leer <- function(f) tryCatch(as.data.frame(data.table::fread(f, encoding = "UTF-8", na.strings = c("", "NA"), showProgress = FALSE)),
                             error = function(e) NULL)

# ---- 1. Tablas de resultados (se omiten listados de casos individuales) ----
OMITIR <- c("F06_atipicos_detalle", "F10_anomalias_multivariadas", "F04_duplicados_reales")
tablas <- list()
for (f in list.files(file.path(SAL, "03_tablas"), pattern = "\\.csv$", full.names = TRUE)) {
  k <- sub("\\.csv$", "", basename(f)); if (k %in% OMITIR) next
  d <- leer(f); if (is.null(d) || nrow(d) > 5000) next
  tablas[[k]] <- d
}
cat("Tablas:", length(tablas), "\n")

# ---- 2. Bitacora y log ------------------------------------------------
bit  <- leer(file.path(SAL, "01_bitacora", "bitacora_maestra.csv"))
logx <- { f <- file.path(SAL, "01_bitacora", "log_consola.txt"); if (file.exists(f)) readLines(f, warn = FALSE, encoding = "UTF-8") else character() }
tiempos <- do.call(rbind, lapply(logx, function(l) { m <- regmatches(l, regexec("^\\s*\\d+\\s+(F\\d{2})_[A-Za-z_]+\\.R\\s+([0-9.]+)\\s*$", l))[[1]]
  if (length(m) == 3) data.frame(fase = m[2], segundos = as.numeric(m[3])) }))
if (is.null(tiempos)) tiempos <- data.frame(fase = character(), segundos = numeric())
t_total <- suppressWarnings(as.numeric(sub(".*Tiempo total:\\s*([0-9.]+).*", "\\1", tail(grep("Tiempo total", logx, value = TRUE), 1))))
ciclos  <- grep("^\\s*Ciclo \\d+:", logx, value = TRUE)
regla   <- grep("Regla cumplida", logx, value = TRUE)
hall <- { f <- file.path(SAL, "hallazgos.txt"); if (file.exists(f)) grep("^\\s*\\d+\\.", readLines(f, warn = FALSE, encoding = "UTF-8"), value = TRUE) else character() }

# ---- 3. Graficos: se copia una seleccion (la pagina usa graficos
#         interactivos; los PNG quedan como galeria y respaldo) -----------
idx  <- leer(file.path(SAL, "02_graficos", "indice_graficos.csv"))
pngs <- list.files(file.path(SAL, "02_graficos"), pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
unlink(list.files(file.path(WEB, "img"), pattern = "\\.png$", full.names = TRUE))
graf <- lapply(pngs, function(p) {
  a <- basename(p); file.copy(p, file.path(WEB, "img", a), overwrite = TRUE)
  list(fase = substr(a, 1, 3), archivo = a, titulo = if (!is.null(idx) && a %in% idx$archivo) idx$titulo[match(a, idx$archivo)] else a)
})
cat("Graficos:", length(graf), "\n")

# ---- 4. Codigo de cada fase (panel de la esquina) -----------------------
scripts <- list()
for (f in c(list.files(file.path(raiz, "scripts"), pattern = "^(F\\d{2}_.*|00_.*)\\.R$", full.names = TRUE)))
  scripts[[sub("\\.R$", "", basename(f))]] <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# ---- 5. Extras: mapa, simulador, equipo -------------------------------
geo  <- { f <- file.path(raiz, "datos_aux", "bolivia_deptos.geojson"); if (file.exists(f)) fromJSON(f, simplifyVector = FALSE) else NULL }
sim  <- { f <- file.path(SAL, "05_modelos", "simulador_logit.json"); if (file.exists(f)) fromJSON(f, simplifyVector = FALSE) else NULL }
equipo <- leer(file.path(raiz, "datos_aux", "equipo.csv"))
cfg  <- list(version = "3.1", titulo = "Datos sucios a patrones reales", subtitulo = "Empleo vulnerable en Bolivia",
             smn = 2500, edad_min = 19, filas = NA, columnas = NA)
cf <- file.path(raiz, "scripts", "00_config.R")
if (file.exists(cf)) { tx <- readLines(cf, warn = FALSE)
  g1 <- function(k) suppressWarnings(as.numeric(sub(paste0(".*", k, "\\s*=\\s*([0-9.]+).*"), "\\1", grep(paste0("^\\s*", k, "\\s*="), tx, value = TRUE)[1])))
  cfg$smn <- g1("smn"); cfg$edad_min <- g1("edad_min") }
if (!is.null(bit) && nrow(bit)) { cfg$filas <- bit$filas[1]; cfg$columnas <- bit$columnas[1] }

# ---- 6. Escritura -----------------------------------------------------
EH <- list(cfg = cfg, equipo = equipo, tablas = tablas, bitacora = bit, graficos = graf,
           hallazgos = hall, scripts = scripts, geo = geo, sim = sim,
           log = list(tiempos = tiempos, tiempo_total = t_total, ciclos = ciclos, regla = regla))
js <- paste0("window.EH = ", toJSON(EH, dataframe = "rows", auto_unbox = TRUE, na = "null", digits = I(10), null = "null"), ";")
writeBin(charToRaw(enc2utf8(js)), file.path(WEB, "data", "datos.js"))
cat(sprintf("docs/data/datos.js: %.0f KB\nAbrir: %s\n", file.size(file.path(WEB, "data", "datos.js")) / 1024,
            normalizePath(file.path(WEB, "index.html"), mustWork = FALSE)))
