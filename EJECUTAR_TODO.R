# =====================================================================
#  PROYECTO FINAL - MINERIA DE DATOS | UMSA - Informatica
#  Datos sucios a patrones reales: Empleo vulnerable en Bolivia
#  Encuesta de Hogares 2024 (INE) | version 3.1
#  EJECUTAR_TODO.R : corre las 12 fases en orden con un solo Run
#  (Ctrl + Shift + S o boton Source en RStudio).
#  Requisito: abrir EH2024_Vulnerabilidad.Rproj y tener persona.csv
#  (EH 2024, CSV estandar) en esta carpeta.
#  Despues: source("GENERAR_DASHBOARD.R") actualiza la pagina en docs/.
#  Para correr una sola fase: abrir scripts/Fxx_*.R y Source; carga sola
#  el resultado de la fase anterior desde salidas/99_checkpoints.
# =====================================================================
rm(list = ls())
source(file.path("scripts", "00_config.R"))

FASES <- c("F01_auditoria.R", "F02_descriptivo_inferencial.R", "F03_formatos.R", "F04_duplicados.R",
           "F05_faltantes_imputacion.R", "F06_outliers.R", "F07_validacion_ciclo.R", "F08_transformacion.R",
           "F09_descriptivo_limpio.R", "F10_modelado.R", "F11_patrones.R", "F12_cierre.R")

# Todo lo que se imprime en consola queda tambien en salidas/01_bitacora/log_consola.txt
LOG <- file.path(DIR$bit, "log_consola.txt")
zz <- file(LOG, open = "wt"); sink(zz, split = TRUE); sink(zz, type = "message")
t_inicio <- Sys.time()
tiempos <- data.frame(fase = character(), segundos = numeric())
tryCatch({
  for (.fase in FASES) {
    t0 <- Sys.time()
    source(file.path("scripts", .fase))
    tiempos <- rbind(tiempos, data.frame(fase = .fase, segundos = round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)))
  }
}, error = function(e) {
  cat("\n*** ERROR en", .fase, ":", conditionMessage(e), "***\n")
  cat("Corregir y volver a ejecutar solo esa fase (scripts/", .fase, ").\n", sep = "")
}, finally = {
  print(tiempos)
  cat("Tiempo total:", round(as.numeric(difftime(Sys.time(), t_inicio, units = "mins")), 1), "minutos\n")
  sink(type = "message"); sink(); close(zz)
})
