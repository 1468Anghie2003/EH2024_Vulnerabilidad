# =====================================================================
#  F12 | CIERRE: EXPORTACION Y BITACORA FINAL
#  CRISP-DM: Despliegue
#  Dataset limpio, matrices codificadas, base mixta, diccionario final,
#  bitacora en Excel y diagrama del ciclo de limpieza con las cifras
#  reales.
# =====================================================================
if (!exists("CFG_OK")) source(file.path("scripts", "00_config.R"))
requiere(c("df", "MAPA", "df_onehot", "df_dummy", "mixto"), "F11")
fase_inicio("F12", "cierre", "Exportacion y bitacora final")

# ---------------------------------------------------------------------
# 12.1 Verificacion de la regla de oro
# ---------------------------------------------------------------------
stopifnot("Se perdieron filas" = nrow(df) == nrow(raw), "Se perdieron columnas" = all(names(raw) %in% names(df)))
cat(sprintf("Regla cumplida: %s filas | %d columnas originales + %d derivadas = %d\n", fmt(nrow(df)), ncol(raw), ncol(df) - ncol(raw), ncol(df)))

# ---------------------------------------------------------------------
# 12.2 Diccionario final (originales + derivadas)
# ---------------------------------------------------------------------
DESC_NUEVAS <- c(
  w = "Ponderador numerico (factor; 0 en copias duplicadas)", edad = "Edad verificada", sexo = "Sexo etiquetado", parentesco = "Parentesco con el jefe",
  jefe = "Es jefe de hogar", zona = "Area etiquetada", departamento = "Departamento etiquetado", condicion = "Condicion de actividad (Ocupado, Desocupado, Inactivo, Fuera de PET)",
  pobre = "Pobreza moderada 0/1", pobre_ext = "Pobreza extrema 0/1", nivel_educ = "Nivel educativo (discretizacion manual)", fecha_nac = "Fecha de nacimiento ISO",
  edad_calc = "Edad calculada desde la fecha", ylab_original = "Ingreso laboral antes de imputar", pob_estudio = "Ocupado de 19+ (poblacion de estudio)",
  crit_horas = "Criterio: menos de 40 horas", crit_ingreso = "Criterio: ingreso menor al SMN", crit_pobreza = "Criterio: hogar pobre",
  n_criterios = "Numero de criterios cumplidos", vulnerable = "VARIABLE OBJETIVO: empleo vulnerable", educ_kmeans = "Educacion discretizada por k-means",
  yhogpc_ancho = "Ingreso pc: bins de igual ancho", quintil = "Quintil ponderado de ingreso pc", jornada = "Jornada laboral discretizada",
  edad_grupo = "Grupo de edad", generacion = "Generacion por anio de nacimiento", peso_nolab = "Participacion del ingreso no laboral",
  dependencia_nolab = "Dependencia de ingresos no laborales", region = "Region geografica (fuente externa)", ivl_depto = "IVL del departamento",
  nivel_ivl = "Nivel IVL del departamento", calidad_empleo = "Calidad del empleo", cluster = "Segmento k-means", anomalia_multiv = "Anomalia multivariada (Mahalanobis)",
  vul_persona = "OBJETIVO A: ingreso laboral propio < SMN 2024", vul_hogar = "OBJETIVO B: vive en hogar pobre (p0 = 1)",
  vul_oit = "Referencia C: cuenta propia o familiar sin remuneracion (OIT)", vul_compuesto = "Compuesto v1: horas < 40 o A o B",
  crit_oit = "Criterio OIT (s04b_12 en 3, 6)", cuadrante = "Cruce de los enfoques A y B", ylab_cero_estructural = "ylab = 0 por categoria sin remuneracion (6, 7)",
  cat_ocupacional = "Categoria ocupacional (s04b_12)", sector = "Sector del establecimiento (s04b_13)", tamano_empresa = "Personas en el establecimiento (s04b_14)",
  informal = "No aporta a la Gestora (AFP)", seguro_salud_trabajo = "Recibe seguro de salud por su trabajo", antiguedad_anios = "Antiguedad en el trabajo (anios)",
  grupo_ocup = "Gran grupo ocupacional (COB)", rama = "Rama de actividad (CAEB)", ingreso_hora = "Ingreso laboral por hora (Bs)",
  miembros = "Personas en el hogar (conteo por folio)", ocupados_hogar = "Ocupados en el hogar", menores_14 = "Menores de 14 en el hogar",
  mayores_60 = "Personas de 60+ en el hogar", carga_hogar = "Miembros por ocupado del hogar", rec_numerico = "Fila con al menos un valor numerico reconstruido")
desc_derivada <- function(v) case_when(
  v %in% names(DESC_NUEVAS) ~ unname(DESC_NUEVAS[v]),
  str_starts(v, "val_") ~ paste("Marca de validacion:", str_remove(v, "^val_")),
  str_starts(v, "out_") ~ paste("Marca de atipico:", str_remove(v, "^out_")),
  str_starts(v, "imp_") ~ paste("Marca de imputacion:", str_remove(v, "^imp_")),
  str_starts(v, "dup_") ~ paste("Marca de duplicado:", str_remove(v, "^dup_")),
  str_starts(v, "txt_") ~ paste("Texto no numerico preservado de", str_remove(v, "^txt_")),
  str_starts(v, "regla_") ~ paste("Regla de reconstruccion aplicada a", str_remove(v, "^regla_")),
  str_ends(v, "_w") ~ paste("Winsorizado de", str_remove(v, "_w$")), TRUE ~ "")
dic_final <- bind_rows(lapply(names(df), function(v) { x <- df[[v]]
  data.frame(variable = v, origen = ifelse(v %in% names(raw), "original", "derivada"),
             descripcion = if (v %in% names(raw)) perfil0$descripcion[perfil0$variable == v] else desc_derivada(v),
             tipo_r = class(x)[1], n_na = sum(is.na(x)), pct_na = round(100 * mean(is.na(x)), 2), n_unicos = length(unique(x[!is.na(x)]))) }))
guardar_tabla(dic_final, "diccionario_final")

# ---------------------------------------------------------------------
# 12.3 Exportacion de datos
# ---------------------------------------------------------------------
exportar <- function(d, nombre) { ruta <- file.path(DIR$dat, nombre); data.table::fwrite(d, ruta, bom = TRUE); cat("  [datos]", nombre, "\n"); ruta }
exportar(df, "persona_limpio.csv")
exportar(df |> filter(pob_estudio), "poblacion_estudio_ocupados19.csv")
exportar(df_onehot, "persona_onehot.csv")
exportar(df_dummy, "persona_dummy.csv")
exportar(mixto, "base_mixta.csv")
bitacora("todas", "Entrega de resultados", "Exportacion CSV UTF-8 con BOM (abre bien en Excel)",
         "persona_limpio, poblacion de estudio, one-hot, dummy y base mixta", "Todas las filas y columnas originales mas las derivadas y marcas",
         nrow(df), sprintf("%d -> %d columnas", ncol(raw), ncol(df)), df, "data.table::fwrite(bom = TRUE)", "04_datos/")

# ---------------------------------------------------------------------
# 12.5 Diagrama del ciclo de limpieza con las cifras reales
# ---------------------------------------------------------------------
n_fase <- function(cod) sum(as.numeric(BITACORA$filas_afectadas[startsWith(BITACORA$fase, cod)]), na.rm = TRUE)
cajas <- tribble(
  ~id, ~x, ~y, ~titulo, ~detalle,
  1, 1, 6, "1. Auditoria", sprintf("%d anomalias | formato\nverificado", nrow(anomalias)),
  2, 3, 6, "2. Formatos", sprintf("%d columnas tipadas", sum(tipos$tipo_despues == "numeric")),
  3, 5, 6, "3. Duplicados", sprintf("%d copias marcadas", sum(df$dup_repetido)),
  4, 5, 4.5, "4. Faltantes", sprintf("%s celdas imputadas", fmt(sum(df$imp_tothrs | df$imp_aestudio | df$imp_ylab))),
  5, 5, 3, "5. Outliers", sprintf("%s valores marcados", fmt(sum(df[grep("^out_", names(df))]))),
  6, 5, 1.5, "6. Validacion", sprintf("%d reglas | %d vuelta(s)", nrow(validacion0), max(historial$iteracion) - 1),
  7, 5, 0, "Dataset limpio", sprintf("%s filas | %d columnas", fmt(nrow(df)), ncol(df)))
flechas <- tribble(~x, ~y, ~xend, ~yend, 1.8, 6, 2.2, 6, 3.8, 6, 4.2, 6, 5, 5.6, 5, 4.95, 5, 4.05, 5, 3.45, 5, 2.55, 5, 1.95, 5, 1.05, 5, 0.45)
p <- ggplot() +
  geom_segment(data = flechas, aes(x, y, xend = xend, yend = yend), color = PAL$sub, linewidth = 1, arrow = arrow(length = unit(0.25, "cm"), type = "closed")) +
  geom_path(data = data.frame(x = c(5.8, 6.6, 6.6, 5.8), y = c(1.5, 1.5, 4.5, 4.5)), aes(x, y), color = PAL$amber, linewidth = 1.1, linetype = "dashed",
            arrow = arrow(length = unit(0.3, "cm"), type = "closed")) +
  annotate("text", x = 6.75, y = 3, label = sprintf("Ciclo\n%d vuelta(s)\nhasta 0\nviolaciones", max(historial$iteracion) - 1), color = PAL$amber, hjust = 0, size = 3.4) +
  geom_path(data = data.frame(x = c(4.2, 1, 1), y = c(4.5, 4.5, 5.6)), aes(x, y), color = PAL$sub, linetype = "dotted", arrow = arrow(length = unit(0.25, "cm"))) +
  annotate("text", x = 2.3, y = 4.7, label = "la bitacora registra cada paso", color = PAL$sub, size = 3) +
  geom_tile(data = cajas, aes(x, y, fill = factor(id)), width = 1.6, height = 0.9, color = PAL$txt, linewidth = 0.4) +
  geom_text(data = cajas, aes(x, y + 0.15, label = titulo), color = "white", fontface = "bold", size = 4) +
  geom_text(data = cajas, aes(x, y - 0.18, label = detalle), color = "white", size = 3.1) +
  scale_fill_manual(values = c(GRAD[2:6], PAL$amber, PAL$mint), guide = "none") +
  coord_cartesian(xlim = c(0, 7.6), ylim = c(-0.6, 6.6)) +
  labs(title = "Ciclo de preprocesamiento aplicado a persona.csv", subtitle = sprintf("Diagrama de clase con las cifras reales | %d registros en la bitacora", nrow(BITACORA)), caption = CAP) +
  sin_ejes + theme(panel.grid = element_blank())
guardar(p, "diagrama_ciclo", 11, 9)

# Bitacora: registros y filas afectadas por fase
g <- BITACORA |> mutate(fase = str_sub(fase, 1, 3)) |> group_by(fase) |> summarise(registros = n(), filas_afectadas = sum(as.numeric(filas_afectadas), na.rm = TRUE))
p <- ggplot(g, aes(fase, registros, fill = registros)) + geom_col(width = 0.65) + geom_text(aes(label = registros), vjust = -0.5, color = PAL$txt, fontface = "bold") +
  scale_fill_gradientn(colors = GRAD, guide = "none") + scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Bitacora: registros por fase", subtitle = sprintf("%d acciones documentadas con responsable, fecha, criterio, filas afectadas e impacto", nrow(BITACORA) + 1),
       x = "Fase", y = "Registros", caption = CAP)
guardar(p, "bitacora_por_fase", 10, 5.5)

bitacora("todas", "Cierre del preprocesamiento", "Verificacion final y exportacion de la bitacora", "Bitacora maestra en CSV y Excel",
         "Ninguna fila ni columna original eliminada", nrow(df), sprintf("%d graficos generados", sum(GRAF$indice$estado == "ok") + 2), df,
         "stopifnot(nrow(df) == nrow(raw)); openxlsx", "01_bitacora/bitacora_maestra.xlsx")

# ---------------------------------------------------------------------
# 12.6 Bitacora en Excel: hoja general, fichas campo/descripcion y una
# hoja por fase
# ---------------------------------------------------------------------
wb <- openxlsx::createWorkbook()
est_cab <- openxlsx::createStyle(fontColour = "#FFFFFF", fgFill = "#1E1B4B", textDecoration = "bold", halign = "center", wrapText = TRUE, border = "Bottom")
hoja <- function(nombre, d, anchos = NULL) {
  openxlsx::addWorksheet(wb, nombre, tabColour = "#8B5CF6")
  openxlsx::writeData(wb, nombre, d, headerStyle = est_cab)
  openxlsx::freezePane(wb, nombre, firstRow = TRUE)
  openxlsx::setColWidths(wb, nombre, cols = seq_len(ncol(d)), widths = if (is.null(anchos)) "auto" else anchos)
}
bit_x <- BITACORA |> mutate(across(everything(), ~ if (is.character(.x)) str_trunc(.x, 30000) else .x))
hoja("Bitacora", bit_x, c(6, 19, 18, 16, 22, 30, 28, 40, 45, 45, 12, 45, 9, 9, 11, 40, 30))
fichas <- bit_x |> mutate(across(everything(), as.character)) |>
  pivot_longer(-id, names_to = "campo", values_to = "descripcion") |> mutate(id = as.integer(id))
hoja("Fichas (campo-descripcion)", fichas, c(6, 18, 110))
for (fz in unique(str_sub(bit_x$fase, 1, 3))) hoja(paste("Fase", fz), bit_x |> filter(str_sub(fase, 1, 3) == fz) |> select(id, fecha_hora, atributo, tipo_problema, metodologia, accion, criterio, filas_afectadas, impacto),
                                                c(6, 19, 30, 28, 40, 45, 45, 12, 45))
hoja("Indice de graficos", GRAF$indice, c(8, 6, 45, 70, 8))
hoja("Anomalias iniciales", anomalias)
hoja("Hallazgos", data.frame(hallazgo = hallazgos), 160)
openxlsx::saveWorkbook(wb, file.path(DIR$bit, "bitacora_maestra.xlsx"), overwrite = TRUE)
cat("  [bitacora] 01_bitacora/bitacora_maestra.xlsx\n")

checkpoint_guardar()
cat("\n", strrep("=", 70), "\n PROYECTO TERMINADO\n", strrep("=", 70), "\n", sep = "")
cat(sprintf(" Filas: %s (sin eliminar ninguna)\n Columnas: %d originales + %d derivadas\n Bitacora: %d registros\n Graficos: %d PNG en salidas/02_graficos\n Tablas: %d CSV en salidas/03_tablas\n Hallazgos: salidas/hallazgos.txt\n",
            fmt(nrow(df)), ncol(raw), ncol(df) - ncol(raw), nrow(BITACORA), sum(GRAF$indice$estado == "ok"), length(list.files(DIR$tab))))
