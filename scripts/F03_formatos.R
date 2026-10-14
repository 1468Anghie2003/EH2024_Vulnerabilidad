# =====================================================================
#  F03 | NORMALIZACION DE ESTRUCTURA Y FORMATOS
#  CRISP-DM: Preparacion de los datos | Bitacora paso 2
#  Cadenas de texto, tokens de vacio, tipos de dato, fechas, etiquetas.
#  Desde aqui se trabaja sobre df; raw sigue intacto.
# =====================================================================
if (!exists("CFG_OK")) source(file.path("scripts", "00_config.R"))
requiere(c("MAPA", "perfil0", "VAL_FINAL", "N_FMT"), "F02")
fase_inicio("F03", "formatos", "Normalizacion de estructura y formatos")
if (exists("eda", inherits = FALSE)) rm(eda)

df <- raw

# ---------------------------------------------------------------------
# 3.1 Cadenas de texto: espacios, tildes, mayusculas y tokens de vacio
# ---------------------------------------------------------------------
TOKENS_NA <- c("", "na", "n/a", "nan", "null", "ns/nr", "ns", "nr", ".", "-", "sin dato")
cambios <- data.frame()
for (v in names(df)) {
  x0 <- df[[v]]; x <- str_squish(x0)
  if (any(str_detect(x, "[^0-9.,\\-]"), na.rm = TRUE)) x <- tolower(stri_trans_general(x, "Latin-ASCII"))
  tok <- !is.na(x) & x %in% TOKENS_NA
  x[tok] <- NA
  df[[v]] <- x
  cambios <- rbind(cambios, data.frame(variable = v, seccion = seccion_de(v),
                                       texto_modificado = sum(!is.na(x0) & !is.na(x) & x0 != x),
                                       tokens_a_na = sum(tok)))
}
guardar_tabla(cambios, "cambios_texto")
bitacora("todas", "Formato de cadenas inconsistente", "str_squish + transliteracion Latin-ASCII + minusculas + tokens de vacio a NA",
         "Normalizacion de texto", paste("Tokens tratados como vacio:", paste(TOKENS_NA[-1], collapse = " | ")),
         sum(cambios$texto_modificado) + sum(cambios$tokens_a_na),
         sprintf("%s celdas de texto homogeneizadas y %s tokens convertidos a NA", fmt(sum(cambios$texto_modificado)), fmt(sum(cambios$tokens_a_na))),
         df, "str_squish(); stri_trans_general(x, 'Latin-ASCII'); tolower()", "F03_cambios_texto.csv")


# ---------------------------------------------------------------------
# 3.1b Valores numericos: si F01 encontro formato regional, se reemplaza
# el texto por el valor reconstruido y se guarda la regla de cada celda.
# Con un archivo en formato estandar este paso no cambia nada.
# ---------------------------------------------------------------------
if (N_FMT > 0 && exists("REC") && !is.null(REC)) {
  n_rec <- 0
  for (v in names(VAL_FINAL)) {
    corr <- !is.na(raw[[v]]) & (grepl(PAT_CORR, raw[[v]]) | grepl(PAT_CIENT, raw[[v]]))
    df[[v]] <- ifelse(is.na(VAL_FINAL[[v]]), NA_character_, format(VAL_FINAL[[v]], digits = 15, scientific = FALSE, trim = TRUE))
    if (any(corr)) df[[paste0("regla_", v)]] <- ifelse(corr, REC$reglas[[v]], NA_character_)
    n_rec <- n_rec + sum(corr)
  }
  df$rec_numerico <- Reduce(`|`, lapply(names(VAL_FINAL), function(v) !is.na(raw[[v]]) & (grepl(PAT_CORR, raw[[v]]) | grepl(PAT_CIENT, raw[[v]]))))
  bitacora(names(VAL_FINAL), "Formato numerico regional", "Sustitucion por el valor reconstruido y validado en F01",
           sprintf("%s celdas reemplazadas; columnas regla_* con la regla de cada celda", fmt(n_rec)),
           "El texto original sigue en raw; cada cambio es trazable", n_rec,
           sprintf("%s filas con al menos un valor reconstruido", fmt(sum(df$rec_numerico))), df,
           "df[[v]] <- VAL_FINAL[[v]]", "F01_reconstruccion_reglas.csv")
}

# ---------------------------------------------------------------------
# 3.2 Tipos de dato: a numerico si >= 95% de los valores se puede leer
# Lo que no se puede leer no se pierde: se guarda en txt_<variable>
# ---------------------------------------------------------------------
tipos <- data.frame()
for (v in names(df)) {
  x <- df[[v]]; tipo <- "character"; perd <- 0
  if (!(v %in% VARS_TEXTO)) {
    xn <- suppressWarnings(as.numeric(str_replace(x, ",", ".")))
    tot <- sum(!is.na(x)); ok <- sum(!is.na(xn))
    if (tot == 0 || ok / tot >= CFG$umbral_num) {
      perd <- tot - ok
      if (perd > 0) df[[paste0("txt_", v)]] <- ifelse(!is.na(x) & is.na(xn), x, NA_character_)
      df[[v]] <- xn; tipo <- "numeric"
    }
  }
  tipos <- rbind(tipos, data.frame(variable = v, tipo_antes = "character", tipo_despues = tipo, no_parseables = perd))
}
guardar_tabla(tipos, "conversion_tipos")
print(tipos |> count(tipo_despues))
bitacora("todas", "Tipo de dato incorrecto (todo se leyo como texto)", sprintf("Conversion a numerico si >= %.0f%% de valores parseables", 100 * CFG$umbral_num),
         sprintf("%d columnas a numerico, %d se mantienen como texto", sum(tipos$tipo_despues == "numeric"), sum(tipos$tipo_despues == "character")),
         paste("Codigos (folio, upm, COB, CAEB) se mantienen como texto:", paste(VARS_TEXTO, collapse = ", ")),
         sum(tipos$no_parseables), sprintf("%d valores no numericos preservados en columnas txt_*", sum(tipos$no_parseables)),
         df, "as.numeric(); umbral_num", "F03_conversion_tipos.csv")

# ---------------------------------------------------------------------
# 3.3 Fechas: fecha de nacimiento y edad calculada a la fecha de referencia
# ---------------------------------------------------------------------
dia <- df$s01a_04a; mes <- df$s01a_04b; anio <- df$s01a_04c
fecha_ok <- !is.na(dia) & !is.na(mes) & !is.na(anio) & dia %in% 1:31 & mes %in% 1:12 & anio >= 1900 & anio <= CFG$anio_eh
df$fecha_nac <- as.Date(ifelse(fecha_ok, sprintf("%04d-%02d-%02d", as.integer(anio), as.integer(mes), as.integer(dia)), NA))
df$edad_calc <- floor(as.numeric(CFG$fecha_ref - df$fecha_nac) / 365.25)
df$val_fecha_invalida <- !is.na(anio) & !fecha_ok
cat("Fechas de nacimiento validas:", fmt(sum(!is.na(df$fecha_nac))), "| invalidas o codigo no sabe:", fmt(sum(df$val_fecha_invalida)), "\n")
bitacora(c("s01a_04a", "s01a_04b", "s01a_04c"), "Fecha dispersa en tres columnas", "Construccion de fecha ISO 8601 (AAAA-MM-DD) con validacion de dia, mes y anio",
         "Nuevas columnas fecha_nac, edad_calc y val_fecha_invalida", "Dia 1-31, mes 1-12, anio 1900-2024; fechas imposibles (31 de febrero) quedan NA",
         sum(df$val_fecha_invalida), "Variable temporal disponible para validar la edad (F07)", df, "as.Date(sprintf('%04d-%02d-%02d', ...))")

# ---------------------------------------------------------------------
# 3.4 Etiquetado de codigos (columnas nuevas, las originales se conservan)
# ---------------------------------------------------------------------
df <- etiquetar(df, MAPA)
df$nivel_educ <- nivel_educ(df$aestudio)
for (v in c("sexo", "zona", "departamento", "condicion")) { cat("\n--", v, "--\n"); print(table(df[[v]], useNA = "ifany")) }
bitacora(c("area", "depto", MAPA$sexo, MAPA$edad, MAPA$paren, "condact", "p0", "pext0"), "Codigos numericos sin etiqueta",
         "Recodificacion con diccionario verificado", "Columnas nuevas: w, edad, sexo, parentesco, jefe, zona, departamento, condicion, pobre, pobre_ext, nivel_educ",
         "Sin tildes en las etiquetas para evitar problemas de codificacion en Windows", nrow(df), "Se agregan 11 columnas", df, "etiquetar(df, MAPA)")

# Factor de expansion faltante o no positivo: mediana del estrato depto x area
df$imp_w <- is.na(df$w) | df$w <= 0
if (any(df$imp_w)) {
  med_w <- df |> filter(!imp_w) |> group_by(departamento, zona) |> summarise(mw = median(w))
  m <- left_join(df[c("departamento", "zona")], med_w, by = c("departamento", "zona"))$mw
  df$w[df$imp_w] <- ifelse(is.na(m[df$imp_w]), median(df$w[!df$imp_w]), m[df$imp_w])
}
bitacora("factor", "Ponderador faltante o no positivo", "Mediana del factor en el mismo departamento y area", "Imputacion del ponderador w",
         "Sin ponderador la persona no puede entrar a ningun indicador poblacional", sum(df$imp_w),
         ifelse(any(df$imp_w), "Ponderador completado y marcado en imp_w", "No hubo casos; columna imp_w en FALSE"), df, "median(w) por depto x zona")

# ---------------------------------------------------------------------
# 3.5 Graficos
# ---------------------------------------------------------------------
g <- tipos |> left_join(perfil0[c("variable", "seccion")], by = "variable") |> count(seccion, tipo_despues)
p <- ggplot(g, aes(n, fct_rev(seccion), fill = tipo_despues)) + geom_col(width = 0.7) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = "white", fontface = "bold", size = 3.4) +
  scale_fill_manual(values = c(numeric = PAL$violet, character = PAL$amber), labels = c(numeric = "Numerica", character = "Texto / codigo"), name = NULL) +
  labs(title = "Tipos de dato despues de la normalizacion", subtitle = "Antes de F03 las 275 columnas eran texto", x = "Columnas", y = NULL, caption = CAP)
guardar(p, "tipos_despues", 10, 6)

g <- cambios |> group_by(seccion) |> summarise(`Texto homogeneizado` = sum(texto_modificado), `Token a NA` = sum(tokens_a_na)) |>
  pivot_longer(-seccion) |> filter(value > 0)
if (nrow(g)) {
  p <- ggplot(g, aes(value, fct_rev(seccion), fill = name)) + geom_col(position = position_dodge(0.7), width = 0.6) +
    geom_text(aes(label = fmt(value)), position = position_dodge(0.7), hjust = -0.2, color = PAL$txt, size = 3.2) +
    scale_fill_manual(values = c(PAL$cyan, PAL$coral), name = NULL) + scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    labs(title = "Celdas modificadas por la normalizacion de texto", x = "Celdas", y = NULL, caption = CAP)
  guardar(p, "celdas_normalizadas", 10, 6)
}

p <- df |> filter(!is.na(edad), !is.na(edad_calc)) |> count(dif = pmax(pmin(edad - edad_calc, 5), -5)) |>
  ggplot(aes(factor(dif), n, fill = abs(dif) <= 1)) + geom_col(width = 0.7) +
  geom_text(aes(label = fmt(n)), vjust = -0.4, color = PAL$txt, size = 3.2) +
  scale_fill_manual(values = c(`TRUE` = PAL$mint, `FALSE` = PAL$coral), labels = c(`TRUE` = "Consistente (+-1)", `FALSE` = "Inconsistente"), name = NULL) +
  labs(title = "Edad declarada vs edad calculada desde la fecha de nacimiento", subtitle = "Diferencia en anios (truncada en +-5)",
       x = "Edad declarada - edad calculada", y = "Personas", caption = CAP)
guardar(p, "edad_vs_fecha", 10, 6)

# Los objetos pesados de la reconstruccion ya no se necesitan (sus resumenes
# quedaron en salidas/03_tablas); se liberan para aligerar los checkpoints
rm(list = intersect(c("REC", "VAL_FINAL"), ls()))
checkpoint_guardar()
