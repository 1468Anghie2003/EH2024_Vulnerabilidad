# =====================================================================
#  F01 | AUDITORIA Y PERFILADO INICIAL
#  CRISP-DM: Comprension de los datos | Bitacora paso 1
#  Entradas: persona.csv | Salidas: raw (intacto), eda (tipado), MAPA,
#  diccionario verificado, control de formato, prueba de resistencia,
#  validacion contra el INE, lista inicial de anomalias, 10 graficos
# =====================================================================
if (!exists("CFG_OK")) source(file.path("scripts", "00_config.R"))
fase_inicio("F01", "auditoria", "Auditoria y perfilado inicial")

# ---------------------------------------------------------------------
# 1.1 Carga: todo como texto para no perder codigos con ceros a la izquierda
# ---------------------------------------------------------------------
raw <- as.data.frame(data.table::fread(RUTA_CSV, colClasses = "character", na.strings = c("", "NA"),
                                       encoding = "UTF-8", showProgress = FALSE))
names(raw) <- tolower(str_trim(names(raw)))
HASH <- unname(tools::md5sum(RUTA_CSV))
cat(sprintf("Dataset: %s filas x %s columnas | md5 %s\n", fmt(nrow(raw)), ncol(raw), HASH))
bitacora("todas", "Ingesta de datos", "Lectura completa como texto (colClasses = character)",
         "Carga del dataset original; el objeto raw no se modifica en ninguna fase",
         "Conservar codigos con ceros a la izquierda y valores no numericos", nrow(raw),
         paste("Linea base. md5", HASH), raw, "data.table::fread(RUTA_CSV, colClasses = 'character')")

# Copia tipada solo para explorar (raw queda intacto)
eda <- utils::type.convert(raw, as.is = TRUE)
for (v in intersect(VARS_TEXTO, names(raw))) eda[[v]] <- raw[[v]]

# ---------------------------------------------------------------------
# 1.2 Identificacion de columnas por sus valores
# Edad, sexo y parentesco se reconocen por lo que contienen (no por su
# posicion): la edad es la columna de rango amplio que coincide con
# 2024 - anio de nacimiento; el sexo solo toma los valores 1 y 2.
# ---------------------------------------------------------------------
MAPA <- detectar_mapa(eda)
print(MAPA$perfil)
guardar_tabla(MAPA$perfil, "deteccion_edad_sexo_parentesco")
bitacora(c(MAPA$edad, MAPA$sexo, MAPA$paren), "Identificacion de columnas clave",
         "Perfil de valores (rango, n de valores, solo 1-2) y correlacion con 2024 - s01a_04c",
         sprintf("Edad = %s, Sexo = %s, Parentesco = %s", MAPA$edad, MAPA$sexo, MAPA$paren),
         "Edad: rango amplio y correlacion maxima con el anio de nacimiento; Sexo: solo valores 1 y 2",
         NA, "Columnas clave confirmadas con los propios datos",
         codigo = "detectar_mapa(eda)", archivo = "F01_deteccion_edad_sexo_parentesco.csv")

eda <- etiquetar(eda, MAPA)

tab_condact <- eda |> count(condact = cod_num(condact), pea = cod_num(pea), pei = cod_num(pei), name = "n") |>
  arrange(condact, pea, pei)
print(tab_condact)
guardar_tabla(tab_condact, "condact_vs_pea_pei")
bitacora("condact", "Codificacion distinta a la del diccionario", "Tabla cruzada condact x pea x pei",
         paste("Esquema detectado:", MAPA$condact, "| se agrega la columna condicion (Ocupado, Desocupado, Inactivo, Fuera de PET)"),
         "Si condact tiene codigos 4 y 5, el esquema de 3 categorias deja a los inactivos sin etiqueta",
         sum(cod_num(eda$condact) >= 4, na.rm = TRUE), "Corrige la subestimacion de inactivos del borrador anterior",
         codigo = "detectar_mapa(); etiquetar()", archivo = "F01_condact_vs_pea_pei.csv")

# ---------------------------------------------------------------------
# 1.3 Diccionario verificado: descripcion, tipo estadistico y perfil
# ---------------------------------------------------------------------
describir <- function(v) {
  if (v == MAPA$edad)  return("Edad en anios cumplidos (verificado por datos)")
  if (v == MAPA$sexo)  return("Sexo: 1 Hombre, 2 Mujer (verificado por datos)")
  if (v == MAPA$paren) return("Parentesco con el jefe del hogar (verificado por datos)")
  case_when(
    v == "folio" ~ "Identificador del hogar", v == "nro" ~ "Numero de orden de la persona en el hogar",
    v == "area" ~ "Area: 1 Urbana, 2 Rural", v == "depto" ~ "Departamento (1 Chuquisaca ... 9 Pando)",
    v == "upm" ~ "Unidad primaria de muestreo", v == "estrato" ~ "Estrato de diseno muestral",
    v == "factor" ~ "Factor de expansion de la persona",
    str_detect(v, "^s01a_04") ~ "Fecha de nacimiento (a dia, b mes, c anio)",
    str_detect(v, "^s01a_05[abc]") ~ "Complemento de edad / datos de nacimiento",
    v == "s01a_06" ~ "Estado civil o conyugal",
    str_detect(v, "^s01a_0[789]") ~ "Autopertenencia a NPIOC", v == "s01a_10" ~ "Idioma materno",
    str_detect(v, "^s01b") ~ "Migracion y residencia habitual 5 anios atras",
    str_detect(v, "^s02a") ~ "Salud: dolencias, atencion y gasto (30 dias)",
    str_detect(v, "^s02b") ~ "Discapacidad (preguntas del grupo de Washington)",
    str_detect(v, "^s02c") ~ "Control prenatal", str_detect(v, "^s02d") ~ "Fecundidad",
    str_detect(v, "^s03a") ~ "Alfabetismo, inscripcion, nivel y curso aprobado",
    str_detect(v, "^s03b") ~ "Establecimiento educativo y causas de no asistencia",
    str_detect(v, "^s03c") ~ "Uso de TIC (internet, celular, computadora)",
    str_detect(v, "^s04a") ~ "Condicion de ocupacion en la semana anterior",
    str_detect(v, "^s04b") ~ "Ocupacion principal, tareas y categoria ocupacional",
    str_detect(v, "^s04c") ~ "Actividad economica del establecimiento",
    str_detect(v, "^s04d") ~ "Horas, estabilidad laboral y aportes",
    str_detect(v, "^s04e") ~ "Ingresos de la ocupacion principal",
    str_detect(v, "^s04f") ~ "Ocupacion secundaria e ingresos",
    str_detect(v, "^s05a") ~ "Jubilaciones, pensiones, alquileres, intereses, remesas",
    str_detect(v, "^s05b") ~ "Bonos estatales (Renta Dignidad, Juancito Pinto, Juana Azurduy)",
    str_detect(v, "^s05c") ~ "Transferencias privadas e ingresos extraordinarios",
    v == "cob_op" ~ "Codigo de ocupacion principal (COB)", v == "caeb_op" ~ "Rama de actividad principal (CAEB)",
    v == "caeb_os" ~ "Rama de actividad secundaria (CAEB)", v == "pet" ~ "Poblacion en edad de trabajar",
    v == "pea" ~ "Poblacion economicamente activa", v == "pei" ~ "Poblacion economicamente inactiva",
    v == "condact" ~ paste("Condicion de actividad:", MAPA$condact),
    v == "phrs" ~ "Horas semanales, ocupacion principal", v == "shrs" ~ "Horas semanales, ocupacion secundaria",
    v == "tothrs" ~ "Horas semanales totales", v == "totper" ~ "Total de personas del hogar",
    v == "yprilab" ~ "Ingreso laboral ocupacion principal (Bs/mes)", v == "yseclab" ~ "Ingreso laboral secundario (Bs/mes)",
    v == "ylab" ~ "Ingreso laboral total (Bs/mes)", v == "ynolab" ~ "Ingreso no laboral (Bs/mes)",
    v == "yper" ~ "Ingreso personal total (Bs/mes)", v == "yhog" ~ "Ingreso total del hogar (Bs/mes)",
    v == "yhogpc" ~ "Ingreso per capita del hogar (Bs/mes)", v == "z" ~ "Linea de pobreza moderada (Bs)",
    v == "zext" ~ "Linea de pobreza extrema (Bs)", v == "p0" ~ "Pobreza moderada (0/1)",
    v == "p1" ~ "Brecha de pobreza moderada (FGT1)", v == "p2" ~ "Severidad de pobreza moderada (FGT2)",
    v == "pext0" ~ "Pobreza extrema (0/1)", v == "pext1" ~ "Brecha de pobreza extrema", v == "pext2" ~ "Severidad de pobreza extrema",
    v == "tipohogar" ~ "Tipo de hogar", v == "cobersalud" ~ "Cobertura de seguro de salud",
    v == "hnv_ult_a" ~ "Hijos nacidos vivos en el ultimo anio", v == "quienatenparto" ~ "Quien atendio el ultimo parto",
    v == "niv_ed" ~ "Nivel educativo alcanzado", v == "niv_ed_g" ~ "Nivel educativo agrupado",
    v == "cmasi" ~ "Condicion de asistencia escolar", v == "educ_prev" ~ "Nivel educativo previo",
    v == "aestudio" ~ "Anios de estudio aprobados", TRUE ~ "Ver cuestionario EH 2024")
}
clasificar <- function(v, x) {
  if (v %in% c("folio", "nro", "upm", "estrato")) return("Identificador")
  if (v == "factor") return("Cuantitativa continua (ponderador)")
  if (v %in% c("depto", "area")) return("Cualitativa nominal (espacial)")
  if (v %in% c("s01a_04a", "s01a_04b", "s01a_04c")) return("Temporal (fecha)")
  if (v %in% c("niv_ed", "niv_ed_g", "educ_prev")) return("Cualitativa ordinal")
  if (v %in% c("p0", "pext0", "pet", "pea", "pei", MAPA$sexo)) return("Cualitativa nominal (binaria)")
  if (v %in% c(VARS_INGRESO, "z", "zext", "p1", "p2", "pext1", "pext2")) return("Cuantitativa continua")
  if (v %in% c("aestudio", "tothrs", "phrs", "shrs", "totper", MAPA$edad)) return("Cuantitativa discreta")
  u <- unique(x[!is.na(x)])
  if (!length(u)) return("Sin datos")
  if (!is.numeric(x)) return(if (length(u) <= 30) "Cualitativa nominal" else "Texto / codigo")
  if (all(u == round(u)) && length(u) <= 15) return("Cualitativa nominal (codificada)")
  if (all(u == round(u))) return("Cuantitativa discreta")
  "Cuantitativa continua"
}
num_stat <- function(x, f) if (is.numeric(x) && any(!is.na(x))) f(x, na.rm = TRUE) else NA_real_
perfil0 <- bind_rows(lapply(names(raw), function(v) {
  x <- eda[[v]]
  data.frame(variable = v, seccion = seccion_de(v), descripcion = describir(v),
             tipo_estadistico = clasificar(v, x), tipo_r = class(x)[1],
             n_validos = sum(!is.na(x)), n_na = sum(is.na(x)), pct_na = round(100 * mean(is.na(x)), 2),
             n_unicos = length(unique(x[!is.na(x)])), min = num_stat(x, min), max = num_stat(x, max),
             media = round(num_stat(x, mean), 3),
             ejemplo = { y <- x[!is.na(x)]; if (length(y)) as.character(y[1]) else NA_character_ })
}))
guardar_tabla(perfil0, "diccionario_verificado")
print(perfil0 |> count(tipo_estadistico, sort = TRUE))
bitacora("todas", "Perfilado", "Perfil univariado de las 275 columnas", "Diccionario verificado con tipo estadistico",
         "Clasificacion: cualitativa (nominal, ordinal), cuantitativa (discreta, continua), temporal, espacial, identificador",
         ncol(raw), "Sin cambios en los datos", codigo = "perfil0", archivo = "F01_diccionario_verificado.csv")

# ---------------------------------------------------------------------
# 1.4 Lista inicial de anomalias a corregir
# ---------------------------------------------------------------------
lim_log <- function(x) { lx <- log1p(x[!is.na(x) & x > 0]); q <- quantile(lx, c(.25, .75)); expm1(q + c(-1, 1) * CFG$k_iqr_log * diff(q)) }
L_ylab <- if (hay(eda, "ylab")) lim_log(eda$ylab) else c(-Inf, Inf)
anac <- cod_num(eda$s01a_04c)
anomalias <- tribble(
  ~anomalia, ~variables, ~n, ~se_corrige_en,
  "Columnas con mas de 80% de NA", "varias", sum(perfil0$pct_na > 100 * CFG$umbral_col), "F05 (se analizan, no se eliminan)",
  "Columnas totalmente vacias", "varias", sum(perfil0$n_validos == 0), "F05",
  "Columnas constantes (un solo valor)", "varias", sum(perfil0$n_unicos == 1), "F05",
  "condact con 5 codigos y diccionario con 3", "condact", sum(cod_num(eda$condact) >= 4, na.rm = TRUE), "F01 (resuelto)",
  "Filas duplicadas exactas", "todas", sum(duplicated(raw)), "F04",
  "Llave folio + nro repetida", "folio, nro", sum(duplicated(raw[c("folio", "nro")])), "F04",
  "Horas semanales > 168 (imposible)", "tothrs, phrs", sum(eda$tothrs > 168 | cod_num(eda$phrs) > 168, na.rm = TRUE), "F07",
  "Ingreso laboral negativo", "ylab", sum(eda$ylab < 0, na.rm = TRUE), "F07",
  "yper distinto de ylab + ynolab", "yper", sum(abs(rowSums(eda[c("ylab", "ynolab")], na.rm = TRUE) - eda$yper) > 1, na.rm = TRUE), "F07",
  "Edad fuera de 0-110", "edad", sum(eda$edad < 0 | eda$edad > 110, na.rm = TRUE), "F07",
  "Edad inconsistente con anio de nacimiento (> 1 anio)", "edad, s01a_04c", sum(abs(CFG$anio_eh - anac - eda$edad) > 1, na.rm = TRUE), "F07",
  "Anios de estudio mayores que edad - 3", "aestudio", sum(eda$aestudio > eda$edad - 3 & eda$edad >= 4, na.rm = TRUE), "F07",
  "Ocupados sin ingreso laboral", "ylab", sum(eda$condicion %in% "Ocupado" & is.na(eda$ylab)), "F05",
  "Ocupados sin horas trabajadas", "tothrs", sum(eda$condicion %in% "Ocupado" & is.na(eda$tothrs)), "F05",
  "Factor de expansion faltante o <= 0", "factor", sum(is.na(eda$w) | eda$w <= 0), "F03",
  "Ingreso laboral atipico (IQR sobre log)", "ylab", sum(eda$ylab > 0 & (eda$ylab < L_ylab[1] | eda$ylab > L_ylab[2]), na.rm = TRUE), "F06")
print(anomalias)
guardar_tabla(anomalias, "anomalias_iniciales")
bitacora("varias", "Lista inicial de anomalias", "Reglas de conteo sobre los datos sin tratar",
         sprintf("%d tipos de anomalia, %s casos en total", nrow(anomalias), fmt(sum(anomalias$n))),
         "Cada anomalia se asigna a la fase del ciclo que la corrige", sum(anomalias$n),
         "Hoja de ruta de la limpieza", codigo = "anomalias", archivo = "F01_anomalias_iniciales.csv")

# ---------------------------------------------------------------------
# 1.4b Control de formato numerico y prueba de resistencia
# (a) Se revisa que cada valor numerico este escrito en formato estandar.
#     Si el archivo hubiera pasado por una configuracion regional distinta
#     (coma decimal, punto de miles) se reconstruye con las reglas R1-R7.
# (b) Prueba de resistencia: sobre el archivo sano se simula esa
#     conversion en 4 de cada 10 hogares y se mide cuanto recuperan las
#     reglas. Asi el tratamiento queda probado aunque el archivo este bien.
# (c) Validacion externa: medias del DDI del INE y pobreza oficial.
# ---------------------------------------------------------------------
formato <- diagnosticar_formato(raw)
N_FMT <- sum(formato$digitos_agrupados + formato$cientifica_con_coma)
print(formato); guardar_tabla(formato, "revision_formato")
VARS_REC <- intersect(names(RANGO_REC), names(raw))
VAL_FINAL <- lapply(setNames(VARS_REC, VARS_REC), function(v) suppressWarnings(as.numeric(raw[[v]])))
REC <- NULL
if (N_FMT > 0) {
  REC   <- reconstruir_numericos(raw)
  REC_C <- reconstruir_cientifica(raw, REC$valores)
  REC$valores <- c(REC$valores, REC_C$valores[setdiff(names(REC_C$valores), names(REC$valores))])
  for (v in names(REC_C$valores)) if (!v %in% names(REC$reglas))
    REC$reglas[[v]] <- ifelse(grepl(PAT_CIENT, raw[[v]]), "R6-R7 notacion cientifica", "Sin cambios")
  VAL_FINAL <- REC$valores
  guardar_tabla(bind_rows(REC$diagnostico, REC_C$diagnostico), "reconstruccion_reglas")
  rm(REC_C)
}
bitacora(formato$variable, "Control de formato numerico",
         "Busqueda de digitos agrupados de a tres ('^d{1,3}(.d{3}){2,}$') y notacion cientifica con coma",
         if (N_FMT > 0) sprintf("%s valores con formato regional reconstruidos (reglas R1-R7)", fmt(N_FMT)) else "Formato estandar en todas las variables numericas: sin cambios",
         "Un valor numerico mal leido cambia todas las cifras posteriores", N_FMT,
         "Valores numericos listos para el analisis", raw, "diagnosticar_formato(raw)", "F01_revision_formato.csv")

# (b) Prueba de resistencia
set.seed(CFG$semilla)
fol <- unique(raw$folio); fol_p <- sample(fol, round(CFG$pct_prueba_formato * length(fol)))
ip <- which(raw$folio %in% fol_p)
cols_p <- unique(c("folio", "nro", "estrato", "p0", "p1", "pext0", VARS_REC))
orig <- raw[ip, cols_p]
if (N_FMT > 0) for (v in VARS_REC) orig[[v]] <- format(VAL_FINAL[[v]][ip], digits = 15, scientific = FALSE, trim = TRUE)
sim <- orig; for (v in VARS_REC) sim[[v]] <- simular_formato_regional(orig[[v]])
t_rec <- Sys.time()
REC_P <- reconstruir_numericos(sim)
cat(sprintf("Prueba de resistencia: %s personas, %.1f s\n", fmt(nrow(sim)), as.numeric(difftime(Sys.time(), t_rec, units = "secs"))))
prueba <- comparar_recuperacion(orig, sim, REC_P$valores) |> filter(alterados > 0)
prueba_reglas <- REC_P$diagnostico
print(prueba); guardar_tabla(prueba, "prueba_formato_resumen"); guardar_tabla(prueba_reglas, "prueba_formato_reglas")
medias_prueba <- bind_rows(lapply(c("ylab", "yhogpc", "yhog", "factor", "z"), function(v) {
  real <- suppressWarnings(as.numeric(orig[[v]])); dir <- lectura_directa(sim[[v]]); rec <- REC_P$valores[[v]]
  data.frame(variable = v, media_real = mean(real, na.rm = TRUE), media_lectura_directa = mean(dir, na.rm = TRUE),
             media_con_tratamiento = mean(rec, na.rm = TRUE))
}))
print(medias_prueba); guardar_tabla(medias_prueba, "prueba_formato_medias")
ej <- unlist(lapply(c("ylab", "yhogpc", "factor", "z"), function(v) head(sample(which(sim[[v]] != orig[[v]])), 4)))
ej_var <- rep(c("ylab", "yhogpc", "factor", "z"), each = 4)
ejemplos_formato <- data.frame(variable = ej_var,
                               valor_real = mapply(function(v, i) as.numeric(orig[[v]][i]), ej_var, ej),
                               como_se_veria = mapply(function(v, i) sim[[v]][i], ej_var, ej),
                               lectura_directa = mapply(function(v, i) lectura_directa(sim[[v]][i]), ej_var, ej),
                               con_tratamiento = mapply(function(v, i) REC_P$valores[[v]][i], ej_var, ej),
                               regla = mapply(function(v, i) REC_P$reglas[[v]][i], ej_var, ej))
guardar_tabla(ejemplos_formato, "prueba_formato_ejemplos")
N_PRUEBA <- sum(prueba$alterados)
bitacora(prueba$variable, "Prueba de resistencia al formato regional",
         sprintf("Simulacion en %.0f%% de los hogares (%s personas) y reconstruccion a ciegas con R1-R5", 100 * CFG$pct_prueba_formato, fmt(nrow(sim))),
         sprintf("%s de %s valores alterados recuperados exactamente (%.2f%%)", fmt(sum(prueba$recuperados_exactos)), fmt(N_PRUEBA),
                 100 * sum(prueba$recuperados_exactos) / N_PRUEBA),
         "El tratamiento debe probarse aunque el archivo de trabajo venga sano", N_PRUEBA,
         sprintf("Ingreso laboral medio: real %s | lectura directa %s | con tratamiento %s",
                 fmt(medias_prueba$media_real[1]), fmt(medias_prueba$media_lectura_directa[1]), fmt(medias_prueba$media_con_tratamiento[1])),
         codigo = "simular_formato_regional(); reconstruir_numericos(sim)", archivo = "F01_prueba_formato_resumen.csv")
rm(orig, sim, REC_P)

# (c) Validacion contra el DDI oficial del INE (catalogo ANDA 163)
d_num <- as.data.frame(VAL_FINAL)
val_ddi <- validar_contra_ddi(d_num)
print(val_ddi); guardar_tabla(val_ddi, "validacion_contra_ddi")
p0n <- suppressWarnings(as.numeric(raw$p0))
consist_p0 <- data.frame(indicador = "Coherencia yhogpc < z con p0", coincidencia_p0 = mean((d_num$yhogpc < d_num$z) == (p0n == 1), na.rm = TRUE))
w_v <- d_num$factor
pob_tasa <- data.frame(indicador = c("Pobreza moderada (poblacion, ponderada)", "Pobreza extrema (poblacion, ponderada)"),
                       calculada = c(sum(w_v * (p0n == 1), na.rm = TRUE) / sum(w_v[!is.na(p0n)]),
                                     sum(w_v * (suppressWarnings(as.numeric(raw$pext0)) == 1), na.rm = TRUE) / sum(w_v[!is.na(p0n)])),
                       oficial_ine = c(0.377, 0.128), fuente = "INE, Encuesta de Hogares 2024 (comunicado de resultados de pobreza)")
print(consist_p0); print(pob_tasa)
guardar_tabla(consist_p0, "consistencia_pobreza"); guardar_tabla(pob_tasa, "pobreza_vs_oficial")
bitacora(c("ylab", "yhogpc", "z", "p0", "factor"), "Validacion externa", "Medias del DDI del INE, coherencia yhogpc < z con p0 y tasa de pobreza oficial",
         sprintf("ylab medio %s (DDI %s) | p0 coherente %.1f%% | pobreza %.1f%% (INE 37.7%%)",
                 fmt(val_ddi$media_calculada[val_ddi$variable == "ylab"]), fmt(val_ddi$media_oficial[val_ddi$variable == "ylab"]),
                 100 * consist_p0$coincidencia_p0, 100 * pob_tasa$calculada[1]),
         "Antes de analizar, los datos deben reproducir las cifras publicadas por el INE", NA, "Datos validados para el analisis",
         codigo = "validar_contra_ddi(); consistencia p0", archivo = "F01_validacion_contra_ddi.csv")
rm(d_num)

# ---------------------------------------------------------------------
# 1.5 Graficos de auditoria
# ---------------------------------------------------------------------
# G1 Tipos de variable (anillo)
g <- perfil0 |> count(tipo_estadistico) |> mutate(p = n / sum(n), tipo_estadistico = fct_reorder(tipo_estadistico, n))
p <- ggplot(g, aes(x = 2, y = n, fill = tipo_estadistico)) +
  geom_col(color = PAL$bg, linewidth = 1.2, width = 1) +
  geom_text(aes(label = ifelse(p > 0.03, paste0(n, "\n", pct(p, 0)), "")), position = position_stack(vjust = 0.5),
            color = "white", size = 3.3, fontface = "bold") +
  annotate("text", x = 0.6, y = 0, label = paste0(ncol(raw), "\nvariables"), color = PAL$txt, size = 6, fontface = "bold") +
  coord_polar(theta = "y") + xlim(0.6, 2.5) + scale_fill_manual(values = rep(DISC, 2), name = NULL) +
  labs(title = "Tipos de variable en persona.csv", subtitle = "Clasificacion estadistica de las 275 columnas", caption = CAP) +
  sin_ejes + theme(legend.position = "right")
guardar(p, "tipos_variable", 11, 7)

# G2 Faltantes por seccion (lollipop)
g <- perfil0 |> group_by(seccion) |> summarise(pct_na = mean(pct_na), n_cols = n())
p <- ggplot(g, aes(pct_na, fct_reorder(seccion, pct_na))) +
  geom_segment(aes(x = 0, xend = pct_na, yend = seccion), color = PAL$grid, linewidth = 2.5) +
  geom_point(aes(size = n_cols, color = pct_na)) +
  geom_text(aes(label = sprintf("%.1f%%  (%d cols)", pct_na, n_cols)), hjust = -0.35, color = PAL$txt, size = 3.6) +
  scale_color_gradientn(colors = GRAD, guide = "none") + scale_size(range = c(5, 14), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.35)), labels = \(x) paste0(x, "%")) +
  labs(title = "Valores faltantes por seccion", x = "% promedio de NA", y = NULL,
       subtitle = "Gran parte son NA estructurales: la pregunta no aplica a esa persona", caption = CAP)
guardar(p, "na_por_seccion")

# G3 Faltantes seccion x departamento (heatmap)
secs <- split(names(raw), seccion_de(names(raw)))
g <- bind_rows(lapply(names(secs), function(s)
  data.frame(departamento = eda$departamento, seccion = s, na = rowMeans(is.na(raw[secs[[s]]]))))) |>
  filter(!is.na(departamento)) |> group_by(departamento, seccion) |> summarise(na = mean(na))
p <- ggplot(g, aes(departamento, seccion, fill = na)) +
  geom_tile(color = PAL$bg, linewidth = 1.2) + geom_text(aes(label = pct(na, 0)), color = "white", size = 3.2) +
  scale_fill_gradientn(colors = GRAD, labels = percent, name = "% NA") +
  labs(title = "Mapa de calor de faltantes", subtitle = "Seccion del cuestionario x departamento", x = NULL, y = NULL, caption = CAP) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "right")
guardar(p, "na_seccion_depto")

# G4 Codigo de barras de faltantes: 300 personas x 275 columnas
set.seed(CFG$semilla)
idx <- sample(nrow(raw), min(300, nrow(raw)))
ord <- order(eda$condicion[idx], eda$edad[idx])
g <- as.data.frame(is.na(raw[idx[ord], ])) |> mutate(fila = row_number()) |>
  pivot_longer(-fila, names_to = "variable", values_to = "na") |>
  mutate(variable = factor(variable, levels = names(raw)), seccion = seccion_de(as.character(variable)))
cortes <- eda$condicion[idx[ord]]
p <- ggplot(g, aes(variable, fila, fill = na)) + geom_raster() +
  facet_grid(~seccion, scales = "free_x", space = "free_x",
             labeller = as_labeller(c("0 Identificacion" = "Id", "1 Sociodemografica" = "S1", "2 Salud" = "S2 Salud",
                                      "3 Educacion" = "S3 Educ", "4 Empleo" = "S4 Empleo", "5 Ingresos no laborales" = "S5 Ingresos",
                                      "6 Calculadas (ingreso/pobreza)" = "Calc"))) +
  scale_fill_manual(values = c(`FALSE` = PAL$violet, `TRUE` = PAL$grid), labels = c("Dato presente", "Faltante"), name = NULL) +
  scale_y_reverse(expand = c(0, 0)) +
  labs(title = "Patron de faltantes: codigo de barras", x = "Columnas (por seccion)", y = "Personas (ordenadas por condicion de actividad)",
       subtitle = "Las franjas oscuras en Empleo coinciden con personas no ocupadas: faltante estructural", caption = CAP) +
  theme(axis.text.x = element_blank(), panel.grid = element_blank(), strip.text = element_text(size = 7))
guardar(p, "na_codigo_barras", 13, 7)

# G5 Distribucion del % de NA por columna
p <- ggplot(perfil0, aes(pct_na)) +
  geom_histogram(aes(fill = after_stat(x)), binwidth = 5, boundary = 0, color = PAL$bg) +
  geom_vline(xintercept = 100 * CFG$umbral_col, color = PAL$amber, linetype = "dashed", linewidth = 0.9) +
  annotate("text", x = 100 * CFG$umbral_col, y = Inf, vjust = 1.5, hjust = 1.05, color = PAL$amber, size = 3.6,
           label = sprintf("%d columnas superan el 80%%", sum(perfil0$pct_na > 100 * CFG$umbral_col))) +
  scale_fill_gradientn(colors = GRAD, guide = "none") +
  labs(title = "Cuantas columnas estan vacias y en que medida", x = "% de NA de la columna", y = "Columnas",
       subtitle = "Umbral de eliminacion del 80%: se analiza en F05 si el faltante es estructural", caption = CAP)
guardar(p, "histograma_pct_na")

# G6 Prueba visual de cual columna es la edad
g <- bind_rows(lapply(MAPA$perfil$columna, function(v)
  data.frame(columna = paste0(v, " -> ", MAPA$perfil$rol_detectado[MAPA$perfil$columna == v]),
             valor = cod_num(eda[[v]]), edad_por_nacimiento = CFG$anio_eh - anac))) |>
  filter(!is.na(valor), !is.na(edad_por_nacimiento), edad_por_nacimiento >= 0) |> slice_sample(n = 9000)
p <- ggplot(g, aes(edad_por_nacimiento, valor)) +
  geom_abline(slope = 1, intercept = 0, color = PAL$amber, linetype = "dashed") +
  geom_point(color = PAL$violet, alpha = 0.25, size = 1) + facet_wrap(~columna, scales = "free_y") +
  labs(title = "Identificacion de columnas: cual es la edad",
       subtitle = "Solo la edad cae sobre la diagonal 2024 - anio de nacimiento", x = "2024 - anio de nacimiento (s01a_04c)",
       y = "Valor de la columna", caption = CAP)
guardar(p, "verificacion_edad_sexo", 12, 5.5)

# G7 condact x pea/pei
p <- tab_condact |> mutate(across(c(condact, pea, pei), ~ ifelse(is.na(.x), "NA", as.character(.x))),
                           grupo = paste0("pea=", pea, " | pei=", pei)) |>
  ggplot(aes(grupo, condact, fill = n)) + geom_tile(color = PAL$bg, linewidth = 1.2) +
  geom_text(aes(label = fmt(n)), color = "white", size = 3.6, fontface = "bold") +
  scale_fill_gradientn(colors = GRAD, guide = "none") +
  labs(title = "Codificacion real de condact", subtitle = paste("Esquema detectado:", MAPA$condact),
       x = NULL, y = "Codigo condact", caption = CAP)
guardar(p, "condact_codificacion", 10, 6)

# G8 Lista inicial de anomalias
p <- anomalias |> filter(n > 0) |>
  ggplot(aes(n, fct_reorder(anomalia, n))) +
  geom_segment(aes(x = 0.8, xend = n, yend = anomalia), color = PAL$grid, linewidth = 2.2) +
  geom_point(aes(color = se_corrige_en), size = 5) +
  geom_text(aes(label = fmt(n)), hjust = -0.5, color = PAL$txt, size = 3.3) +
  scale_x_log10(labels = comma, expand = expansion(mult = c(0, 0.2))) +
  scale_color_manual(values = rep(DISC, 2), name = "Fase") +
  labs(title = "Lista inicial de anomalias a corregir", subtitle = "Escala logaritmica | color = fase del ciclo que la trata",
       x = "Casos", y = NULL, caption = CAP) + theme(legend.position = "right")
guardar(p, "anomalias_iniciales", 12, 7)

# G9 Prueba de resistencia al formato regional
g <- prueba |> mutate(variable = fct_reorder(variable, alterados))
p <- ggplot(g, aes(alterados, variable)) + geom_col(fill = PAL$grid, width = 0.7) +
  geom_col(aes(x = recuperados_exactos), fill = PAL$mint, width = 0.4) +
  geom_text(aes(label = sprintf("%s alterados | %.1f%% recuperados", fmt(alterados), pct_recuperados)), hjust = -0.05, color = PAL$txt, size = 3.2) +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.45))) +
  labs(title = "Prueba de resistencia: si el archivo se abriera con otro formato regional",
       subtitle = sprintf("Simulacion en %.0f%% de los hogares | barra verde = valores recuperados exactamente", 100 * CFG$pct_prueba_formato),
       x = "Valores", y = NULL, caption = CAP)
guardar(p, "prueba_formato", 11, 6.5)

# G10 Validacion contra el DDI del INE
g <- val_ddi |> filter(!is.na(media_oficial)) |> mutate(r = media_calculada / media_oficial)
p <- ggplot(g, aes(r, fct_rev(variable))) + geom_vline(xintercept = 1, color = PAL$mint, linetype = "dashed") +
  geom_point(size = 5, color = PAL$cyan) + geom_text(aes(label = sprintf("%s vs %s", fmt(media_calculada, 1), fmt(media_oficial, 1))), vjust = -1.2, color = PAL$txt, size = 3) +
  scale_x_continuous(labels = percent, limits = c(0.9, 1.1)) +
  labs(title = "Media de cada variable frente a la publicada por el INE (DDI)", subtitle = "100% = coincide con el dato oficial",
       x = "Media calculada / media oficial", y = NULL, caption = CAP)
guardar(p, "validacion_ddi", 11, 6)

checkpoint_guardar()
