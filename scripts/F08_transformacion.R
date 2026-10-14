# =====================================================================
#  F08 | TRANSFORMACION: VARIABLE OBJETIVO, DISCRETIZACION, DUMMY,
#        ONE-HOT ENCODING E INTEGRACION DE FUENTES MIXTAS
#  CRISP-DM: Preparacion de los datos (construccion de atributos)
#  Las matrices codificadas son objetos aparte: df conserva todo.
# =====================================================================
if (!exists("CFG_OK")) source(file.path("scripts", "00_config.R"))
requiere(c("df", "MAPA"), "F07")
fase_inicio("F08", "transformacion", "Variable objetivo, discretizacion, codificacion y fuentes mixtas")
set.seed(CFG$semilla)

# ---------------------------------------------------------------------
# 8.1 Variables objetivo: tres lentes sobre el mismo ocupado de 19+
#   A Persona: ylab < SMN 2024 (Bs 2,500, DS 5154)
#   B Hogar  : p0 = 1 (ingreso per capita del hogar < linea z del INE)
#   C OIT    : cuenta propia o familiar sin remuneracion (s04b_12 = 3, 6)
# Un enfoque distinto produce un resultado distinto: se miden los tres
# sobre la misma poblacion y se cuantifica su concordancia.
# ---------------------------------------------------------------------
df <- marcar_vulnerable(df)
est <- df |> filter(pob_estudio)
tasas_enfoque <- bind_rows(lapply(names(ENFOQUES), function(k) {
  x <- est[[ENFOQUES[[k]]]]; ic <- ic_prop(as.numeric(x == "Vulnerable"), est$w)
  data.frame(enfoque = k, nombre = NOMBRE_ENF[[k]], tasa = ic[["est"]], li = ic[["li"]], ls = ic[["ls"]],
             n_muestra = sum(!is.na(x)), vulnerables_muestra = sum(x == "Vulnerable", na.rm = TRUE),
             poblacion_vulnerable = sum(est$w[x %in% "Vulnerable"]), poblacion = sum(est$w[!is.na(x)]))
}), data.frame(enfoque = "compuesto", nombre = "Compuesto v1 (horas < 40 o A o B)",
               tasa = wmean(est$vul_compuesto == "Vulnerable", est$w), li = NA, ls = NA, n_muestra = sum(!is.na(est$vul_compuesto)),
               vulnerables_muestra = sum(est$vul_compuesto == "Vulnerable", na.rm = TRUE),
               poblacion_vulnerable = sum(est$w[est$vul_compuesto %in% "Vulnerable"]), poblacion = sum(est$w)))
print(tasas_enfoque)
guardar_tabla(tasas_enfoque, "tasas_por_enfoque")
cuadrante <- est |> filter(!is.na(cuadrante)) |> count(cuadrante, wt = w) |> mutate(p = n / sum(n))
cruce_abc <- est |> filter(!is.na(vul_persona), !is.na(vul_hogar), !is.na(vul_oit)) |>
  count(A = vul_persona, B = vul_hogar, C = vul_oit, wt = w) |> mutate(p = n / sum(n))
guardar_tabla(cruce_abc, "cruce_abc")
guardar_tabla(cuadrante, "cuadrante_persona_hogar")
kappa_w <- function(a, b, w) { ok <- !is.na(a) & !is.na(b); a <- a[ok] == "Vulnerable"; b <- b[ok] == "Vulnerable"; w <- w[ok]
  po <- sum(w * (a == b)) / sum(w); pa <- sum(w * a) / sum(w); pb <- sum(w * b) / sum(w); pe <- pa * pb + (1 - pa) * (1 - pb)
  c(acuerdo = po, kappa = (po - pe) / (1 - pe)) }
concordancia <- bind_rows(lapply(list(c("persona", "hogar"), c("persona", "oit"), c("hogar", "oit")), function(pr) {
  k <- kappa_w(est[[ENFOQUES[[pr[1]]]]], est[[ENFOQUES[[pr[2]]]]], est$w)
  data.frame(enfoque_1 = pr[1], enfoque_2 = pr[2], acuerdo = k[["acuerdo"]], kappa = k[["kappa"]]) }))
print(concordancia)
guardar_tabla(concordancia, "concordancia_enfoques")
componentes <- est |> mutate(combinacion = paste0(ifelse(crit_horas %in% TRUE, "H", "-"), ifelse(crit_ingreso %in% TRUE, "I", "-"), ifelse(crit_pobreza %in% TRUE, "P", "-"))) |>
  count(combinacion, wt = w) |> mutate(p = n / sum(n))
guardar_tabla(componentes, "combinaciones_criterios")
tasa_nac <- tasas_enfoque$tasa[tasas_enfoque$enfoque == "persona"]
bitacora(c("ylab", "p0", "s04b_12", "condact", "edad"), "Construccion de las variables objetivo", "Tres enfoques sobre la misma poblacion (ocupados de 19+)",
         "Nuevas columnas vul_persona, vul_hogar, vul_oit, vul_compuesto, cuadrante (vulnerable = vul_persona)",
         sprintf("A ylab < %d Bs (SMN 2024); B p0 = 1 (linea oficial del INE); C s04b_12 en {3, 6} (OIT)", CFG$smn),
         sum(df$pob_estudio), paste(sprintf("%s %.1f%%", tasas_enfoque$enfoque, 100 * tasas_enfoque$tasa), collapse = " | "),
         df, "marcar_vulnerable(df)", "F08_tasas_por_enfoque.csv")
bitacora(c("vul_persona", "vul_hogar", "vul_oit"), "Concordancia entre enfoques", "Kappa de Cohen ponderado por factor",
         paste(sprintf("%s-%s kappa %.2f (acuerdo %.0f%%)", concordancia$enfoque_1, concordancia$enfoque_2, concordancia$kappa, 100 * concordancia$acuerdo), collapse = " | "),
         "Kappa < 0.4: los enfoques identifican a personas distintas", NA,
         paste(sprintf("%s %.1f%%", cuadrante$cuadrante, 100 * cuadrante$p), collapse = " | "), codigo = "kappa_w()", archivo = "F08_concordancia_enfoques.csv")

# ---------------------------------------------------------------------
# 8.1b Variables del empleo (seccion 4): calidad y formalidad
# ---------------------------------------------------------------------
CAT_OCUP <- c(`1` = "Obrero o empleado", `2` = "Empleador con salario", `3` = "Cuenta propia", `4` = "Empleador sin salario",
              `5` = "Cooperativista", `6` = "Familiar sin remuneracion", `7` = "Aprendiz sin remuneracion", `8` = "Trabajadora del hogar")
SECTOR <- c(`1` = "Administracion publica", `2` = "Empresa publica", `3` = "Privada mediana o grande", `4` = "Privada familiar o micro",
            `5` = "ONG", `6` = "Organismo internacional")
num_or_na <- function(v) if (v %in% names(df)) cod_num(df[[v]]) else rep(NA_real_, nrow(df))
co <- num_or_na("s04b_12")
df$cat_ocupacional <- factor(unname(CAT_OCUP[as.character(co)]), levels = CAT_OCUP)
sec <- num_or_na("s04b_13")
df$sector <- factor(ifelse(co %in% c(3, 6), "Independiente o familiar", unname(SECTOR[as.character(sec)])),
                    levels = c(SECTOR, "Independiente o familiar"))
tam <- num_or_na("s04b_14")
df$tamano_empresa <- cut(tam, c(0, 1, 5, 20, 100, Inf), labels = c("1 (solo)", "2-5", "6-20", "21-100", "Mas de 100"))
afil <- num_or_na("s04f_35"); aporta <- num_or_na("s04f_36")
df$informal <- ifelse(df$condicion %in% "Ocupado", !(afil %in% 1 & aporta %in% 1), NA)
segs <- num_or_na("s04c_20a_2")
df$seguro_salud_trabajo <- ifelse(df$condicion %in% "Ocupado", segs %in% 1, NA)
ant_t <- num_or_na("s04b_11aa"); ant_p <- num_or_na("s04b_11ab")
df$antiguedad_anios <- ifelse(df$condicion %in% "Ocupado", ant_t / c(`2` = 52, `4` = 12, `8` = 1)[as.character(ant_p)], NA)
df$grupo_ocup <- factor(etq_cob(df$cob_op), levels = GRUPOS_COB)
df$rama <- etq_rama(df$caeb_op)
df$ingreso_hora <- ifelse(df$condicion %in% "Ocupado" & df$tothrs > 0, df$ylab / (df$tothrs * 4.33), NA)
hog <- df |> group_by(folio) |> summarise(miembros = n(), ocupados_hogar = sum(condicion %in% "Ocupado"),
                                          menores_14 = sum(edad < 14, na.rm = TRUE), mayores_60 = sum(edad >= 60, na.rm = TRUE))
df <- df |> left_join(hog, by = "folio") |> mutate(carga_hogar = miembros / pmax(ocupados_hogar, 1))
emp <- df |> filter(pob_estudio)
tasa_informal <- wmean(emp$informal, emp$w)
resumen_empleo <- bind_rows(
  emp |> filter(!is.na(cat_ocupacional)) |> group_by(variable = "cat_ocupacional", categoria = as.character(cat_ocupacional)) |>
    summarise(poblacion = sum(w), ylab_mediana = wquant(ylab, w, 0.5), tasa_persona = tasa_w(vul_persona, w), tasa_hogar = tasa_w(vul_hogar, w), informal = wmean(informal, w), .groups = "drop"),
  emp |> filter(!is.na(sector)) |> group_by(variable = "sector", categoria = as.character(sector)) |>
    summarise(poblacion = sum(w), ylab_mediana = wquant(ylab, w, 0.5), tasa_persona = tasa_w(vul_persona, w), tasa_hogar = tasa_w(vul_hogar, w), informal = wmean(informal, w), .groups = "drop"),
  emp |> filter(!is.na(tamano_empresa)) |> group_by(variable = "tamano_empresa", categoria = as.character(tamano_empresa)) |>
    summarise(poblacion = sum(w), ylab_mediana = wquant(ylab, w, 0.5), tasa_persona = tasa_w(vul_persona, w), tasa_hogar = tasa_w(vul_hogar, w), informal = wmean(informal, w), .groups = "drop")) |>
  group_by(variable) |> mutate(pct_ocupados = poblacion / sum(poblacion)) |> ungroup()
print(resumen_empleo |> mutate(across(where(is.numeric), ~ round(.x, 3))))
guardar_tabla(resumen_empleo, "resumen_variables_empleo")
bitacora(c("s04b_12", "s04b_13", "s04b_14", "s04f_35", "s04f_36", "s04c_20a_2", "s04b_11aa", "cob_op", "caeb_op"), "Variables de empleo dispersas en codigos",
         "Etiquetado con el DDI del INE y construccion de indicadores de calidad del empleo",
         "Nuevas columnas cat_ocupacional, sector, tamano_empresa, informal, seguro_salud_trabajo, antiguedad_anios, grupo_ocup, rama, ingreso_hora, miembros, ocupados_hogar, carga_hogar",
         "Informal = no aporta a la Gestora (AFP) en su ocupacion; carga del hogar = miembros por ocupado", nrow(df),
         sprintf("Informalidad (no aporta a la Gestora) en ocupados de 19+: %.1f%%", 100 * tasa_informal), df,
         "etiquetas DDI; group_by(folio)", "F08_resumen_variables_empleo.csv")

# ---------------------------------------------------------------------
# 8.2 Discretizacion
# ---------------------------------------------------------------------
# a) Manual por criterio de dominio: nivel educativo (ya construido)
# b) No supervisada: k-means sobre anios de estudio (19+), metodo del codo
a19 <- df$aestudio[!is.na(df$edad) & df$edad >= CFG$edad_min & !is.na(df$aestudio) & !df$dup_repetido]
codo <- data.frame(k = 1:8, wss = sapply(1:8, function(k) { set.seed(CFG$semilla); kmeans(a19, k, nstart = 10)$tot.withinss }))
set.seed(CFG$semilla); km_e <- kmeans(a19, 3, nstart = 25)
orden <- order(km_e$centers[, 1]); etq_km <- c("Bajo", "Medio", "Alto")
centros <- sort(km_e$centers[, 1]); cortes_km <- c(-Inf, (centros[-1] + centros[-3]) / 2, Inf)
df$educ_kmeans <- factor(ifelse(!is.na(df$edad) & df$edad >= CFG$edad_min, as.character(cut(df$aestudio, cortes_km, labels = etq_km)), NA), levels = etq_km)
cat("Centros k-means (anios de estudio):", round(centros, 1), "| cortes:", round(cortes_km[2:3], 1), "\n")

# c) Igual ancho vs igual frecuencia sobre el ingreso per capita
x <- df$yhogpc_w; x[df$dup_repetido] <- NA
reglas_bins <- c(Configurado = 5, Sturges = nclass.Sturges(na.omit(x)), `Freedman-Diaconis` = nclass.FD(na.omit(x)))
df$yhogpc_ancho <- cut(x, 5, dig.lab = 6)
qw <- unique(wquant(x, df$w, seq(0, 1, 0.2)))
df$quintil <- factor(paste0("Q", cut(x, qw, include.lowest = TRUE, labels = FALSE)), levels = paste0("Q", 1:5))
df$quintil[is.na(x)] <- NA
print(reglas_bins)

# d) Discretizaciones de dominio adicionales
df$jornada <- cut(df$tothrs, c(-Inf, 0, 39.99, 48, Inf), labels = c("Sin horas", "Subempleo <40h", "Normal 40-48h", "Sobrecarga >48h"))
df$edad_grupo <- cut(df$edad, c(-Inf, 13, 18, 24, 34, 44, 59, Inf), labels = c("0-13", "14-18", "19-24", "25-34", "35-44", "45-59", "60+"))
anio_nac <- CFG$anio_eh - df$edad
df$generacion <- cut(anio_nac, c(-Inf, 1945, 1964, 1980, 1996, 2012, Inf),
                     labels = c("Silenciosa (<1946)", "Baby boomers (1946-64)", "X (1965-80)", "Millennials (1981-96)", "Z (1997-2012)", "Alfa (2013+)"))
df$peso_nolab <- ifelse(df$yper > 0, df$ynolab / df$yper, NA)
df$dependencia_nolab <- cut(df$peso_nolab, c(-Inf, 0, 0.25, 0.5, Inf), labels = c("Ninguna", "Baja (<25%)", "Media (25-50%)", "Alta (>50%)"))
bitacora(c("aestudio", "yhogpc", "tothrs", "edad", "ynolab"), "Variables continuas dificiles de interpretar",
         "Discretizacion manual (dominio), k-means (no supervisada), igual ancho e igual frecuencia (Sturges y Freedman-Diaconis)",
         "Nuevas columnas nivel_educ, educ_kmeans, yhogpc_ancho, quintil, jornada, edad_grupo, generacion, dependencia_nolab",
         sprintf("Sturges sugiere %d bins y Freedman-Diaconis %d; se usan 5 para quintiles ponderados", reglas_bins[2], reglas_bins[3]),
         nrow(df), "8 variables categoricas nuevas", df, "cut(); kmeans(); wquant()")

# ---------------------------------------------------------------------
# 8.3 One-hot (k columnas) y dummy (k-1 columnas, con categoria base)
# One-hot: algoritmos de distancia (KNN, k-means). Dummy: modelos
# lineales, donde k columnas mas el intercepto son colineales.
# ---------------------------------------------------------------------
codificar <- function(d, vars, drop_first) {
  out <- d[c("folio", "nro")]
  for (v in vars) { f <- droplevels(as.factor(d[[v]])); niv <- levels(f); if (drop_first) niv <- niv[-1]
    for (l in niv) out[[paste0(v, "_", make.names(l))]] <- ifelse(is.na(f), NA_integer_, as.integer(f == l)) }
  out
}
vars_cat <- c("departamento", "zona", "sexo", "nivel_educ", "jornada", "quintil", "edad_grupo")
df_onehot <- codificar(df, vars_cat, FALSE)
df_dummy  <- codificar(df, vars_cat, TRUE)
cols_cod <- data.frame(variable = vars_cat, categorias = sapply(vars_cat, function(v) nlevels(droplevels(as.factor(df[[v]]))))) |>
  mutate(onehot = categorias, dummy = categorias - 1, base = sapply(vars_cat, function(v) levels(droplevels(as.factor(df[[v]])))[1]))
print(cols_cod)
guardar_tabla(cols_cod, "columnas_codificacion")
bitacora(vars_cat, "Variables categoricas no utilizables por modelos numericos", "One-hot encoding (k) y variables dummy (k-1)",
         sprintf("Matrices separadas: one-hot %d columnas, dummy %d columnas", ncol(df_onehot) - 2, ncol(df_dummy) - 2),
         paste("Categorias base:", paste(cols_cod$variable, cols_cod$base, sep = "=", collapse = ", ")), nrow(df),
         "df no cambia; las matrices se exportan aparte", df_onehot, "codificar(df, vars_cat, drop_first)")

# Trampa de la variable dummy: k columnas + intercepto = colinealidad perfecta
trampa_d <- bind_cols(df["ylab"], df_onehot |> select(starts_with("departamento_")))[df$pob_estudio & df$ylab %in% df$ylab[df$ylab > 0], ]
m_trampa <- lm(log(ylab) ~ ., data = trampa_d)
m_ok     <- lm(log(ylab) ~ ., data = trampa_d |> select(-2))
trampa <- data.frame(termino = names(coef(m_trampa)), con_k_columnas = coef(m_trampa)) |>
  left_join(data.frame(termino = names(coef(m_ok)), con_k_menos_1 = coef(m_ok)), by = "termino")
print(trampa)
guardar_tabla(trampa, "trampa_dummy")
bitacora("departamento", "Multicolinealidad perfecta", "lm con k columnas one-hot vs k-1 dummy",
         sprintf("Con k columnas %d coeficiente(s) quedan sin estimar (NA)", sum(is.na(coef(m_trampa)))),
         "Por eso en regresion se usa dummy (k-1) con categoria base", NA, "Demostracion numerica", codigo = "lm(log(ylab) ~ .)", archivo = "F08_trampa_dummy.csv")

# Regresion con dummies: diferencia porcentual frente a la categoria base
reg_d <- bind_cols(ly = log(df$ylab), df_dummy |> select(starts_with(c("sexo_", "zona_", "nivel_educ_", "departamento_")))) |>
  filter(df$pob_estudio, is.finite(ly))
m_dummy <- lm(ly ~ ., data = reg_d)
coef_dummy <- as.data.frame(summary(m_dummy)$coefficients) |> tibble::rownames_to_column("termino") |> setNames(c("termino", "b", "ee", "t", "p")) |>
  filter(termino != "(Intercept)") |> mutate(dif_pct = 100 * (exp(b) - 1))
guardar_tabla(coef_dummy, "regresion_dummies")
bitacora(c("sexo", "zona", "nivel_educ", "departamento"), "Interpretacion de variables categoricas", "Regresion log-lineal con dummies k-1",
         sprintf("R2 ajustado = %.3f", summary(m_dummy)$adj.r.squared), "Coeficiente b -> diferencia porcentual 100 x (exp(b) - 1) frente a la base",
         nrow(reg_d), "", codigo = "lm(log(ylab) ~ dummies)", archivo = "F08_regresion_dummies.csv")

# ---------------------------------------------------------------------
# 8.4 Indice de Vulnerabilidad Laboral (IVL) por departamento, un indice
# por enfoque. Componentes min-max: desempleo, subempleo, informalidad,
# baja educacion y la tasa del enfoque (A persona o B hogar).
# ---------------------------------------------------------------------
vul      <- calc_ivl(df, departamento, enfoque = "persona")
vul_h    <- calc_ivl(df, departamento, enfoque = "hogar")
vul_area <- bind_rows(calc_ivl(df, departamento, zona, enfoque = "persona"), calc_ivl(df, departamento, zona, enfoque = "hogar"))
ivl_comp <- bind_rows(vul, vul_h)
print(ivl_comp |> select(enfoque, departamento, ivl, rango, tasa_enfoque, informalidad) |> arrange(enfoque, rango) |> mutate(across(where(is.numeric), ~ round(.x, 3))))
guardar_tabla(vul, "ivl_departamento"); guardar_tabla(vul_h, "ivl_departamento_hogar")
guardar_tabla(ivl_comp, "ivl_departamento_enfoques"); guardar_tabla(vul_area, "ivl_departamento_area")
bitacora(c("condact", "tothrs", "s04f_36", "aestudio", "vul_persona", "vul_hogar"), "Construccion de indicador compuesto",
         "IVL = promedio de 5 componentes escalados min-max (desempleo, subempleo, informalidad, baja educacion, tasa del enfoque); uno por enfoque",
         "Indice 0-100 por departamento para A persona y B hogar", "Ponderado por factor; mayor IVL = mas vulnerable", 18,
         paste(sprintf("%s: A %.0f / B %.0f", vul$departamento, vul$ivl, vul_h$ivl[match(vul$departamento, vul_h$departamento)]), collapse = "; "),
         codigo = "calc_ivl(df, departamento, enfoque = ...)", archivo = "F08_ivl_departamento_enfoques.csv")

# ---------------------------------------------------------------------
# 8.5 Fuentes mixtas: cuantitativa individual + cualitativa territorial
#     + cualitativa derivada (ejemplo 4 de clase)
# ---------------------------------------------------------------------
regiones <- data.frame(departamento = c("La Paz", "Oruro", "Potosi", "Cochabamba", "Chuquisaca", "Tarija", "Santa Cruz", "Beni", "Pando"),
                       region = c("Altiplano", "Altiplano", "Altiplano", "Valles", "Valles", "Valles", "Llanos", "Llanos", "Llanos"))
nivel_dep <- vul |> transmute(departamento = as.character(departamento), ivl_depto = ivl, nivel_ivl = nivel)
calidad <- function(ylab, tothrs) factor(case_when(
  is.na(ylab) | is.na(tothrs) ~ NA_character_,
  ylab >= 2 * CFG$smn & tothrs >= 40 & tothrs <= 48 ~ "Buena",
  ylab >= CFG$smn & tothrs <= 60 ~ "Aceptable",
  ylab >= CFG$smn ~ "Exigente",
  TRUE ~ "Precaria"), levels = c("Precaria", "Exigente", "Aceptable", "Buena"))

# merge() como en el ejemplo de clase, sobre la poblacion de estudio
ocupados <- df |> filter(pob_estudio) |> mutate(departamento = as.character(departamento))
antes_cols <- ncol(ocupados)
mixto <- ocupados |> merge(regiones, by = "departamento", all.x = TRUE) |> merge(nivel_dep, by = "departamento", all.x = TRUE) |>
  mutate(region = factor(region, levels = c("Altiplano", "Valles", "Llanos")), calidad_empleo = calidad(ylab, tothrs),
         departamento = factor(departamento, levels = DEPTOS))
# Las mismas columnas se agregan a df con left_join (no altera el orden de filas)
df <- df |> mutate(departamento = as.character(departamento)) |> left_join(regiones, by = "departamento") |> left_join(nivel_dep, by = "departamento") |>
  mutate(departamento = factor(departamento, levels = DEPTOS), region = factor(region, levels = c("Altiplano", "Valles", "Llanos")),
         calidad_empleo = ifelse(pob_estudio, as.character(calidad(ylab, tothrs)), NA), calidad_empleo = factor(calidad_empleo, levels = c("Precaria", "Exigente", "Aceptable", "Buena")))
stopifnot(nrow(df) == nrow(raw))
cat("Integracion: de", antes_cols, "a", ncol(mixto), "columnas | sin region asignada:", sum(is.na(mixto$region)), "\n")
print(table(mixto$region, mixto$calidad_empleo, useNA = "ifany"))
bitacora(c("departamento", "region", "nivel_ivl", "calidad_empleo"), "Fuentes de distinta naturaleza", "Integracion por llave comun (merge por departamento) y triangulacion",
         sprintf("Fuente A: EH (cuantitativa individual); B: region geografica (cualitativa territorial); C: nivel IVL (cualitativa derivada); nueva variable cualitativa calidad_empleo. %d -> %d columnas", antes_cols, ncol(mixto)),
         "Union por departamento; en df se usa left_join para no alterar el orden", nrow(mixto), "Base mixta exportada en F12", mixto, "merge(regiones, by = 'departamento')")

# ---------------------------------------------------------------------
# Graficos
# ---------------------------------------------------------------------
p <- ggplot(codo, aes(k, wss)) + geom_line(color = PAL$violet, linewidth = 1.2) + geom_point(aes(color = k == 3), size = 4.5) +
  scale_color_manual(values = c(`TRUE` = PAL$amber, `FALSE` = PAL$violet), guide = "none") + scale_y_continuous(labels = abrev) +
  annotate("text", x = 3, y = codo$wss[3], label = "  k = 3 (codo)", hjust = 0, vjust = -1, color = PAL$amber) +
  labs(title = "Metodo del codo para discretizar anios de estudio", subtitle = "Suma de cuadrados dentro de grupos segun el numero de grupos", x = "k", y = "WSS", caption = CAP)
guardar(p, "codo_kmeans_educacion", 10, 6)

g <- df |> filter(!is.na(educ_kmeans), !is.na(nivel_educ), !dup_repetido) |> count(nivel_educ, educ_kmeans)
p <- ggplot(g, aes(educ_kmeans, nivel_educ, fill = n)) + geom_tile(color = PAL$bg, linewidth = 1.2) + geom_text(aes(label = fmt(n)), color = "white", fontface = "bold") +
  scale_fill_gradientn(colors = GRAD, guide = "none") +
  labs(title = "Discretizacion manual vs k-means", subtitle = sprintf("Centros k-means: %s anios | cortes en %s y %s", paste(round(centros, 1), collapse = ", "), round(cortes_km[2], 1), round(cortes_km[3], 1)),
       x = "Grupo k-means", y = "Nivel educativo (manual)", caption = CAP)
guardar(p, "manual_vs_kmeans", 10, 6)

p <- (ggplot(df |> filter(!is.na(yhogpc_ancho)), aes(yhogpc_ancho)) + geom_bar(fill = PAL$cyan, width = 0.7) +
        labs(title = "Igual ancho", x = NULL, y = "Personas") + theme(axis.text.x = element_text(angle = 30, hjust = 1))) +
  (ggplot(df |> filter(!is.na(quintil)), aes(quintil, weight = w)) + geom_bar(fill = PAL$violet, width = 0.7) + scale_y_continuous(labels = abrev) +
     labs(title = "Igual frecuencia (quintiles ponderados)", x = NULL, y = "Personas expandidas")) +
  plot_annotation(title = "Discretizacion del ingreso per capita", subtitle = sprintf("Igual ancho concentra casi todo en el primer bin | Sturges sugiere %d bins, Freedman-Diaconis %d", reglas_bins[2], reglas_bins[3]),
                  caption = CAP, theme = theme_eh())
guardar(p, "bins_ancho_vs_frecuencia", 12, 6)

g <- df |> filter(!is.na(quintil), !is.na(departamento)) |> count(departamento, quintil, wt = w) |> group_by(departamento) |> mutate(p = n / sum(n)) |> ungroup()
ordq <- g |> filter(quintil == "Q1") |> arrange(p) |> pull(departamento)
p <- ggplot(g |> mutate(departamento = factor(departamento, levels = ordq)), aes(p, departamento, fill = quintil)) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(p > 0.06, pct(p, 0), "")), position = position_stack(vjust = 0.5, reverse = TRUE), color = "white", size = 3) +
  scale_x_continuous(labels = percent) + scale_fill_manual(values = c(PAL$coral, PAL$pink, PAL$violet, PAL$indigo, PAL$cyan), name = "Quintil (Q1 = mas pobre)") +
  labs(title = "Quintiles de ingreso per capita por departamento", subtitle = "Ordenado por la proporcion en el quintil mas pobre", x = NULL, y = NULL, caption = CAP)
guardar(p, "quintiles_departamento")

g <- df |> filter(pob_estudio, !is.na(jornada), jornada != "Sin horas", !is.na(departamento), !is.na(zona)) |>
  count(departamento, zona, jornada, wt = w) |> group_by(departamento, zona) |> mutate(p = n / sum(n)) |> ungroup()
p <- ggplot(g, aes(p, departamento, fill = jornada)) + geom_col(width = 0.75, position = position_stack(reverse = TRUE)) + facet_wrap(~zona) +
  geom_text(aes(label = ifelse(p > 0.08, pct(p, 0), "")), position = position_stack(vjust = 0.5, reverse = TRUE), color = "white", size = 2.8) +
  scale_x_continuous(labels = percent) + scale_fill_manual(values = c(`Subempleo <40h` = PAL$coral, `Normal 40-48h` = PAL$mint, `Sobrecarga >48h` = PAL$amber), name = NULL) +
  labs(title = "Jornada laboral discretizada", subtitle = "Ocupados de 19+ por departamento y area", x = NULL, y = NULL, caption = CAP)
guardar(p, "jornada_depto_area", 12, 6.5)

g <- cols_cod |> select(variable, `One-hot (k)` = onehot, `Dummy (k-1)` = dummy) |> pivot_longer(-variable, names_to = "tipo", values_to = "cols")
p <- ggplot(g, aes(cols, fct_reorder(variable, cols), fill = tipo)) + geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = cols), position = position_dodge(0.7), hjust = -0.4, color = PAL$txt, size = 3.3) +
  scale_fill_manual(values = c(`One-hot (k)` = PAL$pink, `Dummy (k-1)` = PAL$indigo), name = NULL) +
  labs(title = "Columnas generadas por la codificacion", subtitle = sprintf("One-hot: %d columnas | Dummy: %d columnas", sum(cols_cod$onehot), sum(cols_cod$dummy)),
       x = "Columnas", y = NULL, caption = CAP)
guardar(p, "onehot_vs_dummy", 9, 6)

g <- trampa |> pivot_longer(-termino, names_to = "modelo", values_to = "b") |>
  mutate(modelo = recode(modelo, con_k_columnas = "One-hot: k columnas", con_k_menos_1 = "Dummy: k-1 columnas"),
         estado = ifelse(is.na(b), "No estimable (NA)", "Estimado"), b = ifelse(is.na(b), 0, b), termino = str_remove(termino, "departamento_"))
p <- ggplot(g, aes(b, termino, color = estado)) + geom_vline(xintercept = 0, color = PAL$sub) +
  geom_point(size = 4) + geom_text(data = g |> filter(estado != "Estimado"), aes(label = "NA: colinealidad perfecta"), hjust = -0.1, size = 3.2) +
  facet_wrap(~modelo) + scale_color_manual(values = c(Estimado = PAL$cyan, `No estimable (NA)` = PAL$coral), name = NULL) +
  labs(title = "La trampa de la variable dummy", subtitle = "Con las k columnas y el intercepto, R no puede estimar un coeficiente", x = "Coeficiente (log ingreso)", y = NULL, caption = CAP)
guardar(p, "trampa_dummy", 12, 6)

p <- coef_dummy |> mutate(termino = str_replace_all(termino, "_", " ")) |>
  ggplot(aes(dif_pct, fct_reorder(termino, dif_pct), color = dif_pct > 0)) + geom_vline(xintercept = 0, color = PAL$sub, linetype = "dashed") +
  geom_segment(aes(x = 0, xend = dif_pct, yend = fct_reorder(termino, dif_pct)), linewidth = 1.5, alpha = 0.6) + geom_point(size = 3.8) +
  geom_text(aes(label = sprintf("%+.0f%%", dif_pct)), hjust = ifelse(coef_dummy$dif_pct > 0, -0.4, 1.4), size = 3, color = PAL$txt) +
  scale_color_manual(values = c(`TRUE` = PAL$mint, `FALSE` = PAL$coral), guide = "none") + scale_x_continuous(expand = expansion(mult = 0.2)) +
  labs(title = "Regresion con variables dummy: ingreso laboral", subtitle = sprintf("Diferencia porcentual frente a la categoria base (%s) | R2 aj = %.3f",
       paste(cols_cod$base[cols_cod$variable %in% c("sexo", "zona", "nivel_educ", "departamento")], collapse = ", "), summary(m_dummy)$adj.r.squared),
       x = "% frente a la base", y = NULL, caption = CAP)
guardar(p, "regresion_dummies", 11, 8)

g <- mixto |> filter(!is.na(calidad_empleo), !is.na(region), !is.na(sexo)) |> count(region, sexo, calidad_empleo, wt = w) |>
  group_by(region, sexo) |> mutate(p = n / sum(n)) |> ungroup()
p <- ggplot(g, aes(p, sexo, fill = calidad_empleo)) + geom_col(position = position_stack(reverse = TRUE), width = 0.7) +
  geom_text(aes(label = ifelse(p > 0.07, pct(p, 0), "")), position = position_stack(vjust = 0.5, reverse = TRUE), color = "white", size = 3) +
  facet_wrap(~region, ncol = 1) + scale_x_continuous(labels = percent) +
  scale_fill_manual(values = c(Precaria = PAL$coral, Exigente = PAL$amber, Aceptable = PAL$indigo, Buena = PAL$mint), name = NULL) +
  labs(title = "Calidad del empleo por region y sexo", subtitle = "Variable cualitativa construida con ingreso y horas tras integrar la region", x = NULL, y = NULL, caption = CAP)
guardar(p, "calidad_empleo_region", 11, 8)

p <- mixto |> filter(!is.na(region), ylab > 0) |> ggplot(aes(ylab, region, fill = region)) + geom_violin(color = NA, alpha = 0.8) +
  geom_boxplot(width = 0.13, fill = PAL$bg, color = PAL$txt, outlier.color = PAL$coral, outlier.size = 0.5) +
  geom_vline(xintercept = CFG$smn, color = PAL$amber, linetype = "dashed") +
  scale_x_log10(labels = lab_bs) + scale_fill_manual(values = c(Altiplano = PAL$violet, Valles = PAL$cyan, Llanos = PAL$amber), guide = "none") +
  labs(title = "Ingreso laboral por region geografica", subtitle = "Dato cuantitativo individual agrupado por una clasificacion cualitativa externa | ambar = SMN",
       x = "Ingreso laboral (log)", y = NULL, caption = CAP)
guardar(p, "ingreso_por_region", 11, 6)

tipo_col <- function(d) c(cuanti = sum(sapply(d, is.numeric)), cuali = sum(sapply(d, function(x) is.factor(x) | is.character(x))))
g <- data.frame(momento = rep(c("Antes de integrar", "Despues de integrar"), each = 2), tipo = rep(c("Variables cuantitativas", "Variables cualitativas"), 2),
                n = c(tipo_col(ocupados), tipo_col(mixto)))
p <- ggplot(g, aes(n, fct_rev(fct_inorder(momento)), fill = tipo)) + geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_text(aes(label = n), position = position_dodge(0.7), hjust = -0.3, color = PAL$txt, size = 3.4) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) + scale_fill_manual(values = c(`Variables cuantitativas` = PAL$cyan, `Variables cualitativas` = PAL$pink), name = NULL) +
  labs(title = "Integracion de fuentes mixtas: antes y despues", subtitle = "La union por departamento agrega region, nivel IVL y calidad del empleo", x = "Columnas", y = NULL, caption = CAP)
guardar(p, "fuentes_mixtas", 11, 5)

rm(ocupados, est, reg_d, trampa_d)
checkpoint_guardar()
