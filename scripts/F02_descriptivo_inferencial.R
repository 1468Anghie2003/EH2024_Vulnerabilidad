# =====================================================================
#  F02 | ANALISIS DE LOS DATOS SIN TRATAR: DESCRIPTIVO E INFERENCIAL
#  CRISP-DM: Comprension de los datos
#  Tendencia central, dispersion, posicion, forma, visualizacion,
#  estimacion de parametros y pruebas de hipotesis.
#  Se hace ANTES de limpiar para poder comparar en F09.
# =====================================================================
if (!exists("CFG_OK")) source(file.path("scripts", "00_config.R"))
requiere(c("raw", "eda", "MAPA"), "F01")
fase_inicio("F02", "descriptivo_inferencial", "Estadistica descriptiva e inferencial (datos sin tratar)")

eda$nivel_educ <- nivel_educ(eda$aestudio)
vars_q <- intersect(VARS_CUANTI, names(eda))

# ---------------------------------------------------------------------
# 2.1 Medidas de tendencia central, dispersion, posicion y forma
# ---------------------------------------------------------------------
med0 <- medidas(eda, vars_q, "Sin tratar")
print(med0 |> mutate(across(where(is.numeric), ~ round(.x, 2))) |> select(variable, n, n_na, media, mediana, moda, desv_est, asimetria, curtosis))
guardar_tabla(med0, "medidas_resumen_sin_tratar")
bitacora(vars_q, "Analisis descriptivo", "Media, media ponderada, mediana, moda, rango, varianza, DE, CV, P10, Q1, Q3, P90, IQR, asimetria, curtosis",
         "Tabla de medidas sobre los datos sin tratar", "Linea base para comparar con el dataset limpio (F09)",
         NA, "Sin cambios", codigo = "medidas(eda, vars_q)", archivo = "F02_medidas_resumen_sin_tratar.csv")

# Deciles ponderados del ingreso laboral (medidas de posicion)
ocup0 <- eda |> filter(condicion %in% "Ocupado", ylab > 0)
deciles <- bind_rows(
  ocup0 |> group_by(grupo = "Bolivia") |> reframe(decil = paste0("D", 1:9), valor = wquant(ylab, w, (1:9) / 10)),
  ocup0 |> filter(!is.na(sexo)) |> group_by(grupo = as.character(sexo)) |> reframe(decil = paste0("D", 1:9), valor = wquant(ylab, w, (1:9) / 10)),
  ocup0 |> filter(!is.na(zona)) |> group_by(grupo = as.character(zona)) |> reframe(decil = paste0("D", 1:9), valor = wquant(ylab, w, (1:9) / 10)))
guardar_tabla(deciles |> pivot_wider(names_from = decil, values_from = valor), "deciles_ingreso_laboral")

# Tablas de frecuencia de variables cualitativas
frecuencias <- bind_rows(lapply(c("sexo", "zona", "departamento", "condicion", "nivel_educ", "pobre", "pobre_ext"), function(v)
  eda |> filter(!is.na(.data[[v]])) |> group_by(categoria = as.character(.data[[v]])) |>
    summarise(n = n(), poblacion = sum(w, na.rm = TRUE)) |>
    mutate(variable = v, pct = n / sum(n), pct_ponderado = poblacion / sum(poblacion), .before = 1)))
guardar_tabla(frecuencias, "frecuencias_cualitativas")
bitacora(c("sexo", "zona", "departamento", "condicion", "nivel_educ", "pobre"), "Analisis descriptivo",
         "Frecuencias absolutas, relativas y ponderadas por factor; deciles ponderados del ingreso",
         "Tablas de frecuencia y de posicion", "Todo indicador poblacional se pondera por factor", NA, "Sin cambios",
         codigo = "count(..., wt = w); wquant()", archivo = "F02_frecuencias_cualitativas.csv")

# Datos curiosos y valores extremos (se tratan en F06 y F07)
ocu <- eda |> filter(condicion %in% "Ocupado")
cat0 <- cod_num(ocu$s04b_12)
decada <- function(e) sprintf("entre %d y %d anios", 10 * floor(e / 10), 10 * floor(e / 10) + 9)
i_max <- which.max(ocu$ylab); top <- ocu[i_max, ]
med_y <- median(ocu$ylab[ocu$ylab > 0], na.rm = TRUE)
nh <- table(eda$folio)
n_vac <- sum(vapply(names(raw), function(v) sum(is.na(eda[[v]])), numeric(1)))   # incluye celdas solo con espacios
curiosidades <- tribble(
  ~clave, ~titulo, ~valor, ~unidad, ~detalle, ~se_trata_en,
  "horas_max", "La semana mas larga", max(eda$tothrs, na.rm = TRUE), "horas",
    sprintf("Una semana tiene 168 horas. %d personas declaran mas que eso: es un dato imposible.", sum(eda$tothrs > 168, na.rm = TRUE)), "F07",
  "horas_80", "Trabajan mas de 80 horas", sum(ocu$tothrs > 80, na.rm = TRUE), "personas",
    "Mas de 11 horas diarias, los 7 dias de la semana.", "F06",
  "ylab_max", "El ingreso mas alto", max(ocu$ylab, na.rm = TRUE), "Bs al mes",
    sprintf("%s, %s, %s, %s %s. Equivale a %.0f veces el ingreso tipico (mediana Bs %s).",
            etq_cob(top$cob_op), tolower(c("Obrero o empleado", "Empleador con salario", "Cuenta propia", "Empleador sin salario", "Cooperativista",
                                            "Familiar sin remuneracion", "Aprendiz sin remuneracion", "Trabajadora del hogar")[cod_num(top$s04b_12)]),
            top$departamento, tolower(as.character(top$sexo)), decada(top$edad), max(ocu$ylab, na.rm = TRUE) / med_y, fmt(med_y)), "F06",
  "ylab_min", "El ingreso mas bajo (mayor a 0)", min(ocu$ylab[ocu$ylab > 0], na.rm = TRUE), "Bs al mes",
    sprintf("%d ocupados declaran menos de Bs 100 al mes.", sum(ocu$ylab > 0 & ocu$ylab < 100, na.rm = TRUE)), "F06",
  "sin_pago", "Trabajan sin recibir pago", sum(cat0 %in% CFG$cat_sin_pago), "personas",
    "Familiares que ayudan en el negocio o aprendices: trabajan, pero su ingreso es 0 por definicion.", "F05",
  "joven", "El trabajador mas joven", min(ocu$edad, na.rm = TRUE), "anios",
    sprintf("%d ocupados tienen menos de 14 anios.", sum(ocu$edad < 14, na.rm = TRUE)), "F08",
  "mayor", "El trabajador de mayor edad", max(ocu$edad, na.rm = TRUE), "anios",
    sprintf("%d ocupados tienen 80 anios o mas.", sum(ocu$edad >= 80, na.rm = TRUE)), "F08",
  "hogar", "El hogar mas numeroso", max(nh), "personas",
    sprintf("El hogar tipico tiene %s personas.", median(nh)), "F04",
  "estudio", "Mas anios de estudio", max(eda$aestudio, na.rm = TRUE), "anios",
    sprintf("%d personas declaran mas anios de estudio de los posibles para su edad.", sum(eda$aestudio > eda$edad - 3 & eda$edad >= 4, na.rm = TRUE)), "F07",
  "vacias", "Celdas vacias", n_vac, "celdas",
    sprintf("El %.1f%% del archivo. La mayoria son preguntas que no corresponden a esa persona.", 100 * n_vac / (nrow(raw) * ncol(raw))), "F05")
print(curiosidades); guardar_tabla(curiosidades, "curiosidades")

# ---------------------------------------------------------------------
# 2.2 Visualizacion
# ---------------------------------------------------------------------
# G1 Histograma + poligono de frecuencias
g <- bind_rows(
  data.frame(var = "Edad (anios)", x = eda$edad), data.frame(var = "Anios de estudio", x = eda$aestudio),
  data.frame(var = "Horas semanales", x = eda$tothrs), data.frame(var = "log10 ingreso laboral", x = log10(eda$ylab[eda$ylab > 0]))) |>
  filter(!is.na(x), is.finite(x))
ref <- g |> group_by(var) |> summarise(media = mean(x), mediana = median(x))
p <- ggplot(g, aes(x)) +
  geom_histogram(aes(fill = after_stat(count)), bins = 30, color = PAL$bg) +
  geom_freqpoly(bins = 30, color = PAL$amber, linewidth = 0.8) +
  geom_vline(data = ref, aes(xintercept = media), color = PAL$cyan, linetype = "dashed") +
  geom_vline(data = ref, aes(xintercept = mediana), color = PAL$coral) +
  geom_label(data = ref, aes(x = media, y = Inf, label = sprintf("media %.1f\nmediana %.1f", media, mediana)),
             vjust = 1.1, hjust = -0.05, size = 3, fill = PAL$panel, color = PAL$txt, label.size = 0) +
  facet_wrap(~var, scales = "free") + scale_fill_gradientn(colors = GRAD, guide = "none") +
  labs(title = "Histogramas y poligonos de frecuencia", subtitle = "Linea punteada = media | linea continua = mediana | curva ambar = poligono",
       x = NULL, y = "Frecuencia", caption = CAP)
guardar(p, "histogramas_poligonos", 12, 8)

# G2 Boxplots para captar atipicos
g <- bind_rows(
  data.frame(var = "Edad", x = eda$edad), data.frame(var = "Anios de estudio", x = eda$aestudio),
  data.frame(var = "Horas semanales", x = eda$tothrs), data.frame(var = "Ingreso laboral (log10)", x = log10(eda$ylab[eda$ylab > 0])),
  data.frame(var = "Ingreso no laboral (log10)", x = log10(eda$ynolab[eda$ynolab > 0])),
  data.frame(var = "Ingreso per capita (log10)", x = log10(eda$yhogpc[eda$yhogpc > 0]))) |> filter(is.finite(x))
n_out <- g |> group_by(var) |> summarise(out = sum(x < quantile(x, .25) - 1.5 * IQR(x) | x > quantile(x, .75) + 1.5 * IQR(x)))
p <- ggplot(g, aes(x, var)) +
  geom_boxplot(fill = PAL$indigo, color = PAL$txt, alpha = 0.8, outlier.color = PAL$coral, outlier.size = 0.8, outlier.alpha = 0.5, width = 0.5) +
  geom_text(data = n_out, aes(x = Inf, y = var, label = paste(fmt(out), "atipicos")), hjust = 1.1, vjust = -1.6, color = PAL$amber, size = 3.2) +
  facet_wrap(~var, scales = "free", ncol = 2) +
  labs(title = "Diagramas de caja: deteccion visual de atipicos", subtitle = "Puntos coral = fuera de 1.5 x IQR | ingresos en escala log10",
       x = NULL, y = NULL, caption = CAP) + theme(axis.text.y = element_blank(), strip.text = element_text(hjust = 0))
guardar(p, "boxplots_atipicos", 12, 8)

# G3 Barras: poblacion por departamento (ponderada)
g <- eda |> filter(!is.na(departamento)) |> count(departamento, wt = w) |> mutate(p = n / sum(n))
p <- ggplot(g, aes(n, fct_reorder(departamento, n), fill = n)) + geom_col(width = 0.7) +
  geom_text(aes(label = paste0(abrev(n), "  (", pct(p), ")")), hjust = -0.1, color = PAL$txt, size = 3.4) +
  scale_fill_gradientn(colors = GRAD, guide = "none") + scale_x_continuous(labels = abrev, expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Poblacion expandida por departamento", subtitle = "Diagrama de barras ponderado por el factor de expansion",
       x = "Personas", y = NULL, caption = CAP)
guardar(p, "barras_departamento", 10, 6)

# G4 Barras agrupadas: condicion de actividad por sexo
g <- eda |> filter(!is.na(condicion), !is.na(sexo), condicion != "Fuera de PET") |> count(sexo, condicion, wt = w) |>
  group_by(sexo) |> mutate(p = n / sum(n))
p <- ggplot(g, aes(condicion, p, fill = sexo)) + geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = pct(p)), position = position_dodge(0.75), vjust = -0.5, color = PAL$txt, size = 3.4) +
  scale_fill_manual(values = COL_SEXO, name = NULL) + scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Condicion de actividad por sexo", subtitle = "Poblacion en edad de trabajar, ponderada", x = NULL, y = NULL, caption = CAP)
guardar(p, "barras_condicion_sexo", 10, 6)

# G5 Graficos de anillo (pastel): zona, sexo, pobreza
g <- bind_rows(
  eda |> filter(!is.na(zona)) |> count(cat = as.character(zona), wt = w) |> mutate(var = "Area"),
  eda |> filter(!is.na(sexo)) |> count(cat = as.character(sexo), wt = w) |> mutate(var = "Sexo"),
  eda |> filter(!is.na(pobre)) |> count(cat = ifelse(pobre == 1, "Pobre", "No pobre"), wt = w) |> mutate(var = "Pobreza moderada")) |>
  group_by(var) |> mutate(p = n / sum(n))
p <- ggplot(g, aes(x = 2, y = p, fill = cat)) + geom_col(color = PAL$bg, linewidth = 1.5, width = 1) +
  geom_text(aes(label = paste0(cat, "\n", pct(p))), position = position_stack(vjust = 0.5), color = "white", size = 3.4, fontface = "bold") +
  coord_polar(theta = "y") + xlim(0.5, 2.5) + facet_wrap(~var) +
  scale_fill_manual(values = c(Urbana = PAL$cyan, Rural = PAL$amber, Hombre = PAL$cyan, Mujer = PAL$pink, Pobre = PAL$coral, `No pobre` = PAL$indigo), guide = "none") +
  labs(title = "Composicion de la poblacion", subtitle = "Graficos de anillo ponderados por factor", caption = CAP) + sin_ejes
guardar(p, "anillos_composicion", 12, 5)

# G6 Piramide poblacional (corregida: edad y sexo verificados)
g <- eda |> filter(!is.na(sexo), !is.na(edad)) |>
  mutate(grupo = cut(edad, c(seq(0, 80, 5), Inf), right = FALSE, labels = c(paste0(seq(0, 75, 5), "-", seq(4, 79, 5)), "80+"))) |>
  count(grupo, sexo, wt = w) |> mutate(n = ifelse(sexo == "Hombre", -n, n))
p <- ggplot(g, aes(n, grupo, fill = sexo)) + geom_col(width = 0.85) +
  geom_vline(xintercept = 0, color = PAL$txt, linewidth = 0.4) +
  scale_x_continuous(labels = \(x) abrev(abs(x))) +
  scale_fill_manual(values = COL_SEXO, name = NULL) +
  labs(title = "Piramide poblacional", subtitle = paste("Edad =", MAPA$edad, "| Sexo =", MAPA$sexo, "(verificados en F01)"),
       x = "Personas (expandidas)", y = NULL, caption = CAP)
guardar(p, "piramide_poblacional", 9, 8)

# G7 Medidas de forma
g <- med0 |> select(variable, asimetria, curtosis) |> pivot_longer(-variable) |> filter(!is.na(value))
p <- ggplot(g, aes(value, fct_reorder(variable, value))) +
  geom_vline(xintercept = 0, color = PAL$sub, linetype = "dashed") +
  geom_segment(aes(x = 0, xend = value, yend = variable), color = PAL$grid, linewidth = 2) +
  geom_point(aes(color = value), size = 4.5) + geom_text(aes(label = round(value, 1)), vjust = -1.1, color = PAL$txt, size = 3) +
  facet_wrap(~name, scales = "free_x") + scale_color_gradientn(colors = GRAD, guide = "none") +
  labs(title = "Medidas de forma: asimetria y curtosis", subtitle = "Asimetria > 0: cola a la derecha | curtosis > 0: colas pesadas (exceso respecto a la normal)",
       x = NULL, y = NULL, caption = CAP)
guardar(p, "asimetria_curtosis", 12, 6)

# G8 Medidas de posicion: deciles del ingreso laboral
p <- deciles |> mutate(decil = factor(decil, levels = paste0("D", 1:9))) |>
  ggplot(aes(decil, valor, color = grupo, group = grupo)) + geom_line(linewidth = 1.2) + geom_point(size = 3) +
  geom_hline(yintercept = CFG$smn, color = PAL$amber, linetype = "dashed") +
  annotate("text", x = 1, y = CFG$smn, label = paste("Salario minimo", CFG$smn, "Bs"), vjust = -0.6, hjust = 0, color = PAL$amber, size = 3.3) +
  scale_y_log10(labels = label_number(prefix = "Bs ", big.mark = ",")) +
  scale_color_manual(values = c(Bolivia = PAL$txt, Hombre = PAL$cyan, Mujer = PAL$pink, Urbana = PAL$violet, Rural = PAL$amber), name = NULL) +
  labs(title = "Deciles del ingreso laboral", subtitle = "Ocupados, deciles ponderados | escala logaritmica", x = "Decil", y = "Ingreso mensual", caption = CAP)
guardar(p, "deciles_ingreso", 11, 6.5)

# G9 Ingreso laboral por departamento (ridgeline)
p <- ocup0 |> filter(!is.na(departamento)) |>
  ggplot(aes(ylab, fct_reorder(departamento, ylab, median), fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 2.2, rel_min_height = 0.01, color = PAL$txt, linewidth = 0.2, quantile_lines = TRUE, quantiles = 2) +
  geom_vline(xintercept = CFG$smn, color = PAL$amber, linetype = "dashed") +
  scale_x_log10(labels = lab_bs) +
  scale_fill_gradientn(colors = GRAD, guide = "none") +
  labs(title = "Distribucion del ingreso laboral por departamento", subtitle = "Linea blanca = mediana | linea ambar = salario minimo | escala log",
       x = "Ingreso laboral mensual", y = NULL, caption = CAP)
guardar(p, "ridgeline_ingreso_depto")

# G10 Matriz de correlacion (Spearman, triangulo inferior)
vc <- intersect(c("edad", "aestudio", "totper", "tothrs", "ylab", "ynolab", "yper", "yhog", "yhogpc", "p1", "pobre"), names(eda))
eda[vc] <- lapply(eda[vc], cod_num)
vc <- vc[vapply(vc, function(v) sum(!is.na(eda[[v]])) > 30 && isTRUE(sd(eda[[v]], na.rm = TRUE) > 0), logical(1))]
cm0 <- suppressWarnings(cor(eda[vc], use = "pairwise.complete.obs", method = "spearman"))
guardar_tabla(as.data.frame(cm0) |> tibble::rownames_to_column("variable"), "correlacion_spearman_sin_tratar")
g <- as.data.frame(as.table(cm0)) |> setNames(c("a", "b", "r")) |> filter(as.integer(a) >= as.integer(b))
p <- ggplot(g, aes(b, a, fill = r)) + geom_tile(color = PAL$bg, linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3, color = "white") +
  scale_fill_gradient2(low = PAL$cyan, mid = PAL$panel, high = PAL$pink, limits = c(-1, 1), name = "rho") +
  labs(title = "Matriz de correlacion de Spearman", subtitle = "Datos sin tratar | Spearman por la asimetria de los ingresos", x = NULL, y = NULL, caption = CAP) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right")
guardar(p, "matriz_correlacion", 9, 8)
bitacora(vc, "Analisis de relaciones", "Correlacion de Spearman por pares completos", "Matriz de correlacion",
         "Spearman: robusta a asimetria y atipicos", NA, "Sin cambios", codigo = "cor(method = 'spearman')",
         archivo = "F02_correlacion_spearman_sin_tratar.csv")

# G11 Horas semanales por sexo
p <- eda |> filter(condicion %in% "Ocupado", tothrs > 0, tothrs <= 120, !is.na(sexo)) |>
  ggplot(aes(tothrs, fill = sexo, color = sexo)) + geom_density(alpha = 0.35, linewidth = 0.8, adjust = 1.4) +
  geom_vline(xintercept = c(40, 48), linetype = "dashed", color = PAL$amber) +
  annotate("text", x = 40, y = Inf, label = "40 h ", hjust = 1, vjust = 2, color = PAL$amber) +
  annotate("text", x = 48, y = Inf, label = " 48 h", hjust = 0, vjust = 2, color = PAL$amber) +
  scale_fill_manual(values = COL_SEXO, name = NULL) + scale_color_manual(values = COL_SEXO, name = NULL) +
  labs(title = "Horas semanales trabajadas por sexo", subtitle = "Ocupados | por debajo de 40 h = subempleo por horas",
       x = "Horas", y = "Densidad", caption = CAP)
guardar(p, "horas_por_sexo")

# ---------------------------------------------------------------------
# 2.3 Estimacion de parametros: IC 95% con tamanio efectivo de Kish
# (el diseno complejo de la EH infla la varianza; n efectivo lo aproxima)
# ---------------------------------------------------------------------
estim <- bind_rows(
  ocup0 |> filter(!is.na(departamento)) |> group_by(dominio = as.character(departamento)) |>
    summarise(t(ic_media(ylab, w)) |> as.data.frame()) |> mutate(parametro = "Ingreso laboral medio (Bs)"),
  ocup0 |> summarise(t(ic_media(ylab, w)) |> as.data.frame()) |> mutate(dominio = "Bolivia", parametro = "Ingreso laboral medio (Bs)"),
  eda |> filter(!is.na(departamento), !is.na(pobre)) |> group_by(dominio = as.character(departamento)) |>
    summarise(t(ic_prop(pobre, w)) |> as.data.frame()) |> mutate(parametro = "Tasa de pobreza moderada"),
  eda |> filter(!is.na(pobre)) |> summarise(t(ic_prop(pobre, w)) |> as.data.frame()) |> mutate(dominio = "Bolivia", parametro = "Tasa de pobreza moderada"))
guardar_tabla(estim, "estimacion_intervalos")

# G12 Forest plot de estimaciones
forest <- function(d, tit, etq) {
  nac <- d$est[d$dominio == "Bolivia"]
  ggplot(d |> filter(dominio != "Bolivia"), aes(est, fct_reorder(dominio, est))) +
    geom_vline(xintercept = nac, color = PAL$amber, linetype = "dashed") +
    geom_errorbar(aes(xmin = li, xmax = ls), width = 0.3, color = PAL$sub, linewidth = 0.8) +
    geom_point(aes(color = est), size = 4) + scale_color_gradientn(colors = GRAD, guide = "none") +
    scale_x_continuous(labels = etq) + labs(title = tit, x = NULL, y = NULL)
}
p <- forest(estim |> filter(startsWith(parametro, "Ingreso")), "Ingreso laboral medio", label_number(prefix = "Bs ", big.mark = ",")) +
  forest(estim |> filter(startsWith(parametro, "Tasa")), "Tasa de pobreza moderada", percent) +
  plot_annotation(title = "Estimacion de parametros con intervalos de confianza del 95%",
                  subtitle = "Linea ambar = estimacion nacional | error estandar con tamanio efectivo de Kish", caption = CAP, theme = theme_eh())
guardar(p, "forest_intervalos", 13, 6)
bitacora(c("ylab", "p0"), "Inferencia: estimacion", "Media y proporcion ponderadas con IC 95% (n efectivo de Kish)",
         "Intervalos por departamento", "IC = estimacion +- 1.96 x EE", NA, "Sin cambios", codigo = "ic_media(); ic_prop()",
         archivo = "F02_estimacion_intervalos.csv")

# ---------------------------------------------------------------------
# 2.4 Pruebas de hipotesis: t de Student, ANOVA, chi-cuadrado,
#     correlacion y regresion
# ---------------------------------------------------------------------
pruebas0 <- pruebas_hipotesis(eda, "Sin tratar")
print(pruebas0 |> select(prueba, estadistico, p_valor, decision))
guardar_tabla(pruebas0, "pruebas_hipotesis")

# Regresion (ecuacion de Mincer): log(ylab) ~ educacion + experiencia + controles
mz0 <- ajustar_mincer(eda); mincer0 <- mz0$modelo; coef0 <- mz0$coef
print(coef0 |> mutate(across(where(is.numeric), ~ round(.x, 4))))
guardar_tabla(coef0, "regresion_mincer_sin_tratar")
cat(sprintf("Mincer: R2 ajustado = %.3f | retorno de un anio de estudio = %.1f%%\n", summary(mincer0)$adj.r.squared, coef0$efecto_pct[coef0$termino == "aestudio"]))
bitacora(c("ylab", "aestudio", "sexo", "departamento", "zona", "p0"), "Inferencia: pruebas de hipotesis",
         "t de Welch, ANOVA, Kruskal-Wallis, chi-cuadrado, correlacion de Pearson y Spearman, regresion lineal (Mincer)",
         sprintf("%d pruebas y un modelo de regresion (R2 aj = %.3f)", nrow(pruebas0), summary(mincer0)$adj.r.squared),
         "Nivel de significancia 0.05", NA, "Sin cambios", codigo = "pruebas_hipotesis(); lm(log(ylab) ~ ...)",
         archivo = "F02_pruebas_hipotesis.csv")

# G13 t de Student: violin del ingreso por sexo con el resultado en el grafico
tt0 <- pruebas0[1, ]
p <- eda |> filter(condicion %in% "Ocupado", ylab > 0, !is.na(sexo)) |>
  ggplot(aes(sexo, ylab, fill = sexo)) + geom_violin(color = NA, alpha = 0.85) +
  geom_boxplot(width = 0.12, fill = PAL$bg, color = PAL$txt, outlier.shape = NA) +
  stat_summary(fun = median, geom = "label", aes(label = paste("Mediana Bs", fmt(after_stat(y)))), fill = PAL$panel, color = PAL$txt, size = 3.2, vjust = -0.6) +
  scale_y_log10(labels = lab_bs) + scale_fill_manual(values = COL_SEXO, guide = "none") +
  labs(title = "Prueba t: ingreso laboral de hombres y mujeres",
       subtitle = sprintf("t = %.2f | p = %s | %s. %s", tt0$estadistico, format.pval(tt0$p_valor, 3), tt0$decision, tt0$interpretacion),
       x = NULL, y = "Ingreso laboral (log)", caption = CAP)
guardar(p, "prueba_t_ingreso_sexo", 10, 7)

# G14 ANOVA: ingreso por departamento
an0 <- pruebas0[2, ]; kw0 <- pruebas0[3, ]
p <- eda |> filter(condicion %in% "Ocupado", ylab > 0, !is.na(departamento)) |>
  ggplot(aes(fct_reorder(departamento, ylab, median), ylab, fill = departamento)) +
  geom_boxplot(color = PAL$txt, outlier.color = PAL$coral, outlier.size = 0.5, outlier.alpha = 0.4, alpha = 0.85) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = PAL$amber, color = PAL$bg) +
  scale_y_log10(labels = lab_bs) + scale_fill_manual(values = DISC, guide = "none") +
  labs(title = "ANOVA: ingreso laboral por departamento",
       subtitle = sprintf("F = %.2f, p = %s | Kruskal-Wallis H = %.1f, p = %s | rombo = media", an0$estadistico, format.pval(an0$p_valor, 3), kw0$estadistico, format.pval(kw0$p_valor, 3)),
       x = NULL, y = "Ingreso laboral (log)", caption = CAP) + theme(axis.text.x = element_text(angle = 25, hjust = 1))
guardar(p, "anova_ingreso_depto", 11, 7)

# G15 Chi-cuadrado: pobreza por area
cz0 <- pruebas0[4, ]
p <- eda |> filter(!is.na(zona), !is.na(pobre)) |> count(zona, pobre = ifelse(pobre == 1, "Pobre", "No pobre"), wt = w) |>
  group_by(zona) |> mutate(p = n / sum(n)) |>
  ggplot(aes(zona, p, fill = pobre)) + geom_col(width = 0.6, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = pct(p)), position = position_stack(vjust = 0.5, reverse = TRUE), color = "white", fontface = "bold") +
  scale_fill_manual(values = c(Pobre = PAL$coral, `No pobre` = PAL$indigo), name = NULL) + scale_y_continuous(labels = percent) +
  labs(title = "Chi-cuadrado: pobreza y area de residencia", subtitle = sprintf("X2 = %.1f, gl = %s, p = %s | %s", cz0$estadistico, cz0$gl, format.pval(cz0$p_valor, 3), cz0$decision),
       x = NULL, y = NULL, caption = CAP)
guardar(p, "chi2_pobreza_area", 9, 7)

# G16 Correlacion y regresion simple: educacion vs ingreso
cp0 <- pruebas0[6, ]
set.seed(CFG$semilla)
g <- eda |> filter(condicion %in% "Ocupado", ylab > 0, !is.na(aestudio)) |> slice_sample(n = 5000)
p <- ggplot(g, aes(aestudio, ylab)) +
  geom_jitter(aes(color = zona), width = 0.3, alpha = 0.35, size = 1.2) +
  geom_smooth(method = "lm", formula = y ~ x, color = PAL$amber, fill = PAL$amber, alpha = 0.2) +
  scale_y_log10(labels = lab_bs) + scale_color_manual(values = COL_ZONA, name = NULL) +
  labs(title = "Correlacion y regresion: educacion e ingreso", subtitle = sprintf("Pearson r = %.2f (p = %s) | Spearman rho = %.2f | recta = minimos cuadrados",
       cp0$estadistico, format.pval(cp0$p_valor, 3), pruebas0$estadistico[7]), x = "Anios de estudio", y = "Ingreso laboral (log)", caption = CAP)
guardar(p, "correlacion_educacion_ingreso")

# G17 Regresion multiple de Mincer: efecto porcentual de cada variable
p <- coef0 |> filter(termino != "(Intercept)", termino != "edad2") |>
  ggplot(aes(efecto_pct, fct_reorder(termino, efecto_pct))) + geom_vline(xintercept = 0, color = PAL$sub, linetype = "dashed") +
  geom_errorbar(aes(xmin = li, xmax = ls), width = 0.25, color = PAL$sub) + geom_point(aes(color = efecto_pct > 0), size = 4.5) +
  geom_text(aes(label = sprintf("%+.1f%%", efecto_pct)), vjust = -1.1, color = PAL$txt, size = 3.4) +
  scale_color_manual(values = c(`TRUE` = PAL$mint, `FALSE` = PAL$coral), guide = "none") +
  labs(title = "Regresion de Mincer: que explica el ingreso laboral", subtitle = sprintf("Efecto porcentual sobre el ingreso, IC 95%% | R2 ajustado = %.3f | datos sin tratar", summary(mincer0)$adj.r.squared),
       x = "Cambio porcentual en el ingreso", y = NULL, caption = CAP)
guardar(p, "regresion_mincer", 10, 6)

checkpoint_guardar()
