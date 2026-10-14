# =====================================================================
#  F09 | ANALISIS DEL DATASET LIMPIO: ANTES vs DESPUES
#  CRISP-DM: Preparacion de los datos (verificacion de calidad)
#  Se repiten las medidas, correlaciones y pruebas de F02 sobre el
#  dataset limpio y se cuantifica el efecto de la limpieza.
# =====================================================================
if (!exists("CFG_OK")) source(file.path("scripts", "00_config.R"))
requiere(c("df", "MAPA", "med0", "pruebas0", "cm0", "coef0"), "F08")
fase_inicio("F09", "descriptivo_limpio", "Analisis del dataset limpio: antes vs despues")

dl <- df |> filter(!dup_repetido)
vars_q <- intersect(VARS_CUANTI, names(dl))
med1 <- medidas(dl, vars_q, "Limpio")
comp_medidas <- med0 |> select(variable, n_na_antes = n_na, media_antes = media, desv_antes = desv_est, asim_antes = asimetria, curt_antes = curtosis, max_antes = maximo) |>
  left_join(med1 |> select(variable, n_na_despues = n_na, media_despues = media, desv_despues = desv_est, asim_despues = asimetria, curt_despues = curtosis, max_despues = maximo), by = "variable") |>
  mutate(cambio_media_pct = 100 * (media_despues / media_antes - 1), cambio_desv_pct = 100 * (desv_despues / desv_antes - 1))
print(comp_medidas |> mutate(across(where(is.numeric), ~ round(.x, 2))))
guardar_tabla(med1, "medidas_resumen_limpio"); guardar_tabla(comp_medidas, "comparacion_antes_despues")
bitacora(vars_q, "Verificacion de la limpieza", "Recalculo de medidas de tendencia, dispersion y forma", "Comparacion antes vs despues",
         "Una buena limpieza completa datos sin deformar la distribucion", NA,
         sprintf("NA en variables cuantitativas: %s -> %s", fmt(sum(comp_medidas$n_na_antes)), fmt(sum(comp_medidas$n_na_despues))),
         dl, "medidas(dl, vars_q, 'Limpio')", "F09_comparacion_antes_despues.csv")

# Histogramas antes (datos sin tratar) vs despues (limpio),
# con los mismos cortes, para el selector de variables de la pagina
VARS_LOG <- c(VARS_INGRESO, "factor")
hist_ad <- bind_rows(lapply(vars_q, function(v) {
  antes <- if (v == "edad") cod_num(raw[[MAPA$edad]]) else cod_num(raw[[v]])
  antes <- antes[!df$dup_repetido]; despues <- dl[[v]]
  esc <- v %in% VARS_LOG
  tr <- function(x) { x <- x[!is.na(x)]; if (esc) log10(x[x > 0]) else x }
  a <- tr(antes); b <- tr(despues); if (!length(a) || !length(b)) return(NULL)
  rg <- range(c(a, b)); br <- seq(rg[1], rg[2] + 1e-9, length.out = 41)
  data.frame(variable = v, escala = ifelse(esc, "log10", "lineal"), desde = head(br, -1), hasta = br[-1],
             antes = as.vector(table(cut(a, br, include.lowest = TRUE))), despues = as.vector(table(cut(b, br, include.lowest = TRUE))))
}))
guardar_tabla(hist_ad, "histogramas_antes_despues")

# Faltantes por variable de analisis, dentro de su dominio
dom <- list(ylab = dl$condicion %in% "Ocupado", tothrs = dl$condicion %in% "Ocupado", aestudio = !is.na(dl$edad) & dl$edad >= 4,
            niv_ed_g = !is.na(dl$edad) & dl$edad >= 4, edad = rep(TRUE, nrow(dl)), factor = rep(TRUE, nrow(dl)))
raw_nd <- raw[!df$dup_repetido, ]
na_comp <- bind_rows(lapply(names(dom), function(v) {
  antes <- if (v == "edad") cod_num(raw_nd[[MAPA$edad]]) else if (v == "ylab") dl$ylab_original else cod_num(raw_nd[[v]])
  despues <- if (v == "factor") dl$w else dl[[v]]
  data.frame(variable = v, Antes = mean(is.na(antes[dom[[v]]])), Despues = mean(is.na(despues[dom[[v]]])))
}))
guardar_tabla(na_comp, "faltantes_antes_despues")

# Correlaciones y pruebas sobre el dataset limpio
vc <- rownames(cm0)
cm1 <- suppressWarnings(cor(dl[vc], use = "pairwise.complete.obs", method = "spearman"))
guardar_tabla(as.data.frame(cm1) |> tibble::rownames_to_column("variable"), "correlacion_spearman_limpio")
pruebas1 <- pruebas_hipotesis(dl, "Limpio")
comp_pruebas <- bind_rows(pruebas0, pruebas1)
guardar_tabla(comp_pruebas, "pruebas_antes_despues")
mz1 <- ajustar_mincer(dl); coef1 <- mz1$coef
guardar_tabla(bind_rows(coef0 |> mutate(momento = "Sin tratar"), coef1 |> mutate(momento = "Limpio")), "mincer_antes_despues")
print(comp_pruebas |> select(momento, prueba, estadistico, p_valor, decision))
bitacora(c("ylab", "aestudio", "sexo", "departamento", "zona"), "Verificacion inferencial", "Mismas pruebas de F02 sobre datos limpios",
         sprintf("%d de %d decisiones se mantienen", sum(pruebas0$decision == pruebas1$decision), nrow(pruebas0)),
         "Si las conclusiones cambian, la limpieza altero la senal; si se mantienen, la refuerza", NA,
         sprintf("Retorno de un anio de estudio: %.1f%% -> %.1f%%", coef0$efecto_pct[coef0$termino == "aestudio"], coef1$efecto_pct[coef1$termino == "aestudio"]),
         codigo = "pruebas_hipotesis(dl, 'Limpio'); ajustar_mincer(dl)", archivo = "F09_pruebas_antes_despues.csv")

# Tablero de calidad de datos (completitud, validez, unicidad, consistencia)
# Consistencia: yhog = suma de yper del hogar y yper = ylab + ynolab
v_ini <- validacion0 |> filter(tipo == "Corregible (vuelve al ciclo)")
calidad_tab <- data.frame(
  dimension = c("Completitud (variables de analisis)", "Validez (reglas corregibles)", "Unicidad (llave folio + nro)", "Consistencia (edad = fecha de nacimiento)"),
  Antes = c(1 - mean(na_comp$Antes), 1 - sum(v_ini$violaciones) / nrow(df), 1 - sum(df$dup_repetido) / nrow(df),
            mean(abs(cod_num(raw[[MAPA$edad]]) - df$edad_calc) <= 1, na.rm = TRUE)),
  Despues = c(1 - mean(na_comp$Despues), 1 - viol_corregibles_final / nrow(df), 1,
              mean(abs(dl$edad - dl$edad_calc) <= 1, na.rm = TRUE)))
guardar_tabla(calidad_tab, "tablero_calidad")

# ---------------------------------------------------------------------
# Graficos
# ---------------------------------------------------------------------
g <- calidad_tab |> pivot_longer(-dimension, names_to = "momento", values_to = "valor")
p <- ggplot(g, aes(valor, dimension)) + geom_line(aes(group = dimension), color = PAL$grid, linewidth = 3) +
  geom_point(aes(color = momento), size = 6) + geom_text(aes(label = pct(valor), color = momento), vjust = -1.4, size = 3.4, show.legend = FALSE) +
  scale_x_continuous(labels = percent, limits = c(0, 1.05)) + scale_color_manual(values = c(Antes = PAL$coral, Despues = PAL$mint), name = NULL) +
  labs(title = "Tablero de calidad de datos", subtitle = "Cuatro dimensiones de calidad antes y despues del preprocesamiento | ninguna fila ni columna eliminada",
       x = NULL, y = NULL, caption = CAP)
guardar(p, "tablero_calidad", 11, 5.5)

g <- na_comp |> pivot_longer(-variable, names_to = "momento", values_to = "pct_na")
p <- ggplot(g, aes(pct_na, fct_reorder(variable, pct_na))) + geom_line(aes(group = variable), color = PAL$grid, linewidth = 3) +
  geom_point(aes(color = momento), size = 5) + geom_text(aes(label = pct(pct_na), color = momento), vjust = -1.2, size = 3.2, show.legend = FALSE) +
  scale_x_continuous(labels = percent) + scale_color_manual(values = c(Antes = PAL$coral, Despues = PAL$mint), name = NULL) +
  labs(title = "Faltantes dentro del dominio: antes vs despues", subtitle = "Solo la no respuesta se imputa; los NA estructurales se conservan", x = "% NA", y = NULL, caption = CAP)
guardar(p, "faltantes_antes_despues", 10, 5.5)

g <- bind_rows(
  data.frame(var = "Ingreso laboral (log10)", momento = "Antes", x = suppressWarnings(log10(cod_num(raw_nd$ylab)))),
  data.frame(var = "Ingreso laboral (log10)", momento = "Despues", x = log10(dl$ylab)),
  data.frame(var = "Horas semanales", momento = "Antes", x = cod_num(raw_nd$tothrs)),
  data.frame(var = "Horas semanales", momento = "Despues", x = dl$tothrs),
  data.frame(var = "Anios de estudio", momento = "Antes", x = cod_num(raw_nd$aestudio)),
  data.frame(var = "Anios de estudio", momento = "Despues", x = dl$aestudio)) |> filter(is.finite(x))
p <- ggplot(g, aes(x, fill = momento, color = momento)) + geom_density(alpha = 0.3, linewidth = 0.8, adjust = 1.3) + facet_wrap(~var, scales = "free") +
  scale_fill_manual(values = c(Antes = PAL$coral, Despues = PAL$cyan), name = NULL) + scale_color_manual(values = c(Antes = PAL$coral, Despues = PAL$cyan), name = NULL) +
  labs(title = "Distribuciones antes y despues de la limpieza", subtitle = "Las colas imposibles desaparecen y la forma central se conserva", x = NULL, y = "Densidad", caption = CAP)
guardar(p, "distribuciones_antes_despues", 13, 5.5)

g <- comp_medidas |> select(variable, Antes = desv_antes, Despues = desv_despues) |> pivot_longer(-variable, names_to = "momento") |>
  bind_rows(comp_medidas |> select(variable, Antes = media_antes, Despues = media_despues) |> pivot_longer(-variable, names_to = "momento") |> mutate(medida = "Media")) |>
  mutate(medida = ifelse(is.na(medida), "Desviacion estandar", medida)) |> group_by(variable, medida) |> mutate(rel = value / value[momento == "Antes"]) |>
  filter(momento == "Despues", is.finite(rel))
p <- ggplot(g, aes(rel - 1, fct_reorder(variable, abs(rel - 1)), fill = rel > 1)) + geom_col(width = 0.6) + facet_wrap(~medida) +
  geom_vline(xintercept = 0, color = PAL$sub) + geom_text(aes(label = sprintf("%+.1f%%", 100 * (rel - 1))), hjust = ifelse(g$rel > 1, -0.15, 1.15), color = PAL$txt, size = 3) +
  scale_x_continuous(labels = percent, expand = expansion(mult = 0.35)) + scale_fill_manual(values = c(`TRUE` = PAL$cyan, `FALSE` = PAL$coral), guide = "none") +
  labs(title = "Cuanto cambio cada variable con la limpieza", subtitle = "Variacion porcentual de la media y la desviacion estandar (limpio vs sin tratar)", x = NULL, y = NULL, caption = CAP)
guardar(p, "cambio_media_desviacion", 12, 6)

dif <- as.data.frame(as.table(cm1 - cm0)) |> setNames(c("a", "b", "d")) |> filter(as.integer(a) > as.integer(b))
p <- ggplot(dif, aes(b, a, fill = d)) + geom_tile(color = PAL$bg, linewidth = 1) + geom_text(aes(label = sprintf("%+.2f", d)), color = "white", size = 3) +
  scale_fill_gradient2(low = PAL$cyan, mid = PAL$panel, high = PAL$pink, name = "Cambio") +
  labs(title = "Cambio en las correlaciones tras la limpieza", subtitle = "rho limpio - rho sin tratar (Spearman)", x = NULL, y = NULL, caption = CAP) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right")
guardar(p, "cambio_correlaciones", 9, 8)

g <- as.data.frame(as.table(cm1)) |> setNames(c("a", "b", "r")) |> filter(as.integer(a) >= as.integer(b))
p <- ggplot(g, aes(b, a, fill = r)) + geom_tile(color = PAL$bg, linewidth = 1) + geom_text(aes(label = sprintf("%.2f", r)), size = 3, color = "white") +
  scale_fill_gradient2(low = PAL$cyan, mid = PAL$panel, high = PAL$pink, limits = c(-1, 1), name = "rho") +
  labs(title = "Matriz de correlacion de Spearman (dataset limpio)", x = NULL, y = NULL, caption = CAP) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1), legend.position = "right")
guardar(p, "matriz_correlacion_limpio", 9, 8)

g <- comp_pruebas |> mutate(etq = paste0(prueba, "\n", str_trunc(hipotesis_nula, 55)), lp = -log10(pmax(p_valor, 1e-300)))
p <- ggplot(g, aes(lp, fct_rev(fct_inorder(etq)), color = momento)) + geom_vline(xintercept = -log10(0.05), color = PAL$amber, linetype = "dashed") +
  geom_line(aes(group = etq), color = PAL$grid, linewidth = 2.5) + geom_point(size = 4.5) +
  scale_color_manual(values = c(`Sin tratar` = PAL$coral, Limpio = PAL$mint), name = NULL) + scale_x_continuous(trans = "log1p") +
  labs(title = "Pruebas de hipotesis antes y despues de limpiar", subtitle = "Eje: -log10(p) | a la derecha de la linea ambar se rechaza H0 al 5%",
       x = "-log10(p-valor)", y = NULL, caption = CAP) + theme(axis.text.y = element_text(size = 8))
guardar(p, "pruebas_antes_despues", 12, 7)

g <- bind_rows(coef0 |> mutate(momento = "Sin tratar"), coef1 |> mutate(momento = "Limpio")) |> filter(!termino %in% c("(Intercept)", "edad2"))
p <- ggplot(g, aes(efecto_pct, termino, color = momento)) + geom_vline(xintercept = 0, color = PAL$sub, linetype = "dashed") +
  geom_errorbar(aes(xmin = li, xmax = ls), width = 0.25, position = position_dodge(0.5)) + geom_point(size = 4, position = position_dodge(0.5)) +
  scale_color_manual(values = c(`Sin tratar` = PAL$coral, Limpio = PAL$mint), name = NULL) +
  labs(title = "Regresion de Mincer antes y despues de la limpieza", subtitle = sprintf("R2 ajustado: %.3f -> %.3f | efecto porcentual con IC 95%%", summary(mincer0)$adj.r.squared, mz1$r2),
       x = "Cambio porcentual en el ingreso", y = NULL, caption = CAP)
guardar(p, "mincer_antes_despues", 11, 6)

rm(raw_nd)
checkpoint_guardar()
