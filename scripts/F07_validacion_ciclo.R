# =====================================================================
#  F07 | VALIDACION DE CONSISTENCIA, ETIQUETACION Y CICLO DE LIMPIEZA
#  CRISP-DM: Preparacion de los datos | Bitacora paso 6
#  Reproduce el diagrama de clase: si la validacion encuentra valores
#  imposibles, se vuelven NA, regresan a Faltantes (4) y Outliers (5) y
#  se valida otra vez, hasta que no quede ninguna violacion corregible.
# =====================================================================
if (!exists("CFG_OK")) source(file.path("scripts", "00_config.R"))
requiere(c("df", "MAPA"), "F06")
fase_inicio("F07", "validacion_ciclo", "Validacion, etiquetacion y ciclo de limpieza")

# ---------------------------------------------------------------------
# 7.1 Etiquetacion: diccionario con etiqueta, unidad y rango valido
# La etiqueta define que valores son posibles (ejemplo 3 de clase)
# ---------------------------------------------------------------------
diccionario_etq <- tribble(
  ~variable,  ~etiqueta,                         ~unidad,       ~min_val, ~max_val, ~accion,
  "edad",     "Edad en anios cumplidos",          "anios",       0,        110,      "Reemplazar por edad calculada desde la fecha de nacimiento",
  "aestudio", "Anios de estudio aprobados",       "anios",       0,        25,       "NA y hot deck",
  "tothrs",   "Horas trabajadas por semana",      "horas",       0,        126,      "NA y hot deck",
  "ylab",     "Ingreso laboral mensual",          "bolivianos",  0,        200000,   "NA y hot deck",
  "ynolab",   "Ingreso no laboral mensual",       "bolivianos",  0,        200000,   "Solo marcar",
  "yhogpc",   "Ingreso per capita del hogar",     "bolivianos",  0,        200000,   "Solo marcar")
for (i in seq_len(nrow(diccionario_etq))) {
  v <- diccionario_etq$variable[i]
  attr(df[[v]], "label") <- paste0(diccionario_etq$etiqueta[i], " (", diccionario_etq$unidad[i], ")")
}
guardar_tabla(diccionario_etq, "diccionario_etiquetas")
print(sapply(diccionario_etq$variable, function(v) attr(df[[v]], "label")))
bitacora(diccionario_etq$variable, "Variables sin etiqueta ni rango declarado", "Etiquetacion hibrida: etiqueta manual + validacion automatica por rango",
         "Etiqueta adjunta como atributo label y rango valido por variable", "La etiqueta lleva la unidad y el rango; el rango guia la correccion",
         nrow(diccionario_etq), "Metadatos disponibles para validar", codigo = "attr(df[[v]], 'label') <- ...", archivo = "F07_diccionario_etiquetas.csv")

# ---------------------------------------------------------------------
# 7.2 Reglas de negocio
# Corregibles: valor imposible -> NA -> se reimputa (vuelve al paso 4)
# Solo marcar: inconsistencias de definicion del INE, no se tocan
# ---------------------------------------------------------------------
tol <- function(a, b) !is.na(a) & !is.na(b) & abs(a - b) > pmax(1, 0.01 * abs(b))
reglas_corregibles <- function(d) list(
  edad_fuera_rango     = !is.na(d$edad) & (d$edad < 0 | d$edad > 110),
  aestudio_fuera_rango = !is.na(d$aestudio) & (d$aestudio < 0 | d$aestudio > 25),
  aestudio_mayor_edad  = !is.na(d$aestudio) & !is.na(d$edad) & d$edad >= 4 & d$aestudio > d$edad - 3,
  horas_imposibles     = d$condicion %in% "Ocupado" & !is.na(d$tothrs) & (d$tothrs < 0 | d$tothrs > 126),
  ingreso_negativo     = !is.na(d$ylab) & d$ylab < 0,
  ingreso_fuera_rango  = !is.na(d$ylab) & d$ylab > 200000)
reglas_marcar <- function(d) list(
  yper_distinto_suma   = !d$imp_ylab & tol(rowSums(cbind(d$ylab_original, d$ynolab), na.rm = TRUE), d$yper),
  pext0_mayor_p0       = !is.na(d$pobre_ext) & !is.na(d$pobre) & d$pobre_ext > d$pobre,
  p0_vs_linea          = !is.na(d$pobre) & !is.na(d$yhogpc) & !is.na(d$z) & (d$pobre == 1) != (d$yhogpc < d$z),
  pet_menor_14         = cod_num(d$pet) %in% 1 & !is.na(d$edad) & d$edad < CFG$edad_pet,
  edad_vs_nacimiento   = !is.na(d$edad) & !is.na(d$edad_calc) & abs(d$edad - d$edad_calc) > 1,
  suma_horas           = !d$imp_tothrs & tol(rowSums(cbind(d$phrs, d$shrs), na.rm = TRUE), d$tothrs),
  ocupado_sin_horas    = d$condicion %in% "Ocupado" & d$tothrs %in% 0,
  no_ocupado_con_ylab  = !(d$condicion %in% "Ocupado") & !is.na(d$ylab_original) & d$ylab_original > 0,
  ynolab_fuera_rango   = !is.na(d$ynolab) & d$ynolab > 200000)

viol0 <- c(sapply(reglas_corregibles(df), sum), sapply(reglas_marcar(df), sum))
validacion0 <- data.frame(regla = names(viol0), violaciones = viol0,
                          tipo = ifelse(names(viol0) %in% names(reglas_corregibles(df)), "Corregible (vuelve al ciclo)", "Solo marcar (definicion INE)"))
print(validacion0)
guardar_tabla(validacion0, "validacion_inicial")
rmk <- reglas_marcar(df)
for (r in names(rmk)) df[[paste0("val_", r)]] <- rmk[[r]]
bitacora(validacion0$regla, "Inconsistencias logicas", "15 reglas de negocio (6 corregibles, 9 de marcado)",
         "Marcado en columnas val_*", "Lo imposible se corrige; lo que depende de la definicion del INE solo se marca",
         sum(validacion0$violaciones), paste(sprintf("%s=%d", validacion0$regla, validacion0$violaciones), collapse = "; "), df,
         "reglas_corregibles(df); reglas_marcar(df)", "F07_validacion_inicial.csv")

# ---------------------------------------------------------------------
# 7.3 Ciclo: Validacion -> Faltantes -> Outliers -> Validacion
# ---------------------------------------------------------------------
tothrs_pre <- df$tothrs
for (r in names(reglas_corregibles(df))) df[[paste0("val_", r)]] <- FALSE
historial <- data.frame()
iter <- 0
repeat {
  iter <- iter + 1
  rc <- reglas_corregibles(df); n <- sapply(rc, sum)
  historial <- rbind(historial, data.frame(iteracion = iter, regla = names(n), violaciones = n))
  cat(sprintf("  Ciclo %d: %d violaciones corregibles\n", iter, sum(n)))
  if (sum(n) == 0 || iter > CFG$max_iter) break
  for (r in names(rc)) df[[paste0("val_", r)]] <- df[[paste0("val_", r)]] | rc[[r]]
  # Paso 6 -> 4: lo imposible pasa a NA y se reimputa
  e <- rc$edad_fuera_rango; df$edad[e] <- df$edad_calc[e]
  a <- rc$aestudio_fuera_rango | rc$aestudio_mayor_edad; df$aestudio[a] <- NA
  h <- rc$horas_imposibles; df$tothrs[h] <- NA
  y <- rc$ingreso_negativo | rc$ingreso_fuera_rango; df$ylab[y] <- NA
  df$imp_aestudio <- df$imp_aestudio | a; df$imp_tothrs <- df$imp_tothrs | h; df$imp_ylab <- df$imp_ylab | y
  ed <- !is.na(df$edad) & df$edad >= 4
  df$aestudio[ed] <- hot_deck(df$aestudio[ed], paste(cut(df$edad[ed], c(3, 6, 9, 12, 15, 19, 29, Inf)), df$zona[ed]), 0, 25)
  oc <- df$condicion %in% "Ocupado"
  df$tothrs[oc] <- hot_deck(df$tothrs[oc], paste(df$departamento, df$zona, df$sexo)[oc], 0, 126)
  df$ylab[oc]   <- hot_deck(df$ylab[oc], paste(df$departamento, df$sexo, df$nivel_educ)[oc], 0, 200000)
  df$nivel_educ <- nivel_educ(df$aestudio)
  # Paso 4 -> 5: se recalculan marcas y winsorizado de los valores nuevos
  for (v in c("ylab", "tothrs")) {
    x <- base_out(df, v); L <- limites(x, VARS_OUT[[v]])
    df[[paste0("out_", v)]] <- !is.na(x) & (x < L[1] | x > L[2])
    df[[paste0(v, "_w")]] <- ifelse(!is.na(df[[v]]) & df[[v]] > 0, pmin(pmax(df[[v]], L[1]), L[2]), df[[v]])
  }
  bitacora(names(n)[n > 0], "Valores imposibles", "Ciclo de limpieza: validacion -> faltantes -> outliers",
           sprintf("Vuelta %d: %d valores pasan a NA y se reimputan (hot deck en rango valido); edad fuera de rango se toma de la fecha de nacimiento", iter, sum(n)),
           "Repetir hasta que no queden violaciones corregibles", sum(n),
           paste(sprintf("%s=%d", names(n)[n > 0], n[n > 0]), collapse = "; "), df, "repeat { reglas -> NA -> hot_deck -> limites }")
}
viol_final <- sapply(reglas_corregibles(df), sum)
if (sum(viol_final) > 0) {   # acotamiento final si algo no convergio
  a <- reglas_corregibles(df)$aestudio_mayor_edad; df$aestudio[a] <- pmax(df$edad[a] - 3, 0)
  df$nivel_educ <- nivel_educ(df$aestudio)
  bitacora("aestudio", "Violacion residual tras el ciclo", "Acotamiento a edad - 3", "Ajuste final", "Ultimo recurso tras el maximo de vueltas", sum(a), "", df, "pmax(edad - 3, 0)")
}
guardar_tabla(historial, "historial_ciclo")
viol_corregibles_final <- sum(sapply(reglas_corregibles(df), sum))
cat("Violaciones corregibles al cerrar el ciclo:", viol_corregibles_final, "\n")

# Etiquetas: fuera de rango antes y despues
fuera <- function(d) sapply(seq_len(nrow(diccionario_etq)), function(i) {
  x <- d[[diccionario_etq$variable[i]]]; sum(!is.na(x) & (x < diccionario_etq$min_val[i] | x > diccionario_etq$max_val[i])) })
etq_antes <- data.frame(variable = diccionario_etq$variable, Antes = fuera(df |> mutate(tothrs = tothrs_pre)))
etq_antes$Antes[etq_antes$variable == "ylab"] <- validacion0$violaciones[validacion0$regla == "ingreso_negativo"] +
  validacion0$violaciones[validacion0$regla == "ingreso_fuera_rango"]
etq_comp <- etq_antes |> mutate(Despues = fuera(df))
guardar_tabla(etq_comp, "etiquetas_fuera_de_rango")
bitacora(diccionario_etq$variable, "Valores fuera del rango de la etiqueta", "Correccion guiada por metadatos",
         sprintf("Fuera de rango: %d antes -> %d despues", sum(etq_comp$Antes), sum(etq_comp$Despues)),
         "ynolab y yhogpc solo se marcan: son agregados calculados por el INE", sum(etq_comp$Antes), "", df, "fuera(df)", "F07_etiquetas_fuera_de_rango.csv")

# ---------------------------------------------------------------------
# Graficos
# ---------------------------------------------------------------------
p <- ggplot(validacion0, aes(violaciones + 1, fct_reorder(regla, violaciones), fill = tipo)) + geom_col(width = 0.65) +
  geom_text(aes(label = fmt(violaciones)), hjust = -0.3, color = PAL$txt, size = 3.3) +
  scale_x_log10(labels = \(x) fmt(x - 1), expand = expansion(mult = c(0, 0.2))) +
  scale_fill_manual(values = c(PAL$coral, PAL$indigo), name = NULL) +
  labs(title = "Reglas de negocio: violaciones detectadas", subtitle = "Escala log | coral = se corrige en el ciclo | indigo = se marca", x = "Casos", y = NULL, caption = CAP)
guardar(p, "reglas_negocio", 11, 7)

g <- historial |> group_by(iteracion) |> summarise(total = sum(violaciones))
p <- ggplot(historial, aes(factor(iteracion), violaciones, fill = regla)) + geom_col(width = 0.6) +
  geom_text(data = g, aes(factor(iteracion), total, label = fmt(total)), inherit.aes = FALSE, vjust = -0.5, color = PAL$txt, fontface = "bold") +
  scale_fill_manual(values = DISC, name = NULL) + scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Ciclo de limpieza: convergencia", subtitle = "Validacion -> Faltantes -> Outliers -> Validacion, hasta cero violaciones corregibles",
       x = "Vuelta del ciclo", y = "Violaciones corregibles", caption = CAP) + theme(legend.position = "right")
guardar(p, "ciclo_convergencia", 10, 6)

g <- etq_comp |> pivot_longer(-variable, names_to = "momento", values_to = "n") |>
  left_join(diccionario_etq |> select(variable, etiqueta, unidad), by = "variable") |> mutate(nombre = paste0(etiqueta, "\n(", unidad, ")"))
p <- ggplot(g, aes(n, fct_reorder(nombre, n), fill = momento)) + geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_text(aes(label = fmt(n)), position = position_dodge(0.75), hjust = -0.2, color = PAL$txt, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) + scale_fill_manual(values = c(Antes = PAL$coral, Despues = PAL$mint), name = NULL) +
  labs(title = "Valores fuera del rango declarado en la etiqueta", subtitle = "La etiqueta define que es posible; lo imposible se corrige, no se ignora",
       x = "Casos", y = NULL, caption = CAP) + theme(axis.text.y = element_text(size = 8))
guardar(p, "etiquetas_validacion", 11, 6)

g <- bind_rows(data.frame(momento = "Antes", horas = tothrs_pre[df$condicion %in% "Ocupado"]),
               data.frame(momento = "Despues", horas = df$tothrs[df$condicion %in% "Ocupado"])) |> filter(!is.na(horas))
p <- ggplot(g, aes(horas, fill = momento, color = momento)) + geom_density(alpha = 0.32, linewidth = 0.9, adjust = 1.2) +
  geom_vline(xintercept = 126, color = PAL$amber, linetype = "dashed") +
  annotate("text", x = 126, y = 0, label = " maximo declarado: 126 h", hjust = 0, vjust = -0.6, color = PAL$amber, size = 3) +
  scale_fill_manual(values = c(Antes = PAL$coral, Despues = PAL$cyan), name = NULL) + scale_color_manual(values = c(Antes = PAL$coral, Despues = PAL$cyan), name = NULL) +
  labs(title = "Horas trabajadas: antes y despues de la correccion", subtitle = "Los valores imposibles se retiran y se reemplazan por hot deck",
       x = "Horas por semana", y = "Densidad", caption = CAP)
guardar(p, "horas_antes_despues", 11, 6)

set.seed(CFG$semilla)
g <- df |> filter(!is.na(pobre), yhogpc > 0, !is.na(z), !dup_repetido) |> slice_sample(n = min(6000, nrow(df))) |>
  mutate(estado = ifelse(val_p0_vs_linea, "Inconsistente", ifelse(pobre == 1, "Pobre", "No pobre")))
p <- ggplot(g, aes(z, yhogpc, color = estado)) + geom_jitter(width = 15, alpha = 0.5, size = 1.2) +
  geom_abline(slope = 1, intercept = 0, color = PAL$amber, linetype = "dashed") +
  scale_y_log10(labels = lab_bs) + scale_color_manual(values = c(Pobre = PAL$coral, `No pobre` = PAL$indigo, Inconsistente = PAL$amber), name = NULL) +
  labs(title = "Consistencia de la pobreza con la linea de pobreza", subtitle = sprintf("p0 debe ser 1 si yhogpc < z | %s casos inconsistentes (solo se marcan)", fmt(sum(df$val_p0_vs_linea))),
       x = "Linea de pobreza z (Bs)", y = "Ingreso per capita del hogar (log)", caption = CAP)
guardar(p, "consistencia_pobreza")

checkpoint_guardar()
