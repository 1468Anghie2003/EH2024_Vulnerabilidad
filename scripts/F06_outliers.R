# =====================================================================
#  F06 | DETECCION Y TRATAMIENTO DE VALORES ATIPICOS
#  CRISP-DM: Preparacion de los datos | Bitacora paso 5
#  Metodos: IQR clasico (k = 1.5), IQR de Tukey sobre logaritmo (k = 1.5) para
#  ingresos, puntaje z (|z| > 3) y z robusto con MAD (|z| > 3.5).
#  Tratamiento: winsorizado en columna nueva <var>_w; el valor original
#  se conserva. Los errores de medicion se corrigen en F07.
# =====================================================================
if (!exists("CFG_OK")) source(file.path("scripts", "00_config.R"))
requiere(c("df", "MAPA"), "F05")
fase_inicio("F06", "outliers", "Deteccion y tratamiento de valores atipicos")

VARS_OUT <- c(ylab = "IQR log", ynolab = "IQR log", yper = "IQR log", yhogpc = "IQR log",
              tothrs = "IQR k=1.5", edad = "IQR k=1.5", aestudio = "IQR k=1.5")
METODOS <- c("IQR k=1.5", "IQR log", "z > 3", "MAD z > 3.5")

comparacion_out <- bind_rows(lapply(names(VARS_OUT), function(v) bind_rows(lapply(METODOS, function(mt) {
  x <- base_out(df, v); L <- limites(x, mt)
  data.frame(variable = v, metodo = mt, lim_inf = L[1], lim_sup = L[2],
             bajo = sum(x < L[1], na.rm = TRUE), alto = sum(x > L[2], na.rm = TRUE), n_validos = sum(!is.na(x)),
             elegido = mt == VARS_OUT[[v]])
})))) |> mutate(total = bajo + alto, pct = total / n_validos)
print(comparacion_out |> filter(elegido) |> select(variable, metodo, lim_inf, lim_sup, bajo, alto, pct))
guardar_tabla(comparacion_out, "comparacion_metodos_atipicos")

# Marcado y winsorizado con el metodo elegido por variable
antes_w <- df
for (v in names(VARS_OUT)) {
  x <- base_out(df, v); L <- limites(x, VARS_OUT[[v]])
  df[[paste0("out_", v)]] <- !is.na(x) & (x < L[1] | x > L[2])
  if (v %in% c(VARS_INGRESO, "tothrs")) df[[paste0(v, "_w")]] <- ifelse(!is.na(df[[v]]) & df[[v]] > 0, pmin(pmax(df[[v]], L[1]), L[2]), df[[v]])
  r <- comparacion_out |> filter(variable == v, elegido)
  bitacora(v, "Valores atipicos", paste("Rango intercuartilico:", VARS_OUT[[v]]),
           ifelse(v %in% c(VARS_INGRESO, "tothrs"), paste0("Marcado en out_", v, " y winsorizado en ", v, "_w"), paste0("Marcado en out_", v)),
           sprintf("Limites [%s ; %s]%s", fmt(r$lim_inf, 1), fmt(r$lim_sup, 1),
                   ifelse(v %in% VARS_INGRESO, " | ingresos: IQR sobre log porque la distribucion es log-normal", "")),
           r$total, sprintf("%s atipicos (%.2f%%); el valor original se conserva", fmt(r$total), 100 * r$pct), df,
           sprintf("limites(x, '%s'); pmin(pmax(x, L1), L2)", VARS_OUT[[v]]))
}

# Clasificacion: atipico plausible vs posible error de medicion
tipo_atipico <- df |> filter(out_ylab | out_tothrs) |>
  transmute(folio, nro, departamento, edad, aestudio, tothrs, ylab,
            clasificacion = case_when(tothrs > 126 ~ "Posible error: horas imposibles (se corrige en F07)",
                                      ylab > 200000 ~ "Posible error: ingreso fuera del rango declarado (F07)",
                                      out_ylab & ylab > 0 & aestudio >= 13 ~ "Plausible: ingreso alto con educacion superior",
                                      out_ylab ~ "Plausible: ingreso extremo, se conserva y se winsoriza en ylab_w",
                                      TRUE ~ "Plausible: jornada extensa"))
print(count(tipo_atipico, clasificacion))
guardar_tabla(tipo_atipico, "atipicos_detalle")
bitacora(c("ylab", "tothrs"), "Origen del atipico", "Reglas de plausibilidad (rango declarado y coherencia con educacion)",
         "Separacion entre atipico real y error de medicion", "Un atipico real se conserva; un error se corrige en la validacion",
         nrow(tipo_atipico), paste(sprintf("%s: %d", count(tipo_atipico, clasificacion)$clasificacion, count(tipo_atipico, clasificacion)$n), collapse = " | "),
         codigo = "case_when()", archivo = "F06_atipicos_detalle.csv")

# ---------------------------------------------------------------------
# Graficos
# ---------------------------------------------------------------------
p <- ggplot(comparacion_out, aes(metodo, variable, fill = pct)) + geom_tile(color = PAL$bg, linewidth = 1.2) +
  geom_text(aes(label = sprintf("%s\n(%.1f%%)", fmt(total), 100 * pct)), color = "white", size = 3.2) +
  geom_tile(data = comparacion_out |> filter(elegido), color = PAL$amber, fill = NA, linewidth = 1.2) +
  scale_fill_gradientn(colors = GRAD, guide = "none") +
  labs(title = "Atipicos detectados segun el metodo", subtitle = "Recuadro ambar = metodo elegido para cada variable | z > 3 supone normalidad y falla con ingresos",
       x = NULL, y = NULL, caption = CAP)
guardar(p, "metodos_atipicos", 11, 6.5)

nlab <- comparacion_out |> filter(variable == "ylab", elegido)
g <- bind_rows(data.frame(version = "Original", y = df$ylab), data.frame(version = "Winsorizado", y = df$ylab_w)) |> filter(y > 0)
p <- ggplot(g, aes(version, y, fill = version)) + geom_violin(color = NA, alpha = 0.85) +
  geom_boxplot(width = 0.12, fill = PAL$bg, color = PAL$txt, outlier.color = PAL$coral, outlier.size = 0.6) +
  geom_hline(yintercept = c(nlab$lim_inf, nlab$lim_sup), color = PAL$amber, linetype = "dashed") +
  scale_y_log10(labels = lab_bs) + scale_fill_manual(values = c(PAL$indigo, PAL$mint), guide = "none") +
  labs(title = "Tratamiento de atipicos en el ingreso laboral",
       subtitle = sprintf("IQR sobre log, k = %s | %s casos fuera de [Bs %s ; Bs %s] | lineas = limites", CFG$k_iqr_log, fmt(nlab$total), fmt(nlab$lim_inf), fmt(nlab$lim_sup)),
       x = NULL, y = "Ingreso laboral (log)", caption = CAP)
guardar(p, "winsorizado_ylab", 9, 7)

set.seed(CFG$semilla)
g <- df |> filter(condicion %in% "Ocupado", ylab > 0, !is.na(edad)) |> slice_sample(n = min(8000, sum(df$condicion %in% "Ocupado")))
p <- ggplot(g, aes(edad, ylab)) + geom_point(aes(color = out_ylab), alpha = 0.5, size = 1.1) +
  geom_point(data = g |> filter(out_ylab), color = PAL$coral, size = 2) +
  scale_y_log10(labels = lab_bs) + scale_color_manual(values = c(`FALSE` = PAL$indigo, `TRUE` = PAL$coral), labels = c("Normal", "Atipico"), name = NULL) +
  labs(title = "Atipicos del ingreso en el espacio edad-ingreso", subtitle = "Los atipicos bajos son ingresos casi nulos; los altos, ingresos extremos reales",
       x = "Edad", y = "Ingreso laboral (log)", caption = CAP)
guardar(p, "atipicos_edad_ingreso")

g <- bind_rows(data.frame(version = "Original", x = df$tothrs), data.frame(version = "Winsorizado", x = df$tothrs_w)) |> filter(!is.na(x))
p <- ggplot(g, aes(x, version, fill = version)) + geom_boxplot(color = PAL$txt, outlier.color = PAL$coral, alpha = 0.85, width = 0.5) +
  scale_fill_manual(values = c(PAL$violet, PAL$mint), guide = "none") +
  labs(title = "Horas semanales: original vs winsorizado", subtitle = "IQR k = 1.5 sobre ocupados", x = "Horas", y = NULL, caption = CAP)
guardar(p, "winsorizado_horas", 10, 5)

rm(antes_w)
checkpoint_guardar()
