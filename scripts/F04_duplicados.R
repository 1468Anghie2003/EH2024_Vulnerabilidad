# =====================================================================
#  F04 | GESTION DE DUPLICADOS
#  CRISP-DM: Preparacion de los datos | Bitacora paso 3
#  (a) Deteccion real en el dataset: se MARCA, no se borra.
#  (b) Ejemplo de clase aplicado: simulacion controlada de carga doble
#      y comparacion de tres correcciones con antes y despues.
# =====================================================================
if (!exists("CFG_OK")) source(file.path("scripts", "00_config.R"))
requiere(c("df", "MAPA"), "F03")
fase_inicio("F04", "duplicados", "Deteccion y correccion de duplicados")

# ---------------------------------------------------------------------
# 4.1 Deteccion real. La llave de la EH es folio + nro (hogar + persona)
# ---------------------------------------------------------------------
orig <- names(raw)
df$dup_exacto   <- duplicated(df[orig]) | duplicated(df[orig], fromLast = TRUE)
df$dup_clave    <- duplicated(df[c("folio", "nro")]) | duplicated(df[c("folio", "nro")], fromLast = TRUE)
df$dup_repetido <- duplicated(df[c("folio", "nro")])          # segunda y siguientes copias
clave_cercana   <- paste(df$folio, df$edad, df$sexo, df$parentesco)
df$dup_cercano  <- !df$dup_clave & (duplicated(clave_cercana) | duplicated(clave_cercana, fromLast = TRUE)) &
  !is.na(df$edad) & df$parentesco %in% 3:20
resumen_dup <- data.frame(
  tipo = c("Duplicado exacto (todas las columnas)", "Llave folio + nro repetida", "Copias a excluir del analisis", "Casi duplicado (mismo hogar, edad, sexo y parentesco)"),
  registros = c(sum(df$dup_exacto), sum(df$dup_clave), sum(df$dup_repetido), sum(df$dup_cercano)))
print(resumen_dup)
guardar_tabla(resumen_dup, "duplicados_reales")
guardar_tabla(df[df$dup_clave, c("folio", "nro", "edad", "sexo", "departamento", "ylab")], "duplicados_detalle")

df$w[df$dup_repetido] <- 0     # la copia queda en el dataset pero no pesa en ningun indicador
bitacora(c("folio", "nro"), "Registros duplicados", "duplicated() sobre todas las columnas y sobre la llave folio + nro",
         "Marcado en dup_exacto, dup_clave, dup_repetido y dup_cercano; las copias reciben w = 0",
         "Regla del proyecto: no se elimina ninguna fila. La primera ocurrencia se conserva como valida",
         sum(df$dup_repetido), sprintf("%d copias neutralizadas (w = 0); %d casi duplicados solo marcados para revision",
                                      sum(df$dup_repetido), sum(df$dup_cercano)), df,
         "duplicated(df[c('folio','nro')]); df$w[df$dup_repetido] <- 0", "F04_duplicados_reales.csv")

# ---------------------------------------------------------------------
# 4.2 Simulacion controlada (ejemplo 1 de clase con datos reales)
# Se repite al azar el 5% de los registros, como una doble carga de
# boletas, y se comparan tres estrategias de correccion.
# ---------------------------------------------------------------------
set.seed(CFG$semilla)
base_dup <- df |> filter(!dup_repetido) |>
  transmute(folio, nro, departamento, sexo, edad, zona, w, ylab, tothrs, yhogpc, pobre)
repetidos <- base_dup[sample(nrow(base_dup), floor(CFG$pct_dup_sim * nrow(base_dup))), ]
repetidos$ylab <- repetidos$ylab * runif(nrow(repetidos), 0.85, 1.15)    # segunda visita con ingreso algo distinto
datos_sucios <- bind_rows(base_dup, repetidos)

n_exacto <- sum(duplicated(datos_sucios)); n_llave <- sum(duplicated(datos_sucios[c("folio", "nro")]))
sin_exactos <- datos_sucios[!duplicated(datos_sucios), ]
primera     <- datos_sucios |> distinct(folio, nro, .keep_all = TRUE)
condicion   <- datos_sucios |> group_by(folio, nro) |> slice_max(order_by = ylab, n = 1, with_ties = FALSE, na_rm = FALSE) |> ungroup()

indicadores <- function(d, etiqueta) data.frame(version = etiqueta, registros = nrow(d), poblacion = sum(d$w, na.rm = TRUE),
  ingreso_medio = wmean(d$ylab, d$w), tasa_pobreza = wmean(d$pobre, d$w))
comparacion_dup <- bind_rows(indicadores(base_dup, "Original limpio"), indicadores(datos_sucios, "Con duplicados"),
                             indicadores(sin_exactos, "a) Duplicados exactos fuera"), indicadores(primera, "b) Primera ocurrencia"),
                             indicadores(condicion, "c) Mayor ingreso"))
print(comparacion_dup)
guardar_tabla(comparacion_dup, "simulacion_duplicados")
bitacora(c("folio", "nro"), "Duplicados simulados (ejemplo de clase)", "Carga doble del 5% + duplicated(), distinct() y slice_max()",
         sprintf("%d exactos y %d por llave detectados; tres correcciones comparadas", n_exacto, n_llave),
         "La simulacion trabaja sobre una copia; df no se modifica", nrow(repetidos),
         sprintf("Con duplicados la poblacion expandida se desvia %.1f%% y el ingreso medio %.2f%%",
                 100 * (comparacion_dup$poblacion[2] / comparacion_dup$poblacion[1] - 1),
                 100 * (comparacion_dup$ingreso_medio[2] / comparacion_dup$ingreso_medio[1] - 1)),
         datos_sucios, "distinct(folio, nro, .keep_all = TRUE)", "F04_simulacion_duplicados.csv")

# ---------------------------------------------------------------------
# 4.3 Graficos
# ---------------------------------------------------------------------
p <- ggplot(resumen_dup, aes(registros, fct_rev(fct_inorder(tipo)), fill = registros > 0)) + geom_col(width = 0.6) +
  geom_text(aes(label = fmt(registros)), hjust = -0.3, color = PAL$txt, size = 3.8) +
  scale_fill_manual(values = c(`TRUE` = PAL$coral, `FALSE` = PAL$mint), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(title = "Duplicados reales en persona.csv", subtitle = "Se marcan con columnas dup_*; ninguna fila se elimina", x = "Registros", y = NULL, caption = CAP)
guardar(p, "duplicados_reales", 10, 5)

p <- comparacion_dup |> mutate(dif = registros - nrow(base_dup)) |>
  ggplot(aes(registros, fct_rev(fct_inorder(version)), fill = dif == 0)) + geom_col(width = 0.7) +
  geom_vline(xintercept = nrow(base_dup), color = PAL$amber, linetype = "dashed", linewidth = 0.8) +
  geom_text(aes(label = paste0(comma(registros), "  (", ifelse(dif >= 0, "+", ""), comma(dif), ")")), hjust = -0.08, color = PAL$txt, size = 3.3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.25)), labels = comma) +
  scale_fill_manual(values = c(`TRUE` = PAL$mint, `FALSE` = PAL$coral), guide = "none") +
  labs(title = "Duplicados: antes y despues de cada correccion", subtitle = "Linea punteada = tamanio original | verde = recupera el tamanio original",
       x = "Registros", y = NULL, caption = CAP)
guardar(p, "simulacion_registros", 11, 6)

p <- comparacion_dup |>
  mutate(across(c(poblacion, ingreso_medio, tasa_pobreza), ~ 100 * (.x / .x[version == "Original limpio"] - 1))) |>
  pivot_longer(c(poblacion, ingreso_medio, tasa_pobreza), names_to = "indicador", values_to = "sesgo") |>
  mutate(indicador = recode(indicador, poblacion = "Poblacion expandida", ingreso_medio = "Ingreso laboral medio", tasa_pobreza = "Tasa de pobreza")) |>
  filter(version != "Original limpio") |>
  ggplot(aes(sesgo, fct_rev(fct_inorder(version)), fill = indicador)) + geom_col(position = position_dodge(0.75), width = 0.65) +
  geom_text(aes(label = sprintf("%+.2f%%", sesgo)), position = position_dodge(0.75), hjust = ifelse(TRUE, -0.15, 0), color = PAL$txt, size = 3) +
  geom_vline(xintercept = 0, color = PAL$txt, linewidth = 0.4) +
  scale_fill_manual(values = c(`Poblacion expandida` = PAL$indigo, `Ingreso laboral medio` = PAL$cyan, `Tasa de pobreza` = PAL$coral), name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0.1, 0.3))) +
  labs(title = "Efecto de los duplicados sobre los indicadores", subtitle = "Desvio porcentual frente al dataset original | la carga doble infla la poblacion expandida",
       x = "% de desvio", y = NULL, caption = CAP)
guardar(p, "simulacion_sesgo", 11, 6)

rm(base_dup, repetidos, datos_sucios, sin_exactos, primera, condicion)
checkpoint_guardar()
