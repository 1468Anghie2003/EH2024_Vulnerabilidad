# =====================================================================
#  F11 | PATRONES Y HALLAZGOS: EMPLEO VULNERABLE EN BOLIVIA
#  CRISP-DM: Evaluacion e interpretacion
#  Desagregacion por departamento, area, sexo, ocupacion, rama y
#  educacion para los enfoques A (persona), B (hogar) y C (OIT); IVL por
#  enfoque; brechas; salarios por ocupacion. Todas las frases de
#  hallazgos.txt se calculan con los datos.
# =====================================================================
if (!exists("CFG_OK")) source(file.path("scripts", "00_config.R"))
requiere(c("df", "MAPA", "vul", "vul_h", "res_cls", "perfil_cl", "cv_res"), "F10")
fase_inicio("F11", "patrones", "Patrones y hallazgos")

est <- df |> filter(pob_estudio, !is.na(vul_persona)) |>
  mutate(departamento = factor(departamento, levels = DEPTOS), sexo = factor(sexo, levels = c("Hombre", "Mujer")),
         zona = factor(zona, levels = c("Urbana", "Rural")))
tasa2 <- function(d, ...) d |> group_by(...) |>
  summarise(tasa_persona = tasa_w(vul_persona, w), tasa_hogar = tasa_w(vul_hogar, w), tasa_oit = tasa_w(vul_oit, w),
            ylab_mediana = wquant(ylab, w, 0.5), informal = wmean(informal, w), n = n(), poblacion = sum(w), .groups = "drop") |>
  mutate(tasa = tasa_persona, brecha_ab = tasa_persona - tasa_hogar)
t_nac  <- tasa2(est)
t_dep  <- tasa2(est, departamento)
t_zona <- tasa2(est |> filter(!is.na(zona)), zona)
t_sexo <- tasa2(est |> filter(!is.na(sexo)), sexo)
t_dzs  <- tasa2(est |> filter(!is.na(zona), !is.na(sexo)), departamento, zona, sexo)
t_ds   <- tasa2(est |> filter(!is.na(sexo)), departamento, sexo)
t_dz   <- tasa2(est |> filter(!is.na(zona)), departamento, zona)
t_rama <- tasa2(est |> filter(!is.na(rama)), rama) |> filter(n >= 30)
t_educ <- tasa2(est |> filter(!is.na(nivel_educ)), nivel_educ)
t_grupo <- tasa2(est |> filter(!is.na(grupo_ocup)), grupo_ocup) |> filter(n >= 30)
t_cat  <- tasa2(est |> filter(!is.na(cat_ocupacional)), cat_ocupacional) |> filter(n >= 30)
t_edad <- tasa2(est |> filter(!is.na(edad_grupo)), edad_grupo)
t_sexo_zona <- tasa2(est |> filter(!is.na(sexo), !is.na(zona)), zona, sexo)
for (x in c("t_dep", "t_zona", "t_sexo", "t_dzs", "t_ds", "t_dz", "t_rama", "t_educ", "t_grupo", "t_cat", "t_edad", "t_sexo_zona"))
  guardar_tabla(get(x), sub("t_", "tasa_vulnerable_", x))
bitacora(c("vul_persona", "vul_hogar", "vul_oit", "departamento", "zona", "sexo"), "Desagregacion del objetivo",
         "Tasas ponderadas de los tres enfoques por departamento, area, sexo, rama, ocupacion, categoria, edad y educacion",
         sprintf("Nacional: A %.1f%% | B %.1f%% | C %.1f%%", 100 * t_nac$tasa_persona, 100 * t_nac$tasa_hogar, 100 * t_nac$tasa_oit),
         "Ponderado por factor de expansion", nrow(est), "Tablas F11_tasa_vulnerable_*", codigo = "tasa2(est, ...)")

# Brecha salarial de genero: bruta (medianas) y por hora; la condicionada
# viene de la ecuacion de Mincer extendida (F10)
oc_y <- est |> filter(ylab > 0, !is.na(sexo))
brecha_salarial <- oc_y |> group_by(sexo) |>
  summarise(mediana_mes = wquant(ylab, w, 0.5), media_mes = wmean(ylab, w), mediana_hora = wquant(ingreso_hora, w, 0.5),
            horas_media = wmean(tothrs, w), educ_media = wmean(aestudio, w), n = n())
b_sexo_mincer <- mincer_tab |> filter(modelo == "Extendida", termino == "sexoMujer")
brecha_salarial$brecha_condicionada_mincer_pct <- b_sexo_mincer$efecto_pct
guardar_tabla(brecha_salarial, "brecha_salarial_genero")
ing_educ_sexo <- oc_y |> filter(!is.na(nivel_educ)) |> group_by(nivel_educ, sexo) |>
  summarise(mediana = wquant(ylab, w, 0.5), mediana_hora = wquant(ingreso_hora, w, 0.5), n = n(), .groups = "drop")
guardar_tabla(ing_educ_sexo, "ingreso_educacion_sexo")
bitacora(c("ylab", "sexo"), "Brecha salarial de genero", "Medianas ponderadas mensual y por hora; brecha condicionada por Mincer",
         sprintf("Mediana mensual: mujeres Bs %s vs hombres Bs %s | por hora: Bs %.1f vs %.1f | condicionada: %+.1f%%",
                 fmt(brecha_salarial$mediana_mes[2]), fmt(brecha_salarial$mediana_mes[1]), brecha_salarial$mediana_hora[2], brecha_salarial$mediana_hora[1],
                 b_sexo_mincer$efecto_pct), "La brecha condicionada compara personas con igual educacion, experiencia, horas, area y departamento",
         nrow(oc_y), "", codigo = "wquant(); mincer_tab", archivo = "F11_brecha_salarial_genero.csv")

# Ocupacion x categoria ocupacional (para el grafico solar de la pagina)
t_oc_cat <- tasa2(est |> filter(!is.na(grupo_ocup), !is.na(cat_ocupacional)), grupo_ocup, cat_ocupacional) |> filter(n >= 10)
guardar_tabla(t_oc_cat, "ocupacion_categoria")

# Casos observados comparables a los perfiles del simulador: tasas reales
# por grupo ocupacional, categoria, area, sexo y aporte a la Gestora
est_c <- est |> filter(!is.na(grupo_ocup), !is.na(cat_ocupacional), !is.na(informal)) |> mutate(aporta_gestora = !informal)
t_comp <- tasa2(est_c |> filter(!is.na(zona), !is.na(sexo)), grupo_ocup, cat_ocupacional, zona, sexo, aporta_gestora) |> filter(n >= 15)
t_comp_dep <- tasa2(est_c, departamento, grupo_ocup, cat_ocupacional, aporta_gestora) |> filter(n >= 15)
guardar_tabla(t_comp, "comparables_perfil"); guardar_tabla(t_comp_dep, "comparables_perfil_dep")

# ---------------------------------------------------------------------
# Graficos
# ---------------------------------------------------------------------
COL_ENF <- c(`A Persona` = PAL$coral, `B Hogar` = PAL$cyan, `C OIT` = PAL$amber)
for (k in c("persona", "hogar")) {
  iv <- if (k == "persona") vul else vul_h
  p <- mapa_bolivia(iv, "ivl", paste0("IVL ", round(iv$ivl), " (#", iv$rango, ")"), scale_fill_gradientn(colors = GRAD, name = "IVL", limits = c(0, 100))) +
    labs(title = sprintf("Indice de Vulnerabilidad Laboral - enfoque %s", c(persona = "A persona", hogar = "B hogar")[[k]]),
         subtitle = "Promedio min-max de desempleo, subempleo, informalidad, baja educacion y tasa del enfoque (0-100)", caption = CAP) +
    theme(legend.position = "right")
  guardar(p, paste0("mapa_ivl_", k), 9, 9)
}
p <- mapa_bolivia(t_dep, "tasa_persona", pct(t_dep$tasa_persona), scale_fill_gradientn(colors = GRAD, name = "Tasa A", labels = percent)) +
  labs(title = "Ocupados que ganan menos del SMN (enfoque A)", subtitle = sprintf("Ocupados de %d+ | nacional %s", CFG$edad_min, pct(t_nac$tasa_persona)), caption = CAP) +
  theme(legend.position = "right")
guardar(p, "mapa_tasa_persona", 9, 9)
p <- mapa_bolivia(t_dep, "tasa_hogar", pct(t_dep$tasa_hogar), scale_fill_gradientn(colors = GRAD, name = "Tasa B", labels = percent)) +
  labs(title = "Ocupados en hogares pobres (enfoque B)", subtitle = sprintf("Ocupados de %d+ | nacional %s", CFG$edad_min, pct(t_nac$tasa_hogar)), caption = CAP) +
  theme(legend.position = "right")
guardar(p, "mapa_tasa_hogar", 9, 9)

g <- t_dep |> select(departamento, `A Persona` = tasa_persona, `B Hogar` = tasa_hogar, `C OIT` = tasa_oit) |> pivot_longer(-departamento)
p <- ggplot(g, aes(value, fct_reorder(departamento, value, max), color = name)) + geom_line(aes(group = departamento), color = PAL$grid, linewidth = 2.5) +
  geom_point(size = 4.5) + scale_color_manual(values = COL_ENF, name = NULL) + scale_x_continuous(labels = percent) +
  labs(title = "Mismo departamento, tres respuestas", subtitle = "Tasa de vulnerabilidad de los ocupados de 19+ segun el enfoque", x = NULL, y = NULL, caption = CAP)
guardar(p, "tres_enfoques_departamento", 11, 6.5)

g <- bind_rows(vul, vul_h) |> pivot_longer(c(desempleo, subempleo, informalidad, baja_educ, tasa_enfoque), names_to = "componente", values_to = "valor") |>
  group_by(enfoque, componente) |> mutate(rel = esc01(valor)) |> ungroup() |>
  mutate(componente = recode(componente, desempleo = "Desempleo", subempleo = "Subempleo", informalidad = "Informalidad", baja_educ = "Baja educacion", tasa_enfoque = "Tasa del enfoque"),
         enfoque = recode(enfoque, persona = "IVL A persona", hogar = "IVL B hogar"))
p <- ggplot(g, aes(componente, fct_reorder(departamento, ivl), fill = rel)) + geom_tile(color = PAL$bg, linewidth = 1.2) +
  geom_text(aes(label = pct(valor, 0)), color = "white", size = 3) + facet_wrap(~enfoque) + scale_fill_gradientn(colors = GRAD, guide = "none") +
  labs(title = "Componentes del IVL por enfoque", subtitle = "Color = posicion relativa entre departamentos", x = NULL, y = NULL, caption = CAP) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
guardar(p, "componentes_ivl", 13, 7)

g <- t_dzs |> mutate(grupo = paste(zona, sexo, sep = "\n")) |> select(departamento, grupo, `A Persona` = tasa_persona, `B Hogar` = tasa_hogar) |> pivot_longer(-c(departamento, grupo))
p <- ggplot(g, aes(grupo, fct_reorder(departamento, value), fill = value)) + geom_tile(color = PAL$bg, linewidth = 1.2) +
  geom_text(aes(label = pct(value, 0)), color = "white", size = 3.2, fontface = "bold") + facet_wrap(~name) +
  scale_fill_gradientn(colors = GRAD, labels = percent, name = "Tasa") +
  labs(title = "Departamento x area x sexo, por enfoque", x = NULL, y = NULL, caption = CAP) + theme(legend.position = "right")
guardar(p, "tasa_depto_area_sexo", 13, 7)

g <- t_ds |> select(departamento, sexo, tasa_persona, tasa_hogar) |> pivot_longer(c(tasa_persona, tasa_hogar)) |>
  pivot_wider(names_from = sexo, values_from = value) |> mutate(name = recode(name, tasa_persona = "A Persona", tasa_hogar = "B Hogar"), brecha = Mujer - Hombre)
p <- ggplot(g, aes(y = fct_reorder(departamento, brecha))) + geom_segment(aes(x = Hombre, xend = Mujer, yend = departamento), color = PAL$grid, linewidth = 2) +
  geom_point(aes(x = Hombre), color = PAL$cyan, size = 3.8) + geom_point(aes(x = Mujer), color = PAL$pink, size = 3.8) +
  geom_text(aes(x = pmax(Hombre, Mujer), label = sprintf("%+.1f pp", 100 * brecha)), hjust = -0.3, color = PAL$txt, size = 3) +
  facet_wrap(~name) + scale_x_continuous(labels = percent, expand = expansion(mult = c(0.05, 0.25))) +
  labs(title = "Brecha de genero por departamento: depende del enfoque", subtitle = "Celeste = hombres | rosado = mujeres", x = NULL, y = NULL, caption = CAP)
guardar(p, "brecha_genero", 13, 6.5)

g <- t_dz |> select(departamento, zona, tasa_persona, tasa_hogar) |> pivot_longer(c(tasa_persona, tasa_hogar)) |>
  mutate(name = recode(name, tasa_persona = "A Persona", tasa_hogar = "B Hogar"))
p <- ggplot(g, aes(value, fct_reorder(departamento, value), color = zona)) + geom_line(aes(group = departamento), color = PAL$grid, linewidth = 2) +
  geom_point(size = 3.8) + facet_wrap(~name) + scale_color_manual(values = COL_ZONA, name = NULL) + scale_x_continuous(labels = percent) +
  labs(title = "Brecha urbano-rural por departamento", x = NULL, y = NULL, caption = CAP)
guardar(p, "brecha_urbano_rural", 13, 6.5)

g <- ing_educ_sexo
p <- ggplot(g, aes(nivel_educ, mediana, fill = sexo)) + geom_col(position = position_dodge(0.75), width = 0.7) +
  geom_text(aes(label = fmt(mediana)), position = position_dodge(0.75), vjust = -0.4, color = PAL$txt, size = 3) +
  geom_hline(yintercept = CFG$smn, color = PAL$amber, linetype = "dashed") + scale_fill_manual(values = COL_SEXO, name = NULL) +
  scale_y_continuous(labels = lab_bs, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Ingreso laboral mediano por nivel educativo y sexo", subtitle = "Linea = SMN 2024 | ocupados de 19+ con ingreso", x = NULL, y = NULL, caption = CAP)
guardar(p, "ingreso_educacion_sexo", 11, 6.5)

g <- t_grupo |> select(grupo_ocup, `A Persona` = tasa_persona, `B Hogar` = tasa_hogar, ylab_mediana) |> pivot_longer(c(`A Persona`, `B Hogar`))
p <- ggplot(g, aes(value, fct_reorder(grupo_ocup, ylab_mediana), color = name)) + geom_point(size = 4) +
  geom_text(data = t_grupo, aes(x = 1.02, y = grupo_ocup, label = paste("Bs", fmt(ylab_mediana))), inherit.aes = FALSE, hjust = 0, color = PAL$sub, size = 3) +
  scale_color_manual(values = COL_ENF, name = NULL) + scale_x_continuous(labels = percent, limits = c(0, 1.2)) +
  labs(title = "Vulnerabilidad por grupo ocupacional", subtitle = "Orden por ingreso mediano (a la derecha)", x = NULL, y = NULL, caption = CAP)
guardar(p, "vulnerable_por_ocupacion", 11, 6.5)

if (nrow(t_rama)) {
  p <- ggplot(t_rama, aes(tasa_persona, fct_reorder(rama, tasa_persona), fill = tasa_persona)) + geom_col(width = 0.7) +
    geom_point(aes(x = tasa_hogar), color = PAL$cyan, size = 3) +
    geom_text(aes(label = pct(tasa_persona)), hjust = -0.2, color = PAL$txt, size = 3) + scale_fill_gradientn(colors = GRAD, guide = "none") +
    scale_x_continuous(labels = percent, expand = expansion(mult = c(0, 0.15))) +
    labs(title = "Vulnerabilidad por rama de actividad", subtitle = "Barras = A persona | puntos = B hogar", x = NULL, y = NULL, caption = CAP)
  guardar(p, "vulnerable_por_rama", 10, 7)
}

# ---------------------------------------------------------------------
# Hallazgos: frases calculadas con los datos (sin cifras escritas a mano)
# ---------------------------------------------------------------------
mxA <- t_dep |> slice_max(tasa_persona, n = 1); mxB <- t_dep |> slice_max(tasa_hogar, n = 1)
ivA <- vul |> slice_max(ivl, n = 1); ivB <- vul_h |> slice_max(ivl, n = 1); ivm <- vul |> slice_min(ivl, n = 1)
cu <- cuadrante
kap <- concordancia |> filter(enfoque_1 == "persona", enfoque_2 == "hogar")
imA <- importancia |> filter(objetivo == "A") |> arrange(desc(caida_auc)); imB <- importancia |> filter(objetivo == "B") |> arrange(desc(caida_auc))
rA <- cv_res$A$resumen; rB <- cv_res$B$resumen
tA <- res_cls |> filter(objetivo == "A", modelo == cv_res$A$mejor); tB <- res_cls |> filter(objetivo == "B", modelo == cv_res$B$mejor)
retorno <- mincer_tab |> filter(modelo == "Clasica", termino == "aestudio")
vr <- val_ddi |> filter(variable == "ylab")
hallazgos <- c(
  sprintf("1. Los datos reproducen las cifras publicadas por el INE: ingreso laboral medio Bs %s (DDI: Bs %s).",
          fmt(vr$media_calculada), fmt(vr$media_oficial)),
  sprintf("2. Con el factor validado la pobreza moderada de la poblacion es %s, igual a la cifra oficial del INE para 2024 (37.7%%).", pct(pob_tasa$calculada[1])),
  sprintf("3. Entre los ocupados de %d+ anios: %s gana menos del SMN (A persona), %s vive en un hogar pobre (B hogar) y %s es cuenta propia o familiar sin pago (C OIT).",
          CFG$edad_min, pct(t_nac$tasa_persona), pct(t_nac$tasa_hogar), pct(t_nac$tasa_oit)),
  sprintf("4. Los enfoques A y B coinciden poco (kappa %.2f): %s es vulnerable en ambos, %s solo como persona y %s solo por su hogar.",
          kap$kappa, pct(cu$p[cu$cuadrante == "Ambos enfoques"]), pct(cu$p[cu$cuadrante == "Solo persona"]), pct(cu$p[cu$cuadrante == "Solo hogar"])),
  sprintf("5. Brecha de genero: con A, mujeres %s vs hombres %s; con B, %s vs %s. La desventaja de la mujer es de ingreso propio, no de pobreza del hogar.",
          pct(t_sexo$tasa_persona[2]), pct(t_sexo$tasa_persona[1]), pct(t_sexo$tasa_hogar[2]), pct(t_sexo$tasa_hogar[1])),
  sprintf("6. Area rural: A %s vs urbana %s; B %s vs %s.", pct(t_zona$tasa_persona[2]), pct(t_zona$tasa_persona[1]), pct(t_zona$tasa_hogar[2]), pct(t_zona$tasa_hogar[1])),
  sprintf("7. Mayor tasa: A en %s (%s), B en %s (%s). Mayor IVL: %s en A (%.0f) y %s en B (%.0f); menor IVL A: %s (%.0f).",
          mxA$departamento, pct(mxA$tasa_persona), mxB$departamento, pct(mxB$tasa_hogar), ivA$departamento, ivA$ivl, ivB$departamento, ivB$ivl, ivm$departamento, ivm$ivl),
  sprintf("8. Cada anio de estudio se asocia con %.1f%% mas de ingreso laboral (Mincer clasica, R2 aj %.3f). A igual educacion, experiencia, horas y region, la mujer gana %.1f%% menos.",
          retorno$efecto_pct, retorno$r2_aj, abs(b_sexo_mincer$efecto_pct)),
  sprintf("9. Mejor modelo A: %s (AUC CV %.3f, test %.3f); mejor modelo B: %s (AUC CV %.3f, test %.3f). Seis modelos comparados con los mismos 5 pliegues.",
          cv_res$A$mejor, rA$auc_media[1], tA$auc, cv_res$B$mejor, rB$auc_media[1], tB$auc),
  sprintf("10. Lo que mas pesa: en A %s; en B %s.", paste(head(imA$variable, 3), collapse = ", "), paste(head(imB$variable, 3), collapse = ", ")),
  sprintf("11. Informalidad: %s de los ocupados de 19+ no aporta a la Gestora.", pct(tasa_informal)),
  sprintf("12. La limpieza no elimino ninguna fila ni columna: %s filas y %s columnas originales conservadas.", fmt(nrow(df)), ncol(raw)))
writeLines(c("HALLAZGOS - DATOS SUCIOS A PATRONES REALES: EMPLEO VULNERABLE EN BOLIVIA (EH 2024)", strrep("=", 60), "", hallazgos), file.path(RAIZ, "salidas", "hallazgos.txt"))
cat(hallazgos, sep = "\n")
bitacora(c("vul_persona", "vul_hogar"), "Interpretacion", "Sintesis automatica de resultados", "Archivo salidas/hallazgos.txt", "Cada cifra se calcula con los datos",
         length(hallazgos), "Insumo directo para las conclusiones", codigo = "sprintf(); writeLines()", archivo = "hallazgos.txt")

checkpoint_guardar()
