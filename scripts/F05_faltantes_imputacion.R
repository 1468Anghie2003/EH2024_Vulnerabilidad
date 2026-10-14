# =====================================================================
#  F05 | TRATAMIENTO DE VALORES FALTANTES E IMPUTACION
#  CRISP-DM: Preparacion de los datos | Bitacora paso 4
#  1) Tipo de faltante: estructural vs no respuesta, regla del 80%
#  2) Mecanismo: MCAR / MAR / MNAR
#  3) Imputacion simple (media, mediana, moda), por regresion, hot deck,
#     KNN, arbol y multiple (reglas de Rubin)
#  4) Evaluacion por enmascaramiento y analisis de sensibilidad
#  Toda celda imputada queda marcada en una columna imp_*.
# =====================================================================
if (!exists("CFG_OK")) source(file.path("scripts", "00_config.R"))
requiere(c("df", "MAPA"), "F04")
fase_inicio("F05", "faltantes_imputacion", "Valores faltantes e imputacion")
set.seed(CFG$semilla)

# ---------------------------------------------------------------------
# 5.1 Faltante estructural vs no respuesta
# Cada variable tiene un dominio: la poblacion a la que se le pregunta.
# Un NA fuera del dominio es estructural (la pregunta no aplica) y no
# se imputa. Un NA dentro del dominio es no respuesta.
# ---------------------------------------------------------------------
dominio_de <- function(v, d) {
  ocu <- d$condicion %in% "Ocupado"
  if (v %in% c("tothrs", "phrs", "shrs", "ylab", "yprilab", "yseclab", "cob_op", "caeb_op", "caeb_os") ||
      str_detect(v, "^s04[b-f]")) return(ocu)
  if (str_starts(v, "s04a") || v %in% c("pet", "pea", "pei", "condact")) return(!is.na(d$edad) & d$edad >= CFG$edad_pet)
  if (str_starts(v, "s03") || v %in% c("aestudio", "niv_ed", "niv_ed_g", "cmasi", "educ_prev")) return(!is.na(d$edad) & d$edad >= 4)
  if (str_detect(v, "^s02[cd]") || v %in% c("hnv_ult_a", "quienatenparto")) return(d$sexo %in% "Mujer" & !is.na(d$edad) & d$edad >= 15)
  rep(TRUE, nrow(d))
}
VARS_IMPUTAR <- c("tothrs", "aestudio", "niv_ed_g", "ylab")
faltantes <- bind_rows(lapply(names(raw), function(v) {
  x <- df[[v]]; dom <- dominio_de(v, df)
  data.frame(variable = v, seccion = seccion_de(v), n_na = sum(is.na(x)), pct_na = mean(is.na(x)),
             n_dominio = sum(dom), na_fuera_dominio = sum(is.na(x) & !dom), na_en_dominio = sum(is.na(x) & dom),
             pct_na_dominio = ifelse(sum(dom) > 0, mean(is.na(x[dom])), NA_real_))
})) |> mutate(
  tipo_faltante = case_when(n_na == 0 ~ "Sin faltantes",
                            na_en_dominio == 0 ~ "Estructural (no aplica)",
                            pct_na_dominio > CFG$umbral_col ~ "Estructural de filtro (pregunta condicionada)",
                            na_en_dominio / n_na < 0.5 ~ "Mayormente estructural",
                            TRUE ~ "No respuesta en el dominio"),
  decision = case_when(variable %in% VARS_IMPUTAR ~ "Se imputa en su dominio (variable de analisis)",
                       pct_na > CFG$umbral_col & str_starts(tipo_faltante, "Estructural") ~ "Supera 80%: se CONSERVA, el vacio es estructural",
                       pct_na > CFG$umbral_col ~ "Supera 80%: se CONSERVA y se excluye del modelado",
                       n_na == 0 ~ "Completa",
                       TRUE ~ "Se conserva sin imputar (no interviene en el analisis)"))
print(faltantes |> count(tipo_faltante, decision))
guardar_tabla(faltantes, "clasificacion_faltantes")
bitacora("todas", "Valores faltantes", "Dominio de cada pregunta (PET, ocupados, edad >= 4, mujeres >= 15) y regla del 80%",
         sprintf("%d columnas superan el 80%% de NA: ninguna se elimina", sum(faltantes$pct_na > CFG$umbral_col)),
         "Una columna se eliminaria solo si supera 80% de NA y no aporta informacion; aqui el vacio es estructural y la regla del proyecto prohibe borrar",
         sum(faltantes$n_na), sprintf("%s NA estructurales (no se imputan) y %s NA de no respuesta",
                                     fmt(sum(faltantes$na_fuera_dominio)), fmt(sum(faltantes$na_en_dominio))),
         df, "dominio_de(); faltantes", "F05_clasificacion_faltantes.csv")

# ---------------------------------------------------------------------
# 5.2b Ingreso laboral estructuralmente cero
# s04b_12 = 6 (trabajador familiar sin remuneracion) y 7 (aprendiz sin
# remuneracion): por definicion del cuestionario no reciben ingreso. En
# esas categorias el 99.6% y el 97.9% de los ocupados no tiene ylab: no
# es no respuesta, es un vacio estructural. El INE ya los cuenta con
# ingreso 0 en yper, yhog y p0 (yper = ynolab en el 100% de esos casos).
# Imputarles un ingreso inventaria salarios que no existen y bajaria la
# pobreza artificialmente; se fija ylab = 0 y se marca.
# ---------------------------------------------------------------------
dom_ocu <- df$condicion %in% "Ocupado"
cat_ocup <- cod_num(df$s04b_12)
df$ylab_original <- df$ylab   # copia del valor original (NA incluidos)
df$ylab_cero_estructural <- dom_ocu & is.na(df$ylab) & cat_ocup %in% CFG$cat_sin_pago
diag_cero <- df |> filter(dom_ocu) |> mutate(categoria = cod_num(s04b_12)) |> group_by(categoria) |>
  summarise(ocupados = n(), sin_ylab = sum(is.na(ylab)), pct_sin_ylab = mean(is.na(ylab)),
            cero_estructural = sum(ylab_cero_estructural), no_respuesta = sum(is.na(ylab) & !ylab_cero_estructural))
print(diag_cero)
guardar_tabla(diag_cero, "ylab_faltante_por_categoria")
df$ylab[df$ylab_cero_estructural] <- 0
bitacora(c("ylab", "s04b_12"), "Faltante estructural confundido con no respuesta", "Cruce de ylab faltante con la categoria ocupacional (s04b_12)",
         "ylab = 0 para familiares y aprendices sin remuneracion; se marca en ylab_cero_estructural",
         "Categorias sin pago por definicion del cuestionario; coincide con el tratamiento del INE en yper y p0",
         sum(df$ylab_cero_estructural), sprintf("Quedan %d faltantes reales de ylab (no respuesta) para imputar", sum(dom_ocu & is.na(df$ylab))),
         df, "df$ylab[ocupado & is.na(ylab) & s04b_12 %in% c(6, 7)] <- 0", "F05_ylab_faltante_por_categoria.csv")

# ---------------------------------------------------------------------
# 5.2 Mecanismo del faltante (MCAR / MAR / MNAR)
# Si la ausencia depende de variables observadas no es MCAR (compatible
# con MAR). MNAR no se puede probar con los datos: se aborda con el
# analisis de sensibilidad de 5.7.
# ---------------------------------------------------------------------
mecanismo <- function(var, dom) {
  s <- df[dom & !df$dup_repetido, ]; m <- is.na(s[[var]])
  if (sum(m) < 5 || sum(!m) < 5) return(NULL)
  r1 <- lapply(c("sexo", "zona", "departamento", "nivel_educ", "jefe"), function(cv) {
    g <- s[[cv]]; ok <- !is.na(g); tb <- table(m[ok], g[ok])
    if (any(dim(tb) < 2)) return(NULL)
    ct <- suppressWarnings(chisq.test(tb))
    data.frame(variable = var, covariable = cv, prueba = "Chi-cuadrado", estadistico = unname(ct$statistic), p_valor = ct$p.value)
  })
  r2 <- lapply(c("edad", "aestudio"), function(cv) {
    if (cv == var || sum(!is.na(s[[cv]][m])) < 3 || sum(!is.na(s[[cv]][!m])) < 3) return(NULL)
    tt <- t.test(s[[cv]][m], s[[cv]][!m])
    data.frame(variable = var, covariable = cv, prueba = "t de Welch", estadistico = unname(tt$statistic), p_valor = tt$p.value)
  })
  bind_rows(c(r1, r2))
}
dom_ocu <- df$condicion %in% "Ocupado"
dom_edu <- !is.na(df$edad) & df$edad >= 4
mec <- bind_rows(mecanismo("ylab", dom_ocu), mecanismo("tothrs", dom_ocu), mecanismo("aestudio", dom_edu)) |>
  mutate(conclusion = ifelse(p_valor < 0.05, "La ausencia depende de la covariable: no es MCAR", "Sin evidencia contra MCAR"))
resumen_mec <- mec |> group_by(variable) |>
  summarise(pruebas = n(), significativas = sum(p_valor < 0.05),
            mecanismo = ifelse(significativas > 0, "MAR plausible (no MCAR); MNAR posible, se evalua por sensibilidad", "Compatible con MCAR"))
print(resumen_mec)
guardar_tabla(mec, "mecanismo_faltantes")
bitacora(c("ylab", "tothrs", "aestudio"), "Mecanismo de ausencia", "Chi-cuadrado (ausencia x covariable) y t de Welch (covariable entre con/sin dato)",
         paste(sprintf("%s: %s", resumen_mec$variable, resumen_mec$mecanismo), collapse = " | "),
         "p < 0.05 indica que la ausencia depende de lo observado (descarta MCAR)", NA,
         "Justifica imputar con modelos condicionados a covariables y no con la media global", codigo = "mecanismo()",
         archivo = "F05_mecanismo_faltantes.csv")

# ---------------------------------------------------------------------
# 5.3 Imputacion de horas y anios de estudio (insumos del modelo de ingreso)
# Hot deck aleatorio dentro de clase; solo donan valores en rango valido
# ---------------------------------------------------------------------
tothrs_antes <- df$tothrs; aestudio_antes <- df$aestudio
df$imp_tothrs <- dom_ocu & is.na(df$tothrs)
clase_h <- paste(df$departamento, df$zona, df$sexo)
df$tothrs[dom_ocu] <- hot_deck(df$tothrs[dom_ocu], clase_h[dom_ocu], 0, 126)
bitacora("tothrs", "No respuesta en horas (ocupados)", "Hot deck aleatorio dentro de clase departamento x area x sexo",
         "Imputacion de tothrs", "Donantes con 0 a 126 horas; los no ocupados no se imputan (NA estructural)",
         sum(df$imp_tothrs), "Marcado en imp_tothrs", df, "hot_deck(tothrs, clase, 0, 126)")

df$imp_aestudio <- dom_edu & is.na(df$aestudio)
clase_e <- paste(ifelse(is.na(df$niv_ed_g), "s/n", df$niv_ed_g), cut(df$edad, c(3, 9, 14, 19, 29, 44, 64, Inf)), df$zona)
df$aestudio[dom_edu] <- hot_deck(df$aestudio[dom_edu], clase_e[dom_edu], 0, 25)
df$nivel_educ <- nivel_educ(df$aestudio)
bitacora("aestudio", "No respuesta en anios de estudio", "Hot deck dentro de clase nivel educativo agrupado x grupo de edad x area",
         "Imputacion de aestudio y recalculo de nivel_educ", "Donantes de 0 a 25 anios de estudio; menores de 4 anios no se imputan",
         sum(df$imp_aestudio), "Marcado en imp_aestudio", df, "hot_deck(aestudio, clase, 0, 25)")

if (hay(df, "niv_ed_g")) {
  df$imp_niv_ed_g <- dom_edu & is.na(df$niv_ed_g)
  modas <- df |> filter(!is.na(niv_ed_g), !is.na(nivel_educ)) |> group_by(nivel_educ) |> summarise(moda = moda(niv_ed_g))
  m <- modas$moda[match(df$nivel_educ, modas$nivel_educ)]
  df$niv_ed_g[df$imp_niv_ed_g] <- m[df$imp_niv_ed_g]
  bitacora("niv_ed_g", "No respuesta en variable categorica", "Imputacion simple por moda dentro de nivel_educ",
           "Imputacion de niv_ed_g", "Variables cualitativas: moda (la media no tiene sentido)", sum(df$imp_niv_ed_g),
           "Marcado en imp_niv_ed_g", df, "moda(niv_ed_g) por nivel_educ")
}

# ---------------------------------------------------------------------
# 5.4 Ingreso laboral: metodos de imputacion
# ---------------------------------------------------------------------
oc <- df |> mutate(.fila = row_number()) |> filter(condicion %in% "Ocupado", !dup_repetido, !ylab_cero_estructural)
oc$ly <- log1p(pmax(oc$ylab, 0)); oc$ly[is.na(oc$ylab)] <- NA
rellenar <- function(d, ref) {
  for (v in c("edad", "aestudio", "tothrs")) d[[v]][is.na(d[[v]])] <- median(ref[[v]], na.rm = TRUE)
  for (v in c("sexo", "zona", "departamento")) d[[v]][is.na(d[[v]])] <- moda(ref[[v]])
  d$nivel_educ <- nivel_educ(d$aestudio); d
}
F_YLAB <- ly ~ aestudio + edad + I(edad^2) + sexo + zona + departamento + tothrs
feats <- function(d, ref) {
  X <- cbind(d$edad, d$aestudio, d$tothrs); mu <- c(mean(ref$edad), mean(ref$aestudio), mean(ref$tothrs))
  s <- c(sd(ref$edad), sd(ref$aestudio), sd(ref$tothrs))
  cbind(sweep(sweep(X, 2, mu), 2, s, "/"), mujer = d$sexo %in% "Mujer", rural = d$zona %in% "Rural",
        sapply(DEPTOS, function(k) 2 * (d$departamento %in% k)))
}
knn_pred <- function(tr, te, k = CFG$k_knn) {
  Xtr <- feats(tr, tr); Xte <- feats(te, tr); ytr <- tr$ly; n2 <- rowSums(Xtr^2); out <- numeric(nrow(Xte))
  for (ix in split(seq_len(nrow(Xte)), ceiling(seq_len(nrow(Xte)) / 250))) {
    D <- outer(rowSums(Xte[ix, , drop = FALSE]^2), n2, "+") - 2 * Xte[ix, , drop = FALSE] %*% t(Xtr)
    out[ix] <- apply(D, 1, function(r) mean(ytr[which(r <= sort(r, partial = k)[k])[seq_len(k)]]))
  }
  out
}
imputadores <- list(
  "Media"                  = function(tr, te) rep(mean(expm1(tr$ly)), nrow(te)),
  "Mediana"                = function(tr, te) rep(median(expm1(tr$ly)), nrow(te)),
  "Regresion"              = function(tr, te) { m <- lm(F_YLAB, tr); expm1(predict(m, alinear(te, tr, c("sexo", "zona", "departamento")))) },
  "Regresion estocastica"  = function(tr, te) { m <- lm(F_YLAB, tr)
    expm1(predict(m, alinear(te, tr, c("sexo", "zona", "departamento"))) + rnorm(nrow(te), 0, sigma(m))) },
  "Hot deck"               = function(tr, te) { cl <- paste(c(tr$departamento, te$departamento), c(tr$sexo, te$sexo), c(as.character(tr$nivel_educ), as.character(te$nivel_educ)))
    y <- c(tr$ly, rep(NA, nrow(te))); expm1(hot_deck(y, cl)[-seq_len(nrow(tr))]) },
  "KNN (k = 5)"            = function(tr, te) expm1(knn_pred(tr, te)),
  "Arbol de regresion"     = function(tr, te) { m <- rpart::rpart(ly ~ aestudio + edad + sexo + zona + departamento + tothrs, tr, method = "anova", cp = 0.002)
    expm1(predict(m, alinear(te, tr, c("sexo", "zona", "departamento")))) })

# 5.4a Evaluacion por enmascaramiento: se ocultan 10% de ingresos conocidos
obs <- rellenar(oc |> filter(!is.na(ly)), oc |> filter(!is.na(ly)))
mask <- sample(nrow(obs), round(0.10 * nrow(obs)))
tr <- obs[-mask, ]; te <- obs[mask, ]; real <- expm1(te$ly)
eval_imp <- bind_rows(lapply(names(imputadores), function(mt) {
  set.seed(CFG$semilla); pr <- imputadores[[mt]](tr, te); pr[is.na(pr)] <- median(expm1(tr$ly))
  data.frame(metodo = mt, rmse_log = sqrt(mean((log1p(pmax(pr, 0)) - log1p(real))^2)),
             sesgo_media_pct = 100 * (mean(pr) / mean(real) - 1), ratio_desv_est = sd(pr) / sd(real),
             ks = unname(suppressWarnings(ks.test(pr, real)$statistic)), acierto_umbral_smn = mean((pr < CFG$smn) == (real < CFG$smn)))
}))
cand <- eval_imp |> filter(abs(ratio_desv_est - 1) <= 0.25)
if (!nrow(cand)) cand <- eval_imp
METODO_YLAB <- cand$metodo[order(-cand$acierto_umbral_smn, cand$ks)][1]
print(eval_imp |> mutate(across(where(is.numeric), ~ round(.x, 3))))
cat("Metodo elegido para ylab:", METODO_YLAB, "\n")
guardar_tabla(eval_imp, "evaluacion_enmascaramiento")
bitacora("ylab", "Eleccion del metodo de imputacion", "Enmascaramiento: se oculta 10% de ingresos conocidos y se comparan 7 metodos",
         paste("Metodo elegido:", METODO_YLAB),
         "Mayor acierto al clasificar bajo/sobre el salario minimo entre los metodos que conservan la dispersion (ratio DE entre 0.75 y 1.25)",
         length(mask), "Eleccion objetiva y reproducible del metodo", codigo = "eval_imp", archivo = "F05_evaluacion_enmascaramiento.csv")

# 5.4b Cada metodo aplicado a los faltantes reales (para comparar y para sensibilidad)
falt_y <- rellenar(oc |> filter(is.na(ly)), obs)
completo <- lapply(names(imputadores), function(mt) { set.seed(CFG$semilla)
  pr <- imputadores[[mt]](obs, falt_y); pr[is.na(pr)] <- median(expm1(obs$ly)); pmax(pr, 0) })
names(completo) <- names(imputadores)
comp_metodos <- bind_rows(
  data.frame(metodo = "Sin imputar (observados)", media = mean(expm1(obs$ly)), desv_est = sd(expm1(obs$ly)), mediana = median(expm1(obs$ly))),
  bind_rows(lapply(names(completo), function(mt) { y <- c(expm1(obs$ly), completo[[mt]])
    data.frame(metodo = mt, media = mean(y), desv_est = sd(y), mediana = median(y)) })))
guardar_tabla(comp_metodos, "comparacion_metodos")

# 5.4c Aplicacion del metodo elegido sobre df
ylab_antes <- df$ylab
df$imp_ylab <- FALSE
df$ylab[falt_y$.fila] <- completo[[METODO_YLAB]]
df$imp_ylab[falt_y$.fila] <- TRUE
bitacora("ylab", "No respuesta en ingreso laboral (ocupados)", METODO_YLAB, "Imputacion de ylab con el metodo elegido",
         "Solo ocupados; los no ocupados tienen NA estructural. Copia del valor original en ylab_original",
         sum(df$imp_ylab), sprintf("Media de ylab: %s -> %s Bs", fmt(mean(ylab_antes[dom_ocu], na.rm = TRUE)), fmt(mean(df$ylab[dom_ocu], na.rm = TRUE))),
         df, paste0("imputadores[['", METODO_YLAB, "']](obs, falt_y)"))

# 5.4d Experimento: que pasaria si se imputara un sueldo a quienes
# trabajan sin pago (categorias 6 y 7). Se les asigna un ingreso con el
# mismo modelo y se recalcula la tasa A y la pobreza de la poblacion.
sp <- rellenar(df |> mutate(.fila = row_number()) |> filter(ylab_cero_estructural), obs)
set.seed(CFG$semilla)
y_sp <- pmax(imputadores[["Regresion estocastica"]](obs, sp), 0)
nmiem <- ave(rep(1, nrow(df)), df$folio, FUN = length)
suma_hog <- tapply(y_sp, df$folio[sp$.fila], sum)
extra <- ifelse(df$folio %in% names(suma_hog), suma_hog[as.character(df$folio)], 0); extra[is.na(extra)] <- 0
yhogpc_exp <- df$yhogpc + extra / nmiem
pob_w <- function(pobre) sum(df$w * pobre, na.rm = TRUE) / sum(df$w[!is.na(pobre)])
d_exp <- df; d_exp$ylab[sp$.fila] <- y_sp; d_exp <- marcar_vulnerable(d_exp); d_base <- marcar_vulnerable(df)
tA <- function(d) { e <- d |> filter(pob_estudio, !is.na(vul_persona)); tasa_w(e$vul_persona, e$w) }
exp_sin_pago <- data.frame(
  escenario = c("Ingreso 0 por definicion (decision del proyecto, igual que el INE)", "Si se les inventara un sueldo con el modelo"),
  personas = sum(df$ylab_cero_estructural), ingreso_asignado_medio = c(0, mean(y_sp)),
  tasa_A = c(tA(d_base), tA(d_exp)),
  pobreza_poblacion = c(pob_w(df$yhogpc < df$z), pob_w(yhogpc_exp < df$z)))
print(exp_sin_pago); guardar_tabla(exp_sin_pago, "experimento_sin_pago")
bitacora(c("ylab", "p0"), "Experimento: imputar a quienes trabajan sin pago", "Regresion estocastica aplicada a las categorias 6 y 7 y recalculo de yhogpc",
         sprintf("Pobreza de la poblacion: %.1f%% -> %.1f%% | tasa A: %.1f%% -> %.1f%%", 100 * exp_sin_pago$pobreza_poblacion[1], 100 * exp_sin_pago$pobreza_poblacion[2],
                 100 * exp_sin_pago$tasa_A[1], 100 * exp_sin_pago$tasa_A[2]),
         "Inventar ingresos que no existen haria desaparecer pobreza que si existe", nrow(sp), "Se confirma la decision de 5.2b",
         codigo = "imputadores[['Regresion estocastica']](obs, sp)", archivo = "F05_experimento_sin_pago.csv")
rm(d_exp, d_base, sp)

# ---------------------------------------------------------------------
# 5.5 Imputacion multiple (m = 5) por regresion estocastica bayesiana
# Reglas de Rubin: T = U_media + (1 + 1/m) * B
# ---------------------------------------------------------------------
mod_mi <- lm(F_YLAB, obs)
b <- coef(mod_mi); ok_b <- !is.na(b); V <- vcov(mod_mi, complete = FALSE); gl <- mod_mi$df.residual
tt_mi <- delete.response(terms(mod_mi))
mf_mi <- model.frame(tt_mi, alinear(falt_y, obs, c("sexo", "zona", "departamento")), xlev = mod_mi$xlevels, na.action = na.pass)
Xf <- model.matrix(tt_mi, mf_mi)[, names(b)[ok_b], drop = FALSE]
imps <- lapply(seq_len(CFG$m_imp), function(j) {
  set.seed(CFG$semilla + j)
  sig <- sigma(mod_mi) * sqrt(gl / rchisq(1, gl))
  bj <- MASS::mvrnorm(1, b[ok_b], V)
  pmax(expm1(drop(Xf %*% bj) + rnorm(nrow(Xf), 0, sig)), 0)
})
rubin <- function(Q, U, m = length(Q)) { qb <- mean(Q); ub <- mean(U); B <- var(Q); Tt <- ub + (1 + 1 / m) * B
  data.frame(estimacion = qb, var_dentro = ub, var_entre = B, var_total = Tt, ee = sqrt(Tt), li = qb - 1.96 * sqrt(Tt), ls = qb + 1.96 * sqrt(Tt),
             fraccion_info_faltante = (1 + 1 / m) * B / Tt) }
est_mi <- lapply(seq_along(imps), function(j) {
  d <- df; d$ylab[falt_y$.fila] <- imps[[j]]; d <- marcar_vulnerable(d)
  e <- d |> filter(pob_estudio)
  mw <- wmean(e$ylab, e$w); vw <- wmean((e$ylab - mw)^2, e$w) / n_ef(e$w)
  v <- e |> filter(!is.na(vulnerable)); pv <- wmean(v$vulnerable == "Vulnerable", v$w)
  data.frame(imputacion = j, media_ylab = mw, var_media = vw, tasa_vul = pv, var_tasa = pv * (1 - pv) / n_ef(v$w))
}) |> bind_rows()
rubin_tab <- bind_rows(cbind(parametro = "Ingreso laboral medio (ocupados 19+)", rubin(est_mi$media_ylab, est_mi$var_media)),
                       cbind(parametro = "Tasa de empleo vulnerable", rubin(est_mi$tasa_vul, est_mi$var_tasa)))
print(est_mi); print(rubin_tab)
guardar_tabla(est_mi, "imputacion_multiple_por_conjunto"); guardar_tabla(rubin_tab, "reglas_de_rubin")
bitacora("ylab", "Incertidumbre de la imputacion", sprintf("Imputacion multiple m = %d con parametros aleatorizados + reglas de Rubin", CFG$m_imp),
         "Estimaciones combinadas de ingreso medio y tasa de vulnerabilidad", "Varianza total = varianza dentro + (1 + 1/m) x varianza entre",
         nrow(falt_y), sprintf("Tasa de vulnerabilidad combinada %.1f%% (IC %.1f%% - %.1f%%)", 100 * rubin_tab$estimacion[2], 100 * rubin_tab$li[2], 100 * rubin_tab$ls[2]),
         codigo = "MASS::mvrnorm(); rubin()", archivo = "F05_reglas_de_rubin.csv")

# ---------------------------------------------------------------------
# 5.6 Metodos considerados y no aplicados (justificacion)
# ---------------------------------------------------------------------
metodos_tabla <- tribble(
  ~metodo, ~aplicado, ~justificacion,
  "Eliminacion (listwise)", "Solo en sensibilidad", "Borra filas: viola la regla del proyecto y sesga si el faltante no es MCAR",
  "Media / mediana", "Comparacion", "Conservan el promedio pero encogen la dispersion",
  "Moda", "Si (niv_ed_g)", "Adecuada para variables cualitativas",
  "Regresion", "Comparacion", "Predice el valor esperado: subestima la variabilidad",
  "Hot deck en clase", "Si (tothrs, aestudio)", "Toma valores reales de donantes parecidos; conserva la distribucion",
  "KNN", "Comparacion / posible eleccion", "Donantes por distancia en edad, educacion, horas, sexo, area y departamento",
  "Arbol de regresion", "Comparacion", "Captura interacciones sin suponer linealidad",
  "Multiple con Rubin", "Si (inferencia)", "Cuantifica la incertidumbre que agrega imputar",
  "Algoritmo EM", "No", "Supone normalidad multivariada; los ingresos son muy asimetricos y la imputacion multiple cubre el mismo objetivo")
metodos_tabla$aplicado[startsWith(metodos_tabla$metodo, word(METODO_YLAB, 1))][1] <- "Si (ylab, elegido)"
guardar_tabla(metodos_tabla, "metodos_imputacion")

# ---------------------------------------------------------------------
# 5.7 Analisis de sensibilidad: tasa de empleo vulnerable segun como se
# traten los faltantes de ylab
# ---------------------------------------------------------------------
tasa_con <- function(yvals) { d <- df; d$ylab[falt_y$.fila] <- yvals; d <- marcar_vulnerable(d)
  v <- d |> filter(!is.na(vulnerable)); wmean(v$vulnerable == "Vulnerable", v$w) }
sensib <- bind_rows(
  data.frame(escenario = "Eliminacion (solo casos completos)", tasa = tasa_con(rep(NA_real_, nrow(falt_y)))),
  data.frame(escenario = "Faltante = 0 (peor caso)", tasa = tasa_con(rep(0, nrow(falt_y)))),
  data.frame(escenario = "Faltante = 3 x SMN (mejor caso)", tasa = tasa_con(rep(3 * CFG$smn, nrow(falt_y)))),
  bind_rows(lapply(names(completo), function(mt) data.frame(escenario = mt, tasa = tasa_con(completo[[mt]])))),
  data.frame(escenario = "Multiple (Rubin)", tasa = rubin_tab$estimacion[2]))
print(sensib)
guardar_tabla(sensib, "sensibilidad_tasa_vulnerable")
bitacora("vulnerable", "Posible faltante MNAR", "Analisis de sensibilidad con 11 escenarios", "Comparacion de la tasa de empleo vulnerable",
         "Si la conclusion se mantiene en escenarios extremos, es robusta al mecanismo de ausencia", nrow(falt_y),
         sprintf("Rango de la tasa: %.1f%% a %.1f%%", 100 * min(sensib$tasa), 100 * max(sensib$tasa)),
         codigo = "tasa_con()", archivo = "F05_sensibilidad_tasa_vulnerable.csv")

# ---------------------------------------------------------------------
# 5.8 Graficos
# ---------------------------------------------------------------------
g <- faltantes |> group_by(seccion) |>
  summarise(Estructural = sum(na_fuera_dominio), `No respuesta` = sum(na_en_dominio), Presentes = sum(nrow(df) - n_na)) |>
  pivot_longer(-seccion) |> group_by(seccion) |> mutate(p = value / sum(value))
p <- ggplot(g, aes(p, fct_rev(seccion), fill = factor(name, levels = c("Presentes", "Estructural", "No respuesta")))) +
  geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
  geom_text(aes(label = ifelse(p > 0.04, pct(p, 0), "")), position = position_stack(vjust = 0.5, reverse = TRUE), color = "white", size = 3.2) +
  scale_fill_manual(values = c(Presentes = PAL$indigo, Estructural = PAL$grid, `No respuesta` = PAL$coral), name = NULL) +
  scale_x_continuous(labels = percent) +
  labs(title = "Composicion de las celdas por seccion", subtitle = "Estructural = la pregunta no aplica (no se imputa) | No respuesta = falta dentro del dominio",
       x = NULL, y = NULL, caption = CAP)
guardar(p, "composicion_faltantes")

p <- ggplot(faltantes |> filter(n_na > 0), aes(pct_na, pct_na_dominio, color = tipo_faltante)) +
  annotate("rect", xmin = CFG$umbral_col, xmax = 1, ymin = -Inf, ymax = Inf, fill = PAL$coral, alpha = 0.08) +
  geom_vline(xintercept = CFG$umbral_col, color = PAL$amber, linetype = "dashed") +
  geom_point(size = 2.6, alpha = 0.8) +
  geom_text(data = faltantes |> filter(variable %in% VARS_IMPUTAR), aes(label = variable), vjust = -1, color = PAL$txt, size = 3.4) +
  scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) + scale_color_manual(values = DISC, name = NULL) +
  labs(title = "Mapa de decision de faltantes", subtitle = "Eje X: % NA en todo el dataset | eje Y: % NA dentro del dominio de la pregunta | franja = supera 80%",
       x = "% NA total", y = "% NA en el dominio", caption = CAP) + guides(color = guide_legend(nrow = 2))
guardar(p, "mapa_decision_faltantes", 11, 7.5)

g <- bind_rows(lapply(c("sexo", "zona", "departamento", "nivel_educ"), function(cv)
  df |> filter(dom_ocu, !dup_repetido, !is.na(.data[[cv]])) |> group_by(cat = as.character(.data[[cv]])) |>
    summarise(pct_na = mean(is.na(ylab_original) & !ylab_cero_estructural)) |> mutate(covariable = cv))) |>
  left_join(mec |> filter(variable == "ylab", prueba == "Chi-cuadrado") |> select(covariable, p_valor), by = "covariable") |>
  mutate(panel = sprintf("%s (p = %s)", covariable, format.pval(p_valor, 2)))
p <- ggplot(g, aes(pct_na, fct_reorder(cat, pct_na))) + geom_segment(aes(x = 0, xend = pct_na, yend = cat), color = PAL$grid, linewidth = 2) +
  geom_point(aes(color = pct_na), size = 4) + facet_wrap(~panel, scales = "free_y") +
  scale_color_gradientn(colors = GRAD, guide = "none") + scale_x_continuous(labels = percent) +
  labs(title = "Mecanismo de ausencia del ingreso laboral", subtitle = "% de ocupados sin ingreso declarado por grupo | p del chi-cuadrado en cada panel",
       x = "% sin ingreso declarado", y = NULL, caption = CAP)
guardar(p, "mecanismo_ylab", 12, 8)

g <- eval_imp |> transmute(metodo, `Acierto bajo/sobre SMN` = acierto_umbral_smn, `Ratio de desv. estandar` = ratio_desv_est,
                           `RMSE (log)` = rmse_log, `KS (distancia de distribuciones)` = ks) |>
  pivot_longer(-metodo) |> group_by(name) |> mutate(rel = esc01(value))
p <- ggplot(g, aes(name, fct_rev(metodo), fill = rel)) + geom_tile(color = PAL$bg, linewidth = 1.2) +
  geom_text(aes(label = round(value, 3), fontface = ifelse(metodo == METODO_YLAB, "bold", "plain")), color = "white", size = 3.6) +
  scale_fill_gradientn(colors = GRAD, guide = "none") +
  labs(title = "Evaluacion de metodos de imputacion por enmascaramiento", subtitle = paste("Se ocultaron ingresos conocidos y se compararon con lo imputado | elegido:", METODO_YLAB),
       x = NULL, y = NULL, caption = CAP) + theme(axis.text.x = element_text(angle = 15, hjust = 1))
guardar(p, "evaluacion_metodos", 11, 6.5)

g <- comp_metodos |> mutate(across(c(media, desv_est), ~ .x / .x[1], .names = "rel_{.col}")) |>
  select(metodo, rel_media, rel_desv_est) |> pivot_longer(-metodo) |>
  mutate(name = recode(name, rel_media = "Media", rel_desv_est = "Desviacion estandar"))
p <- ggplot(g, aes(value, fct_rev(fct_inorder(metodo)))) + geom_vline(xintercept = 1, color = PAL$amber, linetype = "dashed") +
  geom_line(aes(group = metodo), color = PAL$grid, linewidth = 2.5) + geom_point(aes(color = name), size = 4.5) +
  scale_color_manual(values = c(Media = PAL$cyan, `Desviacion estandar` = PAL$coral), name = NULL) +
  scale_x_continuous(labels = \(x) paste0(round(100 * x), "%")) +
  labs(title = "Imputar por la media conserva el promedio pero encoge la dispersion",
       subtitle = "Media y desviacion estandar de ylab tras imputar, relativas a los datos observados (100%)", x = NULL, y = NULL, caption = CAP)
guardar(p, "media_vs_dispersion", 11, 6)

g <- bind_rows(data.frame(tipo = "Observado", y = expm1(obs$ly)), data.frame(tipo = paste("Imputado:", METODO_YLAB), y = completo[[METODO_YLAB]])) |> filter(y > 0)
p <- ggplot(g, aes(y, fill = tipo, color = tipo)) + geom_density(alpha = 0.35, linewidth = 0.8) +
  geom_vline(xintercept = CFG$smn, color = PAL$amber, linetype = "dashed") +
  scale_x_log10(labels = lab_bs) + scale_fill_manual(values = c(PAL$amber, PAL$violet), name = NULL) + scale_color_manual(values = c(PAL$amber, PAL$violet), name = NULL) +
  labs(title = "Ingreso laboral: observado vs imputado", subtitle = sprintf("%s valores imputados | la diferencia refleja quienes no declaran (MAR), no un error", fmt(nrow(falt_y))),
       x = "Ingreso laboral (log)", y = "Densidad", caption = CAP)
guardar(p, "ylab_observado_vs_imputado")

g <- bind_rows(lapply(seq_along(imps), function(j) data.frame(conjunto = paste("Imputacion", j), y = imps[[j]]))) |> filter(y > 0)
p <- ggplot(g, aes(y, color = conjunto)) + geom_density(linewidth = 0.8) +
  geom_density(data = data.frame(y = expm1(obs$ly[obs$ly > 0])), aes(y), inherit.aes = FALSE, color = PAL$txt, linetype = "dashed") +
  scale_x_log10(labels = lab_bs) + scale_color_manual(values = DISC, name = NULL) +
  labs(title = sprintf("Imputacion multiple: %d conjuntos plausibles", CFG$m_imp),
       subtitle = sprintf("Rubin: tasa de vulnerabilidad %.1f%% (IC 95%% %.1f%% - %.1f%%) | fraccion de informacion faltante %.3f | punteada = observado",
                          100 * rubin_tab$estimacion[2], 100 * rubin_tab$li[2], 100 * rubin_tab$ls[2], rubin_tab$fraccion_info_faltante[2]),
       x = "Ingreso laboral imputado (log)", y = "Densidad", caption = CAP)
guardar(p, "imputacion_multiple")

p <- ggplot(sensib, aes(tasa, fct_reorder(escenario, tasa))) +
  annotate("rect", xmin = min(sensib$tasa), xmax = max(sensib$tasa), ymin = -Inf, ymax = Inf, fill = PAL$violet, alpha = 0.08) +
  geom_segment(aes(x = min(tasa), xend = tasa, yend = escenario), color = PAL$grid, linewidth = 2) +
  geom_point(aes(color = escenario == METODO_YLAB), size = 4.5) + geom_text(aes(label = pct(tasa)), hjust = -0.4, color = PAL$txt, size = 3.3) +
  scale_color_manual(values = c(`TRUE` = PAL$amber, `FALSE` = PAL$violet), guide = "none") +
  scale_x_continuous(labels = percent, expand = expansion(mult = c(0.05, 0.2))) +
  labs(title = "Analisis de sensibilidad: tasa de empleo vulnerable", subtitle = "Cada punto trata los ingresos faltantes de otra forma | ambar = metodo elegido",
       x = "Tasa de empleo vulnerable (ocupados 19+)", y = NULL, caption = CAP)
guardar(p, "sensibilidad_vulnerable", 11, 6.5)

g <- bind_rows(data.frame(var = "Horas semanales (ocupados)", momento = "Antes", x = tothrs_antes[dom_ocu]),
               data.frame(var = "Horas semanales (ocupados)", momento = "Despues", x = df$tothrs[dom_ocu]),
               data.frame(var = "Anios de estudio (4+)", momento = "Antes", x = aestudio_antes[dom_edu]),
               data.frame(var = "Anios de estudio (4+)", momento = "Despues", x = df$aestudio[dom_edu])) |>
  filter(!is.na(x) | TRUE)
etq <- g |> group_by(var, momento) |> summarise(etq = sprintf("%s: %s NA", first(momento), fmt(sum(is.na(x))))) |>
  mutate(vj = ifelse(momento == "Antes", 2, 3.6))
g <- g |> filter(!is.na(x), x <= 126)
p <- ggplot(g, aes(x, color = momento, fill = momento)) + geom_density(alpha = 0.25, linewidth = 0.9, adjust = 1.5) +
  geom_text(data = etq, aes(x = Inf, y = Inf, label = etq, vjust = vj), hjust = 1.1, size = 3.3, inherit.aes = FALSE, color = PAL$txt) +
  facet_wrap(~var, scales = "free") + scale_color_manual(values = c(Antes = PAL$coral, Despues = PAL$cyan), name = NULL) +
  scale_fill_manual(values = c(Antes = PAL$coral, Despues = PAL$cyan), name = NULL) +
  labs(title = "Hot deck: antes y despues", subtitle = "La forma de la distribucion se conserva tras imputar", x = NULL, y = "Densidad", caption = CAP)
guardar(p, "hot_deck_antes_despues", 12, 6)

rm(obs, tr, te, oc)
checkpoint_guardar()
