# =====================================================================
#  F10 | MODELADO: APRENDIZAJE AUTOMATICO Y MINERIA DE DATOS
#  CRISP-DM: Modelado y Evaluacion
#  1 Clasificacion supervisada para los dos enfoques:
#      A Persona (ylab < SMN)  y  B Hogar (p0 = 1)
#    Seis modelos comparados con la MISMA validacion cruzada estratificada
#    (comparar_modelos() en scripts/00_modelos.R), eleccion por AUC con
#    la regla de 1 error estandar, prueba pareada entre modelos, umbral
#    de Youden fuera de pliegue y evaluacion final en un test intacto.
#  2 Balanceo de clases (submuestreo y pesos) para el enfoque B
#  3 Regresion del ingreso: ecuacion de Mincer vs Random Forest
#  4 Agrupamiento (k-means)  5 Reglas de asociacion  6 Anomalias
#  7 Patrones por cohorte (pseudo serie temporal)
#  Sin fuga de informacion: ningun ingreso, linea de pobreza ni p0 entra
#  como predictor, porque son los que definen los objetivos.
# =====================================================================
if (!exists("CFG_OK")) source(file.path("scripts", "00_config.R"))
requiere(c("df", "MAPA", "vul"), "F09")
fase_inicio("F10", "modelado", "Modelado: aprendizaje automatico y mineria de datos")
set.seed(CFG$semilla)

agrupar_raros <- function(x, min_p = 0.01) { x <- ifelse(is.na(x) | x == "", "Sin dato", as.character(x)); f <- table(x) / length(x)
  x[x %in% names(f)[f < min_p]] <- "Otra"; factor(x) }
sin_dato <- function(x) factor(ifelse(is.na(x), "Sin dato", as.character(x)))

# ---------------------------------------------------------------------
# 10.0 Matriz de modelado (ocupados de 19+)
# ---------------------------------------------------------------------
base <- df |> mutate(.fila = row_number()) |> filter(pob_estudio, !is.na(vul_persona), !is.na(vul_hogar)) |>
  transmute(.fila, folio, nro, w,
            yA = as.integer(vul_persona == "Vulnerable"), yB = as.integer(vul_hogar == "Vulnerable"),
            edad, edad2 = edad^2, aestudio, tothrs = ifelse(is.na(tothrs), median(tothrs, na.rm = TRUE), tothrs),
            sexo = droplevels(factor(sexo)), zona = droplevels(factor(zona)),
            departamento = factor(departamento, levels = DEPTOS),
            jefe = factor(ifelse(jefe, "Jefe de hogar", "Otro miembro")),
            cat_ocupacional = agrupar_raros(cat_ocupacional), sector = agrupar_raros(sector),
            tamano_empresa = sin_dato(tamano_empresa), informal = factor(ifelse(informal %in% TRUE, "No aporta", "Aporta")),
            seguro_salud = factor(ifelse(seguro_salud_trabajo %in% TRUE, "Si", "No")),
            grupo_ocup = agrupar_raros(grupo_ocup), rama = agrupar_raros(rama),
            antiguedad = pmin(ifelse(is.na(antiguedad_anios), median(antiguedad_anios, na.rm = TRUE), antiguedad_anios), 60),
            carga_hogar = pmin(carga_hogar, 15), menores_14, mayores_60)
X_ML <- c("edad", "edad2", "aestudio", "tothrs", "sexo", "zona", "departamento", "jefe", "cat_ocupacional", "sector",
          "tamano_empresa", "informal", "seguro_salud", "grupo_ocup", "rama", "antiguedad", "carga_hogar", "menores_14", "mayores_60")
X_ML <- predictores_ok(base, "yA", X_ML)
base <- base |> filter(if_all(all_of(X_ML), ~ !is.na(.x)))
OBJ <- c(A = "yA", B = "yB")
NOMBRE_OBJ <- c(A = "A Persona (ylab < SMN)", B = "B Hogar (hogar pobre)")
cat("Predictores:", paste(X_ML, collapse = ", "), "\n")

# Particion 70/30 estratificada por la combinacion (yA, yB): el test no se
# usa para nada hasta la evaluacion final
idx_tr <- unlist(lapply(split(seq_len(nrow(base)), paste(base$yA, base$yB)), function(i) sample(i, round(0.7 * length(i)))))
train <- base[idx_tr, ]; test <- base[-idx_tr, ]
bitacora(X_ML, "Preparacion de la matriz de modelado", "Particion 70/30 estratificada por (yA, yB) + validacion cruzada 5-fold en el entrenamiento",
         sprintf("Train %s, test %s | prevalencia A %.1f%%, B %.1f%%", fmt(nrow(train)), fmt(nrow(test)), 100 * mean(base$yA), 100 * mean(base$yB)),
         "Sin fuga: ingresos, z, p0 y yhogpc no son predictores. Las horas si entran: ningun enfoque se define con ellas",
         nrow(base), sprintf("%d predictores de las secciones 1, 3 y 4", length(X_ML)), base, "split(paste(yA, yB)); sample()")

# ---------------------------------------------------------------------
# 10.1 Comparacion de 6 modelos por validacion cruzada (para A y B)
# ---------------------------------------------------------------------
cv_res <- list(); finales <- list(); test_res <- list(); rocs <- list(); calib <- list(); conf <- list()
for (o in names(OBJ)) {
  cat(sprintf("\n  Objetivo %s\n", NOMBRE_OBJ[[o]]))
  tr <- train; tr$y <- tr[[OBJ[[o]]]]; te <- test; te$y <- te[[OBJ[[o]]]]
  cv_res[[o]] <- comparar_modelos(tr, OBJ[[o]], X_ML)
  finales[[o]] <- list()
  for (mo in names(MODELOS)) {
    set.seed(CFG$semilla)
    finales[[o]][[mo]] <- MODELOS[[mo]]$fit(tr, X_ML)
    p <- MODELOS[[mo]]$pred(finales[[o]][[mo]], te, X_ML)
    u <- cv_res[[o]]$umbrales[[mo]]
    test_res[[length(test_res) + 1]] <- cbind(objetivo = o, modelo = mo, metricas_bin(te$y, p, u))
    rocs[[length(rocs) + 1]] <- cbind(objetivo = o, modelo = mo, curva_roc(te$y, p))
    calib[[length(calib) + 1]] <- cbind(objetivo = o, modelo = mo, curva_calibracion(te$y, p))
  }
  r <- cv_res[[o]]$resumen
  bitacora(paste0("vul_", c(A = "persona", B = "hogar")[[o]]), "Seleccion de modelo",
           sprintf("comparar_modelos(): %d modelos, CV %d-fold estratificada, AUC media +- EE; regla de 1 error estandar", length(MODELOS), CFG$k_cv),
           sprintf("%s | mejor AUC: %s (%.3f) | elegido: %s", NOMBRE_OBJ[[o]], cv_res[[o]]$mejor, r$auc_media[1], cv_res[[o]]$elegido),
           "Mismos pliegues para todos; umbral de Youden sobre predicciones fuera de pliegue; el test no interviene",
           nrow(tr), paste(sprintf("%s %.3f", r$modelo, r$auc_media), collapse = " | "),
           codigo = "comparar_modelos(train, y, X_ML)", archivo = "F10_cv_resumen.csv")
}
test_res <- do.call(rbind, test_res); rocs <- do.call(rbind, rocs); calib <- do.call(rbind, calib)
cv_resumen <- bind_rows(lapply(names(cv_res), function(o) cbind(objetivo = o, cv_res[[o]]$resumen)))
cv_pliegues <- bind_rows(lapply(names(cv_res), function(o) cbind(objetivo = o, cv_res[[o]]$cv)))
cv_pruebas <- bind_rows(lapply(names(cv_res), function(o) cbind(objetivo = o, cv_res[[o]]$pruebas)))
print(cv_resumen |> select(objetivo, modelo, auc_media, auc_de, brier_media, exactitud_bal_youden, rol) |> mutate(across(where(is.numeric), ~ round(.x, 4))))
print(test_res |> select(objetivo, modelo, auc, exactitud_balanceada, sensibilidad, especificidad, f1, brier) |> mutate(across(where(is.numeric), ~ round(.x, 3))))
guardar_tabla(cv_resumen, "cv_resumen"); guardar_tabla(cv_pliegues, "cv_por_pliegue"); guardar_tabla(cv_pruebas, "cv_pruebas_pareadas")
guardar_tabla(test_res, "test_metricas"); guardar_tabla(rocs, "test_roc"); guardar_tabla(calib, "test_calibracion")
res_cls <- test_res
bitacora(c("vul_persona", "vul_hogar"), "Evaluacion final", "Conjunto de prueba (30%) nunca visto; umbral fijado antes con los pliegues",
         "Metricas: AUC, exactitud balanceada, sensibilidad, especificidad, precision, F1, Brier y log-loss",
         "El test confirma (o no) lo que dijo la validacion cruzada", nrow(test),
         paste(sprintf("%s %s AUC %.3f", test_res$objetivo, test_res$modelo, test_res$auc)[test_res$modelo %in% c(cv_res$A$mejor, cv_res$B$mejor)], collapse = " | "),
         codigo = "metricas_bin(te$y, p, umbral_youden)", archivo = "F10_test_metricas.csv")

# Importancia por permutacion del mejor modelo de cada objetivo
importancia <- bind_rows(lapply(names(OBJ), function(o) {
  mo <- cv_res[[o]]$mejor; te <- test; te$y <- te[[OBJ[[o]]]]
  te <- te[sample(nrow(te), min(3000, nrow(te))), ]
  cbind(objetivo = o, modelo = mo, importancia_permutacion(mo, finales[[o]][[mo]], te, X_ML))
})) |> group_by(objetivo) |> mutate(rel = pmax(caida_auc, 0) / sum(pmax(caida_auc, 0))) |> ungroup()
guardar_tabla(importancia, "importancia_permutacion")
print(importancia |> group_by(objetivo) |> slice_max(caida_auc, n = 5))
bitacora(X_ML, "Interpretacion del modelo", "Importancia por permutacion (caida de AUC en el test al desordenar cada variable)",
         paste(sapply(names(OBJ), function(o) { t <- importancia |> filter(objetivo == o) |> arrange(desc(caida_auc))
           sprintf("%s: %s", o, paste(head(t$variable, 3), collapse = ", ")) }), collapse = " | "),
         "Independiente del modelo; mide cuanto depende la prediccion de cada variable", nrow(test), "",
         codigo = "importancia_permutacion()", archivo = "F10_importancia_permutacion.csv")

# Odds ratios de la logistica (interpretacion)
or_tab <- bind_rows(lapply(names(OBJ), function(o) {
  m <- finales[[o]][["Regresion logistica"]]
  as.data.frame(summary(m)$coefficients) |> tibble::rownames_to_column("termino") |> setNames(c("termino", "b", "ee", "z", "p")) |>
    filter(termino != "(Intercept)", !termino %in% c("edad2")) |>
    mutate(objetivo = o, or = exp(b), li = exp(b - 1.96 * ee), ls = exp(b + 1.96 * ee))
}))
guardar_tabla(or_tab, "logistica_odds_ratios")

# ---------------------------------------------------------------------
# 10.2 Simulador: logistica compacta (10 variables) exportada a JSON.
# Se elige la logistica porque sus coeficientes se pueden auditar y
# calcular en el navegador; se reporta su AUC para ser transparentes.
# ---------------------------------------------------------------------
X_SIM <- intersect(c("sexo", "edad", "edad2", "aestudio", "zona", "departamento", "cat_ocupacional", "grupo_ocup", "tothrs", "informal", "carga_hogar"), X_ML)
SIM <- list(variables = X_SIM, smn = CFG$smn, modelos = list())
sim_eval <- list()
for (o in names(OBJ)) {
  tr <- train; tr$y <- tr[[OBJ[[o]]]]; te <- test; te$y <- te[[OBJ[[o]]]]
  m <- glm(reformulate(X_SIM, "y"), data = tr, family = binomial())
  p <- predict(m, te, type = "response"); u <- umbral_youden(tr$y, predict(m, tr, type = "response"))
  sim_eval[[o]] <- cbind(objetivo = o, modelo = "Logistica del simulador", metricas_bin(te$y, p, u))
  b <- coef(m); b[is.na(b)] <- 0
  fac <- X_SIM[vapply(X_SIM, function(v) is.factor(tr[[v]]), logical(1))]
  SIM$modelos[[o]] <- list(nombre = NOMBRE_OBJ[[o]], intercepto = unname(b["(Intercept)"]), umbral = u,
    auc_test = auc_rank(te$y, p), prevalencia = mean(tr$y),
    numericas = as.list(b[intersect(names(b), setdiff(X_SIM, fac))]),
    factores = lapply(setNames(fac, fac), function(v) { lv <- levels(tr[[v]])
      setNames(as.list(vapply(lv, function(l) { k <- paste0(v, l); if (k %in% names(b)) unname(b[k]) else 0 }, numeric(1))), lv) }))
  # Verificacion: la formula exportada reproduce predict()
  chk <- te[1:200, ]; eta <- rep(SIM$modelos[[o]]$intercepto, nrow(chk))
  for (v in names(SIM$modelos[[o]]$numericas)) eta <- eta + SIM$modelos[[o]]$numericas[[v]] * chk[[v]]
  for (v in fac) eta <- eta + unlist(SIM$modelos[[o]]$factores[[v]])[as.character(chk[[v]])]
  stopifnot(max(abs(plogis(eta) - predict(m, chk, type = "response"))) < 1e-8)
}
SIM$niveles <- lapply(setNames(X_SIM, X_SIM), function(v) if (is.factor(train[[v]])) levels(train[[v]]) else range(train[[v]]))
sim_eval <- do.call(rbind, sim_eval)
guardar_tabla(sim_eval, "simulador_metricas")
jsonlite::write_json(SIM, file.path(DIR$mod, "simulador_logit.json"), auto_unbox = TRUE, digits = 10, pretty = TRUE)
saveRDS(finales, file.path(DIR$mod, "modelos_clasificacion.rds"))
bitacora(X_SIM, "Despliegue del modelo", "Regresion logistica compacta exportada a JSON (coeficientes y umbral de Youden)",
         sprintf("Simulador: AUC test A %.3f, B %.3f", sim_eval$auc[1], sim_eval$auc[2]),
         "La formula en el navegador reproduce predict() de R (diferencia < 1e-8)", nrow(train), "salidas/05_modelos/simulador_logit.json",
         codigo = "jsonlite::write_json(SIM)", archivo = "simulador_logit.json")

# ---------------------------------------------------------------------
# 10.3 Balanceo de clases (enfoque B, 30% de prevalencia): el modelo
# original vs submuestreo aleatorio de la clase mayoritaria vs pesos
# ---------------------------------------------------------------------
trB <- train; trB$y <- trB$yB; teB <- test; teB$y <- teB$yB
i1 <- which(trB$y == 1); i0 <- which(trB$y == 0)
conjuntos <- list(Original = trB, `Submuestreo aleatorio` = trB[c(i1, sample(i0, length(i1))), ])
balanceo <- bind_rows(lapply(names(conjuntos), function(k) {
  m <- suppressWarnings(glm(reformulate(X_ML, "y"), data = conjuntos[[k]], family = binomial())); p <- predict(m, teB, type = "response")
  cbind(conjunto = k, n_train = nrow(conjuntos[[k]]), metricas_bin(teB$y, p, 0.5))
}), {
  wts <- ifelse(trB$y == 1, 0.5 / mean(trB$y), 0.5 / mean(1 - trB$y))
  m <- suppressWarnings(glm(reformulate(X_ML, "y"), data = trB, family = quasibinomial(), weights = wts)); p <- predict(m, teB, type = "response")
  cbind(conjunto = "Pesos por clase", n_train = nrow(trB), metricas_bin(teB$y, p, 0.5))
})
print(balanceo |> select(conjunto, n_train, auc, sensibilidad, especificidad, exactitud_balanceada))
guardar_tabla(balanceo, "balanceo_clases")
bitacora("vul_hogar", "Desbalance de clases (30% vs 70%)", "Logistica original vs submuestreo aleatorio vs pesos por clase (umbral 0.5)",
         paste(sprintf("%s: sens %.2f, esp %.2f", balanceo$conjunto, balanceo$sensibilidad, balanceo$especificidad), collapse = " | "),
         "El balanceo mueve el punto de corte, no la capacidad de ordenar (AUC casi igual); el umbral de Youden logra lo mismo sin descartar datos",
         nrow(trB), "", codigo = "sample(i0, length(i1)); weights", archivo = "F10_balanceo_clases.csv")

# ---------------------------------------------------------------------
# 10.4 Regresion del ingreso laboral
# a) Ecuacion de Mincer clasica: ln(ylab) = b0 + b1 educ + b2 exp + b3 exp^2
#    + controles (exp = edad - educ - 6, experiencia potencial)
# b) Comparacion por CV 5-fold: Mincer extendida (lm) vs Random Forest
# ---------------------------------------------------------------------
rg <- df |> mutate(.fila = row_number()) |> filter(pob_estudio, ylab > 0, tothrs > 0, !is.na(aestudio)) |>
  mutate(ly = log(ylab), exper = pmax(edad - aestudio - 6, 0), exper2 = exper^2, lhoras = log(tothrs),
         sexo = droplevels(factor(sexo)), zona = droplevels(factor(zona)), departamento = factor(departamento, levels = DEPTOS))
m_clasica <- lm(ly ~ aestudio + exper + exper2, data = rg, weights = w)
m_ext <- lm(ly ~ aestudio + exper + exper2 + sexo + zona + lhoras + departamento, data = rg, weights = w)
mincer_tab <- bind_rows(lapply(list(Clasica = m_clasica, Extendida = m_ext), function(m)
  as.data.frame(summary(m)$coefficients) |> tibble::rownames_to_column("termino") |> setNames(c("termino", "b", "ee", "t", "p")) |>
    mutate(efecto_pct = 100 * (exp(b) - 1), li = 100 * (exp(b - 1.96 * ee) - 1), ls = 100 * (exp(b + 1.96 * ee) - 1),
           r2_aj = summary(m)$adj.r.squared, n = nobs(m))), .id = "modelo")
mincer_grupo <- bind_rows(c(
  lapply(c("Hombre", "Mujer"), function(s) {
    m <- lm(ly ~ aestudio + exper + exper2 + zona + lhoras, data = rg |> filter(sexo == .env$s), weights = w)
    data.frame(grupo = s, retorno_educ = 100 * (exp(coef(m)[["aestudio"]]) - 1), r2_aj = summary(m)$adj.r.squared, n = nobs(m)) }),
  lapply(c("Urbana", "Rural"), function(z) {
    m <- lm(ly ~ aestudio + exper + exper2 + sexo + lhoras, data = rg |> filter(zona == .env$z), weights = w)
    data.frame(grupo = z, retorno_educ = 100 * (exp(coef(m)[["aestudio"]]) - 1), r2_aj = summary(m)$adj.r.squared, n = nobs(m)) })))
# Perfil edad-ingreso implicito (Mincer clasica)
perfil_mincer <- data.frame(exper = 0:45) |> mutate(aestudio = 12, exper2 = exper^2) |>
  mutate(ingreso = exp(predict(m_clasica, pick(everything()))), pico = exper[which.max(ingreso)])
print(mincer_tab |> filter(termino %in% c("aestudio", "exper", "sexoMujer", "zonaRural", "lhoras")) |> select(modelo, termino, b, efecto_pct, r2_aj))
print(mincer_grupo)
guardar_tabla(mincer_tab, "mincer_coeficientes"); guardar_tabla(mincer_grupo, "mincer_por_grupo"); guardar_tabla(perfil_mincer, "mincer_perfil_experiencia")
ret <- mincer_tab |> filter(termino == "aestudio")
bitacora(c("ylab", "aestudio", "edad"), "Retorno de la educacion", "Ecuacion de Mincer por MCO ponderado (clasica y extendida), por sexo y area",
         sprintf("Retorno por anio de estudio: %.1f%% (clasica), %.1f%% (extendida); R2 aj %.3f / %.3f", ret$efecto_pct[1], ret$efecto_pct[2], ret$r2_aj[1], ret$r2_aj[2]),
         "exp = edad - anios de estudio - 6 (experiencia potencial); b de exp^2 < 0: el ingreso crece y luego se estabiliza",
         nrow(rg), sprintf("Mujeres %.1f%% vs hombres %.1f%% por anio", mincer_grupo$retorno_educ[2], mincer_grupo$retorno_educ[1]),
         codigo = "lm(log(ylab) ~ aestudio + exper + I(exper^2) + ...)", archivo = "F10_mincer_coeficientes.csv")

f_rg <- c("aestudio", "exper", "exper2", "sexo", "zona", "lhoras", "departamento")
f_rf <- c("aestudio", "edad", "tothrs", "sexo", "zona", "departamento")
rg2 <- rg |> select(-any_of(c("cat_ocupacional", "grupo_ocup", "rama", "informal", "tamano_empresa"))) |>
  inner_join(base |> select(.fila, cat_ocupacional, grupo_ocup, rama, informal, tamano_empresa), by = ".fila")
fold_rg <- pliegues(rep(1, nrow(rg2)), CFG$k_cv, CFG$semilla)
X_RF <- c(f_rf, "cat_ocupacional", "grupo_ocup", "rama", "informal", "tamano_empresa")
cv_rg <- bind_rows(lapply(seq_len(CFG$k_cv), function(f) {
  tr <- rg2[fold_rg != f, ]; te <- rg2[fold_rg == f, ]
  m1 <- lm(reformulate(f_rg, "ly"), data = tr); p1 <- predict(m1, te)
  m2 <- lm(reformulate(c(f_rg, "cat_ocupacional", "grupo_ocup", "rama", "informal", "tamano_empresa"), "ly"), data = tr); p2 <- predict(m2, te)
  m3 <- ranger::ranger(reformulate(X_RF, "ly"), data = tr, num.trees = 300, min.node.size = 10, seed = CFG$semilla, num.threads = 2); p3 <- predict(m3, te)$predictions
  r2 <- function(p) 1 - sum((te$ly - p)^2) / sum((te$ly - mean(te$ly))^2)
  data.frame(pliegue = f, modelo = c("Mincer extendida (lm)", "Mincer + empleo (lm)", "Random Forest"),
             r2 = c(r2(p1), r2(p2), r2(p3)), rmse_log = sqrt(c(mean((te$ly - p1)^2), mean((te$ly - p2)^2), mean((te$ly - p3)^2))),
             mae_bs = c(median(abs(exp(te$ly) - exp(p1))), median(abs(exp(te$ly) - exp(p2))), median(abs(exp(te$ly) - exp(p3)))))
}))
eval_rg <- cv_rg |> group_by(modelo) |> summarise(r2 = mean(r2), r2_de = sd(cv_rg$r2[cv_rg$modelo == first(modelo)]),
                                                  rmse_log = mean(rmse_log), mae_mediano_bs = mean(mae_bs)) |> arrange(desc(r2))
print(eval_rg)
guardar_tabla(eval_rg, "regresion_ingreso_metricas"); guardar_tabla(cv_rg, "regresion_ingreso_cv")
bitacora("ylab", "Prediccion de una variable continua", "CV 5-fold: Mincer extendida (lm), Mincer + variables de empleo (lm) y Random Forest",
         paste(sprintf("%s R2 %.3f", eval_rg$modelo, eval_rg$r2), collapse = " | "),
         "R2 sobre log(ylab) fuera de pliegue; error absoluto mediano en Bs", nrow(rg2), "", codigo = "lm(); ranger()", archivo = "F10_regresion_ingreso_metricas.csv")
lm_rg <- lm(reformulate(f_rg, "ly"), data = rg2[fold_rg != 1, ]); te_rg <- rg2[fold_rg == 1, ]

# Ocupacion y salario: mediana ponderada por grupo ocupacional y sexo
ocup_salario <- df |> filter(pob_estudio, ylab > 0, !is.na(grupo_ocup), !is.na(sexo)) |>
  group_by(grupo_ocup, sexo) |> summarise(n = n(), poblacion = sum(w), mediana = wquant(ylab, w, 0.5), p25 = wquant(ylab, w, 0.25),
                                          p75 = wquant(ylab, w, 0.75), ingreso_hora = wquant(ingreso_hora, w, 0.5),
                                          bajo_smn = wmean(ylab < CFG$smn, w), .groups = "drop")
guardar_tabla(ocup_salario, "ocupacion_salario")

# ---------------------------------------------------------------------
# 10.5 Agrupamiento: k-means sobre ocupados de 19+
# ---------------------------------------------------------------------
cl <- df |> filter(pob_estudio) |> transmute(.fila = which(df$pob_estudio), w, vulnerable, vul_hogar, departamento, zona, sexo,
                                             educacion = aestudio, edad, ingreso = log1p(ylab_w), horas = tothrs_w, ingreso_hogar = log1p(yhogpc_w)) |>
  filter(if_all(c(educacion, edad, ingreso, horas, ingreso_hogar), ~ !is.na(.x)))
vars_cl <- c("educacion", "edad", "ingreso", "horas", "ingreso_hogar")
Z <- scale(cl[vars_cl])
codo_cl <- data.frame(k = 2:7, wss = sapply(2:7, function(k) { set.seed(CFG$semilla); kmeans(Z, k, nstart = 10, iter.max = 50)$tot.withinss }))
s_idx <- sample(nrow(Z), min(2000, nrow(Z))); dz <- dist(Z[s_idx, ])
codo_cl$silueta <- sapply(codo_cl$k, function(k) { set.seed(CFG$semilla); km <- kmeans(Z, k, nstart = 10, iter.max = 50)
  mean(cluster::silhouette(km$cluster[s_idx], dz)[, 3]) })
K_CL <- codo_cl$k[which.max(codo_cl$silueta)]
set.seed(CFG$semilla); km <- kmeans(Z, K_CL, nstart = 25, iter.max = 100)
cl$cluster <- km$cluster
etiq_alto <- c(educacion = "Educacion alta", edad = "Mayor edad", ingreso = "Ingreso alto", horas = "Jornada larga", ingreso_hogar = "Hogar de ingreso alto")
etiq_bajo <- c(educacion = "Educacion baja", edad = "Jovenes", ingreso = "Ingreso bajo", horas = "Jornada corta", ingreso_hogar = "Hogar de ingreso bajo")
nombre_cl <- apply(km$centers, 1, function(z) { o <- order(-abs(z))[1:2]
  paste(sapply(o, function(j) { v <- colnames(km$centers)[j]; ifelse(z[j] > 0, etiq_alto[v], etiq_bajo[v]) }), collapse = " / ") })
cl$perfil <- factor(paste0("G", cl$cluster, ": ", nombre_cl[cl$cluster]))
perfil_cl <- cl |> group_by(perfil) |> summarise(n = n(), poblacion = sum(w), educacion = wmean(educacion, w), edad = wmean(edad, w),
  ingreso_bs = wmean(expm1(ingreso), w), horas = wmean(horas, w), tasa_vulnerable = wmean(vulnerable == "Vulnerable", w),
  tasa_hogar = wmean(vul_hogar == "Vulnerable", w),
  pct_mujer = wmean(sexo == "Mujer", w), pct_rural = wmean(zona == "Rural", w))
print(perfil_cl)
guardar_tabla(perfil_cl, "clusters_perfil"); guardar_tabla(codo_cl, "clusters_codo_silueta")
df$cluster <- NA_character_; df$cluster[cl$.fila] <- as.character(cl$perfil)
bitacora(vars_cl, "Segmentacion de ocupados", sprintf("k-means sobre variables estandarizadas; k elegido por silueta (k = %d)", K_CL),
         "Nueva columna cluster con nombre interpretable", "Codo + coeficiente de silueta en muestra de 2000", nrow(cl),
         paste(sprintf("%s (A %.0f%%, B %.0f%%)", perfil_cl$perfil, 100 * perfil_cl$tasa_vulnerable, 100 * perfil_cl$tasa_hogar), collapse = " | "), df,
         "kmeans(scale(X), k, nstart = 25)", "F10_clusters_perfil.csv")

# Muestra anonima para la nube 3D de la pagina (sin identificadores;
# ingreso redondeado a Bs 50 y horas a 5)
set.seed(CFG$semilla)
cl_muestra <- cl[sample(nrow(cl), min(1500, nrow(cl))), ] |>
  transmute(perfil, educacion, edad, ingreso_bs = 50 * round(expm1(ingreso) / 50), horas = 5 * round(horas / 5),
            vul_A = vulnerable == "Vulnerable", vul_B = vul_hogar == "Vulnerable")
guardar_tabla(cl_muestra, "clusters_muestra_3d")

pca <- prcomp(Z); var_pc <- summary(pca)$importance[2, 1:3]
cl$PC1 <- pca$x[, 1]; cl$PC2 <- pca$x[, 2]; cl$PC3 <- pca$x[, 3]
if (TIENE("plotly") && TIENE("htmlwidgets")) {
  tryCatch({
    s3 <- cl[sample(nrow(cl), min(4000, nrow(cl))), ]
    w3 <- plotly::plot_ly(s3, x = ~PC1, y = ~PC2, z = ~PC3, color = ~perfil, colors = DISC[seq_len(K_CL)], type = "scatter3d", mode = "markers",
                          marker = list(size = 2.5, opacity = 0.75)) |>
      plotly::layout(title = list(text = "Clusters de ocupados en 3D (componentes principales)", font = list(color = PAL$txt)),
                     paper_bgcolor = PAL$bg, font = list(color = PAL$txt),
                     scene = list(bgcolor = PAL$bg, xaxis = list(color = PAL$sub), yaxis = list(color = PAL$sub), zaxis = list(color = PAL$sub)))
    htmlwidgets::saveWidget(w3, file.path(GRAF$dir, "F10_3D_clusters_interactivo.html"), selfcontained = TRUE)
    cat("  [grafico 3D] F10_3D_clusters_interactivo.html\n")
  }, error = function(e) message("  3D interactivo omitido: ", conditionMessage(e)))
}

# ---------------------------------------------------------------------
# 10.6 Reglas de asociacion ponderadas: X -> Vulnerable / No vulnerable
# soporte = P(X y Y), confianza = P(Y | X), lift = confianza / P(Y)
# ---------------------------------------------------------------------
reglas_para <- function(ycol, etiqueta) {
  tr_ar <- df |> filter(pob_estudio, !is.na(.data[[ycol]])) |>
    transmute(w, Y = as.character(.data[[ycol]]), sexo = as.character(sexo), area = as.character(zona), region = as.character(region),
              educacion = as.character(nivel_educ), edad = as.character(edad_grupo), jefe = ifelse(jefe, "jefe", "no jefe"),
              categoria = as.character(cat_ocupacional), informal = ifelse(informal %in% TRUE, "no aporta", "aporta"))
  items <<- setdiff(names(tr_ar), c("w", "Y")); W <- sum(tr_ar$w); py <- wmean(tr_ar$Y == "Vulnerable", tr_ar$w)
  bind_rows(lapply(c(lapply(items, identity), combn(items, 2, simplify = FALSE)), function(cols) {
    s <- tr_ar |> filter(if_all(all_of(cols), ~ !is.na(.x))) |> group_by(across(all_of(cols))) |>
      summarise(sup_x = sum(w) / W, soporte = sum(w * (Y == "Vulnerable")) / W, n = n(), .groups = "drop")
    s$antecedente <- do.call(paste, c(lapply(cols, function(cc) paste0(cc, "=", s[[cc]])), sep = " & "))
    s[c("antecedente", "sup_x", "soporte", "n")]
  })) |> mutate(consecuente = etiqueta, p_y = py, confianza = soporte / sup_x, lift = confianza / p_y) |>
    filter(soporte >= 0.02, n >= 30, lift > 1) |> arrange(desc(lift))
}
reglas <- bind_rows(reglas_para("vul_persona", "A Persona vulnerable"), reglas_para("vul_hogar", "B Hogar vulnerable"))
tr_ar <- df |> filter(pob_estudio)
print(head(reglas |> mutate(across(where(is.numeric), ~ round(.x, 3))), 15))
guardar_tabla(reglas, "reglas_asociacion")
bitacora(items, "Patrones de co-ocurrencia", "Reglas de asociacion ponderadas por factor (antecedentes de 1 y 2 items)",
         sprintf("%d reglas con soporte >= 2%%, n >= 30 y lift > 1", nrow(reglas)), "Soporte, confianza y lift; ponderado (arules no admite pesos de encuesta); consecuente = vulnerable segun A o B",
         nrow(tr_ar), ifelse(nrow(reglas) > 0, sprintf("Regla top: %s -> %s (lift %.2f)", reglas$antecedente[1], reglas$consecuente[1], reglas$lift[1]), ""),
         codigo = "group_by(items) |> summarise(soporte, confianza, lift)", archivo = "F10_reglas_asociacion.csv")

# ---------------------------------------------------------------------
# 10.7 Deteccion de anomalias multivariadas (Mahalanobis robusta MCD)
# ---------------------------------------------------------------------
an <- df |> mutate(.fila = row_number()) |> filter(pob_estudio, ylab > 0, !is.na(aestudio), !is.na(tothrs), !is.na(edad)) |>
  transmute(.fila, folio, nro, departamento, edad, aestudio, tothrs, ylab, ly = log(ylab), out_ylab, out_tothrs)
M <- as.matrix(an[c("ly", "tothrs", "aestudio", "edad")])
rob <- MASS::cov.rob(M[sample(nrow(M), min(5000, nrow(M))), ], method = "mcd")
an$d2 <- mahalanobis(M, rob$center, rob$cov)
lim_d2 <- qchisq(0.999, df = 4)
an$anomalia <- an$d2 > lim_d2
df$anomalia_multiv <- FALSE; df$anomalia_multiv[an$.fila] <- an$anomalia
cruce_anom <- an |> count(anomalia_multiv = anomalia, atipico_univariado = out_ylab | out_tothrs)
print(cruce_anom)
guardar_tabla(an |> filter(anomalia) |> arrange(desc(d2)) |> select(-.fila), "anomalias_multivariadas")
bitacora(c("ylab", "tothrs", "aestudio", "edad"), "Combinaciones atipicas", "Distancia de Mahalanobis con media y covarianza robustas (MCD)",
         "Marcado en anomalia_multiv", sprintf("D2 > chi2(0.999, 4) = %.1f", lim_d2), sum(an$anomalia),
         sprintf("%d anomalias multivariadas; %d no eran atipicos univariados", sum(an$anomalia), sum(an$anomalia & !(an$out_ylab | an$out_tothrs))), df,
         "MASS::cov.rob(method = 'mcd'); mahalanobis()", "F10_anomalias_multivariadas.csv")

# ---------------------------------------------------------------------
# 10.8 Patrones secuenciales / series temporales
# La EH es de corte transversal: no hay serie de tiempo. Se construye una
# pseudo serie por anio de nacimiento (cohortes).
# ---------------------------------------------------------------------
cohortes <- df |> filter(!dup_repetido, !is.na(edad), edad >= 25, edad <= 85) |> mutate(anio_nac = CFG$anio_eh - edad) |>
  group_by(anio_nac) |> summarise(educacion = wmean(aestudio, w), tasa_vulnerable = wmean(vulnerable == "Vulnerable", ifelse(is.na(vulnerable), NA, w)),
                                  tasa_hogar = wmean(vul_hogar == "Vulnerable", ifelse(is.na(vul_hogar), NA, w)),
                                  pct_rural = wmean(zona == "Rural", w), n = n())
guardar_tabla(cohortes, "cohortes")
bitacora("edad", "Ausencia de dimension temporal", "Pseudo serie por cohorte de nacimiento (25 a 85 anios)",
         "Educacion y vulnerabilidad por anio de nacimiento", "Patrones secuenciales en sentido estricto no aplican a un corte transversal", nrow(cohortes),
         sprintf("Educacion media: %.1f anios (cohorte %d) -> %.1f anios (cohorte %d)", cohortes$educacion[1], cohortes$anio_nac[1], tail(cohortes$educacion, 1), tail(cohortes$anio_nac, 1)),
         codigo = "group_by(anio_nac)", archivo = "F10_cohortes.csv")

# ---------------------------------------------------------------------
# Graficos
# ---------------------------------------------------------------------
COL_MOD <- setNames(c(PAL$sub, PAL$cyan, PAL$indigo, PAL$amber, PAL$mint, PAL$pink), names(MODELOS))
g <- cv_pliegues |> mutate(objetivo = NOMBRE_OBJ[objetivo])
gm <- cv_resumen |> mutate(objetivo = NOMBRE_OBJ[objetivo])
p <- ggplot(g, aes(auc, fct_reorder(modelo, auc), color = modelo)) +
  geom_errorbarh(data = gm, aes(xmin = auc_media - auc_de, xmax = auc_media + auc_de, y = modelo, x = NULL), height = 0.25, color = PAL$sub) +
  geom_jitter(height = 0.12, width = 0, size = 2, alpha = 0.6) + geom_point(data = gm, aes(x = auc_media, y = modelo), size = 5, shape = 18) +
  geom_text(data = gm, aes(x = auc_media, y = modelo, label = sprintf("%.3f %s", auc_media, rol)), vjust = -1.1, size = 3, color = PAL$txt) +
  facet_wrap(~objetivo) + scale_color_manual(values = COL_MOD, guide = "none") +
  labs(title = "Comparacion de modelos por validacion cruzada (5 pliegues)", subtitle = "Puntos = AUC de cada pliegue | rombo = media | barra = +- 1 DE",
       x = "AUC", y = NULL, caption = CAP)
guardar(p, "cv_comparacion_modelos", 13, 6.5)

g <- test_res |> mutate(objetivo = NOMBRE_OBJ[objetivo]) |> select(objetivo, modelo, AUC = auc, `Exactitud bal.` = exactitud_balanceada, Sensibilidad = sensibilidad,
                                                                 Especificidad = especificidad, F1 = f1) |> pivot_longer(-c(objetivo, modelo))
p <- ggplot(g, aes(name, modelo, fill = value)) + geom_tile(color = PAL$bg, linewidth = 1.2) + geom_text(aes(label = sprintf("%.3f", value)), color = "white", size = 3.2) +
  facet_wrap(~objetivo) + scale_fill_gradientn(colors = GRAD, guide = "none") +
  labs(title = "Metricas en el conjunto de prueba (30%, nunca visto)", subtitle = "Umbral de Youden fijado con los pliegues de entrenamiento", x = NULL, y = NULL, caption = CAP) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
guardar(p, "test_metricas", 13, 6)

p <- ggplot(rocs |> mutate(objetivo = NOMBRE_OBJ[objetivo]), aes(fpr, tpr, color = modelo)) + geom_abline(color = PAL$sub, linetype = "dashed") +
  geom_path(linewidth = 0.9) + facet_wrap(~objetivo) + scale_color_manual(values = COL_MOD, name = NULL) + coord_equal() +
  labs(title = "Curvas ROC en el conjunto de prueba", x = "1 - especificidad", y = "Sensibilidad", caption = CAP) + theme(legend.position = "right")
guardar(p, "curvas_roc", 13, 6)

g <- importancia |> mutate(objetivo = paste0(NOMBRE_OBJ[objetivo], " | ", modelo)) |> group_by(objetivo) |> slice_max(caida_auc, n = 10) |> ungroup()
p <- ggplot(g, aes(caida_auc, reorder(paste(variable, objetivo, sep = "___"), caida_auc), fill = caida_auc)) + geom_col(width = 0.65) +
  scale_y_discrete(labels = function(x) sub("___.*$", "", x)) + facet_wrap(~objetivo, scales = "free_y") +
  scale_fill_gradientn(colors = GRAD, guide = "none") +
  labs(title = "Importancia por permutacion del mejor modelo", subtitle = "Caida del AUC en el test al desordenar cada variable", x = "Caida de AUC", y = NULL, caption = CAP)
guardar(p, "importancia_permutacion", 13, 6.5)

p <- or_tab |> filter(objetivo == "A") |> mutate(termino = fct_reorder(termino, or)) |> slice_max(abs(log(or)), n = 25) |>
  ggplot(aes(or, termino, color = or > 1)) + geom_vline(xintercept = 1, color = PAL$sub, linetype = "dashed") +
  geom_errorbarh(aes(xmin = li, xmax = ls), height = 0.25) + geom_point(size = 3) + scale_x_log10() +
  scale_color_manual(values = c(`TRUE` = PAL$coral, `FALSE` = PAL$mint), guide = "none") +
  labs(title = "Regresion logistica (A persona): odds ratios", subtitle = "OR > 1 aumenta la probabilidad de ganar menos del SMN | IC 95%", x = "Odds ratio (log)", y = NULL, caption = CAP)
guardar(p, "odds_ratios", 11, 9)

p <- ggplot(balanceo |> select(conjunto, Sensibilidad = sensibilidad, Especificidad = especificidad, AUC = auc) |> pivot_longer(-conjunto),
            aes(value, conjunto, color = name)) + geom_line(aes(group = conjunto), color = PAL$grid, linewidth = 2.5) + geom_point(size = 4.5) +
  scale_color_manual(values = c(Sensibilidad = PAL$coral, Especificidad = PAL$cyan, AUC = PAL$amber), name = NULL) +
  labs(title = "Balanceo de clases (enfoque B, 30% de prevalencia)", subtitle = "Balancear cambia el punto de corte (sensibilidad vs especificidad), no el AUC",
       x = NULL, y = NULL, caption = CAP)
guardar(p, "balanceo_clases", 11, 5)

g <- mincer_tab |> filter(modelo == "Extendida", termino != "(Intercept)", !termino %in% c("exper2")) |> mutate(termino = fct_reorder(termino, efecto_pct))
p <- ggplot(g, aes(efecto_pct, termino, color = efecto_pct > 0)) + geom_vline(xintercept = 0, color = PAL$sub, linetype = "dashed") +
  geom_errorbarh(aes(xmin = li, xmax = ls), height = 0.25) + geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%+.1f%%", efecto_pct)), vjust = -0.9, size = 3, color = PAL$txt) +
  scale_color_manual(values = c(`TRUE` = PAL$mint, `FALSE` = PAL$coral), guide = "none") +
  labs(title = "Ecuacion de Mincer extendida: efecto sobre el ingreso laboral", subtitle = sprintf("100 x (exp(b) - 1) | R2 ajustado %.3f | n = %s", ret$r2_aj[2], fmt(ret$n[2])),
       x = "Efecto porcentual sobre el ingreso", y = NULL, caption = CAP)
guardar(p, "mincer_extendida", 11, 7)

p <- ggplot(perfil_mincer, aes(exper, ingreso)) + geom_line(color = PAL$cyan, linewidth = 1.4) +
  geom_vline(xintercept = perfil_mincer$pico[1], color = PAL$amber, linetype = "dashed") +
  annotate("text", x = perfil_mincer$pico[1], y = min(perfil_mincer$ingreso), label = paste("pico a los", perfil_mincer$pico[1], "anios de experiencia"), hjust = -0.05, color = PAL$amber, size = 3.4) +
  scale_y_continuous(labels = lab_bs) +
  labs(title = "Perfil experiencia-ingreso implicito en la ecuacion de Mincer", subtitle = "Persona con 12 anios de estudio (bachiller) | curva concava por el termino exp^2", x = "Anios de experiencia potencial", y = "Ingreso laboral predicho", caption = CAP)
guardar(p, "mincer_perfil", 10, 6)

p <- ggplot(cv_rg, aes(r2, modelo, color = modelo)) + geom_jitter(height = 0.1, size = 3, alpha = 0.7) +
  geom_point(data = eval_rg, aes(r2, modelo), shape = 18, size = 6) + scale_color_manual(values = c(PAL$cyan, PAL$violet, PAL$mint), guide = "none") +
  labs(title = "Prediccion del ingreso laboral: R2 fuera de pliegue", subtitle = "Puntos = pliegues | rombo = media", x = "R2 (log ingreso)", y = NULL, caption = CAP)
guardar(p, "regresion_ingreso_cv", 10, 5)

g <- ocup_salario |> mutate(grupo_ocup = fct_reorder(grupo_ocup, mediana))
p <- ggplot(g, aes(mediana, grupo_ocup, color = sexo)) + geom_vline(xintercept = CFG$smn, color = PAL$amber, linetype = "dashed") +
  geom_line(aes(group = grupo_ocup), color = PAL$grid, linewidth = 2.5) + geom_point(size = 4) + scale_color_manual(values = COL_SEXO, name = NULL) +
  scale_x_continuous(labels = lab_bs) +
  labs(title = "Ingreso laboral mediano por grupo ocupacional y sexo", subtitle = "Ocupados de 19+ con ingreso | linea = SMN 2024", x = NULL, y = NULL, caption = CAP)
guardar(p, "ocupacion_salario", 11, 6.5)

p <- (ggplot(codo_cl, aes(k, wss)) + geom_line(color = PAL$violet, linewidth = 1.1) + geom_point(color = PAL$violet, size = 3.5) + labs(title = "Codo", x = "k", y = "WSS")) +
  (ggplot(codo_cl, aes(k, silueta)) + geom_line(color = PAL$cyan, linewidth = 1.1) + geom_point(aes(color = k == K_CL), size = 4) +
     scale_color_manual(values = c(`TRUE` = PAL$amber, `FALSE` = PAL$cyan), guide = "none") + labs(title = "Silueta", x = "k", y = "Silueta media")) +
  plot_annotation(title = "Eleccion del numero de grupos", subtitle = sprintf("k = %d maximiza el coeficiente de silueta", K_CL), caption = CAP, theme = theme_eh())
guardar(p, "clusters_codo_silueta", 12, 5.5)

cen <- cl |> group_by(perfil) |> summarise(PC1 = mean(PC1), PC2 = mean(PC2))
p <- ggplot(cl |> slice_sample(n = min(8000, nrow(cl))), aes(PC1, PC2, color = perfil)) + geom_point(alpha = 0.35, size = 1) +
  geom_label(data = cen, aes(label = str_wrap(perfil, 22)), fill = PAL$panel, size = 3, show.legend = FALSE) +
  scale_color_manual(values = DISC, name = NULL) + guides(color = guide_legend(override.aes = list(size = 4, alpha = 1), ncol = 2)) +
  labs(title = "Segmentos de ocupados (k-means)", subtitle = sprintf("Proyeccion en componentes principales | PC1 %.0f%% y PC2 %.0f%% de la varianza", 100 * var_pc[1], 100 * var_pc[2]),
       caption = CAP)
guardar(p, "clusters_pca", 11, 8)

g <- perfil_cl |> select(perfil, educacion, edad, ingreso_bs, horas, tasa_vulnerable, pct_mujer, pct_rural) |> pivot_longer(-perfil) |>
  group_by(name) |> mutate(rel = esc01(value),
                           etq = ifelse(name %in% c("tasa_vulnerable", "pct_mujer", "pct_rural"), pct(value, 0), ifelse(name == "ingreso_bs", paste("Bs", fmt(value)), fmt(value, 1))))
p <- ggplot(g, aes(name, str_wrap(perfil, 30), fill = rel)) + geom_tile(color = PAL$bg, linewidth = 1.2) + geom_text(aes(label = etq), color = "white", size = 3.3) +
  scale_fill_gradientn(colors = GRAD, guide = "none") +
  labs(title = "Perfil de cada segmento", subtitle = "Promedios ponderados | color = posicion relativa entre segmentos", x = NULL, y = NULL, caption = CAP) +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
guardar(p, "clusters_perfil", 12, 6)

if (nrow(reglas)) {
  p <- ggplot(reglas, aes(soporte, confianza, size = lift, color = consecuente)) + geom_point(alpha = 0.7) +
    geom_text(data = reglas |> group_by(consecuente) |> slice_max(lift, n = 4), aes(label = str_wrap(antecedente, 30)), size = 2.8, color = PAL$txt, vjust = -1.2) +
    scale_color_manual(values = c(`A Persona vulnerable` = PAL$coral, `B Hogar vulnerable` = PAL$cyan), name = "Consecuente") + scale_size(range = c(1.5, 9), name = "Lift") + guides(size = guide_legend(override.aes = list(color = PAL$sub))) +
    scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
    labs(title = "Reglas de asociacion: soporte, confianza y lift", subtitle = sprintf("%d reglas con lift > 1 | ponderadas por factor de expansion", nrow(reglas)),
         x = "Soporte", y = "Confianza", caption = CAP) + theme(legend.position = "right")
  guardar(p, "reglas_dispersion", 12, 7)
  g <- reglas |> group_by(consecuente) |> slice_max(lift, n = 8) |> ungroup()
  p <- ggplot(g, aes(lift, fct_reorder(antecedente, lift), color = consecuente)) + geom_vline(xintercept = 1, color = PAL$sub, linetype = "dashed") +
    geom_segment(aes(x = 1, xend = lift, yend = fct_reorder(antecedente, lift)), linewidth = 1.8, alpha = 0.6) + geom_point(size = 4) +
    geom_text(aes(label = sprintf("conf %.0f%% | sop %.1f%%", 100 * confianza, 100 * soporte)), hjust = -0.15, size = 2.9, color = PAL$txt) +
    facet_wrap(~consecuente, scales = "free_y", ncol = 1) + scale_color_manual(values = c(`A Persona vulnerable` = PAL$coral, `B Hogar vulnerable` = PAL$cyan), guide = "none") + scale_x_continuous(expand = expansion(mult = c(0, 0.45))) +
    labs(title = "Reglas mas fuertes hacia cada clase", subtitle = "Lift > 1: el antecedente hace mas probable la clase", x = "Lift", y = NULL, caption = CAP) +
    theme(axis.text.y = element_text(size = 8))
  guardar(p, "reglas_top", 12, 9)
}

p <- ggplot(an |> slice_sample(n = min(8000, nrow(an))), aes(aestudio, ylab)) + geom_jitter(aes(color = anomalia), width = 0.3, alpha = 0.5, size = 1.2) +
  geom_point(data = an |> filter(anomalia), color = PAL$coral, size = 2.2) +
  scale_y_log10(labels = lab_bs) + scale_color_manual(values = c(`FALSE` = PAL$indigo, `TRUE` = PAL$coral), labels = c("Normal", "Anomalia"), name = NULL) +
  labs(title = "Anomalias multivariadas", subtitle = sprintf("Mahalanobis robusta sobre ingreso, horas, educacion y edad | %d casos sobre chi2(0.999)", sum(an$anomalia)),
       x = "Anios de estudio", y = "Ingreso laboral (log)", caption = CAP)
guardar(p, "anomalias_multivariadas")

g <- cohortes |> select(anio_nac, `Anios de estudio` = educacion, `Tasa A persona` = tasa_vulnerable, `Tasa B hogar` = tasa_hogar) |> pivot_longer(-anio_nac)
p <- ggplot(g, aes(anio_nac, value)) + geom_point(color = PAL$violet, alpha = 0.6) + geom_smooth(method = "loess", formula = y ~ x, color = PAL$amber, fill = PAL$amber, alpha = 0.15) +
  facet_wrap(~name, scales = "free_y", ncol = 1) +
  labs(title = "Pseudo serie temporal por cohorte de nacimiento", subtitle = "Cada punto es un anio de nacimiento (personas de 25 a 85 anios) | curva = suavizado loess",
       x = "Anio de nacimiento", y = NULL, caption = CAP)
guardar(p, "cohortes", 11, 8)

checkpoint_guardar()
