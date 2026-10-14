# =====================================================================
#  00_modelos.R
#  Biblioteca de modelado: UNA funcion prueba todos los modelos con la
#  misma particion, las mismas metricas y validacion cruzada, y dice
#  cual conviene y por que.
#
#    res <- comparar_modelos(datos, y = "yA", x = PREDICTORES)
#    res$resumen      # metricas medias +- DE por modelo (k pliegues)
#    res$mejor        # mayor AUC media
#    res$elegido      # regla de 1 error estandar: el mas simple que no es
#                     # significativamente peor que el mejor
#    res$pruebas      # t pareada de AUC por pliegue contra el mejor
#
#  Modelos incluidos (fit ajusta, pred devuelve la probabilidad de 1):
#    Naive Bayes, Regresion logistica, Logistica L1 (lasso), Arbol de
#    decision (CART), Random Forest y Gradient Boosting (GBM).
#  Para agregar un modelo basta con sumar una entrada a MODELOS.
# =====================================================================

# ---------------------------------------------------------------------
# Metricas de clasificacion binaria (y en 0/1, p = probabilidad de 1)
# ---------------------------------------------------------------------
auc_rank <- function(y, p) {               # AUC = P(p de un 1 > p de un 0), via Mann-Whitney
  ok <- !is.na(y) & !is.na(p); y <- y[ok]; p <- p[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0); if (!n1 || !n0) return(NA_real_)
  (sum(rank(p)[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
metricas_bin <- function(y, p, umbral = 0.5) {
  pr <- as.integer(p >= umbral)
  tp <- sum(pr == 1 & y == 1); tn <- sum(pr == 0 & y == 0); fp <- sum(pr == 1 & y == 0); fn <- sum(pr == 0 & y == 1)
  sens <- tp / max(1, tp + fn); esp <- tn / max(1, tn + fp); prec <- tp / max(1, tp + fp)
  pc <- pmin(pmax(p, 1e-6), 1 - 1e-6)
  data.frame(auc = auc_rank(y, p), exactitud = (tp + tn) / length(y), exactitud_balanceada = (sens + esp) / 2,
             sensibilidad = sens, especificidad = esp, precision = prec,
             f1 = ifelse(prec + sens > 0, 2 * prec * sens / (prec + sens), 0),
             brier = mean((p - y)^2), logloss = -mean(y * log(pc) + (1 - y) * log(1 - pc)),
             vp = tp, vn = tn, fp = fp, fn = fn, umbral = umbral)
}
umbral_youden <- function(y, p) {          # maximiza sensibilidad + especificidad - 1
  u <- sort(unique(round(p, 3))); if (length(u) > 400) u <- quantile(p, seq(0.01, 0.99, length.out = 400), names = FALSE)
  j <- vapply(u, function(t) { pr <- p >= t; mean(pr[y == 1]) + mean(!pr[y == 0]) - 1 }, numeric(1))
  u[which.max(j)]
}
curva_roc <- function(y, p, n = 120) {
  u <- unique(c(Inf, quantile(p, seq(1, 0, length.out = n), names = FALSE), -Inf))
  data.frame(umbral = u, fpr = vapply(u, function(t) mean(p[y == 0] >= t), 1), tpr = vapply(u, function(t) mean(p[y == 1] >= t), 1))
}
curva_calibracion <- function(y, p, bins = 10) {
  g <- cut(p, unique(quantile(p, seq(0, 1, length.out = bins + 1))), include.lowest = TRUE)
  data.frame(p_media = tapply(p, g, mean), y_media = tapply(y, g, mean), n = as.vector(table(g)))
}

# ---------------------------------------------------------------------
# Catalogo de modelos: cada uno es function(train, test, x) -> prob
# 'complejidad' ordena para la regla de 1 error estandar.
# ---------------------------------------------------------------------
mm <- function(d, x, niveles = NULL) {     # matriz de diseno con niveles fijos (para glmnet)
  model.matrix(reformulate(x), model.frame(reformulate(x), d, na.action = na.pass))[, -1, drop = FALSE]
}
MODELOS <- list(
  "Naive Bayes" = list(complejidad = 1,
    fit  = function(tr, x) e1071::naiveBayes(reformulate(x, "factor(y)"), data = tr),
    pred = function(m, te, x) predict(m, te[x], type = "raw")[, "1"]),
  "Regresion logistica" = list(complejidad = 1,
    fit  = function(tr, x) suppressWarnings(glm(reformulate(x, "y"), data = tr, family = binomial())),
    pred = function(m, te, x) suppressWarnings(predict(m, te, type = "response"))),
  "Logistica L1 (lasso)" = list(complejidad = 1,
    fit  = function(tr, x) { X <- mm(tr, x); m <- glmnet::cv.glmnet(X, tr$y, family = "binomial", nfolds = 3, alpha = 1); m$cols <- colnames(X); m },
    pred = function(m, te, x) { X <- mm(te, x); falta <- setdiff(m$cols, colnames(X))
      if (length(falta)) X <- cbind(X, matrix(0, nrow(X), length(falta), dimnames = list(NULL, falta)))
      as.numeric(predict(m, X[, m$cols, drop = FALSE], s = "lambda.1se", type = "response")) }),
  "Arbol de decision" = list(complejidad = 2,
    fit  = function(tr, x) rpart::rpart(reformulate(x, "factor(y)"), data = tr, method = "class",
                                        control = rpart::rpart.control(cp = 0.002, minbucket = 30, maxdepth = 8)),
    pred = function(m, te, x) predict(m, te, type = "prob")[, "1"]),
  "Random Forest" = list(complejidad = 3,
    fit  = function(tr, x) ranger::ranger(reformulate(x, "factor(y)"), data = tr, probability = TRUE, num.trees = 300,
                                          min.node.size = 15, respect.unordered.factors = "order", seed = CFG$semilla, num.threads = 2),
    pred = function(m, te, x) predict(m, te)$predictions[, "1"]),
  "Gradient Boosting (GBM)" = list(complejidad = 3,
    fit  = function(tr, x) gbm::gbm(reformulate(x, "y"), data = tr, distribution = "bernoulli", n.trees = 300,
                                    interaction.depth = 4, shrinkage = 0.05, bag.fraction = 0.8, n.minobsinnode = 20, verbose = FALSE),
    pred = function(m, te, x) suppressMessages(predict(m, te, n.trees = 300, type = "response")))
)
ajustar_predecir <- function(mo, tr, te, x) { m <- MODELOS[[mo]]$fit(tr, x); MODELOS[[mo]]$pred(m, te, x) }

# Pliegues estratificados por una o mas columnas
pliegues <- function(estrato, k, semilla) {
  set.seed(semilla); f <- integer(length(estrato))
  for (g in split(seq_along(estrato), estrato)) f[g] <- sample(rep_len(sample(k), length(g)))
  f
}

# ---------------------------------------------------------------------
# comparar_modelos: validacion cruzada k-fold estratificada
# ---------------------------------------------------------------------
comparar_modelos <- function(datos, y, x, modelos = names(MODELOS), k = CFG$k_cv, semilla = CFG$semilla,
                             estrato = datos[[y]], verbose = TRUE) {
  d <- datos; d$y <- as.integer(d[[y]])
  fold <- pliegues(estrato, k, semilla)
  oof <- matrix(NA_real_, nrow(d), length(modelos), dimnames = list(NULL, modelos))
  cv <- list(); tiempos <- setNames(numeric(length(modelos)), modelos)
  for (mo in modelos) {
    t0 <- Sys.time()
    for (f in seq_len(k)) {
      tr <- d[fold != f, ]; te <- d[fold == f, ]
      set.seed(semilla + f)
      p <- ajustar_predecir(mo, tr, te, x)
      oof[fold == f, mo] <- p
      cv[[length(cv) + 1]] <- cbind(modelo = mo, pliegue = f, metricas_bin(te$y, p, 0.5))
    }
    tiempos[mo] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (verbose) cat(sprintf("    %-26s AUC CV = %.3f  (%.1f s)\n", mo,
                             mean(sapply(cv[sapply(cv, function(r) r$modelo == mo)], `[[`, "auc")), tiempos[mo]))
  }
  cv <- do.call(rbind, cv)
  # Umbral de Youden sobre las predicciones fuera de pliegue (nunca sobre el test)
  umbrales <- vapply(modelos, function(mo) umbral_youden(d$y, oof[, mo]), numeric(1))
  oof_met <- do.call(rbind, lapply(modelos, function(mo) cbind(modelo = mo, metricas_bin(d$y, oof[, mo], umbrales[mo]))))
  resumen <- do.call(rbind, lapply(modelos, function(mo) {
    r <- cv[cv$modelo == mo, ]
    data.frame(modelo = mo, complejidad = MODELOS[[mo]]$complejidad,
               auc_media = mean(r$auc), auc_de = sd(r$auc), auc_ee = sd(r$auc) / sqrt(k),
               brier_media = mean(r$brier), logloss_media = mean(r$logloss),
               exactitud_bal_youden = oof_met$exactitud_balanceada[oof_met$modelo == mo],
               sensibilidad_youden = oof_met$sensibilidad[oof_met$modelo == mo],
               especificidad_youden = oof_met$especificidad[oof_met$modelo == mo],
               f1_youden = oof_met$f1[oof_met$modelo == mo], umbral_youden = umbrales[mo], segundos = tiempos[mo])
  }))
  resumen <- resumen[order(-resumen$auc_media), ]
  mejor <- resumen$modelo[1]
  # Regla de 1 error estandar: entre los que no quedan a mas de 1 EE del
  # mejor, el de menor complejidad (y luego mayor AUC)
  lim <- resumen$auc_media[1] - resumen$auc_ee[1]
  cand <- resumen[resumen$auc_media >= lim, ]
  elegido <- cand$modelo[order(cand$complejidad, -cand$auc_media)][1]
  # t pareada por pliegue: cada modelo contra el mejor
  pruebas <- do.call(rbind, lapply(setdiff(modelos, mejor), function(mo) {
    a <- cv$auc[cv$modelo == mejor]; b <- cv$auc[cv$modelo == mo]
    tt <- t.test(a, b, paired = TRUE)
    data.frame(modelo = mo, contra = mejor, dif_auc = mean(a - b), t = unname(tt$statistic), p_valor = tt$p.value,
               conclusion = ifelse(tt$p.value < 0.05, "Significativamente peor que el mejor", "Sin diferencia significativa"))
  }))
  resumen$rol <- ifelse(resumen$modelo == mejor, ifelse(mejor == elegido, "Mejor y elegido", "Mejor AUC"),
                        ifelse(resumen$modelo == elegido, "Elegido (regla 1 EE)", ""))
  list(cv = cv, resumen = resumen, oof = oof, oof_metricas = oof_met, umbrales = umbrales,
       mejor = mejor, elegido = elegido, pruebas = pruebas, pliegue = fold)
}

# Importancia por permutacion (caida de AUC en el test al desordenar una
# variable; el modelo se ajusta una sola vez)
importancia_permutacion <- function(mo, modelo, te, x, repeticiones = 3, semilla = CFG$semilla) {
  set.seed(semilla); base <- auc_rank(te$y, MODELOS[[mo]]$pred(modelo, te, x))
  do.call(rbind, lapply(x, function(v) {
    caidas <- vapply(seq_len(repeticiones), function(r) { t2 <- te; t2[[v]] <- sample(t2[[v]])
      base - auc_rank(t2$y, MODELOS[[mo]]$pred(modelo, t2, x)) }, numeric(1))
    data.frame(variable = v, caida_auc = mean(caidas), de = sd(caidas))
  }))
}
