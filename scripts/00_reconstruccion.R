# =====================================================================
#  00_reconstruccion.R
#  Control de formato numerico y prueba de resistencia
#
#  POR QUE EXISTE
#  Un CSV descargado puede abrirse o guardarse con una configuracion
#  regional distinta (coma decimal, punto como separador de miles). En
#  ese caso un valor con decimales pierde el punto y sus digitos quedan
#  agrupados de a tres:  4524.85009765625  ->  "452.485.009.765.625".
#  Una lectura directa tomaria 452.485 y subestimaria el ingreso.
#
#  QUE HACE
#  1. diagnosticar_formato(): revisa cada columna numerica del archivo.
#  2. Si encuentra valores con ese patron, los reconstruye con reglas
#     verificables (cada valor guarda la regla que lo resolvio):
#     R1 Rango oficial (min y max publicados por el INE en el DDI).
#     R2 Precision float32: solo una ubicacion del punto da un float32 exacto.
#     R3 Identidades contables del INE:
#          pobres (p0 = 1): yhogpc = z * (1 - p1); no pobres: yhogpc >= z
#          yhog = yhogpc * miembros ; yhog = suma de yper del hogar
#          yper = ylab + ynolab ; ylab = yprilab + yseclab
#     R4 Coherencia del factor dentro del estrato de muestreo.
#     R5 Maxima verosimilitud log-normal para lo que siga ambiguo.
#     R6-R7 Notacion cientifica con coma e identidades FGT.
#  3. simular_formato_regional(): prueba de resistencia. Se toma el
#     archivo sano, se simula como quedaria con formato regional y se
#     mide cuanto de el recuperan las reglas.
# =====================================================================

PAT_CORR <- "^-?[0-9]{1,3}(\\.[0-9]{3}){2,}$"   # 2 o mas grupos de 3: imposible en un CSV sano
f32 <- function(v) readBin(writeBin(as.numeric(v), raw(), size = 4), "double", size = 4, n = length(v))
es_f32 <- function(v) abs(f32(v) - v) <= 2e-14 * pmax(1, abs(v))

# Metadatos oficiales del DDI (INE, EH 2024): rango y estadisticos publicados
DDI_OFICIAL <- data.frame(
  variable = c("factor", "z", "zext", "ylab", "yprilab", "ynolab", "yper", "yhog", "yhogpc", "tothrs"),
  minimo   = c(12.6, 761.8, 436.7, 6.7, 6.7, 0, 0, 0, 0, 1),
  maximo   = c(4607.5, 1228.8, 603.8, 82270, 75064.9, 25000, 97270, 106375, 39358.7, 192),
  media    = c(313.9, 995.5, 511.2, 3330, 3267.5, 258.2, 1667, 5847.8, 1666.7, 44.4),
  media_ponderada = c(NA, 965.2, 497.9, 3277.5, 3211.2, 234.5, 1606.6, 5683.1, 1606.6, 44.7),
  desv_est = c(261.7, 175.7, 67.5, 3052, 2983.1, 831, 2666.5, 4961.9, NA, 19.6),
  total_expandido = c(12397406, NA, NA, 5189886.4, 5187105.5, 12397406, 12397406, 12389839.4, 12389839.4, 6163349.5))

RANGO_REC <- list(factor = c(12.6, 4607.5), z = c(600, 1300), zext = c(300, 700),
                  yhogpc = c(0, 39358.7), yhog = c(0, 106375), yper = c(0, 97270), ylab = c(6.7, 82270),
                  yprilab = c(6.7, 75064.9), yseclab = c(0.01, 60000), ynolab = c(0, 25000),
                  phrs = c(0, 192), shrs = c(0, 192), tothrs = c(0, 192))
VARS_DOUBLE <- "factor"   # el factor se guarda en doble precision: no aplica R2

candidatos_de <- function(x, rango, usa_f32) lapply(x, function(s) {
  if (is.na(s)) return(NA_real_)
  if (!grepl(PAT_CORR, s)) return(suppressWarnings(as.numeric(s)))
  D <- gsub("\\.", "", s); neg <- startsWith(D, "-"); D <- sub("^-", "", D); L <- nchar(D)
  v <- as.numeric(D) / 10^(L - seq_len(L)); if (neg) v <- -v
  v <- unique(v[v >= rango[1] & v <= rango[2]])
  if (!length(v)) return(NA_real_)
  if (usa_f32) { e <- v[es_f32(v)]; if (length(e)) v <- e }
  v
})
n_cand <- function(cl) vapply(cl, function(v) if (length(v) == 1 && is.na(v)) 0L else length(v), 1L)
fijar  <- function(cl, i, val) { cl[i] <- as.list(val); cl }

reconstruir_numericos <- function(r) {
  vars <- intersect(names(RANGO_REC), names(r))
  corr <- lapply(setNames(vars, vars), function(v) !is.na(r[[v]]) & grepl(PAT_CORR, r[[v]]))
  C    <- lapply(setNames(vars, vars), function(v) candidatos_de(r[[v]], RANGO_REC[[v]], !(v %in% VARS_DOUBLE)))
  regla <- lapply(setNames(vars, vars), function(v) {
    k <- n_cand(C[[v]])
    ifelse(!corr[[v]], "Sin cambios", ifelse(k == 1, "R1-R2 rango oficial + float32", NA_character_)) })
  marcar <- function(v, idx, nombre) { idx <- idx[is.na(regla[[v]][idx])]; regla[[v]][idx] <<- nombre }
  unico <- function(v) n_cand(C[[v]]) == 1
  val   <- function(v) vapply(C[[v]], function(z) if (length(z) == 1) z else NA_real_, 1)

  p0 <- suppressWarnings(as.numeric(r$p0)); p1 <- suppressWarnings(as.numeric(r$p1))
  folio <- r$folio; nhh <- as.integer(ave(seq_along(folio), folio, FUN = length))

  # --- R3a pobreza: yhogpc consistente con p0 y p1 (z ya es unico por rango) ---
  z <- val("z")
  amb <- which(n_cand(C$yhogpc) > 1)
  for (i in amb) {
    v <- C$yhogpc[[i]]; if (is.na(p0[i]) || is.na(z[i])) next
    if (p0[i] == 1) { k <- v[v < z[i]]; if (length(k) > 1 && !is.na(p1[i])) k <- k[which.min(abs(k - z[i] * (1 - p1[i])))] }
    else k <- v[v >= z[i]]
    if (length(k)) C$yhogpc[[i]] <- k
  }
  marcar("yhogpc", which(unico("yhogpc") & corr$yhogpc), "R3 identidad de pobreza (p0, p1, z)")

  # --- R3b yhog = yhogpc * miembros del hogar ---
  idx <- which((n_cand(C$yhogpc) > 1 | n_cand(C$yhog) > 1) & n_cand(C$yhogpc) > 0 & n_cand(C$yhog) > 0)
  for (i in idx) {
    g <- expand.grid(pc = C$yhogpc[[i]], h = C$yhog[[i]])
    g <- g[abs(g$h - g$pc * nhh[i]) <= 0.01 * pmax(1, g$h), , drop = FALSE]
    if (nrow(g)) { C$yhogpc[[i]] <- unique(g$pc); C$yhog[[i]] <- unique(g$h) }
  }
  marcar("yhogpc", which(unico("yhogpc") & corr$yhogpc), "R3 yhog = yhogpc x miembros")
  marcar("yhog",   which(unico("yhog") & corr$yhog),     "R3 yhog = yhogpc x miembros")

  # --- R3c yhog = suma de yper del hogar ---
  yh <- val("yhog")
  grupos <- split(seq_along(folio), folio)
  for (g in grupos) {
    k <- n_cand(C$yper[g]); if (all(k <= 1) || is.na(yh[g[1]])) next
    if (prod(pmax(k, 1)) > 5000) next
    opciones <- expand.grid(lapply(C$yper[g], function(v) if (length(v) == 1 && is.na(v)) 0 else v))
    err <- abs(rowSums(opciones) - yh[g[1]])
    if (min(err) <= 0.01 * max(1, yh[g[1]])) {
      best <- unlist(opciones[which.min(err), ])
      for (j in seq_along(g)) if (k[j] > 1) C$yper[[g[j]]] <- best[j]
    }
  }
  marcar("yper", which(unico("yper") & corr$yper), "R3 yhog = suma de yper del hogar")

  # --- R3d yper = ylab + ynolab  y  ylab = yprilab + yseclab ---
  par_suma <- function(tot, a, b, ia, ib) {
    vt <- val(tot)
    idx <- which(!is.na(vt) & (n_cand(C[[a]]) > 1 | n_cand(C[[b]]) > 1))
    for (i in idx) {
      A <- C[[a]][[i]]; B <- C[[b]][[i]]
      if (length(A) == 1 && is.na(A)) A <- 0
      if (length(B) == 1 && is.na(B)) B <- 0
      g <- expand.grid(a = A, b = B); e <- abs(g$a + g$b - vt[i])
      ok <- e <= 0.01 * max(1, vt[i])
      if (any(ok)) { g <- g[ok, ]; if (n_cand(C[[a]])[i] > 1) C[[a]][[i]] <<- unique(g$a); if (n_cand(C[[b]])[i] > 1) C[[b]][[i]] <<- unique(g$b) }
    }
  }
  par_suma("yper", "ylab", "ynolab"); par_suma("ylab", "yprilab", "yseclab")
  # tambien en sentido inverso: si ylab y ynolab ya son unicos, fijan yper
  for (v in c("ylab", "ynolab", "yprilab", "yseclab")) marcar(v, which(unico(v) & corr[[v]]), "R3 yper = ylab + ynolab / ylab = yprilab + yseclab")

  # --- R4 factor: coherencia dentro del estrato (iterativo, en log) ---
  if ("factor" %in% vars) {
    est <- if ("estrato" %in% names(r)) r$estrato else rep("1", nrow(r))
    elegir <- function(cl, ref) mapply(function(v, m) if (length(v) == 1) v else v[which.min(abs(log(v) - m))], cl, ref)
    w <- elegir(C$factor, rep(log(313.9), nrow(r)))
    for (it in 1:10) { m <- ave(log(w), est, FUN = median); w_new <- elegir(C$factor, m); if (all(w_new == w)) break; w <- w_new }
    amb_f <- n_cand(C$factor) > 1
    C$factor <- as.list(w); marcar("factor", which(amb_f), "R4 coherencia del factor en el estrato")
  }

  # --- R5 maxima verosimilitud log-normal para lo que siga ambiguo ---
  for (v in vars) {
    k <- n_cand(C[[v]]); if (!any(k > 1)) next
    ref <- log1p(val(v)[k == 1 & !is.na(val(v))]); mu <- mean(ref); s <- sd(ref)
    idx <- which(k > 1)
    C[[v]][idx] <- lapply(C[[v]][idx], function(x) x[which.max(dnorm(log1p(x), mu, s))])
    marcar(v, idx, "R5 maxima verosimilitud (log-normal)")
  }

  salida <- lapply(setNames(vars, vars), function(v) vapply(C[[v]], function(z) z[1], 1))
  diag <- do.call(rbind, lapply(vars, function(v) {
    t <- table(factor(regla[[v]][corr[[v]]]))
    if (!length(t)) return(NULL)
    data.frame(variable = v, regla = names(t), casos = as.integer(t), pct_de_alterados = round(100 * as.integer(t) / sum(corr[[v]]), 1))
  }))
  resumen <- data.frame(variable = vars, n_validos = vapply(vars, function(v) sum(!is.na(r[[v]])), 1),
                        n_alterados = vapply(vars, function(v) sum(corr[[v]]), 1))
  resumen$pct_alterados <- round(100 * resumen$n_alterados / pmax(1, resumen$n_validos), 1)
  list(valores = salida, reglas = regla, diagnostico = diag, resumen = resumen)
}

# ---------------------------------------------------------------------
# Segundo patron de formato regional: notacion cientifica con coma decimal
# ("1,00E+05" = 100000) y, en las medidas FGT (p1, p2, pext1, pext2),
# exponentes que ya no corresponden ("8,39E+09" en una variable que vive
# entre 0 y 1). R6: coma -> punto. R7: identidades FGT del INE:
#   p1 = (z - yhogpc) / z si el hogar es pobre, 0 si no; p2 = p1^2
#   pext1 = (zext - yhogpc) / zext si es pobre extremo; pext2 = pext1^2
# ---------------------------------------------------------------------
PAT_CIENT <- "^-?[0-9]+,[0-9]+[Ee][+-]?[0-9]+$"
VARS_FGT  <- c("p1", "p2", "pext1", "pext2")

reconstruir_cientifica <- function(r, rec_vals) {
  cols <- names(r)[vapply(r, function(x) any(grepl(PAT_CIENT, x)), logical(1))]
  if (!length(cols)) return(list(valores = list(), diagnostico = NULL))
  num <- function(v) if (!is.null(rec_vals[[v]])) rec_vals[[v]] else suppressWarnings(as.numeric(sub(",", ".", r[[v]])))
  yh <- num("yhogpc"); z <- num("z"); ze <- num("zext")
  p0 <- suppressWarnings(as.numeric(r$p0)); pe0 <- suppressWarnings(as.numeric(r$pext0))
  ident <- list(p1 = ifelse(p0 %in% 1, pmax(0, (z - yh) / z), 0),
                pext1 = ifelse(pe0 %in% 1, pmax(0, (ze - yh) / ze), 0))
  ident$p2 <- ident$p1^2; ident$pext2 <- ident$pext1^2
  out <- list(); diag <- list()
  for (v in cols) {
    x <- r[[v]]; m <- !is.na(x) & grepl(PAT_CIENT, x)
    val <- suppressWarnings(as.numeric(sub(",", ".", x)))
    regla <- rep("R6 notacion cientifica (coma -> punto)", sum(m))
    if (v %in% VARS_FGT) {
      malo <- m & (is.na(val) | val > 1 | val < 0)
      val[malo] <- ident[[v]][malo]
      regla[malo[m]] <- "R7 identidad FGT (z, yhogpc)"
    }
    out[[v]] <- val
    diag[[v]] <- data.frame(variable = v, regla = names(table(regla)), casos = as.integer(table(regla)))
  }
  list(valores = out, diagnostico = do.call(rbind, diag))
}

# ---------------------------------------------------------------------
# Diagnostico de formato del archivo real
# ---------------------------------------------------------------------
diagnosticar_formato <- function(r) {
  vars <- intersect(c(names(RANGO_REC), "p1", "p2", "pext1", "pext2"), names(r))
  do.call(rbind, lapply(vars, function(v) {
    x <- r[[v]]; ok <- !is.na(x)
    data.frame(variable = v, n_valores = sum(ok),
               digitos_agrupados = sum(ok & grepl(PAT_CORR, x)),
               cientifica_con_coma = sum(ok & grepl(PAT_CIENT, x)),
               no_numericos = sum(ok & is.na(suppressWarnings(as.numeric(x))) & !grepl(PAT_CORR, x) & !grepl(PAT_CIENT, x)))
  }))
}

# ---------------------------------------------------------------------
# Prueba de resistencia: como quedaria el archivo con formato regional.
# Regla observada en archivos convertidos: los enteros no cambian; un
# valor con decimales y 7 o mas digitos significativos pierde el punto
# decimal y sus digitos se agrupan de a tres desde la derecha.
# ---------------------------------------------------------------------
simular_formato_regional <- function(x) {
  v <- suppressWarnings(as.numeric(x)); out <- x
  m <- !is.na(v) & v != trunc(v)
  t <- sprintf("%.15g", v[m]); neg <- startsWith(t, "-")
  d <- sub("^0+", "", gsub("[.-]", "", t))
  largo <- nchar(d) >= 7 & !grepl("e", t)
  g <- stringi::stri_reverse(gsub("(\\d{3})(?=\\d)", "\\1.", stringi::stri_reverse(d), perl = TRUE))
  nuevo <- ifelse(largo, paste0(ifelse(neg, "-", ""), g), x[m])
  out[m] <- nuevo; out
}
comparar_recuperacion <- function(original, simulado, recuperado) {
  do.call(rbind, lapply(names(recuperado), function(v) {
    a <- recuperado[[v]]; b <- suppressWarnings(as.numeric(original[[v]]))
    alt <- !is.na(simulado[[v]]) & simulado[[v]] != original[[v]]
    ok <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & abs(a - b) <= 1e-6 * pmax(1, abs(b)))
    data.frame(variable = v, n_valores = sum(!is.na(b)), alterados = sum(alt),
               pct_alterados = round(100 * sum(alt) / max(1, sum(!is.na(b))), 1),
               recuperados_exactos = sum(ok & alt), pct_recuperados = round(100 * sum(ok & alt) / max(1, sum(alt)), 2))
  }))
}

# Lectura directa (sin tratamiento previo): toma el primer grupo como parte entera.
lectura_directa <- function(x) {
  n <- suppressWarnings(as.numeric(x)); f <- is.na(n) & !is.na(x)
  n[f] <- suppressWarnings(as.numeric(stringr::str_extract(x[f], "^-?\\d+(\\.\\d+)?"))); n
}

validar_contra_ddi <- function(d, w_col = "factor") {
  w <- d[[w_col]]
  do.call(rbind, lapply(intersect(DDI_OFICIAL$variable, names(d)), function(v) {
    x <- d[[v]]; o <- DDI_OFICIAL[DDI_OFICIAL$variable == v, ]; ok <- !is.na(x)
    data.frame(variable = v,
               media_oficial = o$media, media_calculada = round(mean(x[ok]), 1),
               desv_oficial = o$desv_est, desv_calculada = round(sd(x[ok]), 1),
               max_oficial = o$maximo, max_calculado = round(max(x[ok]), 1),
               total_exp_oficial = o$total_expandido,
               total_exp_calculado = if (v == "factor") round(sum(x[ok])) else round(sum(w[ok]), 1),
               dif_media_pct = round(100 * (mean(x[ok]) / o$media - 1), 2))
  }))
}
