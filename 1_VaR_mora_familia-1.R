##########################################################
# VALUE AT RISK DEL RATIO DE MORA (VENCIDA PAR30 / SALDO)
#  1. VaR histórico simple
#  2. Rango bootstrap del VaR (incertidumbre de la estimación)
#  3. VaR paramétrico: normal, lognormal y beta
#  4. Bondad de ajuste de las distribuciones (AIC y KS)
#  5. Backtesting con test de Kupiec para comparar los métodos
#
# Requisito: tener creados los objetos alco y alco1.
# Ejecuta antes 2_Bootstrap_Productos_optimizado.R hasta la
# sección "Cargar base".
##########################################################

library(MASS)     # antes que dplyr, para que select() siga siendo el de dplyr
library(dplyr)
library(rsample)

#=========================#
# Parámetros              #
#=========================#

niveles     <- c(0.95, 0.99)  # niveles de confianza del VaR
n_boot      <- 500            # réplicas bootstrap para el rango del VaR
n_boot_bt   <- 200            # réplicas dentro del backtesting (más liviano)
ventana_min <- 24             # meses mínimos de historia para estimar en el backtesting
desde_2021  <- c("AUTOS")     # familias que usan alco1 (periodos > 202012)

# Semillas del proceso actual (transcritas de tu script: verifícalas)
semillas <- c("PRECISO"                = 987452,
              "TARJETA"                = 698245,
              "LINEA ABIERTA"          = 233158,
              "AUTOS"                  = 945867,
              "CASAS COMERCIALES"      = 985157,
              "CCSA PERSONAS"          = 254631,
              "SOBREGIRO"              = 378912,
              "EDUCATIVO"              = 964852,
              "HABITAR"                = 595126,
              "CCSA EMPRESAS"          = 710112,
              "PRODUCTIVO EMPRESAS"    = 273256,
              "PRODUCTIVO PES Y OTROS" = 318529,
              "MICROFINANZAS"          = 462485)

ruta_salida <- 'C:/Users/dfsantil/Documents/1.Riesgo de Crédito/Apetito/2024/Morosidad/APETITO30/'

#=========================#
# Funciones base          #
#=========================#

# Serie mensual de mora de una familia, ordenada por periodo
serie_familia <- function(familia) {
  base <- if (familia %in% desde_2021) alco1 else alco
  base %>%
    ungroup() %>%
    filter(FamiliaAlco == familia) %>%
    arrange(CodigoPeriodo)
}

# VaR histórico: percentil directo de la serie (type = 3, igual que funcion_pXX)
var_hist <- function(x, a) unname(quantile(x, a, type = 3))

# Expected Shortfall histórico: promedio de los meses que caen en la cola
es_hist <- function(x, a) mean(x[x >= var_hist(x, a)])

# Remuestras bootstrap (matriz n meses x n_boot réplicas).
# Mismo esquema que el proceso actual: misma semilla = mismos sorteos.
remuestras <- function(x, semilla, n_boot) {
  set.seed(semilla)
  boot <- bootstraps(data.frame(nombre = x), times = n_boot)
  sapply(boot$splits, function(s) analysis(s)$nombre)
}

#=========================#
# VaR paramétrico         #
#=========================#

# Ajusta las tres distribuciones a la serie de mora:
#  - Normal:    media y desviación estándar de la serie
#  - Lognormal: máxima verosimilitud sobre log(mora); captura la cola derecha
#  - Beta:      método de momentos; pensada para proporciones entre 0 y 1
ajustar_dist <- function(x) {
  xp <- pmin(pmax(x, 1e-6), 1 - 1e-6)    # lognormal y beta no admiten 0 ni 1
  m  <- mean(xp)
  k  <- m * (1 - m) / var(xp) - 1        # concentración de la beta
  list(mu      = mean(x),
       sd      = sd(x),
       meanlog = mean(log(xp)),
       sdlog   = sqrt(mean((log(xp) - mean(log(xp)))^2)),
       shape1  = m * k,
       shape2  = (1 - m) * k,
       xp      = xp)
}

# VaR paramétrico: percentil de la distribución ajustada
var_param <- function(p, a) c(
  Normal    = p$mu + qnorm(a) * p$sd,
  Lognormal = qlnorm(a, p$meanlog, p$sdlog),
  Beta      = qbeta(a, p$shape1, p$shape2))

# Expected Shortfall paramétrico (fórmulas cerradas)
es_param <- function(p, a) {
  s1 <- p$shape1; s2 <- p$shape2
  c(Normal    = p$mu + p$sd * dnorm(qnorm(a)) / (1 - a),
    Lognormal = exp(p$meanlog + p$sdlog^2 / 2) * pnorm(p$sdlog - qnorm(a)) / (1 - a),
    Beta      = s1 / (s1 + s2) * (1 - pbeta(qbeta(a, s1, s2), s1 + 1, s2)) / (1 - a))
}

# Bondad de ajuste: AIC (menor = mejor) y test KS (p > 0,05 = no se rechaza).
# El p-valor KS es aproximado porque los parámetros salen de la misma serie.
bondad_ajuste <- function(p, familia) {
  x  <- p$xp
  ll <- c(Normal    = sum(dnorm(x, p$mu, p$sd, log = TRUE)),
          Lognormal = sum(dlnorm(x, p$meanlog, p$sdlog, log = TRUE)),
          Beta      = sum(dbeta(x, p$shape1, p$shape2, log = TRUE)))
  ks <- suppressWarnings(c(
          ks.test(x, "pnorm",  p$mu,      p$sd)$p.value,
          ks.test(x, "plnorm", p$meanlog, p$sdlog)$p.value,
          ks.test(x, "pbeta",  p$shape1,  p$shape2)$p.value))
  data.frame(Familia          = familia,
             Distribucion     = names(ll),
             LogVerosimilitud = ll,
             AIC              = 2 * 2 - 2 * ll,   # las tres tienen 2 parámetros
             KS_p_valor       = ks,
             row.names        = NULL) %>%
    mutate(Mejor_ajuste = AIC == min(AIC))
}

#=======================================================#
# 1. Resultados del VaR para una familia                #
#=======================================================#

var_familia <- function(familia, graficar = TRUE) {

  serie <- serie_familia(familia)
  x     <- serie$MORA
  mu    <- mean(x)
  saldo <- tail(serie$SALDO_TOTAL, 1)
  p     <- ajustar_dist(x)
  M     <- remuestras(x, semillas[[familia]], n_boot)

  res <- do.call(rbind, lapply(niveles, function(a) {
    vb <- apply(M, 2, var_hist, a = a)   # VaR de cada réplica bootstrap
    eb <- apply(M, 2, es_hist,  a = a)   # ES de cada réplica bootstrap
    data.frame(
      Familia       = familia,
      Nivel         = paste0(a * 100, "%"),
      Enfoque       = c("Histórico", "Bootstrap", "Paramétrico", "Paramétrico", "Paramétrico"),
      Metodo        = c("Historico", "Bootstrap_media", "Normal", "Lognormal", "Beta"),
      Meses         = length(x),
      Mora_media    = mu,
      VaR           = c(var_hist(x, a), mean(vb), unname(var_param(p, a))),
      ES            = c(es_hist(x, a),  mean(eb), unname(es_param(p, a))),
      # Rango bootstrap (2,5% - 97,5%) solo para el VaR histórico
      VaR_rango_inf = c(unname(quantile(vb, 0.025)), rep(NA, 4)),
      VaR_rango_sup = c(unname(quantile(vb, 0.975)), rep(NA, 4)),
      row.names     = NULL)
  })) %>%
    mutate(Mora_inesperada        = VaR - Mora_media,
           Saldo_ultimo_corte     = saldo,
           Vencida_esperada_USD   = Mora_media * saldo,
           Vencida_maxima_USD     = VaR * saldo,
           Vencida_inesperada_USD = Mora_inesperada * saldo)

  ajuste <- bondad_ajuste(p, familia)

  if (graficar) {
    v_hist <- res$VaR[res$Metodo == "Historico"]   # VaR 95% y 99% históricos
    v99    <- res[res$Nivel == "99%", ]
    col_d  <- c(Normal = "darkgreen", Lognormal = "purple", Beta = "brown")

    op <- par(mfrow = c(1, 2))

    # Panel 1: serie en el tiempo con la media y el VaR histórico
    plot(x, type = "l", lwd = 2, xaxt = "n",
         main = paste("Mora mensual -", familia), xlab = "", ylab = "Mora")
    idx <- unique(round(seq(1, length(x), length.out = 6)))
    axis(1, at = idx, labels = serie$CodigoPeriodo[idx], cex.axis = 0.8)
    abline(h = mu, col = "blue", lwd = 2)
    abline(h = v_hist, col = c("orange", "red"), lwd = 2, lty = 2)
    legend("topleft", bty = "n", cex = 0.8, lwd = 2, lty = c(1, 2, 2),
           col = c("blue", "orange", "red"),
           legend = c("Media", "VaR 95% histórico", "VaR 99% histórico"))

    # Panel 2: histograma con las tres distribuciones ajustadas
    rango <- range(c(x, v99$VaR))
    malla <- seq(rango[1], rango[2], length.out = 300)
    h     <- hist(x, breaks = 20, plot = FALSE)
    ymax  <- max(h$density,
                 dnorm(malla, p$mu, p$sd),
                 dlnorm(malla, p$meanlog, p$sdlog),
                 dbeta(malla, p$shape1, p$shape2))
    plot(h, freq = FALSE, col = "grey85", border = "white",
         xlim = rango, ylim = c(0, ymax * 1.05),
         main = "Distribución y ajustes", xlab = "Mora")
    lines(malla, dnorm(malla, p$mu, p$sd),                col = col_d["Normal"],    lwd = 2)
    lines(malla, dlnorm(malla, p$meanlog, p$sdlog),       col = col_d["Lognormal"], lwd = 2)
    lines(malla, dbeta(malla, p$shape1, p$shape2),        col = col_d["Beta"],      lwd = 2)
    abline(v = v_hist[2], col = "red", lwd = 2, lty = 2)                             # VaR 99% histórico
    abline(v = v99$VaR[match(names(col_d), v99$Metodo)], col = col_d, lwd = 1, lty = 3)  # VaR 99% paramétricos
    legend("topright", bty = "n", cex = 0.8, lwd = 2, lty = c(1, 1, 1, 2),
           col = c(col_d, "red"),
           legend = c(names(col_d), "VaR 99% histórico"))

    par(op)
  }

  list(var = res, ajuste = ajuste)
}

#=======================================================#
# 2. Backtesting: ¿qué método habría acertado mejor?    #
#=======================================================#

# Test de Kupiec: ¿la tasa de excepciones es compatible con la esperada?
# k = excepciones, n = meses evaluados, p = 1 - nivel de confianza
kupiec <- function(k, n, p) {
  xlogy  <- function(a, b) ifelse(a == 0, 0, a * log(b))
  pi_obs <- k / n
  logL0  <- xlogy(n - k, 1 - p) + xlogy(k, p)
  logL1  <- xlogy(n - k, 1 - pi_obs) + xlogy(k, pi_obs)
  LR     <- -2 * (logL0 - logL1)
  1 - pchisq(LR, df = 1)   # p-valor
}

backtest_familia <- function(familia, a) {

  x <- serie_familia(familia)$MORA
  n <- length(x)
  if (n - ventana_min < 12) {
    warning(familia, ": menos de 12 meses para evaluar; se omite el backtesting")
    return(NULL)
  }

  set.seed(semillas[[familia]])
  cortes <- ventana_min:(n - 1)

  # Para cada corte: VaR de cada método estimado solo con la historia hasta ese mes
  pred <- t(sapply(cortes, function(t) {
    h  <- x[1:t]
    vb <- replicate(n_boot_bt, var_hist(sample(h, replace = TRUE), a))
    c(Historico          = var_hist(h, a),
      Bootstrap_media    = mean(vb),
      Bootstrap_mas2DVSD = mean(vb) + qnorm(0.9772499) * sd(vb),
      var_param(ajustar_dist(h), a))       # Normal, Lognormal, Beta
  }))

  real <- x[cortes + 1]      # mora real del mes siguiente
  exc  <- real > pred        # TRUE = la mora superó el VaR (excepción)
  k    <- colSums(exc)
  nev  <- length(real)

  data.frame(
    Familia         = familia,
    Nivel           = paste0(a * 100, "%"),
    Metodo          = colnames(pred),
    Meses_evaluados = nev,
    Excepciones     = k,
    Tasa_observada  = k / nev,
    Tasa_esperada   = 1 - a,
    p_valor_Kupiec  = sapply(k, kupiec, n = nev, p = 1 - a),
    VaR_promedio    = colMeans(pred),
    row.names       = NULL
  ) %>%
    mutate(Resultado = ifelse(p_valor_Kupiec >= 0.05, "Aprueba", "Rechaza"))
}

# Regla de decisión: entre los métodos que aprueban Kupiec,
# se elige el de menor VaR promedio (acierta igual con menos holgura)
elegir_metodo <- function(bt) {
  bt %>%
    group_by(Familia, Nivel) %>%
    mutate(VaR_min_ok = if (any(Resultado == "Aprueba"))
                          min(VaR_promedio[Resultado == "Aprueba"]) else NA_real_,
           Elegido    = Resultado == "Aprueba" & VaR_promedio == VaR_min_ok) %>%
    ungroup() %>%
    dplyr::select(-VaR_min_ok)
}

#=========================#
# Ejecución               #
#=========================#

familia <- "PRECISO"

# VaR por método (histórico, bootstrap y paramétricos) + gráfico
res <- var_familia(familia)
print(res$var)

# ¿Qué distribución se ajusta mejor a la mora de esta familia?
print(res$ajuste)

# Backtesting de los seis métodos y método elegido
bt <- elegir_metodo(do.call(rbind, lapply(niveles, backtest_familia, familia = familia)))
print(bt)

# ---- Todas las familias de una vez (descomenta para usar) ----
# todas      <- lapply(names(semillas), var_familia, graficar = FALSE)
# var_todas  <- do.call(rbind, lapply(todas, `[[`, "var"))
# aj_todas   <- do.call(rbind, lapply(todas, `[[`, "ajuste"))
# bt_todas   <- elegir_metodo(do.call(rbind, lapply(names(semillas), function(f)
#                 do.call(rbind, lapply(niveles, backtest_familia, familia = f)))))
# write.table(var_todas, paste0(ruta_salida, 'VaR_mora_familias.txt'),
#             sep = "\t", dec = ".", row.names = FALSE)
# write.table(aj_todas,  paste0(ruta_salida, 'Ajuste_distribuciones_familias.txt'),
#             sep = "\t", dec = ".", row.names = FALSE)
# write.table(bt_todas,  paste0(ruta_salida, 'Backtesting_VaR_familias.txt'),
#             sep = "\t", dec = ".", row.names = FALSE)
