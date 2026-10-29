##########################################################
# VALUE AT RISK DEL RATIO DE MORA (VENCIDA PAR30 / SALDO)
#  1. VaR histórico simple (método principal)
#  2. Rango bootstrap del VaR (incertidumbre de la estimación)
#  3. Backtesting con test de Kupiec para comparar métodos
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

# Expected Shortfall: promedio de los meses que caen en la cola
es_hist <- function(x, a) mean(x[x >= var_hist(x, a)])

# Remuestras bootstrap (matriz n meses x n_boot réplicas).
# Mismo esquema que el proceso actual: misma semilla = mismos sorteos.
remuestras <- function(x, semilla, n_boot) {
  set.seed(semilla)
  boot <- bootstraps(data.frame(nombre = x), times = n_boot)
  sapply(boot$splits, function(s) analysis(s)$nombre)
}

#=======================================================#
# 1. Resultados del VaR para una familia                #
#=======================================================#

var_familia <- function(familia, graficar = TRUE) {

  serie <- serie_familia(familia)
  x     <- serie$MORA
  mu    <- mean(x)
  saldo <- tail(serie$SALDO_TOTAL, 1)
  M     <- remuestras(x, semillas[[familia]], n_boot)

  res <- do.call(rbind, lapply(niveles, function(a) {
    vh <- var_hist(x, a)                 # VaR histórico
    vb <- apply(M, 2, var_hist, a = a)   # VaR de cada réplica bootstrap
    data.frame(
      Familia                = familia,
      Nivel                  = paste0(a * 100, "%"),
      Meses                  = length(x),
      Mora_media             = mu,
      VaR_historico          = vh,
      VaR_rango_inf          = unname(quantile(vb, 0.025)),
      VaR_rango_sup          = unname(quantile(vb, 0.975)),
      VaR_bootstrap_media    = mean(vb),   # = columna P95/P99, fila MEDIA, del proceso actual
      ES_historico           = es_hist(x, a),
      Mora_inesperada        = vh - mu,
      Saldo_ultimo_corte     = saldo,
      Vencida_esperada_USD   = mu * saldo,
      Vencida_maxima_USD     = vh * saldo,
      Vencida_inesperada_USD = (vh - mu) * saldo
    )
  }))

  if (graficar) {
    op <- par(mfrow = c(1, 2))

    # Serie en el tiempo con la media y los VaR
    plot(x, type = "l", lwd = 2, xaxt = "n",
         main = paste("Mora mensual -", familia), xlab = "", ylab = "Mora")
    idx <- unique(round(seq(1, length(x), length.out = 6)))
    axis(1, at = idx, labels = serie$CodigoPeriodo[idx], cex.axis = 0.8)
    abline(h = mu, col = "blue", lwd = 2)
    abline(h = res$VaR_historico, col = c("orange", "red"), lwd = 2, lty = 2)

    # Distribución de la mora
    hist(x, breaks = 20, col = "grey85", border = "white",
         main = "Distribución", xlab = "Mora")
    abline(v = mu, col = "blue", lwd = 2)
    abline(v = res$VaR_historico, col = c("orange", "red"), lwd = 2, lty = 2)
    legend("topright", bty = "n", lwd = 2, lty = c(1, 2, 2),
           col = c("blue", "orange", "red"),
           legend = c("Media", paste("VaR", res$Nivel)))

    par(op)
  }

  res
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

  # Para cada corte: VaR estimado solo con la historia disponible hasta ese mes
  pred <- t(sapply(cortes, function(t) {
    h  <- x[1:t]
    vb <- replicate(n_boot_bt, var_hist(sample(h, replace = TRUE), a))
    c(Historico          = var_hist(h, a),
      Bootstrap_media    = mean(vb),
      Bootstrap_mas2DVSD = mean(vb) + qnorm(0.9772499) * sd(vb))
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

# Resultados del VaR + gráfico
res_var <- var_familia(familia)
print(res_var)

# Comparación de métodos
bt <- elegir_metodo(do.call(rbind, lapply(niveles, backtest_familia, familia = familia)))
print(bt)

# ---- Todas las familias de una vez (descomenta para usar) ----
# res_todas <- do.call(rbind, lapply(names(semillas), var_familia, graficar = FALSE))
# bt_todas  <- elegir_metodo(do.call(rbind, lapply(names(semillas), function(f)
#                do.call(rbind, lapply(niveles, backtest_familia, familia = f)))))
# write.table(res_todas, paste0(ruta_salida, 'VaR_mora_familias.txt'),
#             sep = "\t", dec = ".", row.names = FALSE)
# write.table(bt_todas, paste0(ruta_salida, 'Backtesting_VaR_familias.txt'),
#             sep = "\t", dec = ".", row.names = FALSE)
