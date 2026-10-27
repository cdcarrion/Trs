# =============================================================================
# Análisis de clientes PERFIL ALTO por rango de clasificación (AAA / AA / A vs resto)
# Objetivo: entender qué características de buró tienen los AAA, AA y A que,
# aun así, terminan en PERFIL ALTO, y contrastarlos con REVISION MANUAL / SIN INFORMACION.
#
# Salida: analisis_perfil_alto_resultados.xlsx
# =============================================================================

# ---- 0. Paquetes ------------------------------------------------------------
pkgs <- c("readxl", "dplyr", "tidyr", "openxlsx", "rpart")
faltan <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(faltan)) install.packages(faltan)
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(openxlsx); library(rpart)
})

# ---- Parámetros (AJUSTAR) ---------------------------------------------------
ARCHIVO <- "C:/ruta/a/tu_base.xlsx"
HOJA    <- 1
SALIDA  <- "analisis_perfil_alto_resultados.xlsx"

# =============================================================================
# 1. Carga y limpieza
# =============================================================================
# Se lee todo como texto y luego se convierte: evita problemas con "163,757.93" y celdas vacías
df <- read_excel(ARCHIVO, sheet = HOJA, col_types = "text", .name_repair = "minimal")
names(df) <- trimws(names(df))
df <- as.data.frame(df[, !duplicated(names(df))], stringsAsFactors = FALSE)  # quita scoreBuro/tipoModelo/identificacionCliente repetidos
df[] <- lapply(df, function(x) { x <- trimws(x); x[x %in% c("", "NA", "NULL")] <- NA; x })

# Columnas de identificación / contexto (no son variables de buró)
COLS_CONTEXTO <- c("identificacionCliente", "codigoPeriodo", "perfilRiesgo", "nombreModelo", "tipoModelo",
                   "codigoOficina", "nombreOficina", "nombreRegionOficina", "perfilRiesgoAnt",
                   "segmentoNewCliente", "estadoCliente", "rangoScoreBuro", "rangoClasificacion",
                   "fuente", "clasificacion", "calificacion", "nodo", "fechaIngesta", "periodo")
COLS_FECHA <- c("minFechaAperturaSBS", "minFechaAperturaSEPS", "minFechaAperturaSICOM",
                "fechaIngesta", "periodo")
COLS_SN <- c("otrosProdSistema", "atrasoTitularMayor60DiasTcBp", "atrasoTitularMayor60DiasTcOtros",
             "atrasoTitularMayor90DiasCarteraBp", "atrasoTitularMayor90DiasCarteraOtros")

a_numero <- function(x) suppressWarnings(as.numeric(gsub(",", "", x)))

# Banderas S/N -> 1/0
for (v in intersect(COLS_SN, names(df))) {
  df[[v]] <- ifelse(toupper(df[[v]]) == "S", 1, ifelse(toupper(df[[v]]) == "N", 0, NA_real_))
}

# Variables de buró numéricas = todo lo que no es contexto ni fecha
VARS_BURO <- setdiff(names(df), c(COLS_CONTEXTO, COLS_FECHA))
for (v in setdiff(VARS_BURO, COLS_SN)) df[[v]] <- a_numero(df[[v]])
VARS_BURO <- VARS_BURO[sapply(df[VARS_BURO], function(x) any(!is.na(x)))]

# Banda limpia: "1. AAA" -> "AAA"
df$banda <- toupper(trimws(sub("^\\s*\\d+\\.\\s*", "", df$rangoClasificacion)))
df$grupo <- ifelse(df$banda %in% c("AAA", "AA", "A"), "1. AAA-AA-A",
            ifelse(grepl("SIN INFO", df$banda), "3. SIN INFORMACION", "2. OTRAS CON INFO"))

hay_otros_perfiles <- n_distinct(df$perfilRiesgo, na.rm = TRUE) > 1
alto <- df %>% filter(toupper(trimws(perfilRiesgo)) == "PERFIL ALTO")
cat(sprintf("Registros totales: %s | PERFIL ALTO: %s\n",
            format(nrow(df), big.mark = ","), format(nrow(alto), big.mark = ",")))

# =============================================================================
# 2. Distribución de PERFIL ALTO por rango de clasificación
# =============================================================================
dist <- alto %>%
  count(rangoClasificacion, name = "clientes") %>%
  arrange(rangoClasificacion) %>%
  mutate(pct = clientes / sum(clientes), pct_acum = cumsum(pct))

# =============================================================================
# 3. Cruces de contexto: ¿por qué un AAA/AA/A cae en PERFIL ALTO?
#    (% por columna = distribución dentro de cada rangoClasificacion)
# =============================================================================
vars_ctx <- intersect(c("tipoModelo", "nombreModelo", "perfilRiesgoAnt", "segmentoNewCliente",
                        "estadoCliente", "nombreRegionOficina", "fuente", "rangoScoreBuro",
                        "calificacion"), names(alto))
cruces <- lapply(vars_ctx, function(v) {
  cat_v <- ifelse(is.na(alto[[v]]), "(vacío)", alto[[v]])
  tb <- prop.table(table(cat_v, alto$rangoClasificacion), 2)
  out <- as.data.frame.matrix(round(tb, 4))
  cbind(categoria = rownames(out), out, row.names = NULL)
})
names(cruces) <- vars_ctx

# =============================================================================
# 4. Perfil de buró por banda: cobertura, % > 0, media, p25, mediana, p75
# =============================================================================
resumen_var <- function(x) {
  c(n            = length(x),
    pct_con_dato = mean(!is.na(x)),
    pct_mayor_0  = mean(!is.na(x) & x > 0),
    media        = if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE),
    p25          = quantile(x, .25, na.rm = TRUE, names = FALSE),
    mediana      = median(x, na.rm = TRUE),
    p75          = quantile(x, .75, na.rm = TRUE, names = FALSE))
}

estad <- do.call(rbind, lapply(split(alto, alto$rangoClasificacion), function(g) {
  do.call(rbind, lapply(VARS_BURO, function(v) {
    data.frame(rangoClasificacion = g$rangoClasificacion[1], variable = v,
               t(resumen_var(g[[v]])), check.names = FALSE)
  }))
}))
rownames(estad) <- NULL

pivotear <- function(col) {
  estad %>%
    select(variable, rangoClasificacion, all_of(col)) %>%
    pivot_wider(names_from = rangoClasificacion, values_from = all_of(col)) %>%
    arrange(match(variable, VARS_BURO))
}
medianas  <- pivotear("mediana")
cobertura <- pivotear("pct_con_dato")
pct_pos   <- pivotear("pct_mayor_0")

# Vista resumida por grupo (AAA-AA-A / otras con info / sin info)
por_grupo <- function(datos, grupo, f) {
  m <- sapply(split(datos[VARS_BURO], grupo), function(g) sapply(g, f))
  data.frame(variable = rownames(m), m, check.names = FALSE, row.names = NULL)
}
med_grupo <- por_grupo(alto, alto$grupo, function(x) median(x, na.rm = TRUE))
cob_grupo <- por_grupo(alto, alto$grupo, function(x) mean(!is.na(x)))

# =============================================================================
# 5. Poder discriminante: AAA-AA-A vs otras bandas CON información (dentro de PERFIL ALTO)
#    KS e IV por variable -> qué variables explican que sean AAA/AA/A
# =============================================================================
ks_stat <- function(a, b) {
  a <- a[!is.na(a)]; b <- b[!is.na(b)]
  if (!length(a) || !length(b)) return(NA_real_)
  g <- sort(unique(c(a, b)))
  max(abs(ecdf(a)(g) - ecdf(b)(g)))
}

iv_calc <- function(x, y, bins = 10) {
  bin <- rep("MISSING", length(x))
  m <- !is.na(x)
  if (any(m)) {
    br <- unique(quantile(x[m], probs = seq(0, 1, length.out = bins + 1), names = FALSE))
    bin[m] <- if (length(br) > 1) as.character(cut(x[m], breaks = br, include.lowest = TRUE))
              else "VALOR_UNICO"
  }
  t  <- table(bin, factor(y, levels = c(0, 1)))
  b1 <- t[, "1"]; b0 <- t[, "0"]
  pb <- (b1 + .5) / (sum(b1) + .5)
  pm <- (b0 + .5) / (sum(b0) + .5)
  sum((pb - pm) * log(pb / pm))
}

con_info <- alto %>% filter(grupo != "3. SIN INFORMACION")
y <- as.integer(con_info$grupo == "1. AAA-AA-A")

disc <- data.frame()
if (length(unique(y)) == 2) {
  disc <- do.call(rbind, lapply(VARS_BURO, function(v) {
    x <- con_info[[v]]
    data.frame(variable               = v,
               KS                     = ks_stat(x[y == 1], x[y == 0]),
               IV                     = iv_calc(x, y),
               mediana_AAA_AA_A       = median(x[y == 1], na.rm = TRUE),
               mediana_otras_con_info = median(x[y == 0], na.rm = TRUE))
  }))
  disc$poder <- cut(disc$IV, c(-Inf, .02, .1, .3, .5, Inf),
                    labels = c("Nulo", "Débil", "Medio", "Fuerte", "Sospechoso (>0.5)"))
  disc <- disc[order(-disc$IV), ]
}

# =============================================================================
# 6. Reglas: árbol de decisión poco profundo AAA-AA-A vs otras con info
#    Se excluye scoreBuro: interesa ver QUÉ variables de buró llevan al buen score
# =============================================================================
reglas <- "No se pudo entrenar el árbol (no hay ambos grupos)."
importancia <- data.frame()
if (length(unique(y)) == 2) {
  X_vars <- setdiff(VARS_BURO, "scoreBuro")
  datos_arbol <- con_info[X_vars]
  datos_arbol[is.na(datos_arbol)] <- -1          # -1 = sin dato
  datos_arbol$objetivo <- factor(ifelse(y == 1, "AAA_AA_A", "OTRAS"))
  pesos <- ifelse(y == 1, 0.5 / mean(y), 0.5 / mean(1 - y))   # equivalente a class_weight = "balanced"

  arbol <- rpart(objetivo ~ ., data = datos_arbol, weights = pesos, method = "class",
                 control = rpart.control(maxdepth = 4,
                                         minbucket = max(20, floor(nrow(datos_arbol) * .02)),
                                         cp = 0.001))
  reglas <- capture.output(print(arbol))
  if (requireNamespace("rpart.plot", quietly = TRUE)) {
    reglas <- c(reglas, "", "Reglas legibles:", capture.output(print(rpart.plot::rpart.rules(arbol, cover = TRUE))))
  }
  if (!is.null(arbol$variable.importance)) {
    imp <- sort(arbol$variable.importance, decreasing = TRUE)
    importancia <- data.frame(variable = names(imp), importancia = round(imp / sum(imp), 3),
                              row.names = NULL)
  }
}

# =============================================================================
# 7. (Si la base trae otros perfiles) AAA-AA-A en ALTO vs AAA-AA-A en otros perfiles
# =============================================================================
comp_perfil <- data.frame()
if (hay_otros_perfiles) {
  buenos <- df %>% filter(grupo == "1. AAA-AA-A", !is.na(perfilRiesgo))
  if (nrow(buenos) > 0) {
    comp_perfil <- por_grupo(buenos, buenos$perfilRiesgo, function(x) median(x, na.rm = TRUE))
  }
}

# =============================================================================
# 8. Exportar
# =============================================================================
wb <- createWorkbook()
hoja <- function(nombre, datos) { addWorksheet(wb, nombre); writeData(wb, nombre, datos) }

hoja("1_Distribucion", dist)
addWorksheet(wb, "2_Cruces")
fila <- 1
for (v in names(cruces)) {
  writeData(wb, "2_Cruces", paste0(v, "  (% por columna de rangoClasificacion)"), startRow = fila)
  writeData(wb, "2_Cruces", cruces[[v]], startRow = fila + 1)
  fila <- fila + nrow(cruces[[v]]) + 4
}
hoja("3_Mediana_por_grupo", med_grupo)
hoja("3b_Cobertura_por_grupo", cob_grupo)
hoja("4_Mediana_por_banda", medianas)
hoja("4b_Cobertura_por_banda", cobertura)
hoja("4c_Pct_mayor0_por_banda", pct_pos)
hoja("5_Estadisticos_detalle", estad)
if (nrow(disc)) hoja("6_Poder_discriminante", disc)
hoja("7_Reglas_arbol", data.frame(reglas = reglas))
if (nrow(importancia)) hoja("7b_Importancia_vars", importancia)
if (nrow(comp_perfil)) hoja("8_AAA_A_por_perfil", comp_perfil)
saveWorkbook(wb, SALIDA, overwrite = TRUE)

# ---- Consola ----------------------------------------------------------------
cat("\n=== Distribución PERFIL ALTO ===\n"); print(dist)
if (nrow(disc)) { cat("\n=== Top 15 variables que distinguen AAA-AA-A (IV) ===\n"); print(head(disc, 15), row.names = FALSE) }
cat("\n=== Reglas ===\n"); cat(reglas, sep = "\n")
if (nrow(importancia)) { cat("\nImportancia de variables:\n"); print(head(importancia, 15), row.names = FALSE) }
cat("\nResultados en:", normalizePath(SALIDA, mustWork = FALSE), "\n")
