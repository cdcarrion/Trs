# =============================================================================
# Análisis exploratorio: clientes PERFIL ALTO con rangoClasificacion AAA, AA, A
# Describe cada columna por grupo (AAA / AA / A)
# =============================================================================
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(openxlsx)
})

ARCHIVO <- "C:/ruta/a/tu_base.xlsx"   # <-- AJUSTAR
SALIDA  <- "eda_perfil_alto_AAA_AA_A.xlsx"

# ---- 1. Carga ---------------------------------------------------------------
base <- read_excel(ARCHIVO, col_types = "text", .name_repair = "minimal")
base <- base[, !duplicated(names(base))]          # quita columnas repetidas (scoreBuro, tipoModelo, ...)
base <- base %>% mutate(across(everything(), ~ na_if(trimws(.x), "")))

# ---- 2. Filtro: PERFIL ALTO y AAA / AA / A ----------------------------------
df <- base %>%
  mutate(banda = sub("^\\s*\\d+\\.\\s*", "", rangoClasificacion)) %>%   # "1. AAA" -> "AAA"
  filter(perfilRiesgo == "PERFIL ALTO", banda %in% c("AAA", "AA", "A")) %>%
  mutate(banda = factor(banda, levels = c("AAA", "AA", "A")))

cat("Clientes por grupo:\n"); print(table(df$banda))

# ---- 3. Tipos de columna ----------------------------------------------------
a_num <- function(x) suppressWarnings(as.numeric(gsub(",", "", x)))   # "163,757.93" -> 163757.93

excluir <- c("identificacionCliente", "banda", "codigoPeriodo", "codigoOficina",
             "fechaIngesta", "periodo",
             "minFechaAperturaSBS", "minFechaAperturaSEPS", "minFechaAperturaSICOM")

es_num <- sapply(df, function(x) {
  v <- x[!is.na(x)]
  length(v) > 0 && all(!is.na(a_num(v)))
})
vars_num <- setdiff(names(df)[es_num], excluir)
vars_cat <- setdiff(names(df)[!es_num], excluir)
vars_cat <- vars_cat[sapply(df[vars_cat], n_distinct) <= 30]          # solo categóricas con pocas categorías

df <- df %>% mutate(across(all_of(vars_num), a_num))

# ---- 4. Variables numéricas: estadísticos por grupo ------------------------
resumen_num <- df %>%
  select(banda, all_of(vars_num)) %>%
  pivot_longer(-banda, names_to = "variable", values_to = "valor") %>%
  group_by(variable, banda) %>%
  summarise(
    n            = n(),
    pct_con_dato = mean(!is.na(valor)),
    pct_mayor_0  = mean(valor > 0, na.rm = TRUE),
    media        = mean(valor, na.rm = TRUE),
    min          = quantile(valor, 0,    na.rm = TRUE, names = FALSE),
    p25          = quantile(valor, 0.25, na.rm = TRUE, names = FALSE),
    mediana      = quantile(valor, 0.50, na.rm = TRUE, names = FALSE),
    p75          = quantile(valor, 0.75, na.rm = TRUE, names = FALSE),
    max          = quantile(valor, 1,    na.rm = TRUE, names = FALSE),
    .groups = "drop"
  ) %>%
  mutate(variable = factor(variable, levels = vars_num)) %>%
  arrange(variable, banda)

# Vista lado a lado: mediana y media de AAA vs AA vs A
comparativo <- resumen_num %>%
  select(variable, banda, mediana, media) %>%
  pivot_wider(names_from = banda, values_from = c(mediana, media))

# ---- 5. Variables categóricas: frecuencias por grupo ------------------------
resumen_cat <- df %>%
  select(banda, all_of(vars_cat)) %>%
  pivot_longer(-banda, names_to = "variable", values_to = "valor") %>%
  mutate(valor = coalesce(valor, "(vacío)")) %>%
  count(variable, valor, banda) %>%
  group_by(variable, banda) %>%
  mutate(pct = round(n / sum(n), 4)) %>%
  ungroup() %>%
  pivot_wider(names_from = banda, values_from = c(n, pct), values_fill = 0) %>%
  arrange(variable, valor)

# ---- 6. Exportar ------------------------------------------------------------
wb <- createWorkbook()
for (h in list(list("Numericas_detalle", resumen_num),
               list("Numericas_comparativo", comparativo),
               list("Categoricas", resumen_cat))) {
  addWorksheet(wb, h[[1]]); writeData(wb, h[[1]], h[[2]]); freezePane(wb, h[[1]], firstRow = TRUE)
}
saveWorkbook(wb, SALIDA, overwrite = TRUE)

print(comparativo, n = Inf)
cat("\nResultados en:", normalizePath(SALIDA, mustWork = FALSE), "\n")
