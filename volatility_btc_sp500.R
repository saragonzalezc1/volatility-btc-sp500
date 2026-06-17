
# -----------------------------------------------------------------------------
# 0. INSTALACIÓN Y CARGA DE PAQUETES
# -----------------------------------------------------------------------------

library(readxl)
library(forecast)
library(tseries)
library(data.table)
library(ggplot2)
library(timeDate)
library(timeSeries)
library(fBasics)
library(FinTS)
library(zoo)
library(fGarch)
library(rugarch)
library(writexl)

# -----------------------------------------------------------------------------
# 1. CARGA Y PROCESAMIENTO DE DATOS
# -----------------------------------------------------------------------------

ruta_archivo <- "market_prices.xlsx"  # Ajusta el path según tu directorio de trabajo

Y <- read_excel(ruta_archivo, sheet = "Sheet1")

# Eliminar filas con valores faltantes en cualquiera de las dos series
Y <- Y[!is.na(Y$r_BTC), ]
Y <- Y[!is.na(Y$r_SP500), ]

cat("Dataset cargado:", nrow(Y), "observaciones diarias\n\n")

# -----------------------------------------------------------------------------
# 2. ANÁLISIS EXPLORATORIO: GRÁFICAS DE RETORNOS
# -----------------------------------------------------------------------------

# ---- 2.1 Retornos diarios de Bitcoin ----
ggplot(Y, aes(x = 1:nrow(Y), y = r_BTC)) +
  geom_line(color = "#12719e", linewidth = 0.4) +
  geom_hline(yintercept = 0, color = "darkred", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Retornos Diarios: Bitcoin",
       x     = "Observaciones",
       y     = "Retorno Logarítmico")

# ---- 2.2 Retornos diarios del S&P 500 ----
ggplot(Y, aes(x = 1:nrow(Y), y = r_SP500)) +
  geom_line(color = "#12719e", linewidth = 0.4) +
  geom_hline(yintercept = 0, color = "darkred", linetype = "dashed") +
  theme_minimal() +
  labs(title = "Retornos Diarios: S&P 500",
       x     = "Observaciones",
       y     = "Retorno Logarítmico")

# -----------------------------------------------------------------------------
# 3. ANÁLISIS DE NORMALIDAD
# -----------------------------------------------------------------------------

# ---- 3.1 Bitcoin ----

# Parámetros para curva normal teórica
mu_btc    <- mean(Y$r_BTC, na.rm = TRUE)
sigma_btc <- sd(Y$r_BTC,   na.rm = TRUE)

# Histograma con densidad empírica y curva normal teórica superpuesta
ggplot(Y, aes(x = r_BTC)) +
  geom_histogram(aes(y = ..density..),
                 bins  = 50,
                 fill  = "steelblue",
                 color = "white",
                 alpha = 0.7) +
  geom_density(color = "darkred", linewidth = 1) +
  stat_function(fun      = dnorm,
                args     = list(mean = mu_btc, sd = sigma_btc),
                color    = "black",
                linewidth = 1,
                linetype = "dashed") +
  theme_classic() +
  labs(title = "Distribución de los Retornos de BTC",
       x     = "Retorno",
       y     = "Densidad")

# Test de normalidad Jarque-Bera
# H0: los retornos siguen una distribución normal
cat("=== JARQUE-BERA: Bitcoin ===\n")
jarqueberaTest(Y$r_BTC)

# ---- 3.2 S&P 500 ----

mu_sp    <- mean(Y$r_SP500, na.rm = TRUE)
sigma_sp <- sd(Y$r_SP500,   na.rm = TRUE)

ggplot(Y, aes(x = r_SP500)) +
  geom_histogram(aes(y = ..density..),
                 bins  = 50,
                 fill  = "steelblue",
                 color = "white",
                 alpha = 0.7) +
  geom_density(color = "darkred", linewidth = 1) +
  stat_function(fun      = dnorm,
                args     = list(mean = mu_sp, sd = sigma_sp),
                color    = "black",
                linewidth = 1,
                linetype = "dashed") +
  theme_classic() +
  labs(title = "Distribución de los Retornos de S&P 500",
       x     = "Retorno",
       y     = "Densidad")

cat("=== JARQUE-BERA: S&P 500 ===\n")
jarqueberaTest(Y$r_SP500)

# -----------------------------------------------------------------------------
# 4. DIAGNÓSTICOS PREVIOS A LA MODELACIÓN
# -----------------------------------------------------------------------------
# Se evalúa autocorrelación en retornos y en retornos al cuadrado,
# y se detecta heterocedasticidad condicional (efectos ARCH).
# -----------------------------------------------------------------------------

# ---- 4.1 Bitcoin ----

cat("=== DIAGNÓSTICOS: Bitcoin ===\n")

# Ljung-Box sobre retornos (H0: no hay autocorrelación en la media)
cat("Ljung-Box sobre retornos:\n")
print(Box.test(Y$r_BTC, lag = 20, type = "Ljung-Box"))

# ACF y PACF de retornos al cuadrado
# Autocorrelación significativa → evidencia de heterocedasticidad condicional
acf(Y$r_BTC^2,  main = "ACF de r_BTC²")
pacf(Y$r_BTC^2, main = "PACF de r_BTC²")

# Ljung-Box sobre retornos al cuadrado (H0: no hay efectos ARCH)
cat("Ljung-Box sobre retornos al cuadrado:\n")
print(Box.test(Y$r_BTC^2, lag = 20, type = "Ljung-Box"))

# Test ARCH-LM (H0: no hay efectos ARCH)
cat("Test ARCH-LM:\n")
print(ArchTest(Y$r_BTC, lags = 12))

# ---- 4.2 S&P 500 ----

cat("\n=== DIAGNÓSTICOS: S&P 500 ===\n")

cat("Ljung-Box sobre retornos:\n")
print(Box.test(Y$r_SP500, lag = 20, type = "Ljung-Box"))

acf(Y$r_SP500^2,  main = "ACF de r_SP500²")
pacf(Y$r_SP500^2, main = "PACF de r_SP500²")

# ACF y PACF sobre retornos (para detectar estructura en la media → AR(1))
acf(Y$r_SP500,  main = "ACF de r_SP500")
pacf(Y$r_SP500, main = "PACF de r_SP500")

cat("Ljung-Box sobre retornos al cuadrado:\n")
print(Box.test(Y$r_SP500^2, lag = 20, type = "Ljung-Box"))

cat("Test ARCH-LM:\n")
print(ArchTest(Y$r_SP500, lags = 12))

# -----------------------------------------------------------------------------
# 5. ESTIMACIÓN DE VOLATILIDAD
# -----------------------------------------------------------------------------

window <- 250          # Ventana de 250 días (≈ 1 año bursátil)
n      <- nrow(Y)

# -----------------------------------------------------------------------------
# 5.1 VOLATILIDAD HISTÓRICA (ventana móvil)
# -----------------------------------------------------------------------------
# Benchmark: asigna igual peso a todas las observaciones en la ventana.
# No modela explícitamente la dinámica temporal de la varianza.
# -----------------------------------------------------------------------------

# ---- Bitcoin ----
vol_hist_BTC      <- rollapply(Y$r_BTC, width = window,
                                FUN = sd, align = "right", fill = NA)
vol_hist_1d_BTC   <- tail(vol_hist_BTC, 1)
mean_hist_BTC     <- mean(vol_hist_BTC, na.rm = TRUE)

plot(vol_hist_BTC, type = "l",
     main = "Volatilidad Histórica (Ventana 250) - BTC",
     ylab = "Volatilidad", xlab = "Tiempo")

# ---- S&P 500 ----
vol_hist_SP500    <- rollapply(Y$r_SP500, width = window,
                                FUN = sd, align = "right", fill = NA)
vol_hist_1d_SP500 <- tail(vol_hist_SP500, 1)
mean_hist_SP500   <- mean(vol_hist_SP500, na.rm = TRUE)

plot(vol_hist_SP500, type = "l",
     main = "Volatilidad Histórica (Ventana 250) - S&P 500",
     ylab = "Volatilidad", xlab = "Tiempo")

# -----------------------------------------------------------------------------
# 5.2 EWMA — RiskMetrics (λ = 0.94)
# -----------------------------------------------------------------------------
# Asigna mayor peso a observaciones recientes.
# Reacciona más rápido a choques que la volatilidad histórica.
# -----------------------------------------------------------------------------

lambda <- 0.94

# ---- Bitcoin ----
vol_ewma_BTC        <- rep(NA, n)
vol_ewma_BTC[window] <- var(Y$r_BTC[1:window])   # Inicializar con varianza histórica

for (t in (window + 1):n) {
  vol_ewma_BTC[t] <- lambda * vol_ewma_BTC[t - 1] +
    (1 - lambda) * Y$r_BTC[t - 1]^2
}
vol_ewma_BTC <- sqrt(vol_ewma_BTC)

# Pronóstico 1 día adelante: σ²(t+1) = λ·σ²(t) + (1-λ)·r²(t)
var_forecast_btc  <- lambda * tail(vol_ewma_BTC, 1)^2 +
  (1 - lambda) * tail(Y$r_BTC, 1)^2
vol_ewma_1d_BTC   <- sqrt(var_forecast_btc)
mean_ewma_BTC     <- mean(vol_ewma_BTC, na.rm = TRUE)

plot(vol_ewma_BTC, type = "l",
     main = "Volatilidad EWMA (λ = 0.94) - BTC",
     ylab = "Volatilidad", xlab = "Tiempo")

# ---- S&P 500 ----
vol_ewma_SP500        <- rep(NA, n)
vol_ewma_SP500[window] <- var(Y$r_SP500[1:window])

for (t in (window + 1):n) {
  vol_ewma_SP500[t] <- lambda * vol_ewma_SP500[t - 1] +
    (1 - lambda) * Y$r_SP500[t - 1]^2
}
vol_ewma_SP500 <- sqrt(vol_ewma_SP500)

var_forecast_sp   <- lambda * tail(vol_ewma_SP500, 1)^2 +
  (1 - lambda) * tail(Y$r_SP500, 1)^2
vol_ewma_1d_SP500 <- sqrt(var_forecast_sp)
mean_ewma_SP500   <- mean(vol_ewma_SP500, na.rm = TRUE)

plot(vol_ewma_SP500, type = "l",
     main = "Volatilidad EWMA (λ = 0.94) - S&P 500",
     ylab = "Volatilidad", xlab = "Tiempo")

# -----------------------------------------------------------------------------
# 5.3 ARCH
# -----------------------------------------------------------------------------
# Orden seleccionado según PACF de retornos al cuadrado:
#   Bitcoin → ARCH(5)   (dependencia significativa hasta lag 5)
#   S&P 500 → ARCH(6)   (dependencia significativa hasta lag 6)
# Se usa distribución t-Student por presencia de colas pesadas.
# -----------------------------------------------------------------------------

# ---- Bitcoin: ARCH(5) ----
spec_arch_BTC <- ugarchspec(
  variance.model    = list(model = "sGARCH", garchOrder = c(5, 0)),
  mean.model        = list(armaOrder = c(0, 0)),
  distribution.model = "std"
)
fit_arch_BTC     <- ugarchfit(spec = spec_arch_BTC, data = Y$r_BTC)
sigma_arch_BTC   <- sigma(fit_arch_BTC)
fore_arch_BTC    <- ugarchforecast(fit_arch_BTC, n.ahead = 1)
vol_arch_1d_BTC  <- as.numeric(sigma(fore_arch_BTC))
mean_arch_BTC    <- as.numeric(mean(sigma_arch_BTC))

cat("\n=== ARCH(5): Bitcoin ===\n")
show(fit_arch_BTC)

plot(as.numeric(sigma_arch_BTC), type = "l",
     main = "Volatilidad Condicional ARCH(5) - BTC",
     ylab = "Volatilidad", xlab = "Tiempo")

# ---- S&P 500: ARCH(6) con AR(1) en la media ----
# AR(1) en la media porque Ljung-Box detectó autocorrelación en los retornos
spec_arch_SP500 <- ugarchspec(
  variance.model    = list(model = "sGARCH", garchOrder = c(6, 0)),
  mean.model        = list(armaOrder = c(1, 0)),
  distribution.model = "std"
)
fit_arch_SP500    <- ugarchfit(spec = spec_arch_SP500, data = Y$r_SP500)
sigma_arch_SP500  <- sigma(fit_arch_SP500)
fore_arch_SP500   <- ugarchforecast(fit_arch_SP500, n.ahead = 1)
vol_arch_1d_SP500 <- as.numeric(sigma(fore_arch_SP500))
mean_arch_SP500   <- as.numeric(mean(sigma_arch_SP500))

cat("\n=== ARCH(6): S&P 500 ===\n")
show(fit_arch_SP500)

plot(as.numeric(sigma_arch_SP500), type = "l",
     main = "Volatilidad Condicional ARCH(6) - S&P 500",
     ylab = "Volatilidad", xlab = "Tiempo")

# -----------------------------------------------------------------------------
# 5.4 GARCH(1,1)
# -----------------------------------------------------------------------------
# Captura persistencia de la volatilidad con solo 3 parámetros.
# α+β cercano a 1 indica alta persistencia (proceso casi-integrado IGARCH).
# -----------------------------------------------------------------------------

# ---- Bitcoin ----
spec_garch_BTC <- ugarchspec(
  variance.model    = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model        = list(armaOrder = c(0, 0)),
  distribution.model = "std"
)
fit_garch_BTC    <- ugarchfit(spec = spec_garch_BTC, data = Y$r_BTC)
sigma_garch_BTC  <- sigma(fit_garch_BTC)
fore_garch_BTC   <- ugarchforecast(fit_garch_BTC, n.ahead = 1)
vol_garch_1d_BTC <- as.numeric(sigma(fore_garch_BTC))
mean_garch_BTC   <- as.numeric(mean(sigma_garch_BTC))

cat("\n=== GARCH(1,1): Bitcoin ===\n")
show(fit_garch_BTC)
cat("Persistencia (α+β):", round(
  coef(fit_garch_BTC)["alpha1"] + coef(fit_garch_BTC)["beta1"], 4), "\n")

plot(as.numeric(sigma_garch_BTC), type = "l",
     main = "Volatilidad Condicional GARCH(1,1) - BTC",
     ylab = "Volatilidad", xlab = "Tiempo")

# ---- S&P 500 ----
spec_garch_SP500 <- ugarchspec(
  variance.model    = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model        = list(armaOrder = c(1, 0)),
  distribution.model = "std"
)
fit_garch_SP500    <- ugarchfit(spec = spec_garch_SP500, data = Y$r_SP500)
sigma_garch_SP500  <- sigma(fit_garch_SP500)
fore_garch_SP500   <- ugarchforecast(fit_garch_SP500, n.ahead = 1)
vol_garch_1d_SP500 <- as.numeric(sigma(fore_garch_SP500))
mean_garch_SP500   <- as.numeric(mean(sigma_garch_SP500))

cat("\n=== GARCH(1,1): S&P 500 ===\n")
show(fit_garch_SP500)
cat("Persistencia (α+β):", round(
  coef(fit_garch_SP500)["alpha1"] + coef(fit_garch_SP500)["beta1"], 4), "\n")

plot(as.numeric(sigma_garch_SP500), type = "l",
     main = "Volatilidad Condicional GARCH(1,1) - S&P 500",
     ylab = "Volatilidad", xlab = "Tiempo")

# -----------------------------------------------------------------------------
# 5.5 GJR-GARCH(1,1)
# -----------------------------------------------------------------------------
# Extiende el GARCH capturando el efecto apalancamiento (leverage effect):
# los retornos negativos generan mayor volatilidad que los positivos.
# γ > 0 y significativo → presencia de efecto apalancamiento.
# Persistencia: α + β + γ/2
# -----------------------------------------------------------------------------

# ---- Bitcoin ----
spec_gjr_BTC <- ugarchspec(
  variance.model    = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model        = list(armaOrder = c(0, 0)),
  distribution.model = "std"
)
fit_gjr_BTC    <- ugarchfit(spec = spec_gjr_BTC, data = Y$r_BTC)
sigma_gjr_BTC  <- sigma(fit_gjr_BTC)
fore_gjr_BTC   <- ugarchforecast(fit_gjr_BTC, n.ahead = 1)
vol_gjr_1d_BTC <- as.numeric(sigma(fore_gjr_BTC))
mean_gjr_BTC   <- as.numeric(mean(sigma_gjr_BTC))

cat("\n=== GJR-GARCH(1,1): Bitcoin ===\n")
show(fit_gjr_BTC)
cat("Persistencia (α+β+γ/2):", round(
  coef(fit_gjr_BTC)["alpha1"] + coef(fit_gjr_BTC)["beta1"] +
    coef(fit_gjr_BTC)["gamma1"] * 0.5, 4), "\n")

plot(as.numeric(sigma_gjr_BTC), type = "l",
     main = "Volatilidad Condicional GJR-GARCH(1,1) - BTC",
     ylab = "Volatilidad", xlab = "Tiempo")

# ---- S&P 500 ----
spec_gjr_SP500 <- ugarchspec(
  variance.model    = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model        = list(armaOrder = c(1, 0)),
  distribution.model = "std"
)
fit_gjr_SP500    <- ugarchfit(spec = spec_gjr_SP500, data = Y$r_SP500)
sigma_gjr_SP500  <- sigma(fit_gjr_SP500)
fore_gjr_SP500   <- ugarchforecast(fit_gjr_SP500, n.ahead = 1)
vol_gjr_1d_SP500 <- as.numeric(sigma(fore_gjr_SP500))
mean_gjr_SP500   <- as.numeric(mean(sigma_gjr_SP500))

cat("\n=== GJR-GARCH(1,1): S&P 500 ===\n")
show(fit_gjr_SP500)
cat("Persistencia (α+β+γ/2):", round(
  coef(fit_gjr_SP500)["alpha1"] + coef(fit_gjr_SP500)["beta1"] +
    coef(fit_gjr_SP500)["gamma1"] * 0.5, 4), "\n")

plot(as.numeric(sigma_gjr_SP500), type = "l",
     main = "Volatilidad Condicional GJR-GARCH(1,1) - S&P 500",
     ylab = "Volatilidad", xlab = "Tiempo")

# -----------------------------------------------------------------------------
# 6. TABLA RESUMEN DE VOLATILIDADES
# -----------------------------------------------------------------------------

tabla_resumen <- data.frame(
  Activo             = rep(c("Bitcoin", "S&P 500"), each = 5),
  Modelo             = rep(c("Histórica", "EWMA", "ARCH",
                             "GARCH(1,1)", "GJR-GARCH(1,1)"), 2),
  Volatilidad_1dia   = c(vol_hist_1d_BTC,  vol_ewma_1d_BTC,
                         vol_arch_1d_BTC,  vol_garch_1d_BTC,
                         vol_gjr_1d_BTC,
                         vol_hist_1d_SP500, vol_ewma_1d_SP500,
                         vol_arch_1d_SP500, vol_garch_1d_SP500,
                         vol_gjr_1d_SP500),
  Volatilidad_Promedio = c(mean_hist_BTC,  mean_ewma_BTC,
                           mean_arch_BTC,  mean_garch_BTC,
                           mean_gjr_BTC,
                           mean_hist_SP500, mean_ewma_SP500,
                           mean_arch_SP500, mean_garch_SP500,
                           mean_gjr_SP500)
)

cat("\n=== TABLA RESUMEN DE VOLATILIDADES ===\n")
print(round(tabla_resumen[, 3:4] * 100, 4))   # Mostrar en porcentaje
print(tabla_resumen)

# -----------------------------------------------------------------------------
# 7. MODELO CCC — VOLATILIDAD DEL PORTAFOLIO (60% BTC / 40% S&P 500)
# -----------------------------------------------------------------------------
# Constant Conditional Correlation (CCC):
#   - Volatilidad individual: GARCH(1,1) para BTC, GJR-GARCH(1,1) para S&P 500
#   - Correlación entre residuos estandarizados: constante en el tiempo
#   - Volatilidad del portafolio: σ²_p = w'Σw
#
# Nota: el loop de ventanas rodantes usa GARCH(1,1) estándar para ambos
# activos como aproximación. Para el S&P 500 se incorpora el coeficiente
# gamma del GJR estimado globalmente (fit_gjr_SP500). Esto es una
# simplificación deliberada — una re-estimación completa del GJR en cada
# ventana sería más rigurosa pero computacionalmente costosa.
# -----------------------------------------------------------------------------

Wi <- as.matrix(c(0.6, 0.4))   # Pesos del portafolio: 60% BTC, 40% S&P 500
M  <- n - window + 1            # Número de ventanas

VolatilidadCCC <- matrix(0, nrow = M, ncol = 3)
# Columnas: [1] BTC, [2] S&P 500, [3] Portafolio

retornos_BTC <- Y$r_BTC
retornos_SP  <- Y$r_SP500

# ---- 7.1 Volatilidad rodante BTC: GARCH(1,1) ----
for (i in 1:M) {

  Contador <- window + i - 1
  ret      <- retornos_BTC[i:Contador]

  Garch  <- garch(ret, order = c(1, 1))
  sigma2 <- Garch$fitted.values[, 1]

  omega <- Garch$coef[1]
  alpha <- Garch$coef[2]
  beta  <- Garch$coef[3]

  # Pronóstico 1 paso: σ²(t+1) = ω + α·r²(t) + β·σ²(t)
  sigma_forecast     <- omega + alpha * ret[window]^2 + beta * sigma2[window]
  VolatilidadCCC[i, 1] <- sqrt(sigma_forecast)
}

# ---- 7.2 Volatilidad rodante S&P 500: GARCH(1,1) + gamma del GJR global ----
gamma_gjr <- coef(fit_gjr_SP500)["gamma1"]

for (i in 1:M) {

  Contador <- window + i - 1
  ret      <- retornos_SP[i:Contador]

  Garch  <- garch(ret, order = c(1, 1))
  sigma2 <- Garch$fitted.values[, 1]

  omega <- Garch$coef[1]
  alpha <- Garch$coef[2]
  beta  <- Garch$coef[3]

  # Indicador de retorno negativo (efecto apalancamiento)
  I_t <- ifelse(ret[window] < 0, 1, 0)

  sigma_forecast <- omega +
    alpha   * ret[window]^2 +
    gamma_gjr * I_t * ret[window]^2 +
    beta    * sigma2[window]

  VolatilidadCCC[i, 2] <- sqrt(sigma_forecast)
}

# ---- 7.3 Correlación constante entre residuos estandarizados ----
# Se estiman modelos GARCH(1,1) sobre la muestra completa para obtener
# los residuos estandarizados y calcular la correlación condicional.
Garch_BTC_full <- garch(retornos_BTC, order = c(1, 1))
Garch_SP_full  <- garch(retornos_SP,  order = c(1, 1))

resid_BTC <- Garch_BTC_full$residuals
resid_SP  <- Garch_SP_full$residuals
sigma_BTC <- sqrt(Garch_BTC_full$fitted.values[, 1])
sigma_SP  <- sqrt(Garch_SP_full$fitted.values[, 1])

z_BTC <- resid_BTC / sigma_BTC
z_SP  <- resid_SP  / sigma_SP

rho <- cor(z_BTC, z_SP, use = "complete.obs")
cat("\nCorrelación condicional constante (ρ):", round(rho, 4), "\n")

# ---- 7.4 Volatilidad del portafolio ----
# σ²_p = w₁²·σ₁² + w₂²·σ₂² + 2·w₁·w₂·ρ·σ₁·σ₂
for (i in 1:M) {

  sigma1 <- VolatilidadCCC[i, 1]
  sigma2 <- VolatilidadCCC[i, 2]

  var_port <- Wi[1]^2 * sigma1^2 +
    Wi[2]^2 * sigma2^2 +
    2 * Wi[1] * Wi[2] * rho * sigma1 * sigma2

  VolatilidadCCC[i, 3] <- sqrt(var_port)
}

# ---- 7.5 Pronóstico de volatilidad diaria del portafolio ----
Volatilidad_1dia_CCC <- VolatilidadCCC[M, 3]
cat("Volatilidad diaria pronosticada del portafolio (CCC):",
    round(Volatilidad_1dia_CCC * 100, 3), "%\n")

# ---- 7.6 Exportar series de volatilidad CCC ----
Vol_CCC_df <- data.frame(
  t          = 1:M,
  BTC        = VolatilidadCCC[, 1],
  SP500      = VolatilidadCCC[, 2],
  Portafolio = VolatilidadCCC[, 3]
)

write_xlsx(Vol_CCC_df, "Volatilidad_CCC.xlsx")
cat("Series de volatilidad CCC exportadas a: Volatilidad_CCC.xlsx\n")
