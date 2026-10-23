# Regresión lineal múltiple y modelos dinámicos (espacio de estados) en R

De la regresión lineal clásica a modelos de espacio de estados con el paquete `dlm`: validación de supuestos, regresión dinámica, filtro de Kalman y suavizado.

## Qué encontrarás aquí

| Archivo | Qué hace |
|---|---|
| `scripts/regresion_lineal_multiple.R` | Regresión lineal múltiple con diagnóstico (residuos, VIF, Breusch-Pagan, normalidad). |
| `scripts/validacion_regresion.R` | Validación de supuestos de la regresión. |
| `scripts/regresion_multiple_dlm.R` | Regresión múltiple comparada con regresión dinámica (`dlmModReg`) y filtro de Kalman. |
| `scripts/modelo_de_regresion.R` | Regresión dinámica con estimación de máxima verosimilitud (`dlmMLE`), filtrado y suavizado. |
| `scripts/regresion_dlm_1.R` | Regresión dinámica con `dlm`: ajuste, VIF y pronóstico. |
| `scripts/regresion_dlm_2.R` | Variante del modelo anterior con tendencia local (`dlmModPoly`). |
| `scripts/regresion_con_errores_arima.R` | Regresión con componente ARIMA y modelo dinámico. |
| `scripts/modelos_espacio_de_estados.R` | Modelos de espacio de estados: tendencia local, verosimilitud, filtro y suavizado. |
| `scripts/regresion_espacio_de_estados.R` | Regresión multivariante en espacio de estados. |
| `scripts/modelo_completo.Rmd` | Documento R Markdown con el modelo completo. |

## Cómo ejecutarlo

1. Descarga o clona este repositorio y abre RStudio.
2. Fija la carpeta del repositorio como directorio de trabajo (`Session > Set Working Directory`).
3. Los scripts leen los datos desde la carpeta `datos/`. Instala los paquetes que falten con `install.packages("nombre")`.

**Paquetes usados:** `GGally`, `PerformanceAnalytics`, `bigmemory`, `car`, `corrplot`, `devtools`, `dlm`, `dplyr`, `forecast`, `ggplot2`, `ggrepel`, `gridExtra`, `lmtest`, `lubridate`, `ppcor`, `psych`, `readxl`, `rjags`, `tidyverse`, `tseries`, `tsm`.

## Datos

Los scripts leen `datos/BD_Indice.xlsx` (hoja `BD`): base de elaboración propia con datos públicos (variables de humedad, temperatura y precipitación, e índice) para un estudio.

## Autoría

Código y análisis de Alejandra, con fines educativos y de portafolio. Más sobre mi trabajo en analítica de datos: [alejadata.pro](https://alejadata.pro/).
