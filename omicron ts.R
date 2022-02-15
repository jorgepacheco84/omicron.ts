library(TTR)
library(forecast)
library(ggplot2)

# Construir serie de tiempo de conteo de casos y logaritmo de casos

omicron <- c(1058, 1858, 3134, 3799, 4380, 4064, 4069, 3922, 4847, 7291, 8270, 
             9284, 9454, 8904, 7533, 9509, 12500, 14757, 14449, 13674, 14780,
             14291, 18446, 24037, 26727, 29175, 28480, 26244, 22192, 29844, 35197, 
             37468, 36297, 34328, 31063, 24970, 33362, 36635, 38446, 35841, 35038,
             28076)
omicron.ts <- ts(omicron, frequency = 7)
plot.ts(omicron.ts)
log_omicron <- log(omicron)
log_omicron.ts <- ts(log_omicron, frequency = 7)
plot.ts(log_omicron.ts)

# Descomponer ambas series de tiempo según tendencia, estacionalidad y variabilidad aleatoria

omicron.ts.dec <- decompose(omicron.ts)
plot(omicron.ts.dec)
log_omicron.ts.dec <- decompose(log_omicron.ts)
plot(log_omicron.ts.dec)
log_omicron.ts.seasonadj <- log_omicron.ts - log_omicron.ts.dec$seasonal
plot(log_omicron.ts.seasonadj)

# Primera proyección con todos los datos

log_omicron_forecasts <- HoltWinters(log_omicron.ts)
plot(log_omicron_forecasts)
omicron.pred <- predict(log_omicron_forecasts, 48, prediction.interval = TRUE, level=0.95)

plot(log_omicron.ts, xlim = c(1,8), ylim = c(7,12))
lines(omicron.pred[,1], lty=2, col="blue")
lines(omicron.pred[,2], lty=2, col="orange")
lines(omicron.pred[,3], lty=2, col="orange")

# Segunda proyección hasta semana 35

omicron2 <- omicron[-35:-42]
log_omicron2 <- log(omicron2)
log_omicron.ts2 <- ts(log_omicron2, frequency = 7)
plot.ts(log_omicron.ts2)

log_omicron_forecasts2 <- HoltWinters(log_omicron.ts2)
plot(log_omicron_forecasts2)
omicron.pred2 <- predict(log_omicron_forecasts2, 48, prediction.interval = TRUE, level=0.95)

log_omicron.ts[1:34] = NA

plot(exp(log_omicron.ts2), ylab = "N� de casos nuevos", xlab = "Semanas", xlim = c(1,8), ylim = c(0,120000))
lines(exp(log_omicron.ts), col = "red")
lines(exp(omicron.pred2[,1]), lty=2, col="blue")
lines(exp(omicron.pred2[,2]), lty=2, col="orange")
lines(exp(omicron.pred2[,3]), lty=2, col="orange")

plot(log_omicron.ts2, ylab = "Logaritmo de casos nuevos", xlab = "Semanas", xlim = c(1,8), ylim = c(7,12))
lines(log_omicron.ts, col = "red")
lines(omicron.pred2[,1], lty=2, col="blue")
lines(omicron.pred2[,2], lty=2, col="orange")
lines(omicron.pred2[,3], lty=2, col="orange")

exp(omicron.pred[36,1]) - exp(log_omicron.ts)[36] +
exp(omicron.pred[37,1]) - exp(log_omicron.ts)[37] +
exp(omicron.pred[38,1]) - exp(log_omicron.ts)[38] +
exp(omicron.pred[39,1]) - exp(log_omicron.ts)[39] +
exp(omicron.pred[40,1]) - exp(log_omicron.ts)[40] +
exp(omicron.pred[41,1]) - exp(log_omicron.ts)[41] +
exp(omicron.pred[42,1]) - exp(log_omicron.ts)[42]
