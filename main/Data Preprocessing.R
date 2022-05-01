data = read.csv("lib/coivd-data.csv", header = TRUE, na.strings = "0")

head(data)
names(data) = c("id", "date", "total", "confirmed", "PUI")


data$date = as.Date(data$date,'%Y-%m-%d')

dayOfYear = as.numeric(format(data$date[1], "%j"))

series = ts(data$confirmed, frequency = 365, start = c(2020, dayOfYear))
plot.ts(series)

library(zoo)

series = na.locf(series)

plot.ts(series)

dser = diff(log(series),1)

#dser = na.locf(dser)

plot.ts(dser)

dser


plot.ts(log(series))


library(astsa)

acf2(ddser, max.lag = 20, ylim = c(-0.2,0.6))


sar = sarima(log(series), p = 4, d = 2, q = 5, P = 2, D = 0, Q = 0, S = 12)
ts.plot(series)

sarima.for(log(series), p = 2, d = 1, q = 0, P = 1, D = 0, Q = 0, S = 12, n.ahead = 12)

sarima.for(log(series), p = 4, d = 2, q = 5, P = 2, D = 0, Q = 0, S = 12, n.ahead = 12)



arima(log(series), order=c(2,1,0),seasonal = list(order = c(1,1,0), period = 12),method="ML")

arima(log(series), order=c(4,2,5),seasonal = list(order = c(2,0,0), period = 12),method="ML")

arima(log(series), order=c(4,2,5),seasonal = list(order = c(1,0,2), period = 12),method="ML")

