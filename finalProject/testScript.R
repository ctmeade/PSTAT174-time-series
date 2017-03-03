# Import Data
ts <- read.table("c02.txt", skip = 3, header = F, sep = "")

# Remove exraneous featurs
ts <- ts$V4

# Encode missing values
ts <- ifelse(ts == -99.99, NA, ts)

# Convert vector to time series object
ts <- ts(ts, start=c(1958,3), frequency=12)

plot.ts(ts)
mean(ts, na.rm=T)
abline(h=mean(ts, na.rm=T))

#Difference
ts.12 <- diff(ts, lag = 12)
plot(ts.12)

# Good differencing order, variance decreases
ts.12.1 <- diff(ts.12, lag = 1)
plot(ts.12.1)
var(ts, na.rm = T)
var(ts.12, na.rm = T)
var(ts.12.1, na.rm = T)

#ts.12.1 stationary

### ACF

acf <- acf(ts.12.1, type = "correlation", plot = F, na.action = na.pass, lag.max=12*5)

## Transform the lags from years to months
acf$lag <- acf$lag * 12

## Plot the acf 
plot(acf, xlab="Lag (months)")

### PACF
pacf <- acf(ts.12.1, type = "partial", plot = F, na.action = na.pass, lag.max= 12*5)
pacf$lag <- pacf$lag * 12
plot(pacf, xlab = "Lag (months)")

## Seasonal: ACF cuts off after lag 1 (12), PACF tailing off

model1 <- arima(ts.12.1, order = c(0,1,1), seasonal = c(3,1,1), method = "ML")
Box.test(model1$residuals, lag = 10, fitdf = 0)
acf(model1$residuals, na.action=na.pass, plot = T)

 for (P in seq(1,3)){
    for (Q in seq(1,3)){
      cat(P,Q,sep = " ")
      model <- arima(ts.12.1, order = c(0,1,1), seasonal = c(P,1,Q), method = "ML")
      print(paste(model$aic))
      cat('\n')
  }
 }

model1 <- arima(ts.12.1, order = c(0,1,2), seasonal = c(1,1,2), method = "ML")
hist(model1$residuals)
qqline(model1$residuals)
acf(model1$residuals, na.action = na.pass, plot = T)
Box.test(model1$residuals, lag=13, fitdf=4)
