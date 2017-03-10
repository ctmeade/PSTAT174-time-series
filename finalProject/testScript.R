# Import Data
ts <- read.table("c02.txt", skip = 3, header = F, sep = "")

# Remove exraneous featurs
ts <- ts$V4

# Encode missing values
ts <- ifelse(ts == -99.99, NA, ts)
orig <- ts
next10 <- orig[697:706]
# Convert vector to time series object
ts <- ts(ts, start=c(1958,3), frequency=12)
ts <- ts[1:696]
plot.ts(ts)
var(ts, na.rm=T)

ts.log <- log(ts)
plot.ts(ts.log)
var(ts.log, na.rm=T)
#Difference
ts.log.12 <- diff(ts.log, lag = 12)
plot.ts(ts.log.12)
var(ts.12, na.rm=T)

# Good differencing order, variance decreases
ts.log.12.1 <- diff(ts.log.12, lag = 1)
plot.ts(ts.log.12.1)
var(ts.log.12.1, na.rm = T)
avg <- mean(ts.log.12.1, na.rm = T)
abline(h=avg)
abline(h=0, col = "red")

#ts.12.1 stationary since mean variance time independent, both 0

### ACF

acf <- acf(ts.log.12.1, type = "correlation", plot = F, na.action = na.pass, lag.max=12*5)

## Transform the lags from years to months
acf$lag <- acf$lag * 12

## Plot the acf 
plot(acf, xlab="Lag (months)")

### PACF
pacf <- acf(ts.log.12.1, type = "partial", plot = F, na.action = na.pass, lag.max= 12*5)
pacf$lag <- pacf$lag * 12
plot(pacf, xlab = "Lag (months)")

## Seasonal: ACF cuts off after lag 1 (12), PACF tailing off


model1 <- Arima(ts.log, order = c(0,1,2), seasonal = list(order = c(0,1,1), period = 12))
hist(model1$residuals, breaks = 25, xlim=c(-0.005, 0.005))
Box.test(model1$residuals)
pred <- predict(model1, n.ahead = 10)
pred.orig <- exp(pred$pred)
pred.se <- exp(pred.orig*pred$se)

plot.ts(orig, xlim = c(680,length(orig)+10), ylim = c(380,420))
points((length(orig)+1):(length(orig)+10),pred.orig, col="red")
points((length(orig)+1):(length(orig)+10),next10, pch = 3)
lines((length(orig)+1):(length(orig)+10),pred.orig+1.96*pred.se,lty=2, col="blue") 
lines((length(orig)+1):(length(orig)+10),pred.orig-1.96*pred.se,lty=2, col="blue")

model2 <- Arima(ts.log, order = c(4,1,0), seasonal = list(order = c(2,1,0), period = 12))
hist(model2$residuals, breaks = 25, xlim=c(-0.005, 0.005))
Box.test(model2$residuals)

pred2 <- predict(model2, n.ahead = 10)
pred.2orig <- exp(pred2$pred)
pred.se <- exp(pred$se)

plot.ts(orig, xlim = c(680,length(orig)+10), ylim = c(380,420))
points((length(orig)+1):(length(orig)+10),pred.2orig, col="red")
points((length(orig)+1):(length(orig)+10),next10, pch = 3)
lines((length(orig)+1):(length(orig)+10),pred.2orig+1.96*pred.se,lty=2, col="blue") 
lines((length(orig)+1):(length(orig)+10),pred.2orig-1.96*pred.se,lty=2, col="blue")

