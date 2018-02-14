
######Header File
library(xts)
library(forecast)
library(fpp)
library(tseries)
library(tsoutliers)

##############Reading File####################
hotel_stay <- read.csv("tourist_stay.csv", sep = ",", row.names = 1, header = TRUE)

########convertin into time series
hotel_stay_ts <- ts(as.vector(t(hotel_stay[1:24,])), frequency = 12, start = c(1992,1), 
                    end = c(2015,12))


############Visualisation##############
summary(hotel_stay_ts)
plot(hotel_stay_ts)
abline(reg=lm(hotel_stay_ts ~ time(hotel_stay_ts)))

##cycle across years
cycle(hotel_stay_ts)

##aggregate the cycles to visualise year on year trend
plot(aggregate(hotel_stay_ts,FUN=mean))

##Box plot across months to see the seasonal effect
boxplot(hotel_stay_ts~cycle(hotel_stay_ts), main = "Seasonal Effect")


############Stationary test##############
adf.test(hotel_stay_ts, alternative = "stationary", k=0)

adf.test(log(hotel_stay_ts), alternative = "stationary", k=0)

adf.test(diff(log(hotel_stay_ts)), alternative = "stationary", k=0)


####################Data Preprocessing################

#########Normalize and Standardize Data##########
normalised_ts = (hotel_stay_ts - min(hotel_stay_ts)) / (max(hotel_stay_ts)- min(hotel_stay_ts))

standardised_ts = (normalised_ts - mean(normalised_ts)) / (sd(normalised_ts))
plot(standardised_ts, main = "Normalised and Standardised Series")

#######function to conver the predicted values########
back_conversion = function(x)
{
  nor = (x * sd(normalised_ts)) + mean(normalised_ts)
  ori = (nor * (max(hotel_stay_ts)- min(hotel_stay_ts))) + min(hotel_stay_ts)
  return(ori)
}


#########find outliers#############
hotel_stay_outlier<-tso0(hotel_stay_ts,types=c("AO","LS","TC"), tsmethod = "auto.arima")

###########Decomposition into Trend and Seasonality###############
####Trend cycle decomposition and plot

#####Monthly trend
fit <- stl(hotel_stay_ts, s.window=5)
plot(hotel_stay_ts, col="gray", main="Monthly Trend Cycle", ylab="", xlab="")
lines(fit$time.series[,2],col="red",ylab="Trend")

plot(hotel_stay_ts, main = "Trend Monthly- 2 * 12 MA")
lines(ma(hotel_stay_ts,12), col = "green")

####Quaterly Trend
plot(hotel_stay_ts, main = "Quarterly Trend")
lines(ma(hotel_stay_ts,3), col = "blue")

######Daily Trend
plot(hotel_stay_ts, main = "Trend Cycle Daily")
lines(ma(hotel_stay_ts,7), col = "Blue")
###not feasible

#####Trend-Cycle, Seasonal and remainder decomposition using STL########
fit2 <- stl(hotel_stay_ts, t.window=15, s.window="periodic", robust=TRUE)
plot(fit2)

fit3 <- stl(hotel_stay_ts, s.window= 5, robust=TRUE)
plot(fit3)


###################Forecasting################
#######Forecasting with decomposition########
adjusted_stl = seasadj(fit2)
forecast_stl_decomposed = forecast(fit2, method="naive", h = 24, level = 0.95)
plot(forecast_stl_decomposed)

#############Holt Winters##########
model_holtwinters = HoltWinters(hotel_stay_ts)
predict_holtwinters = predict(model_holtwinters, n.ahead = 2*12)

ts.plot(hotel_stay_ts, predict_holtwinters, log = "y", lty = c(1,3))

##########with normalised and standardised data
model_holtwinters_std = HoltWinters(standardised_ts)
predict_holtwinters_std = predict(model_holtwinters_std, n.ahead = 2*12)
####gives error while modelling

################ARIMA##############
acf(hotel_stay_ts)
pacf(hotel_stay_ts)

##########using original data############
model_arima = auto.arima(log(hotel_stay_ts))
predict_arima  = predict(model_arima, n.ahead = 2*12)

ts.plot(hotel_stay_ts, 2.718^predict_arima$pred, log = "y", lty = c(1,3))

##########using standarsised data#########
model_arima_std = auto.arima(standardised_ts)
predict_arima_std = predict(model_arima_std, n.ahead = 2*12)

forecasted_arima_std = lapply(as.list(predict_arima_std$pred), FUN = back_conversion)
forecasted_arima_std = forecasted_arima_std[[1]]
ts.plot(hotel_stay_ts,forecasted_arima_std, log = "y",lty = c(1,3))

