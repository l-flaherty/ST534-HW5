#################Written By Liam Flaherty For ST534 HW 5###############################
#####1. Load Required Packages#####
library(tidyverse)
library(stats)
library(tseries)
library(forecast)
path="Academics/ST534 Time Series/Homework/Homework 5/"

data4=read.csv(paste0(path, "data4.csv"))
summary(data4)
str(data4)
ts_data4=ts(data4$data)                #Convert from DF to Time Series Object#
n_data4=nrow(data4)
auto.arima(ts_data4)                   #to give us an idea what to search for#


beer=read.csv(paste0(path, "beer.csv")) |>
  mutate(yyyyq=paste0(year, "-", quarter))|>
  select(yyyyq, data)
summary(beer)
str(beer)
ts_beer=ts(beer$data)                #Convert from DF to Time Series Object#
n_beer=nrow(beer)
auto.arima(ts_beer)                  #to give us an idea what to search for#
s=4                                  #The seasonality#



###################################################################################################
###################################################################################################
#####2. Data4#####
###2a. Plot Data (Ljung-Box says fit model)###
par(mfrow=c(1,1))
plot(data4, 
     type="l", 
     lwd="2",
     col="blue",
     main="Data Set 4",
     xlab="Time",
     ylab="Value")

whitenoise6=Box.test(ts_data4,                #Do we need to fit model?#
                     lag=6, 
                     type="Ljung-Box")   
whitenoise6                                   #p small \implies yes#

whitenoise12=Box.test(ts_data4, 
                      lag=12, 
                      type="Ljung-Box")
whitenoise12



###2b. Augmented Dickey-Fuller (Unit Root Test)###
adf_result_data4=adf.test(ts_data4)           #Null is that there is a root#
adf_result_data4                              #since p is 0.2, don't reject null; assume non-stationary#

diff_data4=diff(ts_data4, differences = 1)

par(mfrow=c(1,1))
plot(diff_data4,
     main="Differenced Data",
     xlab="Time",
     ylab="Value",
     col="black",
     lwd=2)

par(mfrow=c(1,2))
data4diff_acf=acf(diff_data4, lag.max=40, 
                  main=paste0("Differenced Time Series Data Of Length ", n_data4-1, "\n", "Estimated ACF"), 
                  ci.col="blue", 
                  col="red", 
                  lwd=4)
data4diff_acf                            #ACF refuses to die#

data4diff_pacf=pacf(diff_data4, lag.max=40, 
                    main=paste0("Differenced Time Series Data Of Length ", n_data4-1, "\n", "Estimated PACF"), 
                    ci.col="blue", 
                    col="red", 
                    lwd=4)
data4diff_pacf                                  #PACF also refuses to die#

whitenoise6=Box.test(diff_data4,                #Do we need to fit model?#
                     lag=6, 
                     type="Ljung-Box")  
whitenoise6                                    #large p \implies no#

whitenoise12=Box.test(diff_data4,                #Do we need to fit model?#
                     lag=12, 
                     type="Ljung-Box")  
whitenoise12                                    #large p \implies no#



###2c. Plot ACF and PACF###
par(mfrow=c(1,2))
data4_acf=acf(ts_data4, lag.max=40, 
              main=paste0("Time Series Data Of Length ", n_data4, "\n", "Estimated ACF"), 
              ci.col="blue", 
              col="red", 
              lwd=4)
data4_acf                            #ACF dies out slowly#

data4_pacf=pacf(ts_data4, lag.max=40, 
                main=paste0("Time Series Data Of Length ", n_data4, "\n", "Estimated PACF"), 
                ci.col="blue", 
                col="red", 
                lwd=4)
data4_pacf                          #PACF cuts off at lag 2#




###2d. Try A Few Different Models###
ARIMA_model=vector()
aic=vector()              
bic=vector()
LBtest=vector()

for (p in 1:4) {
  for (d in 1:2) {
    for (q in 1:4) {
            model=arima(ts_data4, 
                        order=c(p-1,d-1,q-1))
            resid=residuals(model)
            
            ARIMA_model[4*2*(p-1)+4*(d-1)+q]=paste0("ARIMA(", p-1, ",", d-1, ",", q-1, ")")
            aic[4*2*(p-1)+4*(d-1)+q]=round(AIC(model),2)
            bic[4*2*(p-1)+4*(d-1)+q]=round(BIC(model),2)
            LBtest[4*2*(p-1)+4*(d-1)+q]=round(Box.test(resid, lag=21, type="Ljung-Box")$p.value,2)
    }
  }
}

df=data.frame(ARIMA_model, aic, bic, LBtest)
df=df[order(df$aic),]
df





###################################################################################################
###################################################################################################
#####3. Beer#####
###3a. Plot Data (Ljung-Box says fit model)###
par(mfrow=c(1,1))
plot(y=beer$data, 
     x=1:length(beer$yyyyq),
     type="l", 
     lwd=2,
     col="blue",
     main="Quarterly Beer Production",
     xlab="Time",
     ylab="Value",
     xaxt="n")
axis(1, 
     at=seq(1, length(beer$yyyyq), 4),
     labels=beer$yyyyq[seq(1, length(beer$yyyyq), 4)],
     las=1)

whitenoise6_beer=Box.test(ts_beer,                #Do we need to fit model?#
                     lag=6, 
                     type="Ljung-Box")   
whitenoise6_beer                                   #p small \implies yes#

whitenoise12_beer=Box.test(ts_beer, 
                      lag=12, 
                      type="Ljung-Box")
whitenoise12_beer





###3b. Augmented Dickey-Fuller (Unit Root Test) For Seasonal Part###
ts_beer_seasonal=ts_beer[seq(2, length(ts_beer), by=4)]   #Only look at every 4th#
sadf_result=suppressWarnings(adf.test(ts_beer_seasonal))
sadf_result                                              

#technically stationary from test, but visually, looks trending#
ts_beer_sdiff=diff(ts_beer, lag = 4)
plot(y=ts_beer_sdiff, 
     x=1:28,
     type="l", 
     lwd=2,
     col="blue",
     main="Quarterly Beer Production After Seasonal Difference",
     xlab="Time",
     ylab="Value",
     xaxt="n")





###3c. ACF and PACF###
par(mfrow=c(1,2))
beer_acf=acf(ts_beer, lag.max=40, 
             main=paste0("Time Series Data Of Length ", n_beer, "\n", "Estimated ACF"), 
             ci.col="blue", 
             col="red", 
             lwd=4)
beer_acf                            #ACF dies out slowly at seasonal lags#

beer_pacf=pacf(ts_beer, lag.max=40, 
               main=paste0("Time Series Data Of Length ", n_beer, "\n", "Estimated PACF"), 
               ci.col="blue", 
               col="red", 
               lwd=4)
beer_pacf                          #PACF cuts off at seasonal lag 1#


#try with the seasonal difference#
par(mfrow=c(1,2))
beer_sacf=acf(ts_beer_sdiff, lag.max=40, 
             main=paste0("ACF After Seasonal Difference"), 
             ci.col="blue", 
             col="red", 
             lwd=4)
beer_sacf                            #ACF dies out slowly#

beer_spacf=pacf(ts_beer, lag.max=40, 
               main=paste0("PACF After Seasonal Difference"), 
               ci.col="blue", 
               col="red", 
               lwd=4)
beer_spacf                          #PACF cuts off at lag 2#





###3d. Regular ARIMA Components###
adf_result_beer=adf.test(ts_beer)
adf_result_beer                           #large p implies take difference#
ts_beer1=diff(ts_beer, lag=1)


###3e. Look at ACF PACF after lag 1 difference###
beer_acf1=acf(ts_beer1, lag.max=40, 
             main=paste0("ACF After Regular Difference"), 
             ci.col="blue", 
             col="red", 
             lwd=4)
beer_acf1                            #ACF dies out slowly at seasonal lags#

beer_pacf1=pacf(ts_beer, lag.max=40, 
               main=paste0("PACF After Regular Difference"), 
               ci.col="blue", 
               col="red", 
               lwd=4)
beer_pacf1                          #PACF cuts off at seasonal lag 1#






###3f. Try A Few Different Models###
ARIMAs_model=vector()
aic=vector()              
bic=vector()
LBtest=vector()
s=4                    #clear from data#

for (p in 1:4) {
  for (d in 1:2) {
    for (q in 1:4) {
      for (P in 1:4) {
        for (D in 1:2) {
          for (Q in 1:4) {
            mycount=(((((p-1)*2 + (d-1))*4 + (q-1))*4 + (P-1))*2 + (D-1))*4 + Q
            mymodel=paste0("ARIMA(", p-1, ",", d-1, ",", q-1, ")(", P-1, ",", D-1, ",", Q-1, ")",s)
            
            tryCatch({
              model=arima(ts_beer, 
                          order=c(p-1,d-1,q-1),
                          seasonal=list(order=c(P-1,D-1,Q-1), period=s))
              resid=residuals(model)
              
              ARIMAs_model[mycount]=mymodel
              aic[mycount]=round(AIC(model),2)
              bic[mycount]=round(BIC(model),2)
              LBtest[mycount]=round(Box.test(resid, lag=21, type="Ljung-Box")$p.value,2)
            }, error=function(e) {
              ARIMAs_model[mycount]=mymodel
              aic[mycount]=999
              bic[mycount]=999
              LBtest[mycount]=999
            })
          }
        }
      }
    }
  }
}

df=data.frame(ARIMAs_model, aic, bic, LBtest)
df=df[order(df$aic),]
df



############Notes########################
#########################################
