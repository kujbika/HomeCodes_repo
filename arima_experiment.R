#####In this small experiment, we analyze a time series' stationarity, and
## we try to fit the best ARIMA(p,d,q) to it
# Used packages: forecast, lattice, urca


#generate a dataset that follows a (2,1,3) ARIMA Process
set.seed(12303024)
RawData <-  ts ( cumsum( arima.sim( n = 1010, 
                                            list( ar = c(0.7, -0.5),
                                                  ma = c(-0.2, 0.2, 0.5) ) ) ) ) 

forecast::tsdisplay( RawData )
tseries::adf.test( RawData ) 
tseries::kpss.test( RawData )
urca::summary( urca::ur.df( RawData ) )
#from the graphical test (acf), and the appropriate unit root tests
#we conclude that the ts is almost surely not stationary
# -> starting to differentiate


forecast::Acf( diff( RawData ) )
forecast::tsdisplay( diff(RawData ) ) 
tseries::kpss.test( diff( RawData ) ) 
#from the kpss test we derive that d = 1 is an appropriate choice

#forecast can do it with a one-line for finding the number of integration
d <- forecast::ndiffs( RawData ) 

#finding the value of p,q with grid search
predgrid <- expand.grid( p = 0:5, q = 0:5 )
predgrid$AIC <- apply(predgrid, 1, function( x ) AIC(forecast::Arima( RawData, order = c(x[1], d, x[2] ) ) ) ) 
lattice::wireframe( AIC~ p + q, data = predgrid)
lattice::levelplot( AIC~ p + q, data = predgrid)
i <- which.min(predgrid$AIC)

#lets fit an arima
fit <- arima( RawData, order = c( predgrid[i,1], d, predgrid[i,2]) ) 
summary(fit)
tsdiag( fit ) 
#the model is accurate diganosticaly: no autocorrelation for the residuals (from the Ljung-Box statistics - the last p value counts)

qqnorm( resid( fit ) ) 
qqline( resid( fit ) )
#this is the best graphical test for checking normality

forecast::forecast(fit)
plot( forecast::forecast( fit), xlim = c( 1000, 1025), ylim = c( -60, -30 ) )  #legyezo abra


#hold-out sample validation
fitTrain <- forecast::Arima( RawData[ 1:1000 ], order = c( predgrid[i,1], d, predgrid[i,2]) ) #a kozepso argumentumot tudjuk
forecast::accuracy(forecast::forecast(fitTrain), RawData[ 1001:1010])


#####a forecast one-line for everything we covered:
forecast::auto.arima( RawData, trace = T)

#it might not find the (2,1,3), but almost.
