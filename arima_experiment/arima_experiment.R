#####In this small experiment, we shall analyze a time series' stationarity, and
#### we plan to fit the best ARIMA(p,d,q) to it.
###At the end a small convergence investigation happens:
##Do the estimated p,q-s for ARMA get close to the real p,q ones?
# Used packages: forecast, lattice, urca, ggplot, ggextra, ggalt


#generate a dataset that follows a (2,1,3) ARIMA Process
RawData <-  ts ( cumsum( arima.sim( n = 1010, 
                                            list( ar = c(0.7, -0.5),
                                                  ma = c(-0.2, 0.2, 0.5) ) ) ) )


#plotting it, and check the acf 
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

#forecast can do it with a one-line for finding the number of integration of the process
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
#the model is accurate diganosticaly: no autocorrelation for the residuals:
#from the Ljung-Box statistics - the last p value matters mostly



#qqplot is the best graphical test for checking normality
qqnorm( resid( fit ) ) 
qqline( resid( fit ) )

#forecasting with ARMA.
forecast::forecast(fit)
plot( forecast::forecast( fit), xlim = c( 1000, 1025), ylim = c( -60, -30 ) )


#hold-out sample validation
fitTrain <- forecast::Arima( RawData[ 1:1000 ], order = c( predgrid[i,1], d, predgrid[i,2]) ) #a kozepso argumentumot tudjuk
forecast::accuracy(forecast::forecast(fitTrain), RawData[ 1001:1010])


#a forecast one-line for everything we covered:
res = forecast::auto.arima( RawData, trace = T)
#it might not find the (2,1,3), but almost.


#################EMPIRICAL JOINT DISTRIBUTION OF p,q-S

#SimArima function takes 3 inputs: the length of the generated ARIMA(2,1,3) process
#and the boolean values whether we want to use implementation, or the forecast package.
#The output of the function is the p,q values.
#Notice how slower the implementation is.
SimArima <- function(len = 1000, is_implementation = 0, is_forecast = 0){
  RawData <-  ts ( cumsum( arima.sim( n = len, 
                                      list( ar = c(0.7, -0.5),
                                            ma = c(-0.2, 0.2, 0.5) ) ) ) )
  if (is_forecast == 1) return (forecast::auto.arima( RawData, trace = F)$arma[ c( 1, 2 ) ] )
  if (is_implementation == 1){
    predgrid <- expand.grid( p = 0:5, q = 0:5 )
    predgrid$AIC <- apply(predgrid, 1, function( x ) AIC(forecast::Arima( RawData, 
                                                                          order = c(x[1], 
                                                                          forecast::ndiffs(RawData), x[2] ) ) ) ) 
    return ( predgrid[ which.min(predgrid$AIC), c(1,2)] )
    
  }
}


#ploty function takes the same inputs. The output of it is the desired plot.
ploty <- function(len = 1000, is_implementation = 0, is_forecast = 0){
  dist_table <- data.table::data.table("#" = 1 : 1500, "p" = 0, "q" = 0)
  result <- replicate(1500, SimArima(len, is_implementation, is_forecast ) )
  dist_table$p = result[1,]
  dist_table$q = result[2,]
  p <- ggplot(dist_table, aes(x=p, y=q)) + 
    labs(title = "The empirical joint distribution of ARMA parameters", subtitle = paste( len, " long times teries"))+
    geom_count(aes(size = ..prop.., group = 1)) + scale_size_area( max_size = 12) +
    theme(legend.position = "left", legend.title = element_text(colour="black", size=10, 
                                                                face="bold"),legend.background = element_rect(
                                                                  fill="grey86",
                                                                  size=0.5, linetype="solid", 
                                                                  colour ="black"))
  q <- ggExtra::ggMarginal(p, type="histogram", margins = "both")
  return (q)
}

t1 <- Sys.time()
p1 <- ploty(len = 500, is_forecast = 1)
p2 <- ploty(len = 1000, is_forecast = 1)
p3 <- ploty(len = 2000, is_forecast = 1)
p4 <- ploty(len = 5000, is_forecast = 1)
t2 <- Sys.time()
#remember: It is an ARIMA(2,1,3) process originally!


#Conclusion: The longer our time series is, the more accurate
#estimate for ARIMA(p,d,q) parameters.
#Even at len = 5000, the probability of getting the right p,q
#is approx. 30% (according to p4 plot).



#######THE END############


