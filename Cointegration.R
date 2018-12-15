library( urca )
library( egcm )
### the problem of a fake regression
ps <- replicate( 10000, {
  y <- rnorm( 100 )
  x <- rnorm( 100 )
  summary( lm( y ~ x ) )$coefficients[ 2, 4 ]
})

hist( ps ) #it is supposed to be a linear density function: uniform distr
mean( ps<0.05 ) #this is the expectation

ps <- replicate( 10000, {
  x <- cumsum( rnorm( 250 ) )
  y <- cumsum( rnorm( 250 ) )
  summary( lm( y ~ x ) )$coefficients[ 2, 4 ]
})

hist( ps )
mean( ps<0.05 )
#what happened here? It is supposed to be uniformly dsitributed!!
#key: COINTEGRATION
#-------------
#example of cointegrated time serieses:
#3 months risk free return and 6 months risk free return
TB3MS <- quantmod::getSymbols( "TB3MS", src = "FRED", auto.assign = FALSE )
TB6MS <- quantmod::getSymbols( "TB6MS", src = "FRED", auto.assign = FALSE )

TB3MS <- window( TB3MS, start = "1960-01-01" )
TB6MS <- window( TB6MS, start = "1960-01-01" )

plot( TB3MS )
lines( TB6MS, col = "red" )

spread <- TB6MS - TB3MS

plot( spread ) # the spread is stationary because of arbitrage reasons

#augmented-dickey-fuller stationariry test
summary( ur.df( TB3MS ) )
summary( ur.df( TB6MS ) )
summary( ur.df( spread ) ) #this is really stationer

#Engle-Granger Cointergration model
summary( egcm( TB6MS, TB3MS ) ) 

eps <- rnorm( 1000 )
x <- cumsum( rnorm( 1000 ) )
y <- eps + 1.5*x

summary( egcm( x, y ) )

### Cointegration (multiple times series)

u <- cumsum( rnorm( 10000 ) )

x <- 0.50*u + rnorm( 10000 )
y <- 0.25*u + rnorm( 10000 )
z <- 0.10*u + rnorm( 10000 )

summary( ca.jo( data.frame( x, y, z ) ) )
ca.jo( data.frame( x, y, z ) )@V
c( 0.5, 0.25, 0.1 )%*%ca.jo( data.frame( x, y, z ) )@V

#original example of Johansen
data( denmark )
summary( ca.jo( denmark[ , c( "LRM", "LRY", "IBO", "IDE" ) ], season = 4 ) )

