# Option pricing methods
*__Marcell Kujbus, Financial Mathematician/Empirical Finance Researcher__*
This folder consists of two option pricing methods: the binomial one is just theoretical, but the GARCH based is a "real world" application. The latter was a school team project: I worked on that with 3 friends of mine.
Some information about the GARCH-option pricing: After downloading an arbitrary time series based on a ticker, I fit an ARIMA(1,1,1)-GARCH(1,1) model to it. Based on the fit, 1000 trajectories are getting simulated, and the option gets its price according to the pricing formula.
According to the plots of the option prices versus the strike price, the empirical method is perfectly accurate.
What is the difference between the the price of the estimation and the Black-Scholes price?
What is the reason for this?
