# Option pricing methods
*__Marcell Kujbus, Financial Mathematician/Empirical Finance Researcher__*
This folder consists of two option pricing methods: the binomial one is just theoretical, but the GARCH based is a "real world" application.
Some information about the GARCH-option pricing: 
	1: After downloading an arbitrary time series based on a ticker,
	2: I fit an ARIMA(1,1,1)-GARCH(1,1) model to it.
	3: Based on the fit, 1000 trajectories are getting simulated,
	4: And the option gets its price according to the pricing formula
According to the plots of the option prices versus the strike price, the empirical method is perfectly accurate.
What is the difference between the the price of the estimation and the Black-Scholes price?
What is the reason for this?