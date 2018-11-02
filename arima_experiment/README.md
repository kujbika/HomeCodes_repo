# Can we estimate the ARIMA(p,d,q) process efficiently?
*__Marcell Kujbus, Financial Mathematician/Empirical Finance Researcher__*

In this small experiment, I generate an ARIMA(2,1,3) process, and after that, I start to investigate it. How to get the number of integration?
What are the appropriate statistical tests for stationarity? 
A grid search is made to get back the ARMA(p,d) values. How confident can we be after minimising the AIC? 
I did a convergence test as well: After simulating 1500 trajectories of ARIMA(2,1,3) processes (with lenghts 500, 1000, 2000, 5000 respectively), in what proportion
did we get back the right 2,1,3 indicators? 
The experiment shows, that the arima estimation is unbiased asymptotically, but the convergence is really-really slow.

