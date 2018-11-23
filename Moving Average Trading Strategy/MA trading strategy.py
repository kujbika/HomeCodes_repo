# -*- coding: utf-8 -*-
"""
I investigate here how MA trading strategies work when an arbitrary number of
American assets are considered. Comparison is made between MA and the buy and hold strategy.
Object Orientated Programming was used.
I optimized the lag for the strategy as well.

Made by Marcell Kujbus
email: marcellkujbus@gmail.com
"""
from pandas_datareader import data
import matplotlib.pyplot as plt
import pandas as pd
import matplotlib.patches as patches
import numpy as np

def DataDownloader( ticker, start, end):
    '''
    This function downloads financial data for a given company (ticker), in a 
    daily basis from start to end.
    The output is a dictionary with the corresponding logreturns and the very first
    price observed.
    '''
    stock_prices = data.get_data_yahoo( ticker, start, end)
    stock_prices_null = stock_prices['Adj Close'][0]
    close = np.log(stock_prices['Adj Close']).diff().dropna()
    dict = {"logreturns" : close, "starting" : stock_prices_null}
    return dict

def SignalCreator(ticker, start, end, short, long):
    '''
    Based on the Moving Average strategy (short - long) this functions gives us the signal
    whether we should deal a long or a short position.
    1 means buying
    0 means selling
    Be warned that short << long
    '''
    price_dict = DataDownloader( ticker, start, end)
    close = price_dict['starting'] * np.exp( np.cumsum( price_dict["logreturns"] ) )
    diff = close.rolling(window = short).mean() - close.rolling(window = long).mean()
    res = diff.apply(np.sign)
    return (res + 1)/2
    
def PlotPy(ticker, start, end, s, l):
    '''
    This is a function made for plotting purposes.
    The output is a 3-plot: one with logreturns,
    one with the price evolution and one is the MA difference evolution.
    '''
    price_dict = DataDownloader( ticker, start, end)
    close = price_dict['starting'] * np.exp( np.cumsum( price_dict["logreturns"] ) )

    fig, (ax1, ax2, ax3) = plt.subplots(3, 1, figsize=(16,9))
    ax1.plot(price_dict["logreturns"].index, price_dict["logreturns"], label=f"{ticker} logreturns")
    ax1.legend(loc=2)
    ax1.grid(axis = "both", linestyle = '--')

    short_rolling = close.rolling(window = s).mean()
    long_rolling = close.rolling(window = l).mean()
    ax2.plot(close.index, close, label = f"{ticker} stock price") 
    ax2.plot(short_rolling.index, short_rolling, label = f"{s} days rolling MA")
    ax2.plot(long_rolling.index, long_rolling, label = f"{l} days rolling MA")
    ax2.legend(loc = 2)
    ax2.grid(axis = "both", linestyle = '--')
    
    ax3.plot(close.index, short_rolling - long_rolling, label = f"{s} - {l} MA")
    ax3.grid(axis = "both", linestyle = '--')
    ax3.axhline(y = 0, linestyle = "-", color = "red")
    ax3.set_xlabel('Date', fontsize=16, fontweight = "bold")
    ax3.legend(loc = 2)

class Portfolio():
    '''
    This class was made to model our hypothetical investors behaviour. 
    The above defined strategy was implemented in this Portfolio class.
    '''
    def __init__(self, *args):
        k = (len(args) - 1) // 3
        self.signal = [args[i] for i in range(0,k)]
        self.stock = [args[i] for i in range(k, 2* k)]
        self.price = [args[i] for i in range(2 * k, 3 * k)]
        self.number_of_long = np.sum(self.signal)
        self.cash = args[-1]
    def Value(self):
        value = 0
        for i in range(0, len(self.stock)):
            value += self.stock[i] * self.price[i]
        return value
    def __str__(self):
        return "In this round the Investor will behave according to the signals\n" \
        f"{self.signal}. Current wealth level is {self.cash + self.Value()}."
    
    def Business(self):
        '''
        The output of the Business method is a list consisting of the cash level, the 
        stock value, and the wealth level.
        '''
        w0 = self.cash
        for i in range(0,len(self.stock)):
            if self.signal[i] == 0 and self.stock[i]:
               w0 += self.stock[i] * self.price[i]
               self.stock[i] = 0
            elif self.signal[i] ==0 and self.stock[i] ==0:
                pass
        if self.number_of_long:
            amount = w0 / self.number_of_long
            for i in range(0,len(self.stock)):
                if self.signal[i]:
                    self.stock[i] += amount / self.price[i]
            self.cash = 0
        else:
            if self.cash == 0:
                self.cash += w0
            else:
                pass
        return [self.cash, self.Value(), self.cash + self.Value()]
      
def DatabaseGenerator(*args, **kwargs): 
    '''
    This function creates a datatable about the trading strategy.
    Inputs: *args as arbitrary tickers g.e 'AAPL', 'MSFT', 'AMZN',....
            **kwargs as some additional info g. e start = start, end = end, short = s, long = l
    The output is a dataframe consisting of several time series.
    '''
    df = pd.DataFrame()
    for i in args:
        df[f"{i} signal"] = SignalCreator(i, kwargs['start'], kwargs['end'], kwargs['short'], kwargs['long'])
        df[f"{i} shares"] = 0
        df[f"{i} share price"] = ( DataDownloader(i, kwargs['start'], kwargs['end'])['starting']
                            * np.exp( np.cumsum( DataDownloader(i, kwargs['start'], kwargs['end'])["logreturns"] ) ))
    df = df.dropna()
    df['Cash'] = 0
    df['Share value'] = 0
    df['Wealth'] = 0
    df['Buy and hold Wealth'] = 0
    for i in range(0,len(args)):
        df.iloc[0, 3*i+1] = 1
    return df

def StrategyTrajectory(dataframe):
    '''
    This function takes a DatabaseGenerator output as an input, and it trades the
    strategy on it. 
    '''
    for i in range(0, len(dataframe)):
        k = (len(dataframe.columns) - 4 ) //3
        arg = []
        for j in range(0, len(dataframe.columns) - 6, 3):
            arg.append(dataframe.iloc[i,j])
        for j in range(1,len(dataframe.columns) - 5, 3):
            arg.append(dataframe.iloc[i, j])
        for j in range(2,len(dataframe.columns) - 4, 3):
            arg.append(dataframe.iloc[i, j])
        arg.append(dataframe.iloc[i, -4])
        portf = Portfolio( *arg )
        result = portf.Business()
        dataframe.iloc[i , -4] = result[0]
        dataframe.iloc[i , -3] = result[1]
        dataframe.iloc[i, -2]  = result[2]
        if i < len(dataframe) - 1:
            dataframe.iloc[i+1, -4 ] = result[0]
            for j in range(0,k):
                dataframe.iloc[i + 1, 3*j + 1] = portf.stock[j]
        dataframe.iloc[i, -1] = np.sum([arg[i] for i in range(len(arg) - 1 - k, len(arg)-1)])
    return dataframe


def Evaluate(tickers, short, long, start, end, no_tickers, re_run):
    '''
    The Evaluate function randomly chooses no_tickers tickers from a given list
    Then it evaluates the strategy on it
    After each round, it starts all over again with the random choosing method.
    The output is an expected yearly premium.
    '''
    r_ma = 0
    r_bh = 0
    for i in range(0,re_run):
        tick = np.random.choice(tickers, no_tickers, replace = False)
        df = DatabaseGenerator(*tick, start = start, end = end, short = short, long = long)
        result = StrategyTrajectory(df)
        r_ma += np.log((result.iloc[-1, -2] / result.iloc[0, -2] )) / len(df)
        r_bh += np.log((result.iloc[-1, -1] / result.iloc[0, -1] )) / len(df)
    return (r_ma -r_bh) * 250/ re_run

def CreateReturns(shortlong_list, tickers, start, end, no_tickers, re_run):
    '''
    This creates average returns for different short-long pairs.
    I use this function to optimize the MA strategy. It runs slowly.
    '''
    for i in shortlong_list:
        short = i[0]
        long = i[1]
        yield Evaluate(tickers = tickers, short = short, long = long, start = start,
                   end = end, no_tickers = no_tickers, re_run = re_run)


#################let's see some examples
start = "2014-10-01"
end = "2018-11-18"

PlotPy('AAPL', start, end, 10, 50)
PlotPy('MS', "2017-10-01", end, 40, 140)

p_example = Portfolio(1,0,1,0,1,0,0,1,
                      10,20, 0, 4, 5, 10, 0, 32,
                      100, 56, 231, 10, 80, 90, 42, 21, 0)
p_example.stock
p_example.price
p_example.number_of_long
p_example.signals()
p_example.stock
print(p_example)
p_example.Value()
p_example.Business()

df = DatabaseGenerator('AAPL','MSFT', 'MS', 'AMZN', 'BA', 'GS', 'BLK', start = start, end = end, short = 10, long = 50)
example = StrategyTrajectory(df)
     
fig, ax = plt.subplots(1,1, figsize = (16,12))
ax.plot(example['Wealth'].index, example['Wealth'], label = 'MA strategy')
ax.plot(example['Buy and hold Wealth'].index, example['Buy and hold Wealth'], label = 'Buy and hold strategy')
ax.legend(prop = {'size' : 20})
ax.grid(axis = "both", linestyle = '--')

#################let's jump into the research
#Optimal strategy grid search
tickers = ['AAPL','MSFT', 'MS', 'AMZN', 'BA', 'GS', 'BLK', 'ESRX','GE', 'BRK-B', 
           'JNJ', 'ALXN', 'GOOGL', 'AMT']
sl = [(s,l) for s in range(10, 51, 10) for l in range(50, 251, 50)]

returns = CreateReturns(shortlong_list = sl, tickers = tickers, start = start, end = end,
                        no_tickers = 5, re_run = 5)
result = list(returns)

#plot the result return in a scatter plot to choose the optimal short-long pair
res_data = pd.DataFrame(sl)
res_data['return'] = result
idx = np.argmax(res_data['return'])
values = res_data.iloc[idx,:]
fig, ax = plt.subplots(1,1,figsize = (14,12))
x = res_data.iloc[:,0]
y = res_data.iloc[:,1]
colors = res_data['return']
area = res_data['return'] * 10000
plt.scatter(x, y, s = area, alpha = .7, c = colors, cmap = 'plasma')
plt.xlabel('SMA lag', fontsize=16, fontweight = "bold")
plt.ylabel('LMA lag', fontsize=16, fontweight = "bold")
rect = patches.Rectangle((values[0] - 5,values[1] - 10), 10, 20, linewidth = 3, edgecolor = 'r', facecolor = 'none')
ax.add_patch(rect)
cbar = plt.colorbar()
ax.spines['right'].set_visible(False)
ax.spines['top'].set_visible(False)
cbar.ax.set_title('Expected yearly logreturn')
ax.grid(axis = "both", linestyle = '--')
plt.show()

res_data