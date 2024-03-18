# Impor libraries 
from openbb_terminal.sdk import openbb
import pandas as pd
import quantstats as qs
from pathlib import Path

# diwnload the stocjk price data 
def import_data(ticker, period):
    if period == 'daily':
        tmp = openbb.stocks.load(ticker, start_date="2007-01-01", end_date="2023-11-30")
    elif period == 'weekly':
        tmp = openbb.stocks.load(ticker, start_date="2007-01-01", end_date="2023-11-30", weekly=True)
    elif period == 'monthly':
        tmp = openbb.stocks.load(ticker, start_date="2007-01-01", end_date="2023-11-30", monthly=True)
    return(tmp)

ticker = 'MC.PA'
data_daily = import_data(ticker = ticker, period ='daily')
data_weekly = import_data(ticker= ticker, period = 'weekly')
data_monthly = import_data(ticker = ticker, period = 'monthly')
  
def save_data(ticker, period, data):
  filepath = Path('/Users/benj/Desktop/Montreux_backtest_n/data/test/' + ticker + "_" + period + '.csv')  
  filepath.parent.mkdir(parents=True, exist_ok=True)  
  return(data.to_csv(filepath))

save_data("lvmh", "daily", data_daily)
save_data("lvmh", "weekly", data_weekly)
save_data("lvmh", "monthly", data_monthly)

# plot the returns and moving average 
aapl_returns = data["Adj Close"].pct_change()

# Quant statistics on apple returns 
qs.reports.metrics(aapl_returns, mode="full")

# Connect to the Inteactive Brokers API 

from ipabi.client import EClient
