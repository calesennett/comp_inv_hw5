import QSTK.qstkutil.qsdateutil as du
import QSTK.qstkutil.tsutil as tsu
import QSTK.qstkutil.DataAccess as da
import QSTK.qstkstudy.EventProfiler as ep
import datetime as dt
import numpy as np
import pandas as pd
from itertools import *
import matplotlib.pyplot as plt

def main():
    s_date = dt.datetime(2008, 1, 1)
    e_date = dt.datetime(2008, 12, 31)
    lookback = 20
    print "Reading data..."
    data, symbols, timestamps = setup(s_date, e_date, lookback)
    upper_bol, lower_bol, indicator_bol = bol_band(data, symbols, timestamps, lookback, s_date)
    create_plots(symbols, data, upper_bol, lower_bol, indicator_bol)

def create_plots(syms, data, ub, lb, ib):
    prices = data['close'][syms[0]]
    plt.clf()
    plt.figure(1)
    plt.subplot(211)
    plt.plot(prices.index, prices.values)
    plt.plot(ub.index, ub.values)
    plt.plot(lb.index, lb.values)
    plt.legend(syms)
    plt.ylabel('Adjusted Close')
    plt.xlabel('Date')

    plt.subplot(212)
    plt.plot(ib.index, ib.values)
    plt.legend(["Indicator"])
    plt.ylabel('Value')
    plt.xlabel('Date')

    plt.savefig('figure.pdf', format='pdf')

def bol_band(data, syms, timestamps, lookback, s_date):
    prices = data['close'][syms[0]]
    rm = pd.rolling_mean(prices, lookback).truncate(before=s_date)
    rstd = pd.rolling_std(prices, lookback).truncate(before=s_date)
    upper_bol_vals = []
    lower_bol_vals = []
    indicator_bol_vals = []

    for timestamp in rm.index:
        upper_bol_val = (rm[timestamp] + 2 * rstd[timestamp])
        lower_bol_val = (rm[timestamp] - 2 * rstd[timestamp])
        indicator_bol_val = (prices[timestamp] - rm[timestamp]) / rstd[timestamp]
        upper_bol_vals.append(upper_bol_val)
        lower_bol_vals.append(lower_bol_val)
        indicator_bol_vals.append(indicator_bol_val)

    return pd.DataFrame(upper_bol_vals, index=rm.index) \
          ,pd.DataFrame(lower_bol_vals, index=rm.index) \
          ,pd.DataFrame(indicator_bol_vals, index=rm.index)

def setup(s_date, e_date, lookback):
    time_of_day = dt.timedelta(hours=16)
    timestamps = du.getNYSEdays(s_date - dt.timedelta(days=lookback+10), e_date, time_of_day)
    data, symbols = read_data(timestamps)
    return data, symbols, timestamps

def read_data(timestamps):
    data_obj = da.DataAccess('Yahoo')
    symbols = ['GOOG']
    keys = ['close']
    all_data = data_obj.get_data(timestamps, symbols, keys)

    # remove NaN from price data
    all_data = dict(zip(keys, all_data))
    return all_data, symbols


if __name__ == "__main__":
    main()

