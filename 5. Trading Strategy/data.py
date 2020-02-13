import yfinance as yf
import pandas as pd
import numpy as np
from datetime import datetime


import matplotlib.pyplot as plt
import mpl_finance as mpf

temp = ""

tickers = pd.read_csv("Tickers.csv")
for i in tickers.Name:
    temp += i + " "

tickers = temp
del i
del temp

data = yf.download("AAPL", start="2019-01-01", group_by="Tickers", threads = True)
l = list(data.index)
for i in range(len(l)):
    l[i] = l[i].strftime("%m/%d/%Y")

fig = plt.figure(figsize=(24, 8))
ax = fig.add_subplot(1, 1, 1)
ax.set_xticks(range(0, len(l), 30))
ax.set_xticklabels(l)
mpf.candlestick2_ochl(ax, data['Open'], data['Close'], data['High'], data['Low'],
                     width=0.5, colorup='g', colordown='r',
                     alpha=0.6)
plt.grid()