import yfinance as yf
import pandas as pd
import numpy as py

temp = ""

tickers = pd.read_csv("Tickers.csv")
for i in tickers.Name:
    temp += i + " "

tickers = temp
del i
del temp

data = yf.download(tickers, group_by="Tickers", threads = True)