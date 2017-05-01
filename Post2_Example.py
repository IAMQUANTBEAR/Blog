import numpy as np
import pandas as pd
import scipy.stats
import pandas_datareader.data as web

data = web.DataReader("SPY", "yahoo", "2000-01-01", "2009-12-31")
prices = data["Adj Close"]
returns = np.log(prices) - np.log(prices.shift(1))[1:]
returns.name = "Log Ret"


#all used parameters
lookback = 500
alpha = 0.95
lam = 0.95

#PARAMETRIC VAR
VaR_parametric = []

for i in range(lookback,returns.shape[0]):
    tmpret = returns.ix[(i-lookback+1):(i+1),]
    VaR_parametric.append(tmpret.mean()+scipy.stats.norm.ppf(1-alpha)*tmpret.std())
VaR_parametric = pd.Series(VaR_parametric).shift(1)
VaR_parametric.index = returns.index[lookback:]
periodreturns = returns.ix[lookback:,]

exceptions_parametric = (periodreturns < VaR_parametric)
rollingExceptions_parametric = (exceptions_parametric.rolling(window=250).sum())[250:]



#HISTORICAL SIMULATION
#basic method
VaR_hs = []
for i in range(lookback,returns.shape[0]):
    tmpret = returns.ix[(i-lookback+1):(i+1),]
    VaR_hs.append(tmpret.quantile(1-alpha))
VaR_hs = pd.Series(VaR_hs).shift(1)
VaR_hs.index = returns.index[lookback:]
periodreturns = returns.ix[lookback:,]

exceptions_hs = (periodreturns < VaR_hs)
rollingExceptions_hs = (exceptions_hs.rolling(window=250).sum())[250:]



#age weighting
ageweights = pd.Series((lam**(np.arange(lookback,0,-1)-1)*(1-lam))/(1-lam**lookback),name="Ageweight")
VaR_aw = []
for i in range(lookback,returns.shape[0]):
    tmpret = returns.ix[(i-lookback+1):(i+1),]
    cmb = pd.concat([tmpret.reset_index(drop=True),ageweights],axis=1)
    cmb = cmb.sort_values("Log Ret").reset_index(drop=True)
    cmb.ix[:,1] = cmb.ix[:,1].cumsum()
    sel = [i for i,x in enumerate(cmb.ix[:,1]) if x < 1-alpha]
    sel.append(0)
    sel = max(sel)
    #linear interpolation
    tmpvar = cmb.ix[sel,"Log Ret"] + ((1-alpha)-cmb.ix[sel,"Ageweight"]) * ((cmb.ix[sel+1,"Log Ret"]-cmb.ix[sel,"Log Ret"])/(cmb.ix[sel+1,"Ageweight"]-cmb.ix[sel,"Ageweight"]))
    VaR_aw.append(tmpvar)
    
VaR_aw = pd.Series(VaR_aw).shift(1)
VaR_aw.index = returns.index[lookback:]
periodreturns = returns.ix[lookback:,]

exceptions_aw = (periodreturns < VaR_aw)
rollingExceptions_aw = (exceptions_aw.rolling(window=250).sum())[250:]



#vol scaling
vols = returns.rolling(center=False,window=20).std().bfill()
scaledret = returns/vols

VaR_vol = []
for i in range(lookback,returns.shape[0]):
    tmpret = scaledret.ix[(i-lookback+1):(i+1),]*vols[i]
    VaR_vol.append(tmpret.quantile(1-alpha))
    
VaR_vol = pd.Series(VaR_vol).shift(1)
VaR_vol.index = returns.index[lookback:]
periodreturns = returns.ix[lookback:,]

exceptions_vol = (periodreturns < VaR_vol)
rollingExceptions_vol = (exceptions_vol.rolling(window=250).sum())[250:]




#age weighted & vol scaled
ageweights = pd.Series((lam**(np.arange(lookback,0,-1)-1)*(1-lam))/(1-lam**lookback),name="Ageweight")
vols = returns.rolling(center=False,window=20).std().bfill()
scaledret = returns/vols

VaR_awvol = []
for i in range(lookback,returns.shape[0]):
    tmpret = scaledret.ix[(i-lookback+1):(i+1),]*vols[(i-19):i].max()
    cmb = pd.concat([tmpret.reset_index(drop=True),ageweights],axis=1)
    cmb = cmb.sort_values("Log Ret").reset_index(drop=True)
    cmb.ix[:,1] = cmb.ix[:,1].cumsum()
    sel = [i for i,x in enumerate(cmb.ix[:,1]) if x < 1-alpha]
    sel.append(0)
    sel = max(sel)
    tmpvar = cmb.ix[sel,"Log Ret"] + ((1-alpha)-cmb.ix[sel,"Ageweight"]) * ((cmb.ix[sel+1,"Log Ret"]-cmb.ix[sel,"Log Ret"])/(cmb.ix[sel+1,"Ageweight"]-cmb.ix[sel,"Ageweight"]))
    VaR_awvol.append(tmpvar)
    
VaR_awvol = pd.Series(VaR_awvol).shift(1)
VaR_awvol.index = returns.index[lookback:]
periodreturns = returns.ix[lookback:,]

exceptions_awvol = (periodreturns < VaR_awvol)
rollingExceptions_awvol = (exceptions_awvol.rolling(window=250).sum())[250:]