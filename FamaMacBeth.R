#load packages
require(quantmod)
require(PerformanceAnalytics)

#define ticker
ticker <- c('A','ABC','ABT','ADBE','ADI','ADP','ADSK','AEP','AES',
		'AET','AIV','ALL','AMAT','AMGN','AMZN','AN','ANTM','AON',
		'APA','APD','AVY','AZO','BA','BAC','BAX','BBBY','BBT','BBY','BCR',
		'BDX','BEN','BHI','BIIB','BK','BLL','BMY','BSX','BXP','CA',
		'CAG','CAH','CAT','CCL','CELG','CHK','CI','CINF',
		'CL','CLX','CMA','CMI','CNP','COF','COH','COL','COP',
		'COST','CPB','CSCO','CSX','CTAS','CTL','CTSH','CTXS','CVS','CVX',
		'D','DD','DE','DGX','DHI','DIS','DOV','DOW','DRI','DTE','DUK',
		'DVN','EA','EBAY','ECL','ED','EFX','EIX','EL','EMN','EMR','EOG','EQR',
		'ESRX','ETFC','ETN','ETR','EXC','F','FDX','FE','FIS','FISV','FITB',
		'FLR','FTR','GD','GILD','GIS','GLW','GPC','GPS','GS','GT',
		'GWW','HAL','HAR','HAS','HBAN','HD','HIG','HOG','HPQ','HRB',
		'HSY','HUM','IBM','IFF','INTU','IP','IPG','ITW','JCI','JNJ',
		'JNPR','JPM','JWN','K','KEY','KIM','KLAC','KMB','KO','KR','KSS','LEG',
		'LEN','LH','LLL','LLTC','LLY','LNC','LUV','MAS',
		'MCD','MCO','MDT','MKC','MMC','MMM','MO','MON','MRK','MRO',
		'MS','MSFT','MU','MUR','MYL','NEE','NI','NKE','NOV','NSC',
		'NTAP','NTRS','NUE','NVDA','NWL','OMC','ORCL','OXY','PAYX','PBI',
		'PCAR','PCG','PDCO','PEG','PEP','PFE','PFG','PG','PGR','PH','PHM',
		'PKI','PNC','PNW','PPG','PPL','PSA','PX','R','RAI','RHI','ROK',
		'SBUX','SEE','SHW','SNA','SO','SPG','SPLS','SRE','STI','STT',
		'STZ','SWK','SYK','SYMC','SYY','T','TAP','TGNA','TGT','TIF','TJX',
		'TMK','TMO','TROW','TSN','TWX','TXN','UNH','UNP','UPS','USB',
		'UTX','VFC','VLO','VMC','VNO','VRSN','VZ','WAT','WFM','WHR',
		'WM','WMB','WMT','WY','XEL','XL','XLNX','XOM','XRX','YHOO','YUM','ZBH','ZION')


#load ticker data
getSymbols(ticker,from="2002-01-01",to="2016-12-31")
prices <- NULL
for(i in ticker){
	prices <- cbind(prices,Ad(get(i)))
}
colnames(prices) <- ticker
returns <- na.omit(Return.calculate(prices))

#load additional data
getSymbols(c("SPY","SHY"),from="2002-01-01",to="2016-12-31")
indexprices <- Ad(SPY)
cashprices <- Ad(SHY)
bondprices <- Ad(TLT)
indexreturns <- na.omit(Return.calculate(indexprices))
cashreturns <- na.omit(Return.calculate(cashprices))



#fixed beta
betas_fixed <- NULL
pvals_fixed
for(i in ticker){
	reg = lm(returns["/2004-12-31",i]~indexreturns["/2004-12-31"])
	betas_fixed <- c(betas_fixed,as.numeric(coef(reg)[2]))
	pvals_fixed <- c(pvals_fixed,as.numeric(coef(summary(reg))[8]))
}

isreturns <- returns["2004-01-01/2010-12-31"]
dailypremium <- NULL
for(i in 1:nrow(isreturns)){
	reg <- lm(as.numeric(isreturns[i,])~betas)
	dailypremium <- c(dailypremium,as.numeric(coef(reg)[2]))
}
dailypremium <- xts(dailypremium,index(isreturns))
lb <- 252
avrgpremium <- na.omit(runMean(dailypremium,lb))

weights_fb <- NULL
for(i in avrgpremium){
	if(i>0){
		weights_fb <- rbind(weights_fb,c(1,0))
	}else{
		weights_fb <- rbind(weights_fb,c(0,1))
	}
}
weights_fb <- xts(weights_fb,index(avrgpremium))
rets <- cbind(indexreturns,cashreturns)[index(avrgpremium)]
strat_fb <- Return.rebalancing(rets,weights_fb)
plotdata = cbind(strat_fb,indexreturns[index(strat_fb)])
colnames(plotdata) = c("FM-Regression","SPY")
chart.CumReturns(plotdata,ylab="Cumulative Return",main="Fixed Beta Performance",legend.loc="topleft",colorset=c(1,8))




#fixed beta 2
betas_fixed2 <- NULL
pvals_fixed2 <- NULL
for(i in ticker){
	reg <- lm(returns["2004-01-01/2004-12-31",i]~indexreturns["2004-01-01/2004-12-31"])
	betas_fixed2 <- c(betas_fixed2,as.numeric(coef(reg)[2]))
	pvals_fixed2 <- c(pvals_fixed2,as.numeric(coef(summary(reg))[8]))
}


isreturns <- returns["2004-01-01/2010-12-31"]
dailypremium <- NULL
for(i in 1:nrow(isreturns)){
	reg <- lm(as.numeric(isreturns[i,])~betas)
	dailypremium <- c(dailypremium,as.numeric(coef(reg)[2]))
}
dailypremium <- xts(dailypremium,index(isreturns))
lb <- 252
avrgpremium <- na.omit(runMean(dailypremium,lb))


weights_fb2 <- NULL
for(i in avrgpremium){
	if(i>0){
		weights_fb2 <- rbind(weights_fb2,c(1,0))
	}else{
		weights_fb2 <- rbind(weights_fb2,c(0,1))
	}
}
weights_fb2 <- xts(weights_fb2,index(avrgpremium))
rets <- cbind(indexreturns,cashreturns)[index(avrgpremium)]
strat_fb2 <- Return.rebalancing(rets,weights2)
plotdata = cbind(strat_fb2,indexreturns[index(strat_fb2)])
colnames(plotdata) = c("FM-Regression","SPY")
chart.CumReturns(plotdata,ylab="Cumulative Return",main="Fixed Beta Performance",legend.loc="topleft",colorset=c(1,8))






#rolling betas
isreturns <- returns["/2010-12-31"]
lb = 252
weights_rolling <- NULL
for(i in lb:nrow(isreturns)){
	print(paste(i-lb,"/",nrow(isreturns)-lb))
	tmpret <- isreturns[(i-lb+1):i,]
	betas <- NULL
	for(tick in ticker){
		reg <- lm(tmpret[,tick]~indexreturns[index(tmpret)])
		betas <- c(betas,as.numeric(coef(reg)[2]))
	}
	dailypremium <- NULL
	for(day in 1:nrow(tmpret)){
		reg <- lm(as.numeric(tmpret[day,])~betas)
		dailypremium <- c(dailypremium,as.numeric(coef(reg)[2]))
	}
	avrgpremium <- mean(dailypremium)
	if(avrgpremium>0){
		weights_rolling <- rbind(weights_rolling,c(1,0))
	}else{
		weights_rolling <- rbind(weights_rolling,c(0,1))
	}
}
weights_rolling <- xts(weights_rolling,index(isreturns)[lb:nrow(isreturns)])
rets <- cbind(indexreturns,cashreturns)[index(weights_rolling)]
strat_rolling = Return.rebalancing(rets,weights_rolling)
plotdata = cbind(strat_rolling,indexreturns[index(strat_rolling)])
colnames(plotdata) = c("FM-Regression","SPY")
chart.CumReturns(plotdata,ylab="Cumulative Return",main="Rolling Beta Performance",legend.loc="topleft",colorset=c(1,8))


