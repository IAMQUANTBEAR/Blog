#load packages
require(quantmod)
require(PerformanceAnalytics)

#function to calculate exceptions and rolling exceptions and does the plotting
getVaRExceptions <- function(returns,VaR,file=NULL){
	exceptions <- returns<VaR
	exceptions[1] <- FALSE
	
	exceptions_plot <- NA
	for(i in 2:nrow(exceptions)){
		if(exceptions[i,]){
			if(!exceptions[i-1,]){
				exceptions_plot[i-1] <- VaR[i,]
			}
			exceptions_plot <- c(exceptions_plot,returns[i])
		}else{
			exceptions_plot <- c(exceptions_plot,NA)
		}
	}
	exceptions_plot <- xts(exceptions_plot,index(exceptions))
	
	rollingExceptions <- runSum(exceptions,250)
	
	if(!is.null(file)){
		jpeg(file,1280,640)
	}
	par(mfrow=c(2,1))
	plot(x = returns,  ylab = "Log Returns",main = "VaR Exceptions for SPY",  major.ticks= "years",
			minor.ticks = FALSE, col = "black")
	lines(VaR,col="lightblue",lwd=2)
	lines(exceptions_plot,col="red",lwd=2)
	legend("topleft",legend=c("Log Returns","Exceptions","VaR"),col=c("black","red","lightblue"),lwd=c(1,1,2),bty="n")
	plot(x=rollingExceptions,main="Rolling VaR Exceptions for SPY",ylab="# of Exceptions")
	lines(rollingExceptions,lwd=1)
	abline(h=12.5,col="blue",lwd=2)
	abline(h=9,col="blue",lwd=2,lty=2)
	abline(h=16,col="blue",lwd=2,lty=2)
	legend("topleft",legend=c("Exceptions","Expected","Quantiles"),col=c("black","blue","blue"),lty=c(1,2),lwd=c(1,2,2),bty="n")
	par(mfrow=c(1,1))
	if(!is.null(file)){
		dev.off()
	}
}


#load data
getSymbols("SPY",from="2000-01-01",to="2009-12-31")
prices <- Ad(SPY)
returns <- Return.calculate(prices,"log")[-1]

#all used parameters
lookback <- 500
alpha <- 0.95
lambda <- 0.95


#PARAMETRIC VAR
VaR_parametric <- NULL

for(i in lookback:nrow(returns)){
	tmpret <- returns[(i-lookback+1):i]
	VaR_parametric <- c(VaR_parametric,mean(tmpret)+qnorm(1-alpha)*sd(tmpret))
}
periodreturns <- returns[lookback:nrow(returns)]
VaR_parametric <- xts(VaR_parametric,index(periodreturns))
VaR_parametric <- lag(VaR_parametric)

getVaRExceptions(periodreturns,VaR_parametric)


#HISTORICAL SIMULATION
#basic method
VaR_hs <- NULL
for(i in lookback:nrow(returns)){
	tmpreturns <- returns[(i-lookback+1):i]
	VaR_hs <- c(VaR_hs,quantile(tmpreturns,1-alpha))
}

periodreturns <- returns[lookback:nrow(returns)]
VaR_hs <- xts(VaR_hs,index(periodreturns))
VaR_hs <- lag(VaR_hs)

getVaRExceptions(periodreturns,VaR_hs)


#age weighting
ageweights <- rev((lambda^((1:lookback)-1)*(1-lambda))/(1-lambda^lookback))

VaR_aw <- NULL
for(i in lookback:nrow(returns)){
	tmpreturns <- as.numeric(returns[(i-lookback+1):i])
	cmb <- cbind(tmpreturns,ageweights)
	cmb <- cmb[order(cmb[,1],decreasing=FALSE),]
	cmb[,2] <- cumsum(cmb[,2])
	#c(...,1) is for avoiding the edge cases that last days return is the lowest
	#can also be avoided setting lambda to a value > alpha
	sel <- max(c(which(cmb[,2] < (1-alpha)),1))
	#linear interpolation
	tmpvar <- as.numeric(cmb[sel,1] + ((1-alpha)-cmb[sel,2]) * ((cmb[sel+1,1]-cmb[sel,1])/(cmb[sel+1,2]-cmb[sel,2])))
	VaR_aw <- c(VaR_aw,tmpvar)
}
periodreturns <- returns[lookback:nrow(returns)]
VaR_aw <- xts(VaR_aw,index(periodreturns))
VaR_aw <- lag(VaR_aw)

getVaRExceptions(periodreturns,VaR_aw)


#vol scaled
vols <- runSD(returns,20)
vols <- na.locf(vols,fromLast=TRUE)
scaledret <- returns/vols

VaR_vol <- NULL
for(i in lookback:nrow(scaledret)){
	tmpreturns <- scaledret[(i-lookback+1):i]
	tmpreturns <- tmpreturns*as.numeric(vols[i,1])
	VaR_vol <- c(VaR_vol,quantile(tmpreturns,1-alpha))
}

periodreturns <- returns[lookback:nrow(returns)]
VaR_vol <- xts(VaR_vol,index(periodreturns))
VaR_vol <- lag(VaR_vol)

getVaRExceptions(periodreturns,VaR_vol)



#age weighted & vol scaled
ageweights <- rev((lambda^((1:lookback)-1)*(1-lambda))/(1-lambda^lookback))
vols <- runSD(returns,20)
vols <- na.locf(vols,fromLast=TRUE)
scaledret <- returns/vols

VaR_awvol <- NULL
for(i in lookback:nrow(scaledret)){
	tmpreturns <- as.numeric(scaledret[(i-lookback+1):i])
	tmpreturns <- tmpreturns*max(as.numeric(vols[(i-19):i,1]))
	cmb <- cbind(tmpreturns,ageweights)
	cmb <- cmb[order(cmb[,1],decreasing=FALSE),]
	cmb[,2] <- cumsum(cmb[,2])
	sel <- max(c(which(cmb[,2] < (1-alpha)),1))
	tmpvar <- as.numeric(cmb[sel,1] + ((1-alpha)-cmb[sel,2]) * ((cmb[sel+1,1]-cmb[sel,1])/(cmb[sel+1,2]-cmb[sel,2])))
	VaR_awvol <- c(VaR_awvol,tmpvar)
}
periodreturns <- returns[lookback:nrow(returns)]
VaR_awvol <- xts(VaR_awvol,index(periodreturns))
VaR_awvol <- lag(VaR_awvol)

getVaRExceptions(periodreturns,VaR_awvol)

