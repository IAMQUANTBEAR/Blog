#This script allows you to calculate proxy values for the VXX ETF before its inception on the 29t of January 2009
#Prerequisites:

#obtain historical CBOE VIX Futures data following the instructions outlined in this post by Ilya Kipnis (QuantStratTradeR)
#https://quantstrattrader.wordpress.com/2017/04/27/creating-a-vix-futures-term-structure-in-r-from-official-cboe-settlement-data/

#have the variables expiries and masterlist defined exactly as described in the post above

#have obtained TBill Rates from https://www.treasurydirect.gov/instit/annceresult/annceresult_query.htm and manipulated as described in my post

#Loading required packages
require(xts)
require(bizdays)
load_rmetrics_calendars(2004:2018)


#Transforming the expiries
expiries = as.Date(apply(expiries,1,function(x){paste(x,collapse=" ")}),format="%d %b %Y")

#preparing the tbillrates
tbillrates <- read.csv("Blog\\tbill.csv")
tbillrates = xts(tbillrates[,"Rate"],as.Date(tbillrates[,"Date"]))


#defining function to calculate the contract roll weights
getCRW <- function(today){
	today = as.Date(today)
	periodstart = expiries[max(which(expiries<=today))]
	periodend = expiries[min(which(expiries>today))]
	dt = bizdays(periodstart,periodend,"Rmetrics/NYSE")
	dr = bizdays(today,periodend,"Rmetrics/NYSE")-1
	return(c(dr/dt,(dt-dr)/dt))
}

#defining function to calculate TBR
getTBR <- function(today,lastday){
	today = as.Date(today)
	lastday = as.Date(lastday)
	delta = as.numeric(today-lastday)
	rate = tbillrates[max(which(index(tbillrates)<today))]
	tbr = (1/(1-91/360*rate))^(delta/91)-1
	return(tbr)
}


#calculating the index values
days = index(masterlist["2005-12-20/2009-01-29"])
indx = 100000
for(i in 2:length(days)){
	crw = getCRW(days[i-1])
	tbr = getTBR(days[i],days[i-1])
	fut1 = masterlist[days[i-1],which(!is.na(masterlist[days[i-1]]))[1:2]]
	fut2 = masterlist[days[i],which(!is.na(masterlist[days[i]]))[1:2]]
	if(!names(fut1)[1]%in%names(fut2)){
		fut1 = masterlist[days[i-1],which(!is.na(masterlist[days[i-1]]))[2:3]]
	}
	twdi = sum(crw*as.numeric(fut1))
	twdo = sum(crw*as.numeric(fut2))
	cdr = twdo/twdi-1
	val = indx[length(indx)]*(1+cdr+tbr)
	indx = c(indx,val)
}
indx = xts(indx,days)

#adjusting for 10:1 split
indx["2007-03-26/"] = 10*indx["2007-03-26/"]
indxret = (indx/lag(indx)-1)[-1]

#calculating VXX values
vxxvals = 26772.48
for(i in nrow(indxret):1){
	tmp = vxxvals[length(vxxvals)]/(1+indxret[i,])
	vxxvals = c(vxxvals,tmp)
}
vxxvals = rev(vxxvals)
vxxvals = xts(vxxvals,index(indx))