install.packages("quantmod")

library(quantmod)
library(PerformanceAnalytics)

maxDate <- "2007-01-03"
MSFT.prices <- Ad(getSymbols("MSFT", auto.assign = F, from = maxDate))
head(MSFT.prices)

MSFT.rets <- dailyReturn(MSFT.prices)

#Calculating VAR
?VaR
VaR(MSFT.rets, p= 0.95, method="historical")
VaR(MSFT.rets, p= 0.99, method="historical")

#CVaR
CVaR(MSFT.rets, p=0.95, method = "historical")
CVaR(MSFT.rets, p=0.99, method = "historical")

tickers <- c("MSFT", "AAPL","AMZN")
weights <- c(0.50, 0.10, 0.40)
getSymbols(tickers, from = maxDate)


Port.prices <- na.omit(merge(Ad(MSFT), Ad(AAPL), Ad(AMZN)))
Port.returns <- ROC(Port.prices, type = "discrete")[-1,]
colnames(Port.returns) <- tickers

# VaR for the portfolio returns
VaR(Port.returns, p =0.95, 
    weights = weights, portfolio_method = "component", method = "modified")

VaR(Port.returns, p =0.99, 
    weights = weights, portfolio_method = "component", method = "modified")

#Conditional VaR
CVaR(Port.returns, p =0.99, 
    weights = weights, portfolio_method = "component", method = "modified")

#Expected loss
ES(Port.returns, p =0.99, 
    weights = weights, portfolio_method = "component", method = "modified")

#The historical VaR for each of the stocks
VAR.Hist <- VaR (Port.returns, p=0.95, weights = NULL ,portfolio_method = "single", method = "historical")
VAR.Gaus <- VaR (Port.returns, p=0.95, weights = NULL ,portfolio_method = "single", method = "gaussian")
VAR.Mod <- VaR (Port.returns, p=0.95, weights = NULL ,portfolio_method = "single", method = "modified")

All.VAR <- data.frame(rbind(VAR.Hist,VAR.Gaus,VAR.Mod))
rownames(All.VAR) <- c("Hist", "Gaus", "Mod")
All.VAR


#Calculating portfolio Var
PortVAR.Hist <- VaR (Port.returns, p=0.95, weights = weights ,portfolio_method = "component", method = "historical")$hVaR
PortVAR.Gaus <- VaR (Port.returns, p=0.95, weights = weights ,portfolio_method = "component", method = "gaussian")$VaR[1]
PortVAR.Mod <- VaR (Port.returns, p=0.95, weights = weights ,portfolio_method = "component", method = "modified")$MVaR[1]

All.VAR$Portfolio <- 0
All.VAR$Portfolio <- c(PortVAR.Hist,PortVAR.Gaus, PortVAR.Mod)
ALL.VAR <- abs(All.VAR)

All.VAR <- as.data.frame(lapply(All.VAR, function(x) if(is.numeric(x)) abs(x) else x))


All.VAR$Type <- c("Hist", "Gaus", "Mod")


library(reshape2)
library(ggplot2)
plotVar <- melt(All.VAR, variable.name = "Ticker", value.name = "VaR") 

ggplot(plotVar,aes(x=Type, y=VaR, fill = Ticker)) + 
  geom_bar(stat = "identity", position = "dodge")