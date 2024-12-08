# The quantmod package is used for financial modeling and trading
# Installation of the quantmod package
install.packages("quantmod")
library(quantmod)
options("getSymbols.warning4.0" = FALSE)

## Retrieve the JSE stock prices data from Yahoo finance
fin.stocks <- c("SBK.JO", "NED.JO" , "FSR.JO", "ABG.JO", "CPI.JO", "INL.JO", "INP.JO")
?getSymbols
fin.stocks_prices <- getSymbols(fin.stocks,src = "yahoo", from = "2010-01-01", auto.assign = TRUE)

tech.etfs <- c("ETF5IT.JO", "STXNDQ.JO", "SYFANG.JO" , "SYG4IR.JO")
tech.etfs_prices <- getSymbols(tech.etfs, from = "2010-01-01", auto.assign = TRUE)


#Data inspection - remove the missing values
ETF5IT.JO <- na.omit(ETF5IT.JO)
STXNDQ.JO <- na.omit(STXNDQ.JO)
SYFANG.JO <- na.omit(SYFANG.JO)
SYG4IR.JO <- na.omit(SYG4IR.JO)

#View the data
head(SBK.JO)
tail(SBK.JO)
View(SBK.JO)

head(ETF5IT.JO)


# Plot charts
?chartSeries
chartSeries(CPI.JO, theme = "white", TA=NULL, subset = 'last 7 months')
addBBands()
addRSI()
addSMA(n=200)
addMomentum()

#Get quote Fin Stocks
?yahooQF
yahooQF()

query <- yahooQF(c("Symbol", "Name", "Market Capitalization", "P/E Ratio",
                   "Price/Book", "Dividend Yield", "EPS Current Year",
                   "EPS Forward", "Average Analyst Rating"))

?getQuote
stock_info <- getQuote(fin.stocks, what = query)


# Get quotes Tech EFTs
query2 <- yahooQF(c("Symbols", "Name", "First Trade Date",
                    "Percent Change From 50-day Moving Average",
                     "52-week Range"))

etfs_info <- getQuote(tech.etfs, what = query2)

# Calculating the monthly returns
SBK_YTD <- periodReturn(SBK.JO, period = "monthly") * 100
NED_YTD <- periodReturn(NED.JO, period = "monthly") * 100
FSR_YTD <- periodReturn(FSR.JO, period = "monthly") * 100
ABG_YTD <- periodReturn(ABG.JO, period = "monthly") * 100
CPI_YTD <- periodReturn(CPI.JO, period = "monthly") * 100
INL_YTD <- periodReturn(INL.JO, period = "monthly") * 100
INP_YTD <- periodReturn(INP.JO, period = "monthly") * 100


# combing all the returns into a singular view
fin.returns <- cbind(SBK_YTD, NED_YTD,FSR_YTD, ABG_YTD, CPI_YTD, INL_YTD, INP_YTD)

#Renaming the column names
colnames(fin.returns) <-  c("SBK", "NED" , "FSR", "ABG", "CPI", "INL", "INP")
