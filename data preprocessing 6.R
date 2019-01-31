library(zoo)
library(TTR)

raw_data = read.csv('raw_data.csv')

#Dependent Variable: Tendency: The stock price Up/Down
tendency <- function(x){
  pricechange <- Cl(x)-Op(x)
  tendency <- ifelse(pricechange>0,'Up','Down')
  return(tendency)
}

#Basic Variables:
###
#1. Day Close(Daily)
#2. Day Open(Daily)
#3. Highest Price(Daily)
#4. Lowest price(Daily)
#5. Adjusted close price(Daily)
###
#Technical Variables:
#6. MACD
myMACD <- function(x) MACD(Cl(x))[,'signal']

#7. RSI
myRSI <- function(x) RSI(Cl(x))

#8. TRIX
myTRIX <- function(x) TRIX(Cl(x))[,'signal']

#9. SAR
mySAR <- function(x) SAR(cbind(Hi(x),Cl(x)))[,'sar']

#10. ROC - Volume
myROC.Vo <- function(x) ROC(Vo(x))

#11. ROC - Cl
myROC.Cl <- function(x) ROC(Cl(x))

#12. CCI
myCCI <- function(x) CCI(HLC(x))

#13. ATR
myATR        <- function(x) ATR(HLC(x))[,'atr']

#14. SMI
mySMI        <- function(x) SMI(HLC(x))[, "SMI"]

#15. ADX
myADX        <- function(x) ADX(HLC(x))[,'ADX']

#16. Aroon 
myAroon      <- function(x) aroon(cbind(Hi(x),Lo(x)))[,'oscillator']

#17. BB  
myBB         <- function(x) BBands(HLC(x))[,'pctB']

#18. ChaikinVol
myChaikinVol <- function(x) Delt(chaikinVolatility(cbind(Hi(x),Lo(x))))
head(myChaikinVol(AAPL))

#19. CLV 
myCLV        <- function(x) EMA(CLV(HLC(x)))

#20. EMV 
myEMV        <- function(x) EMV(cbind(Hi(x),Lo(x)),Vo(x))[,2]

#21. MFI 
myMFI        <- function(x) MFI(HLC(x),  Vo(x))

#22. Volat
myVolat      <- function(x) volatility(OHLC(x),calc="garman")
#23. CMO - Adjusted Price
myCMO.Ad <- function(x) CMO(Ad(x))

#24. CMO - Volume
myCMO.Vo <- function(x) CMO(Vo(x))

#25. RunMean - Close
myRunMean <- function(x) runMean(Cl(x))

#26. runSD - Close
myRunSD <- function(x) runSD(Cl(x))

#27. GMMA - Volume
myGMMA.Vo <- function(x) GMMA(Vo(x))[,1]


#28. KST - Close
myKST <- function(x) KST(Cl(x))[,'signal']

#29. DPO - Volume
myDPO <-  function(x) DPO(Vo(x))

#30. DVI - Adjusted Price
myDVI <- function(x) DVI(Ad(x))[,'dvi']

row.names(raw_data) <- NULL
raw_data <- na.omit(raw_data)

dim(raw_data)
first(raw_data)
Op(raw_data)
new_data <- function(raw_data){
  df <- data.frame(Tendency = tendency(raw_data),
                   raw_data,
                   ATR = myATR(raw_data),
                   SMI = mySMI(raw_data),
                   ADX = myADX(raw_data),
                   Aroon = myAroon(raw_data),
                   BB = myBB(raw_data),
                   ChaikinVol = myChaikinVol(raw_data),
                   CLV = myCLV(raw_data),
                   CMO.Ad = myCMO.Ad(raw_data),
                   CMO.Vo = myCMO.Vo(raw_data),
                   EMA = EMA(Delt(Cl(raw_data))),
                   EMV = myEMV(raw_data),
                   Volat = myVolat(raw_data),
                   MACD = myMACD(raw_data),
                   MFI = myMFI(raw_data),
                   RSI = myRSI(raw_data),
                   SAR = mySAR(raw_data),
                   RunMean = myRunMean(raw_data),
                   runSD = myRunSD(raw_data),
                   TRIX = myTRIX(raw_data),
                   ROC.Vo = myROC.Vo(raw_data),
                   ROC.Cl = myROC.Cl(raw_data),
                   CCI = myCCI(raw_data),
                   GMMA.Vo.lag3 = myGMMA.Vo(raw_data),
                   KST = myKST(raw_data),
                   DPO = myDPO(raw_data),
                   DVI = myDVI(raw_data))
  return(df)
}

new_data = new_data(raw_data)
write.csv(new_data, 'raw_data.csv')