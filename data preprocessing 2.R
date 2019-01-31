library(zoo)
library(TTR)
library(xts)
library(quantmod)
library(rpart)
library(rpart.plot)
library(lattice)
library(ggplot2)
library(caret)
library(randomForest)
library(tree)
library(e1071)
library(neuralnet)
library(nnet)

#Training Set/Test Set
#combine 5 stocks to 1 set of training set
aapl <- read.csv('AAPL_clean.csv')
googl <- read.csv('GOOGL_clean.csv')
adbe <- read.csv('ADBE_clean.csv')
bidu <- read.csv('BIDU_clean.csv')
crm <- read.csv('CRM_clean.csv')
csco <- read.csv('CSCO_clean.csv')
ibm <- read.csv('IBM_clean.csv')
intc <- read.csv('INTC_clean.csv')
msft <- read.csv('MSFT_clean.csv')
mu <- read.csv('MU_clean.csv')
nvda <- read.csv('NVDA_clean.csv')
orcl <- read.csv('ORCL_clean.csv')
qcom <- read.csv('QCOM_clean.csv')
txn <- read.csv('TXN_clean.csv')
aapl$symbol <- 'AAPL'
googl$symbol <- 'GOOGL'
adbe$symbol <- 'ADBE'
bidu$symbol <- 'BIDU'
crm$symbol <- 'CRM'
csco$symbol <- 'CSCO'
ibm$symbol <- 'IBM'
intc$symbol <- 'INTC'
msft$symbol <- 'MSFT'
mu$symbol <- 'MU'
nvda$symbol <- 'NVDA'
orcl$symbol <- 'ORCL'
qcom$symbol <- 'QCOM'
txn$symbol <- 'TXN'

#Plot
x<-cbind(aapl, googl,adbe,bidu,crm,csco,ibm,intc,msft,mu,nvda,orcl,qcom,txn)
y<-cbind(aapl[,'Adj.Close'],
         googl[,'Adj.Close'],
         adbe[,'Adj.Close'],
         bidu[,'Adj.Close'],
         crm[,'Adj.Close'],
         csco[,'Adj.Close'],
         ibm[,'Adj.Close'],
         intc[,'Adj.Close'],
        msft[,'Adj.Close'],
        mu[,'Adj.Close'],
        nvda[,'Adj.Close'],
        orcl[,'Adj.Close'],
        qcom[,'Adj.Close'],
        txn[,'Adj.Close'])
plot(as.zoo(y),main = "14 stocks Adjusted Close Price",ylab = c("aapl","googl", "adbe",'bidu',
                                                                "crm","csco","ibm","intc",
                                                                "msft","mu","nvda","orcl",
                                                                "qcom","txn")) 


raw_data <- rbind(aapl, googl,adbe,bidu,crm,csco,ibm,intc,msft,mu,nvda,orcl,qcom,txn)
raw_data <- as.data.frame(raw_data)
write.csv(raw_data, file = "raw_data.csv")
