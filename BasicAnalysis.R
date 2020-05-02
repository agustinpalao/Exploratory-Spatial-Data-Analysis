#####     Agustin Palao
#####                    Data for Credit Guarantees
#####                   Create time series graphs

require(foreign)
library(data.table)
require(tseries)
require(forecast)
require(TSA)

#Get the data. From new csv dataset already conditioned
b<-read.csv("Data3.csv")
names(b)[names(b)=="FI"] <- "ID_FI"
b<-as.data.frame(b)


#(2) Modifying vector properties
b$Municipality<-as.character(b$Municipality)
b$State_office_name<-as.character(b$State_office_name)
b$State<-as.character(b$State)
b$Date<- as.Date(b$Date, format = "%m/%d/%Y")

attach(b)

# Count How many operations per type of coverage.
#JFEGA<-length(which(FONAGA==0))
library(dplyr)
JFEGA<- filter(b, FONAGA=="0")
FEGAB<- filter(b, Date<"2008-04-01")
JFON<-filter(b, FONAGA=="1", Nominal_Pctj==0)
FEGFON<-filter(b, FONAGA=="1", Nominal_Pctj>0)

#Include economic activity categories

prod_ch<-read.csv("Prod_Chains.csv")
names(prod_ch)[names(prod_ch)=="Cadena"] <- "Chain"

prod_chains<-merge(b[5:11],prod_ch,by="Chain")


#Create a table by chain 
chain<-table(prod_chains$CATEGORY)
tchain<-as.data.frame(chain)
names(tchain)[names(tchain)=="Var1"] <- "Economic Activity"
names(tchain)[names(tchain)=="Freq"] <- "Number of Guaranteed Credit Lines"
total<-addmargins(chain)
pctg<-(chain[1:5]/total[6])*100
tchain["%"]<-pctg
library(xtable)
options(xtable.floating=FALSE)
options(xtable.timestamp="")

xtable(tchain)


#Analyze agricultural produce first
Agr<-subset(prod_chains, prod_chains$ID_Cat==1)
library(plyr)
Agr_chain<- count(Agr, c("Chain", "DESCRIPTION", "INEGI"))
Agr_chain<-Agr_chain[order(Agr_chain$freq,decreasing=TRUE),]

Agr_chain["Pctg"]<-round((Agr_chain$freq/sum(Agr_chain$freq))*100,digits = 1)
names(Agr_chain)[names(Agr_chain)=="freq"] <- "Number CGS Operations"

Agr_chain1<-head(Agr_chain,n=11)
Agr_chain1["INEGI"]=NULL
Agr_chain1["Chain"]=NULL
rownames(Agr_chain1) <- Agr_chain1[,1]
Agr_chain1[,1] <- NULL
Agr_chain1["TOTAL" ,] <- colSums(Agr_chain1)



library(xtable)
options(xtable.floating=FALSE)
options(xtable.timestamp="")

xtable(Agr_chain1)


#Analyze forestry
Forest<-subset(prod_chains, prod_chains$ID_Cat==2)
library(plyr)
Frst_chain<- count(Forest, c("Chain", "DESCRIPTION", "INEGI"))
Frst_chain<-Frst_chain[order(Frst_chain$freq,decreasing=TRUE),]

Frst_chain["Pctg"]<-round((Frst_chain$freq/sum(Frst_chain$freq))*100,digits = 1)
names(Frst_chain)[names(Frst_chain)=="freq"] <- "Number CGS Operations"

Frst_chain1<-head(Frst_chain,n=12)
Frst_chain1["INEGI"]=NULL
Frst_chain1["Chain"]=NULL
rownames(Frst_chain1) <- Frst_chain1[,1]
Frst_chain1[,1] <- NULL
Frst_chain1["TOTAL" ,] <- colSums(Frst_chain1)

library(xtable)
options(xtable.floating=FALSE)
options(xtable.timestamp="")

xtable(Frst_chain1)

#Analyze livestock
LivStoc<-subset(prod_chains, prod_chains$ID_Cat==3)
library(plyr)
Lvst_chain<- count(LivStoc, c("Chain", "DESCRIPTION", "INEGI"))
Lvst_chain<-Lvst_chain[order(Lvst_chain$freq,decreasing=TRUE),]

Lvst_chain["Pctg"]<-round((Lvst_chain$freq/sum(Lvst_chain$freq))*100,digits = 1)
names(Lvst_chain)[names(Lvst_chain)=="freq"] <- "Number CGS Operations"

Lvst_chain1<-head(Lvst_chain,n=2)
Lvst_chain1["INEGI"]=NULL
Lvst_chain1["Chain"]=NULL
rownames(Lvst_chain1) <- Lvst_chain1[,1]
Lvst_chain1[,1] <- NULL
Lvst_chain1["TOTAL" ,] <- colSums(Lvst_chain1)

xtable(Lvst_chain1)

#Analyze fishery
Fish<-subset(prod_chains, prod_chains$ID_Cat==4)
library(plyr)
Fsh_chain<- count(Fish, c("Chain", "DESCRIPTION", "INEGI"))
Fsh_chain<-Fsh_chain[order(Fsh_chain$freq,decreasing=TRUE),]

Fsh_chain["Pctg"]<-round((Fsh_chain$freq/sum(Fsh_chain$freq))*100,digits = 1)
names(Fsh_chain)[names(Fsh_chain)=="freq"] <- "Number CGS Operations"

Fsh_chain1<-head(Fsh_chain,n=3)
Fsh_chain1["INEGI"]=NULL
Fsh_chain1["Chain"]=NULL
rownames(Fsh_chain1) <- Fsh_chain1[,1]
Fsh_chain1[,1] <- NULL
Fsh_chain1["TOTAL" ,] <- colSums(Fsh_chain1)

xtable(Fsh_chain1)

#Create a table by Financial Intermediaries
FI_cat<-read.csv("FIs.csv")
b_FI <- merge(b, FI_cat , by.x="ID_FI", by.y = "ID") #,all=TRUE)


NumFIsType<-sort(table(FI_cat$FI_TYPE), decreasing = TRUE)
FIsType<-sort(table(b_FI$FI_TYPE), decreasing = TRUE)
pctg_FIsType<-FIsType/margin.table(FIsType)
FIs<-sort(table(FI), decreasing=TRUE)
pctg_FI<-FIs/margin.table(FIs)

FI<-as.data.frame(FI)
names(FI)[names(FI)=="dimnames"] <- "ID_FI"
names(FI)[names(FI)=="FI"] <- "OpCount"
total<-addmargins(FI$OpCount)

#Create table for type of credit
credtype<-sort(table(b$Class), decreasing = TRUE)

# Pie Chart with Percentages
library(plotrix)
lbls <- c("Investment","Working Capital","Collateral-based Inventory")
pct <- round(credtype/sum(credtype)*100,1)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
lp<-pie3D(pct, radius = 2.5, start = 0,theta = 0.9, height = .2, explode=0.2, col = c("orange","green", "pink"),border = c("orange","green","pink"),pty = "s")
lp[3]<-6.4
pie3D.labels(lp,labels=lbls,labelcex = 1.2,labelrad = 2.4, minsep = 0.5)



