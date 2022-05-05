# Clear every thing
rm(list=ls(all=TRUE))
set.seed(9)
# it's good coding practice to load packages at the top of a script
library(lubridate) # work with dates
library(dplyr)     # data manipulation (filter, summarize, mutate)
library(ggplot2)   # graphics
library(gridExtra) # tile several plots next to each other
library(scales)
library(vars)
library(dynlm)
require(urca)
library(reshape2) 
library(zoo)

# import the data
Data <- read.csv("final_Data.csv",header=T)

# Print the names
names(Data)

# Convert my data to time series data
Data = ts(Data, start= c(2020,3), frequency = 365)

##################################################################
##################Ploting the original data#######################

# Plot the data 
par(mfrow=c(3,3))
for (i in 2:ncol(Data))
{
  plot.ts(Data[,i], ylab=colnames(Data)[i])
  print(i)
}
##################################################################
############decomposition and seasonality adjustment #############

Open_PT_sa = decompose (Data[,"Open_PT"])
Open_PT_sa = Open_PT_sa$x - Open_PT_sa$seasonal

Volume_PT_sa = decompose (Data[,"Volume_PT"])
Volume_PT_sa = Volume_PT_sa$x - Volume_PT_sa$seasonal

Open_TH_sa = decompose (Data[,"Open_TH"])
Open_TH_sa = Open_TH_sa$x - Open_TH_sa$seasonal

Volume_TH_sa = decompose (Data[,"Volume_TH"])
Volume_TH_sa = Volume_TH_sa$x - Volume_TH_sa$seasonal

COVID_ND_sa = decompose (Data[,"COVID_ND"])
COVID_ND_sa = COVID_ND_sa$x - COVID_ND_sa$seasonal

'R_ND_100k_sa = decompose (Data[,"R_ND_100k"])
R_ND_100k_sa = R_ND_100k_sa$x - R_ND_100k_sa$seasonal'

COVID_NC_sa = decompose (Data[,"COVID_NC"])
COVID_NC_sa = COVID_NC_sa$x - COVID_NC_sa$seasonal

'R_NC_100k_sa = decompose (Data[,"R_NC_100k"])
R_NC_100k_sa = R_NC_100k_sa$x - R_NC_100k_sa$seasonal'

COVID_TV_sa = decompose (Data[,"COVID_TV"])
COVID_TV_sa = COVID_TV_sa$x - COVID_TV_sa$seasonal

'R_TV_100k_sa = decompose (Data[,"R_TV_100k"])
R_TV_100k_sa = R_TV_100k_sa$x - R_TV_100k_sa$seasonal
'

list = cbind(Open_PT_sa, Volume_PT_sa, Open_TH_sa, Volume_TH_sa,
         COVID_ND_sa,  COVID_NC_sa, COVID_TV_sa)
print(list)
# Plot the data 
par(mfrow=c(3,3))
for (i in 1:ncol(list))
{
  plot.ts(list[,i], ylab=colnames(list)[i])
  print(i)
}

##################################################################
######################Test for stationarity#######################
colnames(Data)
max (Data[,1])
min (Data[,1])
#Setting data as time series by day
Data = ts(Data, start= c(2020,3), frequency = 365)

# Applying unitroottest for all variables

unitroottests = matrix(NA,nrow=ncol(Data), ncol=5)
for (ii in 2:ncol(Data))
{  
  testadf <-ur.df(Data[,ii], type=c("trend"), selectlags="AIC")
  testadfgls <-ur.ers(Data[,ii], type=c("DF-GLS"), model="trend")
  testpp <-ur.pp(Data[,ii], type=c("Z-tau"), model="trend")
  testkpss <-ur.kpss(Data[,ii], type=c("tau"))
  unitroottests[ii,1] <- colnames(Data)[ii]
  unitroottests[ii,2] <- testadf@teststat[1] 
  unitroottests[ii,3] <- testadfgls@teststat[1] 
  unitroottests[ii,4] <- testpp@teststat[1] 
  unitroottests[ii,5] <- testkpss@teststat[1]
  print(ii)
}
colnames(unitroottests) <- c("Variable name","ADF", "ADF-GLS", "PP", "KPSS")
##################################################################
###############Keeping the original data Away#####################
Original_Data  = Data

##################################################################
###############Making necessary transformations###################
Data = Original_Data
# Take the unneeded variables in separate place
Date<- Data[,1]

# Remove the these variable from original data
Data <- Data[,-1]

# Apply the transformation for each variable alone
Data = diff(log(Data))
# bind the data again
Data = cbind(Date,Data)

# Omit all NANs
Data <- na.omit(Data)

# Convert my data to time series data
Data = ts(Data, start= c(2020,3), frequency = 365)


# Applying unitroottest for all variables after transformation

unitroottests_AT = matrix(NA,nrow=ncol(Data), ncol=5)
for (ii in 2:ncol(Data))
{  
  testadf <-ur.df(Data[,ii], type=c("trend"), selectlags="AIC")
  testadfgls <-ur.ers(Data[,ii], type=c("DF-GLS"), model="trend")
  testpp <-ur.pp(Data[,ii], type=c("Z-tau"), model="trend")
  testkpss <-ur.kpss(Data[,ii], type=c("tau"))
  unitroottests_AT[ii,1] <- colnames(Data)[ii]
  unitroottests_AT[ii,2] <- testadf@teststat[1] 
  unitroottests_AT[ii,3] <- testadfgls@teststat[1] 
  unitroottests_AT[ii,4] <- testpp@teststat[1] 
  unitroottests_AT[ii,5] <- testkpss@teststat[1]
  print(ii)
}
colnames(unitroottests_AT) <- c("Variable name","ADF", "ADF-GLS", "PP", "KPSS")

##################################################################
######################Ploting the data ###########################

# Plot the data 
par(mfrow=c(3,3))
for (i in 2:ncol(Data))
{
  plot.ts(Data[,i], ylab=colnames(Data)[i])
  print(i)
}
##################################################################
###########################Select best lag######################


Best_lag_results = VARselect(Data, type= "const", lag.max = 10)
Best_lag_results

##################################################################
###########################Appling VAR model######################

fit <- VAR(Data, ic = "AIC")

fit
##################################################################
#########################Bivariate VAR model######################
# Separate dataset
PT_Data <- Data[,2];
PT_V_Data <- Data[,3];
TH_Data <- Data[,4];
TH_V_Data <- Data[,5];
CND_Data <- Data[,6];
CNC_Data <- Data[,7];
CTV_Data <- Data[,8];

colnames(Data)
Data.PTV <- na.omit(cbind(Data[,2], Data[,3]))
colnames(Data.PTV) <- c("Open_PT","Volume_PT")
Data.OTH <- na.omit(cbind(Data[,2], Data[,4]))
colnames(Data.OTH) <- c("Open_PT","Open_TH")
Data.THV <- na.omit(cbind(Data[,2], Data[,5]))
colnames(Data.THV) <- c("Open_PT","Volume_THV")
Data.CND <- na.omit(cbind(Data[,2], Data[,6]))
colnames(Data.CND) <- c("Open_PT","COVID_ND")
Data.CNC <- na.omit(cbind(Data[,2], Data[,7]))
colnames(Data.CNC) <- c("Open_PT","COVID_NC")
Data.CTV <- na.omit(cbind(Data[,2], Data[,8]))
colnames(Data.CTV) <- c("Open_PT","COVID_TV")
################# Estimate the Reduced-Form VAR Models ######################
fit.PTV <- VAR(Data.PTV, 1); fevd.PTV <- fevd(fit.PTV,n.ahead=60)
fit.OTH <- VAR(Data.OTH, 1); fevd.OTH <- fevd(fit.OTH,n.ahead=60)
fit.THV <- VAR(Data.THV, 1); fevd.THV <- fevd(fit.THV,n.ahead=60)
fit.CND <- VAR(Data.CND, 1); fevd.CND <- fevd(fit.CND,n.ahead=60)
fit.CNC <- VAR(Data.CNC, 1); fevd.CNC <- fevd(fit.CNC,n.ahead=60)
fit.CTV <- VAR(Data.CTV, 1); fevd.CTV <- fevd(fit.CTV,n.ahead=60)

################### Estimate the Structural VAR Models and Compute Impulse Response Functions ########################
par(mfrow=c(2,3))

res_PTV <- irf(fit.PTV,impulse="Volume_PT", response="Open_PT",  n.ahead=60, cumulative = TRUE, ci=0.9, runs=1000)
res_PTV <- ts(cbind(res_PTV$Lower$Volume_PT,res_PTV$irf$Volume_PT,res_PTV$Upper$Volume_PT), start = c(0))
ts.plot(res_PTV,xlab="Days",main="Volume_PT",lwd=c(2,2,2), lty=c(2,1,2),ylim=c(-0.02,0.04), xlim=c(0,60));abline(h=0)

res_OTH <- irf(fit.OTH,impulse="Open_TH", response="Open_PT",  n.ahead=60, cumulative = TRUE, ci=0.9, runs=1000)
res_OTH <- ts(cbind(res_OTH$Lower$Open_TH,res_OTH$irf$Open_TH,res_OTH$Upper$Open_TH), start = c(0))
ts.plot(res_OTH,xlab="Days",main="Open_TH",lwd=c(2,2,2), lty=c(2,1,2),ylim=c(-0.02,0.04), xlim=c(0,60));abline(h=0)

res_THV <- irf(fit.THV,impulse="Volume_THV", response="Open_PT",  n.ahead=60, cumulative = TRUE, ci=0.9, runs=1000)
res_THV <- ts(cbind(res_THV$Lower$Volume_THV,res_THV$irf$Volume_THV,res_THV$Upper$Volume_THV), start = c(0))
ts.plot(res_THV,xlab="Days",main="Volume_THV",lwd=c(2,2,2), lty=c(2,1,2),ylim=c(-0.02,0.04), xlim=c(0,60));abline(h=0)

res_CND <- irf(fit.CND,impulse="COVID_ND", response="Open_PT",  n.ahead=60, cumulative = TRUE, ci=0.9, runs=1000)
res_CND <- ts(cbind(res_CND$Lower$COVID_ND,res_CND$irf$COVID_ND,res_CND$Upper$COVID_ND), start = c(0))
ts.plot(res_CND,xlab="Days",main="COVID_ND",lwd=c(2,2,2), lty=c(2,1,2),ylim=c(-0.02,0.04), xlim=c(0,60));abline(h=0)

res_CNC <- irf(fit.CNC,impulse="COVID_NC", response="Open_PT",  n.ahead=60, cumulative = TRUE, ci=0.9, runs=1000)
res_CNC <- ts(cbind(res_CNC$Lower$COVID_NC,res_CNC$irf$COVID_NC,res_CNC$Upper$COVID_NC), start = c(0))
ts.plot(res_CNC,xlab="Days",main="COVID_NC",lwd=c(2,2,2), lty=c(2,1,2),ylim=c(-0.02,0.04), xlim=c(0,60));abline(h=0)

res_CTV <- irf(fit.CTV,impulse="COVID_TV", response="Open_PT",  n.ahead=60, cumulative = TRUE, ci=0.9, runs=1000)
res_CTV <- ts(cbind(res_CTV$Lower$COVID_TV,res_CTV$irf$COVID_TV,res_CTV$Upper$COVID_TV), start = c(0))
ts.plot(res_CTV,xlab="Days",main="COVID_TV",lwd=c(2,2,2), lty=c(2,1,2),ylim=c(-0.02,0.04), xlim=c(0,60));abline(h=0)


###################### Now combine the forecast error variance decompositions estimated above #####################
allfevd <- cbind(
                fevd.PTV$Open_PT[,"Volume_PT"], 
                fevd.OTH$Open_PT[,"Open_TH"],
                fevd.THV$Open_PT[,"Volume_THV"],
                fevd.CND$Open_PT[,"COVID_ND"],
                fevd.CNC$Open_PT[,"COVID_NC"],
                fevd.CTV$Open_PT[,"COVID_TV"])

res_CTV
res_CNC
res_CND


write.csv(allfevd, file="fevd_linear.csv")
Summery(fit)

