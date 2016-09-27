library(car)
library(sandwich)
library(tseries)
library(vars)
library(xtable)
library(CADFtest)

# We load our data, which contains 12 columns 
# The first is the date.
# the second is the clean energy price index (ECO)
# the third is the technology stock index (PSE)
# the fourth is the price of oil (OIL)
# the fifth is the interest rate data (INTEREST)
# the sixth is the S&P 500 index
# the rest six is a breakdown of alternative energy.

energydata <- read.csv("ChengFedermanData.csv")
energydata <- energydata[c(1:335), ]

#######################################
## Create Graph One
#######################################

normalized <- energydata
for (i in 2:12) {
normalized[,i] <- (energydata[,i]/energydata[1,i])*100
}
normalized <- normalized[,-1]
normalized <- normalized[,-4]
normalized <- normalized[,-4]

# Compute the largest y value used in the data 
max_y <- max(normalized)
# Define colors to be used for each column
plot_colors <- c("blue","red","forestgreen","antiquewhite3","aquamarine",
  "black","blueviolet","chartreuse","darkgoldenrod")

# Graph ECO using y axis that ranges from 0 to max_y.
# Turn off axes and annotations (axis labels) so we can 
# specify them ourself
plot(normalized[,1], col=plot_colors[1], type="b", ylim=c(0,max_y), 
  axes=FALSE, ann=FALSE)
#Here we add the horizontal axis labels 
axis(1, lab = F)
text(axTicks(1), par("usr")[3] - 2, srt=90, adj=1,
  labels=c("1/3/2001  ", "1/3/2002  ", "1/3/2003  ", "1/3/2004  ", 
  "1/3/2005  ", "1/3/2006  ", "1/3/2007  "), xpd=T, cex=0.8)
# Make y axis with horizontal labels that display ticks at 
# every 4 marks. 4*0:max_y is equivalent to c(0,4,8,12).
axis(2, las=1, at=50*0:max_y)
# Create box around plot
box()
# Graph the rest lines
for (i in 9:2) {
  lines(normalized[,i], pch=22, lty=2, type="b", col=plot_colors[i])
}

# Create a title with a black, bold/italic font
title(main="Prices of Indices", col.main="black", font.main=2)

# Create a legend at (1, max_y) that is slightly smaller 
# (cex) and uses the same line colors and points used by 
# the actual plots
legend(1, max_y, names(normalized), cex=0.8, col=plot_colors, 
   pch=21:23, lty=1:3)

#################################################
## Table0 Correlation
#################################################
Table0 <- matrix(nrow = 2, ncol = 7)
colnames(Table0) <- c("ECO","GREENUTIL","CONVERSION","CLEANFUEL","STORAGE",
  "DELIVERY","HARVEST")
rownames(Table0) <- c("PSE","OIL")

Table0[1,1] <- cor(energydata$PSE,energydata$ECO)
Table0[1,2] <- cor(energydata$PSE,energydata$GREENUTIL)
Table0[1,3] <- cor(energydata$PSE,energydata$CONVERSION)
Table0[1,4] <- cor(energydata$PSE,energydata$CLEANFUEL)
Table0[1,5] <- cor(energydata$PSE,energydata$STORAGE)
Table0[1,6] <- cor(energydata$PSE,energydata$DELIVERY)
Table0[1,7] <- cor(energydata$PSE,energydata$HARVEST)
Table0[2,1] <- cor(energydata$OIL,energydata$ECO)
Table0[2,2] <- cor(energydata$OIL,energydata$GREENUTIL)
Table0[2,3] <- cor(energydata$OIL,energydata$CONVERSION)
Table0[2,4] <- cor(energydata$OIL,energydata$CLEANFUEL)
Table0[2,5] <- cor(energydata$OIL,energydata$STORAGE)
Table0[2,6] <- cor(energydata$OIL,energydata$DELIVERY)
Table0[2,7] <- cor(energydata$OIL,energydata$HARVEST)
# Here we get our TeX code for Table 0.
xtable(Table0, digits=2, caption = "Correlation between alternative energy 
  and oil or technology stock prices")


#################################################
## Descriptive Statistics
#################################################

# Generate continuously compounded returns
return <- 100*log(energydata[2:335,-1]/energydata[1:334,-1])
return <- return[,-4]
Table1 <- matrix(nrow = 11, ncol = 10)
# Generate mean, median, maximum, minimum, standard deviations
Table1[1,] <- apply(return, 2, mean)
Table1[2,] <- apply(return, 2, median)
Table1[3,] <- apply(return, 2, max)
Table1[4,] <- apply(return, 2, min)
Table1[5,] <- apply(return, 2, sd)

# Write functions of skewness and kurtosis
skewness <-  function(x) {
m3 <- mean((x-mean(x))^3)
skew <- m3/(sd(x)^3)
return(skew)
}
kurtosis <- function(x) {  
m4 <- mean((x-mean(x))^4) 
kurt <- m4/(mean((x-mean(x))^2)^2)  
return(kurt)
}
Table1[6,] <- apply(return, 2, skewness)
Table1[7,] <- apply(return, 2, kurtosis)
# Jarque-Bera
jbtest <- function(x) {
n <- length(x)
jbstat <- n /6 * (skewness(x)^2+(kurtosis(x)-3)^2/4)
return(jbstat)
}
Table1[8,] <- apply(return, 2, jbtest)
# probability
prob1 <- function(x) {
prob <- 1-pchisq(jbtest(x), df = 2)
return(prob)
} 
Table1[9,] <- apply(return, 2, prob1)
# Number
Table1[10,] <- rep(nrow(return),10)
## Sharpe Ratio
sr <- function(x){
  sratio <- (mean(x)*52-mean(energydata$INTEREST))/sd(x)/sqrt(52)
  return(sratio)
}
Table1[11,] <- apply(return, 2, sr)
colnames(Table1) <- c("ECO","PSE","OIL","SP500","GREENUTIL","CONVERSION",
  "CLEANFUEL","STORAGE","DELIVERY","HARVEST")
rownames(Table1) <- c("Mean","Median","Maximum","Minimum","S.D.",
  "Skewness","Kurtosis","Jarque-Bera","Probability","Nobs","Sharpe ratio")

# Here we get our TeX code for Table 1.
xtable(Table1, digits=2, caption = "Summary statistics for weekly returns")

# Now calculate annual returns.
mean(return)*52
mean(energydata$INTEREST)

#####################################################
## Multi-factor Regression
#####################################################

## Write a function that returns heteskedasticity and autocorrelation
## consistent standard deviation. The NeweyWest() does not return the
## same results as STATA does, so with this new function, we are
## able to replicate the original answers better.

hac <- function(X, y, m) {
  # define a function that takes data and generate OLS estimates
  n <- length(X[,1])
    # n is the number of observations
  k <- length(X[1,])+1
    # k is the number of independent variables
  X <- cbind(rep(1, n), X)
    # include a constant column
  beta_hat <- solve(t(X)%*%X, t(X)%*%y)
    # calculate the coefficients (first coefficient is for the constant)
  e <- y-X%*%beta_hat
  c <- matrix(rep(0,k^2), k, k)
  for (i in 1:n) {
    c <- c + e[i,]*e[i,]* (as.matrix(X[i,])%*%X[i,])
  }
  xsigma0x <- c*n/(n-k)
  a <- matrix(rep(0,k^2), k, k)
  for (l in 1:m) {
    b <- matrix(rep(0,k^2), k, k)
    t <- l+1
    for (i in t:n) {
      b <- b + (as.matrix(X[i,])%*%X[i-l,]+as.matrix(X[i-l,])%*%X[i,])*e[i,]*e[i-l,]
    }
    a <- a+b*(m+1-l)/(m+1)
  }
  varest <- xsigma0x + a*n/(n-k)
  I <- diag(rep(1,k))
  X_inv <- solve(t(X)%*%X, I)
  var <- X_inv %*% varest %*% X_inv
  beta_se <- sqrt(diag(var))
  tstat <- beta_hat / beta_se
  return(beta_hat, beta_se, tstat)
}

# column 1: ECO
eco_s = return$ECO/100
sp_s = return$SNP/100
oil_s = return$OIL/100
ir_s=log(energydata$INTEREST[2:335]/energydata$INTEREST[1:334])
  ## Since we got data not from the authors, but from the websites,
  ## we're not yet sure about how the authors transformed the interest 
  ## rate data here. We tried several transforms (take log, divided by 
  ## 52 since the original number is annualized, take difference, etc.), 
  ## and this one gives us the closest replication and still makes sense.
xdata <- cbind(sp_s, oil_s, ir_s)
hac(xdata, eco_s, 5)
## Adjusted R-square
result1 <- lm(eco_s ~ xdata)
output1 <- summary(result1)
adjR2_1 <- 1-(1-output1$r.squared)*(334-1)/(334-1-1)
adjR2_1
## DW test
dwtest(result1)
## F-stat p-value
1 - pf(output1$fstatistic[1], output1$fstatistic[2], output1$fstatistic[3])

# column 2: PSE
pse_s = return$PSE/100
hac(xdata, pse_s, 5)
## Adjusted R-square
result2 <- lm(pse_s ~ xdata)
output2 <- summary(result2)
adjR2_2 <- 1-(1-output2$r.squared)*(334-1)/(334-1-1)
adjR2_2
## DW test
dwtest(result2)
## F-stat p-value
1 - pf(output2$fstatistic[1], output2$fstatistic[2], output2$fstatistic[3])

# column 3: GREENUTIL
greenutil_s = return$GREENUTIL/100
hac(xdata, greenutil_s, 5)
## Adjusted R-square
result3 <- lm(greenutil_s ~ xdata)
output3 <- summary(result3)
adjR2_3 <- 1-(1-output3$r.squared)*(334-1)/(334-1-1)
adjR2_3
## DW test
dwtest(result3)
## F-stat p-value
1 - pf(output3$fstatistic[1], output3$fstatistic[2], output3$fstatistic[3])

# column 4: CONVERSION
conversion_s = return$CONVERSION/100
hac(xdata, conversion_s, 5)
## Adjusted R-square
result4 <- lm(conversion_s ~ xdata)
output4 <- summary(result4)
adjR2_4 <- 1-(1-output4$r.squared)*(334-1)/(334-1-1)
adjR2_4
## DW test
dwtest(result4)
## F-stat p-value
1 - pf(output4$fstatistic[1], output4$fstatistic[2], output4$fstatistic[3])

# column 5: CLEANFUEL
cleanfuel_s = return$CLEANFUEL/100
hac(xdata, cleanfuel_s, 5)
## Adjusted R-square
result5 <- lm(cleanfuel_s ~ xdata)
output5 <- summary(result5)
adjR2_5 <- 1-(1-output5$r.squared)*(334-1)/(334-1-1)
adjR2_5
## DW test
dwtest(result5)
## F-stat p-value
1 - pf(output5$fstatistic[1], output5$fstatistic[2], output5$fstatistic[3])

# column 6: STORAGE
storage_s = return$STORAGE/100
hac(xdata, storage_s, 5)
## Adjusted R-square
result6 <- lm(storage_s ~ xdata)
output6 <- summary(result6)
adjR2_6 <- 1-(1-output6$r.squared)*(334-1)/(334-1-1)
adjR2_6
## DW test
dwtest(result6)
## F-stat p-value
1 - pf(output6$fstatistic[1], output6$fstatistic[2], output6$fstatistic[3])

# column 7: DELIVERY
delivery_s = return$DELIVERY/100
hac(xdata, delivery_s, 5)
## Adjusted R-square
result7 <- lm(delivery_s ~ xdata)
output7 <- summary(result7)
adjR2_7 <- 1-(1-output7$r.squared)*(334-1)/(334-1-1)
adjR2_7
## DW test
dwtest(result7)
## F-stat p-value
1 - pf(output7$fstatistic[1], output7$fstatistic[2], output7$fstatistic[3])

# column 8: HARVEST
harvest_s = return$HARVEST/100
hac(xdata, harvest_s, 5)
## Adjusted R-square
result8 <- lm(harvest_s ~ xdata)
output8 <- summary(result8)
adjR2_8 <- 1-(1-output8$r.squared)*(334-1)/(334-1-1)
adjR2_8
## DW test
dwtest(result8)
## F-stat p-value
1 - pf(output8$fstatistic[1], output8$fstatistic[2], output8$fstatistic[3])

## Critical values for t distribution with df=333.
qt(p=.99, df=333); qt(p=.95, df=333);qt(p=.90, df=333);
[1] 2.337598
[1] 1.649442
[1] 1.284099

#######################################################
#Unit Root Test
#######################################################

#Here we take the log of each data series:
logdata <- log(energydata[,-1])

###################################
## ADF TEST
###################################

## First, to choose the right number of lags, we use CADFtest. 
CADFtest(logdata[,1], type="drift", max.lag.y=10, criterion="BIC")
CADFtest(logdata[,2], type="drift", max.lag.y=10, criterion="BIC")
CADFtest(logdata[,3], type="drift", max.lag.y=10, criterion="BIC")
CADFtest(logdata[,4], type="drift", max.lag.y=10, criterion="BIC")
CADFtest(logdata[,6], type="drift", max.lag.y=10, criterion="BIC")
CADFtest(logdata[,7], type="drift", max.lag.y=10, criterion="BIC")
CADFtest(logdata[,8], type="drift", max.lag.y=10, criterion="BIC")
CADFtest(logdata[,9], type="drift", max.lag.y=10, criterion="BIC")
CADFtest(logdata[,10], type="drift", max.lag.y=10, criterion="BIC")
CADFtest(logdata[,11], type="drift", max.lag.y=10, criterion="BIC")

df_eco <- ur.df(logdata$ECO, type=("drift"), lag = 0)
summary(df_eco)
  ## The bottom lists the critical values, so that we can know whether the statistic is significant or not.
df_greenutil <- ur.df(logdata$GREENUTIL, type=("drift"), lag = 0)
summary(df_greenutil)
df_conversion <- ur.df(logdata$CONVERSION, type=("drift"), lag = 0)
summary(df_conversion)
df_cleanfuel <- ur.df(logdata$CLEANFUEL, type=("drift"), lag = 0)
summary(df_cleanfuel)
df_storage <- ur.df(logdata$STORAGE, type=("drift"), lag = 0)
summary(df_storage)
df_delivery <- ur.df(logdata$DELIVERY, type=("drift"), lag = 0)
summary(df_delivery)
df_harvest <- ur.df(logdata$HARVEST, type=("drift"), lag = 0)
summary(df_harvest)

# FIRST-DIFFERENCE
fd <- logdata[2:335,]-logdata[1:334,]
df_ecod <- ur.df(fd$ECO, type=("drift"), lag = 0)
summary(df_ecod)
df_greenutild <- ur.df(fd$GREENUTIL, type=("drift"), lag = 0)
summary(df_greenutild)
df_conversiond <- ur.df(fd$CONVERSION, type=("drift"), lag = 0)
summary(df_conversiond)
df_cleanfueld <- ur.df(fd$CLEANFUEL, type=("drift"), lag = 0)
summary(df_cleanfueld)
df_storaged <- ur.df(fd$STORAGE, type=("drift"), lag = 0)
summary(df_storaged)
df_deliveryd <- ur.df(fd$DELIVERY, type=("drift"), lag = 0)
summary(df_deliveryd)
df_harvestd <- ur.df(fd$HARVEST, type=("drift"), lag = 0)
summary(df_harvestd)

#############################
## PP-TEST
#############################
pp_eco <- list()
for (i in 1:15) {
  pp_eco[i] <- ur.pp(logdata$ECO, type="Z-tau", model="constant", use.lag=i)
}
pp_gre <- list()
for (i in 1:15) {
  pp_gre[i] <- ur.pp(logdata$GREENUTIL, type="Z-tau", model="constant", use.lag=i)
}
pp_con <- list()
for (i in 1:15) {
  pp_con[i] <- ur.pp(logdata$CONVERSION, type="Z-tau", model="constant", use.lag=i)
}
pp_cle <- list()
for (i in 1:15) {
  pp_cle[i] <- ur.pp(logdata$CLEANFUEL, type="Z-tau", model="constant", use.lag=i)
}
pp_sto <- list()
for (i in 1:15) {
  pp_sto[i] <- ur.pp(logdata$STORAGE, type="Z-tau", model="constant", use.lag=i)
}
pp_del <- list()
for (i in 1:15) {
  pp_del[i] <- ur.pp(logdata$DELIVERY, type="Z-tau", model="constant", use.lag=i)
}
pp_har <- list()
for (i in 1:15) {
  pp_har[i] <- ur.pp(logdata$HARVEST, type="Z-tau", model="constant", use.lag=i)
}

## PP TEST for first difference
pp_ecod <- list()
for (i in 1:15) {
  pp_ecod[i] <- ur.pp(fd$ECO, type="Z-tau", model="constant", use.lag=i)
}
pp_gred <- list()
for (i in 1:15) {
  pp_gred[i] <- ur.pp(fd$GREENUTIL, type="Z-tau", model="constant", use.lag=i)
}
pp_cond <- list()
for (i in 1:15) {
  pp_cond[i] <- ur.pp(fd$CONVERSION, type="Z-tau", model="constant", use.lag=i)
}
pp_cled <- list()
for (i in 1:15) {
  pp_cled[i] <- ur.pp(fd$CLEANFUEL, type="Z-tau", model="constant", use.lag=i)
}
pp_stod <- list()
for (i in 1:15) {
  pp_stod[i] <- ur.pp(fd$STORAGE, type="Z-tau", model="constant", use.lag=i)
}
pp_deld <- list()
for (i in 1:15) {
  pp_deld[i] <- ur.pp(fd$DELIVERY, type="Z-tau", model="constant", use.lag=i)
}
pp_hard <- list()
for (i in 1:15) {
  pp_hard[i] <- ur.pp(fd$HARVEST, type="Z-tau", model="constant", use.lag=i)
}

###########################
## KPSS TEST
###########################
kpss_eco <- list()
for (i in 1:15) {
  kpss_eco[i] <- ur.kpss(logdata$ECO, type="mu", use.lag=i)
}
kpss_gre <- list()
for (i in 1:15) {
  kpss_gre[i] <- ur.kpss(logdata$GREENUTIL, type="mu", use.lag=i)
}
kpss_con <- list()
for (i in 1:15) {
  kpss_con[i] <- ur.kpss(logdata$CONVERSION, type="mu", use.lag=i)
}
kpss_cle <- list()
for (i in 1:15) {
  kpss_cle[i] <- ur.kpss(logdata$CLEANFUEL, type="mu", use.lag=i)
}
kpss_sto <- list()
for (i in 1:15) {
  kpss_sto[i] <- ur.kpss(logdata$STORAGE, type="mu", use.lag=i)
}
kpss_del <- list()
for (i in 1:15) {
  kpss_del[i] <- ur.kpss(logdata$DELIVER, type="mu", use.lag=i)
}
kpss_har <- list()
for (i in 1:15) {
  kpss_har[i] <- ur.kpss(logdata$HARVEST, type="mu", use.lag=i)
}
## first differences
kpss_ecod <- list()
for (i in 1:15) {
  kpss_ecod[i] <- ur.kpss(fd$ECO, type="mu", use.lag=i)
}
kpss_gred <- list()
for (i in 1:15) {
  kpss_gred[i] <- ur.kpss(fd$GREENUTIL, type="mu", use.lag=i)
}
kpss_cond <- list()
for (i in 1:15) {
  kpss_cond[i] <- ur.kpss(fd$CONVERSION, type="mu", use.lag=i)
}
kpss_cled <- list()
for (i in 1:15) {
  kpss_cled[i] <- ur.kpss(fd$CLEANFUEL, type="mu", use.lag=i)
}
kpss_stod <- list()
for (i in 1:15) {
  kpss_stod[i] <- ur.kpss(fd$STORAGE, type="mu", use.lag=i)
}
kpss_deld <- list()
for (i in 1:15) {
  kpss_deld[i] <- ur.kpss(fd$DELIVER, type="mu", use.lag=i)
}
kpss_hard <- list()
for (i in 1:15) {
  kpss_hard[i] <- ur.kpss(fd$HARVEST, type="mu", use.lag=i)
}
## second differences
fd2 <- fd[2:334,]-fd[1:333,]
kpss_gredd <- list()
for (i in 1:15) {
  kpss_gredd[i] <- ur.kpss(fd2$GREENUTIL, type="mu", use.lag=i)
}
kpss_stodd <- list()
for (i in 1:15) {
  kpss_stodd[i] <- ur.kpss(fd2$STORAGE, type="mu", use.lag=i)
}
kpss_hardd <- list()
for (i in 1:15) {
  kpss_hardd[i] <- ur.kpss(fd2$HARVEST, type="mu", use.lag=i)
}

#########################################
## VAR Regression
#########################################
ldata <- cbind(logdata$ECO,logdata$PSE,logdata$OIL,logdata$INTEREST)
ldata1 <- cbind(logdata$GREENUTIL,logdata$PSE,logdata$OIL,logdata$INTEREST)
ldata2 <- cbind(logdata$CONVERSION,logdata$PSE,logdata$OIL,logdata$INTEREST)
ldata3 <- cbind(logdata$CLEANFUEL,logdata$PSE,logdata$OIL,logdata$INTEREST)
ldata4 <- cbind(logdata$STORAGE,logdata$PSE,logdata$OIL,logdata$INTEREST)
ldata5 <- cbind(logdata$DELIVERY,logdata$PSE,logdata$OIL,logdata$INTEREST)
ldata6 <- cbind(logdata$HARVEST,logdata$PSE,logdata$OIL,logdata$INTEREST)
myvar <- VAR(ldata, p=10)
myvar1 <- VAR(ldata1, p=11)
myvar2 <- VAR(ldata2, p=10)
myvar3 <- VAR(ldata3, p=11)
myvar4 <- VAR(ldata4, p=10)
myvar5 <- VAR(ldata5, p=10)
myvar6 <- VAR(ldata6, p=10)
output <- summary(myvar)
output1 <- summary(myvar1)
output2 <- summary(myvar2)
output3 <- summary(myvar3)
output4 <- summary(myvar4)
output5 <- summary(myvar5)
output6 <- summary(myvar6)
output; output1; output2; output3; output4; output5; output6;

## mean dependent
mean(logdata[11:335,])
## s.d. dependent
sd(logdata[11:335,])

###################################
## Impulse Response Function
###################################
myirf_pse <- irf(myvar, impulse="y2", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_pse$irf[[1]] <- myirf_pse$irf[[1]] * 0.039
myirf_pse$Upper[[1]] <- myirf_pse$Upper[[1]] * 0.039
myirf_pse$Lower[[1]] <- myirf_pse$Lower[[1]] * 0.039
plot(myirf_pse,main="log(Alternative Energy) Response to log(Tech)", xlim=c(1,10), xlab="week", 
  ylab="log of Alternative Energy (LECO)")

myirf_oil <- irf(myvar, impulse="y3", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_oil$irf[[1]] <- myirf_oil$irf[[1]] * 0.039
myirf_oil$Upper[[1]] <- myirf_oil$Upper[[1]] * 0.039
myirf_oil$Lower[[1]] <- myirf_oil$Lower[[1]] * 0.039
plot(myirf_oil,main="log(Alternative Energy) Response to log(Oil)", xlim=c(1,10), xlab="week", 
  ylab="log of Alternative Energy (LECO)")

myirf_pse1 <- irf(myvar1, impulse="y2", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_pse1$irf[[1]] <- myirf_pse1$irf[[1]] * 0.039
myirf_pse1$Upper[[1]] <- myirf_pse1$Upper[[1]] * 0.039
myirf_pse1$Lower[[1]] <- myirf_pse1$Lower[[1]] * 0.039
plot(myirf_pse1,main="log(Greener Utilities) Response to log(Tech)", xlim=c(1,10), xlab="week", 
  ylab="log of Greener Utilities")

myirf_oil1 <- irf(myvar1, impulse="y3", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_oil1$irf[[1]] <- myirf_oil1$irf[[1]] * 0.039
myirf_oil1$Upper[[1]] <- myirf_oil1$Upper[[1]] * 0.039
myirf_oil1$Lower[[1]] <- myirf_oil1$Lower[[1]] * 0.039
plot(myirf_oil1,main="log(Greener Utilities) Response to log(Oil)", xlim=c(1,10), xlab="week", 
  ylab="log of Greener Utilities")

myirf_pse2 <- irf(myvar2, impulse="y2", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_pse2$irf[[1]] <- myirf_pse2$irf[[1]] * 0.06
myirf_pse2$Upper[[1]] <- myirf_pse2$Upper[[1]] * 0.06
myirf_pse2$Lower[[1]] <- myirf_pse2$Lower[[1]] * 0.06
plot(myirf_pse2,main="log(Energy Conversion) Response to log(Tech)", xlim=c(1,10), xlab="week", 
  ylab="log of Energy Conversion")

myirf_oil2 <- irf(myvar2, impulse="y3", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_oil2$irf[[1]] <- myirf_oil2$irf[[1]] * 0.06
myirf_oil2$Upper[[1]] <- myirf_oil2$Upper[[1]] * 0.06
myirf_oil2$Lower[[1]] <- myirf_oil2$Lower[[1]] * 0.06
plot(myirf_oil2,main="log(Energy Conversion) Response to log(Oil)", xlim=c(1,10), xlab="week", 
  ylab="log of Energy Conversion")

myirf_pse3 <- irf(myvar3, impulse="y2", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_pse3$irf[[1]] <- myirf_pse3$irf[[1]] * 0.029
myirf_pse3$Upper[[1]] <- myirf_pse3$Upper[[1]] * 0.029
myirf_pse3$Lower[[1]] <- myirf_pse3$Lower[[1]] * 0.029
plot(myirf_pse3,main="log(Cleaner Fuels) Response to log(Tech)", xlim=c(1,10), xlab="week", 
  ylab="log of Cleaner Fuels")

myirf_oil3 <- irf(myvar3, impulse="y3", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_oil3$irf[[1]] <- myirf_oil3$irf[[1]] * 0.029
myirf_oil3$Upper[[1]] <- myirf_oil3$Upper[[1]] * 0.029
myirf_oil3$Lower[[1]] <- myirf_oil3$Lower[[1]] * 0.029
plot(myirf_oil3,main="log(Cleaner Fuels) Response to log(Oil)", xlim=c(1,10), xlab="week", 
  ylab="log of Cleaner Fuels")

myirf_pse4 <- irf(myvar4, impulse="y2", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_pse4$irf[[1]] <- myirf_pse4$irf[[1]] * 0.045
myirf_pse4$Upper[[1]] <- myirf_pse4$Upper[[1]] * 0.045
myirf_pse4$Lower[[1]] <- myirf_pse4$Lower[[1]] * 0.045
plot(myirf_pse4,main="log(Energy Storage) Response to log(Tech)", xlim=c(1,10), xlab="week", 
  ylab="log of Energy Storage")

myirf_oil4 <- irf(myvar4, impulse="y3", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_oil4$irf[[1]] <- myirf_oil4$irf[[1]] * 0.045
myirf_oil4$Upper[[1]] <- myirf_oil4$Upper[[1]] * 0.045
myirf_oil4$Lower[[1]] <- myirf_oil4$Lower[[1]] * 0.045
plot(myirf_oil4,main="log(Energy Storage) Response to log(Oil)", xlim=c(1,10), xlab="week", 
  ylab="log of Energy Storage")

myirf_pse5 <- irf(myvar5, impulse="y2", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_pse5$irf[[1]] <- myirf_pse5$irf[[1]] * 0.051
myirf_pse5$Upper[[1]] <- myirf_pse5$Upper[[1]] * 0.051
myirf_pse5$Lower[[1]] <- myirf_pse5$Lower[[1]] * 0.051
plot(myirf_pse5,main="log(Power Delivery) Response to log(Tech)", xlim=c(1,10), xlab="week", 
  ylab="log of Power Delivery")

myirf_oil5 <- irf(myvar5, impulse="y3", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_oil5$irf[[1]] <- myirf_oil5$irf[[1]] * 0.051
myirf_oil5$Upper[[1]] <- myirf_oil5$Upper[[1]] * 0.051
myirf_oil5$Lower[[1]] <- myirf_oil5$Lower[[1]] * 0.051
plot(myirf_oil5,main="log(Power Delivery) Response to log(Oil)", xlim=c(1,10), xlab="week", 
  ylab="log of Power Delivery")

myirf_pse6 <- irf(myvar6, impulse="y2", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_pse6$irf[[1]] <- myirf_pse6$irf[[1]] * 0.067
myirf_pse6$Upper[[1]] <- myirf_pse6$Upper[[1]] * 0.067
myirf_pse6$Lower[[1]] <- myirf_pse6$Lower[[1]] * 0.067
plot(myirf_pse6,main="log(Energy Harvesting) Response to log(Tech)", xlim=c(1,10), xlab="week", 
  ylab="log of Energy Harvesting")

myirf_oil6 <- irf(myvar6, impulse="y3", response="y1", n.ahead=10, 
  ortho=FALSE, ci=0.954)
myirf_oil6$irf[[1]] <- myirf_oil6$irf[[1]] * 0.067
myirf_oil6$Upper[[1]] <- myirf_oil6$Upper[[1]] * 0.067
myirf_oil6$Lower[[1]] <- myirf_oil6$Lower[[1]] * 0.067
plot(myirf_oil6,main="log(Energy Harvesting) Response to log(Oil)", xlim=c(1,10), xlab="week", 
  ylab="log of Energy Harvesting")

