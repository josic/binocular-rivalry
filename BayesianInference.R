# Modified from:
#
# John K. Kruschke  
# johnkruschke@gmail.com
# http://www.indiana.edu/~kruschke/BEST/

setwd("/Users/josic/Dropbox/multistableRivalry/GroupedData/groupedRawData(saturation0.9 &0.4)/Kreso's code")

rescale.f = 1000000

# Load the functions used below:
source("DBDA2E-utilities.R") # Must be in R's current working directory.
require(rjags)               # Must have previously installed package rjags.

# Load the data:
data <- read.csv('dataInOneTable.csv') # Read data file; must be in curr. work. dir.

#Define Matrices for results - PUT OUTSIDE LOOP
results04<-matrix(NA,nrow=9,ncol=3)
colnames(results04)<-c("mean","loCI","upCI")
results09<-matrix(NA,nrow=9,ncol=3)
colnames(results09)<-c("mean","loCI","upCI")
###

# Initialize counter and get the right source
i = 1
source("ModelDefinitionT.R")

for( j in unique(data$IDs)) {

# Pick one subject and conditions
data1 <- subset(data, IDs == j & Tdata.perc > 0 )
y.single <- data1[data1$Sat == 0.4 & (data1$Tdata.perc == 1 |  data1$Tdata.perc == 2),]$Tdata.duration/rescale.f
y.fused <- data1[data1$Sat == 0.4 & (data1$Tdata.perc == 3 |  data1$Tdata.perc == 4),]$Tdata.duration/rescale.f

# For testing take only first N values
#y.single <- y.single[1:N]
#y.fused <- y.fused[1:N]

y = c( y.single , y.fused ) # combine data into one vector
x = c( rep(0,length(y.single)) , rep(1,length(y.fused)) ) # create group membership code
Ntotal = length(y)
combined.data = data.frame(x,y)
xName = "x"
yName = "y"


# Run the model 
mcmcCoda = genMCMC( data=combined.data , xName=xName , yName=yName , 
                    numSavedSteps=50000 , saveName=fileNameRoot )

results04[i,]<-c(summary(mcmcCoda)$stat[2,1],summary(mcmcCoda)$quant[2,c(1,5)])
i = i+1
}

# Now do the same thing for high saturation
i = 1

for( j in unique(data$IDs)) {
  
  # Pick one subject and conditions
  data1 <- subset(data, IDs == j & Tdata.perc > 0 )
  y.single <- data1[data1$Sat == 0.9 & (data1$Tdata.perc == 1 |  data1$Tdata.perc == 2),]$Tdata.duration/rescale.f
  y.fused <- data1[data1$Sat == 0.9 & (data1$Tdata.perc == 3 |  data1$Tdata.perc == 4),]$Tdata.duration/rescale.f
  
  
  
  # For testing take only first N values
  #y.single <- y.single[1:N]
  #y.fused <- y.fused[1:N]
  
  y = c( y.single , y.fused ) # combine data into one vector
  x = c( rep(0,length(y.single)) , rep(1,length(y.fused)) ) # create group membership code
  Ntotal = length(y)
  combined.data = data.frame(x,y)
  xName = "x"
  yName = "y"
  
  # Run the mdoel
  mcmcCoda = genMCMC( data=combined.data , xName=xName , yName=yName , 
                      numSavedSteps=50000 , saveName=fileNameRoot )

  results09[i,]<-c(summary(mcmcCoda)$stat[2,1],summary(mcmcCoda)$quant[2,c(1,5)])
  i = i+1
}

# A straigthforward linear model comparison just to check
# Also, just a difference between means

results04lm<-matrix(NA,nrow=9,ncol=3)
colnames(results04)<-c("mean","SE","p-value")
results09lm<-matrix(NA,nrow=9,ncol=3)
colnames(results09)<-c("mean","SE","p-value")
diffmeans04 <- c(1:9)
diffmeans09 <- c(1:9)

i = 1

for( j in unique(data$IDs)) {

  data1 <- subset(data, IDs == j & Tdata.perc > 0 )
  y1.single <- data1[data1$Sat == 0.4 & (data1$Tdata.perc == 1 |  data1$Tdata.perc == 2),]$Tdata.duration/rescale.f
  y1.fused <- data1[data1$Sat == 0.4 & (data1$Tdata.perc == 3 |  data1$Tdata.perc == 4),]$Tdata.duration/rescale.f
  
  y1 = c( y1.single , y1.fused ) # combine data into one vector
  x1 = c( rep(0,length(y1.single)) , rep(1,length(y1.fused)) ) # create group membership code
  
  data1 <- subset(data, IDs == j & Tdata.perc > 0 )
  y2.single <- data1[data1$Sat == 0.9 & (data1$Tdata.perc == 1 |  data1$Tdata.perc == 2),]$Tdata.duration/rescale.f
  y2.fused <- data1[data1$Sat == 0.9 & (data1$Tdata.perc == 3 |  data1$Tdata.perc == 4),]$Tdata.duration/rescale.f
  
  y2 = c( y2.single , y2.fused ) # combine data into one vector
  x2 = c( rep(0,length(y2.single)) , rep(1,length(y2.fused)) )
  
  fit04 = lm(y1~x1)
  fit09 = lm(y2~x2)
  
  results04lm[i,] = c(summary(fit04)$coefficients[2,1],summary(fit04)$coefficients[2,2],summary(fit04)$coefficients[2,4])
  results09lm[i,] = c(summary(fit09)$coefficients[2,1],summary(fit09)$coefficients[2,2],summary(fit09)$coefficients[2,4])
  
  diffmeans04[i] = mean(y1.fused) - mean(y1.single)
  diffmeans09[i] = mean(y2.fused) - mean(y2.single)
  
  i = i+1
  
}

# A comparison of fused and single eye  across conditions, not within as above

# Source for Bayesian model
source("ModelDefinitionT.R")

results.singlelm<-matrix(NA,nrow=9,ncol=3)
colnames(results.singlelm)<-c("mean","SE","p-value")
results.fusedlm<-matrix(NA,nrow=9,ncol=3)
colnames(results.fusedlm)<-c("mean","SE","p-value")
diffmeans.single <- c(1:9)
diffmeans.fused <- c(1:9)
results.singleBayes<-matrix(NA,nrow=9,ncol=3)
colnames(results.singleBayes)<-c("mean","loCI","upCI")
results.fusedBayes<-matrix(NA,nrow=9,ncol=3)
colnames(results.fusedBayes)<-c("mean","loCI","upCI")
mean.low = c(1:9)
mean.high = c(1:9)

i = 1

for( j in unique(data$IDs)) {
  
  data1 <- subset(data, IDs == j & Tdata.perc > 0 )
  y1s.low <- data1[data1$Sat == 0.4 & (data1$Tdata.perc == 1 |  data1$Tdata.perc == 2),]$Tdata.duration/rescale.f
  y1s.high <- data1[data1$Sat == 0.9 & (data1$Tdata.perc == 1 |  data1$Tdata.perc == 2),]$Tdata.duration/rescale.f
  
  y1s = c( y1s.low , y1s.high ) # combine data into one vector
  x1s = c( rep(0,length(y1s.low)) , rep(1,length(y1s.high)) ) # create group membership code
  
  data1 <- subset(data, IDs == j & Tdata.perc > 0 )
  y1f.low <- data1[data1$Sat == 0.4 & (data1$Tdata.perc == 3 |  data1$Tdata.perc == 4),]$Tdata.duration/rescale.f
  y1f.high <- data1[data1$Sat == 0.9 & (data1$Tdata.perc == 3 |  data1$Tdata.perc == 4),]$Tdata.duration/rescale.f
  
  y1f = c( y1f.low , y1f.high ) # combine data into one vector
  x1f = c( rep(0,length(y1f.low)) , rep(1,length(y1f.high)) ) # create group membership code
  
  # fit linear model and save results
  fit.s = lm(y1s~x1s)
  fit.f = lm(y1f~x1f)
  results.singlelm[i,] = c(summary(fit.s)$coefficients[2,1],summary(fit.s)$coefficients[2,2],summary(fit.s)$coefficients[2,4])
  results.fusedlm[i,] = c(summary(fit.f)$coefficients[2,1],summary(fit.f)$coefficients[2,2],summary(fit.s)$coefficients[2,4])
  
  #fit Bayesian model - comment out to run only linear model
  combined.single = data.frame(x1s,y1s)
  combined.fused = data.frame(x1f,y1f)
  mcmcCoda.single = genMCMC( data=combined.single , xName="x1s" , yName="y1s" ,numSavedSteps=20000 , saveName=fileNameRoot )
  mcmcCoda.fused = genMCMC( data=combined.fused , xName="x1f" , yName="y1f" ,numSavedSteps=20000 , saveName=fileNameRoot )
  results.singleBayes[i,]<-c(summary(mcmcCoda.single)$stat[2,1],summary(mcmcCoda.single)$quant[2,c(1,5)])
  results.fusedBayes[i,]<-c(summary(mcmcCoda.fused)$stat[2,1],summary(mcmcCoda.fused)$quant[2,c(1,5)])
#   
  
  
#   mean.low[i] =  mean(y1s.low)
#   mean.high[i] = mean(y1s.high)
  diffmeans.single[i] = mean(y1s.low) - mean(y1s.high)
  diffmeans.fused[i] = mean(y1f.low) - mean(y1f.high)
  
  i = i+1
  
}


# PLOTS FOR DIFFERENCES WITHIN CONDITIONS

# Simple plot of results

#par(mfrow = c(2,1))

plot(results04[,1], type="o", col="blue", ylab = "Diff btw fused & single")
lines(results09[,1], type="o", pch=22, lty=2, col="red")
legend(2,35, # places a legend at the appropriate place 
       c("0.4","0.9"), # puts text in the legend       
       lty=c(1,1), # gives the legend appropriate symbols (lines)      
       lwd=c(2.5,2.5),col=c("blue","red"))

# Uncomment to add error bars
# epsilon = 0.1
# for(i in 1:9) {
#   up = results09[i,1] + results09[i,3]
#   low = results09[i,1] - results09[i,3]
#   segments( i ,low , i, up, col="red")
#   segments(i-epsilon, up , i+epsilon, up, , col="red")
#   segments(i-epsilon, low , i+epsilon, low, , col="red")
#   up = results04[i,1] + results04[i,3]
#   low = results04[i,1] - results04[i,3]
#   segments( i ,low , i, up, col="blue")
#   segments(i-epsilon, up , i+epsilon, up, , col="blue")
#   segments(i-epsilon, low , i+epsilon, low, , col="blue")
# }

# Plot results of linear model

plot(results04lm[,1], type="o", col="blue", ylab = "Diff btw fused & single")
lines(results09lm[,1], type="o", pch=22, lty=2, col="red")

# Plot just the difference in the means

plot(mean.high, type="o", col="blue", ylab = "Mean of single in high/low")
lines(mean.low, type="o", pch=22, lty=2, col="red")

plot(diffmeans04, type="o", col="blue", ylab = "Diff btw fused & single")
lines(diffmeans09, type="o", pch=22, lty=2, col="red")


# PLOTS FOR DIFFERENCE ACROSS CONDITIONS

# Plot results of linear model

par(mfrow=c(2,1))

plot(results.singlelm[,1], type="o", col="blue", ylab = "Diff across conditions")
lines(results.fusedlm[,1], type="o", pch=22, lty=2, col="red")
epsilon = 0.1
for(i in 1:9) {
  up = results.singlelm[i,1] + results.singlelm[i,3]
  low = results.singlelm[i,1] - results.singlelm[i,3]
  segments( i ,low , i, up, col="red")
  segments(i-epsilon, up , i+epsilon, up, , col="red")
  segments(i-epsilon, low , i+epsilon, low, , col="red")
#   up = results04[i,1] + results04[i,3]
#   low = results04[i,1] - results04[i,3]
#   segments( i ,low , i, up, col="blue")
#   segments(i-epsilon, up , i+epsilon, up, , col="blue")
#   segments(i-epsilon, low , i+epsilon, low, , col="blue")
}

# Plot just the difference in the means

plot(diffmeans.single, type="o", col="blue", ylab = "Diff across conditions")
lines(diffmeans.fused, type="o", pch=22, lty=2, col="red")


# Diagnostics from Kruschke's example files

parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}

summaryInfo = smryMCMC( mcmcCoda , 
                        compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
                        saveName=fileNameRoot )
show(summaryInfo)

plotMCMC( mcmcCoda , data=combined.data , xName=xName , yName=yName , 
          compValBeta1=0.0 , ropeBeta1=c(-0.5,0.5) ,
          pairsPlot=TRUE , showCurve=FALSE)