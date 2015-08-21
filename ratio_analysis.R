# This is the analysis of the ratio spent in the fused and single
# eye state


setwd("/Users/josic/Dropbox/multistableRivalry/GroupedData/groupedRawData(saturation0.9 &0.4)/Kreso's code")

source("ModelDefinitionT.R")

data.ratios <- read.csv('ratios_grouped_over_groupedNsingle.csv')

names(data.ratios)<- c("ID","contrast","ratio")

# We analyze the data by first doing individual fits
# A linear model and Bayesian model per subject 

resultslm<-matrix(NA,nrow=9,ncol=3)
colnames(resultslm)<-c("mean","SE","p-value")
results.Bayes<-matrix(NA,nrow=9,ncol=3)
colnames(results.Bayes)<-c("mean","loCI","upCI")
diffmeans <- c(1:9)
prob = c(1:9)

i = 1

for( j in unique(data.ratios$ID)) {
  
  data1 <- data.ratios[data.ratios$ID == j, ]
  y1.low <- data1[data1$contrast == 0.4,]
  y1.high <- data1[data1$contrast == 0.9,]
  
  y1 = c( y1.low$ratio , y1.high$ratio ) # combine data into one vector
  x1 = c( rep(0,length(y1.low$ratio)) , rep(1,length(y1.high$ratio)) ) # create group membership code
 
  fit = lm(y1~x1)
  
  resultslm[i,] = c(summary(fit)$coefficients[2,1],summary(fit)$coefficients[2,2],summary(fit)$coefficients[2,4])
  
  combined = data.frame(x1,y1)
  mcmcCoda = genMCMC( data=combined , xName="x1" , yName="y1" ,numSavedSteps=50000 , saveName=fileNameRoot )
  results.Bayes[i,]<-c(summary(mcmcCoda)$stat[2,1],summary(mcmcCoda)$quant[2,c(1,5)])
  
  # get the results of MCMC run, and compute probability that
  # the slope (beta1) is positive
  mcmcMat = as.matrix(mcmcCoda,chains=TRUE)
  prob[i] = sum(mcmcMat[,3]>0)/length(mcmcMat[,3])
  
  diffmeans[i] = mean(y1.low$ratio) - mean(y1.high$ratio)
  
  i = i+1
  
}
# Box plots for the ratios


par(mfrow=c(3,3))
for(i in 1:9){
  boxplot(ratio~contrast,data=data.ratios[data.ratios$ID==i,])
}