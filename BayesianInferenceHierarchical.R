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


source("HierarchicalModelNorm.R")



#Analyze only fused state
data1 <- data[ data$Tdata.perc > 0 & (data$Tdata.perc == 3 |  data$Tdata.perc == 4), ]
combined.data.fused = data.frame(data1$IDs,as.numeric(data1$Sat == 0.9),data1$Tdata.duration/rescale.f)
names(combined.data.fused)<- c("ID","saturation","duration")
xName = "saturation"
yName = "duration"
sName = "ID"


# Run the model 
mcmcCoda = genMCMC.hierarchical( data=combined.data.fused , xName=xName , yName=yName , sName=sName ,
                                 numSavedSteps=50000 , thinSteps=15 , saveName=fileNameRoot )

fit1<-lmer(duration~saturation+(saturation|ID),data=combined.data.fused)
summary(fit1)

data2<-combined.data.fused
data2$sat.cen<-data2$saturation-.5
fit2<-lmer(duration~sat.cen+(sat.cen|ID),data=data2)
summary(fit2)



data.id<-combined.data.fused[combined.data.fused$duration < 2.0, ]
data.id$subid<-as.numeric(factor(data.id$ID))

par(mfrow=c(3,3))
for(i in 1:9){
  boxplot(duration~saturation,data=data.id[data.id$subid==i,])
}