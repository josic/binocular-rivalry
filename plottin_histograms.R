
# load the data in the csv file
data <- read.csv('dataInOneTable.csv')

# Make a plot of the dominance distribution for
# the single eye, or two eye state for subject AB

# restrict subject and only active percepts
# You can run this with different subjects
data1 <- subset(data, IDs == "AJ" & Tdata.perc > 0 )

# List of all 4 conditions
conditions <- levels(data1$Conds)
saturation <- levels(data1$Sat)

# Cycle over all conditions and make plot of duration for each 1
# Note that we are separating by percept (fused or single eye)
# and by saturation level

par(mfrow=c(2,2))
for (i in c(0.4,0.9)){
  toplot <- subset(data1, data1$Sat == i & (data1$Tdata.perc == 1 |  data1$Tdata.perc == 2))
  #Get it to the right timescale in seconds
  toplot <- toplot$Tdata.duration/100000
  #mean and standard deviation
  m = mean(toplot)
  sig = sd(toplot)
  hist(toplot, breaks=20, freq = FALSE, col = "blue",xlab="Duration of percept", 
       main= paste("Single eye and sat", i), sub = paste("mean=", round(m,2)))         
  # shape and scale
  a = (m/sig)^2; s = (sig)^2/m
  curve(dgamma(x, a,  scale=s), add=TRUE, col='darkblue', lwd=2)
  #Do it again for the fused percepts
  toplot <- subset(data1, data1$Sat == i & (data1$Tdata.perc == 3 |  data1$Tdata.perc == 4))
  toplot <- toplot$Tdata.duration/100000
  m = mean(toplot)
  sig = sd(toplot) 
  hist(toplot, breaks=20, freq = FALSE, col = "blue",xlab="Duration of percept", 
       main= paste("Fused and sat", i),  sub = paste("mean=", round(m,2)))        
  # shape and scale
  a =  (m/sig)^2; s =  (sig)^2/m
  curve(dgamma(x, a,  scale=s), add=TRUE, col='darkblue', lwd=2)
}

# Next show a histogram of the number of visits to each state
par(mfrow=c(1,2))
for (i in c(0.4,0.9)){
  toplot <- subset(data1, data1$Sat == i )
  toplot <- toplot$Tdata.perc
  hist(toplot, col = "blue",xlab="percept number", 
       main= paste("Single eye and sat", i), sub = paste("Total=", sum(toplot>0)))         
}




# SUPPLEMENTARY FIGURE POSSIBILITY - This is the same figure as above, but 
# with the whole vs fused state

par(mfrow=c(2,1))
for (i in c(0.4,0.9)){
  # First create a talbe of all percepts
  data.test <- subset(data, data$Sat == i )
  bar.dat <- table(data.test$Tdata.perc,data.test$IDs)
  # Now create smaller table of what we want
  bar.datnew <- matrix(nrow = 2, ncol = 9)
  colnames(bar.datnew) <- colnames(bar.dat)
  rownames(bar.datnew) <- c(1,2)
  bar.datnew[1,] <- bar.dat[2,] + bar.dat[3,]
  bar.datnew[2,] <- bar.dat[4,] + bar.dat[5,]
  barplot(bar.datnew, main= paste("visits to single and fused state when sat =", i),
          xlab="Subject", legend = rownames(c("single","fused")), beside=TRUE)
}

# For supplementary we should also show the distribution of times in the "0" state

data1 <- subset(data, IDs == "FG" )

par(mfrow=c(1,2))
for (i in c(0.4,0.9)){
  toplot <- subset(data1, data1$Sat == i & data1$Tdata.perc == 0 )
  #Get it to the right timescale in seconds
  toplot <- toplot$Tdata.duration/100000
  m = mean(toplot)
  #mean and standard deviation
  hist(toplot, breaks=20, freq = FALSE, col = "blue",xlab="Duration of no percept", 
       main= paste("Single eye and sat", i), sub = paste("mean=", round(m,2)) )  
}