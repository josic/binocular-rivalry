# This function defines and runs the model
# The data is not standardized
genMCMC.hierarchical = function( data , xName="x" , yName="y" , sName="s" ,
                      numSavedSteps=50000 , thinSteps = 1 , 
                      saveName=NULL , runjagsMethod=runjagsMethodDefault ,
                      nChains=nChainsDefault) 
  { 
  require(rjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  y = data[,yName]
  x = data[,xName]
  s = as.numeric(factor(data[,sName]))
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    x = x ,
    y = y ,
    s = s ,
    Nsubj = max(s)  # should equal length(unique(s))
  )
  #-----------------------------------------------------------------------------
  # THE MODEL.
  # Specify the model for unstandardized data:
  modelString = "
  data {
    Ntotal <- length(y)
    #xm <- mean(x)
    #ym <- mean(y)
  }
  model {
  for ( i in 1:Ntotal ) {
  y[i] ~ dnorm(  beta0[s[i]] + beta1[s[i]] * x[i] , 1/sigma^2 )
  }
  for ( j in 1:Nsubj ) {
      beta0[j] ~ dnorm( beta0mu , 1/(beta0sigma)^2 )  
      beta1[j] ~ dnorm( beta1mu , 1/(beta1sigma)^2 )
    }
  # Priors vague 
  beta0mu ~ dnorm( 0 , 1/(10)^2 )
  beta1mu ~ dnorm( 0 , 1/(10)^2 )
  sigma ~ dunif( 1.0E-3 , 1.0E+3 )
  beta0sigma ~ dunif( 1.0E-3 , 1.0E+3 )
  beta1sigma ~ dunif( 1.0E-3 , 1.0E+3 )
  }
  " # close quote for modelString
  # Write out modelString to a text file
  writeLines( modelString , con="TEMPmodel.txt" )
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Let JAGS do it...
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "beta0" ,  "beta1" , "beta0mu" , "beta1mu" ,
                   "sigma", "beta0sigma" , "beta1sigma" )
  
  adaptSteps = 2000  # Number of steps to "tune" the samplers
  burnInSteps = 2000
  runJagsOut <- run.jags( method=runjagsMethod ,
                          model="TEMPmodel.txt" , 
                          monitor=parameters , 
                          data=dataList ,  
                          #inits=initsList , 
                          n.chains=nChains ,
                          adapt=adaptSteps ,
                          burnin=burnInSteps , 
                          sample=ceiling(numSavedSteps/nChains) ,
                          thin=thinSteps ,
                          summarise=FALSE ,
                          plots=FALSE )
  codaSamples = as.mcmc.list( runJagsOut )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  
  return( codaSamples )
} # end function