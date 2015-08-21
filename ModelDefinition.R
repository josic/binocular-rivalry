# This function defines and runs the model
# The data is not standardized
genMCMC = function( data , xName="x" , yName="y" , 
                    numSavedSteps=50000 , saveName=NULL ) { 
  require(rjags)
  #-----------------------------------------------------------------------------
  # THE DATA.
  y = data[,xName]
  x = data[,yName]
  # Specify the data in a list, for later shipment to JAGS:
  dataList = list(
    x = x ,
    y = y 
  )
  #-----------------------------------------------------------------------------
  # THE MODEL.
  # Specify the model for unstandardized data:
  modelString = "
  data {
    Ntotal <- length(y)
    xm <- mean(x)
    ym <- mean(y)
  }
  model {
  for ( i in 1:Ntotal ) {
  y[i] ~ dnorm( beta0 + beta1 * x[i] , 1/sigma^2 )
  }
  # Priors vague on standardized scale:
  beta0 ~ dnorm( 0 , 1/(10)^2 )  
  beta1 ~ dnorm( 0 , 1/(10)^2 )
  sigma ~ dunif( 1.0E-3 , 1.0E+3 )
  # nu <- nuMinusOne+1
  # nuMinusOne ~ dexp(1/29.0)
  }
  " # close quote for modelString
  # Write out modelString to a text file
  writeLines( modelString , con="TEMPmodel.txt" )
  #-----------------------------------------------------------------------------
  # INTIALIZE THE CHAINS.
  # Let JAGS do it...
  #-----------------------------------------------------------------------------
  # RUN THE CHAINS
  parameters = c( "beta0" ,  "beta1" ,  "sigma" )
  adaptSteps = 500  # Number of steps to "tune" the samplers
  burnInSteps = 1000
  nChains = 1 
  thinSteps = 1
  nIter = 20000
  #nIter = ceiling( ( numSavedSteps * thinSteps ) / nChains )
  # Create, initialize, and adapt the model:
  jagsModel = jags.model( "TEMPmodel.txt" , data=dataList , #inits=initsList , 
                          n.chains=nChains , n.adapt=adaptSteps )
  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )
  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  codaSamples = coda.samples( jagsModel , variable.names=parameters , 
                              n.iter=nIter , thin=thinSteps )
  # resulting codaSamples object has these indices: 
  #   codaSamples[[ chainIdx ]][ stepIdx , paramIdx ]
  if ( !is.null(saveName) ) {
    save( codaSamples , file=paste(saveName,"Mcmc.Rdata",sep="") )
  }
  return( codaSamples )
} # end function