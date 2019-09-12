gc()
rm(list = ls())


library(Rlab)
library(MASS)
library(splines)
library(sem)
library(xtable)
library(parallel)


######### simulation procedure
# 1. Run estimation on the anonymized data and save the residual (U)
# 2. Do simulation based on fitted values plus random draw of U from its empirical distribution



##########################################################################
##########################################################################
########################      Simulation
##########################################################################
##########################################################################


## specify parameter of interest
# beta <- # parameter of interest
# eta <- #measures degree of endogeneity
# xi <- #measures strength of instruments
# Usigma <- #measures noise level in regression model

parameters <- matrix(nrow = 4, ncol = 8)
rownames(parameters) <- c("beta", "eta", "xi", "Usigma")

parameters[1,] <- c(.5,.5, .5, .5, 1, 1, 1, 1)   # beta
parameters[2,] <- c(.3,.5,.3,.5, .3,.5,.3,.5) # eta
parameters[3,] <- c(.5,.5,.7,.7, .5,.5,.7,.7) # xi
parameters[4,] <- c(1.5, 1.5,1.5, 1.5, 1.5, 1.5,1.5, 1.5)   # Usigma




##number of MC iterations
N <- 1000


##bootstrap repetitions
reps <- 500


MC.vec <- vector()
coverage_MC.vec <- vector()

MC_mar.vec <- vector()
coverage_MC_mar.vec <- vector()

MC_res.mat <- matrix(nrow = ncol(parameters), ncol = 2)
MC_coverage.mat <- matrix(nrow = ncol(parameters), ncol = 2)



# Set parameters for quadratic B-splines with 7 knots
df1.c <- 7
degree1.c <- 2


##do simulation for two values for beta

for (value in 1:ncol(parameters)) {
  
  RNGkind("L'Ecuyer-CMRG")
  set.seed(123)
  
  MC.fct <- function(MC){
    
    n <- 2631
    ones <- rep(1,n)
    
    #Parameters
    eta <- parameters[2, value] #measures degree of endogeneity
    xi <- parameters[3, value]  #measures strength of instruments
    Usigma <- parameters[4, value]  #measures noise level in regression model
    
    #Generate Data
    unobs <-  rnorm(4*n)
    
    W2 <-  as.numeric(unobs[1:n]>0)  #binary instrument
    X <- as.numeric(xi*unobs[1:n] + sqrt(1-xi^2)*unobs[(n+1):(2*n)]>0)
    #binary endogenous variable
    U <- Usigma*(eta*unobs[(n+1):(2*n)] +
                   sqrt(1-eta^2)*unobs[(2*n+1):(3*n)]) #error correlated with X
    W1 <- 3+unobs[(3*n+1):(4*n)]  #control variable
    
    Y <-  parameters[1, value]*X + W1 + U  # generate Y
    Y <- as.vector(Y)
    
    
    #############################################  Missing Data Mechanism  ############################
    ## prob as a smooth function of Y
    delta <- vector()
    error <- rnorm(n)
    prob <- pnorm(Y/2+ error/2) 
    for (i in 1:n){ delta[i] <-  rbern(1,prob[i])}
    Y <- delta*Y
    
    ############################################# END:  Missing Data Mechanism  ############################
    
    
    V<- cbind(Y,W1, W2)
    #! V in code is V in Paper
    
    #Use IT-Outsoucring as instrument for Reponse
    Z<- cbind(X,W1, W2)
    
    
    K= length(V[1,])
    tsls1.est<- vector()
    tsls1.est[1:K]<-0#.01# Vector for local Taylor approx. (theta_0 in paper)
    tsls1.est[1]<-qnorm(0.55) # MCAR using that 79.65% are missing of Y^*
    # create starting values for the optimization procedure
    A.mat<- t(Z)%*%(as.vector(dnorm(V%*%tsls1.est[1:K]))*t(t(delta*V)/as.vector(pnorm(V%*%tsls1.est[1:K])^2)))
    start.est<- tsls1.est+ginv(A.mat)%*%(t(Z)%*%(delta/pnorm(V%*%tsls1.est[1:K])-ones))
    
    ###############################
    ######### Constraint Estimator
    ###############################
    ff0<-function(x){return(sum((t(delta/pnorm(V%*%x[1:K])-ones)%*%Z)^2))
    }
    
    tsls1.est<- optim(start.est, ff0, control = list(reltol = 1e-16))$par
    # #   
    prob <- pnorm(V%*%tsls1.est)
    
    
  
    
    
    ##
    BasW1.mat = bs(W1, df=df1.c, degree=degree1.c)
    # Estimator of  g=E[Y/G(..)|W]
    cond.mean.Y <- lm((Y/pnorm(V%*%tsls1.est))~BasW1.mat)$fitted.values
    #plot(W1,cond.mean.Y)

    # Estimator of h(w1,w1c)=E[X|W1=w1,W1C=w1c]
    cond.mean.X <- lm(X~BasW1.mat)$fitted.values

    # Closed form Estimator BETA-Coefficient
    MC.vec <- sum((Y/pnorm(V%*%tsls1.est)-cond.mean.Y)*W2)/sum((X-cond.mean.X)*W2)

    
    
    ##Now bootstrap
    ######################################
    Bootstrap.sample=vector()  	# saves for each subsample the calculated statistics
    for (j in  1:reps){
      sample.index=vector()
      sample.index=sample(1:length(Y), replace=TRUE)
      Yb=vector()
      Xb=vector()
      W1b=vector()
      W2b=vector()
      deltab=vector() 
     
      
      for (i in 1:length(Y)) Yb[i]=Y[sample.index[i]]
      for (i in 1:length(Y)) Xb[i]=X[sample.index[i]]
      for (i in 1:length(Y)) W2b[i]=W2[sample.index[i]]
      for (i in 1:length(Y)) W1b[i]=W1[sample.index[i]]
      for (i in 1:length(Y)) deltab[i]=delta[sample.index[i]]

      ones<-rep(1,1,n)                                                 
      V<- cbind(Yb,W1b,W2b)                   
      Z<- cbind(Xb,W1b,W2b)                 
    
      
      K= length(V[1,])
      tsls1.est<- vector()
      tsls1.est[1:K]<-0#.01# Vector for local Taylor approx. (theta_0 in paper)
      tsls1.est[1]<-qnorm(0.55) # MCAR using that 79.65% are missing of Y^*
      # create starting values for the optimization procedure
      A.mat<- t(Z)%*%(as.vector(dnorm(V%*%tsls1.est[1:K]))*t(t(deltab*V)/as.vector(pnorm(V%*%tsls1.est[1:K])^2)))
      start.est<- tsls1.est+ginv(A.mat)%*%(t(Z)%*%(deltab/pnorm(V%*%tsls1.est[1:K])-ones))
      
      ###
      ff0<-function(x){return(sum((t(deltab/pnorm(V%*%x[1:K])-ones)%*%Z)^2))
      }
      
      tsls1.est<- optim(start.est, ff0, control = list(reltol = 1e-16))$par
      # #   
      prob <- pnorm(V%*%tsls1.est)
      
      
      ##
      BasW1.mat = bs(W1b, df=df1.c, degree=degree1.c)
      # Estimator of  g=E[Y/G(..)|W]
      cond.mean.Y <- lm((Yb/pnorm(V%*%tsls1.est))~BasW1.mat)$fitted.values
      #plot(W1,cond.mean.Y)
      
      # Estimator of h(w1,w1c)=E[X|W1=w1,W1C=w1c]
      cond.mean.X <- lm(Xb~BasW1.mat)$fitted.values
      
      # Closed form Estimator BETA-Coefficient
      Bootstrap.sample[j] <- sum((Yb/pnorm(V%*%tsls1.est)-cond.mean.Y)*W2b)/sum((Xb-cond.mean.X)*W2b)
      
    }
    coverage_MC.vec <- as.numeric(quantile(Bootstrap.sample, probs = c(0.025)) <= parameters[1, value] & parameters[1, value] <= quantile(Bootstrap.sample, probs = c(0.975)))
    
    
    
    
    ############################################
    ######now without correction
    ############################################
    
    Y_mar <- Y[delta == 1]
    n_mar = length(Y_mar)
    ones<-rep(1,1,n_mar)
    
    
    X_mar <- X[delta == 1] 
    
    W1_mar <- W1[delta == 1] 
    W2_mar <- W2[delta == 1]
    
    V<- cbind(Y_mar,W1_mar, W2_mar)
    #! V in code is V in Paper
    #Use IT-Outsoucring as instrument 
    Z<- cbind(X_mar,W1_mar, W2_mar)
    
    

    # ##
    BasW1.mat = bs(W1_mar, df=df1.c, degree=degree1.c)
    # Estimator of  g=E[Y/G(..)|W]
    cond.mean.Y <- lm((Y_mar)~BasW1.mat)$fitted.values
    #plot(W1,cond.mean.Y)

    # Estimator of h(w1,w1c)=E[X|W1=w1,W1C=w1c]
    cond.mean.X <- lm(X_mar~BasW1.mat)$fitted.values

    # Closed form Estimator BETA-Coefficient
    MC_mar.vec <- sum((Y_mar-cond.mean.Y)*W2_mar)/sum((X_mar-cond.mean.X)*W2_mar)
    
    
    ##bootstrap
    ######################################
    Bootstraplistwdel.sample=vector()  	# saves for each subsample the calculated statistics
    for (j in  1:reps){
      sample.index=vector()
      sample.index=sample(1:length(Y_mar), replace=TRUE)
      Yb=vector()
      Xb=vector()
      W1b=vector()
      W2b=vector()
      
      for (i in 1:length(Y_mar)) Yb[i]=Y_mar[sample.index[i]]
      for (i in 1:length(Y_mar)) Xb[i]=X_mar[sample.index[i]]
      for (i in 1:length(Y_mar)) W2b[i]=W2_mar[sample.index[i]]
      for (i in 1:length(Y_mar)) W1b[i]=W1_mar[sample.index[i]]

      ones<-rep(1,1,n_mar)                                                 
      V<- cbind(Yb,W1b,W2b)                   
      Z<- cbind(Xb,W1b,W2b)                 
      
      
      ##
      BasW1.mat = bs(W1b, df=df1.c, degree=degree1.c)
      # Estimator of  g=E[Y/G(..)|W]
      cond.mean.Y <- lm((Yb)~BasW1.mat)$fitted.values
      #plot(W1,cond.mean.Y)
      
      # Estimator of h(w1,w1c)=E[X|W1=w1,W1C=w1c]
      cond.mean.X <- lm(Xb~BasW1.mat)$fitted.values
      
      # Closed form Estimator BETA-Coefficient
      Bootstraplistwdel.sample[j] <- sum((Yb-cond.mean.Y)*W2b)/sum((Xb-cond.mean.X)*W2b)
      
    }
    coverage_MC_mar.vec <- as.numeric(quantile(Bootstraplistwdel.sample, probs = c(0.025)) <= parameters[1, value] & parameters[1, value] <= quantile(Bootstraplistwdel.sample, probs = c(0.975)))
    
    
    c(MC.vec, coverage_MC.vec, MC_mar.vec,coverage_MC_mar.vec)
  }
  
  
  test.mat <- t(mcmapply(MC.fct,1:N, mc.cores = 30))
  test.vec<- colMeans(test.mat)
  
  MC_res.mat[value,1] <- test.vec[1]
  MC_coverage.mat[value,1] <- test.vec[2]

  #without correction
  MC_res.mat[value,2] <- test.vec[3]
  MC_coverage.mat[value,2] <-test.vec[4]

}

MC_res.mat
MC_coverage.mat



###Table results of the simulation

MC_table <- matrix(nrow = ncol(parameters), ncol = 4)
colnames(MC_table) <- c('\\hat{\\beta_0}', 'coverage for \\beta_0', '\\hat{\\beta_0}', 'coverage for \\beta_0')
# rownames(MC_table) <- beta

MC_table[,1] <- MC_res.mat[,1]
MC_table[,2] <- MC_coverage.mat[,1]
MC_table[,3] <- MC_res.mat[,2]
MC_table[,4] <- MC_coverage.mat[,2]


options(xtable.sanitize.text.function=identity)
MC_output.table <- xtable((MC_table), type="latex", include.rownames=TRUE,
                              caption = 'Simulation Results \\label{tab_simulation}',
                              digits=c(0,4,2,4,2))

print(MC_output.table , type="latex", caption.placement="top", file='MC_results_table_five_percent_nominal_value.tex')

