
gc()
rm(list=ls())

library(stats)
library(splines)
library(MASS)
library(crs)
library(sem)
library(stargazer)
library(AER)
library(xtable)
library(fBasics)




### PATHS
######## !! Set the project path to your working directory !!
projectpath = "d:/mirror_h/Copy_Missings/!_Replication_Package/"
                  


outfilepathsuffix = "Tables_output/" 
outfilepath = paste(projectpath, outfilepathsuffix, collapse="TURE", sep="")




########Define number of repetitions for the bootstrap
reps <- 500







################################
### Read DATA ###
################################

# Read Rawdata from ICT Survey
DATA <- read.csv(paste(projectpath, "DATA/", "!_laggedvalues_!ikt2004_start_R_IV_missings_processed_addvars.csv", collapse="True", sep="")) # Fixme: change path etc






#define Results Vector
  ResultsVec <- vector(mode="numeric", length=12)
  Results.SE <- vector(mode="numeric", length=12)
  Results.N  <- vector(mode="numeric", length=12)



  
  
##############################################
#
# 1: Full model with correction
#
###############################################
  
  # Define Variables from  raw data from ICT Survey
  W1_r <- DATA$lnL #lnL 
  W1C_r <- DATA$lnC_raw_zero # capital 
  Y_r <- DATA$lnval #Log Value Added over labor
  X_r <- DATA$v_out_b #I_IT-outsourcing
  W2_r <- DATA$ber_y2k #I_y2K_consulting
  
  
  
  # specify additional other controls
  ost_r <- DATA$ost                     # regional indicator
  pcwork_r <- DATA$pcwork               # control for IT-intensity
  outsourc_r <- DATA$outsourc 
  
  # Define industry dummies:
  br1_raw <- DATA$branche1
  br2_raw <- DATA$branche2
  br3_raw <- DATA$branche3
  br4_raw <- DATA$branche4
  br5_raw <- DATA$branche5
  br6_raw <- DATA$branche6
  br7_raw <- DATA$branche7
  br8_raw <- DATA$branche8
  br9_raw <- DATA$branche9
  br10_raw <- DATA$branche10
  br11_raw <- DATA$branche11
  br12_raw <- DATA$branche12
  br13_raw <- DATA$branche13
  br14_raw <- DATA$branche14
  
  
  #############################################
  #   Trim sample

               
  Y=  subset(Y_r,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)     
  W2= subset(W2_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)
  W1= subset(W1_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)
  W1C=subset(W1C_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)
  X=  subset(X_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)
  
  pcwork=subset(pcwork_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99  & pcwork_r > -99 & outsourc_r > -99)
  ost = subset(ost_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)
  outsourc = subset(outsourc_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)  # now control for lagged outsourcing
  
  br1 <- subset(br1_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br1_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
  br2 <- subset(br2_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br2_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
  br3 <- subset(br3_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br3_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
  br4 <- subset(br4_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br4_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
  br5 <- subset(br5_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br5_raw > -99 &   pcwork_r > -99 & outsourc_r > -99)
  br6 <- subset(br6_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br6_raw > -99 &   pcwork_r > -99 & outsourc_r > -99)
  br7 <- subset(br7_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br7_raw > -99 &   pcwork_r > -99 & outsourc_r > -99)
  br8 <- subset(br8_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br8_raw > -99 &   pcwork_r > -99 & outsourc_r > -99)
  br9 <- subset(br9_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br9_raw > -99 &   pcwork_r > -99 & outsourc_r > -99)
  br10 <- subset(br10_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br10_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
  br11 <- subset(br11_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br11_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
  br12 <- subset(br12_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br12_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
  br13 <- subset(br13_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br13_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
  br14 <- subset(br14_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br14_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
  

  BrDumDum <- cbind(br1, br2, br3, br4, br5, br6, br7, br8, br9, br10, br11, br13)
  # br14 is the reference
  # br12 is zeros
  BrDum <- cbind(ost, outsourc, pcwork, BrDumDum)   # here we now additionally control for lagged outsourcing   
  
  
  ### For full model with correction: define delta
  n= length(Y)
  delta <- vector()
  for(i in 1:n)if(Y[i]==-999){delta[i]<-0} else {delta[i]<-1}
  Y <- delta*Y
  
  ones<-rep(1,1,n)
  
  
  
  ## define V and Z
  V<- cbind(Y,W1,W1C, W2)
  
  #Use IT-Outsoucring as instrument for Reponse
  Z<- cbind(X,W1,W1C, W2)
  


    
#########Digression MKM: Regress Y2K-Consulting on lagged outsourcing and controls to show that both are not tightly related
    MKM_test <- lm(W2~ W1 + W1C + BrDum)
    summary(MKM_test)
    stargazer(MKM_test, title="Regression: Y2K-Consulting on Lagged Outsourcing and Controls",
              omit.table.layout = "n",
              order=c("outsourc", "W1", "W1C", "pcwork", "ost"),
              covariate.labels=c("$outsourcing_{t-1}$", "ln(Labour)", "ln(Capital)", "pcwork",
                                 "ost", "consumer goods", "chemical industry",
                                 "other raw materials", "metal and machine construction",
                                 "electrical engineering", "precision instruments", "automobile",
                                 "wholesale trade", "retail trade", "transportation and postal services",
                                 "banks and insurances", "electronic processing and telecommunication",
                                 "other business-related services" ),
              omit.stat=c("rsq","ser"), align=TRUE, single.row = TRUE, dep.var.labels="y2k-Consult" ,
              out = paste(outfilepath,"TabC3_y2k_on_lagged_outsourcing.tex", collapse="True", sep=""))
##########
    
    
    
######### Estimator for conditional probability

  K= length(V[1,])
  tsls1.est<- vector()
  tsls1.est[1:K]<-0#.01# Vector for local Taylor approx. (theta_0 in paper)
  tsls1.est[1]<-qnorm(0.55) 
  # create starting values for the optimization procedure
  A.mat<- t(Z)%*%(as.vector(dnorm(V%*%tsls1.est[1:K]))*t(t(delta*V)/as.vector(pnorm(V%*%tsls1.est[1:K])^2)))
  start.est<- tsls1.est+ginv(A.mat)%*%(t(Z)%*%(delta/pnorm(V%*%tsls1.est[1:K])-ones))
  
  
  
  # probit IV for estiming selection prob. of response
  ff0<-function(x){return(sum((t(delta/pnorm(V%*%x[1:K])-ones)%*%Z)^2))
  }
  
  tsls1.est<- optim(start.est, ff0, control = list( reltol = 1e-16))$par
  
  # #   
  prob <- pnorm(V%*%tsls1.est)
  #in paper: G(V'THETA)
  



  # OPTIONAL: Cross Validation to choose number of basis functions
  cv.g <- crs(X~W1+W1C, complexity = c("knots"), knots="uniform", basis ="tensor", degree=c(2,2))
  
  df1.c <- cv.g$K[1,2] +cv.g$K[1,1] #dimension = number of segements for W1 + order 2
  degree1.c <- cv.g$K[1,1] # is 2 since we use quadratic B-splines
  df2.c <- cv.g$K[2,2] +cv.g$K[2,1] #dimension = number of segements for W1 + order 2
  degree2.c <- cv.g$K[2,1] # is 2 since we use quadratic B-splines
  
  
  
  #############################################
  ######### Regression
  
  
  OtherControls = BrDum[,]
  OC <- OtherControls
  
  # SEMIPARAMETRIC Estimation of Structural Parameter
  BasW1.mat = bs(W1, df=df1.c, degree=degree1.c)
  BasW1C.mat = bs(W1C, df=df2.c, degree=degree2.c)
  
  
  cond.mean.Y <- lm((Y/pnorm(V%*%tsls1.est))~BasW1.mat+BasW1C.mat+OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]
                    + OC[,6]+OC[,7]+OC[,8]+ OC[,9]+OC[,10]+OC[,11]
                    + OC[,12]+OC[,13]+OC[,14]+OC[,15])$fitted.values
  cond.mean.X <- lm(X~BasW1.mat+BasW1C.mat
                    +OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]+OC[,6]+OC[,7]+OC[,8]
                    + OC[,9]+OC[,10]+OC[,11]+OC[,12]+OC[,13]+OC[,14]+OC[,15])$fitted.values
  
  # Closed form Estimator BETA-Coefficient
  ResultsVec[10] <- sum((Y/pnorm(V%*%tsls1.est)-cond.mean.Y)*W2)/sum((X-cond.mean.X)*W2)
  Results.N[10] <- length(Y)
  


  ##### compare partially linear model with linear model --> TSLS with correction for P(delta)
  Y.bar <- Y/pnorm(V%*%tsls1.est)
  XX <- cbind(X,W1, W1C, BrDum)  
  ZZ <- cbind(W1,W1C, W2, BrDum)
  
  
  # now use in TSLS
  tsls.est= tsls(Y.bar~XX,~ZZ)
  tsls.est$coeff
  tsls.est$coeff[2]
  betahat2slsBrDum = tsls.est$coeff[2]
  betahat2slsBrDum
  
  ResultsVec[6] = betahat2slsBrDum
  Results.N[6] <- length(Y)
  

  



#################################################
## Bootstrap SEs
#####################

set.seed(12345)
Conf.int1=matrix(nrow=2, ncol=1)


# vectors to save the  calculated statistics for each subsample
BootstrapBranche.sample=vector()   	
Bootstrap2SLSBrDum.sample=vector()  	






#################################################
## Bootstrap SEs
#####################
set.seed(12345)


# vectors to save the  calculated statistics for each subsample
BootstrapBranche.sample=vector()   
Bootstrap2SLSBrDum.sample=vector()  	


	




#j=1
for (j in  1:reps){
  sample.index=vector()
  sample.index=sample(1:length(Y), replace=TRUE)
  Yb=vector()
  Xb=vector()
  W1b=vector()
  W1Cb=vector()
  W2b=vector()
  deltab=vector() 
  BrDumb = mat.or.vec(dim(BrDum)[1], dim(BrDum)[2])
  
  
  for (i in 1:length(Y)) Yb[i]=Y[sample.index[i]]
  for (i in 1:length(Y)) Xb[i]=X[sample.index[i]]
  for (i in 1:length(Y)) W2b[i]=W2[sample.index[i]]
  for (i in 1:length(Y)) W1b[i]=W1[sample.index[i]]
  for (i in 1:length(Y)) W1Cb[i]=W1C[sample.index[i]]
  for (i in 1:length(Y)) deltab[i]=delta[sample.index[i]]
  for (i in 1:length(Y)) BrDumb[i,]=BrDum[sample.index[i],]
  
  
  ones<-rep(1,1,n)                                                
  V<- cbind(Yb,W1b,W1Cb, W2b)         
  Z<- cbind(Xb,W1b,W1Cb, W2b)
  
  
  ######### estimator for conditional probability
  ###############################################
  K= length(V[1,])
  
  ff0<-function(x){return(sum((t(deltab/pnorm(V%*%x[1:K])-ones)%*%Z)^2))
  }
  
  
  tsls1.est[1:K]<-0# Vector for local Taylor approx. (theta_0 in paper)
  tsls1.est[1]<-qnorm(0.55)
  # create starting values for the optimization procedure
  A.mat<- t(Z)%*%(as.vector(dnorm(V%*%tsls1.est[1:K]))*t(t(deltab*V)/as.vector(pnorm(V%*%tsls1.est[1:K])^2)))
  start.est<- tsls1.est+ginv(A.mat)%*%(t(Z)%*%(deltab/pnorm(V%*%tsls1.est[1:K])-ones))
  
  tsls1.est<- optim(start.est, ff0)$par
  
  prob <- pnorm(V%*%tsls1.est)
  plot(Yb,prob, type = "p", xlim=c(min(Y), max(Y)), ylim=c(0,1))
  
  
  
  ## Regressions:
  OC = BrDumb[,]
  BasW1.mat = bs(W1b, df=df1.c, degree=degree1.c)
  BasW1C.mat = bs(W1Cb, df=df2.c, degree=degree2.c)

  
  cond.mean.Y <- lm((Yb/pnorm(V%*%tsls1.est))~BasW1.mat+BasW1C.mat+OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]
                    + OC[,6]+OC[,7]+OC[,8]+ OC[,9]+OC[,10]+OC[,11]
                    + OC[,12]+OC[,13]+OC[,14]+OC[,15])$fitted.values
  cond.mean.X <- lm(Xb~BasW1.mat+BasW1C.mat
                    +OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]+OC[,6]+OC[,7]+OC[,8]
                    + OC[,9]+OC[,10]+OC[,11]+OC[,12]+OC[,13]+OC[,14]+OC[,15])$fitted.values
  
  
  
  BETABrancheB <- sum((Yb/pnorm(V%*%tsls1.est)-cond.mean.Y)*W2b)/sum((Xb-cond.mean.X)*W2b)
  BootstrapBranche.sample[j] = BETABrancheB
  
  
  
  ##########  linear model (TSLS)
  Y.barb <- Yb/pnorm(V%*%tsls1.est)
  XXb <- cbind(Xb,W1b, W1Cb, BrDumb) 
  ZZb <- cbind(W1b,W1Cb, W2b, BrDumb) 
  
  # now use in TSLS
  tsls.est= tsls(Y.barb~XXb,~ZZb)
  tsls.est$coeff
  
  Bootstrap2SLSBrDum.sample[j] <- tsls.est$coeff[2]
  

  
  
}

####################################
#   Bootstrap SEs: Results
#
### partially linear model
ResultsVec[10]
quantile(BootstrapBranche.sample,probs= 0.5)

quantile(BootstrapBranche.sample,probs= 0.025)
quantile(BootstrapBranche.sample,probs= 0.05)
quantile(BootstrapBranche.sample,probs= 0.95)
quantile(BootstrapBranche.sample,probs= 0.975)

Results.SE[10] = sd(BootstrapBranche.sample)


###  linear model
Bootstrap2SLSBrDum.sample
betahat2slsBrDum
quantile(Bootstrap2SLSBrDum.sample,probs= 0.5)


quantile(Bootstrap2SLSBrDum.sample,probs= 0.025)
quantile(Bootstrap2SLSBrDum.sample,probs= 0.05)
quantile(Bootstrap2SLSBrDum.sample,probs= 0.95)
quantile(Bootstrap2SLSBrDum.sample,probs= 0.975)

Results.SE[6] = sd(Bootstrap2SLSBrDum.sample)






##############################################
#
# 2: Benchmark - Estimation under MCAR: Listwise deletion --> no correction
#
###############################################

## Retrim sample for the listwise deletion case, i.e. so that everything including Y is fully observed
Y=      subset(Y_r     , Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)   # !! for listwise deletion we also condition on Y being observed
W2=     subset(W2_r    , Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)
W1=     subset(W1_r    , Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)
W1C =   subset(W1C_r   , Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)
X=      subset(X_r     , Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)


pcwork= subset(pcwork_r, Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)
ost =   subset(ost_r   , Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99 & outsourc_r > -99)


br1 <- subset(br1_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br1_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br2 <- subset(br2_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br2_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br3 <- subset(br3_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br3_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br4 <- subset(br4_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br4_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br5 <- subset(br5_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br5_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br6 <- subset(br6_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br6_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br7 <- subset(br7_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br7_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br8 <- subset(br8_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br8_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br9 <- subset(br9_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br9_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br10 <- subset(br10_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br10_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br11 <- subset(br11_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br11_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br12 <- subset(br12_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br12_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br13 <- subset(br13_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br13_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)
br14 <- subset(br14_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br14_raw > -99 &  pcwork_r > -99 & outsourc_r > -99)

BrDumDum <- cbind(br1, br2, br3, br4, br5, br6, br7, br8, br9, br10, br11, br13)
BrDum <- cbind(ost, outsourc, pcwork, BrDumDum)



n=length(Y)


ones<- rep(1,n)





# ## Regressions:
###### Partially linear model: NO CORRECTION 


OtherControls = BrDum[,]
OC <- OtherControls

# SEMIPARAMETRIC Estimation of Structural Parameter
BasW1.mat = bs(W1, df=df1.c, degree=degree1.c)
BasW1C.mat = bs(W1C, df=df2.c, degree=degree2.c)


cond.mean.Y <- lm((Y)~BasW1.mat+BasW1C.mat+OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]
                  + OC[,6]+OC[,7]+OC[,8]+ OC[,9]+OC[,10]+OC[,11]
                  + OC[,12]+OC[,13]+OC[,14]+OC[,15])$fitted.values
cond.mean.X <- lm(X~BasW1.mat+BasW1C.mat
                  +OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]+OC[,6]+OC[,7]+OC[,8]
                  + OC[,9]+OC[,10]+OC[,11]+OC[,12]+OC[,13]+OC[,14]+OC[,15])$fitted.values

# Closed form Estimator BETA-Coefficient
BETA <- sum((Y-cond.mean.Y)*W2)/sum((X-cond.mean.X)*W2)

ResultsVec[8] =BETA
Results.N[8] <- length(Y)





#######  linear model: NO CORRECTION 
XX <- cbind(X,W1, W1C, BrDum) 
ZZ <- cbind(W1,W1C, W2, BrDum) 

# now use in TSLS
tsls.est= tsls(Y~XX,~ZZ)
tsls.est$coeff
tsls.est$coeff[2]
betaBase2slsBrDum = tsls.est$coeff[2]
betaBase2slsBrDum


ResultsVec[2] = betaBase2slsBrDum
Results.N[2] <- length(Y)








##  Bootstrap
###########################
set.seed(12345)


BootstraplistwdelBrDum.sample=vector()  	
Bootstraplistwdel2SLSBrDum.sample =vector()    





for (j in  1:reps){
  sample.index=vector()
  sample.index=sample(1:length(Y), replace=TRUE)
  Yb=vector()
  Xb=vector()
  W1b=vector()
  W1Cb=vector()
  W2b=vector()
  BrDumb = mat.or.vec(dim(BrDum)[1], dim(BrDum)[2])
  
  for (i in 1:length(Y)) Yb[i]=Y[sample.index[i]]
  for (i in 1:length(Y)) Xb[i]=X[sample.index[i]]
  for (i in 1:length(Y)) W2b[i]=W2[sample.index[i]]
  for (i in 1:length(Y)) W1b[i]=W1[sample.index[i]]
  for (i in 1:length(Y)) W1Cb[i]=W1C[sample.index[i]]
  for (i in 1:length(Y)) BrDumb[i,]=BrDum[sample.index[i],]
  
  n=length(Y)
  
  
  ones<- rep(1,n)
  df1.c <- 7
  degree1.c <- 2
  df2.c <- 5
  degree2.c <- 2
  
  
  
  ######### FULL MODEL: partially linear model
  
  OC = BrDumb[,]
  BasW1.mat = bs(W1b, df=df1.c, degree=degree1.c)
  BasW1C.mat = bs(W1Cb, df=df2.c, degree=degree2.c)

  
  cond.mean.Y <- lm((Yb)~BasW1.mat+BasW1C.mat+OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]
                    + OC[,6]+OC[,7]+OC[,8]+ OC[,9]+OC[,10]+OC[,11]
                    + OC[,12]+OC[,13]+OC[,14]+OC[,15])$fitted.values
  cond.mean.X <- lm(Xb~BasW1.mat+BasW1C.mat
                    +OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]+OC[,6]+OC[,7]+OC[,8]
                    + OC[,9]+OC[,10]+OC[,11]+OC[,12]+OC[,13]+OC[,14]+OC[,15])$fitted.values
  
  
  
  BETABrancheB <- sum((Yb-cond.mean.Y)*W2b)/sum((Xb-cond.mean.X)*W2b)
  
  BootstraplistwdelBrDum.sample[j] = BETABrancheB
  
  
  
  ##########  linear model (TSLS)
  Y.barb <- Yb
  XXb <- cbind(Xb,W1b, W1Cb, BrDumb)  
  ZZb <- cbind(W1b,W1Cb, W2b, BrDumb) 
  
  # now use in TSLS
  tsls.est= tsls(Y.barb~XXb,~ZZb)
  tsls.est$coeff
  
  
  # BETA-Coefficient 2SLS with Branchendummies
  Bootstraplistwdel2SLSBrDum.sample[j] <- tsls.est$coeff[2]
  

  
  
  
}

# Partially linear model - NO CORRECTION 
quantile(BootstraplistwdelBrDum.sample,probs= 0.025)
quantile(BootstraplistwdelBrDum.sample,probs= 0.05)
quantile(BootstraplistwdelBrDum.sample,probs= 0.95)
quantile(BootstraplistwdelBrDum.sample,probs= 0.975)

Results.SE[8] = sd(BootstraplistwdelBrDum.sample)


###  linear model
quantile(Bootstraplistwdel2SLSBrDum.sample,probs= 0.025)
quantile(Bootstraplistwdel2SLSBrDum.sample,probs= 0.05)
quantile(Bootstraplistwdel2SLSBrDum.sample,probs= 0.95)
quantile(Bootstraplistwdel2SLSBrDum.sample,probs= 0.975)

Results.SE[2] <- sd(Bootstraplistwdel2SLSBrDum.sample)









##########################################################################################################
#
#     summary results
#
###########################################################################################################

######################   Estimation results
ResultsVec
Results.SE
Results.N




Results <- rbind(ResultsVec,Results.SE,Results.N)
colnames(Results, do.NULL = TRUE, prefix = "col")
colnames(Results) <- c("2sls_biased/no contr", "2sls_biased/w contr", "misemipar/no contr", "misemipar/w contr", "2sls_correct/no contr", "2sls_correct/w contr", "semiparam_biased/no contr", "semiparam_biased/w contr" , "semiparam_correct/no contr", "semiparam_correct/w contr", "MI 2sls_biased/w contr", "MI 2sls_biased/no contr")

rownames(Results, do.NULL = TRUE, prefix = "row")
rownames(Results) <- c("coefficient", "SE", "N")

Results



################## Create new Table with estimation results 
#define Table Results1 Vector
Results1 <- matrix(nrow=3, ncol=4)


#Coefficients
#linear (Tsls)
Results1[,1] <- Results[,2] #listwise deletion
Results1[,2] <- Results[,6] # with correction
#Partially linear
Results1[,3] <- Results[,8] #listwise deletion
Results1[,4] <- Results[,10] # with correction

colnames(Results1, do.NULL = TRUE, prefix = "col")
colnames(Results1) <- c("Listwise Deletion (MCAR)",  "Correction", "Listwise Deletion (MCAR)", "Correction")

rownames(Results1, do.NULL = TRUE, prefix = "row")
rownames(Results1) <- c("coefficient", "SE", "N")

Results1

mainresults_digits <- matrix(c(3,3,0), nrow = dim(Results1)[1], ncol=dim(Results1)[2]+1)
print(xtable(Results1, digits = mainresults_digits), type="latex", file=paste(c(outfilepath),"TabC2_Main_Results_lagged_outs_as_control.tex",  collapse=""))

