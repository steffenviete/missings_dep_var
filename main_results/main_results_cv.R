
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
library(Amelia)



### PATHS

######## !! Set the project path to your working directory !!
projectpath = "d:/mirror_h/Copy_Missings/!_Replication_Package/" 
                  


figurepathsuffix = "Figures_output/"
figurepath = paste(c(projectpath, figurepathsuffix), collapse="")


imputationsourcepathsuffix = "imputation/"
imputationsourcepath = paste(c(projectpath, imputationsourcepathsuffix), collapse="")

outfilepathsuffix = "Tables_output/" 
outfilepath = paste(projectpath, outfilepathsuffix, collapse="TURE", sep="")





########Define number of repetitions for the bootstrap
reps <- 500







################################
### Read DATA ###
################################

# # Read Rawdata from ICT Survey

##### Read in the SYNTHETIC DATA. Note that this data does not reproduce the original results of the paper.
    # DATA <- read.csv(paste(projectpath, "DATA/", "!ikt2004_anonym_V4.csv", collapse="True", sep=""))

##### Read in the ORIGINAL DATA. This data is needed to reproduce the results of the paper and is available upon signing a 
#####   data usage agreement.
    DATA <- read.csv(paste(projectpath, "DATA/", "!ikt2004_start_R_IV_missings_processed_addvars.csv", collapse="True", sep=""))

 
 
#define Results Vector
  ResultsVec <- vector(mode="numeric", length=12)
  Results.SE <- vector(mode="numeric", length=12)
  Results.N  <- vector(mode="numeric", length=12)

  
  
  #and for capital
  CResultsVec <- vector(mode="numeric", length=12)
  CResults.SE <- vector(mode="numeric", length=12) 

  
  
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
## Trim sample:
## Keep an esimtaion sample with univariate missingnes in Y

  Y=  subset(Y_r,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99)     
  W2= subset(W2_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99)
  W1= subset(W1_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99)
  W1C=subset(W1C_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99)
  X=  subset(X_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99)

  pcwork=subset(pcwork_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99  & pcwork_r > -99)
  ost = subset(ost_r, X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99)
  
  
  br1 <- subset(br1_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br1_raw > -99 &  pcwork_r > -99)
  br2 <- subset(br2_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br2_raw > -99 &  pcwork_r > -99)
  br3 <- subset(br3_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br3_raw > -99 &  pcwork_r > -99)
  br4 <- subset(br4_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br4_raw > -99 &  pcwork_r > -99)
  br5 <- subset(br5_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br5_raw > -99 &   pcwork_r > -99)
  br6 <- subset(br6_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br6_raw > -99 &   pcwork_r > -99)
  br7 <- subset(br7_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br7_raw > -99 &   pcwork_r > -99)
  br8 <- subset(br8_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br8_raw > -99 &   pcwork_r > -99)
  br9 <- subset(br9_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br9_raw > -99 &   pcwork_r > -99)
  br10 <- subset(br10_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br10_raw > -99 &  pcwork_r > -99)
  br11 <- subset(br11_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br11_raw > -99 &  pcwork_r > -99)
  br12 <- subset(br12_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br12_raw > -99 &  pcwork_r > -99)
  br13 <- subset(br13_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br13_raw > -99 &  pcwork_r > -99)
  br14 <- subset(br14_raw,X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br14_raw > -99 &  pcwork_r > -99)


  BrDumDum <- cbind(br1, br2, br3, br4, br5, br6, br7, br8, br9, br10, br11, br13)
  # br14 is the reference
  # br12 is zeros
  BrDum <- cbind(ost,  pcwork, BrDumDum)    


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

    
###Plot est of inverse conditional prob that Delta=1 given potential outcomes
  plot(Y,prob, type = "p", xlim=c(1, max(Y)), ylim=c(0.0,1), xlab="ln(Prod)") #
    
  
  pdfname = "rplot"
  FigureFile <- paste(c(figurepath, pdfname, ".pdf"), collapse="")
  FigureFile

  pdf(FigureFile)
  plot(Y,prob, type = "p", xlim=c(1, max(Y)), ylim=c(0.0,1), xlab="ln(Prod)") 
  dev.off()
  



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
                       + OC[,12]+OC[,13]+OC[,14])$fitted.values
    cond.mean.X <- lm(X~BasW1.mat+BasW1C.mat
                      +OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]+OC[,6]+OC[,7]+OC[,8]
                       + OC[,9]+OC[,10]+OC[,11]+OC[,12]+OC[,13]+OC[,14])$fitted.values
    
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
#####Digresssion: IV diagnostics

ivreg.est <- ivreg(Y.bar~XX,~ZZ)
summary(ivreg.est, diagnostics = TRUE)
#First stage results
first <- lm(X~ZZ)
summary(first)



#redefine ZZ so that it does not include the excluded instrument
ZZ <- cbind(W1,W1C, BrDum)
first_wo <- lm(X~ZZ)
#summary(first)
stargazer(first_wo, first, title="First Stage Results (two stage least squares)", 
          omit.table.layout = "n",  
          order=c("ZZW2", "ZZW1", "ZZW1C", "ZZpcwork", "ZZost"), 
          covariate.labels=c("y2k-Consult", "ln(Labour)", "ln(Capital)", "pcwork", 
                              "ost", "consumer goods", "chemical industry", 
                             "other raw materials", "metal and machine construction", 
                             "electrical engineering", "precision instruments", "automobile", 
                             "wholesale trade", "retail trade", "transportation and postal services", 
                             "banks and insurances", "electronic processing and telecommunication", 
                             "other business-related services" ), 
          omit.stat=c("rsq","ser"), align=TRUE, single.row = TRUE, dep.var.labels="outsource" ,  
          out = paste(outfilepath, "/Tab4_First_stage2.tex", collapse="TURE", sep="")
          )

#redefine ZZ again to its original form
ZZ <- cbind(W1,W1C, W2, BrDum) 


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
                      + OC[,12]+OC[,13]+OC[,14])$fitted.values
    cond.mean.X <- lm(Xb~BasW1.mat+BasW1C.mat
                      +OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]+OC[,6]+OC[,7]+OC[,8]
                      + OC[,9]+OC[,10]+OC[,11]+OC[,12]+OC[,13]+OC[,14])$fitted.values

 

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

Y= subset(Y_r, Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99)   # for listwise deletion we also condition on Y being observed
W2= subset(W2_r, Y_r>-99 &X_r>-99 & W1_r>-99 & W2_r> -99& W1C_r> -99 &  pcwork_r > -99)
W1= subset(W1_r, Y_r>-99 &X_r>-99 & W1_r>-99 & W2_r> -99& W1C_r> -99 &  pcwork_r > -99)
W1C = subset(W1C_r, Y_r>-99 &X_r>-99 & W1_r>-99 & W2_r> -99& W1C_r> -99 &  pcwork_r > -99)
X= subset(X_r, Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99& W1C_r> -99 &  pcwork_r > -99)
pcwork=subset(pcwork_r, Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 &  pcwork_r > -99)
ost = subset(ost_r, Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99& W1C_r> -99 &  pcwork_r > -99)


br1 <- subset(br1_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br1_raw > -99 &  pcwork_r > -99)
br2 <- subset(br2_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br2_raw > -99 &  pcwork_r > -99)
br3 <- subset(br3_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br3_raw > -99 &  pcwork_r > -99)
br4 <- subset(br4_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br4_raw > -99 &  pcwork_r > -99)
br5 <- subset(br5_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br5_raw > -99 &  pcwork_r > -99)
br6 <- subset(br6_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br6_raw > -99 &  pcwork_r > -99)
br7 <- subset(br7_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br7_raw > -99 &  pcwork_r > -99)
br8 <- subset(br8_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br8_raw > -99 &  pcwork_r > -99)
br9 <- subset(br9_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br9_raw > -99 &  pcwork_r > -99)
br10 <- subset(br10_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br10_raw > -99 &  pcwork_r > -99)
br11 <- subset(br11_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br11_raw > -99 &  pcwork_r > -99)
br12 <- subset(br12_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br12_raw > -99 &  pcwork_r > -99)
br13 <- subset(br13_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br13_raw > -99 &  pcwork_r > -99)
br14 <- subset(br14_raw,Y_r>-99 & X_r>-99 & W1_r>-99 & W2_r> -99 & W1C_r> -99 & br14_raw > -99 &  pcwork_r > -99)

BrDumDum <- cbind(br1, br2, br3, br4, br5, br6, br7, br8, br9, br10, br11, br13)
BrDum <- cbind(ost,  pcwork, BrDumDum)



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
                  + OC[,12]+OC[,13]+OC[,14])$fitted.values
cond.mean.X <- lm(X~BasW1.mat+BasW1C.mat
                  +OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]+OC[,6]+OC[,7]+OC[,8]
                  + OC[,9]+OC[,10]+OC[,11]+OC[,12]+OC[,13]+OC[,14])$fitted.values

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

# vectors to save the  calculated statistics for each subsample
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


  
  
  ######### FULL MODEL: partially linear model

  OC = BrDumb[,]
  BasW1.mat = bs(W1b, df=df1.c, degree=degree1.c)
  BasW1C.mat = bs(W1Cb, df=df2.c, degree=degree2.c)
  
  cond.mean.Y <- lm((Yb)~BasW1.mat+BasW1C.mat+OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]
                    + OC[,6]+OC[,7]+OC[,8]+ OC[,9]+OC[,10]+OC[,11]
                    + OC[,12]+OC[,13]+OC[,14])$fitted.values
  cond.mean.X <- lm(Xb~BasW1.mat+BasW1C.mat
                    +OC[,1]+OC[,2]+OC[,3]+OC[,4]+OC[,5]+OC[,6]+OC[,7]+OC[,8]
                    + OC[,9]+OC[,10]+OC[,11]+OC[,12]+OC[,13]+OC[,14])$fitted.values
  
  

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








##################################################################################
###  Include the code with imputations
##################################################################################
#code returns results in res.impute.count and in res.impute.tsls.count

imputationsourcefileNamestub = "multiple_imputation_estimation.R"

ImputationSourceFile <- paste(c(imputationsourcepath, imputationsourcefileNamestub), collapse="")


source(ImputationSourceFile)




#for semipar
ResultsVec[4] <- res.impute.count[1,1]
Results.SE[4] <- res.impute.count[1,2]



#mi results for linear model with controls
ResultsVec[11] <- res.impute.tsls.count[1,1]
Results.SE[11] <- res.impute.tsls.count[1,2]





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




################## Create Table with estimation results of the following structure
# Table Results1 (with controls)
#
#                         2SLS (linear)                        |                      semiparametric
#--------------------------+--------------------+--------------+--------------------------+--------------------+--------------
# listwise deletion (MCAR) |  imputation (MAR)  |  Correction  | listwise deletion (MCAR) |  imputation (MAR)  |  Correction  |

#define Table Results1 Vector
Results1 <- matrix(nrow=3, ncol=6)


#Coefficients
#linear (Tsls)
Results1[,1] <- Results[,2] #listwise deletion
Results1[,2] <- Results[,11] # imputation
Results1[,3] <- Results[,6] # with correction
#Partially linear
Results1[,4] <- Results[,8] #listwise deletion
Results1[,5] <- Results[,4] # imputation
Results1[,6] <- Results[,10] # with correction

colnames(Results1, do.NULL = TRUE, prefix = "col")
colnames(Results1) <- c("Listwise Deletion (MCAR)", "Imputation (MAR)", "Correction", "Listwise Deletion (MCAR)", "Imputation (MAR)", "Correction")

rownames(Results1, do.NULL = TRUE, prefix = "row")
rownames(Results1) <- c("coefficient", "SE", "N")

Results1

Results1Table <- paste(c(outfilepath, "Tab5_Main_Results", ".tex"), collapse="")


mainresults_digits <- matrix(c(3,3,0), nrow = dim(Results1)[1], ncol=dim(Results1)[2]+1)


print(xtable(Results1, digits=mainresults_digits), type="latex", file=Results1Table)






################################################################################
##  print summary tables 
################################################################################

###   Industry distribution in estimation sample
##################################################################
myTable <- function(x){
  myDF <- data.frame(table(x))
  myDF$Prop <- round(prop.table(myDF$Freq),4)*100
  myDF <- myDF[,2:3]
  myDF <- rbind(myDF, colSums(myDF))
}

DATA.sample <- subset(DATA, v_out_b >=0 & lnL >=0 & ber_y2k >=0 & lnC_raw_zero>=0 & ost>= 0 & pcwork>=0)
branche <- factor(cbind(DATA.sample$branche1, DATA.sample$branche2, DATA.sample$branche3, DATA.sample$branche4,
                             DATA.sample$branche5, DATA.sample$branche6, DATA.sample$branche7,
                             DATA.sample$branche8, DATA.sample$branche9, DATA.sample$branche10, 
                             DATA.sample$branche11, DATA.sample$branche12, DATA.sample$branche13, 
                             DATA.sample$branche14)%*%(1:14))


industryTable <- myTable(branche)
rownames(industryTable) <- c("consumer goods", "chemical industry", "other raw materials", "metal and machine construction",
                             "electrical engineering", "precision instruments", "automobile", "wholesale trade", "retail trade",
                             "transportation \\& postal services", "banks \\& insurances", "technical services", 
                             "other business-related services", "Total" )
colnames(industryTable) <- c("Obs.", "Percent")

industryTable_digits <- matrix(c(0,0,2), nrow = dim(industryTable)[1], ncol=dim(industryTable)[2]+1, byrow=TRUE)
industry_distribution_table <- xtable(industryTable, digits=industryTable_digits, caption="Industry Distribution")

options(xtable.sanitize.text.function=identity)                    
print(industry_distribution_table, type="latex", caption.placement="top", file=paste(outfilepath,"Tab1_Industry_Distribution.tex", collapse="True", sep=""))





#### Raw data : sum stats 
##################################################################

#### Read the SYNTHETIC DATA:
# DATA.new <- read.csv(paste(projectpath, "DATA/", "!ikt2004_anonym_V4.csv", collapse="True", sep=""))

#### Read the ORIGINAL DATA:
DATA.new <- read.csv(paste(projectpath, "DATA/", "!ikt2004_start_R_IV_missings_processed_addvars.csv", collapse="True", sep=""))


DATA.new.SUM.raw <- subset(DATA.new, select=c(lnval, ums03, mkost_vh, lnC_raw_zero, lnL, v_out_b, ber_y2k, pcwork, ost))
DATA.new.SUM.raw[DATA.new.SUM.raw < 0] <- NA


#replace sales by sales / 1000 for representation in table
DATA.new.SUM.raw["ums03"] <- DATA.new.SUM.raw["ums03"] / 1000

#gen variable for number of obs when both, sales and costs, are missing
DATA.new.SUM.raw$salescostmiss <- 1
DATA.new.SUM.raw$salescostmiss[is.na(DATA.new.SUM.raw$ums03) & is.na(DATA.new.SUM.raw$mkost_vh)] <- NA
DATA.new.SUM.raw <- DATA.new.SUM.raw[,c(1,2,3,10,4,5,6,7,8,9)]

summary_stats_new_raw <- basicStats(DATA.new.SUM.raw) [c("nobs", "NAs", "Mean", "Stdev", "Median", "Minimum", "Maximum"),]
#set number of digits in rows
mdat <- matrix(c(rep(0,(2*11)),rep(3,(5*11))), nrow = 7, ncol=11, byrow=TRUE)
sumstats_table_new_raw <- xtable(summary_stats_new_raw, digits=mdat, caption="Summary Statistics: Raw Data")


#change column headings
names(sumstats_table_new_raw) <- c('ln(prod)','sales (in million Euros)','cost share',
                                   'sales \\& cost share missing', 'ln(Capital)', 'ln(Labor)', 'outsource', 'y2k-Consult',
                                   'pcwork', 'ost')

# cost share = share of sales attributed to intermediate inputs

options(xtable.sanitize.text.function=identity)

align(sumstats_table_new_raw) <- "p{2cm}|p{2cm}p{2cm}p{2cm}p{2cm}p{2cm}p{2cm}p{2cm}p{1,5cm}p{1,5cm}p{1,5cm}"


print(sumstats_table_new_raw, type="latex", caption.placement="top", file=paste(outfilepath,"Tab2_Summary_table_raw_data.tex",  collapse="True", sep=""))







#### estimation sample : sum stats
##################################################################

DATA.new.SUM <- subset(DATA, v_out_b >=0 & lnL >=0 & ber_y2k >=0 & lnC_raw_zero>=0 & ost>= 0 & pcwork>=0 ,
                       select=c(lnval, ums03, mkost_vh, lnC_raw_zero, lnL, v_out_b, ber_y2k, pcwork, ost))
DATA.new.SUM[DATA.new.SUM < 0] <- NA
#replace sales by sales / 1000 for representation in table
DATA.new.SUM["ums03"] <- DATA.new.SUM["ums03"] / 1000


#gen variable for number of obs when both, sales and costs, are missing
DATA.new.SUM$salescostmiss <- 1
DATA.new.SUM$salescostmiss[is.na(DATA.new.SUM$ums03) & is.na(DATA.new.SUM$mkost_vh)] <- NA
DATA.new.SUM <- DATA.new.SUM[,c(1,2,3,10,4,5,6,7,8,9)]


summary_stats_new <- basicStats(DATA.new.SUM) [c("nobs", "NAs", "Mean", "Stdev", "Median", "Minimum", "Maximum"),]



#set number of digits in rows
mdat <- matrix(c(rep(0,(2*11)),rep(3,(5*11))), nrow = 7, ncol=11, byrow=TRUE)

sumstats_table_new <- xtable(summary_stats_new, digits=mdat, caption="Summary Statistics: Estimation Sample")

#change column headings
names(sumstats_table_new) <- c('ln(prod)','sales (in million Euros)','cost share',
                               'sales \\& cost share missing', 'ln(Capital)', 'ln(Labor)', 'outsource', 'y2k-Consult',
                               'pcwork', 'ost')

# cost share = share of sales attributed to intermediate inputs
options(xtable.sanitize.text.function=identity)

align(sumstats_table_new) <- "p{2cm}|p{2cm}p{2cm}p{2cm}p{2cm}p{2cm}p{2cm}p{2cm}p{1,5cm}p{1,5cm}p{1,5cm}"


print(sumstats_table_new, type="latex", caption.placement="top", file=paste(outfilepath,"Tab3_Summary_table_new.tex", collapse="True", sep=""))

