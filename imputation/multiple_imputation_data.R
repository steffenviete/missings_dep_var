# ##################################################################
# #
# # multiple_imputation_data      
# #
# ##################################################################

## Content:
# - generate multiply imputed data set based on the original estimation sample
# - sotres the file "imputed_sample.rda"

library(mi)
library(Amelia)

projectpath = "d:/mirror_h/Copy_Missings/!_Replication_Package/" 



#################################
### Read DATA ###
################################
# for the anonymized data
 #DATA <- read.csv(paste(projectpath, "DATA/", "!ikt2004_anonym_V4.csv", collapse="True", sep=""))    #fixme_: set final path
 DATA <- read.csv(paste(projectpath, "DATA/", "!ikt2004_start_R_IV_missings_processed_addvars.csv", collapse="True", sep=""))    #fixme_: set final path




#############################
### 3. multiple imputation
#############################

W1_r <- DATA$lnL #log Labor
W1C_r <- DATA$lnC_raw_zero # log Capital
Y_r <- DATA$lnval #Log Value Added over labor
X_r <- DATA$v_out_b #I-outsourcing
W2_r <- DATA$ber_y2k #I_y2K_consulting

pcwork_r <- DATA$pcwork # share of employees working predominantly with PCs

# specify additional other controls
ost_r <- DATA$ost         # east Germany indicator



#copy DATA keeping only variables necessary for imputation
# also condition on variables with low missingness being observed:  (v_out_b ber_y2k)
DATA.IMP <- subset(DATA, v_out_b >=0 & ber_y2k >=0 & lnC_raw_zero>=0 & pcwork>=0 & ost>=0, select=c(lnval, lnC_raw_zero, lnL, v_out_b, ber_y2k, ost, pcwork, branche1:branche14))


#assgin "NA" to missing values to create a missings data frame object
DATA.IMP[DATA.IMP < 0] <- NA


#create missing_data.frame object 
mdf <- missing_data.frame(DATA.IMP)

#check classes of missing_variables
show(mdf)

#########Specify Transformation of variables
mdf <- change(mdf, y = c("lnval", "lnC_raw_zero", "lnL", "pcwork") , what = "transformation",to = c( "identity", "identity", "identity", "identity"))



# now impute:
rm(DATA.IMP)
options(mc.cores = 2)


#
imputations <- mi(mdf, n.iter = 100, n.chains = 5, max.minutes = 20, seed = 355)
# By default, mi() assumes linearity between the outcomes and predictors.

show(imputations)


#Check that the mean of each completed variable is roughly the same for each of the 4 chains.
round(mipply(imputations, mean, to.matrix = TRUE), 3)


# check the R-hat statistic
Rhats(imputations)



#############################################################################
##   save the mi-Object that contains the 5 imputation chains
#############################################################################
save(imputations, file = paste(projectpath, "DATA/", "imputed_sample.rda", collapse="True", sep=""))


