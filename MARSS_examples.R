# Examples from "Analysis of multivariate time-series using the MARSS package. UserGuide.pdf"
library(MARSS)

plot_residuals <- function(dat,kem,Z.model)
{
  observations = t(dat)
  matrix.of.biases = matrix(coef(kem, type="matrix")$A,
                            nrow=nrow(observations),ncol=ncol(observations),byrow=T)
  par(mfrow=c(2,3))
  j = as.integer(Z.model)
  for(i in 1:n){
    xs = kem$states[j[i],]
    resids = observations[,i]-matrix.of.biases[,i]-xs
    # plot(resids[!is.na(resids[,i]),i],ylab="residuals")
    plot(resids[!is.na(resids)],ylab="residuals")
    title(rownames(dat)[i])
  }
  par(mfrow=c(1,1))
}

# Examples from Sec. 7: Combining multi-site data to estimate regional population trends
dat = t(harborSealWA)   # transpose
years = dat[1,]    # 1st row of dat contains years
n = nrow(dat)-1
dat = dat[2:nrow(dat),]   # data (log counts), without years

# A MARSS model:
# states: x_t = B*x_{t-1} + u + w_t, where w_t ~ Normal(0,Q)
# observations: y_t = Z*x_t + a + v_t, where v_t ~ Normal(0,R)   # the bias is added, because y_t are log-counts rather than counts!

# Case 1: a single (m=1) well-mixed population with i.i.d. errors, with observations at n=5 locations:
# total population model: n_t = exp(u + w_t)* n_{t-1}
# Taking log, we obtain:
# x_t = x_{t-1} + u + w_t, w_t ~ Normal(0,q), where x_t is now a 1x1 matrix (single population), q is a scalar process variance
# y_t are assumed to be independent observations of the same population - see eq. (7.3) of the UserGuide.
# Model_1: assume the observation errors are independent normal random variables, with identical variances, i.e., require that R=r*I, where I is the n x n unit matrix. 
# Z = (1,1,1,1,1)
Z.model = factor(c(1,1,1,1,1))
R.model = "diagonal and equal"
# Fitting the model:
kem1 = MARSS(dat, model=list(Z=Z.model, R=R.model))
# make figure:
matplot(years, t(dat), xlab="", ylab="index of log abundance", pch=c("1","2","3","4","5"), ylim=c(5,9), bty="L")
lines(years,kem1$states,type="l",lwd=2)   # states x_t estimates from the Kalman filter
lines(years, kem1$states - 1.96*kem1$states.se, type="l", lwd=1, lty=2, col='red')   # 95% CI of x_t
lines(years, kem1$states + 1.96*kem1$states.se, type="l", lwd=1, lty=2, col='red')
title("Observations and total population estimate (Model 1)",cex.main=.9)
# Note that one of the biases (1_1) cannot be estimated, so the algorithm arbitrarily choses a_1=0, hence the population estimate is scaled to the first 
# observation time series, i.e., y_1

coef(kem1, type="vector")  #show the estimated parameter elements as a vector
#show estimated elements for each parameter matrix as a list
coef(kem1)
kem1$logLik   #show the log-likelihood
cat("Model 1 AIC:",kem1$AIC,"\n")  #show the AIC

#plot residuals
plot_residuals(dat, kem1, Z.model)

# Model_2: assume the observation errors are independent normal random variables, with 5 unique variances
# Z = (1,1,1,1,1)
Z.model = factor(c(1,1,1,1,1))
R.model = "diagonal and unequal"
# Fitting the model:
kem2 = MARSS(dat, model=list(Z=Z.model, R=R.model))
# make figure:
matplot(years, t(dat), xlab="", ylab="index of log abundance", pch=c("1","2","3","4","5"), ylim=c(5,9), bty="L")
lines(years,kem2$states,type="l",lwd=2)   # states x_t estimates from the Kalman filter
lines(years, kem2$states - 1.96*kem2$states.se, type="l", lwd=1, lty=2, col='red')   # 95% CI of x_t
lines(years, kem2$states + 1.96*kem2$states.se, type="l", lwd=1, lty=2, col='red')
title("Observations and total population estimate (Model 2)",cex.main=.9)
# Note that one of the biases (1_1) cannot be estimated, so the algorithm arbitrarily choses a_1=0, hence the population estimate is scaled to the first 
# observation time series, i.e., y_1

coef(kem2, type="vector")  #show the estimated parameter elements as a vector
#show estimated elements for each parameter matrix as a list
coef(kem2)
kem2$logLik   #show the log-likelihood
cat("Model 2 AIC:",kem2$AIC,"\n")  #show the AIC

# Model 2's AIC is less than that of Model 1, meaning that Model 2 (with unique observation error variances) is a better model.

#plot residuals
plot_residuals(dat,kem2, Z.model)

# The fact that residuals for both Model 1 and Model 2 have temporal trends indicates that the single population model is inconsistent with the data.
# So we now change the assumption about the structure of the latent population x_t: assume 2 independent subpopulations, "north" and "south", which share their 
# growth rate u, and process variance q, since they share similar environments and prey base.
# Independent populations implies that the off-diagonal elements of population covariance matrix are 0.
# We also assume that y_1 and y_2 are observations of x_n, and y_3, y_4, and y_5 are observations of x_s - this specifies the Z matrix:
Z.model = factor(c(1,1,2,2,2))
U.model = "equal"   # see the assumption about u_n and u_s. Default U.model is "unequal"
Q.model = "diagonal and equal"  # see above
R.model = "diagonal and equal"

kem3 = MARSS(dat, model=list(Z=Z.model, R=R.model, U=U.model, Q=Q.model))
#plot residuals
plot_residuals(dat,kem3,Z.model)
# The residuals look better (less temporal dependence for most of them), but the Hood Canal (HC) residuals are still temporally correlated.
cat("Model 3 (2 independent subpopulations with same population and observation parameters) AIC:",kem3$AIC,"\n")   # show the AIC for the 2-subpopulation model

# Model 4: 5 subpopulations
Z.model = factor(c(1,2,3,4,5))   # each observation is of the corresponding subpopulation
U.model = "equal"   # assume all subpopulations share the same growth rate
Q.model = "diagonal and equal"   # subpopulations are independent, with the same process variance
R.model = "diagonal and unequal"  # observations are independent (because they are of independent populations), with independent variances
kem4 = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model))
#plot residuals
plot_residuals(dat,kem4,Z.model)
# The residuals look even better (less temporal dependence for most of them), but the Hood Canal (HC) residuals are still somewhat temporally correlated.
cat("Model 4 (5 independent subpopulations with same population parameters, but independent observation variances) AIC:",kem4$AIC,"\n")   # show the AIC for the 2-subpopulation model

# Try multiple random initial conditions:
# kem4_mcinit = MARSS(dat, model=list(Z=Z.model, U=U.model, Q=Q.model, R=R.model), control=list(MCInit=TRUE))
# #plot residuals
# plot_residuals(dat,kem4_mcinit,Z.model)
# # The residuals look even better (less temporal dependence for most of them), but the Hood Canal (HC) residuals are still somewhat temporally correlated.
# cat("Model 4 with multiple initial conditions (5 independent subpopulations with same population parameters, but independent observation variances) AIC:",kem4_mcinit$AIC,"\n")   # show the AIC for the 2-subpopulation model


##### DFA example
rm(list=ls())
# load the data (there are 3 datasets contained here)
data(lakeWAplankton)
# we want lakeWAplanktonTrans, which has been transformed
# so the 0s are replaced with NAs and the data z-scored
dat = lakeWAplanktonTrans
# use only the 10 years from 1980-1989
plankdat = dat[dat[,"Year"]>=1980 & dat[,"Year"]<1990,]
plankdat = ts(plankdat, start = c(1980, 1), end = c(1989,12), frequency = 12)
# create vector of phytoplankton group names
phytoplankton = c("Cryptomonas", "Diatoms", "Greens",
                  "Unicells", "Other.algae")
# get only the phytoplankton
dat.spp.1980 = plankdat[,phytoplankton]

# transpose data so time goes across columns
dat.spp.1980 = t(dat.spp.1980)
# get number of time series
N.ts = dim(dat.spp.1980)[1]
# get length of time series
TT = dim(dat.spp.1980)[2]

# Standardize the data:
Sigma = sqrt(apply(dat.spp.1980, 1, var, na.rm=TRUE))  # sd of each phytoplankton time series
y.bar = apply(dat.spp.1980, 1, mean, na.rm=TRUE)       # means
dat.z = (dat.spp.1980 - y.bar) * (1/Sigma)     # standardized phytoplankton time series
rownames(dat.z) = rownames(dat.spp.1980)

# Set up the DFA model in MARSS:
# Assume a model with 5 observed time series and 3 hidden trends (factors):
Z.vals = list("z11", 0, 0, 
              "z21", "z22", 0,
              "z31", "z32", "z33",
              "z41", "z42", "z43",
              "z51","z52","z53")   # numeric values are recognized as fixed values, character values - as estimated values
Z = matrix(Z.vals, nrow=N.ts, ncol=3, byrow = TRUE)

Q = B = diag(1,3)   # set both B and Q to identity matrix

# assume diagonal R matrix with independent diagonal elements
# alternative: R = "diagonal and unequal"
R.vals = list(
  "r11",0,0,0,0,
  0,"r22",0,0,0,
  0,0,"r33",0,0,
  0,0,0,"r44",0,
  0,0,0,0,"r55")
R = matrix(R.vals, nrow=N.ts, ncol=N.ts, byrow=TRUE)

x0 = U = A = "zero"    # mean initial state x0, trend U, and observation bias A are all set to 0 matrices in DFA (?)

V0 = diag(5,3)    # covariance matrix of x0

dfa.model = list(Z=Z, A="zero", R=R, B=B, U=U, Q=Q, x0=x0, V0=V0)   # list of model parameters to pass to MARSS()
cntl.list = list(maxit=1000)      # list of control options to pass to MARSS(). Here we are limiting the number of iterations, for speed. 
                                # Note that this can prevent convergence of the model estimates of states and parameters

# Fit the DFA mdoel:
kemz.DFA.3 = MARSS(dat.z, model=dfa.model, control=cntl.list)

# make plots of data with model fits:
Z.vals = c(kemz.DFA.3$par$Z["z11",], 0, 0, 
              kemz.DFA.3$par$Z["z21",], kemz.DFA.3$par$Z["z22",], 0,
              kemz.DFA.3$par$Z["z31",], kemz.DFA.3$par$Z["z32",], kemz.DFA.3$par$Z["z33",],
              kemz.DFA.3$par$Z["z41",], kemz.DFA.3$par$Z["z42",], kemz.DFA.3$par$Z["z43",],
              kemz.DFA.3$par$Z["z51",],kemz.DFA.3$par$Z["z52",],kemz.DFA.3$par$Z["z53",])
Z = matrix(Z.vals, nrow=N.ts, ncol=3, byrow = TRUE)

kemz.DFA.3$fit = Z %*% kemz.DFA.3$states

par(mfrow=c(3,2))
for (j in 1:N.ts)
{
  species = rownames(dat.z)[j]
  # species_ts = ts(MARSSsimulate(kemz.DFA, tSteps = length(plankdat[,species]))$sim.data[j,,1], start = c(1980, 1), end = c(1989,12), frequency = 12)
  species_ts = ts(kemz.DFA.3$fit[j,], start = c(1980, 1), end = c(1989,12), frequency = 12)
  ts.plot(plankdat[,species], species_ts, type='b', gpars=list(xlab="year", ylab="abundance index", lty=c(0,1), lwd=c(1, 3), pch=c(19,20), col=c('blue','black')))
  title(species)
}

par(mfrow=c(1,1))


# A 2-trend (2-factor) DFA model, using the fprm="dfa" shortcut:
model.list = list(m=2, R="diagonal and unequal")
kemz.DFA.2 = MARSS(dat.z, model=model.list, z.score=TRUE, form="dfa", control=cntl.list)
# Compare AIC values of the 2-factor and 3-factor models:
print(cbind(model=c("3 factors", "2 factors"), AIC=round(c(kemz.DFA.3$AIC, kemz.DFA.2$AIC)), AICc=round(c(kemz.DFA.3$AICc, kemz.DFA.2$AICc))), quote=FALSE)

# Examine a larger suite of possible models:
# Test from 1 to 4 underlying trends (factors) and four different structures for the R matrix:
# 1. Same variance and no covariance ("diagonal and equal")
# 2. different variances & no covariance ("diagonal and unequal")
# 3. same variances & same covariances ("equalvarcov")
# 4. different variances & covariances ("unconstrained")
# set new control params:
cntl.list = list(minit=100, maxit=5000, allow.degen=FALSE)
# set up forms of R matrices:
levers.R = c("diagonal and equal", "diagonal and unequal", "equalvarcov", "unconstrained")
model.data = data.frame()
# fit all the models and store results (takes a long time to run):
for (R in levers.R){
  for (m in 1:(N.ts-1)){  # try m=1, 2, 3, 4
    dfa.model = list(A='zero', R=R, m=m)
    print(dfa.model)
    kemz = MARSS(dat.z, model=dfa.model, control=cntl.list, form="dfa", z.score=TRUE)
    model.data <- rbind(model.data, data.frame(R=R, 
                                               m=m, 
                                               logLik=kemz$logLik,  # log-likelihood of the model
                                               K=kemz$num.params,   # number of estimated parameters in the model
                                               AICc=kemz$AICc,
                                               stringsAsFactors = FALSE))
    assign(paste("kemz", m, R, sep="."), kemz)   # assign the model 'kemz' to kemz.[m].[R]
  }  # end of m loop
}  # end of R loop

# Sort the model selection results by AICs, and compute the Delta AICs and AIC.weight:
sorted.AICs = order(model.data$AICc)
model.data = model.data[sorted.AICs,]   # sort models by their AICs. The lower the AIC, the better is a model
model.data$delta.AICs = model.data$AICc - model.data$AICc[1]   # add Delta AICs
model.data$rel.like = exp(-1*model.data$delta.AICs/2)    # add relative likelihoods for the models
model.data$AIC.weight = round(model.data$rel.like/sum(model.data$rel.like), 3)   # add AIC.weights for models = relative likelihood of the model/sum of relative likelihoods for all models
model.data$AIC.weight.cum = round(cumsum(model.data$AIC.weight), 2)

print(model.data)   # display the model selection results. Note that this is model selection based on model complexity and fit to the data, not based on 
                    # how well the model forecasts the future


