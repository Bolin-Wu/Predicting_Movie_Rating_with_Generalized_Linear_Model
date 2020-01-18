library(tidyverse)
library(mice)


setwd("/Users/bolin/Documents/Master/GLM/exam/exam")

data_complete_csv = read_csv("MOVIE.csv",col_types = cols(
  belongs_to_collection = col_factor(),
  budget = col_double(),
  genres = col_factor(),
  original_language = col_character(),
  popularity = col_double(),
  production_companies = col_character(),
  production_countries = col_character(),
  revenue = col_double(),
  runtime = col_integer(),
  status = col_character(),
  vote_average = col_double(),
  actor = col_character(),
  director = col_character()
))
#----------------------------------------------------#
# If delet the observations that contain NA value.
# a_omit = na.omit(data_complete_csv)
#summary(a_omit) # only 3017 obs remain
#---------------------------------------------------------#
######------------------ Data Cleaning ---------------------######
#---------------------------------------------------------#
#  MI the missing data 
attach(data_complete_csv)
nlevels(factor(genres)) #1295
nlevels(factor(original_language)) #92
nlevels(factor(production_companies)) #22668
nlevels(factor(production_countries)) #2389
nlevels(factor(title)) #42267
nlevels(factor(actor)) # 42639
nlevels(factor(director)) # 19043
nlevels(factor(status)) #6
miss_value_pattern = md.pattern(data_complete_csv) 
miss_value_percentage<- VIM::aggr(data_complete_csv, col=c('green','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, 
                                  ylab=c("Proportion of missing","Missing pattern with proportion"))  #### Green is observed and red is missing.
# Choose site 1 as dependent variavle. The missing percentage is too high in site 2 (more than 67%)
detach(data_complete_csv)


nlevels(factor(data_complete_csv $ production_countries)) 
# More than 17000 levels for genres
# More than 40000 levels for actors
# More than 40000 levels for directors
# More than 40000 levels for production_companies
# More than 40000 levels for title
# More than 2389 levels for 2389
# feels like can not use them in further modeling because it requires big computational power to deal with such many catagories, 
# but will try to see how much will leave after data cleaning.
a1_csv = data_complete_csv  %>% 
  filter(status=="Released") %>% 
  select("belongs_to_collection","budget","popularity","revenue","runtime" 
         ,"vote_average","actor","director" ,"production_companies","vote_count","genres")

md.pattern(a1_csv) 
########  Proportion view, instead of frequencies
aggr_plot <- VIM::aggr( a1_csv, col=c('green','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, 
                       ylab=c("Proportion of missing","Missing pattern with proportion"))  #### Green is observed and red is missing.

# Select the variables for imputation
a1_csv_1 = a1_csv %>% 
  select("runtime","budget","popularity","vote_count" ) #%>% #,"vote_average" ,"director" ,"revenue","production_companies","actor","belongs_to_collection"
#filter( between(row_number(), 1, 1000)
# Can not include the other variables,
# error 1 : Cannot include factors with lots of levels, saying>50, "Maximum number of categories (50) exceeded."
# error 2 :computationally singular error, might caused by the case of linearly dependent columns.
#---------------------------------------#
# the following is used if want to impute missing data for catagorical data #
#---------------------------------------#
#imp.method = c(
#  belongs_to_collection = "logreg",
#  budget = "pmm",
#  popularity = "pmm",
#  revenue = "pmm",
#  runtime = "pmm",
#  vote_average = "pmm",
#  actor = "polyreg",
#  director = "polyreg",
#  production_companies = "polyreg"
#)

# However, after tring serveal times, my computer cannot MI the catagorical data in this dataset because there are too many and
# R session is aborted automatically.
# It can not deal with imputation model with lots of levels, saying >50.
#---------------------------------------#

md.pattern(a1_csv_1) 

# set virtual memory to be very large, to oleve the "vector memory exhausted" error that is encountered when running mice function.
usethis::edit_r_environ("project") 
# say: R_MAX_VSIZE=1000Gb 

timestart<-Sys.time();
imp <- mice(a1_csv_1, seed=12345, m=5, maxit=10, print=FALSE);
timeend<-Sys.time()
runningtime<-timeend-timestart 
# error 1 :Calculate the running time, for the original dataset, takes 6 minutes but still have the problem of "long vectors not supported yet"
# error 2 : Cannot include factors with lots of levels, saying>50, "Maximum number of categories (50) exceeded."
# computationally singular error, might caused by the case of linearly dependent columns.
# the conputational power is limited.
# Time difference of 5.327094 mins

# diagnose the imputation value
imp_data = imp$imp
plot(imp)
lattice::densityplot(imp, layout = c(2, 1))
stripplot( x=imp, pch=16 )   

# For MAR, we want to have very similar density distribution.
# Red, density estimation of imputed values, blue, the observed values


# -------Test: Mutate the imputed data to the model data set.-------#
data_model = a1_csv %>% 
  mutate(runtime_nona = ifelse(is.na(runtime),imp_data $runtime[,2],runtime)) %>% 
  mutate(runtime_nona = 0 ) %>% 
  mutate(budget_nona = 0) %>% 
  select(runtime,runtime_nona )


data_model[which(is.na(data_model$runtime)),"runtime_nona" ] =  imp_data $runtime[,2]
data_model[which(is.na(data_model$runtime)),] 
# compare the column "runtime_nona" with imp_data $runtime[,2]. They are the same, which means that this precoess is successful.
md.pattern(data_model)
# There is no NA in "runtime_nona".

data_model[which(is.na(data_model$runtime)),"runtime" ] =  imp_data $runtime[,2]
md.pattern(data_model)
# ----------------------Test Over---------------------------------#


data_model = a1_csv
data_model[which(is.na(data_model$runtime)),"runtime" ] =  imp_data $runtime[,2]
data_model[which(is.na(data_model$budget)),"budget" ] =  imp_data $budget[,2]
md.pattern(data_model)
# No missing value in budget and runtime
data_model_nona = na.omit(data_model) 
md.pattern(data_model_nona)
# No missing value in all the other variables.
nlevels(factor(data_model_nona$director)) #14999

# Cleanthe outlier value.
summary(data_model_nona) # see if there are any unreasonable data.
data_model_nona_nooutlier =  data_model_nona  %>% 
  #filter( between(runtime, 40, 210)) %>% 
  #filter(runtime > 40) %>%  # according to AFI
  filter(vote_count>0) %>% 
  filter(popularity>0) %>% 
  filter(revenue > 0) # It does not make sense for a released movie to have 0 revenue.21341 0 value
nlevels(factor(data_model_nona$revenue)) 
view(data_model_nona_nooutlier)
summary(data_model_nona_nooutlier) # inthe new data set without outlier data, NA increase to 1746.
nrow(data_model_nona_nooutlier) # 6897 obs in the cleaned data, much more than 3017 obs ( if treat the missing data as MCAR).
# ------------------ Data Cleaning Finish ------------------ #


#------------------------------------------------------------#
######------------------ The modeling -------------------------######
#------------------------------------------------------------#


# Predict if the movie is a series movie
#----- Test with different model with the dataset---------#
#nlevels(factor(data_model_nona_nooutlier$production_countries)) # 633, much lower than other catagorical variable, so that could try to do LMM using this as reference
#summary(factor(data_model_nona_nooutlier$belongs_to_collection)) 
#library(lme4);library(hglm);library("mdhglm")

#-------------------- Logit--------------------#
#library(VGAM)
#Logit <- glm( belongs_to_collection ~ budget + popularity + revenue + runtime + vote_average
#              +as.factor(production_countries)+as.factor(production_companies) , 
#               family=binomial(link='logit'), data = data_model_nona_nooutlier )

#Pred.Prob <- predict( Logit, type='response' )
#Pred.Class <- ifelse( Pred.Prob>=0.5, '1', '0' )
#Obs.Class <- data_model_nona_nooutlier$ belongs_to_collection
#Obs.Class <- data_comp_car$choice
#table( Obs.Class, Pred.Class )  

#caret::confusionMatrix( data=factor( Pred.Class ), reference=factor(Obs.Class) )
# Can not use, the accuracy is much lower than the no information rate.
# Also, can not include both production_companies and production_countries, because R can not handle such big catagorical data.
# Turn to do the prediction.
# --------------------------------------------#

####-------------------- LMM --------------------####
# Predict the rating of a movie
summary(factor(data_model_nona_nooutlier$production_countries))
summary(factor(data_model_nona_nooutlier$genres))
summary(factor(data_model_nona_nooutlier$production_companies))
# Seems like country can be the cluster, but the # of observations in each county is different(?)
library(lme4)
lmm.fit = lmer( vote_average ~ as.factor(belongs_to_collection )+ budget + popularity + revenue + runtime 
                +(1|production_countries), data=data_model_nona_nooutlier, REML=TRUE ) #
# Warning message: Some predictor variables are on very different scales: consider rescaling 
# Scale the numeric variables
# lmm.fit_cou = lmer( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                  scale(popularity) + scale(revenue) + scale(runtime )
#                +(1|production_countries), data=data_model_nona_nooutlier, REML=TRUE )

lmm.fit_scale = lmer( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
                  scale(popularity) + scale(revenue) + scale(runtime )
                +(1|genres), data=data_model_nona_nooutlier, REML=TRUE )

# lmm.fit_com = lmer( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                  scale(popularity) + scale(revenue) + scale(runtime )
#                +(1|production_companies), data=data_model_nona_nooutlier, REML=TRUE )

summary(lmm.fit_gen) 
# AIC(lmm.fit_cou,lmm.fit_gen,lmm.fit_com) # 15087.6 genres is the best
#"Correlation of Fixed Effects" is the estimated correlation between the parameter estimators. It is used to investigate multicollinearity.Not close to 1 or -1 

# Residual Plot
par( mfrow=c(1,2))
dev.off()
plot(fitted(lmm.fit_scale),residuals(lmm.fit_scale,type = "pearson")) 
# Have problem of heteroscedasticity!!

# Transform approach to solve the heteroscedasticity. 
# Because other ways, like GLS mothod, Box-Cox transformation method which are often used in OLS estimated model, may not applicable in LMM 
lmm.fit_trans = lmer( vote_average^2 ~ as.factor(belongs_to_collection )+ log(budget) + 
                      log( popularity)+ log(revenue)+ log(runtime )
                    +(1|genres), data=data_model_nona_nooutlier, REML=TRUE )
par( mfrow=c(2,2))
dev.off()
plot(fitted(lmm.fit_trans),residuals(lmm.fit_trans,type = "pearson")) 
# The problem of heteroscedasticity is solved
qqnorm(residuals(lmm.fit_trans,type = "pearson"))
qqline(residuals(lmm.fit_trans,type = "pearson"),col = "red",lwd = 1)

summary(lmm.fit_trans) # correaltion of fix effects are too high, model may be not reliable
# delet "runtime"
lmm.fit_trans = lmer( vote_average^2 ~ as.factor(belongs_to_collection )+ log(budget) + 
                        log( popularity)+ log(revenue)
                      +(1|genres), data=data_model_nona_nooutlier, REML=TRUE )
summary(lmm.fit_trans)
Cofitted(lmm.fit_trans)
sqrt(predict(lmm.fit_trans, data_model_nona_nooutlier))
####  Extracts random intercept and fixed slope
coef(lmm.fit_trans)  
####  Extracts fixed intercept and fixed slope
lmm.fit_trans@beta 
####  Get estimated random effects
# ranef(lmm.fit)  
####  This is the same as our random intercept
# rbind( coef(lmm.fit)$cluster[,1], 
#       lmm.fit@beta [1] + ranef(lmm.fit)$cluster[,1]  )


#-------------------- Prediction Evaluation For LMM--------------#
# Get a MSE estimate using k-fold, 
# Do not use Leave one out cross-validation (LOOCV) which requires the model to be fit n times
# It can be computationally challenging when n is large.
# data_model_lmm = data_model_nona_nooutlier %>% 
#  select("vote_average", "belongs_to_collection","budget", "popularity","revenue","runtime","production_countries")
nrow(data_model_nona_nooutlier) %% 10
n = nrow(data_model_nona_nooutlier)
k = 10 
# shuffle 
set.seed(1234)
folds = sample(rep(1:k, ceiling(n/k)), n, replace = F)
y_hat = numeric(k) 
for (i in 1:k) {
  train = data_model_nona_nooutlier[folds != i, ]
  test = data_model_nona_nooutlier[folds == i, ]
  mod1 = lmer( vote_average^2 ~ as.factor(belongs_to_collection )+ log(budget) + 
                 log( popularity)+ log(revenue)
               +(1|production_countries) , data=train, REML=TRUE )
  y_hat[i] = mean((data_model_nona_nooutlier $ vote_average [folds ==i] -sqrt(predict(mod1, test,allow.new.levels=TRUE)))^2) # test,allow.new.levels=TRUE, then the prediction will use the unconditional (population-level) values for data with previously unobserved levels (or NAs).
}
mean(y_hat) 
# For genres, with runtime  k-fold MSE = 0.6222254 , without runtime, MSE = 0.6768551
# countries, with runtime  0.6455159, without runtime, MSE = 0.7330577
# production_companies,with runtime 0.6549037, without runtime, MSE = 0.70478

#-------------------- Prediction Evaluation Over --------------------#

#-------------------- GLMM --------------------#
#---Gamma-----#

# glmm.Gamma = glmer( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
                      scale(popularity) + scale(revenue) + scale(runtime )
                    +(1|production_countries), 
                    data=data_model_nona_nooutlier,  family=Gamma(), nAGQ=1L,
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
# summary(glmm.Gamma)
# Fail to converge.

## See how many quadrature points are needed
# Est <- NULL
# for( no.agq in 1:11 ){
  
#   cat( 'Estimation with ',no.agq, ' quadrature points.', sep='', '\n' )
#  fit <- glmer( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                  scale(popularity) + scale(revenue) + scale(runtime )
#                +(1|production_countries), 
#                data=data_model_nona_nooutlier, family=Gamma(), nAGQ=no.agq )  
#  Est <- rbind( Est, c(fit@beta,fit@theta))
  
#}
# par( mfrow=c(2,4), mar=c(4.1,4.1,1,1) )
# apply( Est, 2, function(z) plot(1:11,z,type='b', xlab='Number of quadrature points', ylab='Estimates') )

# Model again with quadrature point = 5.
# glmm.Gamma = glmer( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                      scale(popularity) + scale(revenue) + scale(runtime )
#                    +(1|genres), 
#                    data=data_model_nona_nooutlier,  family=Gamma(), nAGQ=5L,
#                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))  
# summary(glmm.Gamma) # wrong converge
# Residual Plot
# plot(residuals(glmm.Gamma))
# qqnorm(residuals(lmm.fit))
# abline(a=0,b=1,col = "red")

# AIC(glmm.Gamma) #17572.05
# BIC(glmm.Gamma) #17572.05
# Still has warning messages

####  Extracts random intercept and fixed slope
# coef(glmm.Gamma)  
####  Extracts fixed intercept and fixed slope
# glmm.Gamma@beta 
####  Get estimated random effects
# ranef(glmm.Gamma)  


#-------------------- Prediction Evaluation For GLMM, Gamma --------------#

# n = nrow(data_model_nona_nooutlier)
# k = 10 
# shuffle 
# set.seed(1234)
# folds = sample(rep(1:k, ceiling(n/k)), n, replace = F)
# y_hat = numeric(k) 
#for (i in 1:k) {
#  train = data_model_nona_nooutlier[folds != i, ]
#  test = data_model_nona_nooutlier[folds == i, ]
#  mod1 = glmer( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                  scale(popularity) + scale(revenue) + scale(runtime )
#                +(1|production_countries), 
#                data=train,  family=Gamma(), nAGQ=5L,
#                glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
#  y_hat[i] = mean((data_model_nona_nooutlier $ vote_average [folds ==i] - (1/predict(glmm.Gamma,test)))^2) # test,allow.new.levels=TRUE, then the prediction will use the unconditional (population-level) values for data with previously unobserved levels (or NAs).
#}
#mean(y_hat) 
# For GLMM, Gamma,  k-fold MSE = 0.6611752
#-------------------- Prediction Evaluation Over --------------------#


#---inverse.gaussian-----#
# glmm.i.g = glmer( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                    scale(popularity) + scale(revenue) + scale(runtime )
#                  +(1|genres), data=data_model_nona_nooutlier, 
#                  family=inverse.gaussian(), nAGQ=5L )
# error might be caused by having categorical variables with too many levels

#-------------------- HGLM --------------------#
# use covariate to model dispersion parameters
# Gamma
# model_mu <- dhglm::DHGLMMODELING( Model="mean", Link="log", LinPred= vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                                    scale(popularity) + scale(revenue) + scale(runtime )
#                                  +(1|production_countries), RandDist="inverse-gamma" )
# model_phi <- dhglm:: DHGLMMODELING( Model="dispersion")
# Rand.Int.Disp <- dhglm::dhglmfit( RespDist="gamma", DataMain=data_model_nona_nooutlier , MeanModel=model_mu, 
#                                  DispersionModel=model_phi, REML=TRUE, mord=1, dord =2 )

## Can not model HGLM, take too much conputational power.

#-------------------- GLM --------------------#
# Gaussian

#Gaussian.fit <- glm( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                       scale(popularity) + scale(revenue) + scale(runtime )
#                     , data=data_model_nona_nooutlier, family=gaussian() ) 
# log transform
#Gaussian.fit <- glm( log(vote_average) ~ as.factor(belongs_to_collection )*scale(budget)* 
#                       scale(popularity) * scale(revenue) * scale(runtime )
#                     , data=data_gaussian_glm, family=gaussian())
# log the independent variables
#Gaussian.fit <- glm( vote_average ~ as.factor(belongs_to_collection )+ log(budget) + 
#                       log( popularity)+ log(revenue)+ log(runtime )
#                     , data=data_gaussian_glm, family=gaussian())
# use lm function instead
# rating.ols <- lm( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                       scale(popularity) + scale(revenue) + scale(runtime )
#                     , data=data_model_nona_nooutlier) 
# summary(rating.ols )
# par( mfrow=c(2,2))
# plot(rating.ols ) # Have problem of heteroscedasticity!!
# car::ncvTest(rating.ols)
# data_lm = data_model_nona_nooutlier
# GLS Approach
# data_lm$resi= rating.ols$residuals

# varfunc.gls <- lm.gls( vote_average~ as.factor(belongs_to_collection )+ log(budget) + 
#                    log( popularity)+ log(revenue)+ log(runtime ), data = data_lm)
# gls  = nlme::gls(vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#      scale(popularity) + scale(revenue) + scale(runtime ), data=data_model_nona_nooutlier) # weights function need to read more
# summary(gls )
# par( mfrow=c(2,2))
# plot(gls )

# Transformation Approach
# trans.ols <- lm(vote_average^2 ~ as.factor(belongs_to_collection )+ log(budget) + 
#                    log( popularity)+ log(revenue)+ log(runtime ), data = data_lm)
# par( mfrow=c(2,2))
# plot(trans.ols)
#Box-Cox Transformation
#library(MASS)
#data_lm = data_model_nona_nooutlier[-c(6097,5576,4051),]
#rating.ols <- lm( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                    scale(popularity) + scale(revenue) + scale(runtime )
#                  , data=data_lm) 

#par( mfrow=c(2,2) ) 
#plot(rating.ols)
#bc = boxcox(rating.ols,lambda = seq(-3,3))
#best.lam = bc$x[which(bc$y==max(bc$y))]
#fullmodel.inv = lm(vote_average^(best.lam) ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                     scale(popularity) + scale(revenue) + scale(runtime )
#                   , data=data_lm)
#par( mfrow=c(2,2) ) 
#plot(fullmodel.inv)
#car::ncvTest(fullmodel.inv)
#lmtest::bptest(fullmodel.inv)
#summary(Gaussian.fit)
# for ungrouped data, residual deviance no use
#summary(Gaussian.fit)
#par( mfrow=c(2,2) ) 
#dev.off()
#plot(Gaussian.fit)
#car::marginalModelPlots( Gaussian.fit )
#car::influenceIndexPlot(Gaussian.fit)
#----- Pearson residual vs each covariate. No systematic pattern (a straight line) is wanted. -----#  
#car::residualPlots( Gaussian.fit ) 

#predict(Gaussian.fit,data=data_model_nona_nooutlier)

# If exclude No.5576 observation
#data_gaussian_glm  = data_model_nona_nooutlier[-c(5576),]

#Gaussian.fit <- glm( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                       scale(popularity) + scale(revenue) + scale(runtime )
#                     , data=data_gaussian_glm, family=gaussian())

#AIC(Gaussian.fit) #17007.58
# Look at residual plots
#plot(Gaussian.fit)
#car::marginalModelPlots( Gaussian.fit )
#car::influenceIndexPlot(Gaussian.fit)
#data_gaussian_glm  = data_gaussian_glm [-c(6828,6736),] # second round of excluding obs, after seeing the residual piots

# If add quadratic terms
#data_gaussian_glm <- mutate(data_gaussian_glm, popularity2 = popularity^2)

#Gaussian.fit <- glm( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                       scale(popularity) + scale(popularity2) + scale(revenue) + scale(runtime )
#                     , data=data_gaussian_glm, family=gaussian() ) 

#Gaussian.fit <- glm( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                       scale(popularity)+ (scale(popularity))^2 + scale(revenue) + scale(runtime )
#                     , data=data_gaussian_glm, family=gaussian() ) 
#summary(Gaussian.fit)
#plot(residuals(Gaussian.fit))
#plot(Gaussian.fit)
#AIC(Gaussian.fit) #16685.34
#car::residualPlots( Gaussian.fit ) # popularity seems better
#-------------------- Prediction Evaluation For GLM, Gaussian --------------#
#n = nrow(data_gaussian_glm)
#k = 10 
# shuffle 
#set.seed(1234)
#folds = sample(rep(1:k, ceiling(n/k)), n, replace = F)
#y_hat = numeric(k) 
#for (i in 1:k) {
#  train = data_gaussian_glm[folds != i, ]
#  test = data_gaussian_glm[folds == i, ]
#  mod1 = glm( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                scale(popularity)+ scale(popularity2)+ scale(revenue) + scale(runtime )
#              , data=train, family=gaussian() )
#  y_hat[i] = mean((data_gaussian_glm $ vote_average [folds ==i] - predict(mod1,test))^2)
#}
#mean(y_hat)  #with quadratic:0.6598106 #without quadratic: 0.6846683
#-------------------- Prediction Evaluation Over --------------------#

# Gamma
# data_gamma_glm = data_model_nona_nooutlier[-6134,]
# data_gamma_glm <- mutate(data_gamma_glm, popularity2 = popularity^2)
# Gamma.fit <- glm( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
##                    scale(popularity)+scale(popularity2) + scale(revenue) + scale(runtime )
#                  , data=data_gamma_glm, family=Gamma() ) 
# summary(Gamma.fit)
# par( mfrow=c(2,2) ) 
# plot(Gamma.fit)
# car::marginalModelPlots( Gamma.fit )
# car::influenceIndexPlot(Gamma.fit)

# car::residualPlots( Gamma.fit ) 

# Gamma.fit <- glm( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                    poly(scale(popularity),2) + scale(revenue) + poly(scale(runtime ),2)
#                  , data=data_model_nona_nooutlier, family=Gamma() ) 

#-------------------- Prediction Evaluation For GLM, Gamma --------------#
#predict(Gamma.fit,data=data_model_nona_nooutlier)
#(1/predict(Gamma.fit,data=data_model_nona_nooutlier))
#summary(Gamma.fit$fitted.values)


#k = 10 
# shuffle 
#set.seed(1234)
#folds = sample(rep(1:k, ceiling(n/k)), n, replace = F)
#y_hat = numeric(k) 
#for (i in 1:k) {
#  train = data_model_nona_nooutlier[folds != i, ]
#  test = data_model_nona_nooutlier[folds == i, ]
#  mod1 = glm( vote_average ~ as.factor(belongs_to_collection )+ scale(budget) + 
#                poly(scale(popularity),2,raw = T) + scale(revenue) + poly(scale(runtime),2,raw=T)
#              , data=train, family=Gamma() )
#  y_hat[i] = mean((data_model_nona_nooutlier $ vote_average [folds ==i] - (1/predict(mod1,newdata=test)))^2)
#}
#mean(y_hat)  # with 2 quadratic: 0.6800203, with 1 quadratic: 0.6917392 #without quadratic: 1.155785

#-------------------- Prediction Evaluation Over --------------------#

####-------------------- GAM --------------------####
k_basis = 11 # stablized at 10 
gam.fit = mgcv::gam( vote_average^3 ~ s(log(budget),bs='tp',k=k_basis) + as.factor(belongs_to_collection ) +
                       +s(log(popularity),bs='tp',k=k_basis) + s(log(revenue),bs='tp',k=k_basis) 
                      + s( log(runtime ),bs='tp',k=k_basis), 
                     family = gaussian(), data=data_model_nona_nooutlier )
summary(gam.fit)
# Adjust k according to edf
gam.fit = mgcv::gam( vote_average^3 ~ s(scale(budget),bs='tp',k=k_basis) + as.factor(belongs_to_collection ) +
                       +s(scale(popularity),bs='tp',k=k_basis) + s(scale(revenue),bs='tp',k=5) 
                     , 
                     family = gaussian(), data=data_model_nona_nooutlier )

AIC(gam.fit) #16337.68
par( mfrow = c(1,3) )
plot(gam.fit,shade=TRUE, seWithMean=TRUE)
summary(gam.fit)

####  Residual plot

par( mfrow = c(2,2) )
mgcv::gam.check( gam.fit) 
dev.off()
 
# QQ-plot not so bad
#-------------------- Prediction Evaluation For GAM --------------#
gam.fit$fitted.values
mgcv::predict.gam(gam.fit)

n = nrow(data_model_nona_nooutlier)
k = 10 
# shuffle 
set.seed(1234)
folds = sample(rep(1:k, ceiling(n/k)), n, replace = F)
y_hat = numeric(k) 
for (i in 1:k) {
  train = data_model_nona_nooutlier[folds != i, ]
  test = data_model_nona_nooutlier[folds == i, ]
  mod1 = mgcv::gam( vote_average^3 ~ s(scale(budget),bs='tp',k=k_basis) + as.factor(belongs_to_collection ) +
                      +s(scale(popularity),bs='tp',k=k_basis) + s(scale(revenue),bs='tp',k=5) ,  
                    family = gaussian(), data=train )
  y_hat[i] = mean((data_model_nona_nooutlier $ vote_average [folds ==i] - (mgcv::predict.gam( mod1, newdata = test))^(1/3) )^2)
}
mean(y_hat)  
# 0.7251819 vote_average^3 gaussian  
# if change to family = Gamma, bs = cs, almost the same
# 0.7385436 for vote_average^4 .

#-------------------- Prediction Evaluation Over --------------#

#-------------------------------------------------------------#
######------------------ Model Comparisons ---------------------######
#------------------------------------------------------------#
# Use the K-fold MSE produced above.

