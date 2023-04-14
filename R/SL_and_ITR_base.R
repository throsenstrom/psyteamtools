library(dplyr)
library(tlverse)
library(skimr)
library(sl3)
library(Formula) # earth:ia varten
library(plotrix) # earth:ia varten
library(TeachingDemos) # earth:ia varten
library(plotmo) # earth:ia varten
library(earth) # MARS
library(lattice) # caret:ia varten
library(ggplot2)  # caret:ia varten
library(caret) #logreg evaluation
library(pscl) # pseudo R*2 for log.reg

library(tmle3)
library(tmle3mopttx)
library(usethis) # For devtools
library(devtools)
library(Matrix) # for speedglm
library(MASS) # for speedglm
library(speedglm) # for sl, glm_fast

##########################################################
############## SUPERLEARNER: SOFAS MUUTOS ################
##########################################################


### Define the task
task_oireet <- make_sl3_Task(data = dtest, 
                             outcome = "dsofas", 
                             covariates = c("therapy_class", "work", "medication", "therapist", "smoking", "sukupuoli", "ika", "comor_group",
                                            "PBL1", "PBL2", "PBL3", "PBL4", "PBL5", "PBL6", "PBL7", "PBL8", "PBL9", "interrupted",
                                            "core10_bl", "oasis_bl", "audit_bl", "dg_class", "group"),
                             folds= 10)

# ensisijainen malli
task_sum <- make_sl3_Task(data = dtest, 
                          outcome = "dsofas", 
                          covariates = c("therapy_class", "work", "medication", "therapist", "smoking", "sukupuoli", "ika", "comor_group",
                                         "phq_bl", "interrupted","core10_bl", "oasis_bl", "audit_bl", "dg_class", "group"),
                          folds= 10)

?Lrnr_nnet

### Instantiate the Super Learner with Lrnr_sl: specify base learners
lrn_mean <- Lrnr_mean$new()

# penalized regressions:
lrn_ridge <- Lrnr_glmnet$new(alpha = 0) # with specified tuning parameters 1, 0, 0.5
lrn_elastic <- Lrnr_glmnet$new(alpha = 0.5)
lrn_lasso <- Lrnr_glmnet$new(alpha = 1)

# spline regressions:
lrn_earth <- Lrnr_earth$new()  # with default tuning parameters

# tree-based methods
lrn_ranger <- Lrnr_ranger$new() # "paranneltu random forest", faster implementatikon, suitable for highdimensional data
lrn_xgboost <- Lrnr_xgboost$new() #  eXtreme Gradient Boosting
lrn_random <- Lrnr_randomForest$new()

lrn_svm <- Lrnr_svm$new() # SVM: support vector machines




### Create a stack that considers multiple learnes simultanously
stack <- Stack$new(
  lrn_mean, lrn_ridge, lrn_elastic, lrn_lasso, lrn_earth,
  lrn_ranger, lrn_xgboost, lrn_random, lrn_svm
)

stack



### Instantiate the SuperLearner, by using meta-learner. 
# Here, a non-negative least squares (NNLS) regression is used as a default meta-learner 
sl_dsofas <- Lrnr_sl$new(learners = stack, metalearner = Lrnr_nnls$new())

#### Fit the SuperLearner to the prediction task
start_time <- proc.time() # start time

set.seed(4197)
sl_fit_dsofas <- sl_dsofas$train(task = task_oireet) # fitted superlearner object

runtime_sl_dsofas_fit <- proc.time() - start_time # end time - start time = run time
runtime_sl_dsofas_fit ### kaikkien mallien MSE:t


runtime_sl_dsofas_fit <- proc.time() - start_time # end time - start time = run time
runtime_sl_dsofas_fit


### Superlearner TULOKSET sofas muutokselle
sl_preds_dsofas <- sl_fit_dsofas$predict(task = task_oireet)
head(sl_preds_dsofas)

sl_fit_dsofas$coefficients # vain 9:lle (kymmenesta) mallille > "estimated cross-validated MSE for each algorithm"
sl_fit_dsofas$metalearner_fit()


# tarkastellaan erikseen baselearnereiden tuloksia
xgboost_preds_dsofas <- sl_fit_dsofas$learner_fits$Lrnr_xgboost_20_1$predict(task = task_oireet) # eXtreme gradient boodsting
summary(xgboost_preds_dsofas)

lasso_preds_dsofas <- sl_fit_dsofas$learner_fits$Lrnr_glmnet_NULL_deviance_10_1_100_TRUE_FALSE$predict(task=task_oireet) # lasso
summary(lasso_preds_dsofas)


# Table of observed and predicted outcome values (SL, lasso & mean)
library(data.table)
df_plot <- data.table(
  Obs = dtest[["dsofas"]], SL_Pred = sl_preds_dsofas, Lasso_Pred = lasso_preds_dsofas,
  Mean_Pred = sl_fit_dsofas$learner_fits$Lrnr_mean$predict(task_oireet)
)

df_plot <- df_plot[order(df_plot$Obs), ] 

head(df_plot)
colMeans(df_plot)


# Tulokset taulukkoon
library(gt)
sl_fit_dsofas$print() %>% gt()

sl_fit_dsofas$fit_object$cv_meta_fit$fit_object # sama kuin coefficients


# VARIABLE IMPORTANCE
var_import <- importance(sl_fit_dsofas, eval_fun = NULL, fold_number = "validation",
                         type = c("remove"), importance_metric = c("difference"), covariate_groups = NULL)
var_import

importance_plot(var_import)


# obtain the CV predictions for the candidate learners
cv_preds <- sl_fit_dsofas$fit_object$cv_fit$predict_fold(
  task = task_oireet, fold_number = "validation"
)

colMeans(cv_preds) # ennusteiden keskarvot jokaiselle learnerille


# cross-validated (CV) predictive performance, i.e., the CV risk, for each learner included in the SL
cv_risk_dsofas <- sl_fit_dsofas$cv_risk(eval_fun = loss_squared_error) # cv_risk() laskee crossvalidoidut MSE:t 
# jokaiselle baselearnerille aikaisempien painotetuilla kertoimilla saatujen ennusteiden perusteella 

# cv_sl() laskee "cv risk:in" superlearnerille. 
# start_time <- proc.time()
# 
# set.seed(569)
# cv_sl_fit_dsofas <- cv_sl(lrnr_sl = sl_fit_dsofas, task = task, eval_fun = loss_squared_error)
# 
# runtime_cv_sl_fit <- proc.time() - start_time
# runtime_cv_sl_fit


#######################################################################################################

###########################################
###### Individualized treatment rule ######
###########################################


node_list <- list(
  W = c("x1", "x2", ...),
  A = "treatment variable",
  Y = "outcome"
)


### create ensemble learners with sl ###
# See which learners support multi-class classification:
sl3_list_learners(c("categorical"))

# Initialize few of the learners:
lrn_xgboost_50 <- Lrnr_xgboost$new(nrounds = 50)  #  eXtreme Gradient Boosting
lrn_xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
lrn_xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
lrn_mean <- Lrnr_mean$new() 
lrn_glm <- Lrnr_glm_fast$new() 



## OUTCOME REGRESSION: Define the Q learner, which is just a regular learner: 
Q_learner <- Lrnr_sl$new(
  learners = list(lrn_xgboost_100, lrn_mean, lrn_glm),
  metalearner = Lrnr_nnls$new()
)


# PROPENSITY SCORE:  Define the g learner, which is a multinomial learner:
# specify the appropriate loss of the multinomial learner:
mn_metalearner <- make_learner(Lrnr_solnp,
                               eval_function = loss_loglik_multinomial,
                               learner_function = metalearner_linear_multinomial)

g_learner <- make_learner(Lrnr_sl, list(lrn_xgboost_100, lrn_xgboost_500, lrn_mean), mn_metalearner)


# BLIP FUNCTION: Define the Blip learner, which is a multivariate learner:
learners <- list(lrn_xgboost_50, lrn_xgboost_100, lrn_xgboost_500, lrn_mean, lrn_glm)
b_learner <- create_mv_learners(learners = learners)

# specify outcome and treatment regressions and create learner list
learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)


### Targeted estimation of the Mean under the optimal individualized interventions effects
# initialize a tmle specification
tmle_spec <- tmle3_mopttx_blip_revere(
  V = c("x1", "x2", ...), 
  type = "blip2",
  learners = learner_list, maximize = TRUE, complex = TRUE, 
  realistic = FALSE
)

# fit the TML estimator
fit_tmle <- tmle3(tmle_spec, df, node_list, learner_list)
fit_tmle


# How many individuals got assigned each treatment?
table(tmle_spec$return_rule)

