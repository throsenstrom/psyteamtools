test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


# library(dplyr)
# library(tlverse)
# library(skimr)
# library(sl3)
# library(Formula) # earth:ia varten
# library(plotrix) # earth:ia varten
# library(TeachingDemos) # earth:ia varten
# library(plotmo) # earth:ia varten
# library(earth) # MARS
# library(lattice) # caret:ia varten
# library(ggplot2)  # caret:ia varten
# library(caret) #logreg evaluation
# library(pscl) # pseudo R*2 for log.reg
#
# library(tmle3)
# library(tmle3mopttx)
# library(usethis) # For devtools
# library(devtools)
# library(Matrix) # for speedglm
# library(MASS) # for speedglm
# library(speedglm) # for sl, glm_fast
#
# ##########################################################
# ############## SUPERLEARNER: SOFAS MUUTOS ################
# ##########################################################
#
#
# ### Define the task
# task_oireet <- make_sl3_Task(data = dtest,
#                              outcome = "dsofas",
#                              covariates = c("therapy_class", "work", "medication", "therapist", "smoking", "sukupuoli", "ika", "comor_group",
#                                             "PBL1", "PBL2", "PBL3", "PBL4", "PBL5", "PBL6", "PBL7", "PBL8", "PBL9", "interrupted",
#                                             "core10_bl", "oasis_bl", "audit_bl", "dg_class", "group"),
#                              folds= 10)
#
# # ensisijainen malli
# task_sum <- make_sl3_Task(data = dtest,
#                           outcome = "dsofas",
#                           covariates = c("therapy_class", "work", "medication", "therapist", "smoking", "sukupuoli", "ika", "comor_group",
#                                          "phq_bl", "interrupted","core10_bl", "oasis_bl", "audit_bl", "dg_class", "group"),
#                           folds= 10)
#
# ?Lrnr_nnet
#
# ### Instantiate the Super Learner with Lrnr_sl: specify base learners
# lrn_mean <- Lrnr_mean$new()
#
# # penalized regressions:
# lrn_ridge <- Lrnr_glmnet$new(alpha = 0) # with specified tuning parameters 1, 0, 0.5
# lrn_elastic <- Lrnr_glmnet$new(alpha = 0.5)
# lrn_lasso <- Lrnr_glmnet$new(alpha = 1)
#
# # spline regressions:
# lrn_earth <- Lrnr_earth$new()  # with default tuning parameters
#
# # tree-based methods
# lrn_ranger <- Lrnr_ranger$new() # "paranneltu random forest", faster implementatikon, suitable for highdimensional data
# lrn_xgboost <- Lrnr_xgboost$new() #  eXtreme Gradient Boosting
# lrn_random <- Lrnr_randomForest$new()
#
# lrn_svm <- Lrnr_svm$new() # SVM: support vector machines
#
#
#
#
# ### Create a stack that considers multiple learnes simultanously
# stack <- Stack$new(
#   lrn_mean, lrn_ridge, lrn_elastic, lrn_lasso, lrn_earth,
#   lrn_ranger, lrn_xgboost, lrn_random, lrn_svm
# )
#
# stack
#
#
#
# ### Instantiate the SuperLearner, by using meta-learner.
# # Here, a non-negative least squares (NNLS) regression is used as a default meta-learner
# sl_dsofas <- Lrnr_sl$new(learners = stack, metalearner = Lrnr_nnls$new())
#
# #### Fit the SuperLearner to the prediction task
# start_time <- proc.time() # start time
#
# set.seed(4197)
# sl_fit_dsofas <- sl_dsofas$train(task = task_oireet) # fitted superlearner object
#
# runtime_sl_dsofas_fit <- proc.time() - start_time # end time - start time = run time
# runtime_sl_dsofas_fit ### kaikkien mallien MSE:t
#
#
# runtime_sl_dsofas_fit <- proc.time() - start_time # end time - start time = run time
# runtime_sl_dsofas_fit
#
#
# ### Superlearner TULOKSET sofas muutokselle
# sl_preds_dsofas <- sl_fit_dsofas$predict(task = task_oireet)
# head(sl_preds_dsofas)
#
# sl_fit_dsofas$coefficients # vain 9:lle (kymmenesta) mallille > "estimated cross-validated MSE for each algorithm"
# sl_fit_dsofas$metalearner_fit()
#
#
# # tarkastellaan erikseen baselearnereiden tuloksia
# xgboost_preds_dsofas <- sl_fit_dsofas$learner_fits$Lrnr_xgboost_20_1$predict(task = task_oireet) # eXtreme gradient boodsting
# summary(xgboost_preds_dsofas)
#
# lasso_preds_dsofas <- sl_fit_dsofas$learner_fits$Lrnr_glmnet_NULL_deviance_10_1_100_TRUE_FALSE$predict(task=task_oireet) # lasso
# summary(lasso_preds_dsofas)
#
#
# # Table of observed and predicted outcome values (SL, lasso & mean)
# library(data.table)
# df_plot <- data.table(
#   Obs = dtest[["dsofas"]], SL_Pred = sl_preds_dsofas, Lasso_Pred = lasso_preds_dsofas,
#   Mean_Pred = sl_fit_dsofas$learner_fits$Lrnr_mean$predict(task_oireet)
# )
#
# df_plot <- df_plot[order(df_plot$Obs), ]
#
# head(df_plot)
# colMeans(df_plot)
#
#
# # Tulokset taulukkoon
# library(gt)
# sl_fit_dsofas$print() %>% gt()
#
# sl_fit_dsofas$fit_object$cv_meta_fit$fit_object # sama kuin coefficients
#
#
# # VARIABLE IMPORTANCE
# var_import <- importance(sl_fit_dsofas, eval_fun = NULL, fold_number = "validation",
#                          type = c("remove"), importance_metric = c("difference"), covariate_groups = NULL)
# var_import
#
# importance_plot(var_import)
#
#
# # obtain the CV predictions for the candidate learners
# cv_preds <- sl_fit_dsofas$fit_object$cv_fit$predict_fold(
#   task = task_oireet, fold_number = "validation"
# )
#
# colMeans(cv_preds) # ennusteiden keskarvot jokaiselle learnerille
#
#
# # cross-validated (CV) predictive performance, i.e., the CV risk, for each learner included in the SL
# cv_risk_dsofas <- sl_fit_dsofas$cv_risk(eval_fun = loss_squared_error) # cv_risk() laskee crossvalidoidut MSE:t
# # jokaiselle baselearnerille aikaisempien painotetuilla kertoimilla saatujen ennusteiden perusteella
#
# # cv_sl() laskee "cv risk:in" superlearnerille.
# # start_time <- proc.time()
# #
# # set.seed(569)
# # cv_sl_fit_dsofas <- cv_sl(lrnr_sl = sl_fit_dsofas, task = task, eval_fun = loss_squared_error)
# #
# # runtime_cv_sl_fit <- proc.time() - start_time
# # runtime_cv_sl_fit
#
#
# #######################################################################################################
#
# ###########################################
# ###### Individualized treatment rule ######
# ###########################################
#
#
# node_list <- list(
#   W = c("x1", "x2", ...),
#   A = "treatment variable",
#   Y = "outcome"
# )
#
#
# ### create ensemble learners with sl ###
# # See which learners support multi-class classification:
# sl3_list_learners(c("categorical"))
#
# # Initialize few of the learners:
# lrn_xgboost_50 <- Lrnr_xgboost$new(nrounds = 50)  #  eXtreme Gradient Boosting
# lrn_xgboost_100 <- Lrnr_xgboost$new(nrounds = 100)
# lrn_xgboost_500 <- Lrnr_xgboost$new(nrounds = 500)
# lrn_mean <- Lrnr_mean$new()
# lrn_glm <- Lrnr_glm_fast$new()
#
#
#
# ## OUTCOME REGRESSION: Define the Q learner, which is just a regular learner:
# Q_learner <- Lrnr_sl$new(
#   learners = list(lrn_xgboost_100, lrn_mean, lrn_glm),
#   metalearner = Lrnr_nnls$new()
# )
#
#
# # PROPENSITY SCORE:  Define the g learner, which is a multinomial learner:
# # specify the appropriate loss of the multinomial learner:
# mn_metalearner <- make_learner(Lrnr_solnp,
#                                eval_function = loss_loglik_multinomial,
#                                learner_function = metalearner_linear_multinomial)
#
# g_learner <- make_learner(Lrnr_sl, list(lrn_xgboost_100, lrn_xgboost_500, lrn_mean), mn_metalearner)
#
#
# # BLIP FUNCTION: Define the Blip learner, which is a multivariate learner:
# learners <- list(lrn_xgboost_50, lrn_xgboost_100, lrn_xgboost_500, lrn_mean, lrn_glm)
# b_learner <- create_mv_learners(learners = learners)
#
# # specify outcome and treatment regressions and create learner list
# learner_list <- list(Y = Q_learner, A = g_learner, B = b_learner)
#
#
# ### Targeted estimation of the Mean under the optimal individualized interventions effects
# # initialize a tmle specification
# tmle_spec <- tmle3_mopttx_blip_revere(
#   V = c("x1", "x2", ...),
#   type = "blip2",
#   learners = learner_list, maximize = TRUE, complex = TRUE,
#   realistic = FALSE
# )
#
# # fit the TML estimator
# fit_tmle <- tmle3(tmle_spec, df, node_list, learner_list)
# fit_tmle
#
#
# # How many individuals got assigned each treatment?
# table(tmle_spec$return_rule)



#
# AdultDataQuality <- function(id, template=NULL){ # template = "patientQ", "therapistQ", or "questionnaires"
#   # define sub-function to investigate presence of BL and FU values in the questionnaire data
#   QualityBLFU <- function(dql, use_orig_lab=T){
#     if (use_orig_lab){
#       return( any(!is.na(dql[,setdiff(names(dql)[grepl("FU", names(dql))],c("treatment_idFU","visit_idFU","cdateFU"))])) &
#                 any(!is.na(dql[,setdiff(sub("FU", "", names(dql)[grepl("FU", names(dql))]),c("treatment_id","visit_id","cdate"))])) )
#     } else {
#       return( any(!is.na(dql[,setdiff(names(dql)[grepl("FU", names(dql))],c("treatment_idFU","visit_idFU","cdateFU"))])) &
#                 any(!is.na(dql[,setdiff(names(dql)[grepl("BL", names(dql))],c("treatment_id","visit_id","cdate"))])) )
#     }
#   }
#   # Quality of full data
#   if (is.null(template)){
#     has_quality <- (id %in% dsof$patient_id) & (id %in% dcom$patient_id) & (id %in% doasis$patient_id) &
#       (id %in% dphq$patient_id) & (id %in% daudit$patient_id)
#     if (!has_quality){
#       return(has_quality) # return immediately if missing a questionnaire, otherwise check both the baseline and the follow-up...
#     } else {
#       has_quality <- QualityBLFU(dsof[dsof$patient_id == id, ]) &          # SOFAS
#         QualityBLFU(dcom[dcom$patient_id == id, ], use_orig_lab = F) &     # CORE-OM
#         QualityBLFU(doasis[doasis$patient_id == id, ], use_orig_lab = F) & # OASIS
#         QualityBLFU(dphq[dphq$patient_id == id, ], use_orig_lab = F) &     # PHQ-9
#         QualityBLFU(daudit[daudit$patient_id == id, ], use_orig_lab = F)   # AUDIT-C
#       # ...and check patient and therapist Q
#       x <- d[d$patient_id == id,]
#       has_quality <- (all(c("aikuispotilaan_alkuarvio",
#                             "aikuispotilaan_loppu_valiarvio",
#                             "psykoterapeutin_alkuarvio_aikuiset",
#                             "psykoterapeutin_loppu_valiarvio_aikuispotilaat") %in% x$template_code)) &
#         has_quality
#     }
#   } else {
#     if (template == "patientQ"){
#       x <- d[d$patient_id == id,]
#       has_quality <- all(c("aikuispotilaan_alkuarvio",
#                            "aikuispotilaan_loppu_valiarvio") %in% x$template_code)
#     }
#     if (template == "therapistQ"){
#       x <- d[d$patient_id == id,]
#       has_quality <- all(c("psykoterapeutin_alkuarvio_aikuiset",
#                            "psykoterapeutin_loppu_valiarvio_aikuispotilaat") %in% x$template_code)
#     }
#     if (template == "questionnaires"){
#       has_quality <- (id %in% dsof$patient_id) & (id %in% dcom$patient_id) & (id %in% doasis$patient_id) &
#         (id %in% dphq$patient_id) & (id %in% daudit$patient_id)
#       if (!has_quality){
#         return(has_quality) # return immediately if missing a questionnaire
#       } else { # check there is something in both the baseline and the follow-up
#         has_quality <- QualityBLFU(dsof[dsof$patient_id == id, ]) &          # SOFAS
#           QualityBLFU(dcom[dcom$patient_id == id, ], use_orig_lab = F) &     # CORE-OM
#           QualityBLFU(doasis[doasis$patient_id == id, ], use_orig_lab = F) & # OASIS
#           QualityBLFU(dphq[dphq$patient_id == id, ], use_orig_lab = F) &     # PHQ-9
#           QualityBLFU(daudit[daudit$patient_id == id, ], use_orig_lab = F)   # AUDIT-C
#       }
#     }
#     if (!(template %in% c("patientQ","therapistQ","questionnaires"))){
#       errorCondition("Undefined template was inserted to the AdultDataQuality function!")
#     }
#   }
#   return(has_quality)
# }

# build_tidy_FPQR <- function(d_forms, d_tre, d_vis, d_pat, no_kela = T,
#                             no_kids = T, no_interrupted_therapies = T,
#                             no_repeated_FPQR_treatments = T,
#                             no_internal_therapies = T){
#   library(dplyr)
#   library(tidyr)
#
#   d <- d_forms; rm(d_forms) # revert to old naming convention
#   # Uniques IDs
#   uids <- unique(d$patient_id)
#   print(paste0("Unique patients: ", length(uids)))
#
#   if (no_kela){
#     # Find patients in Kela treatments
#     pmuoto_ids <- unique(d$patient_id[(d$string_answer=="kela")&(d$question_code=="palvelumuoto")])
#     length(pmuoto_ids)
#     uids <- setdiff(uids, pmuoto_ids)
#   }
#
#   if (no_kids){
#     # DROP NON-ADULTS HERE (9.12.2021)
#     uids <- uids[uids %in% d$patient_id[(d$question_code=="erikoisala")&(d$string_answer=="aikuisten")]]
#   }
#
#   # Uninitiated psychotherapies
#   uids_uninitiated <- uids[!(uids %in% filter(d, (template_code == "psykoterapeutin_alkuarvio_nuoret") |
#                                                 (template_code == "psykoterapeutin_alkuarvio_aikuiset") |
#                                                 (template_code == "psykoterapeutin_alkuarvio_lapset"))$patient_id)]
#
#   # Uninitiated psychotherapies
#   uids_uninitiated <- uids[!(uids %in% filter(d, (template_code == "psykoterapeutin_alkuarvio_nuoret") |
#                                                 (template_code == "psykoterapeutin_alkuarvio_aikuiset") |
#                                                 (template_code == "psykoterapeutin_alkuarvio_lapset"))$patient_id)]
#
#   # Altogether X completed psychotherapies
#   uids_completed <- uids[(uids %in% filter(d, (template_code == "psykoterapeutin_loppu_valiarvio_nuoret") |
#                                              (template_code == "psykoterapeutin_loppu_valiarvio_aikuispotilaat") |
#                                              (template_code == "psykoterapeutin_loppu_valiarvio_lapset"))$patient_id)]
#
#   # Check how numbers change when requiring final/midterm stamp too (now only 3 more adults drops out)
#   uids_completed_stamped <- rep(F, length(uids_completed))
#   uids_midterm_only_stamped <- rep(F, length(uids_completed))
#   for (i in 1:length(uids_completed)){
#     trids <- unique(d$treatment_id[d$patient_id == uids_completed[i]])
#     uids_completed_stamped[i] <- any(d_vis$visit_type_id[d_vis$treatment_id %in% trids] %in% c("final", "midterm"))
#     uids_midterm_only_stamped[i] <- any(d_vis$visit_type_id[d_vis$treatment_id %in% trids] == "midterm") &
#       !any(d_vis$visit_type_id[d_vis$treatment_id %in% trids] == "final")
#   }
#   uids_midterm_only <- uids_completed[uids_midterm_only_stamped]
#   uids_completed <- uids_completed[uids_completed_stamped]
#
#   if (no_interrupted_therapies){
#     # remove interrupted
#     uids_completed <- setdiff(uids_completed, uids_interrupted)
#     uids_midterm_only <- setdiff(uids_midterm_only, uids_interrupted)
#   }
#
#   ######### Describe register evolution #########
#
#   # Every entered patient
#   entries <- lubridate::as_date(
#     sapply(uids, function(x) min(lubridate::as_date(d$date_created[d$patient_id == x]),na.rm=T)))
#
#   # Initiated therapies
#   dtmp <- filter(d, (template_code == "psykoterapeutin_alkuarvio_nuoret") |
#                    (template_code == "psykoterapeutin_alkuarvio_aikuiset") |
#                    (template_code == "psykoterapeutin_alkuarvio_lapset"))
#   inits <- lubridate::as_date(
#     sapply(setdiff(uids,uids_uninitiated),
#            function(x) min(lubridate::as_date(dtmp$date_created[dtmp$patient_id == x]),na.rm=T)))
#
#   # Completed or interrupted therapies
#   ended <- lubridate::as_date(
#     sapply(unique(c(uids_completed, uids_interrupted)),
#            function(x) max(lubridate::as_date(d$date_created[d$patient_id == x]),na.rm=T)))
#
#   entries <- sort(entries)
#   inits <- sort(inits)
#   ended <- sort(ended)
#
#   ########################################################################
#
#   if (no_repeated_FPQR_treatments){
#
#     ##################################################################################
#     ########## Then restrict to adults with 1 outsourced non-Kela therapy ############
#     ##################################################################################
#
#     # Find patients with multiple treatments
#     mts_ids <- sapply(uids_completed, function(x) length(unique(d$treatment_id[d$patient_id == x])) > 1)
#     mts_ids <- uids_completed[mts_ids]
#     uids_completed <- setdiff(uids_completed, mts_ids)
#     uids_midterm_only <- setdiff(uids_midterm_only, mts_ids)
#   }
#   if (no_internal_therapies){
#     # Remove internal psyhotherapies, leave outsourced only (26.1.2022)
#     int_ids <- sapply(uids_completed, function(x) ("sisainen" %in% d$string_answer[(d$patient_id==x)&(d$question_code=="palvelumuoto")]))
#     int_ids <- uids_completed[int_ids]
#     uids_completed <- setdiff(uids_completed, int_ids)
#     uids_midterm_only <- setdiff(uids_midterm_only, int_ids)
#   }
#
#   ##################################################################################
#
#   ### Pull data to wide tables ###
#
#   ##### Background info ####
#   dbag <- d %>%
#     filter(template_code == "taustatiedot") %>%
#     select(patient_id, date_created, template_code, question_code, string_answer) %>%
#     pivot_wider(names_from = question_code, values_from = string_answer)
#
#   dbag <- cbind(dbag,
#                 many_treatments =
#                   sapply(dbag$patient_id, function(x) (length(unique(d$treatment_id[d$patient_id==x])) > 1)*1))
#
#   ########## SOFAS ##########
#   dsof <- d %>%
#     filter(template_code == "sofas") %>%
#     select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
#     group_by(question_code) %>%
#     mutate(row = row_number()) %>% # to define unique rows for pivoting
#     ungroup() %>%
#     pivot_wider(names_from = question_code, values_from = number_answer) %>%
#     select(-row)
#
#   # Use the visit data
#   dsof <- subset(dsof, select = c(patient_id, sofas_asteikolla, vapaa_aika,
#                                   tyo_tai_opiskelu, perhe_elama_ja_ihmissuhteet,
#                                   itsesta_huolehtiminen,date_created, treatment_id, visit_id))
#   names(dsof) <- c("patient_id","sofas","freetime","work","family","self","date_created","treatment_id","visit_id")
#   dsof <- dataToBLFU_wvis(dsof, items = names(dsof)[c(2:6)], d_vis, interrupt_ids = uids_interrupted, use_orig_lab = T)
#
#   # A SOFAS-based multiple treatments variable
#   dbag <- cbind(dbag,
#                 many_sofases =
#                   sapply(dbag$patient_id, function(x){(length(unique(
#                     na.omit(union(dsof$treatment_id[dsof$patient_id==x],dsof$treatment_idFU[dsof$patient_id==x]))
#                   )) > 1)*1})
#   )
#
#   ########### CORE-OM ############
#   dcom <- d %>%
#     filter(template_code == "core_om") %>%
#     select(patient_id, date_created, template_code, question_name, number_answer, treatment_id, visit_id) %>%
#     group_by(question_name) %>%
#     mutate(row = row_number()) %>% # to define unique rows for pivoting
#     ungroup() %>%
#     pivot_wider(names_from = question_name, values_from = number_answer) %>%
#     select(-row)
#
#   # Sort the CORE-OM items
#   ilabs <- names(dcom)[grepl(" ", substr(names(dcom),1,3), fixed = T)]
#   x <- sort(as.numeric(substr(ilabs,1,2)), index.return = T)
#   dcom <- dcom[,c("patient_id", "date_created", ilabs[x$ix], "treatment_id","visit_id")]
#
#   ### CORE-OM to 1st and last ###
#   dcom <- dataToBLFU_wvis(dcom, items = names(dcom)[3:36], d_vis, interrupt_ids = uids_interrupted)
#
#   ########## PHQ-9 ##########
#   dphq <- d %>%
#     filter(template_code == "PHQ9") %>%
#     select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
#     group_by(question_code) %>%
#     mutate(row = row_number()) %>% # to define unique rows for pivoting
#     ungroup() %>%
#     pivot_wider(names_from = question_code, values_from = number_answer) %>%
#     select(-row)
#
#   dphq <- dataToBLFU_wvis(dphq, items = names(dphq)[6:14], d_vis, interrupt_ids = uids_interrupted)
#
#   ########## OASIS ##########
#   doasis <- d %>%
#     filter(template_code == "oasis") %>%
#     select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
#     group_by(question_code) %>%
#     mutate(row = row_number()) %>% # to define unique rows for pivoting
#     ungroup() %>%
#     pivot_wider(names_from = question_code, values_from = number_answer) %>%
#     select(-row)
#
#   doasis <- dataToBLFU_wvis(doasis, items = names(doasis)[6:10], d_vis, interrupt_ids = uids_interrupted)
#
#   ########## AUDIT ##########
#   daudit <- d %>%
#     filter(template_code == "audit_c") %>%
#     select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
#     group_by(question_code) %>%
#     mutate(row = row_number()) %>% # to define unique rows for pivoting
#     ungroup() %>%
#     pivot_wider(names_from = question_code, values_from = number_answer) %>%
#     select(-row)
#
#   daudit <- dataToBLFU_wvis(daudit, items = names(daudit)[2+c(4,5,8)], d_vis, interrupt_ids = uids_interrupted)
#
#   # A function to detect primary care units as a source of referral
#   IsAdultPrimaryCare <- function(id){
#     x <- d[d$patient_id == id,]
#     from_primary_care <- ("aikuisten" %in% x$string_answer[x$question_code=="erikoisala"]) &
#       ("ostopalvelu" %in% x$string_answer[x$question_code=="palvelumuoto"]) &
#       ( any(x$string_answer[x$question_code=="terapiaan_lahettava_yksikko"] %in% c("muu", "1126002", "2126001")) |
#           ("perusterveydenhuollosta" %in% x$string_answer[x$question_code=="mista_lahetetty"]) )
#     return(from_primary_care)
#   }
#
#   # A function to detect short adult therapies
#   IsShortTherapy <- function(id){
#     x <- d[d$patient_id == id,]
#     short_therapy <- ("aikuisten" %in% x$string_answer[x$question_code=="erikoisala"]) &
#       ("lyhyt" %in% x$string_answer[x$question_code=="terapian_pituus"])
#     return(short_therapy)
#   }
#
#   # Call functions:
#   # PickVar1
#   # AdultDataQuality
#
#
#   ###### Tidy up the data using the functions #######
#   system.time({
#     dd <- data.frame( patient_id = uids_completed,
#                       primary_care = sapply(uids_completed, IsAdultPrimaryCare),
#                       short_therapy = sapply(uids_completed, IsShortTherapy),
#                       full_data = sapply(uids_completed, AdultDataQuality),
#                       patientQ = sapply(uids_completed, AdultDataQuality, template="patientQ"),
#                       therapistQ = sapply(uids_completed, AdultDataQuality, template="therapistQ"),
#                       questionnaires = sapply(uids_completed, AdultDataQuality, template="questionnaires"))
#   })
#
#   dd <- cbind( dd, therapy_class = factor(rep("long", nrow(dd)), levels = c("long", "PC", "short_SC")) )
#   dd$therapy_class[dd$primary_care] <- "PC"
#   dd$therapy_class[(!dd$primary_care)&dd$short_therapy] <- "short_SC"
#
#
#   ######## Characteristics of patients #########
#   system.time({
#     dd <- cbind(dd,
#                 sex = sapply(uids_completed, PickVar1, varnam="sukupuoli", template="taustatiedot"),
#                 cohabiting = sapply(uids_completed, PickVar1,
#                                     varnam="avioliitto", template="aikuispotilaan_alkuarvio"),
#                 work = sapply(uids_completed, PickVar1,
#                               varnam="tyo_opiskelutilanne", template="aikuispotilaan_alkuarvio"),
#                 therapist = sapply(uids_completed, PickVar1,
#                                    varnam="psykoterapeutti", template="aikuispotilaan_alkuarvio"),
#                 medication = sapply(uids_completed, PickVar1,
#                                     varnam="psyykelaakkeet", template="aikuispotilaan_alkuarvio"),
#                 smoking = sapply(uids_completed, PickVar1,
#                                  varnam="tupakointi", template="aikuispotilaan_alkuarvio"),
#                 diagnosis = sapply(uids_completed, PickVar1,
#                                    varnam="paadiagnoosi", template="taustatiedot"),
#                 therapy = sapply(uids_completed, PickVar1,
#                                  varnam="terapiamuoto", template="taustatiedot"),
#                 framework = sapply(uids_completed, PickVar1,
#                                    varnam="terapiasuuntaus", template="taustatiedot"))
#   })
#
#   # Binary for sex
#   dd <- dd %>% mutate(sex_f = (sex=="F")*1)
#
#   # Add dummy variables for tabulating
#   dd <- fastDummies::dummy_cols(dd,select_columns = c("cohabiting","work","medication","smoking","therapist","therapy","framework"))
#
#   # Combine diagnoses for tabulating
#   dd <- dd %>%
#     mutate(dg_depression = grepl("F32",diagnosis)|grepl("F33",diagnosis)|grepl("F34",diagnosis)|
#              grepl("F35",diagnosis)|grepl("F36",diagnosis)|grepl("F37",diagnosis)|grepl("F38",diagnosis)|
#              grepl("F39",diagnosis)) %>%
#     mutate(dg_anxiety = grepl("F40",diagnosis)|grepl("F41",diagnosis)|grepl("F42",diagnosis)|grepl("F43",diagnosis)|
#              grepl("F44",diagnosis)|grepl("F45",diagnosis)|grepl("F46",diagnosis)|grepl("F47",diagnosis)|
#              grepl("F48",diagnosis)|grepl("F49",diagnosis)) %>%
#     mutate(dg_alcohol = grepl("F10",diagnosis)) %>%
#     mutate(dg_physio = grepl("F50",diagnosis)|grepl("F51",diagnosis)|grepl("F52",diagnosis)|grepl("F53",diagnosis)|
#              grepl("F54",diagnosis)|grepl("F55",diagnosis)|grepl("F56",diagnosis)|grepl("F57",diagnosis)|
#              grepl("F58",diagnosis)|grepl("F59",diagnosis)) %>%
#     mutate(dg_schiz_bd = grepl("F31",diagnosis)|grepl("F20",diagnosis)) %>%
#     mutate(dg_other = !(dg_depression|dg_anxiety|dg_alcohol|dg_physio|dg_schiz_bd))
#
#
#
#
#   ############# Outcomes by class ##############
#   # Call functions
#   # SumById
#   # SumById_core
#
#
#   ddd <- data.frame(patient_id = dd$patient_id, therapy_class = dd$therapy_class,
#                     sofas_bl = sapply(dd$patient_id, function(x) ifelse(x %in% dsof$patient_id, dsof$sofas[dsof$patient_id==x], NA)),
#                     sofas_fu = sapply(dd$patient_id, function(x) ifelse(x %in% dsof$patient_id, dsof$sofasFU[dsof$patient_id==x], NA)),
#                     core_bl = sapply(dd$patient_id, SumById_core, dat=dcom, wave="BL"),
#                     core_fu = sapply(dd$patient_id, SumById_core, dat=dcom, wave="FU"),
#                     oasis_bl = sapply(dd$patient_id, SumById, dat=doasis, wave="BL"),
#                     oasis_fu = sapply(dd$patient_id, SumById, dat=doasis, wave="FU"),
#                     phq_bl = sapply(dd$patient_id, SumById, dat=dphq, wave="BL"),
#                     phq_fu = sapply(dd$patient_id, SumById, dat=dphq, wave="FU"),
#                     audit_bl = sapply(dd$patient_id, SumById, dat=daudit, wave="BL", narm=T),
#                     audit_fu = sapply(dd$patient_id, SumById, dat=daudit, wave="FU", narm=T))
#
#   return(list(d=d, dd=dd, ddd=ddd))
#   # save.image(file = "preprocessed.Rdata")
# }

# PickVar1 <- function(id, varnam, template){
#   return(d$string_answer[(d$patient_id == id)&
#                            (d$template_code==template)&
#                            (d$question_code==varnam)][1])
# }

