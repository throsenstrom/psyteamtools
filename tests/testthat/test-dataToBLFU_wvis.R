test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("no errors occur", {
  library(tidyr)
  library(dplyr)
  dsof <- dsof_example %>%
    filter(template_code == "sofas") %>%
    select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
    group_by(question_code) %>%
    mutate(row = row_number()) %>% # to define unique rows for pivoting
    ungroup() %>%
    pivot_wider(names_from = question_code, values_from = number_answer) %>%
    select(-row)
  dsof <- subset(dsof, select = c(patient_id, sofas_asteikolla, vapaa_aika,
                                  tyo_tai_opiskelu, perhe_elama_ja_ihmissuhteet,
                                  itsesta_huolehtiminen,date_created, treatment_id, visit_id))
  names(dsof) <- c("patient_id","sofas","freetime","work","family","self","date_created","treatment_id","visit_id")
  expect_no_error(dataToBLFU_wvis(dsof, items = names(dsof)[c(2:6)], dvis_example,
                          interrupt_ids = NULL, use_orig_lab = T),
                  message = "Data build fails!")
})

############# DON'T RUN / SHOWS HOW TEST DATA WAS CREATED ############
# Create pseudo-data for testing (also in datasets, for the examples)
# npatients <- 100
# nobs <- 2*npatients
# pseudodates <- lubridate::as_date("2020-01-01") +
#   c(1:npatients, 35 + 1:npatients)
# set.seed(87239)
# dsof_example <- data.frame(patient_id = rep(1:npatients, 2),
#                            sofas_asteikolla = runif(nobs,50,100),
#                            vapaa_aika = runif(nobs,50,100),
#                            tyo_tai_opiskelu = runif(nobs,50,100),
#                            perhe_elama_ja_ihmissuhteet = runif(nobs,50,100),
#                            itsesta_huolehtiminen = runif(nobs,50,100),
#                            date_created = pseudodates,
#                            treatment_id = rep(1:npatients, 2),
#                            visit_id = 1:nobs)
# dvis_example <- data.frame(treatment_id = dsof_example$treatment_id,
#                            visit_id = dsof_example$visit_id,
#                            visit_type_id = c(rep("initial",npatients),
#                                              rep("final", npatients)))
# dsof_example <-
#   tidyr::pivot_longer(dsof_example,
#                       2:6,
#                       names_to = "question_code",
#                       values_to = "number_answer")
# dsof_example <- cbind(dsof_example, template_code = "sofas")
