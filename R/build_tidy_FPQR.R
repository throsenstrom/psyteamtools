#' Build Tidy FPQR Wide-Format Data Frame
#'
#' @description Builds a tidy FPQR data. This an early-stage function:
#' if expecting multiple roles for same patients, the function may have
#' inadvertent consequences due to ID-based removal of patients in a role.
#'
#' @param d_forms The BCB forms data table
#' @param d_tre The BCB treatments data table
#' @param d_vis The BCB visits data table
#' @param d_pat The BCB patients data table
#' @param no_kela Exclude SSI (Kela) therapies. Logical value. The default.
#' @param no_kids Exclude non-adults. Logical value. The default.
#' @param no_interrupted_therapies Exclude interrupted therapies. Set FALSE only if you know what you are doing.
#' @param no_repeated_FPQR_treatments Exclude reruns. Set FALSE only if you know what you are doing.
#' @param no_internal_therapies Exclude HUS internal therapies.
#'
#' @return A list of data frames
#' @export
#'
#' @examples
build_tidy_FPQR <- function(d_forms, d_tre, d_vis, d_pat, no_kela = T,
                            no_kids = T, no_interrupted_therapies = T,
                            no_repeated_FPQR_treatments = T,
                            no_internal_therapies = T){
  library(dplyr)
  library(tidyr)

  d <- d_forms; rm(d_forms) # revert to old naming convention
  # Uniques IDs
  uids <- unique(d$patient_id)
  print(paste0("Unique patients: ", length(uids)))

  if (no_kela){
    # Find patients in Kela treatments
    pmuoto_ids <- unique(d$patient_id[(d$string_answer=="kela")&(d$question_code=="palvelumuoto")])
    length(pmuoto_ids)
    uids <- setdiff(uids, pmuoto_ids)
  }

  if (no_kids){
    # DROP NON-ADULTS HERE (9.12.2021)
    uids <- uids[uids %in% d$patient_id[(d$question_code=="erikoisala")&(d$string_answer=="aikuisten")]]
  }

  # Uninitiated psychotherapies
  uids_uninitiated <- uids[!(uids %in% filter(d, (template_code == "psykoterapeutin_alkuarvio_nuoret") |
                                                (template_code == "psykoterapeutin_alkuarvio_aikuiset") |
                                                (template_code == "psykoterapeutin_alkuarvio_lapset"))$patient_id)]

  # Uninitiated psychotherapies
  uids_uninitiated <- uids[!(uids %in% filter(d, (template_code == "psykoterapeutin_alkuarvio_nuoret") |
                                                (template_code == "psykoterapeutin_alkuarvio_aikuiset") |
                                                (template_code == "psykoterapeutin_alkuarvio_lapset"))$patient_id)]

  # Altogether X completed psychotherapies
  uids_completed <- uids[(uids %in% filter(d, (template_code == "psykoterapeutin_loppu_valiarvio_nuoret") |
                                             (template_code == "psykoterapeutin_loppu_valiarvio_aikuispotilaat") |
                                             (template_code == "psykoterapeutin_loppu_valiarvio_lapset"))$patient_id)]

  # Check how numbers change when requiring final/midterm stamp too (now only 3 more adults drops out)
  uids_completed_stamped <- rep(F, length(uids_completed))
  uids_midterm_only_stamped <- rep(F, length(uids_completed))
  for (i in 1:length(uids_completed)){
    trids <- unique(d$treatment_id[d$patient_id == uids_completed[i]])
    uids_completed_stamped[i] <- any(d_vis$visit_type_id[d_vis$treatment_id %in% trids] %in% c("final", "midterm"))
    uids_midterm_only_stamped[i] <- any(d_vis$visit_type_id[d_vis$treatment_id %in% trids] == "midterm") &
      !any(d_vis$visit_type_id[d_vis$treatment_id %in% trids] == "final")
  }
  uids_midterm_only <- uids_completed[uids_midterm_only_stamped]
  uids_completed <- uids_completed[uids_completed_stamped]

  if (no_interrupted_therapies){
    # remove interrupted
    uids_completed <- setdiff(uids_completed, uids_interrupted)
    uids_midterm_only <- setdiff(uids_midterm_only, uids_interrupted)
  }

  ######### Describe register evolution #########

  # Every entered patient
  entries <- lubridate::as_date(
    sapply(uids, function(x) min(lubridate::as_date(d$date_created[d$patient_id == x]),na.rm=T)))

  # Initiated therapies
  dtmp <- filter(d, (template_code == "psykoterapeutin_alkuarvio_nuoret") |
                   (template_code == "psykoterapeutin_alkuarvio_aikuiset") |
                   (template_code == "psykoterapeutin_alkuarvio_lapset"))
  inits <- lubridate::as_date(
    sapply(setdiff(uids,uids_uninitiated),
           function(x) min(lubridate::as_date(dtmp$date_created[dtmp$patient_id == x]),na.rm=T)))

  # Completed or interrupted therapies
  ended <- lubridate::as_date(
    sapply(unique(c(uids_completed, uids_interrupted)),
           function(x) max(lubridate::as_date(d$date_created[d$patient_id == x]),na.rm=T)))

  entries <- sort(entries)
  inits <- sort(inits)
  ended <- sort(ended)

  ########################################################################

  if (no_repeated_FPQR_treatments){

    ##################################################################################
    ########## Then restrict to adults with 1 outsourced non-Kela therapy ############
    ##################################################################################

    # Find patients with multiple treatments
    mts_ids <- sapply(uids_completed, function(x) length(unique(d$treatment_id[d$patient_id == x])) > 1)
    mts_ids <- uids_completed[mts_ids]
    uids_completed <- setdiff(uids_completed, mts_ids)
    uids_midterm_only <- setdiff(uids_midterm_only, mts_ids)
  }
  if (no_internal_therapies){
    # Remove internal psyhotherapies, leave outsourced only (26.1.2022)
    int_ids <- sapply(uids_completed, function(x) ("sisainen" %in% d$string_answer[(d$patient_id==x)&(d$question_code=="palvelumuoto")]))
    int_ids <- uids_completed[int_ids]
    uids_completed <- setdiff(uids_completed, int_ids)
    uids_midterm_only <- setdiff(uids_midterm_only, int_ids)
  }

  ##################################################################################

  ### Pull data to wide tables ###

  ##### Background info ####
  dbag <- d %>%
    filter(template_code == "taustatiedot") %>%
    select(patient_id, date_created, template_code, question_code, string_answer) %>%
    pivot_wider(names_from = question_code, values_from = string_answer)

  dbag <- cbind(dbag,
                many_treatments =
                  sapply(dbag$patient_id, function(x) (length(unique(d$treatment_id[d$patient_id==x])) > 1)*1))

  ########## SOFAS ##########
  dsof <- d %>%
    filter(template_code == "sofas") %>%
    select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
    group_by(question_code) %>%
    mutate(row = row_number()) %>% # to define unique rows for pivoting
    ungroup() %>%
    pivot_wider(names_from = question_code, values_from = number_answer) %>%
    select(-row)

  # Use the visit data
  dsof <- subset(dsof, select = c(patient_id, sofas_asteikolla, vapaa_aika,
                                  tyo_tai_opiskelu, perhe_elama_ja_ihmissuhteet,
                                  itsesta_huolehtiminen,date_created, treatment_id, visit_id))
  names(dsof) <- c("patient_id","sofas","freetime","work","family","self","date_created","treatment_id","visit_id")
  dsof <- dataToBLFU_wvis(dsof, items = names(dsof)[c(2:6)], d_vis, interrupt_ids = uids_interrupted, use_orig_lab = T)

  # A SOFAS-based multiple treatments variable
  dbag <- cbind(dbag,
                many_sofases =
                  sapply(dbag$patient_id, function(x){(length(unique(
                    na.omit(union(dsof$treatment_id[dsof$patient_id==x],dsof$treatment_idFU[dsof$patient_id==x]))
                  )) > 1)*1})
  )

  ########### CORE-OM ############
  dcom <- d %>%
    filter(template_code == "core_om") %>%
    select(patient_id, date_created, template_code, question_name, number_answer, treatment_id, visit_id) %>%
    group_by(question_name) %>%
    mutate(row = row_number()) %>% # to define unique rows for pivoting
    ungroup() %>%
    pivot_wider(names_from = question_name, values_from = number_answer) %>%
    select(-row)

  # Sort the CORE-OM items
  ilabs <- names(dcom)[grepl(" ", substr(names(dcom),1,3), fixed = T)]
  x <- sort(as.numeric(substr(ilabs,1,2)), index.return = T)
  dcom <- dcom[,c("patient_id", "date_created", ilabs[x$ix], "treatment_id","visit_id")]

  ### CORE-OM to 1st and last ###
  dcom <- dataToBLFU_wvis(dcom, items = names(dcom)[3:36], d_vis, interrupt_ids = uids_interrupted)

  ########## PHQ-9 ##########
  dphq <- d %>%
    filter(template_code == "PHQ9") %>%
    select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
    group_by(question_code) %>%
    mutate(row = row_number()) %>% # to define unique rows for pivoting
    ungroup() %>%
    pivot_wider(names_from = question_code, values_from = number_answer) %>%
    select(-row)

  dphq <- dataToBLFU_wvis(dphq, items = names(dphq)[6:14], d_vis, interrupt_ids = uids_interrupted)

  ########## OASIS ##########
  doasis <- d %>%
    filter(template_code == "oasis") %>%
    select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
    group_by(question_code) %>%
    mutate(row = row_number()) %>% # to define unique rows for pivoting
    ungroup() %>%
    pivot_wider(names_from = question_code, values_from = number_answer) %>%
    select(-row)

  doasis <- dataToBLFU_wvis(doasis, items = names(doasis)[6:10], d_vis, interrupt_ids = uids_interrupted)

  ########## AUDIT ##########
  daudit <- d %>%
    filter(template_code == "audit_c") %>%
    select(patient_id, date_created, template_code, question_code, number_answer, treatment_id, visit_id) %>%
    group_by(question_code) %>%
    mutate(row = row_number()) %>% # to define unique rows for pivoting
    ungroup() %>%
    pivot_wider(names_from = question_code, values_from = number_answer) %>%
    select(-row)

  daudit <- dataToBLFU_wvis(daudit, items = names(daudit)[2+c(4,5,8)], d_vis, interrupt_ids = uids_interrupted)
}
