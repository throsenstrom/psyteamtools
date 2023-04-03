#' FPQR item data to baseline-follow-up, or pre-post, format
#'
#' @param dorig Wide-format FPQR data without pre-post separation
#' @param items A string vector of item variable names
#' @param dvis FPQR visits data table
#' @param septh Separation threshold in days needed to assiqn pre vs. post statuses. Defaults to 30.
#' @param interrupt_ids Post-values of optional interrupted therapy IDs are forced to missing values. Defaults to NULL.
#' @param use_orig_lab Logical. Use original item labels instead of generic BL and FU. Defaults to FALSE.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' # Read SOFAS scores from database d to pre-post format
#' library(tidyr)
#' library(dplyr)
#' dsof <- dsof_example %>%
#' filter(template_code == "sofas") %>%
#' select(patient_id, date_created, template_code, question_code, number_answer,
#'  treatment_id, visit_id) %>%
#' group_by(question_code) %>%
#' mutate(row = row_number()) %>% # to define unique rows for pivoting
#' ungroup() %>%
#' pivot_wider(names_from = question_code, values_from = number_answer) %>%
#' select(-row)
#'
#' # SOFAS, time, and ID variables
#' dsof <- subset(dsof, select = c(patient_id, sofas_asteikolla, vapaa_aika,
#'                                 tyo_tai_opiskelu,perhe_elama_ja_ihmissuhteet,
#'                                 itsesta_huolehtiminen,date_created,
#'                                 treatment_id, visit_id))
#' # Rename from Finnish to English
#' names(dsof) <- c("patient_id","sofas","freetime","work","family","self",
#' "date_created","treatment_id","visit_id")
#'
#' # Finally, call dataToBLFU_wvis
#' dsof <- dataToBLFU_wvis(dsof, items = names(dsof)[c(2:6)], dvis_example,
#' interrupt_ids = NULL, use_orig_lab = TRUE)
dataToBLFU_wvis <- function(dorig, items, dvis, septh=30, interrupt_ids=NULL, use_orig_lab=FALSE){
  # A sub-function to take first non-missing value
  FirstNonMissing <- function(x) ifelse(any(!is.na(x)),x[!is.na(x)][1],NA)

  # Take the item set only, plus unique patients
  iset <- dorig[,c("patient_id",items,"treatment_id","visit_id","date_created")]
  if (use_orig_lab){
    names(iset) <- c("id", items,"treatment_id","visit_id","cdate")
  } else {
    names(iset) <- c("id", paste0("BL",1:length(items)),"treatment_id","visit_id","cdate")
  }
  iset$cdate <- lubridate::as_date(iset$cdate)
  uds <- unique(iset$id) # unique patients

  # Find patients with multiple treatments and deal with them later
  mts_ids <- sapply(uds, function(x) length(unique(dorig$treatment_id[dorig$patient_id == x])) > 1)
  mts_ids <- uds[mts_ids]
  uds <- setdiff(uds, mts_ids)

  # Take unique duplicated IDs
  udis <- unique(setdiff(iset$id[duplicated(iset$id)], mts_ids))

  # Patch together a data matrix with BL in columns preceeding FU
  ciset <- data.frame(matrix(NA,length(uds),ncol(iset)*2-1))
  if (use_orig_lab){
    names(ciset) <- c(names(iset),paste0(items, "FU"),"treatment_idFU","visit_idFU","cdateFU")
  } else {
    names(ciset) <- c(names(iset),paste0("FU",1:length(items)),"treatment_idFU","visit_idFU","cdateFU")
  }
  for (i in 1:length(uds)){
    dtmp <- iset[iset$id == uds[i],]
    ciset$id[i] <- uds[i]
    # E.g. audit items wind up to multiple rows: take first non-empty
    # A potential problem could be distinct treatment/visit ids for missing values!
    ciset[i,2:ncol(iset)] <- c(apply(dtmp[dtmp$cdate==min(dtmp$cdate),2:(ncol(dtmp)-1)],2,FirstNonMissing),min(dtmp$cdate))
    if (uds[i] %in% udis){
      ciset[i,(ncol(iset)+1):ncol(ciset)] <- c(apply(dtmp[dtmp$cdate==max(dtmp$cdate),2:(ncol(dtmp)-1)],2,FirstNonMissing),max(dtmp$cdate))
    }
  }

  # Add patients with multiple treatments
  if (length(mts_ids) > 0) {
    for (i in 1:length(mts_ids)){
      treatments <- unique(dorig$treatment_id[dorig$patient_id == mts_ids[i]])
      ntreatments <- length(treatments)
      for (j in 1:ntreatments){
        ciset <- rbind(ciset,rep(NA,ncol(ciset)))
        dtmp <- iset[(iset$id == mts_ids[i])&(iset$treatment_id == treatments[j]),]
        ciset$id[nrow(ciset)] <- mts_ids[i]
        # E.g. audit items wind up to multiple rows: take first non-empty
        ciset[nrow(ciset),2:ncol(iset)] <-
          c(apply(dtmp[dtmp$cdate==min(dtmp$cdate),2:(ncol(dtmp)-1)],2,FirstNonMissing),min(dtmp$cdate))
        if (length(unique(dtmp$cdate))>1){
          ciset[nrow(ciset), (ncol(iset)+1):ncol(ciset)] <-
            c(apply(dtmp[dtmp$cdate==max(dtmp$cdate),2:(ncol(dtmp)-1)],2,FirstNonMissing),max(dtmp$cdate))
        }
      }
    }
  }

  # Delete entries with non-matching visit data (i.e. initial visits recorded after midterm/final considered NA)
  for (i in 1:nrow(ciset)){
    vtypeBL <- dvis$visit_type_id[((!is.na(ciset$treatment_id[i])&(!is.na(ciset$visit_id[i]))))&
                                    (ciset$treatment_id[i]==dvis$treatment_id)&(ciset$visit_id[i] == dvis$visit_id)]
    vtypeFU <- dvis$visit_type_id[((!is.na(ciset$treatment_idFU[i])&(!is.na(ciset$visit_idFU[i]))))&
                                    (ciset$treatment_idFU[i]==dvis$treatment_id)&(ciset$visit_idFU[i] == dvis$visit_id)]
    if (length(vtypeBL)!=0){
      if (!is.na(vtypeBL)){
        if (vtypeBL != "initial"){ciset[i,2:ceiling(ncol(ciset)/2)] <- NA}
      }
    }
    if (length(vtypeFU)!=0){
      if ((!is.na(vtypeFU))&(!is.na(ciset$treatment_idFU[i]))){
        if (vtypeFU == "initial"){ciset[i,(ceiling(ncol(ciset)/2)+1):ncol(ciset)] <- NA}
      }
    }
  }

  # Delete follow-up entries separated by <= septh days (set as missing follow-up data)
  ciset$cdate <- lubridate::as_date(ciset$cdate)
  ciset$cdateFU <- lubridate::as_date(ciset$cdateFU)
  ind <- ((!is.na(ciset$cdateFU)) & (!is.na(ciset$cdate))) &(ciset$cdateFU - ciset$cdate <= septh)
  ciset[ind,(ncol(iset)+1):ncol(ciset)] <- NA

  # Delete follow-up entries of interrupted therapies (set as missing follow-up data)
  if (!is.null(interrupt_ids)){
    ind <- ciset$id %in% interrupt_ids
    ciset[ind,(ncol(iset)+1):ncol(ciset)] <- NA
  }

  # Replace working id label with original and return data
  names(ciset)[names(ciset)=="id"] <- "patient_id"
  return(ciset)
}
