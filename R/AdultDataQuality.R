#' Adult Data Quality function
#' A function to detect data quality (presence of full data, or presence of given dataset if a template is defined)
#'
#' @param id
#' @param template
#'
#' @return
#' @export
#'
#' @examples
#'
AdultDataQuality <- function(id, template=NULL){ # template = "patientQ", "therapistQ", or "questionnaires"
  # define sub-function to investigate presence of BL and FU values in the questionnaire data
  QualityBLFU <- function(dql, use_orig_lab=T){
    if (use_orig_lab){
      return( any(!is.na(dql[,setdiff(names(dql)[grepl("FU", names(dql))],c("treatment_idFU","visit_idFU","cdateFU"))])) &
                any(!is.na(dql[,setdiff(sub("FU", "", names(dql)[grepl("FU", names(dql))]),c("treatment_id","visit_id","cdate"))])) )
    } else {
      return( any(!is.na(dql[,setdiff(names(dql)[grepl("FU", names(dql))],c("treatment_idFU","visit_idFU","cdateFU"))])) &
                any(!is.na(dql[,setdiff(names(dql)[grepl("BL", names(dql))],c("treatment_id","visit_id","cdate"))])) )
    }
  }
  # Quality of full data
  if (is.null(template)){
    has_quality <- (id %in% dsof$patient_id) & (id %in% dcom$patient_id) & (id %in% doasis$patient_id) &
      (id %in% dphq$patient_id) & (id %in% daudit$patient_id)
    if (!has_quality){
      return(has_quality) # return immediately if missing a questionnaire, otherwise check both the baseline and the follow-up...
    } else {
      has_quality <- QualityBLFU(dsof[dsof$patient_id == id, ]) &          # SOFAS
        QualityBLFU(dcom[dcom$patient_id == id, ], use_orig_lab = F) &     # CORE-OM
        QualityBLFU(doasis[doasis$patient_id == id, ], use_orig_lab = F) & # OASIS
        QualityBLFU(dphq[dphq$patient_id == id, ], use_orig_lab = F) &     # PHQ-9
        QualityBLFU(daudit[daudit$patient_id == id, ], use_orig_lab = F)   # AUDIT-C
      # ...and check patient and therapist Q
      x <- d[d$patient_id == id,]
      has_quality <- (all(c("aikuispotilaan_alkuarvio",
                            "aikuispotilaan_loppu_valiarvio",
                            "psykoterapeutin_alkuarvio_aikuiset",
                            "psykoterapeutin_loppu_valiarvio_aikuispotilaat") %in% x$template_code)) &
        has_quality
    }
  } else {
    if (template == "patientQ"){
      x <- d[d$patient_id == id,]
      has_quality <- all(c("aikuispotilaan_alkuarvio",
                           "aikuispotilaan_loppu_valiarvio") %in% x$template_code)
    }
    if (template == "therapistQ"){
      x <- d[d$patient_id == id,]
      has_quality <- all(c("psykoterapeutin_alkuarvio_aikuiset",
                           "psykoterapeutin_loppu_valiarvio_aikuispotilaat") %in% x$template_code)
    }
    if (template == "questionnaires"){
      has_quality <- (id %in% dsof$patient_id) & (id %in% dcom$patient_id) & (id %in% doasis$patient_id) &
        (id %in% dphq$patient_id) & (id %in% daudit$patient_id)
      if (!has_quality){
        return(has_quality) # return immediately if missing a questionnaire
      } else { # check there is something in both the baseline and the follow-up
        has_quality <- QualityBLFU(dsof[dsof$patient_id == id, ]) &          # SOFAS
          QualityBLFU(dcom[dcom$patient_id == id, ], use_orig_lab = F) &     # CORE-OM
          QualityBLFU(doasis[doasis$patient_id == id, ], use_orig_lab = F) & # OASIS
          QualityBLFU(dphq[dphq$patient_id == id, ], use_orig_lab = F) &     # PHQ-9
          QualityBLFU(daudit[daudit$patient_id == id, ], use_orig_lab = F)   # AUDIT-C
      }
    }
    if (!(template %in% c("patientQ","therapistQ","questionnaires"))){
      errorCondition("Undefined template was inserted to the AdultDataQuality function!")
    }
  }
  return(has_quality)
}
