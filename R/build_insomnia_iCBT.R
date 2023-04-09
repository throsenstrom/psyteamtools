#' Build a Tidy Insomnia iCBT Data Frame
#'
#' @description Builds a tidy iCBT data frame by joining multiple data tables from
#' the original Healthfox database. One row corresponds to one questionaire
#' question-value pair.
#'
#' @param d_patient Patient data table.
#' @param d_t Therapy data table.
#' @param d_tp TherapyPhase data table.
#' @param d_tpi TherapyPhaseInquiry data table.
#' @param d_tpiq TherapyPhaseInquiryQuestion data table.
#' @param remove_formula_rows Logical, defaults to TRUE. Remove data rows
#' containing Healthfox's formula-based values that are mostly erroneous.
#'
#' @return A data frame
#' @export
#'
#' @examples
#' # Make test data. Alternatively, read in real data.
#' a <- make_testdata_insomnia_iCBT()
#' # Build one joined data frame from the registry tables
#' d <- build_insomnia_iCBT(
#' d_patient = a$d_patient,
#' d_t = a$d_t,
#' d_tp = a$d_tp,
#' d_tpi = a$d_tpi,
#' d_tpiq = a$d_tpiq
#' )
build_insomnia_iCBT <- function(d_patient, d_t, d_tp, d_tpi, d_tpiq, d_tptq,
                                remove_formula_rows = TRUE){
  # Join the analysis data
  library(magrittr)
  library(dplyr)

  d <- d_tpiq %>%
    rename(., iqTitle = Title) %>%
    left_join(., select(d_tpi, !(DateDone)), by = "InquiryId") %>%
    rename(., iTitle = Title) %>%
    left_join(., d_tp[,c("PhaseId","TherapyId","Phase","Duration")],
              by = "PhaseId") %>%
    left_join(., d_t, by = "TherapyId") %>%
    select(!(Title)) %>% # merely repeats the value "Unettomuuden nettiterapia"
    left_join(., d_patient, by = "PatientId")
  if (remove_formula_rows){
    return(d[!grepl("formula", d$Type),])
  } else {
    return(d)
  }
}
