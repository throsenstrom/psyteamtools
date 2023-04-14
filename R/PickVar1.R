#' A function to pick a value from a variable
#'
#' @param id
#' @param varnam
#' @param template
#'
#' @return
#' @export
#'
#' @examples
PickVar1 <- function(id, varnam, template){
  return(d$string_answer[(d$patient_id == id)&
                           (d$template_code==template)&
                           (d$question_code==varnam)][1])
}
