PickVar1 <- function(id, varnam, template){
  return(d$string_answer[(d$patient_id == id)&
                           (d$template_code==template)&
                           (d$question_code==varnam)][1])
}