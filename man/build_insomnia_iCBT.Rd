% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_insomnia_iCBT.R
\name{build_insomnia_iCBT}
\alias{build_insomnia_iCBT}
\title{Build a Tidy Insomnia iCBT Data Frame}
\usage{
build_insomnia_iCBT(
  d_patient,
  d_t,
  d_tp,
  d_tpi,
  d_tpiq,
  d_tptq,
  remove_formula_rows = TRUE
)
}
\arguments{
\item{d_patient}{Patient data table.}

\item{d_t}{Therapy data table.}

\item{d_tp}{TherapyPhase data table.}

\item{d_tpi}{TherapyPhaseInquiry data table.}

\item{d_tpiq}{TherapyPhaseInquiryQuestion data table.}

\item{remove_formula_rows}{Logical, defaults to TRUE. Remove data rows
containing Healthfox's formula-based values that are mostly erroneous.}
}
\value{
A data frame
}
\description{
Builds a tidy iCBT data frame by joining multiple data tables from
the original Healthfox database. One row corresponds to one questionaire
question-value pair.
}
\examples{
# Make test data. Alternatively, read in real data.
a <- make_testdata_insomnia_iCBT()
# Build one joined data frame from the registry tables
d <- build_insomnia_iCBT(
d_patient = a$d_patient,
d_t = a$d_t,
d_tp = a$d_tp,
d_tpi = a$d_tpi,
d_tpiq = a$d_tpiq
)
}
