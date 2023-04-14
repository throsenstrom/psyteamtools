
<!-- README.md is generated from README.Rmd. Please edit that file -->

# psyteamtools

<!-- badges: start -->
<!-- badges: end -->

The goal of psyteamtools is to provide data-handling tools for the
[PsyTEAM](https://blogs.helsinki.fi/psyteam-research-group/) -research
group’s common tasks.

## Installation

You can install the development version of `psyteamtools` like so:

``` r
# install.packages("devtools")
devtools::install_github("throsenstrom/psyteamtools")
```

## Examples

### Insomnia internet-delivered Cognitive Behavior Therapy (iCBT)

Normally, one would use this package in a sensitive-data environment,
where one could e.g. read in the registry by running the below code:

``` r
d_t <- vroom::vroom("Therapy.csv") # Therapy data table
d_tp <- vroom::vroom("TherapyPhase.csv") # TherapyPhase data table
d_tpi <- vroom::vroom("TherapyPhaseInquiry.csv") # TherapyPhaseInquiry data table
d_tpiq <- vroom::vroom("TherapyPhaseInquiryQuestion.csv") # TherapyPhaseInquiryQuestion data table
d_tptq <- vroom::vroom("TherapyPhaseTaskQuestion.csv") # TherapyPhaseTaskQuestion data table
d_patient <- vroom::vroom("Patient.csv") # Patient data table
```

Outside an environment with real data, one may generate some fake data
as follows:

``` r
library(psyteamtools)
d <- make_testdata_insomnia_iCBT()
d_patient <- d$d_patient
d_t <- d$d_t
d_tp <- d$d_tp
d_tpi <- d$d_tpi
d_tpiq <- d$d_tpiq
```

One may then build a tidy long-format data frame, where one
question-answer is one row by taking:

``` r
# Build a tidy iCBT data frame by joining multiple original-data tables
d <- build_insomnia_iCBT(
  d_patient = d_patient,
  d_t = d_t,
  d_tp = d_tp,
  d_tpi = d_tpi,
  d_tpiq = d_tpiq
)
```

This function by default removes data rows containing Healthfox’s
formula-based values that are mostly erroneous. A user can compute
correct values from the primary data in the frame. To override this
behavior, use the optional argument `remove_formula_rows = FALSE`.

### Finnish Psychotherapy Quality Registry (FPQR)

One may extract data from the FPQR using e.g. (we use the fake example
data `dsof_example` and `dvis_example` in below)

``` r
# FPQR item data to baseline-follow-up, or pre-post, format
# Read SOFAS scores from database d to pre-post format (using example data)
library(tidyr)
library(dplyr)
dsof <- dsof_example %>%
filter(template_code == "sofas") %>%
select(patient_id, date_created, template_code, question_code, number_answer,
 treatment_id, visit_id) %>%
group_by(question_code) %>%
mutate(row = row_number()) %>% # to define unique rows for pivoting
ungroup() %>%
pivot_wider(names_from = question_code, values_from = number_answer) %>%
select(-row)

# SOFAS, time, and ID variables
dsof <- subset(dsof, select = c(patient_id, sofas_asteikolla, vapaa_aika,
                                tyo_tai_opiskelu,perhe_elama_ja_ihmissuhteet,
                                itsesta_huolehtiminen,date_created,
                                treatment_id, visit_id))
# Rename from Finnish to English
names(dsof) <- c("patient_id","sofas","freetime","work","family","self",
"date_created","treatment_id","visit_id")

# Finally, call dataToBLFU_wvis
dsof <- dataToBLFU_wvis(dsof, items = names(dsof)[c(2:6)], dvis_example,
interrupt_ids = NULL, use_orig_lab = TRUE)
```

### Other old-database iCBT’s

TBA

### Other new-database iCBT’s

TBA

### Hilmo etc.

TBA

## Additional tools

### Direction of dependence inference

We include functions from the paper: Rosenström, T. H., Czajkowski, N.
O., Solbakken, O. A., & Saarni, S. E. (2023). Direction of dependence
analysis for pre-post assessments using non-Gaussian methods: A
tutorial. Psychotherapy Research.
<https://doi.org/10.1080/10503307.2023.2167526>

For use instructions, see the paper or the [Bitbucket
repo](https://bitbucket.org/rosenstroem/dda_prepost/src/master/)
