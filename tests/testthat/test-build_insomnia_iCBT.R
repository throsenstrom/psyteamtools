test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

################## Make some test data #####################
set.seed(20230407)
n <- 50 # fifty patients
# Patient data table
pcode <- rbinom(n,2,0.5)+1
yrs <- rep(lubridate::as_date("1960-01-01"),n)
lubridate::year(yrs) <- lubridate::year(yrs) + rpois(n,30)
d_patient <-
  data.frame(PatientId = as.character(1:n),
             Gender = c("Male","Female")[rbinom(n,1,0.5)+1],
             DateOfBirth = yrs,
             PostalCode = c("02780", NA, "01190")[pcode],
             City = c("Espoo", "Helsinki", "Sipoo")[pcode],
             DateRegistered = lubridate::as_date("2020-01-01") + runif(n,1,450),
             AppVersion = c("Android 10 v570", "ios 15.1. v12.3.4")[rbinom(n,1,0.5)+1])
# Therapy data table
nPhase <- round(runif(n,1,8)) # Number of phases done per patient
phases <- unlist(sapply(nPhase, function(x) (1:8)[1:x]))

d_t <- data.frame(PatientId = d_patient$PatientId,
                  TherapyId = as.character(1:n + n*5),
                  TherapistId = (1:4)[round(runif(n,1,4))],
                  Title = c(rep("Unettomuuden nettiterapia",n-1),
                            "Nätteterapi mot sömnlöshet"),
                  LatestPhase = nPhase,
                  DateCreated = d_patient$DateRegistered,
                  DateStarted = d_patient$DateRegistered,
                  DateComplete = d_patient$DateRegistered,
                  DateCanceled = rep(NA,n),
                  DateContacted = d_patient$DateRegistered,
                  DateOpened = d_patient$DateRegistered,
                  DateInvoiced = d_patient$DateRegistered + 45,
                  DateClosed = d_patient$DateRegistered + 45,
                  DateCompleted = d_patient$DateRegistered + 45)

# TherapyPhase data table
d_tp <-
  data.frame(TherapyId = unlist(sapply(d_t$TherapyId,
                                       function(x) rep(x,nPhase[which(d_t$TherapyId==x)]))),
             PhaseId = as.character(1:sum(nPhase)),
             Phase = phases,
             Duration = round(rexp(sum(nPhase),1/8)),
             DateStarted = rep(lubridate::as_date("2021-01-01"),sum(nPhase)))
d_tp <- cbind(d_tp,
              DateEnded = d_tp$DateStarted + d_tp$Duration,
              DateCanceled = c(rep(NA,nrow(d_tp)-1),lubridate::as_date("2022-01-01")))

# TherapyPhaseInquiry data table
pwinu <- d_tp$PhaseId[d_tp$Phase %in% c(1,4,5,7,8)] # PhaseIds with unettomuuden kysely
pwina <- d_tp$PhaseId[d_tp$Phase %in% c(1,7,8)] # PhaseIds with alkoholikysely
ni <- length(c(pwinu,pwina))
d_tpi <- data.frame(PhaseId = c(pwinu,pwina),
                    InquiryId = as.character(1:ni),
                    Title = c(rep("Unettomuuden arvio",length(pwinu)),
                              rep("Alkoholikysely",length(pwina))),
                    DateDone = lubridate::as_date("2021-01-01"))
for (i in 1:nrow(d_tpi)) d_tpi$DateDone[i] <- d_tp$DateStarted[d_tpi$PhaseId[i] == d_tp$PhaseId][1]
#sapply(c(pwinu,pwina), function(x) d_tp$DateStarted[x == d_tp$PhaseId]))

# TherapyPhaseInquiryQuestion data table
uqs <- c("Unettomuuden haitta-asteen pisteet",
         "Kuinka huolestunut tai ahdistunut olet tämänhetkisen nukkumisongelmasi vuoksi",
         "Missä määrin arvioit nukkumisongelmasi häiritsevän päivittäistä toimintaasi",
         "Kuinka helposti luulet muiden huomaavan nukkumisongelmasi heikentäneen elämänlaatuasi",
         "Unessapysymisvaikeus",
         "Nukahtamisvaikeus",
         "Kuinka tyytyväinen tai tyytymätön olet tämänhetkiseen nukkumiseesi",
         "Liian aikainen herääminen aamulla")
aqs <- c("Alkoholikyselyn pisteet",
         "Kuinka usein olet juonut kerralla kuusi tai useampia annoksia?",
         "Kuinka usein juot olutta, viiniä tai muita alkoholijuomia?",
         "Kuinka monta annosta alkoholia yleensä olet ottanut niinä päivinä, jolloin käytit alkoholia?")
opts <- c("Ei", "Vähän", "Jonkin verran", "Paljon", "Erittäin paljon")

d_tpiq <- data.frame(InquiryId = c(rep(pwinu, each=length(uqs)),
                                   rep(pwina, each=length(aqs))),
                     Title = c(rep(uqs,length(pwinu)),
                               rep(aqs,length(pwina))),
                     Value = 0,
                     Type = c(rep(c("formula-integer",rep("option-multiple",7)),length(pwinu)),
                              rep(c("formula-integer",rep("option-multiple",3)),length(pwina))))
d_tpiq <- cbind(d_tpiq,
                DateDone = lubridate::as_date("2021-01-01"), #sapply(d_tpiq$InquiryId, function(x) d_tpi$DateDone[d_tpi$InquiryId==x][1]),
                Text = rep("-", nrow(d_tpiq)))
for (i in 1:nrow(d_tpiq)) d_tpiq$DateDone[i] <- d_tpi$DateDone[d_tpi$InquiryId==d_tpiq$InquiryId[i]][1]
d_tpiq$Value[d_tpiq$Type=="option-multiple"] <- rbinom(nrow(d_tpiq)-sum(d_tpiq$Type=="formula-integer"),4,0.3)
d_tpiq$Text[d_tpiq$Type=="option-multiple"] <- opts[d_tpiq$Value[d_tpiq$Type=="option-multiple"]+1]

############################################################

################# Then test using it #######################
test_that("no errors occur", {
  expect_no_error(build_insomnia_iCBT(d_patient, d_t, d_tp, d_tpi, d_tpiq),
                  message = "Data build fails!")
})

############################################################
