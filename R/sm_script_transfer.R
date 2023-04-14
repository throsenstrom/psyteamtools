#' Sanna's scripts commented out
#'
#' @return
#' @export
#'
#' @examples
sm_script_transfer <- function() {
  return(1+2)


  # library(XML)
  # library(stringr)
  #
  #
  # # PREPROCESS
  # # Load datas
  # dd <- xmlToDataFrame("masennus_viestidata")
  # dg <- xmlToDataFrame("gad_viestidata")
  #
  # # Filter away automatic system messages
  # dd <- dd[dd$Creator == "Minä" | dd$Creator == "Terapeutti", ]
  # dg <- dg[dg$Creator == "Minä" | dg$Creator == "Terapeutti", ]
  #
  # # Load symptom data
  # sd <- xmlToDataFrame("~/shared_with_jupyter/data/dep_bdi.xml", stringsAsFactors = FALSE)
  # sg <- xmlToDataFrame("~/shared_with_jupyter/data/gad_mittari_gad7.xml", stringsAsFactors = FALSE)
  #
  # # Load symptom data
  # sd <- xmlToDataFrame("~/shared_with_jupyter/data/dep_bdi.xml", stringsAsFactors = FALSE)
  # sg <- xmlToDataFrame("~/shared_with_jupyter/data/gad_mittari_gad7.xml", stringsAsFactors = FALSE)
  #
  # #############################################################################################################
  #
  # # Text data
  #
  # # Add variable that denotes the treatment programme
  # dd[, "treatment"] <- rep("dep")
  # dg[, "treatment"] <- rep("gad")
  #
  # # Drop PotilasID to avoid confusion
  # dd = subset(dd, select = -PotilasID)
  # dg = subset(dg, select = -PotilasID)
  #
  # # Rename text variable
  # names(dd)[4] <- "text"
  # names(dg)[4] <- "text"
  #
  #
  # ###############################################################################################################
  #
  # # Symptom data
  #
  # # GAD-7
  # # Change text answer to numeric ordinal values
  # assnums <- function(x) {
  #   dplyr::case_when(x == "Ei lainkaan" ~ 0,
  #                    x == "Useana päivänä" ~ 1,
  #                    x == "Suurimpana osana päivistä" ~ 2,
  #                    x == "Lähes joka päivä" ~ 3)
  # }
  #
  # # Count gad-7 sum score
  # sg[,5:11] <- sapply(sg[,5:11], assnums)
  # sg <- cbind(sg[,2:4], gad7 = rowSums(sg[,5:11]))
  #
  # # Remove unnecessary columns from dep
  # sd <- subset(sd, select = c(2:4, 25))
  #
  # # Rename session column
  # names(sd)[2] <- "session"
  # names(sg)[3] <- "session"
  #
  # # Rename symptom score variable
  # names(sd)[4] <- "symptom_score"
  # names(sg)[4] <- "symptom_score"
  #
  # # Add variable that denotes the treatment programme
  # sd[, "treatment"] <- rep("dep")
  # sg[, "treatment"] <- rep("gad")
  #
  #
  # #################################################################################################################
  #
  # # Which patients sent messages but did not start the treatment
  # ddrop <- unique(dd$UID[which(!dd$UID %in% sd$UID)])
  # gdrop <- unique(dg$UID[which(!dg$UID %in% sg$UID)])
  #
  # # Drop those who did not start
  # length(unique(dd$UID)) # 4088
  # dd <- dd[which(!dd$UID %in% ddrop), ] # -250
  # length(unique(dd$UID)) #3838
  #
  # dg <- dg[which(!dg$UID %in% gdrop), ]
  #
  #
  # ###################################################################################################################
  #
  # # Drop patients with baseline score under a cutpoint
  # # Drop from both text corpus and symptom dataframe
  # # 8 for gad-7 https://doi.org/10.1016/j.genhosppsych.2015.11.005
  # # 10 for BDI-II https://doi.org/10.1016/0272-7358(88)90050-5 https://www.terveysportti.fi/apps/dtk/tmi/article/tmm00157?toc=307487 (/Käypä hoito)
  #
  #
  # # dep
  # sd$symptom_score <- as.numeric(sd$symptom_score)
  # ddrop <- unique(sd$UID[(sd$session == 1)&(sd$symptom_score < 10)])
  # sd <- sd[!(sd$UID %in% ddrop), ]
  # dd <- dd[!(dd$UID %in% ddrop), ]
  #
  # # GAD
  # gdrop <- unique(sg$UID[(sg$session == 1)&(sg$symptom_score < 8)])
  # sg <- sg[!(sg$UID %in% gdrop), ]
  # dg <- dg[!(dg$UID %in% gdrop), ]
  #
  #
  # mean(sd$symptom_score[sd$session == 1])
  # mean(sg$symptom_score[sg$session == 1])
  #
  #
  #
  # # DROP SOME APPARENT TEST USERS
  # # Note! These are not systematically selected
  # ddrop <- unique(dd$UID[-which(grepl("potilas", dd$UID, fixed = TRUE))]) # Drop patients that don't have "potilas" in their ID
  # ddrop <- c(ddrop, unique(sd$UID[-which(grepl("potilas", sd$UID, fixed = TRUE))]))
  # ddrop <- c(ddrop, "potilas-18-d3323")
  # gdrop <- unique(dg$UID[-which(grepl("potilas", dg$UID, fixed = TRUE))])
  # gdrop <- c(gdrop, unique(sg$UID[-which(grepl("potilas", sg$UID, fixed = TRUE))]))
  #
  # dd <- dd[!(dd$UID %in% ddrop), ]
  # sd <- sd[!(sd$UID %in% ddrop), ]
  #
  # dg <- dg[!(dg$UID %in% gdrop), ]
  # sg <- sg[!(sg$UID %in% gdrop), ]
  #
  #
  # ############################################################################################
  #
  # # Data for LDA modeling
  #
  # # NOTE! Cross validation procedure should contain both symptoms and texts and thetas
  #
  # # Build datas so that they are easy to match/merge inside the cross validation loop
  #
  # # Also note! DTM/dgCMatrix should be formed in the cross validation loop? OR a list beforehand?
  #
  #
  # # Check if dataframes include patients with same code
  # #sum(dd$UID %in% dg$UID) # 0
  # #sum(dg$UID %in% dd$UID) # 0
  #
  # # Merge datas
  # d <- merge(dd, dg, all = TRUE)
  #
  #
  #
  # # Count number of messages before dropping rows
  # # Count a) number of messages from every patient AND
  # # b) number of messages sent by a therapist to a certain patient
  # d_pot <- d %>%
  #   filter(Creator == "Minä") %>%
  #   group_by(UID) %>%
  #   mutate(messages_pot = n())
  #
  # d_ter <- d %>%
  #   filter(Creator == "Terapeutti") %>%
  #   group_by(UID) %>%
  #   mutate(messages_ter = n())
  #
  # d <- merge(d_pot, d_ter, all = TRUE) # 54282
  #
  #
  #
  # # remove emojis
  # # SKIP FOR EMOJI DATA
  # text_locate(d$text, "😊")
  # d$text <- stri_replace_all_regex(d$text, "[\U{1F300}-\U{1F6FF}]", " ") # removes at least the most common ones
  # text_locate(d$text, "😊")
  #
  #
  #
  # # Find and remove messages not written in Finnish
  # # profiles <- TC_byte_profiles[names(TC_byte_profiles) %in% c("finnish", "english", "swedish")]
  # # lang <- textcat(d$text, p = profiles)
  # # table(lang)
  # #
  # # d$text[which(lang == "swedish")] # 21, 74, 75
  # # which(lang == "swedish") # 13621, 24921, 27783
  # # d$text[c(13621, 24921, 27783)]
  # # lang[c(13621, 24921, 27783)] <- "finnish"
  # #
  # # d$text[which(lang == "english")] # 31
  # # which(lang == "english") # 35933
  # # d$text[35933]
  # # lang[35933] <- "finnish"
  #
  # # save(lang, file = "lang.Rdata")
  # load("lang.Rdata")
  #
  # # SKIP FOR EMOJI DATA
  # swe <- grep("swedish", lang)
  # eng <- grep("english", lang)
  # drop_rows <- c(swe, eng)
  # d <- d[-drop_rows,]
  #
  # # Check for misclassifications
  # text_locate(d$text, c("och", "jag", "inte", "från", "tack")) # Näissä sekaisin suomea ja ruotsia
  # text_locate(d$text, c("and", "if", "my", "from", "Thank", "Best", "Dear"))
  #
  #
  #
  # # # Script for removing names
  # # # nimet2.txt needs to be uploaded first
  # # nam_dat <- read.delim("nimet2.txt")
  # # nimet <- as.vector(nam_dat$nimi)
  # # save(nimet, file = "nimet.Rdata")
  #
  #
  # ##################################################################################################
  # # Identify therapist messages with emojis or emoticons
  # hymiot <- c(":)", ":-)", ":D", ":-D", "xD", "x-D", "XD", "X-D", ";)", ";-)", ":(", ":-(", ":')", ":'-)", ";(", ";-(",
  #             ":'(", ":'-(", ":o", ":-o", ":O", ":-O", "=)", "=-)", "=D", "=-D", "=)", "=-)", "=(", "=-(", "=')", "='-)",
  #             "='(", "='-(", "=o", "=-o", "=O", "=-O")
  # d_ter$n_emoticons <- stri_count(d_ter$text, fixed = hymiot)
  # d_ter$n_emojis <- stri_count(d_ter$text, charclass = "[\U{1F300}-\U{1F6FF}]")
  #
  # d_ter$emo <- case_when(d_ter$n_emojis > 0 | d_ter$n_emoticons > 0 ~ 1,
  #                        TRUE ~ 0)
  #
  # ##table(d_ter$emo)
  #
  # ################################################################################################
  #
  #
  #
  #
  #
  # # Add names and "hei" to stopwords list
  # # length(stopwords_fi)
  # # stopwords_fi[230] <- "hei"
  # load("nimet.Rdata")
  # stopw <- c(stopwords_fi, nimet, "hei")
  #
  # # Tokenize text
  # d[, "textTokens"] <- NA
  # for (i in 1:nrow(d)) {
  #   t <- text_tokens(d[i,], text_filter(drop_punct = TRUE, drop_number = TRUE, drop = stopw), stemmer = "fi")
  #   n <- length(t[[1]])
  #   tt <- ""
  #   for(j in 1:n) {
  #     ttt <- t[[1]][j]
  #     tt <- paste(tt, ttt, sep = " ")
  #   }
  #   d$textTokens[i] <- tt
  # }
  #
  #
  # # Count number of tokens per document
  # d[, "numberOfTokens"] <- NA
  # for(i in 1:nrow(d)) {
  #   d$numberOfTokens[i] <- sapply(strsplit(d$textTokens[i], " "), length)
  #   d$numberOfTokens[i] <- d$numberOfTokens[i]-1
  # }
  # mean(d$numberOfTokens)
  #
  #
  # # Drop rows with less than 5 words
  # drop_rows <- which(d$numberOfTokens < 5)
  # d <- d[-drop_rows,]
  # mean(d$numberOfTokens)
  #
  #
  #
  # # TÄSSÄ VÄLISSÄ AJASTA JA FILTTERÖI VIESTIT
  #
  # d_s <- merge(sd[, c(1:3, 5)], sg[, c(1:3, 5)], by = c("UID", "session", "Created", "treatment"), all = TRUE)
  # d_s$Created <- dmy_hms(d_s$Created)
  # d_s$session <- as.numeric(d_s$session)
  #
  # d_s <- d_s[with(d_s, order(UID, session)), ]
  #
  #
  # # Add variable for time interval
  # d_s[, "interval"] <- NA
  # u <- unique(d_s$UID)
  # fut_d <- as_datetime(now())
  # for (i in 1:length(u)) {
  #   ses <- d_s$session[d_s$UID == u[i]]
  #   interval_list <- vector(mode = "list", length = length(ses))
  #   for(j in 1:length(ses)) {
  #     if (j == length(ses)) {
  #       interval_list[[j]] <- interval(d_s$Created[(d_s$UID == u[i])&(d_s$session == ses[j])], fut_d)
  #     } else
  #       interval_list[[j]] <- interval(d_s$Created[(d_s$UID == u[i])&(d_s$session == ses[j])],
  #                                      d_s$Created[(d_s$UID == u[i])&(d_s$session == ses[j+1])])
  #   }
  #   for(k in 1:length(interval_list)) {
  #     d_s$interval[(d_s$UID == u[i])&(d_s$session == ses[k])] <- paste0(interval_list[[k]])
  #   }
  # }
  #
  # # Merge datasets for timing
  # df_m <- merge(d_s[, c(1,2,4,5)], d, by = c("UID", "treatment"), all =  TRUE)
  # df_m$Created <- dmy_hms(df_m$Created)
  # length(unique(df_m$UID))
  # sum(is.na(df_m$Created))
  #
  # df_m <- df_m[with(df_m, order(UID, session)), ]
  #
  # # Time diary entries according to session mumber of symptom score
  # df_m[,"CreatedAfterSession"] <- NA
  # u2 <- unique(df_m$UID)
  # for (i in 1:length(u2)) {
  #   times <- unique(df_m$Created[df_m$UID == u2[i]])
  #   ses <- df_m$session[df_m$UID == u2[i]]
  #   for (j in 1:length(ses)) {
  #     s_times <- strsplit(df_m$interval[(df_m$UID == u2[i])&(df_m$session == ses[j])], "--")
  #     start_t <- ymd_hms(s_times[[1]][1])
  #     end_t <- ymd_hms(s_times[[1]][2])
  #     s_int <- interval(start_t, end_t)
  #     for (k in 1:length(times)) {
  #       if (is.na(times[k])) {
  #         df_m$CreatedAfterSession[(df_m$UID == u2[i])&(df_m$Created == times[k])] <- NA
  #       }
  #       else if (times[k] %within% s_int) {
  #         df_m$CreatedAfterSession[(df_m$UID == u2[i])&(df_m$Created == times[k])] <- ses[j]
  #       }
  #     }
  #   }
  # }
  #
  # sum(is.na(df_m$CreatedAfterSession))
  #
  # df_m <- df_m %>%
  #   mutate_at(vars(CreatedAfterSession), ~replace(., is.na(.), 0))
  #
  #
  # d <- merge (d, df_m[, c(1,7,12)], by = c("UID", "text"), all.x = TRUE, all.y = FALSE)
  # d <- distinct(d)
  #
  #
  #
  # # Split accordig to the sender of the message
  # d_pot <- d[which(d$Creator == "Minä"), c(1:6, 8:10)] # Patient
  # d_ter <- d[which(d$Creator == "Terapeutti"), c(1:5, 7, 8:10)] # Therapist
  #
  # mean(d_pot$numberOfTokens)
  # mean(d_ter$numberOfTokens)
  #
  # sd(d_pot$numberOfTokens)
  # min(d_pot$numberOfTokens)
  # max(d_pot$numberOfTokens)
  #
  # sd(d_ter$numberOfTokens)
  # min(d_ter$numberOfTokens)
  # max(d_ter$numberOfTokens)
  #
  # # JATKA TÄSTÄ
  # # Perkaa drop-cont4 niin, että saisit edustavat jutut
  # # Tee testi LDA-malli ja tarkista, nouseeko loppuyhteenvedot vielä
  # # Jos ei -> tallenna data
  # # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #
  # drop_cont4 <- c("Tässä yhteenveto nettiterapiasta",
  #                 "yleistyneen ahdistuneisuushäiriön nettiterapian ajalla",
  #                 "Onnittelut hyvästä työskentelystä ja loppuun viedystä prosessista",
  #                 "Loppuyhteenveto",
  #                 "NETTITERAPIAN PÄÄTTYMINEN",
  #                 "olet saanut tämän",
  #                 "masennuksen nettiterapian ajalla",
  #                 "nettiterapian päätökseen!",
  #                 "Haastamisen vastapainona tutustuttiin") # Loppuyhteenvedot
  #
  # d_drop1 <- d_ter[grepl(paste(drop_cont1, collapse = "|"), d_ter$text), ]
  #
  # d_drop2 <- d_ter[grepl(paste(drop_cont2, collapse = "|"), d_ter$text), ]
  #
  # d_drop3 <- d_ter[grepl(paste(drop_cont3, collapse = "|"), d_ter$text), ]
  #
  # d_drop4 <- d_ter[grepl(paste(drop_cont4, collapse = "|"), d_ter$text), ]
  #
  # d_drop_list <- list(d_drop1, d_drop2, d_drop3, d_drop4)
  # d_drop_k <- Reduce(function(x, y) merge(x, y, all=TRUE), d_drop_list)
  # remove(d_drop_list)
  #
  # d_drop1 <- d_drop1[c(4:7,9:11,27,30,33:35,1006,1010,2101,2138,3554,3599), ]
  # d_drop2 <- d_drop2[c(2:5,10,11,14,15,20,25,33,45,56,60,5004,5037,7005,7048), ]
  # d_drop3 <- d_drop3[c(1:4,6,8,10,14:15,24,27,53,58,62,2501,2520,2542), ]
  # d_drop4 <- d_drop4[c(2,4,5,7,22,24,34,103,105,106,108,112,116,117,124,133,135,141,
  #                      904,912,919,939,1401,1402,1979,2217,2225), ]
  #
  #
  # d_drop_list <- list(d_drop1, d_drop2, d_drop3, d_drop4)
  # d_drop <- Reduce(function(x, y) merge(x, y, all=TRUE), d_drop_list)
  # remove(d_drop_list)
  # remove(d_drop1)
  # remove(d_drop2)
  # remove(d_drop3)
  # remove(d_drop4)
  #
  #
  #
  # # Remove standard messages from therapist data
  #
  # d_ter$standard_mes <- rep(NA)
  # t0 <- proc.time()
  # for (i in 1:nrow(d_ter)) {
  #   lev_sims <- sapply(1:nrow(d_drop), function(x) RecordLinkage::levenshteinSim(d_ter[i,7], d_drop[x,7]))
  #   max_sim <- max(lev_sims)
  #   d_ter$standard_mes[i] <- max_sim
  # }
  # (runtime <- proc.time() - t0) # ~48 min
  #
  #
  # # TARKISTA TÄSSÄ JOSSAIN, ONKO LöYDETYT ID:T TIPUTETTAVIEN JOUKOSSA!!!
  #
  #
  #
  #
  #
  #
  #
  # #d_ter2 <- d_ter[(d_ter$standard_mes > 0.34 & d_ter$standard_mes < 0.46), c(4,9)]
  #
  #
  # # Compare
  # d_drop_k_ter <- d_ter[which(d_ter$standard_mes > 0.38), ]
  #
  # d_drop_dif <- d_drop_k_ter[which(!d_drop_k_ter$text %in% d_drop_k$text), ]
  # d_drop_dif2 <- d_drop_k[which(!d_drop_k$text %in% d_drop_k_ter$text), ]
  #
  # rownames(d_drop_dif) <- 1:nrow(d_drop_dif)
  # rownames(d_drop_dif2) <- 1:nrow(d_drop_dif2)
  #
  #
  # # d_drop_k_ter2 <- d_ter[which(d_ter$standard_mes > 0.39), ]
  # #
  # # d_drop_dif3 <- d_drop_k_ter2[which(!d_drop_k_ter2$text %in% d_drop_k$text), ]
  # # d_drop_dif4 <- d_drop_k[which(!d_drop_k$text %in% d_drop_k_ter2$text), ]
  #
  #
  # dont_drop <- c(271,3,6,7,14,18,23,25,44,54,68,74,76,83,95,98,108,110,114,115,123,127,140,141,147,151,154,
  #                185,200,205,208,218,223,229,234,235,249,256:261,263,300,302,306,307,311,315,355,364,390,
  #                399,403,405,414,418,442,483,497,503:508,511:513,523)
  #
  #
  # #d_drop_dif <- d_drop_dif[-dont_drop, ]
  # d_drop_dif2 <- d_drop_dif2[-c(1210,12,24,29,30,34,44,67,114,146,171,290)]
  #
  # d_drop_k_ter <- d_drop_k_ter[which(!d_drop_k_ter$text %in% d_drop_dif$text[dont_drop]), ]
  # d_drop_k_ter <- merge(d_drop_k_ter, d_drop_dif2, all = TRUE)
  #
  # save(d_drop_k_ter, file = "drop.Rdata")
  #
  #
  # # LATAA TÄSSÄ TIPUTETTAVAT TEKSTIT
  # load("drop.Rdata")
  #
  #
  #
  # d_ter <- d_ter[which(!d_ter$text %in% d_drop_k_ter$text), ]
  #
  #
  #
  # # Remove messages sent after the last treatment session
  # d_ter <- d_ter[!((d_ter$treatment == "dep" & d_ter$CreatedAfterSession > 6)|(d_ter$treatment == "gad" & d_ter$CreatedAfterSession > 11)), ]
  # d_pot <- d_pot[!((d_pot$treatment == "dep" & d_pot$CreatedAfterSession > 6)|(d_pot$treatment == "gad" & d_pot$CreatedAfterSession > 11)), ]
  #
  # save(d_pot, file = "d_pot.Rdata")
  # save(d_ter, file = "d_ter.Rdata")
  # # Viimeisimmästä versiosta poistettu numerot ja nimet
  # # Update: filtteröity pois viimeisen session jälkeen lähetetyt viestit
  # # Jos nimet pitää poistaa uudestaan, uploadaa nimet2.txt tiedostoista!!!
  #
  # load("d_pot.Rdata")
  # load("d_ter.Rdata")
  #
  #
  # ###############################################################################################
  # # RISTIINVALIDOINTI
  # # File path and wd
  # fp <- "~/shared_with_jupyter/projects_sm/messages/"
  # setwd(fp)
  #
  # # library(textmineR)
  # library(glmnet)
  # library(dplyr)
  # library(foreach)
  # library(doSNOW)
  #
  #
  # # Load datas
  # load("d_pot.Rdata")
  # load("d_ter.Rdata")
  # load("d_s_m5.Rdata")
  #
  #
  #
  # ################################################################################################################################
  # # OUTCOME VARIABLES                                                                                                            #
  # #                                                                                                                              #
  # # drop_out: is patient a drop-out or not?                                                                                      #
  # # Factor                                                                                                                       #
  # # 1: dropped out before last treatment session; 0: finished all treatment sessions                                             #
  # #                                                                                                                              #
  # # drop_out_non_resp: is patient a non-responsive drop-out or not?                                                              #
  # # Factor                                                                                                                       #
  # # 1: dropped out AND symptoms declined less than 50 %; 0: did not drop out OR dropped out but symptoms declined 50 % or more   #
  # #                                                                                                                              #
  # # drop_out_resp: is patient a responsive drop_out?                                                                             #
  # #                                                                                                                              #
  # # symptom_change: how much did symptoms change between first session and last treatment session                                #
  # # i.e. symptoms ses 1 - symptoms last ses                                                                                      #
  # # Finishers only: last ses session 7 for dep and session 12 for gad                                                            #
  # #                                                                                                                              #
  # # symptoms_last: Symptom score observed at the last finished session                                                           #
  # #                                                                                                                              #
  # ################################################################################################################################
  #
  #
  # set.seed(123)
  #
  # # Create cross validation folds for dep and gad patients separately
  # d_s_m_d <- d_s_m[d_s_m$treatment == "dep", ]
  # d_s_m_g <- d_s_m[d_s_m$treatment == "gad", ]
  #
  # nfolds <- 10
  # fold_list <- 1:nfolds
  #
  # fold_id <- rep(1:10,ceiling(nrow(d_s_m_d)/10))
  # fold_id <- fold_id[sample.int(nrow(d_s_m_d))]
  # d_s_m_d$fold_id <- fold_id
  #
  # fold_id <- rep(1:10,ceiling(nrow(d_s_m_g)/10))
  # fold_id <- fold_id[sample.int(nrow(d_s_m_g))]
  # d_s_m_g$fold_id <- fold_id
  #
  # # Check
  # d_s_m_d %>%
  #   group_by(fold_id) %>%
  #   summarize(n = n())
  #
  # # Recombine d_s_m
  # d_s_m <- merge(d_s_m_d, d_s_m_g, all = TRUE)
  # rm(list = c("d_s_m_d", "d_s_m_g"))
  #
  #
  # # Add fold id's to text data
  # d_pot <- merge(d_pot, d_s_m[c(1,14)], by = "UID", all.x = TRUE, all.y = FALSE)
  # d_ter <- merge(d_ter, d_s_m[c(1,14)], by = "UID", all.x = TRUE, all.y = FALSE)
  #
  # # Examine fold sizes
  # # Note, these are now messages per fold, not patients
  # d_pot %>%
  #   group_by(fold_id) %>%
  #   summarize(n = n())
  #
  # d_ter %>%
  #   group_by(fold_id) %>%
  #   summarize(n = n())
  #
  # # Add running number to identify documents
  # d_pot$doc_num <- seq.int(nrow(d_pot))
  # d_ter$doc_num <- seq.int(nrow(d_ter))
  #
  #
  # # Create vector for patient topic numbers
  # k_nums <- rep(seq(2, 50, by = 1))
  #
  #
  # # Continuous variable names
  # conts <- c("num_sessions2", "symptoms_ses1", "symptoms_last", "symptom_change", "messages_pot", "messages_ter")
  #
  # # Vector for column names to remove when building predictor matrix
  # drop_2 <- c("UID", "treatment", "drop_out", "symptoms_last", "drop_out_non_resp", "drop_out_resp", "symptom_change",
  #             "messages_ter", "resp_ind", "fold_id", "messages_pot_ter")
  # drop_1 <- c(drop_2, "num_sessions2")
  # drop_3 <- c(drop_2, "symptoms_ses1")
  #
  #
  # # Vectors for values of alpha and lambda
  # alphas <- c(0, 0.25, 0.5, 0.75, 1)
  # lambdas <- exp(seq(-5,2,length.out = 80))
  #
  #
  # #####################################################################################################################################
  #
  # # Run preliminary elastic net models without topic predictors AJA NÄMÄ UUDESTAAN!!!!!!!!!!!!!!!!!!!
  # # Include other covariates for each outcome
  #
  # # Create list for results
  # base_res_list_preds2 <- vector(mode = "list", 10)
  #
  # # Loop over partitions
  # for (i in 1:length(fold_list)) {
  #   # Partition data according to fold id
  #   d_train <- d_s_m[d_s_m$fold_id %in% fold_list[-i], ]
  #   d_test <- d_s_m[d_s_m$fold_id %in% fold_list[i], ]
  #
  #   # Split datas by treatment
  #   d_train_d <- d_train[d_train$treatment == "dep", ]
  #   d_train_g <- d_train[d_train$treatment == "gad", ]
  #   d_test_d <- d_test[d_test$treatment == "dep", ]
  #   d_test_g <- d_test[d_test$treatment == "gad", ]
  #
  #   # Count means and center continuous variables
  #   cont_means_d <- colMeans(d_train[, conts])
  #   d_train_d[, conts] <- scale(d_train_d[, conts], center = cont_means_d, scale = FALSE)
  #   d_test_d[, conts] <- scale(d_test_d[, conts], center = cont_means_d, scale = FALSE)
  #
  #   cont_means_g <- colMeans(d_train_g[, conts])
  #   d_train_g[, conts] <- scale(d_train_g[, conts], center = cont_means_g, scale = FALSE)
  #   d_test_g[, conts] <- scale(d_test_g[, conts], center = cont_means_g, scale = FALSE)
  #
  #   # Create y vectors and x matrices for each model
  #   # 1: Drop-out as an outcome (all patients)
  #   y1_d <- d_train_d$drop_out
  #   y1_g <- d_train_g$drop_out
  #
  #   x1_d <- model.matrix( ~ .-1, d_train_d[, !names(d_train_d) %in% drop_1])
  #   x1_g <- model.matrix( ~ .-1, d_train_g[, !names(d_train_g) %in% drop_1])
  #
  #   y1_d_t <- d_test_d$drop_out
  #   y1_g_t <- d_test_g$drop_out
  #
  #   x1_d_t <- model.matrix( ~ .-1, d_test_d[, !names(d_test_d) %in% drop_1])
  #   x1_g_t <- model.matrix( ~ .-1, d_test_g[, !names(d_test_g) %in% drop_1])
  #
  #   # 2: Drop-out with non-response as an outcome (those patients who reached at least two symptom measurements)
  #   y2_d <- d_train_d$drop_out_non_resp[d_train_d$resp_ind == 1]
  #   y2_g <- d_train_g$drop_out_non_resp[d_train_g$resp_ind == 1]
  #
  #   x2_d <- model.matrix( ~ .-1, d_train_d[d_train_d$resp_ind == 1, !names(d_train_d) %in% drop_1])
  #   x2_g <- model.matrix( ~ .-1, d_train_g[d_train_g$resp_ind == 1, !names(d_train_g) %in% drop_1])
  #
  #   y2_d_t <- d_test_d$drop_out_non_resp[d_test_d$resp_ind == 1]
  #   y2_g_t <- d_test_g$drop_out_non_resp[d_test_g$resp_ind == 1]
  #
  #   x2_d_t <- model.matrix( ~ .-1, d_test_d[d_test_d$resp_ind == 1, !names(d_test_d) %in% drop_1])
  #   x2_g_t <- model.matrix( ~ .-1, d_test_g[d_test_g$resp_ind == 1, !names(d_test_g) %in% drop_1])
  #
  #   # 3: Drop-out with response as an outcome (those patients who reached at least two symptom measurements)
  #   # Use x2 as predictors!
  #   y3_d <- d_train_d$drop_out_resp[d_train_d$resp_ind == 1]
  #   y3_g <- d_train_g$drop_out_resp[d_train_g$resp_ind == 1]
  #
  #   y3_d_t <- d_test_d$drop_out_resp[d_test_d$resp_ind == 1]
  #   y3_g_t <- d_test_g$drop_out_resp[d_test_g$resp_ind == 1]
  #
  #   # 4: Symptoms in last session (LOCF, all patients, controlled for number of sessions and baseline depression)
  #   y4_d <- d_train_d$symptoms_last[d_train_d$resp_ind == 1]
  #   y4_g <- d_train_g$symptoms_last[d_train_g$resp_ind == 1]
  #
  #   x4_d <- model.matrix( ~ .-1, d_train_d[d_train_d$resp_ind == 1, !names(d_train_d) %in% drop_2])
  #   x4_g <- model.matrix( ~ .-1, d_train_g[d_train_g$resp_ind == 1, !names(d_train_g) %in% drop_2])
  #
  #   y4_d_t <- d_test_d$symptoms_last[d_test_d$resp_ind == 1]
  #   y4_g_t <- d_test_g$symptoms_last[d_test_g$resp_ind == 1]
  #
  #   x4_d_t <- model.matrix( ~ .-1, d_test_d[d_test_d$resp_ind == 1, !names(d_test_d) %in% drop_2])
  #   x4_g_t <- model.matrix( ~ .-1, d_test_g[d_test_g$resp_ind == 1, !names(d_test_g) %in% drop_2])
  #
  #   # 5: Symptom change as outcome (LOCF, all patients, controlled for number of sessions)
  #   y5_d <- d_train_d$symptom_change[d_train_d$resp_ind == 1]
  #   y5_g <- d_train_g$symptom_change[d_train_g$resp_ind == 1]
  #
  #   x5_d <- model.matrix( ~ .-1, d_train_d[d_train_d$resp_ind == 1, !names(d_train_d) %in% drop_3])
  #   x5_g <- model.matrix( ~ .-1, d_train_g[d_train_g$resp_ind == 1, !names(d_train_g) %in% drop_3])
  #
  #   y5_d_t <- d_test_d$symptom_change[d_test_d$resp_ind == 1]
  #   y5_g_t <- d_test_g$symptom_change[d_test_g$resp_ind == 1]
  #
  #   x5_d_t <- model.matrix( ~ .-1, d_test_d[d_test_d$resp_ind == 1, !names(d_test_d) %in% drop_3])
  #   x5_g_t <- model.matrix( ~ .-1, d_test_g[d_test_g$resp_ind == 1, !names(d_test_g) %in% drop_3])
  #
  #   # List for gathering results
  #   res <- vector(mode = "list", 5)
  #
  #   set.seed(123)
  #
  #   # Fit elastic net regressions
  #   for (n in 1:length(alphas)) {
  #     # 1: Drop-out
  #     fit1_d <- cv.glmnet(x1_d, y1_d, family = "binomial", alpha = alphas[n], lambda = lambdas)
  #     pred1_d <- predict(fit1_d, x1_d_t, type = "response")
  #     diff1_d <- rep(NA, length(y1_d_t))
  #     for (j in 1:length(y1_d_t)) {
  #       diff1_d[j] <- pred1_d[j] - as.numeric(as.character(y1_d_t[j]))
  #     }
  #     as1_d <- assess.glmnet(fit1_d, x1_d_t, y1_d_t)
  #     fit1_g <- cv.glmnet(x1_g, y1_g, family = "binomial", alpha = alphas[n], lambda = lambdas)
  #     pred1_g <- predict(fit1_g, x1_g_t, type = "response")
  #     diff1_g <- rep(NA, length(y1_g_t))
  #     for (j in 1:length(y1_g_t)) {
  #       diff1_g[j] <- pred1_g[j] - as.numeric(as.character(y1_g_t[j]))
  #     }
  #     as1_g <- assess.glmnet(fit1_g, x1_g_t, y1_g_t)
  #
  #     # 2: Drop-out with non-response
  #     fit2_d <- cv.glmnet(x2_d, y2_d, family = "binomial", alpha = alphas[n], lambda = lambdas)
  #     as2_d <- assess.glmnet(fit2_d, x2_d_t, y2_d_t)
  #     pred2_d <- predict(fit2_d, x2_d_t, type = "response")
  #     diff2_d <- rep(NA, length(y2_d_t))
  #     for (j in 1:length(y2_d_t)) {
  #       diff2_d[j] <- pred2_d[j] - as.numeric(as.character(y2_d_t[j]))
  #     }
  #     fit2_g <- cv.glmnet(x2_g, y2_g, family = "binomial", alpha = alphas[n], lambda = lambdas)
  #     pred2_g <- predict(fit2_g, x2_g_t, type = "response")
  #     diff2_g <- rep(NA, length(y2_g_t))
  #     for (j in 1:length(y2_g_t)) {
  #       diff2_g[j] <- pred2_g[j] - as.numeric(as.character(y2_g_t[j]))
  #     }
  #     as2_g <- assess.glmnet(fit2_g, x2_g_t, y2_g_t)
  #
  #     # 3: Drop-out with response
  #     fit3_d <- cv.glmnet(x2_d, y3_d, family = "binomial", alpha = alphas[n], lambda = lambdas)
  #     pred3_d <- predict(fit3_d, x2_d_t, type = "response")
  #     diff3_d <- rep(NA, length(y3_d_t))
  #     for (j in 1:length(y3_d_t)) {
  #       diff3_d[j] <- pred3_d[j] - as.numeric(as.character(y3_d_t[j]))
  #     }
  #     as3_d <- assess.glmnet(fit3_d, x2_d_t, y3_d_t)
  #     fit3_g <- cv.glmnet(x2_g, y3_g, family = "binomial", alpha = alphas[n], lambda = lambdas)
  #     pred3_g <- predict(fit3_g, x2_g_t, type = "response")
  #     diff3_g <- rep(NA, length(y3_g_t))
  #     for (j in 1:length(y3_g_t)) {
  #       diff3_g[j] <- pred3_g[j] - as.numeric(as.character(y3_g_t[j]))
  #     }
  #     as3_g <- assess.glmnet(fit3_g, x2_g_t, y3_g_t)
  #
  #     # 4: Last session symptoms (LOCF)
  #     fit4_d <- cv.glmnet(x4_d, y4_d, family = "gaussian", alpha = alphas[n], lambda = lambdas)
  #     pred4_d <- predict(fit4_d, x4_d_t)
  #     diff4_d <- rep(NA, length(y4_d_t))
  #     for (j in 1:length(y4_d_t)) {
  #       diff4_d[j] <- pred4_d[j] - as.numeric(as.character(y4_d_t[j]))
  #     }
  #     as4_d <- assess.glmnet(fit4_d, x4_d_t, y4_d_t)
  #     fit4_g <- cv.glmnet(x4_g, y4_g, family = "gaussian", alpha = alphas[n], lambda = lambdas)
  #     pred4_g <- predict(fit4_g, x4_g_t)
  #     diff4_g <- rep(NA, length(y4_g_t))
  #     for (j in 1:length(y4_g_t)) {
  #       diff4_g[j] <- pred4_g[j] - as.numeric(as.character(y4_g_t[j]))
  #     }
  #     as4_g <- assess.glmnet(fit4_g, x4_g_t, y4_g_t)
  #
  #     # 5: Symptom change
  #     fit5_d <- cv.glmnet(x5_d, y5_d, family = "gaussian", alpha = alphas[n], lambda = lambdas)
  #     pred5_d <- predict(fit5_d, x5_d_t)
  #     diff5_d <- rep(NA, length(y5_d_t))
  #     for (j in 1:length(y5_d_t)) {
  #       diff5_d[j] <- pred5_d[j] - as.numeric(as.character(y5_d_t[j]))
  #     }
  #     as5_d <- assess.glmnet(fit5_d, x5_d_t, y5_d_t)
  #     fit5_g <- cv.glmnet(x5_g, y5_g, family = "gaussian", alpha = alphas[n], lambda = lambdas)
  #     pred5_g <- predict(fit5_g, x5_g_t)
  #     diff5_g <- rep(NA, length(y5_g_t))
  #     for (j in 1:length(y5_g_t)) {
  #       diff5_g[j] <- pred5_g[j] - as.numeric(as.character(y5_g_t[j]))
  #     }
  #     as5_g <- assess.glmnet(fit5_g, x5_g_t, y5_g_t)
  #
  #     res[[n]] <- list(fit1_d, fit2_d, fit3_d, fit4_d, fit5_d, fit1_g, fit2_g, fit3_g, fit4_g, fit5_g,
  #                      pred1_d, pred1_g, pred2_d, pred2_g, pred3_d, pred3_g, pred4_d, pred4_g, pred5_d, pred5_g,
  #                      diff1_d, diff1_g, diff2_d, diff2_g, diff3_d, diff3_g, diff4_d, diff4_g, diff5_d, diff5_g,
  #                      as1_d, as2_d, as3_d, as4_d, as5_d, as1_g, as2_g, as3_g, as4_g, as5_g)
  #   }
  #
  #   # Gather results
  #   base_res_list_preds2[[i]] <- res
  # }
  #
  #
  # save(base_res_list, file = "base_res.Rdata")
  # save(base_res_list2, file = "base_res_therapist_messages.Rdata") # This included the number of therapist messages
  #
  # save(base_res_list_preds, file = "base_res_preds.Rdata")
  # save(base_res_list_preds2, file = "base_res_preds2.Rdata")
  #
  #
  # #####################################################################################################################################
  #
  #
  # ########################################
  # # CROSS VALIDATE PATIENT TOPIC NUMBERS #
  # ########################################
  #
  # t0 <- proc.time()
  # for(i in 1:length(k_nums)) {
  #   k <- k_nums[i]
  #
  #   cl <- makeCluster(5, outfile="")
  #   registerDoSNOW(cl)
  #
  #   res <- foreach(j = 1:length(fold_list), .packages = c("textmineR", "glmnet", "dplyr"), .combine = "list", .multicombine = TRUE,
  #                  .init = list(list(), list()), .verbose = TRUE) %dopar% {
  #
  #                    # Partition text data according to fold id
  #                    t_pot_train <- d_pot[d_pot$fold_id %in% fold_list[-j], ]
  #                    t_pot_test <- d_pot[d_pot$fold_id %in% fold_list[j], ]
  #
  #                    # Create document-term matrices
  #                    dtm_p_t <- CreateDtm(doc_vec = t_pot_train$textTokens,
  #                                         doc_names = t_pot_train$doc_num)
  #
  #                    dtm_p_v <- CreateDtm(doc_vec = t_pot_test$textTokens,
  #                                         doc_names = t_pot_test$doc_num)
  #
  #                    # Fit LDA model
  #                    lda_p <- FitLdaModel(dtm_p_t, k = k, iterations = 1000, optimize_alpha = TRUE)
  #
  #                    # Add thetas to data frame
  #                    theta_p <- as.data.frame(lda_p$theta)
  #                    colnames(theta_p) <- paste("p", colnames(theta_p), sep = "_")
  #                    theta_p <- cbind(doc_num = rownames(theta_p), theta_p)
  #                    t_pot_train <- merge(t_pot_train, theta_p)
  #
  #                    # Count mean thetas
  #                    p_nam <- rep(NA, k)
  #                    for (t in 1:k) {
  #                      p_nam[t] <- paste0("p_t_", t)
  #                    }
  #
  #                    p_mean_t <- t_pot_train %>%
  #                      group_by(UID) %>%
  #                      summarise_at(vars(all_of(p_nam)), mean)
  #
  #                    # Predict thetas in test data
  #                    pred_theta_p <- predict(lda_p, dtm_p_v, iterations = 1000)
  #                    colnames(pred_theta_p) <- paste("p", colnames(pred_theta_p), sep = "_")
  #                    pred_theta_p <- cbind(doc_num = rownames(pred_theta_p), pred_theta_p)
  #                    t_pot_test <- merge(t_pot_test, pred_theta_p)
  #                    t_pot_test[, p_nam] <- lapply(p_nam, function(x) as.numeric(as.character(t_pot_test[[x]])))
  #
  #                    # Count mean thetas
  #                    p_mean_v <- t_pot_test %>%
  #                      group_by(UID) %>%
  #                      summarise_at(vars(all_of(p_nam)), mean)
  #
  #                    #####
  #                    # Data wrangling for elastic nets
  #                    ##
  #
  #                    # Partition according to fold id
  #                    d_train <- d_s_m[d_s_m$fold_id %in% fold_list[-j], ]
  #                    d_test <- d_s_m[d_s_m$fold_id %in% fold_list[j], ]
  #
  #
  #                    # Add mean thetas
  #                    d_train <- merge(d_train, p_mean_t, by = "UID", all.x = TRUE)
  #                    d_train[is.na(d_train)] <- 0
  #
  #                    d_test <- merge(d_test, p_mean_v, by = "UID", all.x = TRUE)
  #                    d_test[is.na(d_test)] <- 0
  #
  #
  #                    # Split datas by treatment
  #                    d_train_d <- d_train[d_train$treatment == "dep", ]
  #                    d_train_g <- d_train[d_train$treatment == "gad", ]
  #                    d_test_d <- d_test[d_test$treatment == "dep", ]
  #                    d_test_g <- d_test[d_test$treatment == "gad", ]
  #
  #
  #                    # Center topic variables and continuous covariates
  #                    topic_means_d <- colMeans(d_train_d[names(d_train_d) %in% p_nam])
  #                    d_train_d[, p_nam] <- scale(d_train_d[, p_nam], center = topic_means_d, scale = FALSE)
  #                    d_test_d[, p_nam] <- scale(d_test_d[, p_nam], center = topic_means_d, scale = FALSE)
  #
  #                    cont_means_d <- colMeans(d_train_d[, conts])
  #                    d_train_d[, conts] <- scale(d_train_d[, conts], center = cont_means_d, scale = FALSE)
  #                    d_test_d[, conts] <- scale(d_test_d[, conts], center = cont_means_d, scale = FALSE)
  #
  #                    topic_means_g <- colMeans(d_train_g[names(d_train_g) %in% p_nam])
  #                    d_train_g[, p_nam] <- scale(d_train_g[, p_nam], center = topic_means_g, scale = FALSE)
  #                    d_test_g[, p_nam] <- scale(d_test_g[, p_nam], center = topic_means_g, scale = FALSE)
  #
  #                    cont_means_g <- colMeans(d_train_g[, conts])
  #                    d_train_g[, conts] <- scale(d_train_g[, conts], center = cont_means_g, scale = FALSE)
  #                    d_test_g[, conts] <- scale(d_test_g[, conts], center = cont_means_g, scale = FALSE)
  #
  #
  #                    # Create y vectors and x matrices for each model
  #                    # 1: Drop-out as an outcome (all patients)
  #                    y1_d <- d_train_d$drop_out
  #                    y1_g <- d_train_g$drop_out
  #
  #                    x1_d <- model.matrix( ~ .-1, d_train_d[, !names(d_train_d) %in% drop_1])
  #                    x1_g <- model.matrix( ~ .-1, d_train_g[, !names(d_train_g) %in% drop_1])
  #
  #                    y1_d_t <- d_test_d$drop_out
  #                    y1_g_t <- d_test_g$drop_out
  #
  #                    x1_d_t <- model.matrix( ~ .-1, d_test_d[, !names(d_test_d) %in% drop_1])
  #                    x1_g_t <- model.matrix( ~ .-1, d_test_g[, !names(d_test_g) %in% drop_1])
  #
  #                    # 2: Drop-out with non-response as an outcome (those patients who reached at least two symptom measurements)
  #                    y2_d <- d_train_d$drop_out_non_resp[d_train_d$resp_ind == 1]
  #                    y2_g <- d_train_g$drop_out_non_resp[d_train_g$resp_ind == 1]
  #
  #                    x2_d <- model.matrix( ~ .-1, d_train_d[d_train_d$resp_ind == 1, !names(d_train_d) %in% drop_1])
  #                    x2_g <- model.matrix( ~ .-1, d_train_g[d_train_g$resp_ind == 1, !names(d_train_g) %in% drop_1])
  #
  #                    y2_d_t <- d_test_d$drop_out_non_resp[d_test_d$resp_ind == 1]
  #                    y2_g_t <- d_test_g$drop_out_non_resp[d_test_g$resp_ind == 1]
  #
  #                    x2_d_t <- model.matrix( ~ .-1, d_test_d[d_test_d$resp_ind == 1, !names(d_test_d) %in% drop_1])
  #                    x2_g_t <- model.matrix( ~ .-1, d_test_g[d_test_g$resp_ind == 1, !names(d_test_g) %in% drop_1])
  #
  #                    # 3: Drop-out with response as an outcome (those patients who reached at least two symptom measurements)
  #                    # Use x2 as predictors!
  #                    y3_d <- d_train_d$drop_out_resp[d_train_d$resp_ind == 1]
  #                    y3_g <- d_train_g$drop_out_resp[d_train_g$resp_ind == 1]
  #
  #                    y3_d_t <- d_test_d$drop_out_resp[d_test_d$resp_ind == 1]
  #                    y3_g_t <- d_test_g$drop_out_resp[d_test_g$resp_ind == 1]
  #
  #                    # 4: Symptoms in last session (LOCF, all patients, controlled for number of sessions and baseline depression)
  #                    y4_d <- d_train_d$symptoms_last[d_train_d$resp_ind == 1]
  #                    y4_g <- d_train_g$symptoms_last[d_train_g$resp_ind == 1]
  #
  #                    x4_d <- model.matrix( ~ .-1, d_train_d[d_train_d$resp_ind == 1, !names(d_train_d) %in% drop_2])
  #                    x4_g <- model.matrix( ~ .-1, d_train_g[d_train_g$resp_ind == 1, !names(d_train_g) %in% drop_2])
  #
  #                    y4_d_t <- d_test_d$symptoms_last[d_test_d$resp_ind == 1]
  #                    y4_g_t <- d_test_g$symptoms_last[d_test_g$resp_ind == 1]
  #
  #                    x4_d_t <- model.matrix( ~ .-1, d_test_d[d_test_d$resp_ind == 1, !names(d_test_d) %in% drop_2])
  #                    x4_g_t <- model.matrix( ~ .-1, d_test_g[d_test_g$resp_ind == 1, !names(d_test_g) %in% drop_2])
  #
  #                    # 5: Symptom change as outcome (LOCF, all patients, controlled for number of sessions)
  #                    y5_d <- d_train_d$symptom_change[d_train_d$resp_ind == 1]
  #                    y5_g <- d_train_g$symptom_change[d_train_g$resp_ind == 1]
  #
  #                    x5_d <- model.matrix( ~ .-1, d_train_d[d_train_d$resp_ind == 1, !names(d_train_d) %in% drop_3])
  #                    x5_g <- model.matrix( ~ .-1, d_train_g[d_train_g$resp_ind == 1, !names(d_train_g) %in% drop_3])
  #
  #                    y5_d_t <- d_test_d$symptom_change[d_test_d$resp_ind == 1]
  #                    y5_g_t <- d_test_g$symptom_change[d_test_g$resp_ind == 1]
  #
  #                    x5_d_t <- model.matrix( ~ .-1, d_test_d[d_test_d$resp_ind == 1, !names(d_test_d) %in% drop_3])
  #                    x5_g_t <- model.matrix( ~ .-1, d_test_g[d_test_g$resp_ind == 1, !names(d_test_g) %in% drop_3])
  #
  #
  #                    # List for gathering results
  #                    elastic_res <- vector(mode = "list", 5)
  #
  #                    set.seed(123)
  #
  #                    ###
  #                    # Fit and assess elastic net regressions
  #                    ##
  #
  #                    for (n in 1:length(alphas)) {
  #                      # 1: Drop-out
  #                      fit1_d <- cv.glmnet(x1_d, y1_d, family = "binomial", alpha = alphas[n], lambda = lambdas)
  #                      pred1_d <- predict(fit1_d, x1_d_t, type = "response")
  #                      diff1_d <- rep(NA, length(y1_d_t))
  #                      for (h in 1:length(y1_d_t)) {
  #                        diff1_d[h] <- pred1_d[h] - as.numeric(as.character(y1_d_t[h]))
  #                      }
  #                      as1_d <- assess.glmnet(fit1_d, x1_d_t, y1_d_t)
  #                      fit1_g <- cv.glmnet(x1_g, y1_g, family = "binomial", alpha = alphas[n], lambda = lambdas)
  #                      pred1_g <- predict(fit1_g, x1_g_t, type = "response")
  #                      diff1_g <- rep(NA, length(y1_g_t))
  #                      for (h in 1:length(y1_g_t)) {
  #                        diff1_g[h] <- pred1_g[h] - as.numeric(as.character(y1_g_t[h]))
  #                      }
  #                      as1_g <- assess.glmnet(fit1_g, x1_g_t, y1_g_t)
  #
  #                      # 2: Drop-out with non-response
  #                      fit2_d <- cv.glmnet(x2_d, y2_d, family = "binomial", alpha = alphas[n], lambda = lambdas)
  #                      pred2_d <- predict(fit2_d, x2_d_t, type = "response")
  #                      diff2_d <- rep(NA, length(y2_d_t))
  #                      for (h in 1:length(y2_d_t)) {
  #                        diff2_d[h] <- pred2_d[h] - as.numeric(as.character(y2_d_t[h]))
  #                      }
  #                      as2_d <- assess.glmnet(fit2_d, x2_d_t, y2_d_t)
  #                      fit2_g <- cv.glmnet(x2_g, y2_g, family = "binomial", alpha = alphas[n], lambda = lambdas)
  #                      pred2_g <- predict(fit2_g, x2_g_t, type = "response")
  #                      diff2_g <- rep(NA, length(y2_g_t))
  #                      for (h in 1:length(y2_g_t)) {
  #                        diff2_g[h] <- pred2_g[h] - as.numeric(as.character(y2_g_t[h]))
  #                      }
  #                      as2_g <- assess.glmnet(fit2_g, x2_g_t, y2_g_t)
  #
  #                      # 3: Drop-out with response
  #                      fit3_d <- cv.glmnet(x2_d, y3_d, family = "binomial", alpha = alphas[n], lambda = lambdas)
  #                      pred3_d <- predict(fit3_d, x2_d_t, type = "response")
  #                      diff3_d <- rep(NA, length(y3_d_t))
  #                      for (h in 1:length(y3_d_t)) {
  #                        diff3_d[h] <- pred3_d[h] - as.numeric(as.character(y3_d_t[h]))
  #                      }
  #                      as3_d <- assess.glmnet(fit3_d, x2_d_t, y3_d_t)
  #                      fit3_g <- cv.glmnet(x2_g, y3_g, family = "binomial", alpha = alphas[n], lambda = lambdas)
  #                      pred3_g <- predict(fit3_g, x2_g_t, type = "response")
  #                      diff3_g <- rep(NA, length(y3_g_t))
  #                      for (h in 1:length(y3_g_t)) {
  #                        diff3_g[h] <- pred3_g[h] - as.numeric(as.character(y3_g_t[h]))
  #                      }
  #                      as3_g <- assess.glmnet(fit3_g, x2_g_t, y3_g_t)
  #
  #                      # 4: Last session symptoms (LOCF)
  #                      fit4_d <- cv.glmnet(x4_d, y4_d, family = "gaussian", alpha = alphas[n], lambda = lambdas)
  #                      pred4_d <- predict(fit4_d, x4_d_t)
  #                      diff4_d <- rep(NA, length(y4_d_t))
  #                      for (h in 1:length(y4_d_t)) {
  #                        diff4_d[h] <- pred4_d[h] - as.numeric(as.character(y4_d_t[h]))
  #                      }
  #                      as4_d <- assess.glmnet(fit4_d, x4_d_t, y4_d_t)
  #                      fit4_g <- cv.glmnet(x4_g, y4_g, family = "gaussian", alpha = alphas[n], lambda = lambdas)
  #                      pred4_g <- predict(fit4_g, x4_g_t)
  #                      diff4_g <- rep(NA, length(y4_g_t))
  #                      for (h in 1:length(y4_g_t)) {
  #                        diff4_g[h] <- pred4_g[h] - as.numeric(as.character(y4_g_t[h]))
  #                      }
  #                      as4_g <- assess.glmnet(fit4_g, x4_g_t, y4_g_t)
  #
  #                      # 5: Symptom change
  #                      fit5_d <- cv.glmnet(x5_d, y5_d, family = "gaussian", alpha = alphas[n], lambda = lambdas)
  #                      pred5_d <- predict(fit5_d, x5_d_t)
  #                      diff5_d <- rep(NA, length(y5_d_t))
  #                      for (h in 1:length(y5_d_t)) {
  #                        diff5_d[h] <- pred5_d[h] - as.numeric(as.character(y5_d_t[h]))
  #                      }
  #                      as5_d <- assess.glmnet(fit5_d, x5_d_t, y5_d_t)
  #                      fit5_g <- cv.glmnet(x5_g, y5_g, family = "gaussian", alpha = alphas[n], lambda = lambdas)
  #                      pred5_g <- predict(fit5_g, x5_g_t)
  #                      diff5_g <- rep(NA, length(y5_g_t))
  #                      for (h in 1:length(y5_g_t)) {
  #                        diff5_g[h] <- pred5_g[h] - as.numeric(as.character(y5_g_t[h]))
  #                      }
  #                      as5_g <- assess.glmnet(fit5_g, x5_g_t, y5_g_t)
  #
  #                      elastic_res[[n]] <- list(fit1_d, fit2_d, fit3_d, fit4_d, fit5_d, fit1_g, fit2_g, fit3_g, fit4_g, fit5_g,
  #                                               pred1_d, pred1_g, pred2_d, pred2_g, pred3_d, pred3_g, pred4_d, pred4_g, pred5_d, pred5_g,
  #                                               diff1_d, diff1_g, diff2_d, diff2_g, diff3_d, diff3_g, diff4_d, diff4_g, diff5_d, diff5_g,
  #                                               as1_d, as2_d, as3_d, as4_d, as5_d, as1_g, as2_g, as3_g, as4_g, as5_g)
  #                    }
  #                    elastic_res
  #                  }
  #
  #   resNam <- paste0("res_p_", k)
  #   save(res, file = paste0(resNam, ".Rdata"))
  #   stopCluster(cl)
  # }
  #
  # (runtime <- proc.time() - t0)
  #
  #
  # ################################################################
  # # TULOKSET
  #
  # # File path and wd
  # fp <- "~/shared_with_jupyter/projects_sm/messages/"
  # setwd(fp)
  #
  # library(textmineR)
  # library(glmnet)
  # library(dplyr)
  # # library(foreach)
  # # library(doSNOW)
  #
  #
  # # Load datas
  # load("d_pot.Rdata")
  # load("d_ter.Rdata")
  # load("d_s_m5.Rdata")
  #
  #
  #
  # #########################################################################
  #
  # # Data preparation
  #
  # set.seed(123)
  #
  # # Create cross validation folds for dep and gad patients separately
  # d_s_m_d <- d_s_m[d_s_m$treatment == "dep", ]
  # d_s_m_g <- d_s_m[d_s_m$treatment == "gad", ]
  #
  # nfolds <- 10
  # fold_list <- 1:nfolds
  #
  # fold_id <- rep(1:10,ceiling(nrow(d_s_m_d)/10))
  # fold_id <- fold_id[sample.int(nrow(d_s_m_d))]
  # d_s_m_d$fold_id <- fold_id
  #
  # fold_id <- rep(1:10,ceiling(nrow(d_s_m_g)/10))
  # fold_id <- fold_id[sample.int(nrow(d_s_m_g))]
  # d_s_m_g$fold_id <- fold_id
  #
  # # Check
  # d_s_m_d %>%
  #   group_by(fold_id) %>%
  #   summarize(n = n())
  #
  # # Recombine d_s_m
  # d_s_m <- merge(d_s_m_d, d_s_m_g, all = TRUE)
  # rm(list = c("d_s_m_d", "d_s_m_g"))
  #
  #
  # # Add fold id's to text data
  # d_pot <- merge(d_pot, d_s_m[c(1,14)], by = "UID", all.x = TRUE, all.y = FALSE)
  # d_ter <- merge(d_ter, d_s_m[c(1,14)], by = "UID", all.x = TRUE, all.y = FALSE)
  #
  # # Examine fold sizes
  # # Note, these are now messages per fold, not patients
  # d_pot %>%
  #   group_by(fold_id) %>%
  #   summarize(n = n())
  #
  # d_ter %>%
  #   group_by(fold_id) %>%
  #   summarize(n = n())
  #
  # # Add running number to identify documents
  # d_pot$doc_num <- seq.int(nrow(d_pot))
  # d_ter$doc_num <- seq.int(nrow(d_ter))
  #
  #
  #
  #
  # ################################################################################
  #
  # # Calculations for deviances etc.
  #
  # # Function for deviance calculations
  # devfun <- function(a, b) {
  #   p1 <- a^b
  #   p2 <- (1-a)^(1-b)
  #   p3 <- p1*p2
  #   p4 <- (-2)*(log(p3))
  #   return(p4)
  # }
  #
  #
  # # Load base result data
  # load("base_res_preds3.Rdata")
  # base_res_list_preds <- base_res_list_preds3
  # rm(base_res_list_preds3)
  # d_s_m <- cbind(index = rownames(d_s_m), d_s_m)
  #
  #
  #
  # # Preparations
  # df_nam <- c("dev_drop_out_d", "dev_drop_out_g", "dev_non_resp_d", "dev_non_resp_g", "dev_resp_d",
  #             "dev_resp_g", "mse_sympt_last_d", "mse_sympt_last_g", "mse_sympt_change_d", "mse_sympt_change_g")
  # list_nam <- c("drop_out_d_dev_list", "drop_out_g_dev_list", "non_resp_d_dev_list", "non_resp_g_dev_list",
  #               "resp_d_dev_list", "resp_g_dev_list", "slast_d_mse_list", "slast_g_mse_list",
  #               "schange_d_mse_list", "schange_g_mse_list")
  # dif_nam <- c("dev_diff_drop_out_d", "dev_diff_drop_out_g", "dev_diff_non_resp_d", "dev_diff_non_resp_g",
  #              "dev_diff_resp_d", "dev_diff_resp_g", "mse_diff_sympt_last_d", "mse_diff_sympt_last_g",
  #              "mse_diff_sympt_change_d", "mse_diff_sympt_change_g")
  # # r2_nam <- c("boot_r2_drop_out_d", "boot_r2_drop_out_g", "boot_r2_non_resp_d", "boot_r2_non_resp_g", "boot_r2_resp_d",
  # #             "boot_r2_resp_g", "boot_r2_sympt_last_d", "boot_r2_sympt_last_g", "boot_r2_sympt_change_d", "boot_r2_sympt_change_g")
  # alist <- c("0", "0.25", "0.5", "0.75", "1")
  # r1 <- seq.int(from = 11, to = 16)
  # r2 <- seq.int(from = 21, to = 30)
  # ind <- matrix(2:21, ncol = 5, nrow = 4)
  #
  #
  # # df_nam <- c("dev_drop_out_d", "dev_drop_out_g", "dev_non_resp_d", "dev_non_resp_g", "dev_resp_d",
  # #            "dev_resp_g", "mse_sympt_last_d", "mse_sympt_last_g")
  # # list_nam <- c("drop_out_d_dev_list", "drop_out_g_dev_list", "non_resp_d_dev_list", "non_resp_g_dev_list",
  # #               "resp_d_dev_list", "resp_g_dev_list", "slast_d_mse_list", "slast_g_mse_list")
  # # dif_nam <- c("dev_diff_drop_out_d", "dev_diff_drop_out_g", "dev_diff_non_resp_d", "dev_diff_non_resp_g",
  # #              "dev_diff_resp_d", "dev_diff_resp_g", "mse_diff_sympt_last_d", "mse_diff_sympt_last_g")
  #
  #
  #
  #
  # # Baseline deviances
  # for (i in 1:6) {
  #   d_list <- vector(mode='list', length=5)
  #   df <- as.data.frame(matrix(ncol = 21, nrow = 50))
  #   colnames(df)[1] <- "k"
  #   df$k <- seq(1:50)
  #   df[1,1] <- 0
  #   colnames(df)[2:5] <- c("dev_a0.00", "low_a0.00", "upp_a0.00", "se_a0.00")
  #   colnames(df)[6:9] <- c("dev_a0.25", "low_a0.25", "upp_a0.25", "se_a0.25")
  #   colnames(df)[10:13] <- c("dev_a0.50", "low_a0.50", "upp_a0.50", "se_a0.50")
  #   colnames(df)[14:17] <- c("dev_a0.75", "low_a0.75", "upp_a0.75", "se_a0.75")
  #   colnames(df)[18:21] <- c("dev_a1.00", "low_a1.00", "upp_a1.00", "se_a1.00")
  #   for (a in 1:5) {
  #     devs <- as.data.frame(base_res_list_preds[[1]][[a]][[r1[i]]])
  #     devs <- cbind(index = rownames(devs), devs)
  #     rcount <- nrow(devs)
  #     devs$index2 <- seq.int(from = 1, to = rcount)
  #     devs$index2 <- as.numeric(devs$index2)
  #     devs$diff <- base_res_list_preds[[1]][[a]][[r2[i]]]
  #     devs$fold_id <- rep(1)
  #     for(j in 2:10) {
  #       devs2 <- as.data.frame(base_res_list_preds[[j]][[a]][[r1[i]]])
  #       devs2 <- cbind(index = rownames(devs2), devs2)
  #       rcount2 <- nrow(devs2)
  #       devs2$index2 <- seq.int(from = (rcount+1), to = (rcount + rcount2))
  #       devs2$index2 <- as.numeric(devs2$index2)
  #       rcount <- rcount + rcount2
  #       devs2$diff <- base_res_list_preds[[j]][[a]][[r2[i]]]
  #       devs2$fold_id <- rep(j)
  #       devs <- merge(devs, devs2, by = c("index", "index2", "lambda.1se", "fold_id", "diff"), all = TRUE)
  #     }
  #     devs$drop_out <- case_when(devs$lambda.1se == devs$diff ~ 0,
  #                                TRUE ~ 1)
  #
  #     devs$dev <- mapply(devfun, devs$lambda.1se, devs$drop_out)
  #     dev_mean <- mean(devs$dev)
  #     df[1,ind[1,a]] <- dev_mean
  #
  #     devs$dev_sep <- sapply(devs$dev, function(x) (x - dev_mean)^2)
  #     dev_var <- (1/(length(devs$dev_sep)-1))*sum(devs$dev_sep)
  #
  #     df[1,ind[2,a]] <- dev_mean - 1.96*(sqrt(dev_var/length(devs$dev_sep)))
  #     df[1,ind[3,a]] <- dev_mean + 1.96*(sqrt(dev_var/length(devs$dev_sep)))
  #     df[1,ind[4,a]] <- sqrt(dev_var/length(devs$dev_sep))
  #
  #     names(devs)[names(devs) == "dev"] <- "base_dev"
  #     names(devs)[names(devs) == "dev_sep"] <- "base_dev_sep"
  #     d_list[[a]] <- devs
  #   }
  #   assign(df_nam[i], df)
  #   assign(list_nam[i], d_list)
  # }
  #
  #
  # # Baseline mse
  # for (i in 7:10) {
  #   d_list <- vector(mode='list', length=5)
  #   df <- as.data.frame(matrix(ncol = 21, nrow = 50))
  #   colnames(df)[1] <- "k"
  #   df$k <- seq(1:50)
  #   df[1,1] <- 0
  #   colnames(df)[2:5] <- c("mse_a0.00", "low_a0.00", "upp_a0.00", "se_a0.00")
  #   colnames(df)[6:9] <- c("mse_a0.25", "low_a0.25", "upp_a0.25", "se_a0.25")
  #   colnames(df)[10:13] <- c("mse_a0.50", "low_a0.50", "upp_a0.50", "se_a0.50")
  #   colnames(df)[14:17] <- c("mse_a0.75", "low_a0.75", "upp_a0.75", "se_a0.75")
  #   colnames(df)[18:21] <- c("mse_a1.00", "low_a1.00", "upp_a1.00", "se_a1.00")
  #   for (a in 1:5) {
  #     msef <- as.data.frame(base_res_list_preds[[1]][[a]][[r2[i]]])
  #     rcount <- nrow(msef)
  #     msef$index2 <- seq.int(from = 1, to = rcount)
  #     msef$index2 <- as.numeric(msef$index2)
  #     colnames(msef)[1] <- "ydiff"
  #     msef$fold_id <- rep(1)
  #     for(j in 2:10) {
  #       ms <- as.data.frame(base_res_list_preds[[j]][[a]][[r2[i]]])
  #       colnames(ms)[1] <- "ydiff"
  #       rcount2 <- nrow(ms)
  #       ms$index2 <- seq.int(from = (rcount+1), to = (rcount + rcount2))
  #       ms$index2 <- as.numeric(ms$index2)
  #       rcount <- rcount + rcount2
  #       ms$fold_id <- rep(j)
  #       msef <- merge(msef, ms, all = TRUE)
  #     }
  #     msef$ydiff <- (msef$ydiff)^2
  #
  #     mse <- mean(msef$ydiff)
  #     df[1,(ind[1,a])] <- mse
  #
  #     msef$mse_sep <- sapply(msef$ydiff, function(x) (x - mse)^2)
  #     mse_var <- (1/(length(msef$mse_sep)-1))*(sum(msef$mse_sep))
  #
  #     df[1,(ind[2,a])] <- mse - 1.96*(sqrt(mse_var/length(msef$mse_sep)))
  #     df[1,(ind[3,a])] <- mse + 1.96*(sqrt(mse_var/length(msef$mse_sep)))
  #     df[1,(ind[4,a])] <- sqrt(mse_var/length(msef$mse_sep))
  #
  #     names(msef)[names(msef) == "ydiff"] <- "base_ydiff"
  #     names(msef)[names(msef) == "mse_sep"] <- "base_mse_sep"
  #     d_list[[a]] <- msef
  #   }
  #   assign(df_nam[i], df)
  #   assign(list_nam[i], d_list)
  # }
  #
  #
  #
  #
  # # Load results with topics
  # for(i in 2:50) {
  #   filename <- paste0("n_res_p_", i, ".Rdata") # Huom, muutettu!
  #   newname <- paste0("res_", i)
  #   load(filename)
  #   assign(newname, res)
  # }
  # rm(res)
  #
  #
  # # Functions for bootstrapping R2 confidence intervals
  # # boot.r2 <- function(x) {
  # #   ind <- sample.int(nrow(x), size = nrow(x), replace = TRUE)
  # #   x_b <- x[ind, ]
  # #   x_b$dev <- mapply(devfun, x_b$lambda.1se, x_b$drop_out)
  # #   dev_mean <- mean(x_b$dev)
  # #   base_dev <- mean(x_b$base_dev)
  # #   r2 <- (base_dev - dev_mean) / base_dev
  # #   return(r2)
  # # }
  #
  # # boot.r2.2 <- function(x) {
  # #   ind <- sample.int(nrow(x), size = nrow(x), replace = TRUE)
  # #   x_b <- x[ind, ]
  # #   x_b$dev <- mapply(devfun, x_b$lambda.1se, x_b$drop_out)
  # #   dev_mean <- mean(x_b$dev)
  # #   base_dev <- mean(x_b$base_dev)
  # #   r2 <- (base_dev - dev_mean) / base_dev
  # #   return(r2)
  # # }
  #
  # # nboot <- 10000
  #
  # # r1 <- seq.int(from = 9, to = 14)
  # # r2 <- seq.int(from = 17, to = 24)
  #
  #
  # # Deviances, differences and R2
  # # Updated, with Nagelkerke R2
  # dev_list <- list(dev_drop_out_d, dev_drop_out_g, dev_non_resp_d, dev_non_resp_g,
  #                  dev_resp_d, dev_resp_g)
  # names(dev_list) <- df_nam[1:6]
  #
  # for (i in 1:6) {
  #   dif <- as.data.frame(matrix(ncol = 21, nrow = 49))
  #   colnames(dif)[1] <- "k"
  #   dif$k <- seq(2,50)
  #   colnames(dif)[2:5] <- c("dif_a0.00", "low_a0.00", "upp_a0.00", "nr2_a0.00") # HUOM, NIMI MUUTETTU
  #   colnames(dif)[6:9] <- c("dif_a0.25", "low_a0.25", "upp_a0.25", "nr2_a0.25")
  #   colnames(dif)[10:13] <- c("dif_a0.50", "low_a0.50", "upp_a0.50", "nr2_a0.50")
  #   colnames(dif)[14:17] <- c("dif_a0.75", "low_a0.75", "upp_a0.75", "nr2_a0.75")
  #   colnames(dif)[18:21] <- c("dif_a1.00", "low_a1.00", "upp_a1.00", "nr2_a1.00")
  #
  #   for (k in 2:50) {
  #     res <- get(paste0("res_", k))
  #     for (a in 1:5) {
  #       devs <- as.data.frame(res[[2]][[a]][[r1[i]]])
  #       rcount <- nrow(devs)
  #       devs$index2 <- seq.int(from = 1, to = rcount)
  #       devs$index2 <- as.numeric(devs$index2)
  #       devs$diff <- res[[2]][[a]][[r2[i]]]
  #       devs$fold_id <- rep(1)
  #       for(j in 3:11) {
  #         devs2 <- as.data.frame(res[[j]][[a]][[r1[i]]])
  #         rcount2 <- nrow(devs2)
  #         devs2$index2 <- seq.int(from = (rcount+1), to = (rcount + rcount2))
  #         devs2$index2 <- as.numeric(devs2$index2)
  #         rcount <- rcount + rcount2
  #         devs2$diff <- res[[j]][[a]][[r2[i]]]
  #         devs2$fold_id <- rep(j-1)
  #         devs <- merge(devs, devs2, by = c("index2", "lambda.1se", "diff", "fold_id"), all = TRUE)
  #       }
  #       devs$drop_out <- case_when(devs$lambda.1se == devs$diff ~ 0,
  #                                  TRUE ~ 1)
  #       devs$dev <- mapply(devfun, devs$lambda.1se, devs$drop_out)
  #       dev_mean <- mean(devs$dev)
  #       t_ll <- dev_mean/(-2)
  #
  #       dev_list[[i]][k,(ind[1,a])] <- dev_mean
  #
  #       devs$dev_sep <- sapply(devs$dev, function(x) (x - dev_mean)^2)
  #       dev_var <- (1/(length(devs$dev_sep)-1))*sum(devs$dev_sep)
  #
  #       dev_list[[i]][k,(ind[2,a])] <- dev_mean - 1.96*(sqrt(dev_var/length(devs$dev_sep)))
  #       dev_list[[i]][k,(ind[3,a])] <- dev_mean + 1.96*(sqrt(dev_var/length(devs$dev_sep)))
  #       dev_list[[i]][k,(ind[4,a])] <- sqrt(dev_var/length(devs$dev_sep))
  #
  #       lis <- get(list_nam[i])
  #       devs <- merge(devs, lis[[a]][, c(2,4,6,7,8)], by = c("index2", "drop_out", "fold_id"), all = TRUE)
  #
  #       devs$dev_diff <- devs$dev - devs$base_dev
  #       diff_mean <- mean(devs$dev_diff)
  #       dif[k-1,(ind[1,a])] <- diff_mean
  #
  #       base_dev_var <- (1/(length(devs$base_dev_sep)-1))*sum(devs$base_dev_sep)
  #       base_dev <- mean(devs$base_dev)
  #       base_ll <- base_dev/(-2)
  #
  #       dif[k-1,(ind[2,a])] <- diff_mean - 1.96*(sqrt((dev_var+base_dev_var)/length(devs$dev_sep)))
  #       dif[k-1,(ind[3,a])] <- diff_mean + 1.96*(sqrt((dev_var+base_dev_var)/length(devs$dev_sep)))
  #
  #       # R2
  #       r2c <- 1 - exp(2*(base_ll - t_ll)/length(devs$dev_sep))
  #       r2max <- 1 - exp(2*base_ll/length(devs$dev_sep))
  #       dif[k-1,(ind[4,a])] <- r2c / r2max
  #     }
  #   }
  #   assign(dif_nam[i], dif)
  # }
  #
  # list2env(dev_list, envir = .GlobalEnv)
  # rm(dev_list)
  #
  # # Loopin pitäisi nyt toimia, tarkastele vielä
  # # Pura ja poista lista: list2env
  #
  #
  #
  # # MSEs, differences and R2
  #
  # # TARKASTA NÄMÄ KOODIT VIELÄ
  # # Tarvittaessa voi vaikka simuloida dataa ja katsoa, että toimii siinä kuten pitäisi
  #
  # mse_list <- list(mse_sympt_last_d, mse_sympt_last_g, mse_sympt_change_d, mse_sympt_change_g)
  # names(mse_list) <- df_nam[7:10]
  #
  # for (i in 7:10) {
  #   dif <- as.data.frame(matrix(ncol = 21, nrow = 49))
  #   colnames(dif)[1] <- "k"
  #   dif$k <- seq(2,50)
  #   colnames(dif)[2:5] <- c("dif_a0.00", "low_a0.00", "upp_a0.00", "rr2_a0.00") # HUOM, NIMI MUUTETTU
  #   colnames(dif)[6:9] <- c("dif_a0.25", "low_a0.25", "upp_a0.25", "rr2_a0.25")
  #   colnames(dif)[10:13] <- c("dif_a0.50", "low_a0.50", "upp_a0.50", "rr2_a0.50")
  #   colnames(dif)[14:17] <- c("dif_a0.75", "low_a0.75", "upp_a0.75", "rr2_a0.75")
  #   colnames(dif)[18:21] <- c("dif_a1.00", "low_a1.00", "upp_a1.00", "rr2_a1.00")
  #   for (k in 2:50) {
  #     res <- get(paste0("res_", k))
  #     for (a in 1:5) {
  #       res <- get(paste0("res_", k))
  #       msef <- as.data.frame(res[[2]][[a]][[r2[i]]])
  #       colnames(msef)[1] <- "ydiff"
  #       rcount <- nrow(msef)
  #       msef$index2 <- seq.int(from = 1, to = rcount)
  #       msef$index2 <- as.numeric(msef$index2)
  #       msef$fold_id <- rep(1)
  #       for(j in 3:11) {
  #         ms <- as.data.frame(res[[j]][[a]][[r2[i]]])
  #         colnames(ms)[1] <- "ydiff"
  #         rcount2 <- nrow(ms)
  #         ms$index2 <- seq.int(from = (rcount+1), to = (rcount + rcount2))
  #         ms$index2 <- as.numeric(ms$index2)
  #         rcount <- rcount + rcount2
  #         ms$fold_id <- rep(j-1)
  #         msef <- merge(msef, ms, all = TRUE) # Tässä olisi parempi käyttää cbind -> ei tarvi indeksöidä käsin, ja tulee varmasti oikein
  #       }
  #       msef$ydiff <- (msef$ydiff)^2
  #
  #       mse <- mean(msef$ydiff)
  #       mse_list[[i-6]][k,(ind[1,a])] <- mse
  #
  #       msef$mse_sep <- sapply(msef$ydiff, function(x) (x - mse)^2)
  #       mse_var <- (1/(length(msef$mse_sep)-1))*(sum(msef$mse_sep))
  #
  #       mse_list[[i-6]][k,(ind[2,a])] <- mse - 1.96*(sqrt(mse_var/length(msef$mse_sep)))
  #       mse_list[[i-6]][k,(ind[3,a])] <- mse + 1.96*(sqrt(mse_var/length(msef$mse_sep)))
  #       mse_list[[i-6]][k,(ind[4,a])] <- sqrt(mse_var/length(msef$mse_sep))
  #
  #       lis <- get(list_nam[i])
  #       msef <- merge(msef, lis[[a]], by = c("index2", "fold_id"), all = TRUE)
  #
  #       msef$mse_diff <- msef$ydiff - msef$base_ydiff
  #       diff_mean <- mean(msef$mse_diff)
  #       dif[k-1,(ind[1,a])] <- diff_mean
  #
  #       base_mse_var <- (1/(length(msef$base_mse_sep)-1))*sum(msef$base_mse_sep)
  #       base_mse <- mean(msef$base_ydiff)
  #
  #       dif[k-1,(ind[2,a])] <- diff_mean - 1.96*(sqrt((mse_var+base_mse_var)/length(msef$mse_sep)))
  #       dif[k-1,(ind[3,a])] <- diff_mean + 1.96*(sqrt((mse_var+base_mse_var)/length(msef$mse_sep)))
  #       dif[k-1,(ind[4,a])] <- (base_mse-mse)/base_mse
  #     }
  #   }
  #   assign(dif_nam[i], dif)
  # }
  #
  # list2env(mse_list, envir = .GlobalEnv)
  # rm(mse_list)
  #
  #
  #
  # # Inspect drop-outs
  # # What number of topics would be selected based on standard error?
  #
  # # Dep drop out
  # min_dev_d <- min(dev_drop_out_d$dev_a0.50)
  # min_k_d <- dev_drop_out_d$k[dev_drop_out_d$dev_a0.50 == min(dev_drop_out_d$dev_a0.50)]
  # min_se_d <- dev_drop_out_d$se_a0.50[dev_drop_out_d$dev_a0.50 == min(dev_drop_out_d$dev_a0.50)]
  #
  # k_1se_d <- min(dev_drop_out_d$k[(dev_drop_out_d$k < min_k_d)&(dev_drop_out_d$dev_a0.50 <= (min_dev_d + min_se_d))])
  #
  #
  # # GAD drop out
  # min_dev_g <- min(dev_drop_out_g$dev_a0.50)
  # min_k_g <- dev_drop_out_g$k[dev_drop_out_g$dev_a0.50 == min(dev_drop_out_g$dev_a0.50)]
  # min_se_g <- dev_drop_out_g$se_a0.50[dev_drop_out_g$dev_a0.50 == min(dev_drop_out_g$dev_a0.50)]
  #
  # k_1se_g <- min(dev_drop_out_g$k[(dev_drop_out_g$k < min_k_g)&(dev_drop_out_g$dev_a0.50 <= (min_dev_g + min_se_g))])
  #
  #
  # # GAD drop out with non response
  # min_dev_g_n <- min(dev_non_resp_g$dev_a0.50)
  # min_k_g_n <- dev_non_resp_g$k[dev_non_resp_g$dev_a0.50 == min(dev_non_resp_g$dev_a0.50)]
  # min_se_g_n <- dev_non_resp_g$se_a0.50[dev_non_resp_g$dev_a0.50 == min(dev_non_resp_g$dev_a0.50)]
  #
  # k_1se_g_n <- min(dev_non_resp_g$k[(dev_non_resp_g$k < min_k_g_n)&(dev_non_resp_g$dev_a0.50 <= (min_dev_g_n + min_se_g_n))])
  #
  #
  # k_1se_d
  # k_1se_g
  # k_1se_g_n
  #
  #
  #
  # # LDA models with 10, 11 and 12 topics
  #
  # # dtm_p <- CreateDtm(doc_vec = d_pot$textTokens,
  # #                    doc_names = d_pot$doc_num)
  # #
  # # pot_lda_10 <- FitLdaModel(dtm_p, k = 10, iterations = 1000, optimize_alpha = TRUE)
  # # pot_lda_11 <- FitLdaModel(dtm_p, k = 11, iterations = 1000, optimize_alpha = TRUE)
  # # pot_lda_12 <- FitLdaModel(dtm_p, k = 12, iterations = 1000, optimize_alpha = TRUE)
  # #
  # #
  # # topt11 <- GetTopTerms(pot_lda_11$phi, 10)
  # # theta11 <- as.data.frame(pot_lda_11$theta)
  # # theta11 <- cbind(doc_num = rownames(theta11), theta11)
  # # theta11 <- merge(theta11, d_pot[, c(1,2,5,6,11)], by = "doc_num", all = TRUE)
  # #
  # #
  # # save(pot_lda_10, file = "pot_lda_10.Rdata")
  # # save(pot_lda_11, file = "pot_lda_11.Rdata")
  # # save(pot_lda_12, file = "pot_lda_12.Rdata")
  #
  #
  #
  #
  #
  # ### Get AUC for manuscript
  # auc_b_d <- rep(NA, 10)
  # for(i in 1:10){
  #   auc_b_d[i] <- base_res_list_preds[[i]][[3]][[31]]$auc
  # }
  # mean(auc_b_d)
  #
  # auc_12_d <- rep(NA, 10)
  # for(i in 1:10){
  #   auc_12_d[i] <- res_12[[i+1]][[3]][[31]]$auc
  # }
  # mean(auc_12_d)
  #
  #
  # auc_b_g <- rep(NA, 10)
  # for(i in 1:10){
  #   auc_b_g[i] <- base_res_list_preds[[i]][[3]][[36]]$auc
  # }
  # mean(auc_b_g)
  #
  # auc_12_g <- rep(NA, 10)
  # for(i in 1:10){
  #   auc_12_g[i] <- res_12[[i+1]][[3]][[36]]$auc
  # }
  # mean(auc_12_g)
  #
  #
  #
  #
  # ################################################################################
  #
  # # Visualize R2
  #
  # library(tidyr)
  # library(ggplot2)
  # library(gridExtra)
  # library(ggpubr)
  #
  #
  # # Drop out
  # # Depression
  # r2_drop_out_d_l <- boot_r2_drop_out_d %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_drop_out_d <- r2_drop_out_d_l %>% ggplot(aes(x = k, y = med)) +
  #   geom_line(aes(color = alpha_val), show.legend = FALSE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "R2", title = "Depression") +
  #   ylim(-0.01, 0.07) +
  #   theme_classic()
  #
  # plot_drop_out_d
  #
  #
  # # GAD
  # r2_drop_out_g_l <- boot_r2_drop_out_g %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_drop_out_g <- r2_drop_out_g_l %>% ggplot(aes(x = k, y = med)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "R2", title = "GAD") +
  #   ylim(-0.01, 0.07) +
  #   theme_classic()
  #
  # plot_drop_out_g
  #
  #
  # # Drop out with non-response
  # # Depression
  # r2_non_resp_d_l <- boot_r2_non_resp_d %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_non_resp_d <- r2_non_resp_d_l %>% ggplot(aes(x = k, y = med)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "R2", title = "Depression") +
  #   ylim(-0.01, 0.07) +
  #   theme_classic()
  #
  # plot_non_resp_d
  #
  #
  # # GAD
  # r2_non_resp_g_l <- boot_r2_non_resp_g %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_non_resp_g <- r2_non_resp_g_l %>% ggplot(aes(x = k, y = med)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "R2", title = "Depression") +
  #   ylim(-0.01, 0.07) +
  #   theme_classic()
  #
  # plot_non_resp_g
  #
  #
  # # Drop out with response
  # # Depression
  # r2_resp_d_l <- boot_r2_resp_d %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_resp_d <- r2_resp_d_l %>% ggplot(aes(x = k, y = med)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "R2", title = "Depression") +
  #   ylim(-0.01, 0.07) +
  #   theme_classic()
  #
  # plot_resp_d
  #
  #
  # # GAD
  # r2_resp_g_l <- boot_r2_resp_g %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_resp_g <- r2_resp_g_l %>% ggplot(aes(x = k, y = med)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "R2", title = "Depression") +
  #   ylim(-0.01, 0.07) +
  #   theme_classic()
  #
  # plot_resp_g
  #
  #
  # plot_drop_out_d
  # plot_drop_out_g
  # plot_non_resp_d
  # plot_non_resp_g
  # plot_resp_d
  # plot_resp_d
  #
  #
  #
  #
  # ####
  # # Visualize deviances and standard errors
  #
  # # Drop out
  # # Depression
  # dev_se_drop_out_d_l <- dev_drop_out_d %>%
  #   select(-starts_with(c("low_", "upp_"))) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(..)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_se_drop_out_d <- dev_se_drop_out_d_l %>%
  #   filter(alpha_val == "0.50" & k > 1) %>%
  #   ggplot(aes(x = k, y = ev)) +
  #   geom_line() +
  #   geom_errorbar(aes(ymin = ev-se, ymax = ev+se), size = 0.2) +
  #   #geom_abline(aes(intercept = dev_drop_out_d$dev_a0.50[1], slope = 0), size = 0.2) +
  #   geom_hline(yintercept = min_dev_d+min_se_d, size = 0.2) +
  #   labs(x = "Number of topics", y = "Deviance", title = "Depression") +
  #   theme_classic()
  #
  # plot_se_drop_out_d
  #
  #
  # # GAD
  # dev_se_drop_out_g_l <- dev_drop_out_g %>%
  #   select(-starts_with(c("low_", "upp_"))) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(..)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_se_drop_out_g <- dev_se_drop_out_g_l %>%
  #   filter(alpha_val == "0.50" & k > 1) %>%
  #   ggplot(aes(x = k, y = ev)) +
  #   geom_line() +
  #   geom_errorbar(aes(ymin = ev-se, ymax = ev+se), size = 0.2) +
  #   geom_hline(yintercept = min_dev_g + min_se_g, size = 0.2) +
  #   labs(x = "Number of topics", y = "Deviance", title = "GAD") +
  #   theme_classic()
  #
  # plot_se_drop_out_g
  #
  #
  # # Combination plot for article
  # dev_se_drop_out_d_l$treatment <- rep("Depression")
  # dev_se_drop_out_g_l$treatment <- rep("GAD")
  # dev_se_drop_out_comb <- merge(dev_se_drop_out_d_l, dev_se_drop_out_g_l, all = TRUE)
  #
  # line_specs <- data.frame(treatment = c("Depression", "GAD"), line = c((min_dev_d + min_se_d), (min_dev_g + min_se_g)))
  # xticks <- seq(5, 50, by=5)
  #
  # plot_se_drop_out_comb_free <- dev_se_drop_out_comb %>%
  #   filter(alpha_val == "0.50" & k > 1) %>%
  #   ggplot(aes(x = k, y = ev)) +
  #   geom_line(aes(linetype = treatment)) +
  #   geom_errorbar(aes(ymin = ev-se, ymax = ev+se), size = 0.2) +
  #   scale_x_continuous(breaks = xticks) +
  #   labs(x = "Number of topics", y = "Deviance") +
  #   theme_classic() +
  #   facet_wrap(vars(treatment), nrow = 2, scales = "free") +
  #   theme(strip.background = element_blank(), strip.text.x = element_blank(),
  #         legend.position = c(0.8, 0.95), legend.title= element_blank(), legend.background = element_rect(fill = "#ffffffaa", colour = NA)) +
  #   geom_hline(data = line_specs, aes(yintercept = line), linewidth = 0.2)
  #
  #
  # # theme(legend.position = c(0.9, 0.6)
  # #       ,legend.background = element_rect(fill = "white", colour = NA))
  #
  #
  # # plot_se_drop_out_comb_fixed <- dev_se_drop_out_comb %>%
  # #   filter(alpha_val == "0.50" & k > 1) %>%
  # #   ggplot(aes(x = k, y = ev)) +
  # #   geom_line() +
  # #   geom_errorbar(aes(ymin = ev-se, ymax = ev+se), size = 0.2) +
  # #   scale_x_continuous(breaks = xticks) +
  # #   labs(x = "Number of topics", y = "Deviance") +
  # #   theme_classic() +
  # #   facet_wrap(vars(treatment), scales = "fixed") +
  # #   geom_hline(data = line_specs, aes(yintercept = line), size = 0.2)
  #
  # plot_se_drop_out_comb_free
  # #plot_se_drop_out_comb_fixed
  #
  # # Save plot
  # ggsave("fig1_2_free_2.png", plot = plot_se_drop_out_comb_free, device = "png", width = 100, height = 180, units = "mm", dpi = 300)
  # # ggsave("fig1_2_fixed.png", plot = plot_se_drop_out_comb_fixed, device = "png", width = 200, height = 100, units = "mm", dpi = 300)
  #
  #
  #
  #
  #
  # # Drop out with non-response
  # # Depression
  # dev_se_non_resp_d_l <- dev_non_resp_d %>%
  #   select(-starts_with(c("low_", "upp_"))) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(..)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_se_non_resp_d <- dev_se_non_resp_d_l %>%
  #   filter(alpha_val == "0.50" & k > 1) %>%
  #   ggplot(aes(x = k, y = ev)) +
  #   geom_line() +
  #   geom_errorbar(aes(ymin = ev-se, ymax = ev+se)) +
  #   geom_abline(aes(intercept = dev_non_resp_d$dev_a0.50[1], slope = 0), size = 0.2) +
  #   labs(x = "Number of topics", y = "Deviance", title = "Depression") +
  #   theme_classic()
  #
  # plot_se_non_resp_d
  #
  #
  # # GAD
  # dev_se_non_resp_g_l <- dev_non_resp_g %>%
  #   select(-starts_with(c("low_", "upp_"))) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(..)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_se_non_resp_g <- dev_se_non_resp_g_l %>%
  #   filter(alpha_val == "0.50" & k > 1) %>%
  #   ggplot(aes(x = k, y = ev)) +
  #   geom_line() +
  #   scale_x_continuous(breaks = xticks) +
  #   geom_errorbar(aes(ymin = ev-se, ymax = ev+se), size = 0.2) +
  #   geom_hline(yintercept = min_dev_g_n + min_se_g_n, size = 0.2) +
  #   labs(x = "Number of topics", y = "Deviance", title = element_blank()) +
  #   theme_classic()
  #
  # plot_se_non_resp_g
  #
  # ggsave("fig1_supp.png", plot = plot_se_non_resp_g, device = "png", width = 200, height = 100, units = "mm", dpi = 300)
  #
  #
  #
  # plot_se_drop_out_d
  # plot_se_drop_out_g
  # plot_se_non_resp_d
  # plot_se_non_resp_g
  #
  #
  #
  #
  # ####
  # # Plot prediction error differences for supplement
  #
  # ##
  # # Drop out
  # # Depression
  # dev_diff_drop_out_d_l <- dev_diff_drop_out_d %>%
  #   #select(-starts_with("r2_")) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_diff_drop_out_d <- dev_diff_drop_out_d_l %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "Difference in deviances", title = "Depression") +
  #   theme_classic()
  # #theme(legend.position = "none")
  #
  # plot_diff_drop_out_d
  #
  # plot_r2_drop_out_d <- dev_diff_drop_out_d_l %>%
  #   filter(alpha_val == "0.50" & k > 1) %>%
  #   ggplot(aes(x = k, y = nr2)) +
  #   geom_line(aes(), show.legend = TRUE) +
  #   labs(x = "Number of topics", y = "Pseudo-R squared") +
  #   theme_classic()
  #
  # plot_r2_drop_out_d
  #
  #
  # # GAD
  # dev_diff_drop_out_g_l <- dev_diff_drop_out_g %>%
  #   #select(-starts_with("r2_")) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_diff_drop_out_g <- dev_diff_drop_out_g_l %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "Difference in deviances", title = "GAD") +
  #   theme_classic()
  # #theme(legend.position = "none")
  #
  # plot_diff_drop_out_g
  #
  # plot_r2_drop_out_g <- dev_diff_drop_out_g_l %>%
  #   filter(alpha_val == "0.50" & k > 1) %>%
  #   ggplot(aes(x = k, y = nr2)) +
  #   geom_line(aes(), show.legend = TRUE) +
  #   labs(x = "Number of topics", y = "Pseudo-R squared") +
  #   theme_classic()
  #
  # plot_r2_drop_out_g
  #
  #
  # # Combination plot
  # dev_diff_drop_out_d_l$treatment <- rep("Depression")
  # dev_diff_drop_out_g_l$treatment <- rep("GAD")
  # dev_diff_drop_out_comb <- merge(dev_diff_drop_out_d_l, dev_diff_drop_out_g_l, all = TRUE)
  #
  # xticks <- seq(5, 50, by=5)
  #
  # plot_diff_drop_out_comb <- dev_diff_drop_out_comb %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   scale_x_continuous(breaks = xticks) +
  #   labs(x = "Number of topics", y = "Difference in deviances", title = "Drop-out", color = "Alpha") +
  #   theme_classic() +
  #   facet_grid(cols = vars(treatment))
  #
  # plot_diff_drop_out_comb
  #
  # # R2 combination plot
  # plot_r2_drop_out_comb <- dev_diff_drop_out_comb %>%
  #   filter(alpha_val == "0.50" & k > 1) %>%
  #   ggplot(aes(x = k, y = nr2)) +
  #   geom_line(aes(linetype = treatment), show.legend = TRUE) +
  #   #scale_linetype_manual(values=c("solid", "longdash"))+
  #   labs(x = "Number of topics", y = "Partial pseudo R squared") +
  #   scale_x_continuous(breaks = xticks) +
  #   geom_vline(xintercept = 12, size = 0.2) +
  #   theme_classic() +
  #   theme(legend.title = element_blank(), legend.position = c(0.8, 0.15), plot.margin = margin(24, 5.5, 5.5, 5.5, "pt"))
  #
  # plot_r2_drop_out_comb
  #
  # # Combine R2 plot with deviance-difference plots
  # #plot_drop <- grid.arrange(plot_se_drop_out_comb_free, plot_r2_drop_out_comb, nrow = 2)
  # plot_drop <- ggarrange(plot_se_drop_out_comb_free, plot_r2_drop_out_comb, nrow = 2, heights = c(2,1), labels = c("A", "B"), vjust = 1.2)
  # plot_drop
  #
  # ggsave("fig1_3_1_2.png", plot = plot_drop, device = "png", width = 100, height = 270, units = "mm", dpi = 300)
  #
  #
  #
  # ##
  # # Drop out with non response
  # # Depression
  # dev_diff_non_resp_d_l <- dev_diff_non_resp_d %>%
  #   select(-starts_with("r2_")) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_diff_non_resp_d <- dev_diff_non_resp_d_l %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "Difference in deviances", title = "Depression") +
  #   theme_classic()
  #
  # plot_diff_non_resp_d
  #
  #
  # # GAD
  # dev_diff_non_resp_g_l <- dev_diff_non_resp_g %>%
  #   select(-starts_with("r2_")) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_diff_non_resp_g <- dev_diff_non_resp_g_l %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "Difference in deviances", title = "Depression") +
  #   theme_classic()
  #
  # plot_diff_non_resp_g
  #
  # # Combination plot
  # dev_diff_non_resp_d_l$treatment <- rep("Depression")
  # dev_diff_non_resp_g_l$treatment <- rep("GAD")
  # dev_diff_non_resp_comb <- merge(dev_diff_non_resp_d_l, dev_diff_non_resp_g_l, all = TRUE)
  #
  # plot_diff_non_resp_comb <- dev_diff_non_resp_comb %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   scale_x_continuous(breaks = xticks) +
  #   labs(x = "Number of topics", y = "Difference in deviances", title = "Drop-out with non-response", color = "Alpha") +
  #   theme_classic() +
  #   facet_grid(cols = vars(treatment))
  #
  # plot_diff_non_resp_comb
  #
  #
  # ##
  # # Drop out with response
  # # Depression
  # dev_diff_resp_d_l <- dev_diff_resp_d %>%
  #   select(-starts_with("r2_")) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_diff_resp_d <- dev_diff_resp_d_l %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "Difference in deviances", title = "Depression") +
  #   theme_classic()
  #
  # plot_diff_resp_d
  #
  #
  # # GAD
  # dev_diff_resp_g_l <- dev_diff_resp_g %>%
  #   select(-starts_with("r2_")) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_diff_resp_g <- dev_diff_resp_g_l %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "Difference in deviances", title = "Depression") +
  #   theme_classic()
  #
  # plot_diff_resp_g
  #
  # # Combination plot
  # dev_diff_resp_d_l$treatment <- rep("Depression")
  # dev_diff_resp_g_l$treatment <- rep("GAD")
  # dev_diff_resp_comb <- merge(dev_diff_resp_d_l, dev_diff_resp_g_l, all = TRUE)
  #
  # plot_diff_resp_comb <- dev_diff_resp_comb %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   scale_x_continuous(breaks = xticks) +
  #   labs(x = "Number of topics", y = "Difference in deviances", title = "Drop-out with response", color = "Alpha") +
  #   theme_classic() +
  #   facet_grid(cols = vars(treatment))
  #
  # plot_diff_resp_comb
  #
  #
  # ##
  # # Symptoms in last session
  # # Depression
  # mse_diff_sympt_last_d_l <- mse_diff_sympt_last_d %>%
  #   select(-starts_with("r2_")) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_diff_sympt_last_d <- mse_diff_sympt_last_d_l %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "Difference in mean squared errors", title = "Depression") +
  #   theme_classic()
  #
  # plot_diff_sympt_last_d
  #
  #
  # # GAD
  # mse_diff_sympt_last_g_l <- mse_diff_sympt_last_g %>%
  #   select(-starts_with("r2_")) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_diff_sympt_last_g <- mse_diff_sympt_last_g_l %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "Difference in mean squared errors", title = "GAD") +
  #   theme_classic()
  #
  # plot_diff_sympt_last_g
  #
  #
  # # Combination plot
  # mse_diff_sympt_last_d_l$treatment <- rep("Depression")
  # mse_diff_sympt_last_g_l$treatment <- rep("GAD")
  # mse_diff_sympt_last_comb <- merge(mse_diff_sympt_last_g_l, mse_diff_sympt_last_d_l, all = TRUE)
  #
  # plot_diff_sympt_last_comb <- mse_diff_sympt_last_comb %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   scale_x_continuous(breaks = xticks) +
  #   labs(x = "Number of topics", y = "Difference in mean squared errors", title = "Symptom change", color = "Alpha") +
  #   theme_classic() +
  #   facet_grid(cols = vars(treatment))
  #
  # plot_diff_sympt_last_comb
  #
  #
  # ##
  # # Symptom change
  # # Depression
  # mse_diff_sympt_change_d_l <- mse_diff_sympt_change_d %>%
  #   select(-starts_with("r2_")) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_diff_sympt_change_d <- mse_diff_sympt_change_d_l %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "Difference in mean squared errors", title = "Depression") +
  #   theme_classic()
  #
  # plot_diff_sympt_change_d
  #
  #
  # # GAD
  # mse_diff_sympt_change_g_l <- mse_diff_sympt_change_g %>%
  #   select(-starts_with("r2_")) %>%
  #   gather(key, value, -k) %>%
  #   extract(key, c("measure", "alpha_val"), "(...)_a(.\\...)") %>%
  #   spread(measure, value)
  #
  # plot_diff_sympt_change_g <- mse_diff_sympt_change_g_l %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   labs(x = "Number of topics", y = "Difference in mean squared errors", title = "GAD") +
  #   theme_classic()
  #
  # plot_diff_sympt_change_g
  #
  #
  # # Combination plot
  # mse_diff_sympt_change_d_l$treatment <- rep("Depression")
  # mse_diff_sympt_change_g_l$treatment <- rep("GAD")
  # mse_diff_sympt_change_comb <- merge(mse_diff_sympt_change_g_l, mse_diff_sympt_change_d_l, all = TRUE)
  #
  # plot_diff_sympt_change_comb <- mse_diff_sympt_change_comb %>% ggplot(aes(x = k, y = dif)) +
  #   geom_line(aes(color = alpha_val), show.legend = TRUE) +
  #   geom_ribbon(aes(ymin = low, ymax = upp, color = alpha_val), linetype = "dotted", alpha=0) +
  #   scale_color_manual(values = c("azure4", "darkblue", "darkgreen", "darkmagenta", "darkred")) +
  #   geom_hline(yintercept = 0, size = 0.2) +
  #   scale_x_continuous(breaks = xticks) +
  #   labs(x = "Number of topics", y = "Difference in mean squared errors", title = "Symptom change", color = "Alpha") +
  #   theme_classic() +
  #   facet_grid(cols = vars(treatment))
  #
  # plot_diff_sympt_change_comb
  #
  #
  #
  # # Save supplement plots
  # ggsave("s_fig2_a.png", plot = plot_diff_drop_out_comb, device = "png", width = 200, height = 100, units = "mm", dpi = 300)
  # ggsave("s_fig2_b.png", plot = plot_diff_sympt_last_comb, device = "png", width = 200, height = 100, units = "mm", dpi = 300)
  #
  # ggsave("s_fig3_a.png", plot = plot_diff_non_resp_comb, device = "png", width = 200, height = 100, units = "mm", dpi = 300)
  # ggsave("s_fig3_b.png", plot = plot_diff_resp_comb, device = "png", width = 200, height = 100, units = "mm", dpi = 300)
  # ggsave("s_fig3_c.png", plot = plot_diff_sympt_change_comb, device = "png", width = 200, height = 100, units = "mm", dpi = 300)
  #

}
