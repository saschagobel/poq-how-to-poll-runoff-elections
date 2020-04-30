# ---------------------------------------------------------------------------------------
# HOW TO POLL RUNOFF ELECTIONS
# Peter Selb, Sascha Goebel, and Romain Lachat
# Script for validation of rejective sampling
# April 2020
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
  './data/official-electoral-data-processed/fpe2012_r1'
  './data/official-electoral-data-processed/fpe2012_r1_ov'
  './data/official-electoral-data-processed/fpe2017_r1_ov'
  './data/official-electoral-data-processed/fpe2017_r1'
  './data/official-electoral-data-processed/fpe2012_2017_r1'
  './data/official-electoral-data-processed/fpe2017_2012_r1'
  './data/sample-s20-k200-m20'
  './data/resp_data'
  './data/official-electoral-data/fpe2017_r2.xlsx'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
  './data/simulations_k'
  './data/validation_sample'
  './data/all_sample_metrics'
  './data/best_sample_metrics'
  './data/validation_sample_large'
  './data/emp_ph'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
Line 40 - PREPARATIONS
Line 77 - DATA PROCESSING
Line 117 - VALIDATION
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("poq-how-to-poll-runoff-elections")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")

# read data from disk -------------------------------------------------------------------
fpe2012_r1 <- readRDS("./data/official-electoral-data-processed/fpe2012_r1")
fpe2012_r1_ov <- readRDS("./data/official-electoral-data-processed/fpe2012_r1_ov")
fpe2017_r1_ov <- readRDS("./data/official-electoral-data-processed/fpe2017_r1_ov")
fpe2017_r1 <- readRDS("./data/official-electoral-data-processed/fpe2017_r1")
fpe2012_2017_r1 <- readRDS("./data/official-electoral-data-processed/fpe2012_2017_r1")
fpe2017_2012_r1 <- readRDS("./data/official-electoral-data-processed/fpe2017_2012_r1")
sample_s20_k200_m20 <- readRDS("./data/sample-s20-k200-m20")
resp_data <- readRDS("./data/resp_data")
fpe2017_r2 <- read.xlsx("./data/official-electoral-data/fpe2017_r2.xlsx") %>%
  select(Code.du.département, Libellé.du.département, Code.de.la.commune, 
         Libellé.de.la.commune, Code.du.b.vote, Inscrits, Abstentions, Votants,
         Blancs, Nuls, Exprimés, Nom, Voix, X31, X33) %>%
  set_colnames(c("department", "department_name", "municipality",
                 "municipality_name", "polling_station_number", 
                 "registered_voters", "abstentions", "voters",
                 "blancs", "nuls", "valid_votes", "x1.candidate_name",
                 "x1.votes", "x2.candidate_name", "x2.votes")) %>%
  reshape(direction = 'long', 
          varying = colnames(.)[12:15], 
          timevar = "candidate_number",
          times = c("1", "2"),
          v.names = c("candidate_name", "votes"),
          idvar=colnames(.)[c(1,3,5)])


#### DATA PROCESSING ====================================================================

# compute vote total across polling places and candidates (N) ---------------------------
N_2012_r1_ov <- sum(fpe2012_r1_ov$votes)
N_2017_r1_ov <- sum(fpe2017_r1_ov$votes)

# compute candidates' national vote shares (Vj) -----------------------------------------
Vj_2012_r1_ov <- fpe2012_r1_ov %>% 
  group_by(candidate_number) %>% 
  summarise(sum(votes)) %>% 
  extract2(2) %>% 
  divide_by(N_2012_r1_ov) 
Vj_2017_r1_ov <- fpe2017_r1_ov %>% 
  group_by(as.numeric(candidate_number)) %>% 
  summarise(sum(votes)) %>% 
  extract2(2) %>% 
  divide_by(N_2017_r1_ov)

# compute candidates' national vote totals (Nj) -----------------------------------------
Nj_2012_r1 <- fpe2012_r1 %>% 
  group_by(candidate_number) %>% 
  summarise(sum(votes)) %>% 
  extract2(2)
Nj_2017_2012_r1 <- fpe2017_2012_r1 %>% 
  group_by(as.numeric(candidate_number)) %>% 
  summarise(sum(votes)) %>% 
  extract2(2)

# compute number of voters at polling places (Nh) ---------------------------------------
Nh_2012_r1 <- fpe2012_r1 %>% 
  extract(!duplicated(.$polling_station_unique),TRUE) %>% 
  use_series(voters) %>%
  as.numeric
Nh_2012_2017_r1 <- fpe2012_2017_r1 %>% 
  extract(!duplicated(.$polling_station_unique),TRUE) %>% 
  use_series(voters) %>%
  as.numeric


#### VALIDATION =========================================================================

# register parallel backend -------------------------------------------------------------
registerDoParallel(6)

# validation of choice of number of replications k --------------------------------------
# set sample size
m <- 20
# # set varying number of samples/replications to be drawn
kvar <- seq(0,300, by = 5) %>% replace(1, 1)
# compute first-order inclusion probabilities with PPS of Nh for fix sample size m
ph_2012_r1 <- lapply(m, inclusionprobabilities, a = Nh_2012_r1)
# set number of times the sampling process shall be repeated
sim <- 100
# prepare target to store simulation runs with varying k
simulations_k <- rep(list(NA), times = length(kvar))
set.seed(0300) 
# loop to iterate rejective probability sampling over varying k
for(i in 1:length(kvar)) {
  if (i == 1) {
    cat(yellow("INITIATING SIMULATIONS\n"))
    start_time <- Sys.time()
  }
  k <- kvar[i]
  simulations_k[[i]] <- rejectivePpsSampling(k = k, 
                                             m = m, 
                                             Nh = Nh_2012_r1, 
                                             Nj = Nj_2012_r1,
                                             Vj = Vj_2012_r1_ov, 
                                             ph = unlist(ph_2012_r1), 
                                             sim = sim,
                                             parallelize = TRUE,
                                             data = fpe2012_r1)
  cat(round(i/length(kvar)*100,digit=1),"% done. Execution time: ", 
      round(difftime(Sys.time(), start_time, units = "hours"), digits = 2),
      " hours\n", sep = "")
  if (i == max(length(kvar))) {
    cat(green("SIMULATIONS COMPLETED\n"))
  }
}
saveRDS(simulations_k, "./data/simulations_k")

# validation of rejective sampling strategy against 2017 results ------------------------
# set number of samples to be drawn
k <- 200
# set sample size m
m <- 20
# set number of times the sampling process shall be repeated
sim <- 1000
# compute first-order inclusion probabilities with PPS of Nh for fix 
# sample size m
ph_2012_2017_r1 <- lapply(m, inclusionprobabilities, a = Nh_2012_2017_r1)
set.seed(2003172)
# draw PPS samples from 2012 data with stable polling places in 2017
validation_sample <- rejectivePpsSampling(k = k, 
                                          m = m, 
                                          Nh = Nh_2012_2017_r1, 
                                          Nj = Nj_2012_r1,
                                          Vj = Vj_2012_r1_ov,
                                          ph = unlist(ph_2012_2017_r1), 
                                          sim = 1000, 
                                          parallelize = TRUE,
                                          data = fpe2012_2017_r1)
saveRDS(validation_sample, "./data/validation_sample")
# polling places chosen in the original replicate sampling
sel_sample <- fpe2012_r1[!duplicated(fpe2012_r1$polling_station_unique),]$polling_station_unique %>% 
  extract(unlist(sample_s20_k200_m20[[2]][2]))
# find the position of the polling places chosen in the original 
# replicate sampling within 2012 data with stable polling stations in 2017
sel_sample_2012 <- list(which(fpe2012_2017_r1[!duplicated(fpe2012_2017_r1$polling_station_unique),]$polling_station_unique %in%
                                sel_sample))
best_samples <- validation_sample[[12]] %>% # [[10]] to [[14]] are exactly same
  c(sel_sample_2012)
# compile indices of the validation samples and the sample chosen in the original
# rejective sampling, within 2012 data with stable polling places in 2017
all_samples <- unlist(validation_sample[[19]], recursive = FALSE)
# validate against current 2017 national vote shares
best_sample_metrics <- validation(m = 20,
                                  Vj = Vj_2017_r1_ov, 
                                  Nj = Nj_2017_2012_r1,
                                  ph = ph_2012_2017_r1, 
                                  sample = best_samples, 
                                  data1 = fpe2012_2017_r1, 
                                  data2 = fpe2017_2012_r1)
all_sample_metrics <- validation(m = 20,
                                 Vj = Vj_2017_r1_ov, 
                                 Nj = Nj_2017_2012_r1,
                                 ph = ph_2012_2017_r1, 
                                 sample = all_samples, 
                                 data1 = fpe2012_2017_r1, 
                                 data2 = fpe2017_2012_r1)
saveRDS(all_sample_metrics, "./data/all_sample_metrics")
saveRDS(best_sample_metrics, "./data/best_sample_metrics")

# simulating empirical inclusion probabilities ------------------------------------------
# set number of samples/replications to be drawn
k <- 200
# set sample size m
m <- 20
# set number of times the sampling process shall be repeated
# must be much larger than before
sim <- 25000
# compute first-order inclusion probabilities with PPS of Nh for fix 
# sample size m
ph_2012_2017_r1 <- lapply(m, inclusionprobabilities, a = Nh_2012_2017_r1)
set.seed(2003172)
validation_sample_large <- rejectivePpsSampling(k = k, 
                                                m = m, 
                                                Nh = Nh_2012_2017_r1, 
                                                Nj = Nj_2012_r1,
                                                Vj = Vj_2012_r1_ov,
                                                ph = unlist(ph_2012_2017_r1), 
                                                sim = sim, 
                                                parallelize = TRUE,
                                                data = fpe2012_2017_r1)
saveRDS(validation_sample_large, "./data/validation_sample_large")
best_samples_large <- validation_sample_large[[12]] %>% 
  c(sel_sample_2012)
all_samples_large <- unlist(validation_sample_large[[19]], recursive = FALSE)
# compute inclusion probabilities for 2012_2017 data
data2012_2017 <- fpe2012_2017_r1[!duplicated(fpe2012_2017_r1$polling_station_unique),]
data2012_2017$ph <- unlist(lapply(m, inclusionprobabilities, a = Nh_2012_2017_r1))
incprobs_2012_2017 <- select(data2012_2017, polling_station_unique, ph)
# get correpsonfing polling station numbers
polling_stations_best <- fpe2012_2017_r1[!duplicated(fpe2012_2017_r1$polling_station_unique),][unique(unlist(best_samples_large)),]
polling_stations_best$station_idx <- as.character(unique(unlist(best_samples_large)))
polling_stations_best <- select(polling_stations_best, station_idx, polling_station_unique)
polling_stations_all <- fpe2012_2017_r1[!duplicated(fpe2012_2017_r1$polling_station_unique),][unique(unlist(all_samples_large)),]
polling_stations_all$station_idx <- as.character(unique(unlist(all_samples_large)))
polling_stations_all <- select(polling_stations_all, station_idx, polling_station_unique)
# how often does a station appear in best and in all samples
best_counts <- data.frame(station_idx = as.character(unlist(best_samples_large)), stringsAsFactors = FALSE)
best_counts <- group_by(best_counts, station_idx)
best_counts <- best_counts %>%
  summarize(n = n())
best_counts <- left_join(best_counts, polling_stations_best, by = "station_idx")
all_counts <- data.frame(station_idx = as.character(unlist(all_samples_large)), stringsAsFactors = FALSE)
all_counts <- group_by(all_counts, station_idx)
all_counts <- all_counts %>%
  summarize(n = n())
all_counts <- left_join(all_counts, polling_stations_all, by = "station_idx")
result_best <- left_join(best_counts, incprobs_2012_2017, by = "polling_station_unique")
result_all <- left_join(all_counts, incprobs_2012_2017, by = "polling_station_unique")
# find stations in all_samples_large that were also drawn into best_samples
result_all$group <- 0
result_all[which(result_all$polling_station_unique %in% result_best$polling_station_unique),]$group <- 1
colnames(result_best)[1:2] <- c("station_idx_best", "n_best")
result_all <- left_join(result_all, result_best[, c("station_idx_best", "n_best", "polling_station_unique")],
                        by = "polling_station_unique")
result_all[is.na(result_all$n_best),]$n_best <- 0
result_all$emp_ph <- result_all$n_best/25000
fpe2012_2017_r1_dist <- distinct(fpe2012_2017_r1, polling_station_unique, .keep_all = TRUE)
result_all <- left_join(x = result_all, 
                        y = fpe2012_2017_r1_dist[, c("polling_station_unique", 
                                                     "municipality_name", 
                                                     "polling_station_number")],
                        by = "polling_station_unique")
result_all$selected <- FALSE
result_all[which(result_all$polling_station_unique %in% sel_sample),]$selected <- TRUE
emp_ph <- result_all %>% filter(selected == TRUE)
saveRDS(emp_ph, "./data/emp_ph")

# create data frame with selected sample ------------------------------------------------
m <- 20
data2012 <- fpe2012_r1[!duplicated(fpe2012_r1$polling_station_unique),]
data2012$ph <- unlist(lapply(m, inclusionprobabilities, a = Nh_2012_r1))
data2017 <- fpe2017_r1[which(fpe2017_r1$polling_station_unique %in% sel_sample),]
data2012 <- select(data2012, polling_station_unique, ph)
data2017 <- left_join(data2017, data2012, by = "polling_station_unique")
data2017$local_share <- (data2017$votes/data2017$valid_votes)*100
data2017 <- left_join(x = data2017, 
                      y = emp_ph[,c("emp_ph","polling_station_unique")],
                      by = "polling_station_unique")

# Table 1 and C5 predicted first-round results based on sampled stations ----------------
sample_des <- svydesign(id = ~1, probs = ~ph, data = data2017)
jkdes <- as.svrepdesign(sample_des, type="JK1")
can1 <- svymean(~local_share, subset(jkdes, candidate_number == 1)) # dupont-aignan
can2 <- svymean(~local_share, subset(jkdes, candidate_number == 2)) # le pen
can3 <- svymean(~local_share, subset(jkdes, candidate_number == 3)) # macron
can4 <- svymean(~local_share, subset(jkdes, candidate_number == 4)) # hamon
can5 <- svymean(~local_share, subset(jkdes, candidate_number == 5)) # arthaud
can6 <- svymean(~local_share, subset(jkdes, candidate_number == 6)) # poutou
can7 <- svymean(~local_share, subset(jkdes, candidate_number == 7)) # cheminade
can8 <- svymean(~local_share, subset(jkdes, candidate_number == 8)) # lassalle
can9 <- svymean(~local_share, subset(jkdes, candidate_number == 9)) # mélenchon
can10 <- svymean(~local_share, subset(jkdes, candidate_number == 10)) # asselineau
can11 <- svymean(~local_share, subset(jkdes, candidate_number == 11)) # fillon

# Table 1 and C5 predicted second-round results based on sampled stations ---------------
data2017$polling_station_number <- as.integer(as.character(data2017$polling_station_number))
data2017dist <- distinct(data2017, municipality_name, polling_station_number, .keep_all = TRUE)
fpe2017_r2$municipality_name <- str_replace(fpe2017_r2$municipality_name, " \\(aka.+", "")
fpe2017_r2 <- left_join(fpe2017_r2, data2017dist[,c("municipality_name","polling_station_number", "ph", "emp_ph")], by = c("municipality_name","polling_station_number"))
fpe2017_r2 <- filter(fpe2017_r2, !is.na(ph))
fpe2017_r2$local_share <- (fpe2017_r2$votes/fpe2017_r2$valid_votes)*100
sample_des <- svydesign(id = ~1, probs = ~ph, data = fpe2017_r2)
jkdes <- as.svrepdesign(sample_des, type="JK1")
can1 <- svymean(~local_share, subset(jkdes, candidate_number == 1)) # macron
can2 <- svymean(~local_share, subset(jkdes, candidate_number == 2)) # le pen

# Table 1 and C5 predicted first-round results based on survey responses ----------------
# filter those who have been sampled initially
resp_data <- filter(resp_data, 
                    id == "034Eygalieres" & polling_station_number == "1" |
                      id == "103Salon-de-Provence" & polling_station_number == "2" |
                      id == "353Tregastel" & polling_station_number == "2" |
                      id == "037Avignonet-Lauragais" |
                      id == "116Castelginest" |
                      id == "420Pinsaguel" & polling_station_number == "2" |
                      id == "318Pessac" & polling_station_number == "2" |
                      id == "228Valencay" & polling_station_number == "1" |
                      id == "078Charce-Saint-Ellier-sur-Aubance" |
                      id == "328Ludres" |
                      id == "666Terville" & polling_station_number == "4" |
                      id == "042Bailleval" |
                      id == "486Tinchebray" & polling_station_number == "2" |
                      id == "123Lyon" & polling_station_number == "2" |
                      id == "081Cluses" & polling_station_number == "2" |
                      id == "035Aumale" |
                      id == "137Toulon" & polling_station_number == "1" |
                      id == "063Rueil-Malmaison" & polling_station_number == "3" |
                      id == "048Montreuil" & polling_station_number == "1"|
                      id == "051Noisy-le-Grand") 
# add second stage inclusion probabilities for dataset including only those initially sampled
resp_data$second_stage_2 <- ifelse(resp_data$id == "034Eygalieres", 55/682,
                                   ifelse(resp_data$id == "035Aumale", 55/1355,
                                          ifelse(resp_data$id == "037Avignonet-Lauragais", 55/254,
                                                 ifelse(resp_data$id == "042Bailleval", 55/937, 
                                                        ifelse(resp_data$id == "048Montreuil", 55/595,
                                                               ifelse(resp_data$id == "051Noisy-le-Grand", 55/539,
                                                                      ifelse(resp_data$id == "063Rueil-Malmaison", 55/747,
                                                                             ifelse(resp_data$id == "078Charce-Saint-Ellier-sur-Aubance", 55/494,
                                                                                    ifelse(resp_data$id == "081Cluses", 55/549,
                                                                                           ifelse(resp_data$id == "103Salon-de-Provence", 55/1023,
                                                                                                  ifelse(resp_data$id == "116Castelginest", 55/909,
                                                                                                         ifelse(resp_data$id == "123Lyon", 55/869,
                                                                                                                ifelse(resp_data$id == "137Toulon", 55/403,
                                                                                                                       ifelse(resp_data$id == "228Valencay", 55/972,
                                                                                                                              ifelse(resp_data$id == "318Pessac", 55/857,
                                                                                                                                     ifelse(resp_data$id == "328Ludres", 55/806,
                                                                                                                                            ifelse(resp_data$id == "353Tregastel", 55/743,
                                                                                                                                                   ifelse(resp_data$id == "420Pinsaguel", 55/880,
                                                                                                                                                          ifelse(resp_data$id == "486Tinchebray", 55/982,
                                                                                                                                                                 ifelse(resp_data$id == "666Terville", 55/524, NA
                                                                                                                                                                 ))))))))))))))))))))
# add empirical inclusion probabilities
resp_data$emp_ph <- ifelse(resp_data$id == "034Eygalieres", emp_ph$emp_ph[6],
                           ifelse(resp_data$id == "035Aumale", emp_ph$emp_ph[15],
                                  ifelse(resp_data$id == "037Avignonet-Lauragais", emp_ph$emp_ph[1],
                                         ifelse(resp_data$id == "042Bailleval", emp_ph$emp_ph[11], 
                                                ifelse(resp_data$id == "048Montreuil", emp_ph$emp_ph[18],
                                                       ifelse(resp_data$id == "051Noisy-le-Grand", emp_ph$emp_ph[19],
                                                              ifelse(resp_data$id == "063Rueil-Malmaison", emp_ph$emp_ph[17],
                                                                     ifelse(resp_data$id == "078Charce-Saint-Ellier-sur-Aubance", emp_ph$emp_ph[7],
                                                                            ifelse(resp_data$id == "081Cluses", emp_ph$emp_ph[14],
                                                                                   ifelse(resp_data$id == "103Salon-de-Provence", emp_ph$emp_ph[10],
                                                                                          ifelse(resp_data$id == "116Castelginest", emp_ph$emp_ph[2],
                                                                                                 ifelse(resp_data$id == "123Lyon", emp_ph$emp_ph[13],
                                                                                                        ifelse(resp_data$id == "137Toulon", emp_ph$emp_ph[16],
                                                                                                               ifelse(resp_data$id == "228Valencay", emp_ph$emp_ph[5],
                                                                                                                      ifelse(resp_data$id == "318Pessac", emp_ph$emp_ph[4],
                                                                                                                             ifelse(resp_data$id == "328Ludres", emp_ph$emp_ph[8],
                                                                                                                                    ifelse(resp_data$id == "353Tregastel", emp_ph$emp_ph[20],
                                                                                                                                           ifelse(resp_data$id == "420Pinsaguel", emp_ph$emp_ph[3],
                                                                                                                                                  ifelse(resp_data$id == "486Tinchebray", emp_ph$emp_ph[12],
                                                                                                                                                         ifelse(resp_data$id == "666Terville", emp_ph$emp_ph[9], NA
                                                                                                                                                         ))))))))))))))))))))
first_round_data <- filter(resp_data, !is.na(choice))
first_round_data$choice <- as.factor(first_round_data$choice)
first_round_data <- cbind(first_round_data[,c(1,18:22)], model.matrix( ~ 0 + choice, first_round_data))
first_round <- svydesign(id = ~id + resp_id, probs = ~first_stage + second_stage_2, data = 
                           first_round_data)
first_round <- as.svrepdesign(first_round, type="JK1")
shares <- svymean(~choice1 + choice2 + choice3 + choice4 + choice5 + choice6 + choice7 + 
                    choice8 + choice9 + choice10 + choice11, design = first_round, 
                  na.rm = TRUE, deff = TRUE)
data.frame(shares)*100
# Remaining information for Table 1 comes from 05-forecast.R

# Table C3 predicted first-round results based on sampled stations ----------------------
sample_des <- svydesign(id = ~1, probs = ~emp_ph, data = data2017)
jkdes <- as.svrepdesign(sample_des, type="JK1")
can1 <- svymean(~local_share, subset(jkdes, candidate_number == 1)) # dupont-aignan
can2 <- svymean(~local_share, subset(jkdes, candidate_number == 2)) # le pen
can3 <- svymean(~local_share, subset(jkdes, candidate_number == 3)) # macron
can4 <- svymean(~local_share, subset(jkdes, candidate_number == 4)) # hamon
can5 <- svymean(~local_share, subset(jkdes, candidate_number == 5)) # arthaud
can6 <- svymean(~local_share, subset(jkdes, candidate_number == 6)) # poutou
can7 <- svymean(~local_share, subset(jkdes, candidate_number == 7)) # cheminade
can8 <- svymean(~local_share, subset(jkdes, candidate_number == 8)) # lassalle
can9 <- svymean(~local_share, subset(jkdes, candidate_number == 9)) # mélenchon
can10 <- svymean(~local_share, subset(jkdes, candidate_number == 10)) # asselineau
can11 <- svymean(~local_share, subset(jkdes, candidate_number == 11)) # fillon

# Table C3 predicted second-round results based on sampled stations ---------------------
sample_des <- svydesign(id = ~1, probs = ~emp_ph, data = fpe2017_r2)
jkdes <- as.svrepdesign(sample_des, type="JK1")
can1 <- svymean(~local_share, subset(jkdes, candidate_number == 1)) # macron
can2 <- svymean(~local_share, subset(jkdes, candidate_number == 2)) # le pen

# Table C3 predicted first-round results based on survey responses ----------------------
first_round <- svydesign(id = ~id + resp_id, probs = ~emp_ph + second_stage_2, data = 
                           first_round_data)
first_round <- as.svrepdesign(first_round, type="JK1")
shares <- svymean(~choice1 + choice2 + choice3 + choice4 + choice5 + choice6 + choice7 + 
                    choice8 + choice9 + choice10 + choice11, design = first_round, 
                  na.rm = TRUE, deff = TRUE)
data.frame(shares)*100
# Remaining information for Table C3 comes from 05-forecast.R

# Table C1 ------------------------------------------------------------------------------
# extract deviations
all_sample_devs <- all_sample_metrics[[10]]
best_sample_devs <- best_sample_metrics[[10]][-1001]
# compute estimates
all_sample_estimates <- lapply(all_sample_devs, `+`, Vj_2017_r1_ov)
best_sample_estimates <- lapply(best_sample_devs, `+`, Vj_2017_r1_ov)
# population parameter
Vj_2017_r1_ov <- Vj_2017_r1_ov*100
# compute expected value for best samples (for each candidate)
evb_aignan <- mean(unlist(lapply(best_sample_estimates, `[[`, 1)))*100
evb_lepen <- mean(unlist(lapply(best_sample_estimates, `[[`, 2)))*100
evb_macron <- mean(unlist(lapply(best_sample_estimates, `[[`, 3)))*100
evb_hamon <- mean(unlist(lapply(best_sample_estimates, `[[`, 4)))*100
evb_arthaud <- mean(unlist(lapply(best_sample_estimates, `[[`, 5)))*100
evb_poutou <- mean(unlist(lapply(best_sample_estimates, `[[`, 6)))*100
evb_cheminade <- mean(unlist(lapply(best_sample_estimates, `[[`, 7)))*100
evb_lassalle <- mean(unlist(lapply(best_sample_estimates, `[[`, 8)))*100
evb_melenchon <- mean(unlist(lapply(best_sample_estimates, `[[`, 9)))*100
evb_asselineau <- mean(unlist(lapply(best_sample_estimates, `[[`, 10)))*100
evb_fillon <- mean(unlist(lapply(best_sample_estimates, `[[`, 11)))*100
# compute expected value for all samples (for each candidate)
eva_aignan <- mean(unlist(lapply(all_sample_estimates, `[[`, 1)))*100
eva_lepen <- mean(unlist(lapply(all_sample_estimates, `[[`, 2)))*100
eva_macron <- mean(unlist(lapply(all_sample_estimates, `[[`, 3)))*100
eva_hamon <- mean(unlist(lapply(all_sample_estimates, `[[`, 4)))*100
eva_arthaud <- mean(unlist(lapply(all_sample_estimates, `[[`, 5)))*100
eva_poutou <- mean(unlist(lapply(all_sample_estimates, `[[`, 6)))*100
eva_cheminade <- mean(unlist(lapply(all_sample_estimates, `[[`, 7)))*100
eva_lassalle <- mean(unlist(lapply(all_sample_estimates, `[[`, 8)))*100
eva_melenchon <- mean(unlist(lapply(all_sample_estimates, `[[`, 9)))*100
eva_asselineau <- mean(unlist(lapply(all_sample_estimates, `[[`, 10)))*100
eva_fillon <- mean(unlist(lapply(all_sample_estimates, `[[`, 11)))*100
# compute bias for all_samples
biasa_aignan <- eva_aignan-Vj_2017_r1_ov[1]
biasa_lepen <- eva_lepen-Vj_2017_r1_ov[2]
biasa_macron <- eva_macron-Vj_2017_r1_ov[3]
biasa_hamon <- eva_hamon-Vj_2017_r1_ov[4]
biasa_arthaud <- eva_arthaud-Vj_2017_r1_ov[5]
biasa_poutou <- eva_poutou-Vj_2017_r1_ov[6]
biasa_cheminade <- eva_cheminade-Vj_2017_r1_ov[7]
biasa_lassalle <- eva_lassalle-Vj_2017_r1_ov[8]
biasa_melenchon <- eva_melenchon-Vj_2017_r1_ov[9]
biasa_asselineau <- eva_asselineau-Vj_2017_r1_ov[10]
biasa_fillon <- eva_fillon-Vj_2017_r1_ov[11]
# compute bias for best_samples
biasb_aignan <- evb_aignan-Vj_2017_r1_ov[1]
biasb_lepen <- evb_lepen-Vj_2017_r1_ov[2]
biasb_macron <- evb_macron-Vj_2017_r1_ov[3]
biasb_hamon <- evb_hamon-Vj_2017_r1_ov[4]
biasb_arthaud <- evb_arthaud-Vj_2017_r1_ov[5]
biasb_poutou <- evb_poutou-Vj_2017_r1_ov[6]
biasb_cheminade <- evb_cheminade-Vj_2017_r1_ov[7]
biasb_lassalle <- evb_lassalle-Vj_2017_r1_ov[8]
biasb_melenchon <- evb_melenchon-Vj_2017_r1_ov[9]
biasb_asselineau <- evb_asselineau-Vj_2017_r1_ov[10]
biasb_fillon <- evb_fillon-Vj_2017_r1_ov[11]
# compute variance for all_samples
vara_aignan <- mean((eva_aignan-(unlist(lapply(all_sample_estimates, `[[`, 1))*100))^2)
vara_lepen <- mean((eva_lepen-(unlist(lapply(all_sample_estimates, `[[`, 2))*100))^2)
vara_macron <- mean((eva_macron-unlist(lapply(all_sample_estimates, `[[`, 3))*100)^2)
vara_hamon <- mean((eva_hamon-(unlist(lapply(all_sample_estimates, `[[`, 4))*100))^2)
vara_arthaud <- mean((eva_arthaud-(unlist(lapply(all_sample_estimates, `[[`, 5))*100))^2)
vara_poutou <- mean((eva_poutou-(unlist(lapply(all_sample_estimates, `[[`, 6))*100))^2)
vara_cheminade <- mean((eva_cheminade-(unlist(lapply(all_sample_estimates, `[[`, 7))*100))^2)
vara_lassalle <- mean((eva_lassalle-(unlist(lapply(all_sample_estimates, `[[`, 8))*100))^2)
vara_melenchon <- mean((eva_melenchon-(unlist(lapply(all_sample_estimates, `[[`, 9))*100))^2)
vara_asselineau <- mean((eva_asselineau-(unlist(lapply(all_sample_estimates, `[[`, 10))*100))^2)
vara_fillon <- mean((eva_fillon-(unlist(lapply(all_sample_estimates, `[[`, 11))*100))^2)
# compute variance for best_samples
varb_aignan <- mean((evb_aignan-(unlist(lapply(best_sample_estimates, `[[`, 1))*100))^2)
varb_lepen <- mean((evb_lepen-(unlist(lapply(best_sample_estimates, `[[`, 2))*100))^2)
varb_macron <- mean((evb_macron-(unlist(lapply(best_sample_estimates, `[[`, 3))*100))^2)
varb_hamon <- mean((evb_hamon-(unlist(lapply(best_sample_estimates, `[[`, 4))*100))^2)
varb_arthaud <- mean((evb_arthaud-(unlist(lapply(best_sample_estimates, `[[`, 5))*100))^2)
varb_poutou <- mean((evb_poutou-(unlist(lapply(best_sample_estimates, `[[`, 6))*100))^2)
varb_cheminade <- mean((evb_cheminade-(unlist(lapply(best_sample_estimates, `[[`, 7))*100))^2)
varb_lassalle <- mean((evb_lassalle-(unlist(lapply(best_sample_estimates, `[[`, 8))*100))^2)
varb_melenchon <- mean((evb_melenchon-(unlist(lapply(best_sample_estimates, `[[`, 9))*100))^2)
varb_asselineau <- mean((evb_asselineau-(unlist(lapply(best_sample_estimates, `[[`, 10))*100))^2)
varb_fillon <- mean((evb_fillon-(unlist(lapply(best_sample_estimates, `[[`, 11))*100))^2)
# mse for all_samples
msea_aignan <- mean((Vj_2017_r1_ov[1]-unlist(lapply(all_sample_estimates, `[[`, 1))*100)^2)
msea_lepen <- mean((Vj_2017_r1_ov[2]-unlist(lapply(all_sample_estimates, `[[`, 2))*100)^2)
msea_macron <- mean((Vj_2017_r1_ov[3]-unlist(lapply(all_sample_estimates, `[[`, 3))*100)^2)
msea_hamon <- mean((Vj_2017_r1_ov[4]-unlist(lapply(all_sample_estimates, `[[`, 4))*100)^2)
msea_arthaud <- mean((Vj_2017_r1_ov[5]-unlist(lapply(all_sample_estimates, `[[`, 5))*100)^2)
msea_poutou <- mean((Vj_2017_r1_ov[6]-unlist(lapply(all_sample_estimates, `[[`, 6))*100)^2)
msea_cheminade <- mean((Vj_2017_r1_ov[7]-unlist(lapply(all_sample_estimates, `[[`, 7))*100)^2)
msea_lassalle <- mean((Vj_2017_r1_ov[8]-unlist(lapply(all_sample_estimates, `[[`, 8))*100)^2)
msea_melenchon <- mean((Vj_2017_r1_ov[9]-unlist(lapply(all_sample_estimates, `[[`, 9))*100)^2)
msea_asselineau <- mean((Vj_2017_r1_ov[10]-unlist(lapply(all_sample_estimates, `[[`, 10))*100)^2)
msea_fillon <- mean((Vj_2017_r1_ov[11]-unlist(lapply(all_sample_estimates, `[[`, 11))*100)^2)
# mse for best_samples
mseb_aignan <- mean((Vj_2017_r1_ov[1]-unlist(lapply(best_sample_estimates, `[[`, 1))*100)^2)
mseb_lepen <- mean((Vj_2017_r1_ov[2]-unlist(lapply(best_sample_estimates, `[[`, 2))*100)^2)
mseb_macron <- mean((Vj_2017_r1_ov[3]-unlist(lapply(best_sample_estimates, `[[`, 3))*100)^2)
mseb_hamon <- mean((Vj_2017_r1_ov[4]-unlist(lapply(best_sample_estimates, `[[`, 4))*100)^2)
mseb_arthaud <- mean((Vj_2017_r1_ov[5]-unlist(lapply(best_sample_estimates, `[[`, 5))*100)^2)
mseb_poutou <- mean((Vj_2017_r1_ov[6]-unlist(lapply(best_sample_estimates, `[[`, 6))*100)^2)
mseb_cheminade <- mean((Vj_2017_r1_ov[7]-unlist(lapply(best_sample_estimates, `[[`, 7))*100)^2)
mseb_lassalle <- mean((Vj_2017_r1_ov[8]-unlist(lapply(best_sample_estimates, `[[`, 8))*100)^2)
mseb_melenchon <- mean((Vj_2017_r1_ov[9]-unlist(lapply(best_sample_estimates, `[[`, 9))*100)^2)
mseb_asselineau <- mean((Vj_2017_r1_ov[10]-unlist(lapply(best_sample_estimates, `[[`, 10))*100)^2)
mseb_fillon <- mean((Vj_2017_r1_ov[11]-unlist(lapply(best_sample_estimates, `[[`, 11))*100)^2)
# variance ratio
var_ratio_aignan <- round(varb_aignan/vara_aignan, digit = 2)
var_ratio_lepen <- round(varb_lepen/vara_lepen, digit = 2)
var_ratio_macron <- round(varb_macron/vara_macron, digit = 2)
var_ratio_hamon <- round(varb_hamon/vara_hamon, digit = 2)
var_ratio_arthaud <- round(varb_arthaud/vara_arthaud, digit = 2)
var_ratio_poutou <- round(varb_poutou/vara_poutou, digit = 2)
var_ratio_cheminade <- round(varb_cheminade/vara_cheminade, digit = 2)
var_ratio_lassalle <- round(varb_lassalle/vara_lassalle, digit = 2)
var_ratio_melenchon <- round(varb_melenchon/vara_melenchon, digit = 2)
var_ratio_asselineau <- round(varb_asselineau/vara_asselineau, digit = 2)
var_ratio_fillon <- round(varb_fillon/vara_fillon, digit = 2)

# average biases over all pps and balanced samples --------------------------------------
mean(abs(c(biasa_aignan,biasa_lepen,biasa_macron,biasa_hamon,biasa_arthaud,biasa_poutou,
           biasa_cheminade,biasa_lassalle,biasa_melenchon,biasa_asselineau,biasa_fillon)))
mean(abs(c(biasb_aignan,biasb_lepen,biasb_macron,biasb_hamon,biasb_arthaud,biasb_poutou,
           biasb_cheminade,biasb_lassalle,biasb_melenchon,biasb_asselineau,biasb_fillon)))