# ---------------------------------------------------------------------------------------
# INSTANT RUNOFF FORECASTING
# Peter Selb, Romain Lachat, and Sascha Göbel
# R script written by Sascha Göbel and Peter Selb
# Validation script
# July 2017
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
c("./data/official-electoral-data-processed/fpe2012_r1", 
  "./data/official-electoral-data-processed/fpe2012_r1_ov",
  "./data/official-electoral-data-processed/fpe2017_r1_ov",
  "./data/official-electoral-data-processed/fpe2002_2007_r1",
  "./data/official-electoral-data-processed/fpe2007_2002_r1",
  "./data/official-electoral-data-processed/fpe2007_2012_r1",
  "./data/official-electoral-data-processed/fpe2012_2007_r1",
  "./data/official-electoral-data-processed/fpe2012_2017_r1",
  "./data/official-electoral-data-processed/fpe2017_2012_r1",
  "./data/sample-s20-k200-m20")

# exports -------------------------------------------------------------------------------
c("./data/euclidean-2002-2007", "./data/euclidean-2007-2002", 
  "./data/euclidean-2007-2012", "./data/euclidean-2012-2007", 
  "./data/euclidean-2012-2017", "./data/euclidean-2017-2012",
  "./data/Bs_k", "./data/Bs_m")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("TOP LEVEL DIRECTORY")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")

# read data from disk -------------------------------------------------------------------
fpe2012_r1 <- readRDS("./data/official-electoral-data-processed/fpe2012_r1")
fpe2012_r1_ov <- readRDS("./data/official-electoral-data-processed/fpe2012_r1_ov")
fpe2017_r1_ov <- readRDS("./data/official-electoral-data-processed/fpe2017_r1_ov")
fpe2002_2007_r1 <- readRDS("./data/official-electoral-data-processed/fpe2002_2007_r1")
fpe2007_2002_r1 <- readRDS("./data/official-electoral-data-processed/fpe2007_2002_r1")
fpe2007_2012_r1 <- readRDS("./data/official-electoral-data-processed/fpe2007_2012_r1")
fpe2012_2007_r1 <- readRDS("./data/official-electoral-data-processed/fpe2012_2007_r1")
fpe2012_2017_r1 <- readRDS("./data/official-electoral-data-processed/fpe2012_2017_r1")
fpe2017_2012_r1 <- readRDS("./data/official-electoral-data-processed/fpe2017_2012_r1")
sample_s20_k200_m20 <- readRDS("./data/sample-s20-k200-m20")


#### VALIDATION =========================================================================

# compute candidates' first round vote share at polling place level ---------------------
fpe2002_2007_r1$local_share <- fpe2002_2007_r1$votes %>%
                               # select column with votes for 2002 data with stable 
                               # polling places in 2007 
  divide_by(fpe2002_2007_r1$valid_votes) %>%
  # divide by column with valid votes
  multiply_by(100)
  # scale between 0 and 100
fpe2007_2002_r1$local_share <- fpe2007_2002_r1$votes %>%
                               # select column with votes for 2007 data with stable 
                               # polling places in 2002 
  divide_by(fpe2007_2002_r1$valid_votes) %>%
  # divide by column with valid votes
  multiply_by(100)
  # scale between 0 and 100
fpe2007_2012_r1$local_share <- fpe2007_2012_r1$votes %>%
                               # select column with votes for 2007 data with stable 
                               # polling places in 2012 
  divide_by(fpe2007_2012_r1$valid_votes) %>%
  # divide by column with valid votes
  multiply_by(100)
  # scale between 0 and 100
fpe2012_2007_r1$local_share <- fpe2012_2007_r1$votes %>%
                               # select column with votes for 2012 data with stable 
                               # polling places in 2007 
  divide_by(fpe2012_2007_r1$valid_votes) %>%
  # divide by column with valid votes
  multiply_by(100)
  # scale between 0 and 100
fpe2012_2017_r1$local_share <- fpe2012_2017_r1$votes %>%
                               # select column with votes for 2012 data with stable 
                               # polling places in 2017
  divide_by(fpe2012_2017_r1$valid_votes) %>%
  # divide by column with valid votes
  multiply_by(100)
  # scale between 0 and 100
fpe2017_2012_r1$local_share <- fpe2017_2012_r1$votes %>%
                               # select column with votes for 2017 data with stable 
                               # polling places in 2012 
  divide_by(fpe2017_2012_r1$valid_votes) %>%
  # divide by column with valid votes
  multiply_by(100)
  # scale between 0 and 100

# compute Euclidean distances between polling place and national vote shares ------------
fpe2002_2007_r1$euclid <- fpe2002_2007_r1$local_share %>%
                          # select column with polling place vote shares for 2002 data 
                          # with stable polling places in 2007
  subtract(fpe2002_2007_r1$national_share) %>%
  # subtract column with national vote shares
  raise_to_power(2) %>%
  # square
  tapply(fpe2002_2007_r1$polling_station_unique, sum) %>%
  # sum by polling place
  sqrt %>%
  # take square root
  extract(match(fpe2002_2007_r1$polling_station_unique, names(.))) %>%
  # match polling place distance to polling place in candidate data
  as.numeric
  # tranform to numeric
fpe2007_2002_r1$euclid <- fpe2007_2002_r1$local_share %>%
                          # select column with polling place vote shares for 2007 data 
                          # with stable polling places in 2002
  subtract(fpe2007_2002_r1$national_share) %>%
  # subtract column with national vote shares
  raise_to_power(2) %>%
  # square
  tapply(fpe2007_2002_r1$polling_station_unique, sum) %>%
  # sum by polling place
  sqrt %>%
  # take square root
  extract(match(fpe2007_2002_r1$polling_station_unique, names(.))) %>%
  # match polling place distance to polling place in candidate data
  as.numeric
  # tranform to numeric
fpe2007_2012_r1$euclid <- fpe2007_2012_r1$local_share %>%
                          # select column with polling place vote shares for 2007 data 
                          # with stable polling places in 2012
  subtract(fpe2007_2012_r1$national_share) %>%
  # subtract column with national vote shares
  raise_to_power(2) %>%
  # square
  tapply(fpe2007_2012_r1$polling_station_unique, sum) %>%
  # sum by polling place
  sqrt %>%
  # take square root
  extract(match(fpe2007_2012_r1$polling_station_unique, names(.))) %>%
  # match polling place distance to polling place in candidate data
  as.numeric
  # tranform to numeric
fpe2012_2007_r1$euclid <- fpe2012_2007_r1$local_share %>%
                          # select column with polling place vote shares for 2012 data 
                          # with stable polling places in 2007
  subtract(fpe2012_2007_r1$national_share) %>%
  # subtract column with national vote shares
  raise_to_power(2) %>%
  # square
  tapply(fpe2012_2007_r1$polling_station_unique, sum) %>%
  # sum by polling place
  sqrt %>%
  # take square root
  extract(match(fpe2012_2007_r1$polling_station_unique, names(.))) %>%
  # match polling place distance to polling place in candidate data
  as.numeric
  # tranform to numeric
fpe2012_2017_r1$euclid <- fpe2012_2017_r1$local_share %>%
                          # select column with polling place vote shares for 2012 data 
                          # with stable polling places in 2017
  subtract(fpe2012_2017_r1$national_share) %>%
  # subtract column with national vote shares
  raise_to_power(2) %>%
  # square
  tapply(fpe2012_2017_r1$polling_station_unique, sum) %>%
  # sum by polling place
  sqrt %>%
  # take square root
  extract(match(fpe2012_2017_r1$polling_station_unique, names(.))) %>%
  # match polling place distance to polling place in candidate data
  as.numeric
  # tranform to numeric
fpe2017_2012_r1$euclid <- fpe2017_2012_r1$local_share %>%
                          # select column with polling place vote shares for 2017 data 
                          # with stable polling places in 2012
  subtract(fpe2017_2012_r1$national_share) %>%
  # subtract column with national vote shares
  raise_to_power(2) %>%
  # square
  tapply(fpe2017_2012_r1$polling_station_unique, sum) %>%
  # sum by polling place
  sqrt %>%
  # take square root
  extract(match(fpe2017_2012_r1$polling_station_unique, names(.))) %>%
  # match polling place distance to polling place in candidate data
  as.numeric
  # tranform to numeric

# collapse Euclidean distance to polling station level ----------------------------------
eu_dist_2002_2007 <- fpe2002_2007_r1 %>%
                     # select 2002 data with stable polling places in 2007
  group_by(polling_station_unique) %>%
  # group by polling places
  summarise(voters0207 = mean(voters), euclid0207 = mean(euclid))
  # take mean of voters and Euclidean distance at polling place
eu_dist_2007_2002 <- fpe2007_2002_r1 %>%
                     # select 2007 data with stable polling places in 2002
  group_by(polling_station_unique) %>%
  # group by polling places
  summarise(voters0702 = mean(voters), euclid0702 = mean(euclid))
  # take mean of voters and Euclidean distance at polling place
eu_dist_2007_2012 <- fpe2007_2012_r1 %>%
                     # select 2007 data with stable polling places in 2012
  group_by(polling_station_unique) %>%
  # group by polling places
  summarise(voters0712 = mean(voters), euclid0712 = mean(euclid))
  # take mean of voters and Euclidean distance at polling place
eu_dist_2012_2007 <- fpe2012_2007_r1 %>%
                     # select 2012 data with stable polling places in 2007
  group_by(polling_station_unique) %>%
  # group by polling places
  summarise(voters1207 = mean(voters), euclid1207 = mean(euclid))
  # take mean of voters and Euclidean distance at polling place
eu_dist_2012_2017 <- fpe2012_2017_r1 %>%
                     # select 2012 data with stable polling places in 2017
  group_by(polling_station_unique) %>%
  # group by polling places
  summarise(voters1217 = mean(voters), euclid1217 = mean(euclid))
  # take mean of voters and Euclidean distance at polling place
eu_dist_2017_2012 <- fpe2017_2012_r1 %>%
                     # select 2017 data with stable polling places in 2012
  group_by(polling_station_unique) %>%
  # group by polling places
  summarise(voters1712 = mean(voters), euclid1712 = mean(euclid))
  # take mean of voters and Euclidean distance at polling place

# write euclidean distances to disk -----------------------------------------------------
saveRDS(eu_dist_2002_2007, "./data/euclidean-2002-2007")
saveRDS(eu_dist_2007_2002, "./data/euclidean-2007-2002")
saveRDS(eu_dist_2007_2012, "./data/euclidean-2007-2012")
saveRDS(eu_dist_2012_2007, "./data/euclidean-2012-2007")
saveRDS(eu_dist_2012_2017, "./data/euclidean-2012-2017")
saveRDS(eu_dist_2017_2012, "./data/euclidean-2017-2012")

# compute vote total across polling places and candidates (N) ---------------------------
N_2012_r1_ov <- sum(fpe2012_r1_ov$votes)
N_2017_r1_ov <- sum(fpe2017_r1_ov$votes)

# compute candidates' national vote shares (Vj) -----------------------------------------
Vj_2012_r1_ov <- fpe2012_r1_ov %>% 
                 # select 2012 data
  group_by(candidate_number) %>% 
  # group by candidates
  summarise(sum(votes)) %>% 
  # sum votes by candidates
  extract2(2) %>% 
  # select candidates' vote totals
  divide_by(N_2012_r1_ov)
  # divide candidates' vote totals by vote total across polling places and candidates 
Vj_2017_r1_ov <- fpe2017_r1_ov %>% 
                 # select 2017 data
  group_by(as.numeric(candidate_number)) %>% 
  # group by candidates
  summarise(sum(votes)) %>% 
  # sum votes by candidates
  extract2(2) %>% 
  # select candidates' vote totals
  divide_by(N_2017_r1_ov)
  # divide candidates' vote totals by vote total across polling places and candidates 

# compute number of voters at polling places (Nh) ---------------------------------------
Nh_2012_r1 <- fpe2012_r1 %>% 
              # select data for 2012
  extract(!duplicated(.$polling_station_unique),TRUE) %>% 
  # keep only one row per polling place
  use_series(voters) %>%
  # select column with number of voters at polling place
  as.numeric
  # transform to type numeric
Nh_2012_2017_r1 <- fpe2012_2017_r1 %>% 
                   # select data for 2012
  extract(!duplicated(.$polling_station_unique),TRUE) %>% 
  # keep only one row per polling place
  use_series(voters) %>%
  # select column with number of voters at polling place
  as.numeric
  # transform to type numeric

# validation of choice of number of replications k --------------------------------------
m <- 20
     # set sample size
ph_2012_r1 <- lapply(m, inclusionprobabilities, a = Nh_2012_r1)
              # compute first-order inclusion probabilities with PPS of Nh for fix sample 
              # size m
sim <- 100
       # set number of times the sampling process shall be repeated
kvar <- seq(0,300, by = 5) %>% replace(1, 1)
        # set varying number of samples/replications to be drawn
Bs_k <- rep(list(NA), times = length(kvar))
        # prepare target to store bias B over multiple simulation runs with varying k
set.seed(0300)
  # specify seed to make validation reproducible
for(i in 1:length(kvar)) {
  # set up loop to iterate replicate probability sampling over varying k
  k <- kvar[i]
       # set k to ith value of kvar
  Bs_k[[i]] <- replicatePpsSampling(k = k, 
                                    m = m, 
                                    Nh = Nh_2012_r1, 
                                    Vj = Vj_2012_r1_ov, 
                                    ph = ph_2012_r1, 
                                    sim = sim, 
                                    data = fpe2012_r1)[[1]]
               # draw PPS samples and select minimum B values of simulation runs
}
saveRDS(Bs_k, "./data/Bs_k")
  # write Bs_k to disk
Bs_k <- readRDS("./data/Bs_k")
  # read Bs_k from disk

# validation of choice of stage 1 sample size m -----------------------------------------
k <- 200
     # set fix number of samples/replications to be drawn
m <- seq(2,50, by = 1)
     # set varying sample size m
ph_2012_r1 <- lapply(m, inclusionprobabilities, a = Nh_2012_r1)
              # compute first-order inclusion probabilities with PPS of Nh for varying 
              # sample size m
set.seed(150)
  # specify seed to make validation reproducible
Bs_m <- replicatePpsSampling(k = k, 
                             m = m, 
                             Nh = Nh_2012_r1, 
                             Vj = Vj_2012_r1_ov, 
                             ph = ph_2012_r1, 
                             sim = sim, 
                             data = fpe2012_r1)
        # draw PPS samples
saveRDS(Bs_m, "./data/Bs_m")
  # write Bs_m to disk
Bs_m <- readRDS(Bs_m, "./data/Bs_m")

# validation of replicate sampling strategy ---------------------------------------------
k <- 200
     # set fix number of samples/replications to be drawn
m <- 20
     # set sample size m
sim <- 20
       # set number of times the sampling process shall be repeated
ph_2012_2017_r1 <- lapply(m, inclusionprobabilities, a = Nh_2012_2017_r1)
                   # compute first-order inclusion probabilities with PPS of Nh for fix 
                   # sample size m
set.seed(2003172)
  # specify seed to make sampling reproducible
validation_sample <- replicatePpsSampling(k = k, 
                                          m = m, 
                                          Nh = Nh_2012_2017_r1, 
                                          Vj = Vj_2012_r1_ov, 
                                          ph = ph_2012_2017_r1, 
                                          sim = sim, 
                                          data = fpe2012_2017_r1)
                    # draw PPS samples from 2012 data with stable polling places in 2017
saveRDS(validation_sample, "./data/validation_sample")
  # save samples to disk
validation_sample <- readRDS("./data/validation_sample")
  # read samples from disk
sel_sample <- fpe2012_r1[!duplicated(fpe2012_r1$polling_station_unique),]$polling_station_unique %>% 
              # select 2012 data and keep only one row per polling place and the column
              # with polling place ids
  extract(unlist(sample_s20_k200_m20[[2]][2]))
  # select the polling places chosen in the original replicate sampling
sel_sample_2012 <- list(which(fpe2012_2017_r1[!duplicated(fpe2012_2017_r1$polling_station_unique),]$polling_station_unique %in%
                                sel_sample))
                   # find the position of the polling places chosen in the original 
                   # replicate sampling within 2012 data with stable polling stations in 2017
val_samples <- unlist(validation_sample[[3]], recursive = FALSE) %>%
  c(sel_sample_2012)
  # compile indices of the 4000 validation samples and the sample chosen in the original
  # replicate sampling, within 2012 data with stable polling places in 2017
Bs_val <- validation(m = 20, 
                     Vj = Vj_2017_r1_ov, 
                     ph = ph_2012_2017_r1, 
                     sample = val_samples, 
                     data1 = fpe2012_2017_r1, 
                     data2 = fpe2017_2012_r1)
          # obtain bias B of 2012 polling places in val_samples in 2017
saveRDS(Bs_val, "./data/Bs_val")
  # save distribution of bias B to disk 
Bs_val <- readRDS("./data/Bs_val")
  # read distribution of bias B from disk
