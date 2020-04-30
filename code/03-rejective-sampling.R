# ---------------------------------------------------------------------------------------
# HOW TO POLL RUNOFF ELECTIONS
# Peter Selb, Sascha Goebel, and Romain Lachat
# Script for rejective sampling
# April 2020
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
  './data/official-electoral-data-processed/fpe2012_r1'
  './data/official-electoral-data-processed/fpe2012_r1_ov'
  './data/official-electoral-data-processed/fpe2012_r2_ov'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
  './data/sample-s20-k200-m20' 
  './data/polling-places.csv'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
  Line 29 - PREPARATIONS
  Line 47 - SAMPLING
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
fpe2012_r2_ov <- readRDS("./data/official-electoral-data-processed/fpe2012_r2_ov")


#### SAMPLING ===========================================================================

# compute vote total across polling places and candidates (N) ---------------------------
N_2012_r1_ov <- sum(fpe2012_r1_ov$votes)

# compute candidates' national vote shares (Vj) -----------------------------------------
Vj_2012_r1_ov <- fpe2012_r1_ov %>% 
  group_by(candidate_number) %>% 
  summarise(sum(votes)) %>% 
  magrittr::extract2(2) %>% 
  divide_by(N_2012_r1_ov)

# compute candidates' national vote totals (Nj) -----------------------------------------
Nj_2012_r1 <- fpe2012_r1 %>% 
  group_by(candidate_number) %>% 
  summarise(sum(votes)) %>% 
  extract2(2)

# compute number of voters at polling places (Nh) ---------------------------------------
Nh_2012_r1 <- fpe2012_r1 %>% 
  extract(!duplicated(.$polling_station_unique),TRUE) %>% 
  use_series(voters) %>%
  as.numeric

# set parameters for rejective probability sampling -------------------------------------
# number of samples to be drawn in one simulation run
k <- 200
# sample size
m <- 20
# number of times the sampling process shall be repeated
sim <- 20

# compute first-stage inclusion probabilities with PPS of Nh for varying sample size m --
ph_2012_r1 <- lapply(m, inclusionprobabilities, a = Nh_2012_r1) 

# rejective probability sampling of polling places at the previous election in 2012 -----
set.seed(2003172) # at the time, sampling was based only on Bk as metric
sample_s2_k200_m20 <- rejectivePpsSampling(k = k, 
                                           m = m, 
                                           Nh = Nh_2012_r1, 
                                           Nj = Nj_2012_r1,
                                           Vj = Vj_2012_r1_ov,
                                           ph = unlist(ph_2012_r1), 
                                           sim = sim,
                                           parallelize = FALSE,
                                           data = fpe2012_r1)

# save samples to disk ------------------------------------------------------------------
saveRDS(sample_s20_k200_m20, "./data/sample-s20-k200-m20")

# compile details about polling places from the selected sample -------------------------
# out of the 20 simulation runs, the sample from the second run was selected for 
# practical reasons such as accessibility of polling places
# select 2012 data and keep only one row per polling place and column
# with polling place ids
polling_place_ids <- fpe2012_r1[!duplicated(fpe2012_r1$polling_station_unique),]$polling_station_unique %>% 
  # keep only polling places from the second simulation run
  extract(unlist(sample_s20_k200_m20[[2]][2])) %>%
  # add ids of polling places that are geographically adjacent to sampled places 
  c("486.000161", "318.003833", "318.004033", "318.004133", "318.004233", "63.002892", 
    "63.002992", "63.003192", "63.003292", "420.000131", "137.003783", "137.003883", 
    "123.051169", "103.001013", "34.000213", "666.000357", "666.000457", "666.000557", 
    "48.000493", "228.000236", "353.000122", "81.000174", "81.000374", "81.000474")
polling_places <- fpe2012_r1[fpe2012_r1$polling_station_unique %in% polling_place_ids,] %>%
  extract(, c("department", "municipality_code", "municipality_name", 
              "polling_station_number", "polling_station_unique", "voters", "valid_votes", "votes", 
              "candidate_name")) %>%
  # add column identifying whether polling place was sampled or added post sampling as
  # geographically adjacent 
  mutate(in_sample = ifelse(.$polling_station_unique %in% polling_place_ids[1:20], "yes", 
                            "no"))
# keep only rows with second round winner
polling_places <- polling_places[polling_places$candidate_name == "HOLLANDE",]
# remove leading zeros in polling station number
polling_places$polling_station_number <- str_replace(polling_places$polling_station_number, 
                                                     "^0", "")
# concatenate municiaplity codes with municipality names                    
polling_places$id <- str_c(polling_places$municipality_code, 
                           polling_places$municipality_name)
# concatenate municipality codes with polling station numbers
polling_places$short_id <- str_c(polling_places$municipality_code, 
                                 polling_places$polling_station_number)
# add leading zeros where short id is not 6 digits long 
polling_places$short_id <- str_pad(polling_places$short_id, 6, "left", "0")
# adjust encoding       
polling_places$id <- str_replace(polling_places$id, "ç", "c")
# adjust encoding
polling_places$id <- str_replace(polling_places$id, "é|è", "e")
# rename columns
colnames(polling_places)[c(6, 7, 8)] <- c("fr_voters", "fr_valid_votes", "fr_votes_hollande")
# select 2012 second round data and keep only rows pertaining to the  
# selected polling place ids                   
polling_places_r2 <- fpe2012_r2_ov[fpe2012_r2_ov$polling_station_unique %in% polling_place_ids,] %>%
  # keep only rows with second round winner
  filter(candidate_name == "HOLLANDE")
# rename solumns
colnames(polling_places_r2)[c(10,15)] <- c("sr_valid_votes", "sr_votes_hollande")
# add second round valid votes and votes for second round winner to
# details about selected polling places
polling_places <- cbind(polling_places[,1:8], sr_valid_votes = 
                        polling_places_r2$sr_valid_votes, sr_votes_hollande = 
                        polling_places_r2$sr_votes_hollande, polling_places[,10:11])
# add first order inclusion probabilities for all elements in the sample elements                
polling_places$first_stage <- ph_2012_r1[[1]][which(fpe2012_r1[!duplicated(
  fpe2012_r1$polling_station_unique),]$polling_station_unique %in% 
    polling_places$polling_station_unique)]
# write table to disk
write.csv2(polling_places, file = "./data/polling-places.csv", quote = TRUE)