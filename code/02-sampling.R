# ---------------------------------------------------------------------------------------
# INSTANT RUNOFF FORECASTING
# Peter Selb, Romain Lachat, and Sascha Göbel
# R script written by Sascha Göbel and Peter Selb
# Sampling script
# July 2017
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
c("./data/official-electoral-data-processed/fpe2012_r1",
  "./data/official-electoral-data-processed/fpe2012_r1_ov",
  "./data/official-electoral-data-processed/fpe2012_r2_ov")

# exports -------------------------------------------------------------------------------
c("./data/Opt", "./data/sample-s20-k200-m20", "./data/polling-places.csv")


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
fpe2012_r2_ov <- readRDS("./data/official-electoral-data-processed/fpe2012_r2_ov")
fpe2012_r2 <- readRDS("./data/official-electoral-data-processed/fpe2012_r2_ov") %>%
  mutate(department = as.numeric(as.character(department))) %>%
  # make department column numeric, overseas departments are non-numeric and become NA,
  # warning message is hence not an issue
  filter(!is.na(department))
  # remove rows that are non-numeric in the department column, i.e. overseas departments 


#### DESIGN EFFECT ======================================================================

# prepare electoral data ----------------------------------------------------------------
fpe2012_r2 <- fpe2012_r2[!fpe2012_r2$valid_votes == 0,]
              # remove polling stations without valid votes
fpe2012_r2$vote <- ifelse(fpe2012_r2$candidate_name == "HOLLANDE", 
                          1, 0)
                   # make rows for Hollande 1 and those for Sarkozy 0

# build 2012 voter population data ------------------------------------------------------
vote <- rep(fpe2012_r2$vote, fpe2012_r2$votes)
        # build individual choice vector of voter population:
        # repeat each instance of 1 - "Hollande" and 0 - "Sarkozy" times the 
        # number of votes recorded for the respective candidate at the respective polling 
        # station
psuID <- rep(fpe2012_r2$polling_station_unique, fpe2012_r2$votes)
         # assign a polling station ID to each individual choice
# department <- rep(fpe2012_r2$department, fpe2012_r2$votes)
# region <- rep(fpe2012_r2$municipality_name, fpe2012_r2$votes)
runoff2012 <- data.frame(department = department, region = region, station = psuID, vote = vote)
vote_data <- data.frame(vote = vote, psuID = psuID)
             # build data frame
pp1 <- table(vote_data$psuID) / nrow(vote_data)
       # compute one-draw inclusion probabilities of polling stations

# compute variance components -----------------------------------------------------------
BW <- BW2stagePPS(vote_data$vote, psuID=vote_data$psuID, pp=pp1)
# see Valliant et al. 2013, ch 9

# compute optima for a vector of budgets ------------------------------------------------
Opt <- clusOpt2fixedPSU(C1=200, C2=c(1,2,3,4,5,6,7,8,9,10,20,30,40,50), 
                        m=20, delta=BW["delta"], unit.rv=BW["unit relvar"], k=BW["k"], 
                        tot.cost=10000, cal.sw=1)
# see Valliant et al. 2013, ch 10

# write data to disk --------------------------------------------------------------------
saveRDS(Opt, "./data/Opt")

# compute design effect -----------------------------------------------------------------
Deff <- BW["k"]*(1+(60-1)*BW["delta"])
Deff


#### SAMPLING ===========================================================================

# compute vote total across polling places and candidates (N) ---------------------------
N_2012_r1_ov <- sum(fpe2012_r1_ov$votes)

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

# compute number of voters at polling places (Nh) ---------------------------------------
Nh_2012_r1 <- fpe2012_r1 %>% 
              # select data for 2012
  extract(!duplicated(.$polling_station_unique),TRUE) %>% 
  # keep only one row per polling place
  use_series(voters) %>%
  # select column with number of voters at polling place
  as.numeric
  # transform to type numeric

# set parameters for replicate probability sampling -------------------------------------
k <- 200
     # set number of samples to be drawn in one simulation run
m <- 20
     # set sample size
sim <- 20
       # set number of times the sampling process shall be repeated

# compute first-order inclusion probabilities with PPS of Nh for varying sample size m --
ph_2012_r1 <- lapply(m, inclusionprobabilities, a = Nh_2012_r1) 

# replicate probability sampling of polling places at the previous election in 2012 -----
set.seed(2003172)
  # specify seed to make sampling reproducible
sample_s20_k200_m20 <- replicatePpsSampling(k = k, 
                                            m = m, 
                                            Nh = Nh_2012_r1, 
                                            Vj = Vj_2012_r1_ov, 
                                            ph = ph_2012_r1, 
                                            sim = sim, 
                                            data = fpe2012_r1)
                        # draw PPS samples
saveRDS(sample_s20_k200_m20, "./data/sample-s20-k200-m20")
  # save samples to disk
sample_s20_k200_m20 <- readRDS("./data/sample-s20-k200-m20")
  # read samples from disk

# compile details about polling places from the selected sample -------------------------
  # out of the 20 simulation runs, the sample from the second run was selected for 
  # practical reasons such as accessibility of polling places
polling_place_ids <- fpe2012_r1[!duplicated(fpe2012_r1$polling_station_unique),]$polling_station_unique %>% 
                     # select 2012 data and keep only one row per polling place and column
                     # with polling place ids
   extract(unlist(sample_s20_k200_m20[[2]][2])) %>%
   # keep only polling places from the second simulation run
   c("486.000161", "318.003833", "318.004033", "318.004133", "318.004233", "63.002892", 
     "63.002992", "63.003192", "63.003292", "420.000131", "137.003783", "137.003883", 
     "123.051169", "103.001013", "34.000213", "666.000357", "666.000457", "666.000557", 
     "48.000493", "228.000236", "353.000122", "81.000174", "81.000374", "81.000474")
   # add ids of polling places that are geographically adjacent to sampled places
polling_places <- fpe2012_r1[fpe2012_r1$polling_station_unique %in% polling_place_ids,] %>%
                  # select 2012 data and keep only rows pertaining to the selected 
                  # polling place ids
  extract(, c("department", "municipality_code", "municipality_name", 
              "polling_station_number", "polling_station_unique", "voters", "valid_votes", "votes", 
              "candidate_name")) %>%
  # select specified columns
  mutate(in_sample = ifelse(.$polling_station_unique %in% polling_place_ids[1:20], "yes", 
                            "no"))
  # add column identifying whether polling place was sampled or added post sampling as
  # geographically adjacent 
polling_places <- polling_places[polling_places$candidate_name == "HOLLANDE",]
                  # keep only rows with second round winner
polling_places$polling_station_number <- str_replace(polling_places$polling_station_number, 
                                                     "^0", "")
                                         # remove leading zeros in polling station number
polling_places$id <- str_c(polling_places$municipality_code, 
                           polling_places$municipality_name)
                     # concatenate municiaplity codes with municipality names
polling_places$short_id <- str_c(polling_places$municipality_code, 
                                 polling_places$polling_station_number)
                           # concatenate municipality codes with polling station numbers
polling_places$short_id <- str_pad(polling_places$short_id, 6, "left", "0")
                           # add leading zeros where short id is not 6 digits long 
polling_places$id <- str_replace(polling_places$id, "ç", "c")
                     # adjust encoding
polling_places$id <- str_replace(polling_places$id, "é|è", "e")
                     # adjust encoding
colnames(polling_places)[c(6, 7, 8)] <- c("fr_voters", "fr_valid_votes", "fr_votes_hollande")
                                        # rename columns
polling_places_r2 <- fpe2012_r2_ov[fpe2012_r2_ov$polling_station_unique %in% polling_place_ids,] %>%
                     # select 2012 second round data and keep only rows pertaining to the  
                     # selected polling place ids
  filter(candidate_name == "HOLLANDE")
  # keep only rows with second round winner
colnames(polling_places_r2)[c(10,15)] <- c("sr_valid_votes", "sr_votes_hollande")
                                         # rename solumns
polling_places <- cbind(polling_places[,1:8], sr_valid_votes = 
                        polling_places_r2$sr_valid_votes, sr_votes_hollande = 
                        polling_places_r2$sr_votes_hollande, polling_places[,10:11])
                  # add second round valid votes and votes for second round winner to
                  # details about selected polling places
polling_places$first_stage <- ph_2012_r1[[1]][which(fpe2012_r1[!duplicated(
  fpe2012_r1$polling_station_unique),]$polling_station_unique %in% 
    polling_places$polling_station_unique)]
  # add first order inclusion probabilities for all elements in the sample elements
write.csv2(polling_places, file = "./data/polling-places.csv", quote = TRUE)
  # write table to disk