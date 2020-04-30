# ---------------------------------------------------------------------------------------
# HOW TO POLL RUNOFF ELECTIONS
# Peter Selb, Sascha Goebel, and Romain Lachat
# Script for processing of electoral data
# April 2020
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
  './data/official-electoral-data/fpe2012.xlsx'
  './data/official-electoral-data/fpe2017.txt'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
  './data/official-electoral-data-processed/fpe2012_r1'
  './data/official-electoral-data-processed/fpe2017_r1'
  './data/official-electoral-data-processed/fpe2012_r1_ov'
  './data/official-electoral-data-processed/fpe2017_r1_ov'
  './data/official-electoral-data-processed/fpe2012_r2_ov',
  './data/official-electoral-data-processed/fpe2012_2017_r1'
  './data/official-electoral-data-processed/fpe2017_2012_r1'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
  Line 33 - PREPARATIONS
  Line 46 - DATA PROCESSING
  ")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("poq-how-to-poll-runoff-elections")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")


#### DATA PROCESSING ====================================================================

# import official electoral polling place data and build candidate-round index ----------
fpe2012 <- read.xlsx("./data/official-electoral-data/fpe2012.xlsx", sheet = 1) %>%
  mutate(index = str_c(.$candidate_abbrev, "_", .$round))
fpe2017 <- read.csv2("./data/official-electoral-data/fpe2017.txt") %>%
  select(-c(district, district_name, abs.ins,vot.ins,blancs.ins,blancs.vot,nuls.ins,
            nuls.vot,exp.ins,exp.vot,X1.candidate_number,X1.sex,X1.voix.ins,X1.voix.exp,
            X2.candidate_number,X2.sex,X2.voix.ins,X2.voix.exp,X3.candidate_number,
            X3.sex,X3.voix.ins,X3.voix.exp,X4.candidate_number,X4.sex,X4.voix.ins,
            X4.voix.exp,X5.candidate_number,X5.sex,X5.voix.ins,X5.voix.exp,
            X6.candidate_number,X6.sex,X6.voix.ins,X6.voix.exp,X7.candidate_number,
            X7.sex,X7.voix.ins,X7.voix.exp,X8.candidate_number,X8.sex,X8.voix.ins,
            X8.voix.exp,X9.candidate_number,X9.sex,X9.voix.ins,X9.voix.exp,
            X10.candidate_number,X10.sex,X10.voix.ins,X10.voix.exp,X11.candidate_number,
            X11.sex,X11.voix.ins,X11.voix.exp)) %>% 
  reshape(direction = 'long', 
          varying = colnames(.)[12:44], 
          timevar = "candidate_number",
          times = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
          v.names = c("candidate_name", "candidate_first_name", "votes"),
          idvar=colnames(.)[c(1,3,5)]) %>%
  rename(candidate_name = candidate_first_name, candidate_first_name = candidate_name) %>%
  mutate(index = str_c(.$candidate_name))

# prepare candidates' official national first round results -----------------------------
# source: http://www.electionresources.org/fr/president.php
res2012 <- data.frame(index = unique(fpe2012$index), 
                      national_votes = c(828345, 6421426, 9753629, 3984822, 3275122,
                                         10272705, 643907, 89545, 202548, 411160, 
                                         16860685, 18000668), 
                      national_share = c(2.3, 17.9, 27.2, 11.1, 9.1, 28.6, 1.8, 0.2,
                                         0.6, 1.1, 48.4, 51.6))
res2017 <- data.frame(index = unique(fpe2017$index), 
                      national_votes = c(1695000, 7678491, 8656346, 2291288, 232384, 
                                         394505, 65586, 435301, 7059951, 332547, 
                                         7212995), 
                      national_share = c(4.7, 21.3, 24.01, 6.36, 0.64, 1.09, 0.18, 1.21, 
                                         19.58, 0.92, 20.01))

# join polling place data with national first round results -----------------------------
fpe2012 <- merge(fpe2012, res2012, by = "index") %>%
  select(-index) %>%
  arrange(round, department, municipality_code, polling_station_number, candidate_number)
fpe2017 <- merge(fpe2017, res2017, by = "index") %>%
  select(-index)

# align formatting of polling station numbering -----------------------------------------
fpe2012$polling_station_number <- fpe2012 %>%
  use_series(polling_station_number) %>%
  str_pad(width = 4, pad = "0") %>%
  # add leading zeros until number has four digits
  str_replace_all(pattern = "0(?=[[:digit:]]+\\.)", replacement = "") %>%
  str_replace(pattern = "(?<=\\.)09+(.)?", replacement = "1") %>%
  str_replace(pattern = "(?<=\\.)19+(.)?", replacement = "2") %>%
  str_replace(pattern = "(?<=\\.)39+(.)?", replacement = "4") %>%
  str_replace(pattern = "(?<=\\.[[:digit:]])0+(.)?", replacement = "")

# prepare data with first round and overseas departments' results -----------------------
fpe2012_r1_ov <- fpe2012 %>%
  filter(round == 1)
fpe2017_r1_ov <- fpe2017

# prepare 2012 data with second round and overseas departments' results -----------------
fpe2012_r2_ov <- fpe2012 %>%
  filter(round == 2)

# prepare data with first round but without overseas departments' results ---------------
fpe2012_r1 <- fpe2012 %>%
  filter(round == 1) %>%
  mutate(department = as.numeric(as.character(department))) %>%
  filter(!is.na(department))
fpe2017_r1 <- fpe2017 %>%
  mutate(department = as.numeric(as.character(department))) %>%
  filter(!is.na(department))

# create unique polling place identifiers -----------------------------------------------
fpe2012_r1$polling_station_unique <- fpe2012_r1$municipality_code %>%
  interaction(fpe2012_r1$polling_station_number) %>%
  as.character %>%
  str_c(fpe2012_r1$department)
fpe2017_r1$polling_station_unique <- fpe2017_r1$municipality %>%
  interaction(fpe2017_r1$polling_station_number) %>%
  as.character %>%
  str_c(fpe2017_r1$department)
fpe2017_r1[fpe2017_r1$polling_station_unique == "50.000549",]$polling_station_unique <- 78.000149
  # change polling_station_unique 50.000549 (5th polling station at municipality
  # Brissac-Loire-Aubance) to 78.000149 (1st polling station at 
  # Charcé-Saint-Ellier-sur-Aubance). Necessary to match with sample drawn from 2012.
  # On 15 December 2016 Charcé-Saint-Ellier-sur-Aubance was merged into the new commune 
  # Brissac Loire Aubance (see 
  # https://en.wikipedia.org/wiki/Charc%C3%A9-Saint-Ellier-sur-Aubance)
  # For matching of addresses see
  # http://www.linternaute.com/ville/charce-saint-ellier-sur-aubance/ville-49078/bureaux-vote
  # and
  # http://www.linternaute.com/ville/brissac-quince/ville-49050/bureaux-vote
fpe2012_r1_ov$polling_station_unique <- fpe2012_r1_ov$municipality_code %>%
  interaction(fpe2012_r1_ov$polling_station_number) %>%
  as.character %>%
  str_c(fpe2012_r1_ov$department)
fpe2017_r1_ov$polling_station_unique <- fpe2017_r1_ov$municipality %>%
  interaction(fpe2017_r1_ov$polling_station_number) %>%
  as.character %>%
  str_c(fpe2017_r1_ov$department)
fpe2017_r1_ov[fpe2017_r1_ov$polling_station_unique == "50.000549",]$polling_station_unique <- "78.000149"
  # change polling_station_unique 50.000549 (5th polling station at municipality
  # Brissac-Loire-Aubance) to 78.000149 (1st polling station at 
  # Charcé-Saint-Ellier-sur-Aubance). Necessary to match with sample drawn from 2012.
  # On 15 December 2016 Charcé-Saint-Ellier-sur-Aubance was merged into the new commune 
  # Brissac Loire Aubance (see 
  # https://en.wikipedia.org/wiki/Charc%C3%A9-Saint-Ellier-sur-Aubance)
  # For matching of addresses see
  # http://www.linternaute.com/ville/charce-saint-ellier-sur-aubance/ville-49078/bureaux-vote
  # and
  # http://www.linternaute.com/ville/brissac-quince/ville-49050/bureaux-vote
fpe2012_r2_ov$polling_station_unique <- fpe2012_r2_ov$municipality_code %>%
  interaction(fpe2012_r2_ov$polling_station_number) %>%
  as.character %>%
  str_c(fpe2012_r2_ov$department)

# prepare data with permanent polling stations across election pairs --------------------
fpe2012_2017_r1 <- as.logical(fpe2012_r1$polling_station_unique %in% 
                                fpe2017_r1$polling_station_unique) %>%
  data.frame(a = .) %>%
  use_series(a)  %>%
  mutate(fpe2012_r1, polling_station_across = .) %>%
  filter(polling_station_across == TRUE)
fpe2017_2012_r1 <- as.logical(fpe2017_r1$polling_station_unique %in% 
                                fpe2012_r1$polling_station_unique) %>%
  data.frame(a = .) %>%
  use_series(a)  %>%
  mutate(fpe2017_r1, polling_station_across = .) %>%
  filter(polling_station_across == TRUE)

# adjust candidate numbers for 2012 and 2017 data ---------------------------------------
fpe2012_r1$candidate_number <- fpe2012_r1$candidate_number %>%
  subtract(1)
fpe2012_2017_r1$candidate_number <- fpe2012_2017_r1$candidate_number %>%
  subtract(1)
fpe2012_r1_ov$candidate_number <- fpe2012_r1_ov$candidate_number %>%
  subtract(1)
fpe2012_r2_ov$candidate_number <- fpe2012_r2_ov$candidate_number %>%
  subtract(1)
fpe2017_2012_r1$candidate_number <- as.numeric(fpe2017_2012_r1$candidate_number)

# write data to disk --------------------------------------------------------------------
saveRDS(fpe2012_r1, "./data/official-electoral-data-processed/fpe2012_r1")
saveRDS(fpe2017_r1, "./data/official-electoral-data-processed/fpe2017_r1")
saveRDS(fpe2012_r1_ov, "./data/official-electoral-data-processed/fpe2012_r1_ov")
saveRDS(fpe2017_r1_ov, "./data/official-electoral-data-processed/fpe2017_r1_ov")
saveRDS(fpe2012_r2_ov, "./data/official-electoral-data-processed/fpe2012_r2_ov")
saveRDS(fpe2012_2017_r1, "./data/official-electoral-data-processed/fpe2012_2017_r1")
saveRDS(fpe2017_2012_r1, "./data/official-electoral-data-processed/fpe2017_2012_r1")
