# ---------------------------------------------------------------------------------------
# INSTANT RUNOFF FORECASTING
# Peter Selb, Romain Lachat, and Sascha Göbel
# R script written by Sascha Göbel and Peter Selb
# Electoral data processing script
# July 2017
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
c("./data/official-electoral-data/fpe2002.txt", 
  "./data/official-electoral-data/fpe2007.xlsx",
  "./data/official-electoral-data/fpe2012.xlsx",
  "./data/official-electoral-data/fpe2017.txt")

# exports -------------------------------------------------------------------------------
c("./data/official-electoral-data-processed/fpe2002_r1",
  "./data/official-electoral-data-processed/fpe2007_r1", 
  "./data/official-electoral-data-processed/fpe2012_r1",
  "./data/official-electoral-data-processed/fpe2017_r1",
  "./data/official-electoral-data-processed/fpe2002_r1_ov",
  "./data/official-electoral-data-processed/fpe2007_r1_ov",
  "./data/official-electoral-data-processed/fpe2012_r1_ov",
  "./data/official-electoral-data-processed/fpe2017_r1_ov",
  "./data/official-electoral-data-processed/fpe2012_r2_ov",
  "./data/official-electoral-data-processed/fpe2002_2007_r1",
  "./data/official-electoral-data-processed/fpe2007_2002_r1",
  "./data/official-electoral-data-processed/fpe2007_2012_r1",
  "./data/official-electoral-data-processed/fpe2012_2007_r1",
  "./data/official-electoral-data-processed/fpe2012_2017_r1",
  "./data/official-electoral-data-processed/fpe2017_2012_r1")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("TOP LEVEL DIRECTORY")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")


#### DATA PROCESSING ====================================================================

# import official electoral polling place data and build candidate-round index ----------
fpe2002 <- read.csv2("./data/official-electoral-data/fpe2002.txt") %>%
           # import data
  mutate(index = str_c(.$candidate_abbrev, "_", .$round))
  # build candidate-round index to merge data with national first round result
fpe2007 <- read.xlsx("./data/official-electoral-data/fpe2007.xlsx", sheet = 1) %>%
           # import data
  mutate(index = str_c(.$candidate_abbrev, "_", .$round)) 
  # build candidate-round index to merge data with national first round result
fpe2012 <- read.xlsx("./data/official-electoral-data/fpe2012.xlsx", sheet = 1) %>%
           # import data
  mutate(index = str_c(.$candidate_abbrev, "_", .$round))
  # build candidate-round index to merge data with national first round result
fpe2017 <- read.csv2("./data/official-electoral-data/fpe2017.txt") %>%
           # import data
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
  # remove unwanted columns
  reshape(direction = 'long', 
          varying = colnames(.)[12:44], 
          timevar = "candidate_number",
          times = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
          v.names = c("candidate_name", "candidate_first_name", "votes"),
          idvar=colnames(.)[c(1,3,5)]) %>%
  # tranform into long format
  rename(candidate_name = candidate_first_name, candidate_first_name = candidate_name) %>%
  # rename columns
  mutate(index = str_c(.$candidate_name))
  # build candidate-round index to merge data with national first round result

# prepare candidates' official national first round results -----------------------------
# source: http://www.electionresources.org/fr/president.php
res2002 <- data.frame(index = unique(fpe2002$index),
           # build index to merge with polling place data
                      national_votes = c(667026, 535837, 132686, 1949170, 5665855,
                                         4804713, 660447, 1204689, 1495724, 4610113,
                                         339112, 960480, 1518528, 1113484, 1630045,
                                         1210562, 25537956, 5525032),
                      # add national vote totals
                      national_share = c(2.3, 1.9, 0.5, 6.8, 19.9, 16.9, 2.3, 4.2, 
                                         5.2, 16.2, 1.2, 3.4, 5.3, 3.9, 5.7, 4.2, 
                                         82.2, 17.8))
                      # add national vote share
res2007 <- data.frame(index = unique(fpe2007$index), 
           # build index to merge with polling place data
                      national_votes = c(483008, 707268, 123540, 487857, 818407, 
                                         576666, 6820119, 1498581, 9500112, 11448663,
                                         420645, 3834530, 16790440, 18983138), 
                      # add national vote totals
                      national_share = c(1.3, 1.9, 0.3, 1.3, 2.2, 1.6, 18.6, 4.1, 
                                         25.9, 31.2, 1.1, 10.4, 46.9, 53.1))
                      # add national vote share
res2012 <- data.frame(index = unique(fpe2012$index), 
                      # build index to merge with polling place data
                      national_votes = c(828345, 6421426, 9753629, 3984822, 3275122,
                                         10272705, 643907, 89545, 202548, 411160, 
                                         16860685, 18000668), 
                      # add national vote totals
                      national_share = c(2.3, 17.9, 27.2, 11.1, 9.1, 28.6, 1.8, 0.2,
                                         0.6, 1.1, 48.4, 51.6))
                      # add national vote share
res2017 <- data.frame(index = unique(fpe2017$index), 
           # build index to merge with polling place data
                      national_votes = c(1695000, 7678491, 8656346, 2291288, 232384, 
                                         394505, 65586, 435301, 7059951, 332547, 
                                         7212995), 
                      # add national vote totals
                      national_share = c(4.7, 21.3, 24.01, 6.36, 0.64, 1.09, 0.18, 1.21, 
                                         19.58, 0.92, 20.01))
                      # add national vote share

# join polling place data with national first round results -----------------------------
fpe2002 <- merge(fpe2002, res2002, by = "index") %>%
           # merge data by candidate-round index
  select(-index) %>%
  # remove candidate-round index
  arrange(round, department, municipality_code, polling_station_number, candidate_number)
  # reorder columns
fpe2007 <- merge(fpe2007, res2007, by = "index") %>%
           # merge data by candidate-round index
  select(-index) %>%
  # remove candidate-round index
  arrange(round, department, municipality_code, polling_station_number, candidate_number)
  # reorder columns
fpe2012 <- merge(fpe2012, res2012, by = "index") %>%
           # merge data by candidate-round index
  select(-index) %>%
  # remove candidate-round index
  arrange(round, department, municipality_code, polling_station_number, candidate_number)
  # reorder columns
fpe2017 <- merge(fpe2017, res2017, by = "index") %>%
           # merge data by candidate-round index
  select(-index)
  # remove candidate-round index

# align formatting of polling station numbering -----------------------------------------
fpe2002$polling_station_number <- fpe2002 %>%
                                  # select data for 2002
  use_series(polling_station_number) %>%
  # select column with polling station numbers
  str_pad(width = 4, pad = "0") %>%
  # add leading zeros until number has four digits
  str_replace_all(pattern = "0(?=[[:digit:]]+\\.)", replacement = "")
  # adjust formatting using regex
fpe2007$polling_station_number <- fpe2007 %>%
                                  # select data for 2007
  use_series(polling_station_number) %>%
  # select column with polling station numbers
  str_pad(width = 4, pad = "0") %>%
  # add leading zeros until number has four digits
  str_replace_all(pattern = "0(?=[[:digit:]]+\\.)", replacement = "") %>%
  str_replace(pattern = "(?<=\\.)09+(.)?", replacement = "1") %>%
  str_replace(pattern = "(?<=\\.)19+(.)?", replacement = "2") %>%
  str_replace(pattern = "(?<=\\.)39+(.)?", replacement = "4") %>%
  str_replace(pattern = "(?<=\\.[[:digit:]])0+(.)?", replacement = "")
  # adjust formatting using regex
fpe2012$polling_station_number <- fpe2012 %>%
                                  # select data for 2012
  use_series(polling_station_number) %>%
  # select column with polling station numbers
  str_pad(width = 4, pad = "0") %>%
  # add leading zeros until number has four digits
  str_replace_all(pattern = "0(?=[[:digit:]]+\\.)", replacement = "") %>%
  str_replace(pattern = "(?<=\\.)09+(.)?", replacement = "1") %>%
  str_replace(pattern = "(?<=\\.)19+(.)?", replacement = "2") %>%
  str_replace(pattern = "(?<=\\.)39+(.)?", replacement = "4") %>%
  str_replace(pattern = "(?<=\\.[[:digit:]])0+(.)?", replacement = "")
  # adjust formatting using regex

# prepare data with first round and overseas departments' results -----------------------
fpe2002_r1_ov <- fpe2002 %>%
                 # select data for 2002
  filter(round == 1)
  # keep only rows that pertain to first round results
fpe2007_r1_ov <- fpe2007 %>%
                 # select data for 2007
  filter(round == 1)
  # keep only rows that pertain to first round results
fpe2012_r1_ov <- fpe2012 %>%
                 # select data for 2012
  filter(round == 1)
  # keep only rows that pertain to first round results
fpe2017_r1_ov <- fpe2017

# prepare 2012 data with second round and overseas departments' results -----------------
fpe2012_r2_ov <- fpe2012 %>%
                 # select data for 2012
  filter(round == 2)
  # keep only rows that pertain to second round results

# prepare data with first round but without overseas departments' results ---------------
fpe2002_r1 <- fpe2002 %>%
              # select data for 2002
  filter(round == 1) %>%
  # keep only rows that pertain to first round results
  mutate(department = as.numeric(as.character(department))) %>%
  # make department column numeric, overseas departments are non-numeric and become NA,
  # warning message is hence not an issue
  filter(!is.na(department))
  # remove rows that are non-numeric in the department column, i.e. overseas departments 
fpe2007_r1 <- fpe2007 %>%
              # select data for 2007
  filter(round == 1) %>%
  # keep only rows that pertain to first round results
  mutate(department = as.numeric(as.character(department))) %>%
  # make department column numeric, overseas departments are non-numeric and become NA,
  # warning message is hence not an issue
  filter(!is.na(department))
  # remove rows that are non-numeric in the department column, i.e. overseas departments 
fpe2012_r1 <- fpe2012 %>%
              # select data for 2012
  filter(round == 1) %>%
  # keep only rows that pertain to first round results
  mutate(department = as.numeric(as.character(department))) %>%
  # make department column numeric, overseas departments are non-numeric and become NA
  # warning message is hence not an issue
  filter(!is.na(department))
  # remove rows that are non-numeric in the department column, i.e. overseas departments
fpe2017_r1 <- fpe2017 %>%
              # select data for 2017
  mutate(department = as.numeric(as.character(department))) %>%
  # make department column numeric, overseas departments are non-numeric and become NA
  # warning message is hence not an issue
  filter(!is.na(department))
  # remove rows that are non-numeric in the department column, i.e. overseas departments

# create unique polling place identifiers -----------------------------------------------
fpe2002_r1$polling_station_unique <- fpe2002_r1$municipality_code %>%
                                     # select data for 2002 and column with municipality 
                                     # codes
  interaction(fpe2002_r1$polling_station_number) %>%
  # concatenate municipality code with polling station number
  as.character %>%
  # transform to type "character"
  str_c(fpe2002_r1$department)
  # concatenate with department code
fpe2007_r1$polling_station_unique <- fpe2007_r1$municipality_code %>%
                                     # select data for 2007 and column with municipality 
                                     # codes
  interaction(fpe2007_r1$polling_station_number) %>%
  # concatenate municipality code with polling station number
  as.character %>%
  # transform to type "character"
  str_c(fpe2007_r1$department)
  # concatenate with department code
fpe2012_r1$polling_station_unique <- fpe2012_r1$municipality_code %>%
                                     # select data for 2002 and column with  
                                     # municipality codes
  interaction(fpe2012_r1$polling_station_number) %>%
  # concatenate municipality code with polling station number
  as.character %>%
  # transform to type "character"
  str_c(fpe2012_r1$department)
  # concatenate with department code
fpe2017_r1$polling_station_unique <- fpe2017_r1$municipality %>%
                                     # select data for 2017 and column with  
                                     # municipality codes
  interaction(fpe2017_r1$polling_station_number) %>%
  # concatenate municipality code with polling station number
  as.character %>%
  # transform to type "character"
  str_c(fpe2017_r1$department)
  # concatenate with department code
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
fpe2002_r1_ov$polling_station_unique <- fpe2002_r1_ov$municipality_code %>%
                                        # select data for 2002 and column with 
                                        # municipality codes
  interaction(fpe2002_r1_ov$polling_station_number) %>%
  # concatenate municipality code with polling station number
  as.character %>%
  # transform to type "character"
  str_c(fpe2002_r1_ov$department)
  # concatenate with department code
fpe2007_r1_ov$polling_station_unique <- fpe2007_r1_ov$municipality_code %>%
                                        # select data for 2007 and column with 
                                        # municipality codes
  interaction(fpe2007_r1_ov$polling_station_number) %>%
  # concatenate municipality code with polling station number
  as.character %>%
  # transform to type "character"
  str_c(fpe2007_r1_ov$department)
  # concatenate with department code
fpe2012_r1_ov$polling_station_unique <- fpe2012_r1_ov$municipality_code %>%
                                        # select data for 2002 and column with  
                                        # municipality codes
  interaction(fpe2012_r1_ov$polling_station_number) %>%
  # concatenate municipality code with polling station number
  as.character %>%
  # transform to type "character"
  str_c(fpe2012_r1_ov$department)
  # concatenate with department code
fpe2017_r1_ov$polling_station_unique <- fpe2017_r1_ov$municipality %>%
                                        # select data for 2017 and column with  
                                        # municipality codes
  interaction(fpe2017_r1_ov$polling_station_number) %>%
  # concatenate municipality code with polling station number
  as.character %>%
  # transform to type "character"
  str_c(fpe2017_r1_ov$department)
  # concatenate with department code
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
                                        # select data for 2002 and column with  
                                        # municipality codes
  interaction(fpe2012_r2_ov$polling_station_number) %>%
  # concatenate municipality code with polling station number
  as.character %>%
  # transform to type "character"
  str_c(fpe2012_r2_ov$department)
  # concatenate with department code

# prepare data with permanent polling stations across election pairs --------------------
fpe2002_2007_r1 <- as.logical(fpe2002_r1$polling_station_unique %in% 
                                fpe2007_r1$polling_station_unique) %>%
  # build index identifying permanent polling places
  data.frame(a = .) %>%
  # store index in a data frame column
  use_series(a) %>%
  # select the index column
  mutate(fpe2002_r1, polling_station_across = .) %>%
  # add index column to data
  filter(polling_station_across == TRUE)
  # keep only rows with permanent polling stations
fpe2007_2002_r1 <- as.logical(fpe2007_r1$polling_station_unique %in% 
                                fpe2002_r1$polling_station_unique) %>%
                   # build index identifying permanent polling places
  data.frame(a = .) %>%
  # store index in a data frame column
  use_series(a) %>%
  # select the index column
  mutate(fpe2007_r1, polling_station_across = .) %>%
  # add index column to data
  filter(polling_station_across == TRUE)
  # keep only rows with permanent polling stations
fpe2007_2012_r1 <- as.logical(fpe2007_r1$polling_station_unique %in% 
                                fpe2012_r1$polling_station_unique) %>%
                   # build index identifying permanent polling places
  data.frame(a = .) %>%
  # store index in a data frame column
  use_series(a) %>%
  # select the index column
  mutate(fpe2007_r1, polling_station_across = .) %>%
  # add index column to data
  filter(polling_station_across == TRUE)
  # keep only rows with permanent polling stations
fpe2012_2007_r1 <- as.logical(fpe2012_r1$polling_station_unique %in% 
                                fpe2007_r1$polling_station_unique) %>%
                   # build index identifying permanent polling places
  data.frame(a = .) %>%
  # store index in a data frame column
  use_series(a)  %>%
  # select the index column
  mutate(fpe2012_r1, polling_station_across = .) %>%
  # add index column to data
  filter(polling_station_across == TRUE)
  # keep only rows with permanent polling stations
fpe2012_2017_r1 <- as.logical(fpe2012_r1$polling_station_unique %in% 
                                fpe2017_r1$polling_station_unique) %>%
                   # build index identifying permanent polling places
  data.frame(a = .) %>%
  # store index in a data frame column
  use_series(a)  %>%
  # select the index column
  mutate(fpe2012_r1, polling_station_across = .) %>%
  # add index column to data
  filter(polling_station_across == TRUE)
  # keep only rows with permanent polling stations
fpe2017_2012_r1 <- as.logical(fpe2017_r1$polling_station_unique %in% 
                                fpe2012_r1$polling_station_unique) %>%
                   # build index identifying permanent polling places
  data.frame(a = .) %>%
  # store index in a data frame column
  use_series(a)  %>%
  # select the index column
  mutate(fpe2017_r1, polling_station_across = .) %>%
  # add index column to data
  filter(polling_station_across == TRUE)
  # keep only rows with permanent polling stations

# adjust candidate numbers for 2012 and 2017 data ---------------------------------------
fpe2012_r1$candidate_number <- fpe2012_r1$candidate_number %>%
                               # select data for 2002 and column with 
                               # candidate number
  subtract(1)
  # subtract 1 from each candidate number to make the sequence start at 1 not 2
fpe2012_2017_r1$candidate_number <- fpe2012_2017_r1$candidate_number %>%
                                    # select data for 2002 and column with 
                                    # candidate number
  subtract(1)
  # subtract 1 from each candidate number to make the sequence start at 1 not 2
fpe2012_r1_ov$candidate_number <- fpe2012_r1_ov$candidate_number %>%
                                  # select data for 2002 and column with 
                                  # candidate number
  subtract(1)
  # subtract 1 from each candidate number to make the sequence start at 1 not 2
fpe2012_r2_ov$candidate_number <- fpe2012_r2_ov$candidate_number %>%
                                  # select data for 2002 and column with 
                                  # candidate number
  subtract(1)
  # subtract 1 from each candidate number to make the sequence start at 1 not 2
fpe2017_2012_r1$candidate_number <- as.numeric(fpe2017_2012_r1$candidate_number)

# write data to disk --------------------------------------------------------------------
saveRDS(fpe2002_r1, "./data/official-electoral-data-processed/fpe2002_r1")
saveRDS(fpe2007_r1, "./data/official-electoral-data-processed/fpe2007_r1")
saveRDS(fpe2012_r1, "./data/official-electoral-data-processed/fpe2012_r1")
saveRDS(fpe2017_r1, "./data/official-electoral-data-processed/fpe2017_r1")
saveRDS(fpe2002_r1_ov, "./data/official-electoral-data-processed/fpe2002_r1_ov")
saveRDS(fpe2007_r1_ov, "./data/official-electoral-data-processed/fpe2007_r1_ov")
saveRDS(fpe2012_r1_ov, "./data/official-electoral-data-processed/fpe2012_r1_ov")
saveRDS(fpe2017_r1_ov, "./data/official-electoral-data-processed/fpe2017_r1_ov")
saveRDS(fpe2012_r2_ov, "./data/official-electoral-data-processed/fpe2012_r2_ov")
saveRDS(fpe2002_2007_r1, "./data/official-electoral-data-processed/fpe2002_2007_r1")
saveRDS(fpe2007_2002_r1, "./data/official-electoral-data-processed/fpe2007_2002_r1")
saveRDS(fpe2007_2012_r1, "./data/official-electoral-data-processed/fpe2007_2012_r1")
saveRDS(fpe2012_2007_r1, "./data/official-electoral-data-processed/fpe2012_2007_r1")
saveRDS(fpe2012_2017_r1, "./data/official-electoral-data-processed/fpe2012_2017_r1")
saveRDS(fpe2017_2012_r1, "./data/official-electoral-data-processed/fpe2017_2012_r1")

# read data from disk -------------------------------------------------------------------
fpe2002_r1 <- readRDS("./data/official-electoral-data-processed/fpe2002_r1")
fpe2007_r1 <- readRDS("./data/official-electoral-data-processed/fpe2007_r1")
fpe2012_r1 <- readRDS("./data/official-electoral-data-processed/fpe2012_r1")
fpe2017_r1 <- readRDS("./data/official-electoral-data-processed/fpe2017_r1")
fpe2002_r1_ov <- readRDS("./data/official-electoral-data-processed/fpe2002_r1_ov")
fpe2007_r1_ov <- readRDS("./data/official-electoral-data-processed/fpe2007_r1_ov")
fpe2012_r1_ov <- readRDS("./data/official-electoral-data-processed/fpe2012_r1_ov")
fpe2017_r1_ov <- readRDS("./data/official-electoral-data-processed/fpe2017_r1_ov")
fpe2012_r2_ov <- readRDS("./data/official-electoral-data-processed/fpe2012_r2_ov")
fpe2002_2007_r1 <- readRDS("./data/official-electoral-data-processed/fpe2002_2007_r1")
fpe2007_2002_r1 <- readRDS("./data/official-electoral-data-processed/fpe2007_2002_r1")
fpe2007_2012_r1 <- readRDS("./data/official-electoral-data-processed/fpe2007_2012_r1")
fpe2012_2007_r1 <- readRDS("./data/official-electoral-data-processed/fpe2012_2007_r1")
fpe2012_2017_r1 <- readRDS("./data/official-electoral-data-processed/fpe2012_2017_r1")
fpe2017_2012_r1 <- readRDS("./data/official-electoral-data-processed/fpe2017_2012_r1")
