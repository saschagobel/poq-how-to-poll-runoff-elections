# ---------------------------------------------------------------------------------------
# INSTANT RUNOFF FORECASTING
# Peter Selb, Romain Lachat, and Sascha Göbel
# R script written by Sascha Göbel and Peter Selb
# Exit poll processing script
# July 2017
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
c("./data/exit-poll-data-corrected/*.csv", "./data/polling-places.csv")

# exports -------------------------------------------------------------------------------
c("./data/resp_data", "./data/cand_data")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("TOP LEVEL DIRECTORY")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")


#### DATA PROCESSING ====================================================================

# read file names of corrected surveys --------------------------------------------------
surveys <- list.files(path = "./data/exit-poll-data-corrected/", pattern="*.csv")

# import corrected surveys, assign polling place id, and stack --------------------------
resp_data <- lapply(surveys, function(x) {
             # apply function to all file paths in "surveys"
  read.csv2(paste0("./data/exit-poll-data-corrected/", x), stringsAsFactors = FALSE) %>% 
  # import surveys
    mutate(id = factor(str_extract(x, "(?<=-)[[:digit:]]{3}.+(?=\\.)")))
    # add column with polling place id taken from the file name
  }) %>% 
  rbindlist(fill = TRUE)
  # stack surveys

# rename and remove redundant columns ---------------------------------------------------
resp_data <- rename(resp_data, respondent = Bogen, polling_station_number = polling_station, 
                    sex = Vous.etes...., age = Votre.age.est....) %>%
  # rename variables
  select(-Datensatz.Ursprung)
  # remove column "Datensatz.Ursprung"
colnames(resp_data) <- colnames(resp_data) %>%
  # select column names
  str_replace("Le\\.", "Le") %>%
  # remove blank space inbetween "Le Pen"
  str_replace(".+\\.", "")
  # cut candidate names to surnames

# append inclusion probabilities and metadata -------------------------------------------
polling_places <- read.csv2("./data/polling-places.csv") %>%
  # import file holding polling place/first stage inclusion probabilities
  filter(in_sample == "yes") %>%
  # drop polling places that were not sampled
  mutate(id = str_replace(.$id, "^(?=[[:digit:]]{2}[[:alpha:]])", "0"))
  # add leading zeros to ids with two digits only to merge properly with resp_data
polling_places$second_stage <- c(0.08797654, 0.058651026, 0.080753701, 0.236220472,
                                 0.066006601, 0.068181818, 0.070011669, 0.061728395,
                                 0.12145749, 0.074441687, 0.114503817, 0.064034152,
                                 0.061099796, 0.069044879, 0.109289617, 0.044280443,
                                 0.148883375, 0.080321285, 0.100840336, 0.111317254)
                               # add individual/second stage inclusion probabilities
resp_data <- merge(resp_data, polling_places[,c(13,14,15)])
             # add first and second stage inclusion probabilities and metadata 
             # to resp_data, adjust with filter

# add unique respondent id and transform ids to factor ----------------------------------
resp_data$resp_id <- factor(1:nrow(resp_data))
                     # build respondent id as factor
resp_data$id <- factor(resp_data$id)
                # transform polling place id to factor

# build candidate data with first round vote totals -------------------------------------
# source: http://elections.interieur.gouv.fr/presidentielle-2017/FE.html
cand_data <- data.frame(choice = factor(1:11), 
                        # build variable with candidate id
                        vote_totals = c(232384, 394505, 7059951, 2291288, 8656346, 
                                        7212995, 1695000, 7678491, 65586, 332547, 
                                        435301))
             # build variable with national vote totals

# write data to disk --------------------------------------------------------------------
saveRDS(resp_data, "./data/resp_data")
saveRDS(cand_data, "./data/cand_data")
