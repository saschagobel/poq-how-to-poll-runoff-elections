# ---------------------------------------------------------------------------------------
# HOW TO POLL RUNOFF ELECTIONS
# Peter Selb, Sascha Goebel, and Romain Lachat
# Script for processing of the exit poll
# April 2020
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
  './data/exit-poll-data-corrected/*.csv' 
  './data/polling-places.csv'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
  './data/resp_data' 
  './data/cand_data'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
    Line 28 - PREPARATIONS
    Line 49 - DATA PROCESSING
    ")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("poq-how-to-poll-runoff-elections")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")

# read data from disk -------------------------------------------------------------------
polling_places <- read.csv2("./data/polling-places.csv")
surveys <- list.files(path = "./data/exit-poll-data-corrected/", pattern="*.csv")
resp_data <- lapply(surveys, function(x) {
  read.csv2(paste0("./data/exit-poll-data-corrected/", x), stringsAsFactors = FALSE) %>% 
    mutate(id = factor(str_extract(x, "(?<=-)[[:digit:]]{3}.+(?=\\.)")))
}) %>% 
  rbindlist(fill = TRUE)


#### DATA PROCESSING ====================================================================

# rename and remove redundant columns ---------------------------------------------------
resp_data <- rename(resp_data, respondent = Bogen, polling_station_number = polling_station, 
                    sex = Vous.etes...., age = Votre.age.est....) %>%
  select(-Datensatz.Ursprung)
colnames(resp_data) <- colnames(resp_data) %>%
  str_replace("Le\\.", "Le") %>%
  str_replace(".+\\.", "")

# append inclusion probabilities and metadata -------------------------------------------
polling_places <- polling_places %>%
  filter(in_sample == "yes") %>%
  mutate(id = str_replace(.$id, "^(?=[[:digit:]]{2}[[:alpha:]])", "0"))
polling_places$second_stage <- c(0.08797654, 0.058651026, 0.080753701, 0.236220472,
                                 0.066006601, 0.068181818, 0.070011669, 0.061728395,
                                 0.12145749, 0.074441687, 0.114503817, 0.064034152,
                                 0.061099796, 0.069044879, 0.109289617, 0.044280443,
                                 0.148883375, 0.080321285, 0.100840336, 0.111317254)
resp_data <- merge(resp_data, polling_places[,c(13,14,15)])

# add unique respondent id and transform ids to factor ----------------------------------
resp_data$resp_id <- factor(1:nrow(resp_data))
resp_data$id <- factor(resp_data$id)

# build candidate data with first round vote totals -------------------------------------
# source: http://elections.interieur.gouv.fr/presidentielle-2017/FE.html
cand_data <- data.frame(choice = factor(1:11), 
                        vote_totals = c(232384, 394505, 7059951, 2291288, 8656346, 
                                        7212995, 1695000, 7678491, 65586, 332547, 
                                        435301))

# write data to disk --------------------------------------------------------------------
saveRDS(resp_data, "./data/resp_data")
saveRDS(cand_data, "./data/cand_data")
