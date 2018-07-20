# ---------------------------------------------------------------------------------------
# INSTANT RUNOFF FORECASTING
# Peter Selb, Romain Lachat, and Sascha Göbel
# R script written by Sascha Göbel and Peter Selb
# Additional analyses script
# July 2017
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
c("./data/resp_data", "./data/resp_data_mock", "./data/cand_data")

# exports -------------------------------------------------------------------------------
c("./data/id_posterior2")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("TOP LEVEL DIRECTORY")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")

# read data from disk -------------------------------------------------------------------
resp_data_mock <- readRDS("./data/resp_data_mock")
cand_data <- readRDS("./data/cand_data")
ratings_original <- readRDS("./data/resp_data")[,4:15] %>%
  filter(!is.na(choice)) %>%
  select(-choice)
ratings_original[ratings_original == 0] <- NA


#### HYPOTHETICAL RUNOFF SCENARIOS ======================================================

# build complex survey design object ----------------------------------------------------
ppsdesign_mock <- split(resp_data_mock, f = resp_data_mock$.imp) %>%
                  # split resp_data_mock into a list of imputed data frames
  lapply(function(x) {
    svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = x)
    })
  # specify list of multistage PPS survey designs

# adjust list of survey designs via poststratification ----------------------------------
ppsdesign_post_mock <- lapply(ppsdesign_mock, function(x) {
  postStratify(design = x, strata = ~choice, population = cand_data)
  })

# compute runoff shares for runoff Macron vs Fillon -------------------------------------
shares_post_MaFi <- lapply(ppsdesign_post_mock, function(x) {
  svymean(~choiceMacronFi + choiceFillonMa + abstainMaFi, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute runoff candidates' second round vote shares with poststratification from 
  # each survey design object in the list
shares_post_MaFi <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_post_MaFi, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_post_MaFi, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_post_MaFi, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(shares_post_MaFi, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(shares_post_MaFi, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(shares_post_MaFi, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(shares_post_MaFi, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(shares_post_MaFi, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(shares_post_MaFi, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(shares_post_MaFi, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_post_MaFi, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_post_MaFi, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(shares_post_MaFi, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(shares_post_MaFi, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(shares_post_MaFi, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(shares_post_MaFi, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(shares_post_MaFi, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(shares_post_MaFi, function(x) confint(x, level = 0.99)[2,2]))))
    ) %>%
    # take the mean of votes, ses and deffs over the list of computed vote totals
    # and store in data frame
  set_rownames(c("Macron", "Fillon"))
  # assign row names

# compute runoff shares for runoff Macron vs Fillon -------------------------------------
shares_post_MaMe <- lapply(ppsdesign_post_mock, function(x) {
  svymean(~choiceMacronMe + choiceMelenchonMa + abstainMaMe, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute runoff candidates' second round vote shares with poststratification from 
  # each survey design object in the list
shares_post_MaMe <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_post_MaMe, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_post_MaMe, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_post_MaMe, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(shares_post_MaMe, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(shares_post_MaMe, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(shares_post_MaMe, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(shares_post_MaMe, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(shares_post_MaMe, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(shares_post_MaMe, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(shares_post_MaMe, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_post_MaMe, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_post_MaMe, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(shares_post_MaMe, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(shares_post_MaMe, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(shares_post_MaMe, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(shares_post_MaMe, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(shares_post_MaMe, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(shares_post_MaMe, function(x) confint(x, level = 0.99)[2,2]))))
    ) %>%
    # take the mean of votes, ses and deffs over the list of computed vote totals
    # and store in data frame
  set_rownames(c("Macron", "Melenchon"))
  # assign row names

# compute runoff shares for runoff Le Pen vs Fillon -------------------------------------
shares_post_LeFi <- lapply(ppsdesign_post_mock, function(x) {
  svymean(~choiceLePenFi + choiceFillonLe + abstainLeFi, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute runoff candidates' second round vote shares with poststratification from 
  # each survey design object in the list
shares_post_LeFi <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_post_LeFi, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_post_LeFi, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_post_LeFi, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(shares_post_LeFi, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(shares_post_LeFi, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(shares_post_LeFi, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(shares_post_LeFi, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(shares_post_LeFi, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(shares_post_LeFi, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(shares_post_LeFi, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_post_LeFi, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_post_LeFi, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(shares_post_LeFi, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(shares_post_LeFi, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(shares_post_LeFi, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(shares_post_LeFi, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(shares_post_LeFi, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(shares_post_LeFi, function(x) confint(x, level = 0.99)[2,2]))))
    ) %>%
    # take the mean of votes, ses and deffs over the list of computed vote totals
    # and store in data frame
  set_rownames(c("LePen", "Fillon"))
  # assign row names

# compute runoff shares for runoff Le Pen vs Melenchon ----------------------------------
shares_post_LeMe <- lapply(ppsdesign_post_mock, function(x) {
  svymean(~choiceLePenMe + choiceMelenchonLe + abstainLeMe, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute runoff candidates' second round vote shares with poststratification from 
  # each survey design object in the list
shares_post_LeMe <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_post_LeMe, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_post_LeMe, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_post_LeMe, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(shares_post_LeMe, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(shares_post_LeMe, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(shares_post_LeMe, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(shares_post_LeMe, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(shares_post_LeMe, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(shares_post_LeMe, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(shares_post_LeMe, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_post_LeMe, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_post_LeMe, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(shares_post_LeMe, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(shares_post_LeMe, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(shares_post_LeMe, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(shares_post_LeMe, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(shares_post_LeMe, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(shares_post_LeMe, function(x) confint(x, level = 0.99)[2,2]))))
    ) %>%
    # take the mean of votes, ses and deffs over the list of computed vote totals
    # and store in data frame
  set_rownames(c("LePen", "Melenchon"))
  # assign row names

# compute runoff shares for runoff Fillon vs Melenchon ----------------------------------
shares_post_FiMe <- lapply(ppsdesign_post_mock, function(x) {
  svymean(~choiceFillonMe + choiceMelenchonFi + abstainFiMe, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute runoff candidates' second round vote shares with poststratification from 
  # each survey design object in the list
shares_post_FiMe <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_post_FiMe, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_post_FiMe, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_post_FiMe, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(shares_post_FiMe, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(shares_post_FiMe, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(shares_post_FiMe, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(shares_post_FiMe, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(shares_post_FiMe, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(shares_post_FiMe, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(shares_post_FiMe, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_post_FiMe, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_post_FiMe, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(shares_post_FiMe, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(shares_post_FiMe, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(shares_post_FiMe, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(shares_post_FiMe, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(shares_post_FiMe, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(shares_post_FiMe, function(x) confint(x, level = 0.99)[2,2]))))
    ) %>%
    # take the mean of votes, ses and deffs over the list of computed vote totals
    # and store in data frame
  set_rownames(c("Fillon", "Melenchon"))
  # assign row names


#### SPATIAL ANALYSIS OF CANDIDATE RATINGS ==============================================

# Note: R package skalunfold required. We used a pre-beta version, the package is not yet
#       officially published.

# unidimensional model and quadratic loss function --------------------------------------
posterior1 <- skalunfold.estimate(ratings_original, valence = 2, distance = 2, 
                                  dimension = 1, n.iter = 2000, n.burnin = 1000, 
                                  impute.missing = TRUE)
              # obtain posteriors of spatial positions and valence
id_posterior1 <- skalunfold.postestimate(posterior1)
                 # identify estimation results

# convergence check and information criteria --------------------------------------------
skalunfold.devcheck(posterior1)
  # check Markov Chain convergence
skalunfold.fit(id_posterior1, type="Criteria")
  # evaluate fit of posterior to data

# two-dimensional model and quadratic loss function -------------------------------------
posterior2 <- skalunfold.estimate(ratings_original, valence = 2, distance = 2, 
                                  dimension = 2, n.iter = 2000, n.burnin = 1000, 
                                  impute.missing = TRUE)
              # obtain posteriors of spatial positions and valence

id_posterior2 <- skalunfold.postestimate(posterior2)
                 # identify estimation results

# convergence check and information criteria --------------------------------------------
skalunfold.devcheck(posterior2)
  # check Markov Chain convergence
skalunfold.fit(id_posterior2, type="Criteria")
  # evaluate fit of posterior to data

# write identified estimation results to disk -------------------------------------------
saveRDS(id_posterior2, "./data/id_posterior2")
