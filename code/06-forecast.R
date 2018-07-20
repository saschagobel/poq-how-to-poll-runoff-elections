# ---------------------------------------------------------------------------------------
# INSTANT RUNOFF FORECASTING
# Peter Selb, Romain Lachat, and Sascha Göbel
# R script written by Sascha Göbel and Peter Selb
# Forecasting script
# July 2017
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
c("./data/resp_data_sc1_1", "./data/resp_data_sc1_2", "./data/resp_data_sc2_1",
  "./data/resp_data_sc2_2", "./data/cand_data")

# exports -------------------------------------------------------------------------------
c("./data/forecast-results/*")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("TOP LEVEL DIRECTORY")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")

# read data from disk -------------------------------------------------------------------
resp_data_sc1_1 <- readRDS("./data/resp_data_sc1_1")
resp_data_sc1_2 <- readRDS("./data/resp_data_sc1_2")
resp_data_sc2_1 <- readRDS("./data/resp_data_sc2_1")
resp_data_sc2_2 <- readRDS("./data/resp_data_sc2_2")
cand_data <- readRDS("./data/cand_data")


#### FORECAST - SCENARIO 1.1 ============================================================

# build complex survey design object ----------------------------------------------------
ppsdesign_sc1_1 <- svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = 
                            resp_data_sc1_1)
                 # specify multistage PPS survey design

# adjust survey design via poststratification -------------------------------------------
ppsdesign_post_sc1_1 <- postStratify(design = ppsdesign_sc1_1, strata = ~choice, 
                                   population = cand_data)

# compute vote totals for runoff candidates ---------------------------------------------
votes_sc1_1 <- svytotal(~choiceLePen + choiceMacron, design = ppsdesign_sc1_1, 
                      na.rm = TRUE, deff = TRUE)
             # compute runoff candidates' second round vote totals without 
             # poststratification
votes_total_sc1_1 <- votes_sc1_1[[1]] + votes_sc1_1[[2]]
                   # compute second round vote total over runoff candidates without
                   # poststratification
votes_sc1_1_post <- svytotal(~choiceLePen + choiceMacron, design = ppsdesign_post_sc1_1, 
                          na.rm = TRUE, deff = TRUE)
                  # compute runoff candidates' second round vote totals with 
                  # poststratification
votes_total_sc1_1_post <- votes_sc1_1_post[[1]] + votes_sc1_1_post[[2]]
                        # compute second round vote total over runoff candidates with
                        # poststratification

# compute vote shares for runoff candidates ---------------------------------------------
shares_sc1_1 <- svymean(~choiceLePen + choiceMacron + abstain, design = ppsdesign_sc1_1, 
                      na.rm = TRUE, deff = TRUE)
                  # compute runoff candidates' second round vote shares without 
                  # poststratification
shares_sc1_1_post <- svymean(~choiceLePen + choiceMacron + abstain, design = 
                         ppsdesign_post_sc1_1, na.rm = TRUE, deff = TRUE) %>%
                   # compute runoff candidates' second round vote shares without 
                   # poststratification
  data.frame(., ci90bot = c(confint(., level = 0.90)[1,1],
                            confint(., level = 0.90)[2,1],
                            NA),
             ci90top = c(confint(., level = 0.90)[1,2],
                         confint(., level = 0.90)[2,2],
                         NA),
             ci95bot = c(confint(., level = 0.95)[1,1],
                         confint(., level = 0.95)[2,1],
                         NA),
             ci95top = c(confint(., level = 0.95)[1,2],
                         confint(., level = 0.95)[2,2],
                         NA),
             ci99bot = c(confint(., level = 0.99)[1,1],
                         confint(., level = 0.99)[2,1],
                         NA),
             ci99top = c(confint(., level = 0.99)[1,2],
                         confint(., level = 0.99)[2,2],
                         NA))

# compute runoff abstentions ------------------------------------------------------------
# includes invalid- and null-ballots, and first round minus second round valid votes 
abstentions_sc1_1 <- svytotal(~abstain, design = ppsdesign_sc1_1, na.rm = TRUE, 
                            deff = TRUE)
                   # compute second round abstentions without poststratification
abstentions_sc1_1_post <- svytotal(~abstain, design = ppsdesign_post_sc1_1, na.rm = TRUE, 
                                deff = TRUE)
                       # compute second round abstentions with poststratification

# compute runoff abstention share -------------------------------------------------------
# includes invalid- and null-ballots, and first round minus second round valid votes 
abstention_share_sc1_1 <- svymean(~abstain, design = ppsdesign_sc1_1, na.rm = TRUE, 
                                deff = TRUE)
                        # compute second round share of abstentions without
                        # poststratification
abstention_share_sc1_1_post <- svymean(~abstain, design = ppsdesign_post_sc1_1, na.rm = TRUE, 
                                     deff = TRUE)
                            # compute second round share of abstentions with
                            # poststratification

# compute vote streams to runoff candidates from eliminated first-round candidates ------
vote_stream_sc1_1 <- svyby(~choiceLePen + choiceMacron, ~choice, ppsdesign_sc1_1, svytotal,
                         na.rm = TRUE)
                   # compute second round vote streams without poststratification
vote_stream_sc1_1_post <- svyby(~choiceLePen + choiceMacron, ~choice, ppsdesign_post_sc1_1, 
                              svytotal, na.rm = TRUE)
                       # compute second round vote streams with poststratification

# compute abstention streams from eliminated first-round candidates ---------------------
abstention_stream_sc1_1 <- svyby(~abstain, ~choice, ppsdesign_sc1_1, svytotal,
                               na.rm = TRUE)
                         # compute second round abstention streams without 
                         # poststratification 
abstention_stream_sc1_1_post <- svyby(~abstain, ~choice, ppsdesign_post_sc1_1, 
                                    svytotal, na.rm = TRUE)
                             # compute second round abstention streams with
                             # poststratification

# save forecast -------------------------------------------------------------------------
saveRDS(votes_sc1_1, "./data/forecast-results/votes_sc1_1")
saveRDS(votes_sc1_1_post, "./data/forecast-results/votes_sc1_1_post")
saveRDS(votes_total_sc1_1, "./data/forecast-results/votes_total_sc1_1")
saveRDS(votes_total_sc1_1_post, "./data/forecast-results/votes_total_sc1_1_post")
saveRDS(shares_sc1_1, "./data/forecast-results/shares_sc1_1")
saveRDS(shares_sc1_1_post, "./data/forecast-results/shares_sc1_1_post")
saveRDS(abstentions_sc1_1, "./data/forecast-results/abstentions_sc1_1")
saveRDS(abstentions_sc1_1_post, "./data/forecast-results/abstentions_sc1_1_post")
saveRDS(abstention_share_sc1_1, "./data/forecast-results/abstention_share_sc1_1")
saveRDS(abstention_share_sc1_1_post, "./data/forecast-results/abstention_share_sc1_1_post")
saveRDS(vote_stream_sc1_1, "./data/forecast-results/vote_stream_sc1_1")
saveRDS(vote_stream_sc1_1_post, "./data/forecast-results/vote_stream_sc1_1_post")
saveRDS(abstention_stream_sc1_1, "./data/forecast-results/abstention_stream_sc1_1")
saveRDS(abstention_stream_sc1_1_post, "./data/forecast-results/abstention_stream_sc1_1_post")


#### FORECAST - SCENARIO 1.2 ============================================================

# build complex survey design object ----------------------------------------------------
ppsdesign_sc1_2 <- svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = 
                               resp_data_sc1_2)
                   # specify multistage PPS survey design

# adjust survey design via poststratification -------------------------------------------
ppsdesign_post_sc1_2 <- postStratify(design = ppsdesign_sc1_2, strata = ~choice, 
                                     population = cand_data)

# compute vote totals for runoff candidates ---------------------------------------------
votes_sc1_2 <- svytotal(~choiceLePen + choiceMacron, design = ppsdesign_sc1_2, 
                        na.rm = TRUE, deff = TRUE)
               # compute runoff candidates' second round vote totals without 
               # poststratification
votes_total_sc1_2 <- votes_sc1_2[[1]] + votes_sc1_2[[2]]
                     # compute second round vote total over runoff candidates without
                     # poststratification
votes_sc1_2_post <- svytotal(~choiceLePen + choiceMacron, design = ppsdesign_post_sc1_2, 
                             na.rm = TRUE, deff = TRUE) %>%
                    # compute runoff candidates' second round vote totals with 
                    # poststratification
  data.frame(., ci90bot = c(confint(., level = 0.90)[1,1],
                            confint(., level = 0.90)[2,1]),
             ci90top = c(confint(., level = 0.90)[1,2],
                         confint(., level = 0.90)[2,2]),
             ci95bot = c(confint(., level = 0.95)[1,1],
                         confint(., level = 0.95)[2,1]),
             ci95top = c(confint(., level = 0.95)[1,2],
                         confint(., level = 0.95)[2,2]),
             ci99bot = c(confint(., level = 0.99)[1,1],
                         confint(., level = 0.99)[2,1]),
             ci99top = c(confint(., level = 0.99)[1,2],
                         confint(., level = 0.99)[2,2]))
votes_total_sc1_2_post <- votes_sc1_2_post[[1]] + votes_sc1_2_post[[2]]
                          # compute second round vote total over runoff candidates with
                          # poststratification

# compute vote shares for runoff candidates ---------------------------------------------
shares_sc1_2 <- svymean(~choiceLePen + choiceMacron + abstain, design = ppsdesign_sc1_2, 
                        na.rm = TRUE, deff = TRUE)
                # compute runoff candidates' second round vote shares without 
                # poststratification
shares_sc1_2_post <- svymean(~choiceLePen + choiceMacron + abstain, design = 
                               ppsdesign_post_sc1_2, na.rm = TRUE, deff = TRUE) %>%
                     # compute runoff candidates' second round vote shares without 
                     # poststratification
  data.frame(., ci90bot = c(confint(., level = 0.90)[1,1],
                            confint(., level = 0.90)[2,1],
                            NA),
             ci90top = c(confint(., level = 0.90)[1,2],
                         confint(., level = 0.90)[2,2],
                         NA),
             ci95bot = c(confint(., level = 0.95)[1,1],
                         confint(., level = 0.95)[2,1],
                         NA),
             ci95top = c(confint(., level = 0.95)[1,2],
                         confint(., level = 0.95)[2,2],
                         NA),
             ci99bot = c(confint(., level = 0.99)[1,1],
                         confint(., level = 0.99)[2,1],
                         NA),
             ci99top = c(confint(., level = 0.99)[1,2],
                         confint(., level = 0.99)[2,2],
                         NA))

# compute runoff abstentions ------------------------------------------------------------
# includes invalid- and null-ballots, and first round minus second round valid votes 
abstentions_sc1_2 <- svytotal(~abstain, design = ppsdesign_sc1_2, na.rm = TRUE, 
                              deff = TRUE)
                     # compute second round abstentions without poststratification
abstentions_sc1_2_post <- svytotal(~abstain, design = ppsdesign_post_sc1_2, na.rm = TRUE, 
                                   deff = TRUE) %>%
                          # compute second round abstentions with poststratification
  data.frame(., ci90bot = c(confint(., level = 0.90)[1,1]),
             ci90top = c(confint(., level = 0.90)[1,2]),
             ci95bot = c(confint(., level = 0.95)[1,1]),
             ci95top = c(confint(., level = 0.95)[1,2]),
             ci99bot = c(confint(., level = 0.99)[1,1]),
             ci99top = c(confint(., level = 0.99)[1,2]))

# compute runoff abstention share -------------------------------------------------------
# includes invalid- and null-ballots, and first round minus second round valid votes 
abstention_share_sc1_2 <- svymean(~abstain, design = ppsdesign_sc1_2, na.rm = TRUE, 
                                  deff = TRUE)
                          # compute second round share of abstentions without
                          # poststratification
abstention_share_sc1_2_post <- svymean(~abstain, design = ppsdesign_post_sc1_2, na.rm = TRUE, 
                                       deff = TRUE)
                          # compute second round share of abstentions with
                          # poststratification

# compute vote streams to runoff candidates from eliminated first-round candidates ------
vote_stream_sc1_2 <- svyby(~choiceLePen + choiceMacron, ~choice, ppsdesign_sc1_2, svytotal,
                           na.rm = TRUE)
                     # compute second round vote streams without poststratification
vote_stream_sc1_2_post <- svyby(~choiceLePen + choiceMacron, ~choice, ppsdesign_post_sc1_2, 
                                svytotal, na.rm = TRUE)
                          # compute second round vote streams with poststratification

# compute abstention streams from eliminated first-round candidates ---------------------
abstention_stream_sc1_2 <- svyby(~abstain, ~choice, ppsdesign_sc1_2, svytotal,
                                 na.rm = TRUE)
                           # compute second round abstention streams without 
                           # poststratification 
abstention_stream_sc1_2_post <- svyby(~abstain, ~choice, ppsdesign_post_sc1_2, 
                                      svytotal, na.rm = TRUE)
                                # compute second round abstention streams with
                                # poststratification

# save forecast -------------------------------------------------------------------------
saveRDS(votes_sc1_2, "./data/forecast-results/votes_sc1_2")
saveRDS(votes_sc1_2_post, "./data/forecast-results/votes_sc1_2_post")
saveRDS(votes_total_sc1_2, "./data/forecast-results/votes_total_sc1_2")
saveRDS(votes_total_sc1_2_post, "./data/forecast-results/votes_total_sc1_2_post")
saveRDS(shares_sc1_2, "./data/forecast-results/shares_sc1_2")
saveRDS(shares_sc1_2_post, "./data/forecast-results/shares_sc1_2_post")
saveRDS(abstentions_sc1_2, "./data/forecast-results/abstentions_sc1_2")
saveRDS(abstentions_sc1_2_post, "./data/forecast-results/abstentions_sc1_2_post")
saveRDS(abstention_share_sc1_2, "./data/forecast-results/abstention_share_sc1_2")
saveRDS(abstention_share_sc1_2_post, "./data/forecast-results/abstention_share_sc1_2_post")
saveRDS(vote_stream_sc1_2, "./data/forecast-results/vote_stream_sc1_2")
saveRDS(vote_stream_sc1_2_post, "./data/forecast-results/vote_stream_sc1_2_post")
saveRDS(abstention_stream_sc1_2, "./data/forecast-results/abstention_stream_sc1_2")
saveRDS(abstention_stream_sc1_2_post, "./data/forecast-results/abstention_stream_sc1_2_post")


#### FORECAST - SCENARIO 2.1 ============================================================

# build complex survey design object ----------------------------------------------------
ppsdesign_sc2_1 <- split(resp_data_sc2_1, f = resp_data_sc2_1$.imp) %>%
             # split resp_data_sc2 into a list of imputed data frames
  lapply(function(x) {
    svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = x)
  })
  # specify list of multistage PPS survey designs

# adjust list of survey designs via poststratification ----------------------------------
ppsdesign_post_sc2_1 <- lapply(ppsdesign_sc2_1, function(x) {
  postStratify(design = x, strata = ~choice, population = cand_data)
  })

# compute vote totals for runoff candidates ---------------------------------------------
votes_sc2_1 <- lapply(ppsdesign_sc2_1, function(x) {
  svytotal(~choiceLePen + choiceMacron, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute runoff candidates' second round vote totals without poststratification from 
  # each survey design object in the list
votes_sc2_1 <- rbind(
  data.frame(
    votes = mean(unlist(lapply(votes_sc2_1, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(votes_sc2_1, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(votes_sc2_1, function(x) data.frame(x)[1,3])))),
  data.frame(
    votes = mean(unlist(lapply(votes_sc2_1, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(votes_sc2_1, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(votes_sc2_1, function(x) data.frame(x)[2,3]))))
  ) %>%
  # take the mean of votes, ses and deffs over the list of computed vote totals
  # and store in data frame
  set_rownames(c("Le Pen", "Macron"))
  # assign row names
votes_total_sc2_1 <- votes_sc2_1[1,1] + votes_sc2_1[2,1]
                       # compute second round vote total over runoff candidates without
                       # poststratification
votes_sc2_1_post <- lapply(ppsdesign_post_sc2_1, function(x) {
  svytotal(~choiceLePen + choiceMacron, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute runoff candidates' second round vote totals with poststratification from 
  # each survey design object in the list
votes_sc2_1_post <- rbind(
  data.frame(
  votes = mean(unlist(lapply(votes_sc2_1_post, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(votes_sc2_1_post, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(votes_sc2_1_post, function(x) data.frame(x)[1,3])))),
  data.frame(
    votes = mean(unlist(lapply(votes_sc2_1_post, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(votes_sc2_1_post, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(votes_sc2_1_post, function(x) data.frame(x)[2,3]))))
  ) %>%
  # take the mean of votes, ses and deffs over the list of computed vote totals
  # and store in data frame
  set_rownames(c("Le Pen", "Macron"))
  # assign row names
votes_total_sc2_1_post <- votes_sc2_1_post[1,1] + votes_sc2_1_post[2,1]
                        # compute second round vote total over runoff candidates with
                        # poststratification

# compute runoff shares for runoff candidates -------------------------------------------
shares_sc2_1 <- lapply(ppsdesign_sc2_1, function(x) {
  svymean(~choiceLePen + choiceMacron + abstain, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute runoff candidates' second round vote shares without poststratification from 
  # each survey design object in the list
shares_sc2_1 <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_sc2_1, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_sc2_1, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_sc2_1, function(x) data.frame(x)[1,3])))),
  data.frame(
    votes = mean(unlist(lapply(shares_sc2_1, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_sc2_1, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_sc2_1, function(x) data.frame(x)[2,3]))))
  ) %>%
  # take the mean of votes, ses and deffs over the list of computed vote totals
  # and store in data frame
  set_rownames(c("Le Pen", "Macron"))
  # assign row names
shares_sc2_1_post <- lapply(ppsdesign_post_sc2_1, function(x) {
  svymean(~choiceLePen + choiceMacron + abstain, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute runoff candidates' second round vote shares with poststratification from 
  # each survey design object in the list
shares_sc2_1_post <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_sc2_1_post, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_sc2_1_post, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_sc2_1_post, function(x) data.frame(x)[1,3])))),
  data.frame(
    votes = mean(unlist(lapply(shares_sc2_1_post, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_sc2_1_post, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_sc2_1_post, function(x) data.frame(x)[2,3]))))
  ) %>%
  # take the mean of votes, ses and deffs over the list of computed vote totals
  # and store in data frame
  set_rownames(c("Le Pen", "Macron"))
  # assign row names

# compute runoff abstentions ------------------------------------------------------------
# includes invalid- and null-ballots, and first round minus second round valid votes 
abstentions_sc2_1 <- lapply(ppsdesign_sc2_1, function(x) {
  svytotal(~abstain, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute second round abstentions without poststratification from each survey design 
  # object in the list
abstentions_sc2_1 <- data.frame(
  abstentions = mean(unlist(lapply(abstentions_sc2_1, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstentions_sc2_1, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstentions_sc2_1, function(x) data.frame(x)[1,3])))
  )
  # take the mean of abstentions, ses and deffs over the list of computed abstentions
  # and store in data frame
abstentions_sc2_1_post <- lapply(ppsdesign_post_sc2_1, function(x) {
  svytotal(~abstain, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute second round abstentions with poststratification from each survey design 
  # object in the list
abstentions_sc2_1_post <- data.frame(
  abstentions = mean(unlist(lapply(abstentions_sc2_1_post, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstentions_sc2_1_post, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstentions_sc2_1_post, function(x) data.frame(x)[1,3])))
  )
  # take the mean of abstentions, ses and deffs over the list of computed abstentions
  # and store in data frame

# compute runoff abstention share -------------------------------------------------------
# includes invalid- and null-ballots, and first round minus second round valid votes 
abstention_share_sc2_1 <- lapply(ppsdesign_sc2_1, function(x) {
  svymean(~abstain, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute second round share of abstentions without poststratification from each survey 
  # design object in the list
abstention_share_sc2_1 <- data.frame(
  abstentions = mean(unlist(lapply(abstention_share_sc2_1, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstention_share_sc2_1, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstention_share_sc2_1, function(x) data.frame(x)[1,3])))
  )
  # take the mean of abstention shares, ses and deffs over the list of computed 
  # abstention shares and store in data frame
abstention_share_sc2_1_post <- lapply(ppsdesign_post_sc2_1, function(x) {
  svymean(~abstain, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute second round share of abstentions with poststratification from each survey 
  # design object in the list
abstention_share_sc2_1_post <- data.frame(
  abstentions = mean(unlist(lapply(abstention_share_sc2_1_post, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstention_share_sc2_1_post, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstention_share_sc2_1_post, function(x) data.frame(x)[1,3])))
  )
  # take the mean of abstention shares, ses and deffs over the list of computed 
  # abstention shares and store in data frame

# compute vote streams to runoff candidates from eliminated first-round candidates ------
vote_stream_sc2_1 <- lapply(ppsdesign_sc2_1, function(x) {
  svyby(~choiceLePen + choiceMacron, ~choice, x, svytotal,
        na.rm = TRUE)
  })
  # compute second round vote streams without poststratification from each survey 
  # design object in the list
vote_stream_sc2_1 <- data.frame(
  choice = c(1:11),
  choiceLePen = c(mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[1,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[2,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[3,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[4,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[5,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[6,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[7,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[8,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[9,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[10,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[11,2])))),
  choiceMacron = c(mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[1,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[2,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[3,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[4,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[5,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[6,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[7,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[8,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[9,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[10,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[11,3])))),
  se.choiceLePen = c(mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[1,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[2,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[3,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[4,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[5,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[6,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[7,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[8,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[9,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[10,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[11,4])))),
  se.choiceMacron = c(mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[1,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[2,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[3,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[4,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[5,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[6,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[7,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[8,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[9,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[10,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1, function(x) data.frame(x)[11,5]))))
  )
  # take the mean of second round vote streams and ses over the list of computed vote streams and   
  # store in data frame
vote_stream_sc2_1_post <- lapply(ppsdesign_post_sc2_1, function(x) {
  svyby(~choiceLePen + choiceMacron, ~choice, x, svytotal,
        na.rm = TRUE)
  })
# compute second round vote streams with poststratification from each survey 
# design object in the list
vote_stream_sc2_1_post <- data.frame(
  choice = c(1:11),
  choiceLePen = c(mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[1,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[2,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[3,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[4,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[5,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[6,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[7,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[8,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[9,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[10,2]))),
                  mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[11,2])))),
  choiceMacron = c(mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[1,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[2,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[3,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[4,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[5,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[6,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[7,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[8,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[9,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[10,3]))),
                   mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[11,3])))),
  se.choiceLePen = c(mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[1,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[2,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[3,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[4,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[5,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[6,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[7,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[8,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[9,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[10,4]))),
                     mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[11,4])))),
  se.choiceMacron = c(mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[1,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[2,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[3,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[4,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[5,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[6,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[7,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[8,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[9,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[10,5]))),
                      mean(unlist(lapply(vote_stream_sc2_1_post, function(x) data.frame(x)[11,5]))))
  )
  # take the mean of second round vote streams and ses over the list of computed vote streams and   
  # store in data frame

# compute abstention streams from eliminated first-round candidates ---------------------
abstention_stream_sc2_1 <- lapply(ppsdesign_sc2_1, function(x) {
  svyby(~abstain, ~choice, x, svytotal,
        na.rm = TRUE)
  })
  # compute second round abstention streams without poststratification from each survey 
  # design object in the list
abstention_stream_sc2_1 <- data.frame(
  choice = c(1:11),
  abstain = c(mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[1,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[2,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[3,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[4,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[5,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[6,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[7,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[8,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[9,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[10,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[11,2])))),
  se = c(mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[1,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[2,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[3,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[4,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[5,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[6,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[7,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[8,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[9,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[10,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1, function(x) data.frame(x)[11,3]))))
  )
  # take the mean of second round abstention streams, ses and deffs over the list of computed 
  # vote streams and store in data frame
abstention_stream_sc2_1_post <- lapply(ppsdesign_post_sc2_1, function(x) {
  svyby(~abstain, ~choice, x, svytotal,
        na.rm = TRUE)
})
# compute second round abstention streams with poststratification from each survey 
# design object in the list
abstention_stream_sc2_1_post <- data.frame(
  choice = c(1:11),
  abstain = c(mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[1,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[2,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[3,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[4,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[5,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[6,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[7,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[8,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[9,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[10,2]))),
              mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[11,2])))),
  se = c(mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[1,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[2,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[3,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[4,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[5,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[6,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[7,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[8,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[9,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[10,3]))),
         mean(unlist(lapply(abstention_stream_sc2_1_post, function(x) data.frame(x)[11,3]))))
  )
  # take the mean of second round abstention streams, ses and deffs over the list of computed 
  # vote streams and store in data frame

# save forecast -------------------------------------------------------------------------
saveRDS(votes_sc2_1, "./data/forecast-results/votes_sc2_1")
saveRDS(votes_sc2_1_post, "./data/forecast-results/votes_sc2_1_post")
saveRDS(votes_total_sc2_1, "./data/forecast-results/votes_total_sc2_1")
saveRDS(votes_total_sc2_1_post, "./data/forecast-results/votes_total_sc2_1_post")
saveRDS(shares_sc2_1, "./data/forecast-results/shares_sc2_1")
saveRDS(shares_sc2_1_post, "./data/forecast-results/shares_sc2_1_post")
saveRDS(abstentions_sc2_1, "./data/forecast-results/abstentions_sc2_1")
saveRDS(abstentions_sc2_1_post, "./data/forecast-results/abstentions_sc2_1_post")
saveRDS(abstention_share_sc2_1, "./data/forecast-results/abstention_share_sc2_1")
saveRDS(abstention_share_sc2_1_post, "./data/forecast-results/abstention_share_sc2_1_post")
saveRDS(vote_stream_sc2_1, "./data/forecast-results/vote_stream_sc2_1")
saveRDS(vote_stream_sc2_1_post, "./data/forecast-results/vote_stream_sc2_1_post")
saveRDS(abstention_stream_sc2_1, "./data/forecast-results/abstention_stream_sc2_1")
saveRDS(abstention_stream_sc2_1_post, "./data/forecast-results/abstention_stream_sc2_1_post")


#### FORECAST - SCENARIO 2.2 ============================================================

# build complex survey design object ----------------------------------------------------
ppsdesign_sc2_2 <- split(resp_data_sc2_2, f = resp_data_sc2_2$.imp) %>%
  # split resp_data_sc2 into a list of imputed data frames
  lapply(function(x) {
    svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = x)
  })
  # specify list of multistage PPS survey designs

# adjust list of survey designs via poststratification ----------------------------------
ppsdesign_post_sc2_2 <- lapply(ppsdesign_sc2_2, function(x) {
  postStratify(design = x, strata = ~choice, population = cand_data)
  })

# compute vote totals for runoff candidates ---------------------------------------------
votes_sc2_2 <- lapply(ppsdesign_sc2_2, function(x) {
  svytotal(~choiceLePen + choiceMacron, design = x, na.rm = TRUE, deff = TRUE)
  })
votes_sc2_2 <- rbind(
  data.frame(
    votes = mean(unlist(lapply(votes_sc2_2, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(votes_sc2_2, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(votes_sc2_2, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(votes_sc2_2, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(votes_sc2_2, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(votes_sc2_2, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(votes_sc2_2, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(votes_sc2_2, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(votes_sc2_2, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(votes_sc2_2, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(votes_sc2_2, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(votes_sc2_2, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(votes_sc2_2, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(votes_sc2_2, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(votes_sc2_2, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(votes_sc2_2, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(votes_sc2_2, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(votes_sc2_2, function(x) confint(x, level = 0.99)[2,2]))))
  ) %>%
  # take the mean of votes, ses and deffs over the list of computed vote totals
  # and store in data frame
  set_rownames(c("Le Pen", "Macron"))
  # assign row names
votes_total_sc2_2 <- votes_sc2_2[1,1] + votes_sc2_2[2,1]
                     # compute second round vote total over runoff candidates without
                     # poststratification
votes_sc2_2_post <- lapply(ppsdesign_post_sc2_2, function(x) {
  svytotal(~choiceLePen + choiceMacron, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute runoff candidates' second round vote totals with poststratification from 
  # each survey design object in the list
votes_sc2_2_post <- rbind(
  data.frame(
    votes = mean(unlist(lapply(votes_sc2_2_post, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(votes_sc2_2_post, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(votes_sc2_2_post, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(votes_sc2_2_post, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(votes_sc2_2_post, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(votes_sc2_2_post, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(votes_sc2_2_post, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(votes_sc2_2_post, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(votes_sc2_2_post, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(votes_sc2_2_post, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(votes_sc2_2_post, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(votes_sc2_2_post, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(votes_sc2_2_post, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(votes_sc2_2_post, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(votes_sc2_2_post, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(votes_sc2_2_post, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(votes_sc2_2_post, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(votes_sc2_2_post, function(x) confint(x, level = 0.99)[2,2]))))
  ) %>%
  # take the mean of votes, ses and deffs over the list of computed vote totals
  # and store in data frame
  set_rownames(c("Le Pen", "Macron"))
  # assign row names
votes_total_sc2_2_post <- votes_sc2_2_post[1,1] + votes_sc2_2_post[2,1]
  # compute second round vote total over runoff candidates with
  # poststratification

# compute runoff shares for runoff candidates -------------------------------------------
shares_sc2_2 <- lapply(ppsdesign_sc2_2, function(x) {
  svymean(~choiceLePen + choiceMacron + abstain, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute runoff candidates' second round vote shares without poststratification from 
  # each survey design object in the list
shares_sc2_2 <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_sc2_2, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_sc2_2, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_sc2_2, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(shares_sc2_2, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(shares_sc2_2, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(shares_sc2_2, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(shares_sc2_2, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(shares_sc2_2, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(shares_sc2_2, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(shares_sc2_2, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_sc2_2, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_sc2_2, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(shares_sc2_2, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(shares_sc2_2, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(shares_sc2_2, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(shares_sc2_2, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(shares_sc2_2, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(shares_sc2_2, function(x) confint(x, level = 0.99)[2,2]))))
  ) %>%
  # take the mean of votes, ses and deffs over the list of computed vote totals
  # and store in data frame
  set_rownames(c("Le Pen", "Macron"))
  # assign row names
shares_sc2_2_post <- lapply(ppsdesign_post_sc2_2, function(x) {
  svymean(~choiceLePen + choiceMacron + abstain, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute runoff candidates' second round vote shares with poststratification from 
  # each survey design object in the list
shares_sc2_2_post <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_sc2_2_post, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_sc2_2_post, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_sc2_2_post, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(shares_sc2_2_post, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(shares_sc2_2_post, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(shares_sc2_2_post, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(shares_sc2_2_post, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(shares_sc2_2_post, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(shares_sc2_2_post, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(shares_sc2_2_post, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_sc2_2_post, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_sc2_2_post, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(shares_sc2_2_post, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(shares_sc2_2_post, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(shares_sc2_2_post, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(shares_sc2_2_post, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(shares_sc2_2_post, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(shares_sc2_2_post, function(x) confint(x, level = 0.99)[2,2]))))
  ) %>%
  # take the mean of votes, ses and deffs over the list of computed vote totals
  # and store in data frame
  set_rownames(c("Le Pen", "Macron"))
  # assign row names

# compute runoff abstentions ------------------------------------------------------------
# includes invalid- and null-ballots, and first round minus second round valid votes 
abstentions_sc2_2 <- lapply(ppsdesign_sc2_2, function(x) {
  svytotal(~abstain, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute second round abstentions without poststratification from each survey design 
  # object in the list
abstentions_sc2_2 <- data.frame(
  abstentions = mean(unlist(lapply(abstentions_sc2_2, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstentions_sc2_2, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstentions_sc2_2, function(x) data.frame(x)[1,3])))
  )
  # take the mean of abstentions, ses and deffs over the list of computed abstentions
  # and store in data frame
abstentions_sc2_2_post <- lapply(ppsdesign_post_sc2_2, function(x) {
  svytotal(~abstain, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute second round abstentions with poststratification from each survey design 
  # object in the list
abstentions_sc2_2_post <- data.frame(
  abstentions = mean(unlist(lapply(abstentions_sc2_2_post, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstentions_sc2_2_post, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstentions_sc2_2_post, function(x) data.frame(x)[1,3]))),
  ci90bot = mean(unlist(lapply(abstentions_sc2_2_post, function(x) confint(x, level = 0.90)[1,1]))),
  ci90top = mean(unlist(lapply(abstentions_sc2_2_post, function(x) confint(x, level = 0.90)[1,2]))),
  ci95bot = mean(unlist(lapply(abstentions_sc2_2_post, function(x) confint(x, level = 0.95)[1,1]))),
  ci95top = mean(unlist(lapply(abstentions_sc2_2_post, function(x) confint(x, level = 0.95)[1,2]))),
  ci99bot = mean(unlist(lapply(abstentions_sc2_2_post, function(x) confint(x, level = 0.99)[1,1]))),
  ci99top = mean(unlist(lapply(abstentions_sc2_2_post, function(x) confint(x, level = 0.99)[1,2])))
  )
  # take the mean of abstentions, ses and deffs over the list of computed abstentions
  # and store in data frame

# compute runoff abstention share -------------------------------------------------------
# includes invalid- and null-ballots, and first round minus second round valid votes 
abstention_share_sc2_2 <- lapply(ppsdesign_sc2_2, function(x) {
  svymean(~abstain, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute second round share of abstentions without poststratification from each survey 
  # design object in the list
abstention_share_sc2_2 <- data.frame(
  abstentions = mean(unlist(lapply(abstention_share_sc2_2, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstention_share_sc2_2, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstention_share_sc2_2, function(x) data.frame(x)[1,3])))
  )
  # take the mean of abstention shares, ses and deffs over the list of computed 
  # abstention shares and store in data frame
abstention_share_sc2_2_post <- lapply(ppsdesign_post_sc2_2, function(x) {
  svymean(~abstain, design = x, na.rm = TRUE, deff = TRUE)
  })
  # compute second round share of abstentions with poststratification from each survey 
  # design object in the list
abstention_share_sc2_2_post <- data.frame(
  abstentions = mean(unlist(lapply(abstention_share_sc2_2_post, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstention_share_sc2_2_post, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstention_share_sc2_2_post, function(x) data.frame(x)[1,3])))
  )
  # take the mean of abstention shares, ses and deffs over the list of computed 
  # abstention shares and store in data frame

# compute vote streams to runoff candidates from eliminated first-round candidates ------
vote_stream_sc2_2 <- lapply(ppsdesign_sc2_2, function(x) {
  svyby(~choiceLePen + choiceMacron, ~choice, x, svytotal,
        na.rm = TRUE)
  })
# compute second round vote streams without poststratification from each survey 
# design object in the list
vote_stream_sc2_2 <- data.frame(
  choice = c(1:11),
  choiceLePen = c(mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[1,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[2,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[3,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[4,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[5,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[6,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[7,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[8,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[9,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[10,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[11,2])))),
  choiceMacron = c(mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[1,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[2,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[3,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[4,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[5,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[6,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[7,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[8,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[9,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[10,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[11,3])))),
  se.choiceLePen = c(mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[1,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[2,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[3,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[4,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[5,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[6,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[7,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[8,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[9,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[10,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[11,4])))),
  se.choiceMacron = c(mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[1,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[2,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[3,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[4,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[5,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[6,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[7,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[8,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[9,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[10,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2, function(x) data.frame(x)[11,5]))))
  )
  # take the mean of second round vote streams and ses over the list of computed vote streams and   
  # store in data frame
vote_stream_sc2_2_post <- lapply(ppsdesign_post_sc2_2, function(x) {
  svyby(~choiceLePen + choiceMacron, ~choice, x, svytotal,
        na.rm = TRUE)
})
# compute second round vote streams with poststratification from each survey 
# design object in the list
vote_stream_sc2_2_post <- data.frame(
  choice = c(1:11),
  choiceLePen = c(mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[1,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[2,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[3,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[4,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[5,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[6,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[7,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[8,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[9,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[10,2]))),
                  mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[11,2])))),
  choiceMacron = c(mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[1,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[2,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[3,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[4,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[5,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[6,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[7,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[8,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[9,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[10,3]))),
                   mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[11,3])))),
  se.choiceLePen = c(mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[1,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[2,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[3,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[4,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[5,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[6,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[7,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[8,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[9,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[10,4]))),
                     mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[11,4])))),
  se.choiceMacron = c(mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[1,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[2,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[3,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[4,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[5,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[6,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[7,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[8,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[9,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[10,5]))),
                      mean(unlist(lapply(vote_stream_sc2_2_post, function(x) data.frame(x)[11,5]))))
  )
  # take the mean of second round vote streams and ses over the list of computed vote streams and   
  # store in data frame

# compute abstention streams from eliminated first-round candidates ---------------------
abstention_stream_sc2_2 <- lapply(ppsdesign_sc2_2, function(x) {
  svyby(~abstain, ~choice, x, svytotal,
        na.rm = TRUE)
  })
  # compute second round abstention streams without poststratification from each survey 
  # design object in the list
abstention_stream_sc2_2 <- data.frame(
  choice = c(1:11),
  abstain = c(mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[1,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[2,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[3,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[4,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[5,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[6,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[7,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[8,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[9,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[10,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[11,2])))),
  se = c(mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[1,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[2,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[3,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[4,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[5,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[6,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[7,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[8,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[9,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[10,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2, function(x) data.frame(x)[11,3]))))
  )
  # take the mean of second round abstention streams, ses and deffs over the list of computed 
  # vote streams and store in data frame
abstention_stream_sc2_2_post <- lapply(ppsdesign_post_sc2_2, function(x) {
  svyby(~abstain, ~choice, x, svytotal,
        na.rm = TRUE)
  })
  # compute second round abstention streams with poststratification from each survey 
  # design object in the list
abstention_stream_sc2_2_post <- data.frame(
  choice = c(1:11),
  abstain = c(mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[1,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[2,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[3,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[4,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[5,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[6,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[7,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[8,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[9,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[10,2]))),
              mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[11,2])))),
  se = c(mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[1,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[2,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[3,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[4,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[5,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[6,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[7,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[8,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[9,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[10,3]))),
         mean(unlist(lapply(abstention_stream_sc2_2_post, function(x) data.frame(x)[11,3]))))
  )
  # take the mean of second round abstention streams, ses and deffs over the list of computed 
  # vote streams and store in data frame

# save forecast -------------------------------------------------------------------------
saveRDS(votes_sc2_2, "./data/forecast-results/votes_sc2_2")
saveRDS(votes_sc2_2_post, "./data/forecast-results/votes_sc2_2_post")
saveRDS(votes_total_sc2_2, "./data/forecast-results/votes_total_sc2_2")
saveRDS(votes_total_sc2_2_post, "./data/forecast-results/votes_total_sc2_2_post")
saveRDS(shares_sc2_2, "./data/forecast-results/shares_sc2_2")
saveRDS(shares_sc2_2_post, "./data/forecast-results/shares_sc2_2_post")
saveRDS(abstentions_sc2_2, "./data/forecast-results/abstentions_sc2_2")
saveRDS(abstentions_sc2_2_post, "./data/forecast-results/abstentions_sc2_2_post")
saveRDS(abstention_share_sc2_2, "./data/forecast-results/abstention_share_sc2_2")
saveRDS(abstention_share_sc2_2_post, "./data/forecast-results/abstention_share_sc2_2_post")
saveRDS(vote_stream_sc2_2, "./data/forecast-results/vote_stream_sc2_2")
saveRDS(vote_stream_sc2_2_post, "./data/forecast-results/vote_stream_sc2_2_post")
saveRDS(abstention_stream_sc2_2, "./data/forecast-results/abstention_stream_sc2_2")
saveRDS(abstention_stream_sc2_2_post, "./data/forecast-results/abstention_stream_sc2_2_post")