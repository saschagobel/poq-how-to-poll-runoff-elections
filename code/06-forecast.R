# ---------------------------------------------------------------------------------------
# HOW TO POLL RUNOFF ELECTIONS
# Peter Selb, Sascha Goebel, and Romain Lachat
# Script for forecast
# April 2020
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
  './data/resp_data_sc1
  './data/resp_data_sc2
  './data/cand_data'
  './data/emp_ph'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
  './data/forecast-results/*'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
  Line 32 - PREPARATIONS
  Line 50 - FORECAST - NO IMPUTATION
  Line 153 - FORECAST - WITH IMPUTATION
  Line 500 - FORECAST - NO IMPUTATION - USING EMPIRICAL INCLUSION PROBABILITIES
  Line 631 - FORECAST - WITH IMPUTATION - USING EMPIRICAL INCLUSION PROBABILITIES
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("poq-how-to-poll-runoff-elections")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")

# read data from disk -------------------------------------------------------------------
resp_data_sc1 <- readRDS("./data/resp_data_sc1")
resp_data_sc2 <- readRDS("./data/resp_data_sc2")
cand_data <- readRDS("./data/cand_data")
emp_ph  <- readRDS("./data/emp_ph")


#### FORECAST - NO IMPUTATION ===========================================================

# build complex survey design object ----------------------------------------------------
ppsdesign_sc1 <- svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = 
                           resp_data_sc1)
ppsdesign_sc1 <- as.svrepdesign(ppsdesign_sc1, type="JK1")

# adjust survey design via poststratification -------------------------------------------
ppsdesign_post_sc1 <- postStratify(design = ppsdesign_sc1, strata = ~choice, 
                                   population = cand_data)

# compute vote totals for runoff candidates ---------------------------------------------
votes_sc1 <- svytotal(~choiceLePen + choiceMacron, design = ppsdesign_sc1, 
                      na.rm = TRUE, deff = TRUE)
votes_total_sc1 <- votes_sc1[[1]] + votes_sc1[[2]]
votes_sc1_post <- svytotal(~choiceLePen + choiceMacron, design = ppsdesign_post_sc1, 
                             na.rm = TRUE, deff = TRUE) %>%
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
votes_total_sc1_post <- votes_sc1_post[[1]] + votes_sc1_post[[2]]

# compute vote shares for runoff candidates ---------------------------------------------
shares_sc1 <- svymean(~choiceLePen + choiceMacron + abstain, design = ppsdesign_sc1, 
                        na.rm = TRUE, deff = TRUE)
shares_sc1_post <- svymean(~choiceLePen + choiceMacron + abstain, design = 
                               ppsdesign_post_sc1, na.rm = TRUE, deff = TRUE) %>%
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
abstentions_sc1 <- svytotal(~abstain, design = ppsdesign_sc1, na.rm = TRUE, 
                              deff = TRUE)
abstentions_sc1_post <- svytotal(~abstain, design = ppsdesign_post_sc1, na.rm = TRUE, 
                                   deff = TRUE) %>%
  data.frame(., ci90bot = c(confint(., level = 0.90)[1,1]),
             ci90top = c(confint(., level = 0.90)[1,2]),
             ci95bot = c(confint(., level = 0.95)[1,1]),
             ci95top = c(confint(., level = 0.95)[1,2]),
             ci99bot = c(confint(., level = 0.99)[1,1]),
             ci99top = c(confint(., level = 0.99)[1,2]))

# compute runoff abstention share -------------------------------------------------------
abstention_share_sc1 <- svymean(~abstain, design = ppsdesign_sc1, na.rm = TRUE, 
                                  deff = TRUE)
abstention_share_sc1_post <- svymean(~abstain, design = ppsdesign_post_sc1, na.rm = TRUE, 
                                       deff = TRUE)

# compute vote streams to runoff candidates from eliminated first-round candidates ------
vote_stream_sc1 <- svyby(~choiceLePen + choiceMacron, ~choice, ppsdesign_sc1, svytotal,
                           na.rm = TRUE)
vote_stream_sc1_post <- svyby(~choiceLePen + choiceMacron, ~choice, ppsdesign_post_sc1, 
                                svytotal, na.rm = TRUE)

# compute abstention streams from eliminated first-round candidates ---------------------
abstention_stream_sc1 <- svyby(~abstain, ~choice, ppsdesign_sc1, svytotal,
                                 na.rm = TRUE)
abstention_stream_sc1_post <- svyby(~abstain, ~choice, ppsdesign_post_sc1, 
                                      svytotal, na.rm = TRUE)

# save forecast -------------------------------------------------------------------------
saveRDS(votes_sc1, "./data/forecast-results/votes_sc1")
saveRDS(votes_sc1_post, "./data/forecast-results/votes_sc1_post")
saveRDS(votes_total_sc1, "./data/forecast-results/votes_total_sc1")
saveRDS(votes_total_sc1_post, "./data/forecast-results/votes_total_sc1_post")
saveRDS(shares_sc1, "./data/forecast-results/shares_sc1") # Table 1 Survey Round 2
saveRDS(shares_sc1_post, "./data/forecast-results/shares_sc1_post") # Table 1 Poststratification Round 2
saveRDS(abstentions_sc1, "./data/forecast-results/abstentions_sc1")
saveRDS(abstentions_sc1_post, "./data/forecast-results/abstentions_sc1_post")
saveRDS(abstention_share_sc1, "./data/forecast-results/abstention_share_sc1")
saveRDS(abstention_share_sc1_post, "./data/forecast-results/abstention_share_sc1_post")
saveRDS(vote_stream_sc1, "./data/forecast-results/vote_stream_sc1")
saveRDS(vote_stream_sc1_post, "./data/forecast-results/vote_stream_sc1_post")
saveRDS(abstention_stream_sc1, "./data/forecast-results/abstention_stream_sc1")
saveRDS(abstention_stream_sc1_post, "./data/forecast-results/abstention_stream_sc1_post")


#### FORECAST - WITH IMPUTATION =========================================================

# build complex survey design object ----------------------------------------------------
ppsdesign_sc2 <- split(resp_data_sc2, f = resp_data_sc2$.imp) %>%
  lapply(function(x) {
    svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = x)
  })
ppsdesign_sc2 <- lapply(ppsdesign_sc2, as.svrepdesign, type = "JK1")

# adjust list of survey designs via poststratification ----------------------------------
ppsdesign_post_sc2 <- lapply(ppsdesign_sc2, function(x) {
  postStratify(design = x, strata = ~choice, population = cand_data)
})

# compute vote totals for runoff candidates ---------------------------------------------
votes_sc2 <- lapply(ppsdesign_sc2, function(x) {
  svytotal(~choiceLePen + choiceMacron, design = x, na.rm = TRUE, deff = TRUE)
})
votes_sc2 <- rbind(
  data.frame(
    votes = mean(unlist(lapply(votes_sc2, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(votes_sc2, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(votes_sc2, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(votes_sc2, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(votes_sc2, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(votes_sc2, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.99)[2,2]))))
) %>%
  set_rownames(c("Le Pen", "Macron"))
votes_total_sc2 <- votes_sc2[1,1] + votes_sc2[2,1]
votes_sc2_post <- lapply(ppsdesign_post_sc2, function(x) {
  svytotal(~choiceLePen + choiceMacron, design = x, na.rm = TRUE, deff = TRUE)
})
votes_sc2_post <- rbind(
  data.frame(
    votes = mean(unlist(lapply(votes_sc2_post, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(votes_sc2_post, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(votes_sc2_post, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(votes_sc2_post, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(votes_sc2_post, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(votes_sc2_post, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.99)[2,2]))))
) %>%
  set_rownames(c("Le Pen", "Macron"))
votes_total_sc2_post <- votes_sc2_post[1,1] + votes_sc2_post[2,1]

# compute runoff shares for runoff candidates -------------------------------------------
shares_sc2 <- lapply(ppsdesign_sc2, function(x) {
  svymean(~choiceLePen + choiceMacron + abstain, design = x, na.rm = TRUE, deff = TRUE)
})
shares_sc2 <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_sc2, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_sc2, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_sc2, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(shares_sc2, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_sc2, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_sc2, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.99)[2,2]))))
) %>%
  set_rownames(c("Le Pen", "Macron"))
shares_sc2_post <- lapply(ppsdesign_post_sc2, function(x) {
  svymean(~choiceLePen + choiceMacron + abstain, design = x, na.rm = TRUE, deff = TRUE)
})
shares_sc2_post <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_sc2_post, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_sc2_post, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_sc2_post, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(shares_sc2_post, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_sc2_post, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_sc2_post, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.99)[2,2]))))
) %>%
  set_rownames(c("Le Pen", "Macron"))

# compute runoff abstentions ------------------------------------------------------------
abstentions_sc2 <- lapply(ppsdesign_sc2, function(x) {
  svytotal(~abstain, design = x, na.rm = TRUE, deff = TRUE)
})
abstentions_sc2 <- data.frame(
  abstentions = mean(unlist(lapply(abstentions_sc2, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstentions_sc2, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstentions_sc2, function(x) data.frame(x)[1,3])))
)
abstentions_sc2_post <- lapply(ppsdesign_post_sc2, function(x) {
  svytotal(~abstain, design = x, na.rm = TRUE, deff = TRUE)
})
abstentions_sc2_post <- data.frame(
  abstentions = mean(unlist(lapply(abstentions_sc2_post, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstentions_sc2_post, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstentions_sc2_post, function(x) data.frame(x)[1,3]))),
  ci90bot = mean(unlist(lapply(abstentions_sc2_post, function(x) confint(x, level = 0.90)[1,1]))),
  ci90top = mean(unlist(lapply(abstentions_sc2_post, function(x) confint(x, level = 0.90)[1,2]))),
  ci95bot = mean(unlist(lapply(abstentions_sc2_post, function(x) confint(x, level = 0.95)[1,1]))),
  ci95top = mean(unlist(lapply(abstentions_sc2_post, function(x) confint(x, level = 0.95)[1,2]))),
  ci99bot = mean(unlist(lapply(abstentions_sc2_post, function(x) confint(x, level = 0.99)[1,1]))),
  ci99top = mean(unlist(lapply(abstentions_sc2_post, function(x) confint(x, level = 0.99)[1,2])))
)

# compute runoff abstention share -------------------------------------------------------
abstention_share_sc2 <- lapply(ppsdesign_sc2, function(x) {
  svymean(~abstain, design = x, na.rm = TRUE, deff = TRUE)
})
abstention_share_sc2 <- data.frame(
  abstentions = mean(unlist(lapply(abstention_share_sc2, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstention_share_sc2, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstention_share_sc2, function(x) data.frame(x)[1,3])))
)
abstention_share_sc2_post <- lapply(ppsdesign_post_sc2, function(x) {
  svymean(~abstain, design = x, na.rm = TRUE, deff = TRUE)
})
abstention_share_sc2_post <- data.frame(
  abstentions = mean(unlist(lapply(abstention_share_sc2_post, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstention_share_sc2_post, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstention_share_sc2_post, function(x) data.frame(x)[1,3])))
)

# compute vote streams to runoff candidates from eliminated first-round candidates ------
vote_stream_sc2 <- lapply(ppsdesign_sc2, function(x) {
  svyby(~choiceLePen + choiceMacron, ~choice, x, svytotal,
        na.rm = TRUE)
})
vote_stream_sc2 <- data.frame(
  choice = c(1:11),
  choiceLePen = c(mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[1,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[2,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[3,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[4,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[5,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[6,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[7,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[8,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[9,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[10,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[11,2])))),
  choiceMacron = c(mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[1,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[2,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[3,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[4,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[5,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[6,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[7,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[8,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[9,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[10,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[11,3])))),
  se.choiceLePen = c(mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[1,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[2,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[3,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[4,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[5,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[6,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[7,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[8,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[9,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[10,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[11,4])))),
  se.choiceMacron = c(mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[1,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[2,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[3,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[4,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[5,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[6,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[7,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[8,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[9,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[10,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[11,5]))))
)
vote_stream_sc2_post <- lapply(ppsdesign_post_sc2, function(x) {
  svyby(~choiceLePen + choiceMacron, ~choice, x, svytotal,
        na.rm = TRUE)
})
vote_stream_sc2_post <- data.frame(
  choice = c(1:11),
  choiceLePen = c(mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[1,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[2,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[3,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[4,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[5,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[6,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[7,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[8,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[9,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[10,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[11,2])))),
  choiceMacron = c(mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[1,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[2,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[3,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[4,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[5,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[6,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[7,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[8,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[9,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[10,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[11,3])))),
  se.choiceLePen = c(mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[1,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[2,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[3,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[4,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[5,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[6,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[7,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[8,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[9,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[10,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[11,4])))),
  se.choiceMacron = c(mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[1,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[2,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[3,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[4,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[5,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[6,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[7,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[8,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[9,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[10,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[11,5]))))
)

# compute abstention streams from eliminated first-round candidates ---------------------
abstention_stream_sc2 <- lapply(ppsdesign_sc2, function(x) {
  svyby(~abstain, ~choice, x, svytotal,
        na.rm = TRUE)
})
abstention_stream_sc2 <- data.frame(
  choice = c(1:11),
  abstain = c(mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[1,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[2,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[3,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[4,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[5,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[6,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[7,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[8,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[9,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[10,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[11,2])))),
  se = c(mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[1,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[2,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[3,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[4,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[5,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[6,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[7,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[8,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[9,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[10,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[11,3]))))
)
abstention_stream_sc2_post <- lapply(ppsdesign_post_sc2, function(x) {
  svyby(~abstain, ~choice, x, svytotal,
        na.rm = TRUE)
})
abstention_stream_sc2_post <- data.frame(
  choice = c(1:11),
  abstain = c(mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[1,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[2,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[3,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[4,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[5,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[6,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[7,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[8,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[9,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[10,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[11,2])))),
  se = c(mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[1,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[2,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[3,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[4,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[5,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[6,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[7,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[8,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[9,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[10,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[11,3]))))
)

# save forecast -------------------------------------------------------------------------
saveRDS(votes_sc2, "./data/forecast-results/votes_sc2")
saveRDS(votes_sc2_post, "./data/forecast-results/votes_sc2_post") # Table 2
saveRDS(votes_total_sc2, "./data/forecast-results/votes_total_sc2")
saveRDS(votes_total_sc2_post, "./data/forecast-results/votes_total_sc2_post") # Table 2
saveRDS(shares_sc2, "./data/forecast-results/shares_sc2")
saveRDS(shares_sc2_post, "./data/forecast-results/shares_sc2_post") # Table 1 Imputation round 2
saveRDS(abstentions_sc2, "./data/forecast-results/abstentions_sc2")
saveRDS(abstentions_sc2_post, "./data/forecast-results/abstentions_sc2_post") # Table 2
saveRDS(abstention_share_sc2, "./data/forecast-results/abstention_share_sc2")
saveRDS(abstention_share_sc2_post, "./data/forecast-results/abstention_share_sc2_post")
saveRDS(vote_stream_sc2, "./data/forecast-results/vote_stream_sc2")
saveRDS(vote_stream_sc2_post, "./data/forecast-results/vote_stream_sc2_post") # Table 2
saveRDS(abstention_stream_sc2, "./data/forecast-results/abstention_stream_sc2")
saveRDS(abstention_stream_sc2_post, "./data/forecast-results/abstention_stream_sc2_post") # Table 2


#### FORECAST - NO IMPUTATION - USING EMPIRICAL INCLUSION PROBABILITIES =================

# replace design with empirical inclusion probabilities
resp_data_sc1$first_stage <- ifelse(resp_data_sc1$id == "034Eygalieres", emp_ph$emp_ph[6],
                                     ifelse(resp_data_sc1$id == "035Aumale", emp_ph$emp_ph[15],
                                            ifelse(resp_data_sc1$id == "037Avignonet-Lauragais", emp_ph$emp_ph[1],
                                                   ifelse(resp_data_sc1$id == "042Bailleval", emp_ph$emp_ph[11], 
                                                          ifelse(resp_data_sc1$id == "048Montreuil", emp_ph$emp_ph[18],
                                                                 ifelse(resp_data_sc1$id == "051Noisy-le-Grand", emp_ph$emp_ph[19],
                                                                        ifelse(resp_data_sc1$id == "063Rueil-Malmaison", emp_ph$emp_ph[17],
                                                                               ifelse(resp_data_sc1$id == "078Charce-Saint-Ellier-sur-Aubance", emp_ph$emp_ph[7],
                                                                                      ifelse(resp_data_sc1$id == "081Cluses", emp_ph$emp_ph[14],
                                                                                             ifelse(resp_data_sc1$id == "103Salon-de-Provence", emp_ph$emp_ph[10],
                                                                                                    ifelse(resp_data_sc1$id == "116Castelginest", emp_ph$emp_ph[2],
                                                                                                           ifelse(resp_data_sc1$id == "123Lyon", emp_ph$emp_ph[13],
                                                                                                                  ifelse(resp_data_sc1$id == "137Toulon", emp_ph$emp_ph[16],
                                                                                                                         ifelse(resp_data_sc1$id == "228Valencay", emp_ph$emp_ph[5],
                                                                                                                                ifelse(resp_data_sc1$id == "318Pessac", emp_ph$emp_ph[4],
                                                                                                                                       ifelse(resp_data_sc1$id == "328Ludres", emp_ph$emp_ph[8],
                                                                                                                                              ifelse(resp_data_sc1$id == "353Tregastel", emp_ph$emp_ph[20],
                                                                                                                                                     ifelse(resp_data_sc1$id == "420Pinsaguel", emp_ph$emp_ph[3],
                                                                                                                                                            ifelse(resp_data_sc1$id == "486Tinchebray", emp_ph$emp_ph[12],
                                                                                                                                                                   ifelse(resp_data_sc1$id == "666Terville", emp_ph$emp_ph[9], NA
                                                                                                                                                                   ))))))))))))))))))))
resp_data_sc2$first_stage <- ifelse(resp_data_sc2$id == "034Eygalieres", emp_ph$emp_ph[6],
                                       ifelse(resp_data_sc2$id == "035Aumale", emp_ph$emp_ph[15],
                                              ifelse(resp_data_sc2$id == "037Avignonet-Lauragais", emp_ph$emp_ph[1],
                                                     ifelse(resp_data_sc2$id == "042Bailleval", emp_ph$emp_ph[11], 
                                                            ifelse(resp_data_sc2$id == "048Montreuil", emp_ph$emp_ph[18],
                                                                   ifelse(resp_data_sc2$id == "051Noisy-le-Grand", emp_ph$emp_ph[19],
                                                                          ifelse(resp_data_sc2$id == "063Rueil-Malmaison", emp_ph$emp_ph[17],
                                                                                 ifelse(resp_data_sc2$id == "078Charce-Saint-Ellier-sur-Aubance", emp_ph$emp_ph[7],
                                                                                        ifelse(resp_data_sc2$id == "081Cluses", emp_ph$emp_ph[14],
                                                                                               ifelse(resp_data_sc2$id == "103Salon-de-Provence", emp_ph$emp_ph[10],
                                                                                                      ifelse(resp_data_sc2$id == "116Castelginest", emp_ph$emp_ph[2],
                                                                                                             ifelse(resp_data_sc2$id == "123Lyon", emp_ph$emp_ph[13],
                                                                                                                    ifelse(resp_data_sc2$id == "137Toulon", emp_ph$emp_ph[16],
                                                                                                                           ifelse(resp_data_sc2$id == "228Valencay", emp_ph$emp_ph[5],
                                                                                                                                  ifelse(resp_data_sc2$id == "318Pessac", emp_ph$emp_ph[4],
                                                                                                                                         ifelse(resp_data_sc2$id == "328Ludres", emp_ph$emp_ph[8],
                                                                                                                                                ifelse(resp_data_sc2$id == "353Tregastel", emp_ph$emp_ph[20],
                                                                                                                                                       ifelse(resp_data_sc2$id == "420Pinsaguel", emp_ph$emp_ph[3],
                                                                                                                                                              ifelse(resp_data_sc2$id == "486Tinchebray", emp_ph$emp_ph[12],
                                                                                                                                                                     ifelse(resp_data_sc2$id == "666Terville", emp_ph$emp_ph[9], NA
                                                                                                                                                                     ))))))))))))))))))))

# build complex survey design object ----------------------------------------------------
ppsdesign_sc1 <- svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = 
                             resp_data_sc1)
ppsdesign_sc1 <- as.svrepdesign(ppsdesign_sc1, type="JK1")

# adjust survey design via poststratification -------------------------------------------
ppsdesign_post_sc1 <- postStratify(design = ppsdesign_sc1, strata = ~choice, 
                                   population = cand_data)

# compute vote totals for runoff candidates ---------------------------------------------
votes_sc1 <- svytotal(~choiceLePen + choiceMacron, design = ppsdesign_sc1, 
                      na.rm = TRUE, deff = TRUE)
votes_total_sc1 <- votes_sc1[[1]] + votes_sc1[[2]]
votes_sc1_post <- svytotal(~choiceLePen + choiceMacron, design = ppsdesign_post_sc1, 
                           na.rm = TRUE, deff = TRUE) %>%
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
votes_total_sc1_post <- votes_sc1_post[[1]] + votes_sc1_post[[2]]

# compute vote shares for runoff candidates (Table C3) ----------------------------------
shares_sc1 <- svymean(~choiceLePen + choiceMacron + abstain, design = ppsdesign_sc1, 
                      na.rm = TRUE, deff = TRUE)
shares_sc1_post <- svymean(~choiceLePen + choiceMacron + abstain, design = 
                             ppsdesign_post_sc1, na.rm = TRUE, deff = TRUE) %>%
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
abstentions_sc1 <- svytotal(~abstain, design = ppsdesign_sc1, na.rm = TRUE, 
                            deff = TRUE)
abstentions_sc1_post <- svytotal(~abstain, design = ppsdesign_post_sc1, na.rm = TRUE, 
                                 deff = TRUE) %>%
  data.frame(., ci90bot = c(confint(., level = 0.90)[1,1]),
             ci90top = c(confint(., level = 0.90)[1,2]),
             ci95bot = c(confint(., level = 0.95)[1,1]),
             ci95top = c(confint(., level = 0.95)[1,2]),
             ci99bot = c(confint(., level = 0.99)[1,1]),
             ci99top = c(confint(., level = 0.99)[1,2]))

# compute runoff abstention share -------------------------------------------------------
abstention_share_sc1 <- svymean(~abstain, design = ppsdesign_sc1, na.rm = TRUE, 
                                deff = TRUE)
abstention_share_sc1_post <- svymean(~abstain, design = ppsdesign_post_sc1, na.rm = TRUE, 
                                     deff = TRUE)

# compute vote streams to runoff candidates from eliminated first-round candidates ------
vote_stream_sc1 <- svyby(~choiceLePen + choiceMacron, ~choice, ppsdesign_sc1, svytotal,
                         na.rm = TRUE)
vote_stream_sc1_post <- svyby(~choiceLePen + choiceMacron, ~choice, ppsdesign_post_sc1, 
                              svytotal, na.rm = TRUE)

# compute abstention streams from eliminated first-round candidates ---------------------
abstention_stream_sc1 <- svyby(~abstain, ~choice, ppsdesign_sc1, svytotal,
                               na.rm = TRUE)
abstention_stream_sc1_post <- svyby(~abstain, ~choice, ppsdesign_post_sc1, 
                                    svytotal, na.rm = TRUE)


#### FORECAST - WITH IMPUTATION - USING EMPIRICAL INCLUSION PROBABILITIES ===============

# build complex survey design object ----------------------------------------------------
ppsdesign_sc2 <- split(resp_data_sc2, f = resp_data_sc2$.imp) %>%
  lapply(function(x) {
    svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = x)
  })
ppsdesign_sc2 <- lapply(ppsdesign_sc2, as.svrepdesign, type = "JK1")

# adjust list of survey designs via poststratification ----------------------------------
ppsdesign_post_sc2 <- lapply(ppsdesign_sc2, function(x) {
  postStratify(design = x, strata = ~choice, population = cand_data)
})

# compute vote totals for runoff candidates (Table C4) ----------------------------------
votes_sc2 <- lapply(ppsdesign_sc2, function(x) {
  svytotal(~choiceLePen + choiceMacron, design = x, na.rm = TRUE, deff = TRUE)
})
votes_sc2 <- rbind(
  data.frame(
    votes = mean(unlist(lapply(votes_sc2, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(votes_sc2, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(votes_sc2, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(votes_sc2, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(votes_sc2, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(votes_sc2, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(votes_sc2, function(x) confint(x, level = 0.99)[2,2]))))
) %>%
  set_rownames(c("Le Pen", "Macron"))
votes_total_sc2 <- votes_sc2[1,1] + votes_sc2[2,1]
votes_sc2_post <- lapply(ppsdesign_post_sc2, function(x) {
  svytotal(~choiceLePen + choiceMacron, design = x, na.rm = TRUE, deff = TRUE)
})
votes_sc2_post <- rbind(
  data.frame(
    votes = mean(unlist(lapply(votes_sc2_post, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(votes_sc2_post, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(votes_sc2_post, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(votes_sc2_post, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(votes_sc2_post, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(votes_sc2_post, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(votes_sc2_post, function(x) confint(x, level = 0.99)[2,2]))))
) %>%
  set_rownames(c("Le Pen", "Macron"))
votes_total_sc2_post <- votes_sc2_post[1,1] + votes_sc2_post[2,1]

# compute runoff shares for runoff candidates (Table C3) --------------------------------
shares_sc2 <- lapply(ppsdesign_sc2, function(x) {
  svymean(~choiceLePen + choiceMacron + abstain, design = x, na.rm = TRUE, deff = TRUE)
})
shares_sc2 <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_sc2, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_sc2, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_sc2, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(shares_sc2, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_sc2, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_sc2, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(shares_sc2, function(x) confint(x, level = 0.99)[2,2]))))
) %>%
  set_rownames(c("Le Pen", "Macron"))
shares_sc2_post <- lapply(ppsdesign_post_sc2, function(x) {
  svymean(~choiceLePen + choiceMacron + abstain, design = x, na.rm = TRUE, deff = TRUE)
})
shares_sc2_post <- rbind(
  data.frame(
    votes = mean(unlist(lapply(shares_sc2_post, function(x) data.frame(x)[1,1]))),
    se = mean(unlist(lapply(shares_sc2_post, function(x) data.frame(x)[1,2]))),
    deff = mean(unlist(lapply(shares_sc2_post, function(x) data.frame(x)[1,3]))),
    ci90bot = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.90)[1,1]))),
    ci90top = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.90)[1,2]))),
    ci95bot = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.95)[1,1]))),
    ci95top = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.95)[1,2]))),
    ci99bot = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.99)[1,1]))),
    ci99top = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.99)[1,2])))),
  data.frame(
    votes = mean(unlist(lapply(shares_sc2_post, function(x) data.frame(x)[2,1]))),
    se = mean(unlist(lapply(shares_sc2_post, function(x) data.frame(x)[2,2]))),
    deff = mean(unlist(lapply(shares_sc2_post, function(x) data.frame(x)[2,3]))),
    ci90bot = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.90)[2,1]))),
    ci90top = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.90)[2,2]))),
    ci95bot = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.95)[2,1]))),
    ci95top = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.95)[2,2]))),
    ci99bot = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.99)[2,1]))),
    ci99top = mean(unlist(lapply(shares_sc2_post, function(x) confint(x, level = 0.99)[2,2]))))
) %>%
  set_rownames(c("Le Pen", "Macron"))

# compute runoff abstentions (Table C4) -------------------------------------------------
abstentions_sc2 <- lapply(ppsdesign_sc2, function(x) {
  svytotal(~abstain, design = x, na.rm = TRUE, deff = TRUE)
})
abstentions_sc2 <- data.frame(
  abstentions = mean(unlist(lapply(abstentions_sc2, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstentions_sc2, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstentions_sc2, function(x) data.frame(x)[1,3])))
)
abstentions_sc2_post <- lapply(ppsdesign_post_sc2, function(x) {
  svytotal(~abstain, design = x, na.rm = TRUE, deff = TRUE)
})
abstentions_sc2_post <- data.frame(
  abstentions = mean(unlist(lapply(abstentions_sc2_post, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstentions_sc2_post, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstentions_sc2_post, function(x) data.frame(x)[1,3]))),
  ci90bot = mean(unlist(lapply(abstentions_sc2_post, function(x) confint(x, level = 0.90)[1,1]))),
  ci90top = mean(unlist(lapply(abstentions_sc2_post, function(x) confint(x, level = 0.90)[1,2]))),
  ci95bot = mean(unlist(lapply(abstentions_sc2_post, function(x) confint(x, level = 0.95)[1,1]))),
  ci95top = mean(unlist(lapply(abstentions_sc2_post, function(x) confint(x, level = 0.95)[1,2]))),
  ci99bot = mean(unlist(lapply(abstentions_sc2_post, function(x) confint(x, level = 0.99)[1,1]))),
  ci99top = mean(unlist(lapply(abstentions_sc2_post, function(x) confint(x, level = 0.99)[1,2])))
)

# compute runoff abstention share -------------------------------------------------------
abstention_share_sc2 <- lapply(ppsdesign_sc2, function(x) {
  svymean(~abstain, design = x, na.rm = TRUE, deff = TRUE)
})
abstention_share_sc2 <- data.frame(
  abstentions = mean(unlist(lapply(abstention_share_sc2, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstention_share_sc2, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstention_share_sc2, function(x) data.frame(x)[1,3])))
)
abstention_share_sc2_post <- lapply(ppsdesign_post_sc2, function(x) {
  svymean(~abstain, design = x, na.rm = TRUE, deff = TRUE)
})
abstention_share_sc2_post <- data.frame(
  abstentions = mean(unlist(lapply(abstention_share_sc2_post, function(x) data.frame(x)[1,1]))),
  se = mean(unlist(lapply(abstention_share_sc2_post, function(x) data.frame(x)[1,2]))),
  deff = mean(unlist(lapply(abstention_share_sc2_post, function(x) data.frame(x)[1,3])))
)

# compute vote streams to runoff candidates from eliminated first-round candidates ------
# (Table C4)
vote_stream_sc2 <- lapply(ppsdesign_sc2, function(x) {
  svyby(~choiceLePen + choiceMacron, ~choice, x, svytotal,
        na.rm = TRUE)
})
vote_stream_sc2 <- data.frame(
  choice = c(1:11),
  choiceLePen = c(mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[1,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[2,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[3,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[4,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[5,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[6,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[7,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[8,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[9,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[10,2]))),
                  mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[11,2])))),
  choiceMacron = c(mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[1,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[2,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[3,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[4,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[5,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[6,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[7,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[8,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[9,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[10,3]))),
                   mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[11,3])))),
  se.choiceLePen = c(mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[1,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[2,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[3,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[4,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[5,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[6,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[7,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[8,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[9,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[10,4]))),
                     mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[11,4])))),
  se.choiceMacron = c(mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[1,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[2,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[3,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[4,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[5,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[6,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[7,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[8,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[9,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[10,5]))),
                      mean(unlist(lapply(vote_stream_sc2, function(x) data.frame(x)[11,5]))))
)
vote_stream_sc2_post <- lapply(ppsdesign_post_sc2, function(x) {
  svyby(~choiceLePen + choiceMacron, ~choice, x, svytotal,
        na.rm = TRUE)
})
vote_stream_sc2_post <- data.frame(
  choice = c(1:11),
  choiceLePen = c(mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[1,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[2,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[3,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[4,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[5,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[6,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[7,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[8,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[9,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[10,2]))),
                  mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[11,2])))),
  choiceMacron = c(mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[1,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[2,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[3,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[4,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[5,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[6,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[7,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[8,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[9,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[10,3]))),
                   mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[11,3])))),
  se.choiceLePen = c(mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[1,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[2,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[3,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[4,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[5,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[6,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[7,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[8,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[9,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[10,4]))),
                     mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[11,4])))),
  se.choiceMacron = c(mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[1,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[2,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[3,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[4,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[5,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[6,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[7,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[8,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[9,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[10,5]))),
                      mean(unlist(lapply(vote_stream_sc2_post, function(x) data.frame(x)[11,5]))))
)

# compute abstention streams from eliminated first-round candidates (Table C4) ----------
abstention_stream_sc2 <- lapply(ppsdesign_sc2, function(x) {
  svyby(~abstain, ~choice, x, svytotal,
        na.rm = TRUE)
})
abstention_stream_sc2 <- data.frame(
  choice = c(1:11),
  abstain = c(mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[1,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[2,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[3,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[4,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[5,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[6,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[7,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[8,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[9,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[10,2]))),
              mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[11,2])))),
  se = c(mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[1,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[2,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[3,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[4,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[5,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[6,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[7,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[8,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[9,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[10,3]))),
         mean(unlist(lapply(abstention_stream_sc2, function(x) data.frame(x)[11,3]))))
)
abstention_stream_sc2_post <- lapply(ppsdesign_post_sc2, function(x) {
  svyby(~abstain, ~choice, x, svytotal,
        na.rm = TRUE)
})
abstention_stream_sc2_post <- data.frame(
  choice = c(1:11),
  abstain = c(mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[1,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[2,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[3,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[4,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[5,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[6,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[7,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[8,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[9,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[10,2]))),
              mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[11,2])))),
  se = c(mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[1,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[2,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[3,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[4,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[5,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[6,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[7,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[8,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[9,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[10,3]))),
         mean(unlist(lapply(abstention_stream_sc2_post, function(x) data.frame(x)[11,3]))))
)
