# ---------------------------------------------------------------------------------------
# INSTANT RUNOFF FORECASTING
# Peter Selb, Romain Lachat, and Sascha Göbel
# R script written by Sascha Göbel and Peter Selb
# Vote redistribution script
# July 2017
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
c("./data/resp_data")

# exports -------------------------------------------------------------------------------
c("./data/resp_data_sc1_1", "./data/resp_data_sc1_2", "./data/resp_data_sc2",
  "./data/resp_data_sc2_1", "./data/resp_data_sc2_2", "./data/resp_data_mock")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("TOP LEVEL DIRECTORY")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")

# read data from disk -------------------------------------------------------------------
resp_data <- readRDS("./data/resp_data")


#### VOTE REDISTRIBUTION - SCENARIO 1.1 (NOT INCLUDED IN THE PAPER) =====================
# assumption: no (campaign) learning, first-round voters for runoff candidates 
#             vote the same in the second round

# prepare lower-order preference and choice data for vote redistribution ----------------
resp_data_sc1_1 <- resp_data
                   # set data for scenario 1.1
preference_matrix <- resp_data[,4:14]
                     # build matrix of lower-order preferences
preference_matrix3 <- preference_matrix[,c(5,8)]
                      # select variables that identify lower-order preferences for runoff
                      # candidates
preference_matrix3[preference_matrix3 == 0] <- NA
                                               # replace 0, i.e., don't know, with NA
preference_matrix3$first <- max.col(replace(preference_matrix3, is.na(preference_matrix3), 
                                            -Inf), ties.method="random")
                            # add variable that identifies for each row/respondent the 
                            # column/candidate that was prefered most. NAs are replaced
                            # with -Inf. Ties are handled randomly.
preference_matrix3$two_maxima <- lengths(apply(preference_matrix3[,1:2],1,function(x) 
  which(x==max(x)))) > 1
                                 # add variable that identifies ties
preference_matrix3$first <- ifelse(is.na(preference_matrix3$Macron) & 
                                     is.na(preference_matrix3$LePen), NA, 
                                   preference_matrix3$first)
                            # if no lower-order preference is given for both of the 
                            # runoff candidates, make the choice NA (i.e., no 
                            # redistribution), otherwise substitute choice with prefered
                            # candidate
preference_matrix3$first <- ifelse(preference_matrix3$two_maxima == TRUE, NA, 
                                   preference_matrix3$first)
                            # if the observation exhibits a tie in lower-order preferences
                            # for the runoff candidates, make the choice NA (i.e., no
                            # redistribution as respondents are assumed to abstain), 
                            # otherwise substitute choice with prefered candidate
preference_matrix3$first <- ifelse(is.na(preference_matrix3$Macron) | 
                                     is.na(preference_matrix3$LePen), NA, 
                                   preference_matrix3$first)
                            # if no lower-order preference is given for one of the 
                            # runoff candidates, make the choice NA (i.e., no 
                            # redistribution because no comparison possible), otherwise 
                            # substitute choice with prefered candidate

# redistribute votes for runoff candidates ----------------------------------------------
resp_data_sc1_1$choiceMacron[preference_matrix3$first == 1] <- 1
  # if the respondent prefers Macron over LePen make choice 1
resp_data_sc1_1$choiceMacron[preference_matrix3$first == 2] <- 0
  # if the respondent prefers Le Pen over Macron make choice 0
resp_data_sc1_1$choiceMacron[resp_data_sc1_1$choice == 5] <- 1
  # if the respondent voted for Macron in the first round make choice 1
resp_data_sc1_1$choiceMacron[resp_data_sc1_1$choice == 8] <- 0
  # if the respondent voted for Le Pen in the first round make choice 0
resp_data_sc1_1$choiceLePen[preference_matrix3$first == 2] <- 1
  # if the respondent prefers LePen over Macron make choice 1
resp_data_sc1_1$choiceLePen[preference_matrix3$first == 1] <- 0
  # if the respondent prefers Macron over LePen make choice 0
resp_data_sc1_1$choiceLePen[resp_data_sc1_1$choice == 8] <- 1
  # if the respondent voted for LePen in the first round make choice 1
resp_data_sc1_1$choiceLePen[resp_data_sc1_1$choice == 5] <- 0
  # if the respondent voted for Macron in the first round make choice 0
resp_data_sc1_1 <- resp_data_sc1_1 %>% filter(!is.na(choice))
  # drop observations with choice NA
resp_data_sc1_1$abstain[is.na(resp_data_sc1_1$choiceMacron)] <- 1
resp_data_sc1_1$abstain[resp_data_sc1_1$choiceMacron == 1 | resp_data_sc1_1$choiceLePen == 1] <- 0
  # build variables that identifies abstentions

# wite data to disk ---------------------------------------------------------------------
saveRDS(resp_data_sc1_1, "./data/resp_data_sc1_1")


#### VOTE REDISTRIBUTION - SCENARIO 1.2 =================================================
# assumption: no (campaign) learning, first-round voters for runoff candidates vote 
#             in line with lower-order preferences in second round, if none, they 
#             vote the same in second round as in first round

# prepare lower-order preference and choice data for vote redistribution ----------------
resp_data_sc1_2 <- resp_data
                   # set data for scenario 1.2
  # preference matrices as in scenario 1.1

# redistribute votes for runoff candidates ----------------------------------------------
resp_data_sc1_2$choiceMacron[resp_data_sc1_2$choice == 5] <- 1
  # if the respondent voted for Macron in the first round make choice 1
resp_data_sc1_2$choiceMacron[resp_data_sc1_2$choice == 8] <- 0
  # if the respondent voted for Le Pen in the first round make choice 0
resp_data_sc1_2$choiceMacron[preference_matrix3$first == 1] <- 1
  # if the respondent prefers Macron over LePen make choice 1
resp_data_sc1_2$choiceMacron[preference_matrix3$first == 2] <- 0
  # if the respondent prefers Le Pen over Macron make choice 0
resp_data_sc1_2$choiceLePen[resp_data_sc1_2$choice == 8] <- 1
  # if the respondent voted for Le Pen in the first round make choice 1
resp_data_sc1_2$choiceLePen[resp_data_sc1_2$choice == 5] <- 0
  # if the respondent voted for Macron in the first round make choice 0
resp_data_sc1_2$choiceLePen[preference_matrix3$first == 2] <- 1
  # if the respondent prefers Le Pen over Macron make choice 1
resp_data_sc1_2$choiceLePen[preference_matrix3$first == 1] <- 0
  # if the respondent prefers Macron over LePen make choice 0
resp_data_sc1_2 <- resp_data_sc1_2 %>% filter(!is.na(choice))
  # drop observations with choice NA
resp_data_sc1_2$abstain[is.na(resp_data_sc1_2$choiceMacron)] <- 1
resp_data_sc1_2$abstain[resp_data_sc1_2$choiceMacron == 1 | resp_data_sc1_2$choiceLePen == 1] <- 0
  # build variables that identifies abstentions

# wite data to disk ---------------------------------------------------------------------
saveRDS(resp_data_sc1_2, "./data/resp_data_sc1_2")


#### VOTE REDISTRIBUTION - SCENARIO 2.1 (NOT INCLUDED IN THE PAPER) =====================
# assumption: with (campaign) learning, first-round voters for runoff candidates 
#             vote the same in second round

# prepare lower-order preference and choice data for vote redistribution ----------------
resp_data_sc2 <- resp_data
                 # set data for scenarios 2
resp_data_sc2[resp_data_sc2 == 0] <- NA
                                     # Transfom "don't knows" to NA in order to impute
resp_data_sc2[,4:14] <- lapply(resp_data_sc2[,4:14], factor, ordered = TRUE)
                        # transform lower-order preferences to factor variables
predictor_matrix <- matrix(data = c(1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0), 
                           nrow = ncol(resp_data), ncol = ncol(resp_data), 
                           dimnames = list(colnames(resp_data), colnames(resp_data)), 
                           byrow = TRUE)
                    # define predictor variables (1) for imputation
resp_data_sc2 <- mice(resp_data_sc2, m = 50, 
                      method = c("", "", "", "polr", "polr", "polr", "polr", "polr", 
                                 "polr", "polr", "polr", "polr", "polr", "polr", "", 
                                 "", "", "", "", ""),
                      predictorMatrix = predictor_matrix, maxit = 10) %>%
                 # impute missing lower-order preferences based on reported preferences,
                 # sex, age, and reported choices using ordered polytomous regression. 
                 # Only columns with "polr" are imputed, those with "" are not.
  complete(action = "long")
  # row bind imputed data sets and assign imputation index
resp_data_sc2[,6:16] <- lapply(resp_data_sc2[,6:16], as.integer)
                        # transform lower-order preferences back to integer
preference_matrix4 <- resp_data_sc2[,6:16]
                      # select variables that identify imputed lower-order preferences
preference_matrix5 <- preference_matrix4[,c(5,8)]
                      # select variables that identify lower-order preferences for runoff
                      # candidates
preference_matrix5[preference_matrix5 == 0] <- NA
                                               # replace 0, i.e., don't know, with NA
preference_matrix5$first <- max.col(replace(preference_matrix5, is.na(preference_matrix5), 
                                            -Inf), ties.method="random")
                            # add variable that identifies for each row/respondent the 
                            # column/candidate that was prefered most. NAs are replaced
                            # with -Inf. Ties are handled randomly.
preference_matrix5$two_maxima <- lengths(apply(preference_matrix5[,1:2],1,function(x) 
  which(x==max(x)))) > 1
                                 # add variable that identifies ties
preference_matrix5$first <- ifelse(is.na(preference_matrix5$Macron) & 
                                     is.na(preference_matrix5$LePen), NA, 
                                   preference_matrix5$first)
                            # if no lower-order preference is given for both of the 
                            # runoff candidates, make the choice NA (i.e., no 
                            # redistribution), otherwise substitute choice with prefered
                            # candidate
preference_matrix5$first <- ifelse(preference_matrix5$two_maxima == TRUE, NA, 
                                   preference_matrix5$first)
                            # if the observation exhibits a tie in lower-order preferences
                            # for the runoff candidates, make the choice NA (i.e., no
                            # redistribution as respondents are assumed to abstain), 
                            # otherwise substitute choice with prefered candidate
preference_matrix5$first <- ifelse(is.na(preference_matrix5$Macron) | 
                                     is.na(preference_matrix5$LePen), NA, 
                                   preference_matrix5$first)
                            # if no lower-order preference is given for one of the 
                            # runoff candidates, make the choice NA (i.e., no 
                            # redistribution because no comparison possible), otherwise 
                            # substitute choice with prefered candidate
resp_data_sc2_1 <- resp_data_sc2
                   # set data for scenario 2.1

# redistribute votes for runoff candidates ----------------------------------------------
resp_data_sc2_1$choiceMacron[preference_matrix5$first == 1] <- 1
  # if the respondent prefers Macron over LePen make choice 1
resp_data_sc2_1$choiceMacron[preference_matrix5$first == 2] <- 0
  # if the respondent prefers Le Pen over Macron make choice 0
resp_data_sc2_1$choiceMacron[resp_data_sc2_1$choice == 5] <- 1
  # if the respondent voted for Macron in the first round make choice 1
resp_data_sc2_1$choiceMacron[resp_data_sc2_1$choice == 8] <- 0
  # if the respondent voted for Le Pen in the first round make choice 0
resp_data_sc2_1$choiceLePen[preference_matrix5$first == 2] <- 1
  # if the respondent prefers LePen over Macron make choice 1
resp_data_sc2_1$choiceLePen[preference_matrix5$first == 1] <- 0
  # if the respondent prefers Macron over LePen make choice 0
resp_data_sc2_1$choiceLePen[resp_data_sc2_1$choice == 8] <- 1
  # if the respondent voted for LePen in the first round make choice 1
resp_data_sc2_1$choiceLePen[resp_data_sc2_1$choice == 5] <- 0
  # if the respondent voted for Macron in the first round make choice 0
resp_data_sc2_1 <- resp_data_sc2_1 %>% filter(!is.na(choice))
  # drop observations with choice NA
resp_data_sc2_1$abstain[is.na(resp_data_sc2_1$choiceMacron)] <- 1
resp_data_sc2_1$abstain[resp_data_sc2_1$choiceMacron == 1 | resp_data_sc2_1$choiceLePen == 1] <- 0
  # build variables that identifies abstentions

# wite data to disk ---------------------------------------------------------------------
saveRDS(resp_data_sc2, "./data/resp_data_sc2")
saveRDS(resp_data_sc2_1, "./data/resp_data_sc2_1")


#### VOTE REDISTRIBUTION - SCENARIO 2.2 =================================================
# assumption: with (campaign) learning, first-round voters for runoff candidates 
#             vote in line with lower-order preferences in second round, if no diverging, 
#             they vote the same in second-round as in first-round

# prepare lower-order preference and choice data for vote redistribution ----------------
resp_data_sc2_2 <- resp_data_sc2
                   # set data for scenario 2.2 with imputed data from scenario 2.1
  # preference matrices as in scenario 2.1

# redistribute votes for runoff candidates ----------------------------------------------
resp_data_sc2_2$choiceMacron[resp_data_sc2_2$choice == 5] <- 1
  # if the respondent voted for Macron in the first round make choice 1
resp_data_sc2_2$choiceMacron[resp_data_sc2_2$choice == 8] <- 0
  # if the respondent voted for Le Pen in the first round make choice 0
resp_data_sc2_2$choiceMacron[preference_matrix5$first == 1] <- 1
  # if the respondent prefers Macron over LePen make choice 1
resp_data_sc2_2$choiceMacron[preference_matrix5$first == 2] <- 0
  # if the respondent prefers LePen over Macron make choice 1
resp_data_sc2_2$choiceLePen[resp_data_sc2_2$choice == 8] <- 1
  # if the respondent voted for LePen in the first round make choice 1
resp_data_sc2_2$choiceLePen[resp_data_sc2_2$choice == 5] <- 0
  # if the respondent voted for Macron in the first round make choice 0
resp_data_sc2_2$choiceLePen[preference_matrix5$first == 2] <- 1
  # if the respondent prefers LePen over Macron make choice 1
resp_data_sc2_2$choiceLePen[preference_matrix5$first == 1] <- 0
  # if the respondent prefers Macron over LePen make choice 0
resp_data_sc2_2 <- resp_data_sc2_2 %>% filter(!is.na(choice))
  # drop observations with choice NA
resp_data_sc2_2$abstain[is.na(resp_data_sc2_2$choiceMacron)] <- 1
resp_data_sc2_2$abstain[resp_data_sc2_2$choiceMacron == 1 | resp_data_sc2_2$choiceLePen == 1] <- 0
  # build variables that identifies abstentions

# wite data to disk ---------------------------------------------------------------------
saveRDS(resp_data_sc2_2, "./data/resp_data_sc2_2")


#### VOTE REDISTRIBUTION - MOCK RUNOFFS =================================================
# assumption: as in scenario 2.2

# Macron vs. Fillon - prepare lower-order preference and choice data for vote 
#                     redistribution ----------------------------------------------------
preference_matrix6 <- resp_data_sc2[,6:16]
                      # select variables that identify imputed lower-order preferences
preference_matrix7 <- preference_matrix6[,c(5,6)]
                      # select variables that identify lower-order preferences for runoff
                      # candidates
preference_matrix7[preference_matrix7 == 0] <- NA
                                               # replace 0, i.e., don't know, with NA
preference_matrix7$first <- max.col(replace(preference_matrix7, is.na(preference_matrix7), 
                                            -Inf), ties.method="random")
                            # add variable that identifies for each row/respondent the 
                            # column/candidate that was prefered most. NAs are replaced
                            # with -Inf. Ties are handled randomly.
preference_matrix7$two_maxima <- lengths(apply(preference_matrix7[,1:2],1,function(x) 
  which(x==max(x)))) > 1
                                 # add variable that identifies ties
preference_matrix7$first <- ifelse(is.na(preference_matrix7$Macron) & 
                                     is.na(preference_matrix7$Fillon), NA, 
                                   preference_matrix7$first)
                            # if no lower-order preference is given for both of the 
                            # runoff candidates, make the choice NA (i.e., no 
                            # redistribution), otherwise substitute choice with prefered
                            # candidate
preference_matrix7$first <- ifelse(preference_matrix7$two_maxima == TRUE, NA, 
                                   preference_matrix7$first)
                            # if the observation exhibits a tie in lower-order preferences
                            # for the runoff candidates, make the choice NA (i.e., no
                            # redistribution as respondents are assumed to abstain), 
                            # otherwise substitute choice with prefered candidate
preference_matrix7$first <- ifelse(is.na(preference_matrix7$Macron) | 
                                     is.na(preference_matrix7$Fillon), NA, 
                                   preference_matrix7$first)
                            # if no lower-order preference is given for one of the 
                            # runoff candidates, make the choice NA (i.e., no 
                            # redistribution because no comparison possible), otherwise 
                            # substitute choice with prefered candidate
resp_data_mock <- resp_data_sc2
                  # prepare data for mock runoffs

# Macron vs. Fillon - redistribute votes for runoff candidates --------------------------
resp_data_mock$choiceMacronFi[resp_data_mock$choice == 5] <- 1
  # if the respondent voted for Macron in the first round  make choice 1
resp_data_mock$choiceMacronFi[resp_data_mock$choice == 6] <- 0
  # if the respondent voted for Fillon in the first round make choice 0
resp_data_mock$choiceMacronFi[preference_matrix7$first == 1] <- 1
  # if the respondent prefers Macron over Fillon make choice 1
resp_data_mock$choiceMacronFi[preference_matrix7$first == 2] <- 0
  # if the respondent prefers Fillon over Macron make choice 1
resp_data_mock$choiceFillonMa[resp_data_mock$choice == 6] <- 1
  # if the respondent voted for Fillon in the first round make choice 1
resp_data_mock$choiceFillonMa[resp_data_mock$choice == 5] <- 0
  # if the respondent voted for Macron in the first round make choice 0
resp_data_mock$choiceFillonMa[preference_matrix7$first == 2] <- 1
  # if the respondent prefers Fillon over Macron make choice 1
resp_data_mock$choiceFillonMa[preference_matrix7$first == 1] <- 0
  # if the respondent prefers Macron over Fillon make choice 0
resp_data_mock$abstainMaFi[is.na(resp_data_mock$choiceMacronFi)] <- 1
resp_data_mock$abstainMaFi[resp_data_mock$choiceMacronFi == 1 | resp_data_mock$choiceFillonMa == 1] <- 0
  # build variables that identifies abstentions

# Macron vs. Melenchon - prepare lower-order preference and choice data for vote 
#                        redistribution -------------------------------------------------
preference_matrix8 <- preference_matrix6[,c(5,3)]
                      # select variables that identify lower-order preferences for runoff
                      # candidates
preference_matrix8[preference_matrix8 == 0] <- NA
                                               # replace 0, i.e., don't know, with NA
preference_matrix8$first <- max.col(replace(preference_matrix8, is.na(preference_matrix8), 
                                            -Inf), ties.method="random")
                            # add variable that identifies for each row/respondent the 
                            # column/candidate that was prefered most. NAs are replaced
                            # with -Inf. Ties are handled randomly.
preference_matrix8$two_maxima <- lengths(apply(preference_matrix8[,1:2],1,function(x) 
  which(x==max(x)))) > 1
                                 # add variable that identifies ties
preference_matrix8$first <- ifelse(is.na(preference_matrix8$Macron) & 
                                     is.na(preference_matrix8$Melenchon), NA, 
                                   preference_matrix8$first)
                            # if no lower-order preference is given for both of the 
                            # runoff candidates, make the choice NA (i.e., no 
                            # redistribution), otherwise substitute choice with prefered
                            # candidate
preference_matrix8$first <- ifelse(preference_matrix8$two_maxima == TRUE, NA, 
                                   preference_matrix8$first)
                            # if the observation exhibits a tie in lower-order preferences
                            # for the runoff candidates, make the choice NA (i.e., no
                            # redistribution as respondents are assumed to abstain), 
                            # otherwise substitute choice with prefered candidate
preference_matrix8$first <- ifelse(is.na(preference_matrix8$Macron) | 
                                     is.na(preference_matrix8$Melenchon), NA, 
                                   preference_matrix8$first)
                            # if no lower-order preference is given for one of the 
                            # runoff candidates, make the choice NA (i.e., no 
                            # redistribution because no comparison possible), otherwise 
                            # substitute choice with prefered candidate

# Macron vs. Melenchon - redistribute votes for runoff candidates -----------------------
resp_data_mock$choiceMacronMe[resp_data_mock$choice == 5] <- 1
  # if the respondent voted for Macron in the first round make choice 1
resp_data_mock$choiceMacronMe[resp_data_mock$choice == 3] <- 0
  # if the respondent voted for Melenchon in the first round make choice 0
resp_data_mock$choiceMacronMe[preference_matrix8$first == 1] <- 1
  # if the respondent prefers Macron over Melenchon make choice 1
resp_data_mock$choiceMacronMe[preference_matrix8$first == 2] <- 0
  # if the respondent prefers Melenchon over Macron make choice 1
resp_data_mock$choiceMelenchonMa[resp_data_mock$choice == 3] <- 1
  # if the respondent voted for Melenchon in the first round make choice 1
resp_data_mock$choiceMelenchonMa[resp_data_mock$choice == 5] <- 0
  # if the respondent voted for Macron in the first round make choice 0
resp_data_mock$choiceMelenchonMa[preference_matrix8$first == 2] <- 1
  # if the respondent prefers Melenchon over Macron make choice 1
resp_data_mock$choiceMelenchonMa[preference_matrix8$first == 1] <- 0
  # if the respondent prefers Macron over Melenchon make choice 0
resp_data_mock$abstainMaMe[is.na(resp_data_mock$choiceMacronMe)] <- 1
resp_data_mock$abstainMaMe[resp_data_mock$choiceMacronMe == 1 | resp_data_mock$choiceMelenchonMa == 1] <- 0
  # build variables that identifies abstentions

# Le Pen vs. Fillon - prepare lower-order preference and choice data for vote 
#                     redistribution ----------------------------------------------------
preference_matrix9 <- preference_matrix6[,c(8,6)]
                      # select variables that identify lower-order preferences for runoff
                      # candidates
preference_matrix9[preference_matrix9 == 0] <- NA
                                               # replace 0, i.e., don't know, with NA
preference_matrix9$first <- max.col(replace(preference_matrix9, is.na(preference_matrix9), 
                                            -Inf), ties.method="random")
                            # add variable that identifies for each row/respondent the 
                            # column/candidate that was prefered most. NAs are replaced
                            # with -Inf. Ties are handled randomly.
preference_matrix9$two_maxima <- lengths(apply(preference_matrix9[,1:2],1,function(x) 
  which(x==max(x)))) > 1
                                 # add variable that identifies ties
preference_matrix9$first <- ifelse(is.na(preference_matrix9$LePen) & 
                                     is.na(preference_matrix9$Fillon), NA, 
                                   preference_matrix9$first)
                            # if no lower-order preference is given for both of the 
                            # runoff candidates, make the choice NA (i.e., no 
                            # redistribution), otherwise substitute choice with prefered
                            # candidate
preference_matrix9$first <- ifelse(preference_matrix9$two_maxima == TRUE, NA, 
                                   preference_matrix9$first)
                            # if the observation exhibits a tie in lower-order preferences
                            # for the runoff candidates, make the choice NA (i.e., no
                            # redistribution as respondents are assumed to abstain), 
                            # otherwise substitute choice with prefered candidate
preference_matrix9$first <- ifelse(is.na(preference_matrix9$LePen) | 
                                     is.na(preference_matrix9$Fillon), NA, 
                                   preference_matrix9$first)
                            # if no lower-order preference is given for one of the 
                            # runoff candidates, make the choice NA (i.e., no 
                            # redistribution because no comparison possible), otherwise 
                            # substitute choice with prefered candidate

# Le Pen vs. Fillon - redistribute votes for runoff candidates --------------------------
resp_data_mock$choiceLePenFi[resp_data_mock$choice == 8] <- 1
  # if the respondent voted for Le Pen in the first round make choice 1
resp_data_mock$choiceLePenFi[resp_data_mock$choice == 6] <- 0
  # if the respondent voted for Fillon in the first round make choice 0
resp_data_mock$choiceLePenFi[preference_matrix9$first == 1] <- 1
  # if the respondent prefers Le Pen over Fillon make choice 1
resp_data_mock$choiceLePenFi[preference_matrix9$first == 2] <- 0
  # if the respondent prefers Fillon over Le Pen make choice 1
resp_data_mock$choiceFillonLe[resp_data_mock$choice == 6] <- 1
  # if the respondent voted for Fillon in the first round make choice 1
resp_data_mock$choiceFillonLe[resp_data_mock$choice == 8] <- 0
  # if the respondent voted for Le Pen in the first round make choice 0
resp_data_mock$choiceFillonLe[preference_matrix9$first == 2] <- 1
  # if the respondent prefers Fillon over Le Pen make choice 1
resp_data_mock$choiceFillonLe[preference_matrix9$first == 1] <- 0
  # if the respondent prefers Le Pen over Fillon make choice 0
resp_data_mock$abstainLeFi[is.na(resp_data_mock$choiceLePenFi)] <- 1
resp_data_mock$abstainLeFi[resp_data_mock$choiceLePenFi == 1 | resp_data_mock$choiceFillonLe == 1] <- 0
  # build variables that identifies abstentions

# Le Pen vs. Melenchon - prepare lower-order preference and choice data for vote 
#                        redistribution -------------------------------------------------
preference_matrix10 <- preference_matrix6[,c(8,3)]
                       # select variables that identify lower-order preferences for runoff
                       # candidates
preference_matrix10[preference_matrix10 == 0] <- NA
                                                 # replace 0, i.e., don't know, with NA
preference_matrix10$first <- max.col(replace(preference_matrix10, is.na(preference_matrix10), 
                                             -Inf), ties.method="random")
                             # add variable that identifies for each row/respondent the 
                             # column/candidate that was prefered most. NAs are replaced
                             # with -Inf. Ties are handled randomly.
preference_matrix10$two_maxima <- lengths(apply(preference_matrix10[,1:2],1,function(x) 
  which(x==max(x)))) > 1
                                  # add variable that identifies ties
preference_matrix10$first <- ifelse(is.na(preference_matrix10$LePen) & 
                                      is.na(preference_matrix10$Melenchon), NA, 
                                    preference_matrix10$first)
                             # if no lower-order preference is given for both of the 
                             # runoff candidates, make the choice NA (i.e., no 
                             # redistribution), otherwise substitute choice with prefered
                             # candidate
preference_matrix10$first <- ifelse(preference_matrix10$two_maxima == TRUE, NA, 
                                    preference_matrix10$first)
                             # if the observation exhibits a tie in lower-order preferences
                             # for the runoff candidates, make the choice NA (i.e., no
                             # redistribution as respondents are assumed to abstain), 
                             # otherwise substitute choice with prefered candidate
preference_matrix10$first <- ifelse(is.na(preference_matrix10$LePen) | 
                                      is.na(preference_matrix10$Melenchon), NA, 
                                    preference_matrix10$first)
                             # if no lower-order preference is given for one of the 
                             # runoff candidates, make the choice NA (i.e., no 
                             # redistribution because no comparison possible), otherwise 
                             # substitute choice with prefered candidate

# Le Pen vs. Melenchon - redistribute votes for runoff candidates -----------------------
resp_data_mock$choiceLePenMe[resp_data_mock$choice == 8] <- 1
  # if the respondent voted for Le Pen in the first round make choice 1
resp_data_mock$choiceLePenMe[resp_data_mock$choice == 3] <- 0
  # if the respondent voted for Melenchon in the first round make choice 0
resp_data_mock$choiceLePenMe[preference_matrix10$first == 1] <- 1
  # if the respondent prefers Le Pen over Melenchon make choice 1
resp_data_mock$choiceLePenMe[preference_matrix10$first == 2] <- 0
  # if the respondent prefers Melenchon over Le Pen make choice 1
resp_data_mock$choiceMelenchonLe[resp_data_mock$choice == 3] <- 1
  # if the respondent voted for Melenchon in the first round make choice 1
resp_data_mock$choiceMelenchonLe[resp_data_mock$choice == 8] <- 0
  # if the respondent voted for Le Pen in the first round make choice 0
resp_data_mock$choiceMelenchonLe[preference_matrix10$first == 2] <- 1
  # if the respondent prefers Melenchon over Le Pen make choice 1
resp_data_mock$choiceMelenchonLe[preference_matrix10$first == 1] <- 0
  # if the respondent prefers Le Pen over Melenchon make choice 0
resp_data_mock$abstainLeMe[is.na(resp_data_mock$choiceLePenMe)] <- 1
resp_data_mock$abstainLeMe[resp_data_mock$choiceLePenMe == 1 | resp_data_mock$choiceMelenchonLe == 1] <- 0
  # build variables that identifies abstentions

# Fillon vs. Melenchon - prepare lower-order preference and choice data for vote
#                        redistribution -------------------------------------------------
preference_matrix11 <- preference_matrix6[,c(6,3)]
                       # select variables that identify lower-order preferences for runoff
                       # candidates
preference_matrix11[preference_matrix11 == 0] <- NA
                                                 # replace 0, i.e., don't know, with NA
preference_matrix11$first <- max.col(replace(preference_matrix11, is.na(preference_matrix11), 
                                             -Inf), ties.method="random")
                             # add variable that identifies for each row/respondent the 
                             # column/candidate that was prefered most. NAs are replaced
                             # with -Inf. Ties are handled randomly.
preference_matrix11$two_maxima <- lengths(apply(preference_matrix11[,1:2],1,function(x) 
  which(x==max(x)))) > 1
                                  # add variable that identifies ties
preference_matrix11$first <- ifelse(is.na(preference_matrix11$Fillon) & 
                                      is.na(preference_matrix11$Melenchon), NA, 
                                    preference_matrix11$first)
                             # if no lower-order preference is given for both of the 
                             # runoff candidates, make the choice NA (i.e., no 
                             # redistribution), otherwise substitute choice with prefered
                             # candidate
preference_matrix11$first <- ifelse(preference_matrix11$two_maxima == TRUE, NA, 
                                    preference_matrix11$first)
                             # if the observation exhibits a tie in lower-order preferences
                             # for the runoff candidates, make the choice NA (i.e., no
                             # redistribution as respondents are assumed to abstain), 
                             # otherwise substitute choice with prefered candidate
preference_matrix11$first <- ifelse(is.na(preference_matrix11$Fillon) | 
                                      is.na(preference_matrix11$Melenchon), NA, 
                                    preference_matrix11$first)
                             # if no lower-order preference is given for one of the 
                             # runoff candidates, make the choice NA (i.e., no 
                             # redistribution because no comparison possible), otherwise 
                             # substitute choice with prefered candidate

# Fillon vs. Melenchon - redistribute votes for runoff candidates -----------------------
resp_data_mock$choiceFillonMe[resp_data_mock$choice == 6] <- 1
  # if the respondent voted for Fillon in the first round make choice 1
resp_data_mock$choiceFillonMe[resp_data_mock$choice == 3] <- 0
  # if the respondent voted for Melenchon in the first round make choice 0
resp_data_mock$choiceFillonMe[preference_matrix10$first == 1] <- 1
  # if the respondent prefers Fillon over Melenchon make choice 1
resp_data_mock$choiceFillonMe[preference_matrix10$first == 2] <- 0
  # if the respondent prefers Melenchon over Fillon make choice 1
resp_data_mock$choiceMelenchonFi[resp_data_mock$choice == 3] <- 1
  # if the respondent voted for Melenchon in the first round make choice 1
resp_data_mock$choiceMelenchonFi[resp_data_mock$choice == 6] <- 0
  # if the respondent voted for Fillon in the first round make choice 0
resp_data_mock$choiceMelenchonFi[preference_matrix10$first == 2] <- 1
  # if the respondent prefers Melenchon over Fillon make choice 1
resp_data_mock$choiceMelenchonFi[preference_matrix10$first == 1] <- 0
  # if the respondent prefers Fillon over Melenchon make choice 0
resp_data_mock$abstainFiMe[is.na(resp_data_mock$choiceFillonMe)] <- 1
resp_data_mock$abstainFiMe[resp_data_mock$choiceFillonMe == 1 | resp_data_mock$choiceMelenchonFi == 1] <- 0
  # build variables that identifies abstentions
resp_data_mock <- resp_data_mock %>% filter(!is.na(choice))
  # drop observations with choice NA

# wite data to disk ---------------------------------------------------------------------
saveRDS(resp_data_mock, "./data/resp_data_mock")
