# ---------------------------------------------------------------------------------------
# HOW TO POLL RUNOFF ELECTIONS
# Peter Selb, Sascha Goebel, and Romain Lachat
# Script for redistribution of votes in exit poll
# April 2020
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
  './data/resp_data''
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
  './data/resp_data_sc1'
  './data/resp_data_sc2''
  './data/resp_data_mock'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
  Line 30 - PREPARATIONS
  Line 88 - VOTE REDISTRIBUTION - NO IMPUTATION
  Line 158 - VOTE REDISTRIBUTION - WITH IMPUTATION
  Line 249 - VOTE REDISTRIBUTION - MOCK RUNOFFS
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("poq-how-to-poll-runoff-elections")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")

# read data from disk -------------------------------------------------------------------
resp_data <- readRDS("./data/resp_data")
resp_data <- filter(resp_data, 
            id == "034Eygalieres" & polling_station_number == "1" |
            id == "103Salon-de-Provence" & polling_station_number == "2" |
            id == "353Tregastel" & polling_station_number == "2" |
            id == "037Avignonet-Lauragais" |
            id == "116Castelginest" |
            id == "420Pinsaguel" & polling_station_number == "2" |
            id == "318Pessac" & polling_station_number == "2" |
            id == "228Valencay" & polling_station_number == "1" |
            id == "078Charce-Saint-Ellier-sur-Aubance" |
            id == "328Ludres" |
            id == "666Terville" & polling_station_number == "4" |
            id == "042Bailleval" |
            id == "486Tinchebray" & polling_station_number == "2" |
            id == "123Lyon" & polling_station_number == "2" |
            id == "081Cluses" & polling_station_number == "2" |
            id == "035Aumale" |
            id == "137Toulon" & polling_station_number == "1" |
            id == "063Rueil-Malmaison" & polling_station_number == "3" |
            id == "048Montreuil" & polling_station_number == "1"|
            id == "051Noisy-le-Grand") 
# add second stage inclusion probabilities for dataset including only those initially sampled
resp_data$second_stage <- ifelse(resp_data$id == "034Eygalieres", 55/682,
                           ifelse(resp_data$id == "035Aumale", 55/1355,
                           ifelse(resp_data$id == "037Avignonet-Lauragais", 55/254,
                           ifelse(resp_data$id == "042Bailleval", 55/937, 
                           ifelse(resp_data$id == "048Montreuil", 55/595,
                           ifelse(resp_data$id == "051Noisy-le-Grand", 55/539,
                           ifelse(resp_data$id == "063Rueil-Malmaison", 55/747,
                           ifelse(resp_data$id == "078Charce-Saint-Ellier-sur-Aubance", 55/494,
                           ifelse(resp_data$id == "081Cluses", 55/549,
                           ifelse(resp_data$id == "103Salon-de-Provence", 55/1023,
                           ifelse(resp_data$id == "116Castelginest", 55/909,
                           ifelse(resp_data$id == "123Lyon", 55/869,
                           ifelse(resp_data$id == "137Toulon", 55/403,
                           ifelse(resp_data$id == "228Valencay", 55/972,
                           ifelse(resp_data$id == "318Pessac", 55/857,
                           ifelse(resp_data$id == "328Ludres", 55/806,
                           ifelse(resp_data$id == "353Tregastel", 55/743,
                           ifelse(resp_data$id == "420Pinsaguel", 55/880,
                           ifelse(resp_data$id == "486Tinchebray", 55/982,
                           ifelse(resp_data$id == "666Terville", 55/524, NA
                           ))))))))))))))))))))


#### VOTE REDISTRIBUTION - NO IMPUTATION ================================================

# first-round voters for runoff candidates vote in line with lower-order preferences in 
# second round, if none, they vote the same in second round as in first round

# prepare lower-order preference and choice data for vote redistribution ----------------
resp_data_sc1 <- resp_data
# build matrix of lower-order preferences
preference_matrix <- resp_data[,4:14]
# select variables that identify lower-order preferences for runoff
# candidates
preference_matrix3 <- preference_matrix[,c(5,8)]
# replace 0, i.e., don't know, with NA
preference_matrix3[preference_matrix3 == 0] <- NA
# add variable that identifies for each row/respondent the 
# column/candidate that was prefered most. NAs are replaced
# with -Inf. Ties are handled randomly.
preference_matrix3$first <- max.col(replace(preference_matrix3, is.na(preference_matrix3), 
                                            -Inf), ties.method="random")
# add variable that identifies ties
preference_matrix3$two_maxima <- lengths(apply(preference_matrix3[,1:2],1,function(x) 
  which(x==max(x)))) > 1
# if no lower-order preference is given for both of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution), otherwise substitute choice with prefered
# candidate
preference_matrix3$first <- ifelse(is.na(preference_matrix3$Macron) & 
                                     is.na(preference_matrix3$LePen), NA, 
                                   preference_matrix3$first)
# if the observation exhibits a tie in lower-order preferences
# for the runoff candidates, make the choice NA (i.e., no
# redistribution as respondents are assumed to abstain), 
# otherwise substitute choice with prefered candidate
preference_matrix3$first <- ifelse(preference_matrix3$two_maxima == TRUE, NA, 
                                   preference_matrix3$first)
# if no lower-order preference is given for one of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution because no comparison possible), otherwise 
# substitute choice with prefered candidate
preference_matrix3$first <- ifelse(is.na(preference_matrix3$Macron) | 
                                     is.na(preference_matrix3$LePen), NA, 
                                   preference_matrix3$first)

# redistribute votes for runoff candidates ----------------------------------------------
# if the respondent voted for Macron in the first round make choice 1
resp_data_sc1$choiceMacron[resp_data_sc1$choice == 5] <- 1
# if the respondent voted for Le Pen in the first round make choice 0
resp_data_sc1$choiceMacron[resp_data_sc1$choice == 8] <- 0
# if the respondent prefers Macron over LePen make choice 1
resp_data_sc1$choiceMacron[preference_matrix3$first == 1] <- 1
# if the respondent prefers Le Pen over Macron make choice 0
resp_data_sc1$choiceMacron[preference_matrix3$first == 2] <- 0
# if the respondent voted for Le Pen in the first round make choice 1
resp_data_sc1$choiceLePen[resp_data_sc1$choice == 8] <- 1
# if the respondent voted for Macron in the first round make choice 0 
resp_data_sc1$choiceLePen[resp_data_sc1$choice == 5] <- 0
# if the respondent prefers Le Pen over Macron make choice 1
resp_data_sc1$choiceLePen[preference_matrix3$first == 2] <- 1
# if the respondent prefers Macron over LePen make choice 0
resp_data_sc1$choiceLePen[preference_matrix3$first == 1] <- 0
# drop observations with choice NA
resp_data_sc1 <- resp_data_sc1 %>% filter(!is.na(choice))
# build variables that identifies abstentions
resp_data_sc1$abstain[is.na(resp_data_sc1$choiceMacron)] <- 1
resp_data_sc1$abstain[resp_data_sc1$choiceMacron == 1 | resp_data_sc1$choiceLePen == 1] <- 0

# wite data to disk ---------------------------------------------------------------------
saveRDS(resp_data_sc1, "./data/resp_data_sc1")


#### VOTE REDISTRIBUTION - WITH IMPUTATION ==============================================

# first-round voters for runoff candidates vote in line with lower-order preferences in 
# second round, if no diverging, they vote the same in second-round as in first-round

# prepare lower-order preference and choice data for vote redistribution ----------------
resp_data_sc2 <- resp_data
# Transfom "don't knows" to NA in order to impute
resp_data_sc2[resp_data_sc2 == 0] <- NA
# transform lower-order preferences to factor variables             
resp_data_sc2[,4:14] <- lapply(resp_data_sc2[,4:14], factor, ordered = TRUE)
# define predictor variables (1) for imputation    
predictor_matrix <- matrix(data = c(1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0), 
                           nrow = ncol(resp_data), ncol = ncol(resp_data), 
                           dimnames = list(colnames(resp_data), colnames(resp_data)), 
                           byrow = TRUE)
# impute missing lower-order preferences based on reported preferences,
# sex, age, and reported choices using ordered polytomous regression. 
# Only columns with "polr" are imputed, those with "" are not.   
resp_data_sc2 <- mice(data = resp_data_sc2, m = 50, 
                      method = c("", "", "", "polr", "polr", "polr", "polr", "polr", 
                                 "polr", "polr", "polr", "polr", "polr", "polr", "", 
                                 "", "", "", "", ""),
                      predictorMatrix = predictor_matrix, maxit = 10) %>%
  # row bind imputed data sets and assign imputation index                
  complete(action = "long")
# transform lower-order preferences back to integer
resp_data_sc2[,6:16] <- lapply(resp_data_sc2[,6:16], as.integer)
# select variables that identify imputed lower-order preferences                       
preference_matrix4 <- resp_data_sc2[,6:16]
# select variables that identify lower-order preferences for runoff
# candidates
preference_matrix5 <- preference_matrix4[,c(5,8)]
# replace 0, i.e., don't know, with NA
preference_matrix5[preference_matrix5 == 0] <- NA
# add variable that identifies for each row/respondent the 
# column/candidate that was prefered most. NAs are replaced
# with -Inf. Ties are handled randomly.                   
preference_matrix5$first <- max.col(replace(preference_matrix5, is.na(preference_matrix5), 
                                            -Inf), ties.method="random")
# add variable that identifies ties
preference_matrix5$two_maxima <- lengths(apply(preference_matrix5[,1:2],1,function(x) 
  which(x==max(x)))) > 1
# if no lower-order preference is given for both of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution), otherwise substitute choice with prefered
# candidate     
preference_matrix5$first <- ifelse(is.na(preference_matrix5$Macron) & 
                                     is.na(preference_matrix5$LePen), NA, 
                                   preference_matrix5$first)
# if the observation exhibits a tie in lower-order preferences
# for the runoff candidates, make the choice NA (i.e., no
# redistribution as respondents are assumed to abstain), 
# otherwise substitute choice with prefered candidate
preference_matrix5$first <- ifelse(preference_matrix5$two_maxima == TRUE, NA, 
                                   preference_matrix5$first)
# if no lower-order preference is given for one of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution because no comparison possible), otherwise 
# substitute choice with prefered candidate
preference_matrix5$first <- ifelse(is.na(preference_matrix5$Macron) | 
                                     is.na(preference_matrix5$LePen), NA, 
                                   preference_matrix5$first)

# redistribute votes for runoff candidates ----------------------------------------------
# if the respondent voted for Macron in the first round make choice 1
resp_data_sc2$choiceMacron[resp_data_sc2$choice == 5] <- 1
# if the respondent voted for Le Pen in the first round make choice 0
resp_data_sc2$choiceMacron[resp_data_sc2$choice == 8] <- 0
# if the respondent prefers Macron over LePen make choice 1
resp_data_sc2$choiceMacron[preference_matrix5$first == 1] <- 1
# if the respondent prefers LePen over Macron make choice 1 
resp_data_sc2$choiceMacron[preference_matrix5$first == 2] <- 0
# if the respondent voted for LePen in the first round make choice 1
resp_data_sc2$choiceLePen[resp_data_sc2$choice == 8] <- 1
# if the respondent voted for Macron in the first round make choice 0
resp_data_sc2$choiceLePen[resp_data_sc2$choice == 5] <- 0
# if the respondent prefers LePen over Macron make choice 1
resp_data_sc2$choiceLePen[preference_matrix5$first == 2] <- 1
# if the respondent prefers Macron over LePen make choice 0
resp_data_sc2$choiceLePen[preference_matrix5$first == 1] <- 0
# drop observations with choice NA
resp_data_sc2 <- resp_data_sc2 %>% filter(!is.na(choice))
# build variables that identifies abstentions
resp_data_sc2$abstain[is.na(resp_data_sc2$choiceMacron)] <- 1
resp_data_sc2$abstain[resp_data_sc2$choiceMacron == 1 | resp_data_sc2$choiceLePen == 1] <- 0

# wite data to disk ---------------------------------------------------------------------
saveRDS(resp_data_sc2, "./data/resp_data_sc2")


#### VOTE REDISTRIBUTION - MOCK RUNOFFS =================================================

# Macron vs. Fillon - prepare lower-order preference and choice data for vote 
#                     redistribution ----------------------------------------------------
# select variables that identify imputed lower-order preferences
preference_matrix6 <- resp_data_sc2[,6:16]
# select variables that identify lower-order preferences for runoff
# candidates                      
preference_matrix7 <- preference_matrix6[,c(5,6)]
# replace 0, i.e., don't know, with NA
preference_matrix7[preference_matrix7 == 0] <- NA
# add variable that identifies for each row/respondent the 
# column/candidate that was prefered most. NAs are replaced
# with -Inf. Ties are handled randomly.                   
preference_matrix7$first <- max.col(replace(preference_matrix7, is.na(preference_matrix7), 
                                            -Inf), ties.method="random")
# add variable that identifies ties
preference_matrix7$two_maxima <- lengths(apply(preference_matrix7[,1:2],1,function(x) 
  which(x==max(x)))) > 1
# if no lower-order preference is given for both of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution), otherwise substitute choice with prefered
# candidate     
preference_matrix7$first <- ifelse(is.na(preference_matrix7$Macron) & 
                                     is.na(preference_matrix7$Fillon), NA, 
                                   preference_matrix7$first)
# if the observation exhibits a tie in lower-order preferences
# for the runoff candidates, make the choice NA (i.e., no
# redistribution as respondents are assumed to abstain), 
# otherwise substitute choice with prefered candidate
preference_matrix7$first <- ifelse(preference_matrix7$two_maxima == TRUE, NA, 
                                   preference_matrix7$first)
# if no lower-order preference is given for one of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution because no comparison possible), otherwise 
# substitute choice with prefered candidate
preference_matrix7$first <- ifelse(is.na(preference_matrix7$Macron) | 
                                     is.na(preference_matrix7$Fillon), NA, 
                                   preference_matrix7$first)
# prepare data for mock runoffs          
resp_data_mock <- resp_data_sc2

# Macron vs. Fillon - redistribute votes for runoff candidates --------------------------
# if the respondent voted for Macron in the first round  make choice 1
resp_data_mock$choiceMacronFi[resp_data_mock$choice == 5] <- 1
# if the respondent voted for Fillon in the first round make choice 0  
resp_data_mock$choiceMacronFi[resp_data_mock$choice == 6] <- 0
# if the respondent prefers Macron over Fillon make choice 1 
resp_data_mock$choiceMacronFi[preference_matrix7$first == 1] <- 1
# if the respondent prefers Fillon over Macron make choice 1 
resp_data_mock$choiceMacronFi[preference_matrix7$first == 2] <- 0
# if the respondent voted for Fillon in the first round make choice 1  
resp_data_mock$choiceFillonMa[resp_data_mock$choice == 6] <- 1
# if the respondent voted for Macron in the first round make choice 0  
resp_data_mock$choiceFillonMa[resp_data_mock$choice == 5] <- 0
# if the respondent prefers Fillon over Macron make choice 1  
resp_data_mock$choiceFillonMa[preference_matrix7$first == 2] <- 1
# if the respondent prefers Macron over Fillon make choice 0  
resp_data_mock$choiceFillonMa[preference_matrix7$first == 1] <- 0
# build variables that identifies abstentions  
resp_data_mock$abstainMaFi[is.na(resp_data_mock$choiceMacronFi)] <- 1
resp_data_mock$abstainMaFi[resp_data_mock$choiceMacronFi == 1 | resp_data_mock$choiceFillonMa == 1] <- 0

# Macron vs. Melenchon - prepare lower-order preference and choice data for vote 
#                        redistribution -------------------------------------------------
# select variables that identify lower-order preferences for runoff
# candidates
preference_matrix8 <- preference_matrix6[,c(5,3)]
# replace 0, i.e., don't know, with NA                      
preference_matrix8[preference_matrix8 == 0] <- NA
# add variable that identifies for each row/respondent the 
# column/candidate that was prefered most. NAs are replaced
# with -Inf. Ties are handled randomly.                                               
preference_matrix8$first <- max.col(replace(preference_matrix8, is.na(preference_matrix8), 
                                            -Inf), ties.method="random")
# add variable that identifies ties                            
preference_matrix8$two_maxima <- lengths(apply(preference_matrix8[,1:2],1,function(x) 
  which(x==max(x)))) > 1
# if no lower-order preference is given for both of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution), otherwise substitute choice with prefered
# candidate              
preference_matrix8$first <- ifelse(is.na(preference_matrix8$Macron) & 
                                     is.na(preference_matrix8$Melenchon), NA, 
                                   preference_matrix8$first)
# if the observation exhibits a tie in lower-order preferences
# for the runoff candidates, make the choice NA (i.e., no
# redistribution as respondents are assumed to abstain), 
# otherwise substitute choice with prefered candidate
preference_matrix8$first <- ifelse(preference_matrix8$two_maxima == TRUE, NA, 
                                   preference_matrix8$first)
# if no lower-order preference is given for one of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution because no comparison possible), otherwise 
# substitute choice with prefered candidate
preference_matrix8$first <- ifelse(is.na(preference_matrix8$Macron) | 
                                     is.na(preference_matrix8$Melenchon), NA, 
                                   preference_matrix8$first)

# Macron vs. Melenchon - redistribute votes for runoff candidates -----------------------
# if the respondent voted for Macron in the first round make choice 1
resp_data_mock$choiceMacronMe[resp_data_mock$choice == 5] <- 1
# if the respondent voted for Melenchon in the first round make choice 0
resp_data_mock$choiceMacronMe[resp_data_mock$choice == 3] <- 0
# if the respondent prefers Macron over Melenchon make choice 1
resp_data_mock$choiceMacronMe[preference_matrix8$first == 1] <- 1
# if the respondent prefers Melenchon over Macron make choice 1
resp_data_mock$choiceMacronMe[preference_matrix8$first == 2] <- 0
# if the respondent voted for Melenchon in the first round make choice 1
resp_data_mock$choiceMelenchonMa[resp_data_mock$choice == 3] <- 1
# if the respondent voted for Macron in the first round make choice 0 
resp_data_mock$choiceMelenchonMa[resp_data_mock$choice == 5] <- 0
# if the respondent prefers Melenchon over Macron make choice 1
resp_data_mock$choiceMelenchonMa[preference_matrix8$first == 2] <- 1
# if the respondent prefers Macron over Melenchon make choice 0
resp_data_mock$choiceMelenchonMa[preference_matrix8$first == 1] <- 0
# build variables that identifies abstentions
resp_data_mock$abstainMaMe[is.na(resp_data_mock$choiceMacronMe)] <- 1
resp_data_mock$abstainMaMe[resp_data_mock$choiceMacronMe == 1 | resp_data_mock$choiceMelenchonMa == 1] <- 0

# Le Pen vs. Fillon - prepare lower-order preference and choice data for vote 
#                     redistribution ----------------------------------------------------
# select variables that identify lower-order preferences for runoff
# candidates
preference_matrix9 <- preference_matrix6[,c(8,6)]
# replace 0, i.e., don't know, with NA
preference_matrix9[preference_matrix9 == 0] <- NA
# add variable that identifies for each row/respondent the 
# column/candidate that was prefered most. NAs are replaced
# with -Inf. Ties are handled randomly.                   
preference_matrix9$first <- max.col(replace(preference_matrix9, is.na(preference_matrix9), 
                                            -Inf), ties.method="random")
# add variable that identifies ties
preference_matrix9$two_maxima <- lengths(apply(preference_matrix9[,1:2],1,function(x) 
  which(x==max(x)))) > 1
# if no lower-order preference is given for both of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution), otherwise substitute choice with prefered
# candidate     
preference_matrix9$first <- ifelse(is.na(preference_matrix9$LePen) & 
                                     is.na(preference_matrix9$Fillon), NA, 
                                   preference_matrix9$first)
# if the observation exhibits a tie in lower-order preferences
# for the runoff candidates, make the choice NA (i.e., no
# redistribution as respondents are assumed to abstain), 
# otherwise substitute choice with prefered candidate
preference_matrix9$first <- ifelse(preference_matrix9$two_maxima == TRUE, NA, 
                                   preference_matrix9$first)
# if no lower-order preference is given for one of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution because no comparison possible), otherwise 
# substitute choice with prefered candidate
preference_matrix9$first <- ifelse(is.na(preference_matrix9$LePen) | 
                                     is.na(preference_matrix9$Fillon), NA, 
                                   preference_matrix9$first)

# Le Pen vs. Fillon - redistribute votes for runoff candidates --------------------------
# if the respondent voted for Le Pen in the first round make choice 1
resp_data_mock$choiceLePenFi[resp_data_mock$choice == 8] <- 1
# if the respondent voted for Fillon in the first round make choice 0
resp_data_mock$choiceLePenFi[resp_data_mock$choice == 6] <- 0
# if the respondent prefers Le Pen over Fillon make choice 1
resp_data_mock$choiceLePenFi[preference_matrix9$first == 1] <- 1
# if the respondent prefers Fillon over Le Pen make choice 1
resp_data_mock$choiceLePenFi[preference_matrix9$first == 2] <- 0
# if the respondent voted for Fillon in the first round make choice 1
resp_data_mock$choiceFillonLe[resp_data_mock$choice == 6] <- 1
# if the respondent voted for Le Pen in the first round make choice 0 
resp_data_mock$choiceFillonLe[resp_data_mock$choice == 8] <- 0
# if the respondent prefers Fillon over Le Pen make choice 1
resp_data_mock$choiceFillonLe[preference_matrix9$first == 2] <- 1
# if the respondent prefers Le Pen over Fillon make choice 0
resp_data_mock$choiceFillonLe[preference_matrix9$first == 1] <- 0
# build variables that identifies abstentions
resp_data_mock$abstainLeFi[is.na(resp_data_mock$choiceLePenFi)] <- 1
resp_data_mock$abstainLeFi[resp_data_mock$choiceLePenFi == 1 | resp_data_mock$choiceFillonLe == 1] <- 0

# Le Pen vs. Melenchon - prepare lower-order preference and choice data for vote 
#                        redistribution -------------------------------------------------
# select variables that identify lower-order preferences for runoff
# candidates
preference_matrix10 <- preference_matrix6[,c(8,3)]
# replace 0, i.e., don't know, with NA
preference_matrix10[preference_matrix10 == 0] <- NA
# add variable that identifies for each row/respondent the 
# column/candidate that was prefered most. NAs are replaced
# with -Inf. Ties are handled randomly.                    
preference_matrix10$first <- max.col(replace(preference_matrix10, is.na(preference_matrix10), 
                                             -Inf), ties.method="random")
# add variable that identifies ties
preference_matrix10$two_maxima <- lengths(apply(preference_matrix10[,1:2],1,function(x) 
  which(x==max(x)))) > 1
# if no lower-order preference is given for both of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution), otherwise substitute choice with prefered
# candidate     
preference_matrix10$first <- ifelse(is.na(preference_matrix10$LePen) & 
                                      is.na(preference_matrix10$Melenchon), NA, 
                                    preference_matrix10$first)
# if the observation exhibits a tie in lower-order preferences
# for the runoff candidates, make the choice NA (i.e., no
# redistribution as respondents are assumed to abstain), 
# otherwise substitute choice with prefered candidate
preference_matrix10$first <- ifelse(preference_matrix10$two_maxima == TRUE, NA, 
                                    preference_matrix10$first)
# if no lower-order preference is given for one of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution because no comparison possible), otherwise 
# substitute choice with prefered candidate
preference_matrix10$first <- ifelse(is.na(preference_matrix10$LePen) | 
                                      is.na(preference_matrix10$Melenchon), NA, 
                                    preference_matrix10$first)

# Le Pen vs. Melenchon - redistribute votes for runoff candidates -----------------------
# if the respondent voted for Le Pen in the first round make choice 1
resp_data_mock$choiceLePenMe[resp_data_mock$choice == 8] <- 1
# if the respondent voted for Melenchon in the first round make choice 0 
resp_data_mock$choiceLePenMe[resp_data_mock$choice == 3] <- 0
# if the respondent prefers Le Pen over Melenchon make choice 1
resp_data_mock$choiceLePenMe[preference_matrix10$first == 1] <- 1
# if the respondent prefers Melenchon over Le Pen make choice 1
resp_data_mock$choiceLePenMe[preference_matrix10$first == 2] <- 0
# if the respondent voted for Melenchon in the first round make choice 1
resp_data_mock$choiceMelenchonLe[resp_data_mock$choice == 3] <- 1
# if the respondent voted for Le Pen in the first round make choice 0
resp_data_mock$choiceMelenchonLe[resp_data_mock$choice == 8] <- 0
# if the respondent prefers Melenchon over Le Pen make choice 1
resp_data_mock$choiceMelenchonLe[preference_matrix10$first == 2] <- 1
# if the respondent prefers Le Pen over Melenchon make choice 0
resp_data_mock$choiceMelenchonLe[preference_matrix10$first == 1] <- 0
# build variables that identifies abstentions 
resp_data_mock$abstainLeMe[is.na(resp_data_mock$choiceLePenMe)] <- 1
resp_data_mock$abstainLeMe[resp_data_mock$choiceLePenMe == 1 | resp_data_mock$choiceMelenchonLe == 1] <- 0

# Fillon vs. Melenchon - prepare lower-order preference and choice data for vote
#                        redistribution -------------------------------------------------
# select variables that identify lower-order preferences for runoff
# candidates
preference_matrix11 <- preference_matrix6[,c(6,3)]
# replace 0, i.e., don't know, with NA
preference_matrix11[preference_matrix11 == 0] <- NA
# add variable that identifies for each row/respondent the 
# column/candidate that was prefered most. NAs are replaced
# with -Inf. Ties are handled randomly.                   
preference_matrix11$first <- max.col(replace(preference_matrix11, is.na(preference_matrix11), 
                                             -Inf), ties.method="random")
# add variable that identifies ties
preference_matrix11$two_maxima <- lengths(apply(preference_matrix11[,1:2],1,function(x) 
  which(x==max(x)))) > 1
# if no lower-order preference is given for both of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution), otherwise substitute choice with prefered
# candidate     
preference_matrix11$first <- ifelse(is.na(preference_matrix11$Fillon) & 
                                      is.na(preference_matrix11$Melenchon), NA, 
                                    preference_matrix11$first)
# if the observation exhibits a tie in lower-order preferences
# for the runoff candidates, make the choice NA (i.e., no
# redistribution as respondents are assumed to abstain), 
# otherwise substitute choice with prefered candidate
preference_matrix11$first <- ifelse(preference_matrix11$two_maxima == TRUE, NA, 
                                    preference_matrix11$first)
# if no lower-order preference is given for one of the 
# runoff candidates, make the choice NA (i.e., no 
# redistribution because no comparison possible), otherwise 
# substitute choice with prefered candidate 
preference_matrix11$first <- ifelse(is.na(preference_matrix11$Fillon) | 
                                      is.na(preference_matrix11$Melenchon), NA, 
                                    preference_matrix11$first)
                            
# Fillon vs. Melenchon - redistribute votes for runoff candidates -----------------------
# if the respondent voted for Fillon in the first round make choice 1
resp_data_mock$choiceFillonMe[resp_data_mock$choice == 6] <- 1
# if the respondent voted for Melenchon in the first round make choice 0
resp_data_mock$choiceFillonMe[resp_data_mock$choice == 3] <- 0
# if the respondent prefers Fillon over Melenchon make choice 1
resp_data_mock$choiceFillonMe[preference_matrix10$first == 1] <- 1
# if the respondent prefers Melenchon over Fillon make choice 1 
resp_data_mock$choiceFillonMe[preference_matrix10$first == 2] <- 0
# if the respondent voted for Melenchon in the first round make choice 1
resp_data_mock$choiceMelenchonFi[resp_data_mock$choice == 3] <- 1
# if the respondent voted for Fillon in the first round make choice 0
resp_data_mock$choiceMelenchonFi[resp_data_mock$choice == 6] <- 0
# if the respondent prefers Melenchon over Fillon make choice 1
resp_data_mock$choiceMelenchonFi[preference_matrix10$first == 2] <- 1
# if the respondent prefers Fillon over Melenchon make choice 0 
resp_data_mock$choiceMelenchonFi[preference_matrix10$first == 1] <- 0
# build variables that identifies abstentions
resp_data_mock$abstainFiMe[is.na(resp_data_mock$choiceFillonMe)] <- 1
resp_data_mock$abstainFiMe[resp_data_mock$choiceFillonMe == 1 | resp_data_mock$choiceMelenchonFi == 1] <- 0
# drop observations with choice NA
resp_data_mock <- resp_data_mock %>% filter(!is.na(choice))

# wite data to disk ---------------------------------------------------------------------
saveRDS(resp_data_mock, "./data/resp_data_mock")
