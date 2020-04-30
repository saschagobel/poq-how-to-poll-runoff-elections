# ---------------------------------------------------------------------------------------
# HOW TO POLL RUNOFF ELECTIONS
# Peter Selb, Sascha Goebel, and Romain Lachat
# Functions script
# April 2020
# ---------------------------------------------------------------------------------------


# content -------------------------------------------------------------------------------
cat(underline("FUNCTIONS"),"
Line 17 - rejectivePpsSampling
Line 241 - validation
Line 309 - cint_poll
")


#### rejectivePpsSampling ===============================================================

# k = number of samples to be drawn in one simulation run
# m = sample size
# Nh = number of voters at polling places
# Vj = candidates' national vote shares
# Nj = candidates' national vote totals
# ph = first stage inclusion probabilities
# sim = number of times the process should be repeated
# parallelize = logical indicating whether to parallelize or not
# data = data.frame with electoral data on polling place level

rejectivePpsSampling <- function(k, m, Nh, Vj, Nj, ph, sim, parallelize, data) {
    if (parallelize != TRUE) {
      # prepare targets for storing metrics over repeated simulations
      Bsim <- rep(NA, times = sim)
      MAEsim <- rep(NA, times = sim)
      MSEsim <- rep(NA, times = sim)
      RMSEsim <- rep(NA, times = sim)
      DEVsim <- rep(NA, times = sim)
      MAETsim <- rep(NA, times = sim)
      MSETsim <- rep(NA, times = sim)
      RMSETsim <- rep(NA, times = sim)
      DEVTsim <- rep(NA, times = sim)
      allsamples <- rep(list(NA), times = sim)
      sampleBsim <- rep(list(NA), times = sim)
      sampleMAEsim <- rep(list(NA), times = sim)
      sampleMSEsim <- rep(list(NA), times = sim)
      sampleRMSEsim <- rep(list(NA), times = sim)
      sampleDEVsim <- rep(list(NA), times = sim)
      sampleMAETsim <- rep(list(NA), times = sim)
      sampleMSETsim <- rep(list(NA), times = sim)
      sampleRMSETsim <- rep(list(NA), times = sim)
      sampleDEVTsim <- rep(list(NA), times = sim)
      # loop for simulations
      for(l in 1:sim) {
        # prepare target for storing drawn samples
        sample <- rep(list(matrix(NA)), times = k)
        # prepare target for storing metrics for drawn samples
        Bk <- rep(NA, times = k)
        MAEk <- rep(NA, times = k)
        MSEk <- rep(NA, times = k)
        RMSEk <- rep(NA, times = k)
        DEVk <- rep(NA, times = k)
        MAETk <- rep(NA, times = k)
        MSETk <- rep(NA, times = k)
        RMSETk <- rep(NA, times = k)
        DEVTk <- rep(NA, times = k)
        # loop for samples drawn in one simulation run
        for(i in 1:k) {
          # draw a PPS sample of m polling places with PPS of Nh using
          # Sampford's method
          sample[[i]] <- sampford(size = Nh, n = m)
          # merge sampled polling places with respective inclusion probabilities
          # (ph) and candidate data 
          sample2 <- merge(data.frame(polling_station_unique = data[!duplicated(data$polling_station_unique), ]$polling_station_unique[sample[[i]]], ph = ph[sample[[i]]]), 
                           data, by = "polling_station_unique")
          # specify as complex survey design object
          sample2 <- svydesign(id = ~1, probs = sample2$ph, data = sample2)
          # prepare target for storing candidates' national vote totals for current
          # sample (2012)
          Njk <- rep(NA, times = max(data$candidate_number))
          # loop for different candidates
          for(j in 1:length(Njk)) {
            # compute current candidate's national vote total (Njk)
            Njk[j] <- svytotal(~votes, subset(sample2, candidate_number == j))[1]
          }
          # compute total number of voters (Nk)
          Nk <- svytotal(~valid_votes, subset(sample2, !duplicated(polling_station_unique)))[1]
          # compute candidates' national first round vote share 
          vj <- Njk/Nk
          # compute and store metrics
          Aj <- log((vj/(1-vj))*((1-Vj)/Vj))
          Bk[i] <- sum(Vj*abs(Aj))
          MAEk[i] <- mean(abs(Vj-vj))
          MSEk[i] <- mean((Vj-vj)^2)
          RMSEk[i] <- sqrt(mean((Vj-vj)^2))
          DEVk[i] <- sum(abs(Vj-vj))
          MAETk[i] <- mean(abs(Nj-Njk))
          MSETk[i] <- mean((Nj-Njk)^2)
          RMSETk[i] <- sqrt(mean((Nj-Njk)^2))
          DEVTk[i] <- sum(abs(Nj-Njk))
        }
        # among the k samples drawn in the current simulation run, find and 
        # store the smallest metric respectively
        Bsim[l] <- min(Bk)
        MAEsim[l] <- min(MAEk)
        MSEsim[l] <- min(MSEk)
        RMSEsim[l] <- min(RMSEk)
        DEVsim[l] <- min(DEVk)
        MAETsim[l] <- min(MAETk)
        MSETsim[l] <- min(MSETk)
        RMSETsim[l] <- min(RMSETk)
        DEVTsim[l] <- min(DEVTk)
        # among the k samples drawn in the current simulation run, find 
        # and store the indices of the sampled polling places for the 
        # sample with the smallest metric respectively
        sampleBsim[[l]] <- sample[[which(Bk == min(Bk))]]
        sampleMAEsim[[l]] <- sample[[which(MAEk == min(MAEk))]]
        sampleMSEsim[[l]] <- sample[[which(MSEk == min(MSEk))]]
        sampleRMSEsim[[l]] <- sample[[which(RMSEk == min(RMSEk))]]
        sampleDEVsim[[l]] <- sample[[which(DEVk == min(DEVk))]]
        sampleMAETsim[[l]] <- sample[[which(MAETk == min(MAETk))]]
        sampleMSETsim[[l]] <- sample[[which(MSETk == min(MSETk))]]
        sampleRMSETsim[[l]] <- sample[[which(RMSETk == min(RMSETk))]]
        sampleDEVTsim[[l]] <- sample[[which(DEVTk == min(DEVTk))]]
        # store the indices of all k sampled polling places in the 
        # current simulation run
        allsamples[[l]] <- sample
      }
    } else {
      # parallel loop for simulations
      results <- foreach(l = 1:sim, .packages = c("pps", "survey"), .verbose = FALSE) %dopar% {
        # prepare target for storing drawn samples
        sample <- rep(list(matrix(NA)), times = k)
        # prepare targets for storing metrics for drawn samples
        Bk <- rep(NA, times = k)
        MAEk <- rep(NA, times = k)
        MSEk <- rep(NA, times = k)
        RMSEk <- rep(NA, times = k)
        DEVk <- rep(NA, times = k)
        MAETk <- rep(NA, times = k)
        MSETk <- rep(NA, times = k)
        RMSETk <- rep(NA, times = k)
        DEVTk <- rep(NA, times = k)
        # loop for samples drawn in one simulation run
        for(i in 1:k) {
          # draw a PPS sample of m polling places with PPS of Nh using
          # Sampford's method
          sample[[i]] <- sampford(size = Nh, n = m)
          # merge sampled polling places with respective inclusion probabilities
          # (ph) and candidate data   
          sample2 <- merge(data.frame(polling_station_unique = data[!duplicated(data$polling_station_unique), ]$polling_station_unique[sample[[i]]], ph = ph[sample[[i]]]), 
                          data, by = "polling_station_unique")
          # specify as complex survey design object
          sample2 <- svydesign(id = ~1, probs = sample2$ph, data = sample2)
          # prepare target for storing candidates' national vote totals for current
          # sample (2012)    
          Njk <- rep(NA, times = max(data$candidate_number))
          # loop for different candidates      
          for(j in 1:length(Njk)) {
            # compute current candidate's national vote total (Njk)
            Njk[j] <- svytotal(~votes, subset(sample2, candidate_number == j))[1]
          }
          # compute total number of voters (Nk)
          Nk <- svytotal(~valid_votes, subset(sample2, !duplicated(polling_station_unique)))[1]
          # compute candidates' national first round vote share
          vj <- Njk/Nk
          # compute metrics
          Aj <- log((vj/(1-vj))*((1-Vj)/Vj))
          Bk[i] <- sum(Vj*abs(Aj))
          MAEk[i] <- mean(abs(Vj-vj))
          MSEk[i] <- mean((Vj-vj)^2)
          RMSEk[i] <- sqrt(mean((Vj-vj)^2))
          DEVk[i] <- sum(abs(Vj-vj))
          MAETk[i] <- mean(abs(Nj-Njk))
          MSETk[i] <- mean((Nj-Njk)^2)
          RMSETk[i] <- sqrt(mean((Nj-Njk)^2))
          DEVTk[i] <- sum(abs(Nj-Njk))
        }
        # among the k samples drawn in the current simulation run, find and 
        # store the smallest metric respectively
        Bsim <- min(Bk)
        MAEsim <- min(MAEk)
        MSEsim <- min(MSEk)
        RMSEsim <- min(RMSEk)
        DEVsim <- min(DEVk)
        MAETsim <- min(MAETk)
        MSETsim <- min(MSETk)
        RMSETsim <- min(RMSETk)
        DEVTsim <- min(DEVTk)
        # among the k samples drawn in the current simulation run, find 
        # and store the indices of the sampled polling places for the 
        # sample with the smallest metric respectively
        sampleBsim <- sample[[which(Bk == min(Bk))]]
        sampleMAEsim <- sample[[which(MAEk == min(MAEk))]]
        sampleMSEsim <- sample[[which(MSEk == min(MSEk))]]
        sampleRMSEsim <- sample[[which(RMSEk == min(RMSEk))]]
        sampleDEVsim <- sample[[which(DEVk == min(DEVk))]]
        sampleMAETsim <- sample[[which(MAETk == min(MAETk))]]
        sampleMSETsim <- sample[[which(MSETk == min(MSETk))]]
        sampleRMSETsim <- sample[[which(RMSETk == min(RMSETk))]]
        sampleDEVTsim <- sample[[which(DEVTk == min(DEVTk))]]
        # store the indices of all k sampled polling places in the 
        # current simulation run
        allsamples <- sample
        return(list(Bsim, MAEsim, MSEsim, RMSEsim, DEVsim, 
                    MAETsim, MSETsim, RMSETsim, DEVTsim, 
                    sampleBsim,sampleMAEsim,sampleMSEsim,sampleRMSEsim,sampleDEVsim,
                    sampleMAETsim,sampleMSETsim,sampleRMSETsim,sampleDEVTsim,
                    allsamples))
      }
      # collect results from parallelized call
      Bsim <- unlist(lapply(results, `[[`, 1))
      MAEsim <- unlist(lapply(results, `[[`, 2))
      MSEsim <- unlist(lapply(results, `[[`, 3))
      RMSEsim <- unlist(lapply(results, `[[`, 4))
      DEVsim <- unlist(lapply(results, `[[`, 5))
      MAETsim <- unlist(lapply(results, `[[`, 6))
      MSETsim <- unlist(lapply(results, `[[`, 7))
      RMSETsim <- unlist(lapply(results, `[[`, 8))
      DEVTsim <- unlist(lapply(results, `[[`, 9))
      sampleBsim <- lapply(results, `[[`, 10)
      sampleMAEsim <- lapply(results, `[[`, 11)
      sampleMSEsim <- lapply(results, `[[`, 12)
      sampleRMSEsim <- lapply(results, `[[`, 13)
      sampleDEVsim <- lapply(results, `[[`, 14)
      sampleMAETsim <- lapply(results, `[[`, 15)
      sampleMSETsim <- lapply(results, `[[`, 16)
      sampleRMSETsim <- lapply(results, `[[`, 17)
      sampleDEVTsim <- lapply(results, `[[`, 18)
      allsamples <- lapply(results, `[[`, 19)
    }
  # return a list with metrics for chosen samples and indices of 
  # sampled polling places (rejected and best)
  return(list(Bs = Bsim, MAEs = MAEsim, MSEs = MSEsim, RMSEs = RMSEsim, DEVs = DEVsim,
              MAETs = MAETsim, MSETs = MSETsim, RMSETs = RMSETsim, DEVTs = DEVTsim,
              Bsamples = sampleBsim, MAEsamples = sampleMAEsim, MSEsamples = sampleMSEsim, 
              RMSEsamples = sampleRMSEsim, DEVsamples = sampleDEVsim, MAETsamples = sampleMAETsim,
              MSETsamples = sampleMSETsim, RMSETsamples = sampleRMSETsim, DEVTsamples = sampleDEVTsim, 
              all_samples = allsamples))
}


# validation ============================================================================

# m = sample size
# Vj = candidates' national vote shares
# ph = first order inclusion probabilities
# sample = sample generated with rejectivePpsSampling
# data1 = data.frame with electoral data on polling place level used with
#         rejectivePpsSampling
# data2 = data.frame with electoral data on polling place level at 
#         time != data1, column polling_station_unique must match in
#         data1 and data2

validation <- function(m, Nj, Vj, ph, sample, data1, data2) {
  # define calls to ph       
  ph2 <- rep(x = 1:length(m), each = length(sample))
  # parallel loop for different polling stations in sample
  results <- foreach(i = 1:length(sample), .packages = c("survey"), .verbose = FALSE) %dopar% {
    # select ith polling place in sample from data1
    pp <- data1[!duplicated(data1$polling_station_unique), ]$polling_station_unique[sample[[i]]]
    # merge polling place and ph in sample from data 1 with respective data in 
    # data2 
    sample2 <- merge(data.frame(polling_station_unique = data1[!duplicated(data1$polling_station_unique),]$polling_station_unique, ph = ph[[ph2[i]]]), data2[data2$polling_station_unique %in% pp,], by = "polling_station_unique")
    # specify as complex survey design object
    sample2 <- svydesign(id = ~1, probs = sample2$ph, data = sample2)
    # prepare target for storing candidates' national vote totals for current
    # sample    
    Njk <- rep(NA, times = max(data2$candidate_number))
    # loop for different candidates
    for(j in 1:length(Njk)) {
      # compute current candidate's national vote total (Njk)
      Njk[j] <- svytotal(~votes, subset(sample2, candidate_number == j))[1]
    }
    # compute total number of voters (Nk)
    Nk <- svytotal(~valid_votes, subset(sample2, !duplicated(polling_station_unique)))[1]
    # compute candidates' national first round vote share
    vj <- Njk/Nk
    # compute metrics
    Aj <- log((vj/(1-vj))*((1-Vj)/Vj))
    Bk <- sum(Vj*abs(Aj))
    MAEk <- mean(abs(Vj-vj))
    MSEk <- mean((Vj-vj)^2)
    RMSEk <- sqrt(mean((Vj-vj)^2))
    DEVk <- sum(abs(Vj-vj))
    MAETk <- mean(abs(Nj-Njk))
    MSETk <- mean((Nj-Njk)^2)
    RMSETk <- sqrt(mean((Nj-Njk)^2))
    DEVTk <- sum(abs(Nj-Njk))
    RAWk <- vj-Vj 
    RAWTk <- Njk-Nj
    return(list(Bk,MAEk,MSEk,RMSEk,DEVk,MAETk,MSETk,RMSETk,DEVTk,RAWk,RAWTk))
  }
  # collect results from parallelized call
  Bs <- unlist(lapply(results, `[[`, 1))
  MAEs <- unlist(lapply(results, `[[`, 2))
  MSEs <- unlist(lapply(results, `[[`, 3))
  RMSEs <- unlist(lapply(results, `[[`, 4))
  DEVs <- unlist(lapply(results, `[[`, 5))
  MAETs <- unlist(lapply(results, `[[`, 6))
  MSETs <- unlist(lapply(results, `[[`, 7))
  RMSETs <- unlist(lapply(results, `[[`, 8))
  DEVTs <- unlist(lapply(results, `[[`, 9))
  RAWs <- lapply(results, `[[`, 10)
  RAWTs <- lapply(results, `[[`, 11)
  return(list(Bs = Bs, MAEs = MAEs, MSEs = MSEs, RMSEs = RMSEs, DEVs = DEVs, MAETs = MAETs,
              MSETs = MSETs, RMSETs = RMSETs, DEVTs = DEVTs, RAWs = RAWs, RAWTs = RAWTs))
}


# cint_poll =============================================================================

cint_poll <- function(z, p, n) {
  se <- sqrt(p*(1-p)/n)
  ci_bot <- p - z*se
  ci_top <- p + z*se
  cis <- data.frame(mean = p, top = ci_top, bot = ci_bot)
  colnames(cis) <- c("mean", paste0("top_", z), paste0("bot_", z))
  return(cis)
}
 