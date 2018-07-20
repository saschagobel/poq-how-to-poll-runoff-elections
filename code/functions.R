# ---------------------------------------------------------------------------------------
# INSTANT RUNOFF FORECASTING
# Peter Selb, Romain Lachat, and Sascha Göbel
# R script written by Sascha Göbel and Peter Selb
# Functions script
# July 2017
# ---------------------------------------------------------------------------------------


#### BUILD FUNCTIONS ====================================================================

# replicate probability sampling with polling bias and simulation envelop ---------------
replicatePpsSampling <- function(k, m, Nh, Vj, ph, sim, data) {
                   # set function parameters
                     # k = number of samples to be drawn (numeric of length 1) in one
                     #     simulation run
                     # m = sample size (numeric of length 1 or more for varying sample
                     #     size)
                     # Nh = number of voters at polling places (numeric of length == 
                     #      number of polling places)
                     # Vj = candidates' national vote shares (numeric of length ==
                     #      number of candidates)
                     # ph = first order inclusion probabilities (numeric or list of 
                     #      numerics with varying sample size of length == number
                     #      of polling places and m)
                     # sim = number of times the process should be repeated (numeric of
                     #       length 1)
                     # data = data.frame with electoral data on polling place level
  Bklist <- NULL
            # prepare target for storing results in a list
  progress <- winProgressBar(label = "varying sample sizes (m)", min = 0,
                             max = length(m), width = 400)
              # set up progress bar for varying sample size
  for(h in 1:length(m)) {
  # set up loop to repeat the process for varying sample size
    m2 <- m[h]
          # store sample size of current iteration
    ph2 <- ph[[h]]
           # store inclusion probabilities for sample size of current iteration
    Bsim <- rep(NA, times = sim)
            # prepare target for storing the measure of polling bias for the chosen 
            # sample over repeated simulations
    allsamples <- rep(list(NA), times = sim)
                  # prepare target for storing the indices of all k sampled polling places
                  # over repeated simulations
    samplesim <- rep(list(NA), times = sim)
                 # prepare target for storing the indices of the sampled polling places
                 # over repeated simulations
    progress2 <- winProgressBar(label = "simulating sample draws", min = 0,
                                max = max(sim), width = 400)
                 # set up progress bar for simulations
    for(l in 1:sim) {
    # set up loop for simulations
      sample <- rep(list(matrix(NA)), times = k)
                # prepare target for storing drawn samples
      Bk <- rep(NA, times = k) 
            # prepare target for storing measure of polling bias for drawn samples
      for(i in 1:k) {
      # set up loop for samples drawn in one simulation run
        sample[[i]] <- sampford(size = Nh, n = m2)
                       # draw a PPS sample of m polling places with PPS of Nh using
                       # Sampford's method
        sample2 <- merge(data.frame(polling_station_unique = data[!duplicated(data$polling_station_unique), ]$polling_station_unique[sample[[i]]], ph = ph2[sample[[i]]]), 
        data, by = "polling_station_unique")
                   # merge sampled polling places with respective inclusion probabilities
                   # (ph) and candidate data
        sample2 <- svydesign(id = ~1, probs = sample2$ph, data = sample2)
                   # specify as complex survey design object
        Njk <- rep(NA, times = max(data$candidate_number))
               # prepare target for storing candidates' national vote totals for current
               # sample
        for(j in 1:length(Njk)) {
        # set up loop for different candidates
          Njk[j] <- svytotal(~votes, subset(sample2, candidate_number == j))[1]
                    # compute current candidate's national vote total (Njk)
        }
        Nk <- svytotal(~valid_votes, subset(sample2, !duplicated(polling_station_unique)))[1]
              # compute total number of voters (Nk)
        vj <- Njk/Nk
              # compute candidates' national first round vote share
        Aj <- log((vj/(1-vj))*((1-Vj)/Vj))
              # compute logged odds ratio measure of candidate-specific bias
        Bk[i] <- sum(Vj*abs(Aj))
                 # compute measure of polling bias for current sample
      }
      Bsim[l] <- min(Bk)
                 # among the k samples drawn in the current simulation run, find and 
                 # store the smallest measure of polling bias 
      samplesim[[l]] <- sample[[which(Bk == min(Bk))]]
                        # among the k samples drawn in the current simulation run, find 
                        # and store the indices of the sampled polling places for the 
                        # sample with the smallest measure of polling bias
      allsamples[[l]] <- sample
                         # store the indices of all k sampled polling places in the 
                         # current simulation run
      setWinProgressBar(progress2, l, 
                        title = paste(round(l/max(sim)*100, 0), 
                                      "% done", "(sim =",  l, 
                                      "/", max(sim), ")"))
      # update progress bar for simulation runs
    }
    if(is.null(Bklist)) {
    # if the target for storing function results is empty
      Bklist <- Bsim
                # store measures of polling bias for samples chosen over repeated 
                # simulations
      samplelist <- samplesim
                    # store indices of polling places for samples chosen over repeated
                    # simulations
      allsampleslist <- allsamples
                        # store indices of polling places sampled over repeated
                        # simulations
    } else {
      #otherwise
      Bklist <- c(Bklist, Bsim)
                # add measures of polling bias for samples chosen over repeated 
                # simulations to already stored measures
      samplelist <- c(samplelist, samplesim)
                    # add indices of polling places for samples chosen over repeated
                    # simulations to already stored indices
      allsampleslist <- c(allsampleslist, allsamples)
                        # add indices of polling places sampled chosen over repeated
                        # simulations to already stored indices
    }
    setWinProgressBar(progress, h, 
                      title = paste(round(h/length(m)*100, 0), 
                                    "% done", "(m =",  m[h], 
                                    "/", max(m), ")"))
    # update progress bar for varying sample size
    close(progress2)
    # close progress bar for simulation runs
  }
  close(progress)
  # close progress bar for varying sample size
  return(list(Bklist, samplelist, allsampleslist))
  # return a list with measures of polling bias for chosen samples and indices of 
  # sampled polling places
}

# validation of replicate probability sampling ------------------------------------------
validation <- function(m, Vj, ph, sample, data1, data2) {
              # set function parameters
                # m = sample size (numeric of length 1 or more for varying sample
                #     size)
                # Vj = candidates' national vote shares (numeric of length ==
                #      number of candidates)
                # ph = first order inclusion probabilities (numeric or list of 
                #      numerics with varying sample size of length == number
                #      of polling places and m)
                # sample = sample generated with tiedPpsSampling
                # data1 = data.frame with electoral data on polling place level used with
                #         tiedPpsSampling
                # data2 = data.frame with electoral data on polling place level at 
                #         time != data1, column polling_station_unique must match in
                #         data1 and data2
  ph2 <- rep(x = 1:length(m), each = length(sample))
         # define calls to ph
  Bk <- rep(NA, times = length(sample))
        # prepare target for storing computed measure of polling bias
  progress <- winProgressBar(label = "evaluation", min = 0,
                             max = length(sample), width = 400)
              # set up progress bar for different samples
  for(i in 1:length(sample)) {
    # set up loop for different polling stations in sample
    pp <- data1[!duplicated(data1$polling_station_unique), ]$polling_station_unique[sample[[i]]]
          # select ith polling place in sample from data1
    sample2 <- merge(data.frame(polling_station_unique = data1[!duplicated(data1$polling_station_unique),]$polling_station_unique, ph = ph[[ph2[i]]]), data2[data2$polling_station_unique %in% pp,], by = "polling_station_unique")
               # merge polling place and ph in sample from data 1 with respective data in 
               # data2 
    sample2 <- svydesign(id = ~1, probs = sample2$ph, data = sample2)
               # specify as complex survey design object
    Njk <- rep(NA, times = max(data2$candidate_number))
           # prepare target for storing candidates' national vote totals for current
           # sample
    for(j in 1:length(Njk)) {
      # set up loop for different candidates
      Njk[j] <- svytotal(~votes, subset(sample2, candidate_number == j))[1]
                # compute current candidate's national vote total (Njk)
    }
    Nk <- svytotal(~valid_votes, subset(sample2, !duplicated(polling_station_unique)))[1]
          # compute total number of voters (Nk)
    vj <- Njk/Nk
          # compute candidates' national first round vote share
    Aj <- log((vj/(1-vj))*((1-Vj)/Vj))
          # compute logged odds ratio measure of candidate-specific bias
    Bk[i] <- sum(Vj*abs(Aj))
             # compute measure of polling bias for current sample
    setWinProgressBar(progress, i, 
                      title = paste(round(i/length(sample)*100, 0), 
                                    "% done", "sample",  i, 
                                    "/", length(sample), ")"))
      # update progress bar for different samples
  }
  close(progress)
    # close progress bar
  return(Bk)
  # return a list with measures of polling bias at time data2 for chosen samples at time
  # data1
}

# confidence intervals for polls --------------------------------------------------------
cint_poll <- function(z, p, n) {
  se <- sqrt(p*(1-p)/n)
  ci_bot <- p - z*se
  ci_top <- p + z*se
  cis <- data.frame(mean = p, top = ci_top, bot = ci_bot)
  colnames(cis) <- c("mean", paste0("top_", z), paste0("bot_", z))
  return(cis)
}
 