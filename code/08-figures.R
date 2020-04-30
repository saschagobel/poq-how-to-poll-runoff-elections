# ---------------------------------------------------------------------------------------
# HOW TO POLL RUNOFF ELECTIONS
# Peter Selb, Sascha Goebel, and Romain Lachat
# Script for figures
# April 2020
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
cat(underline("IMPORTS"),"
  './data/all_sample_metrics'
  './data/best_sample_metrics'
  './data/official-electoral-data-processed/fpe2017_r1_ov'
  './data/forecast-results/shares_sc1_post'
  './data/forecast-results/shares_sc2_post'
  './data/id_posterior2'
  './data/simulations_k'
  './data/resp_data'
  './data/cand_data'
  './data/resp_data_sc2'
")

# exports -------------------------------------------------------------------------------
cat(underline("EXPORTS"),"
  './figures/sims.png'
  './figures/forecast-polls.png'
  './figures/spatial.png'
  './figures/choicek.png'
  './figures/prefmap.png'
")

# content -------------------------------------------------------------------------------
cat(underline("CONTENT"),"
  Line 43 - PREPARATIONS
  Line 75 - FIGURE 1
  Line 212 - FIGURE 2
  Line 374 - FIGURE B1
  Line 434 - FIGURE C1
  Line 469 - FIGURE C3
")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("poq-how-to-poll-runoff-elections")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")

# import fonts --------------------------------------------------------------------------
# to get the fonts as in the paper, run (on Windows OS):
# font_import()
# loadfonts()
# loadfonts(device = "postscript")
# loadfonts(device = "win")

# read data from disk -------------------------------------------------------------------
all_sample_metrics <-  readRDS("./data/all_sample_metrics")
best_sample_metrics <-  readRDS("./data/best_sample_metrics")
fpe2017_r1_ov <- readRDS("./data/official-electoral-data-processed/fpe2017_r1_ov")
shares_sc1_post <- readRDS("./data/forecast-results/shares_sc1_post")
shares_sc2_post <- readRDS("./data/forecast-results/shares_sc2_post")
id_posterior2 <- readRDS("./data/id_posterior2")
simulations_k <- readRDS("./data/simulations_k")
resp_data <- readRDS("./data/resp_data")
cand_data <- readRDS("./data/cand_data")
resp_data_sc2 <- readRDS("./data/resp_data_sc2")


#### FIGURE 1 ===========================================================================
# Standard PPS versus balanced samples: simulation results

# compute actual vote shares ------------------------------------------------------------
N_2017_r1_ov <- sum(fpe2017_r1_ov$votes)
Vj_2017_r1_ov <- fpe2017_r1_ov %>% 
  group_by(as.numeric(candidate_number)) %>% 
  summarise(sum(votes)) %>% 
  extract2(2) %>% 
  divide_by(N_2017_r1_ov)

# extract deviations --------------------------------------------------------------------
all_sample_devs <- all_sample_metrics[[10]]
best_sample_devs <- best_sample_metrics[[10]][-1001]

# compute estimates ---------------------------------------------------------------------
all_sample_estimates <- lapply(all_sample_devs, `+`, Vj_2017_r1_ov)
best_sample_estimates <- lapply(best_sample_devs, `+`, Vj_2017_r1_ov)

# prepare data for figure ---------------------------------------------------------------
all_sample_dev_1 <- unlist(lapply(all_sample_estimates, `[[`,1))
all_sample_dev_2 <- unlist(lapply(all_sample_estimates, `[[`,2))
all_sample_dev_3 <- unlist(lapply(all_sample_estimates, `[[`,3))
all_sample_dev_4 <- unlist(lapply(all_sample_estimates, `[[`,4))
all_sample_dev_5 <- unlist(lapply(all_sample_estimates, `[[`,5))
all_sample_dev_6 <- unlist(lapply(all_sample_estimates, `[[`,6))
all_sample_dev_7 <- unlist(lapply(all_sample_estimates, `[[`,7))
all_sample_dev_8 <- unlist(lapply(all_sample_estimates, `[[`,8))
all_sample_dev_9 <- unlist(lapply(all_sample_estimates, `[[`,9))
all_sample_dev_10 <- unlist(lapply(all_sample_estimates, `[[`,10))
all_sample_dev_11 <- unlist(lapply(all_sample_estimates, `[[`,11))
best_sample_dev_1 <- unlist(lapply(best_sample_estimates, `[[`,1))
best_sample_dev_2 <- unlist(lapply(best_sample_estimates, `[[`,2))
best_sample_dev_3 <- unlist(lapply(best_sample_estimates, `[[`,3))
best_sample_dev_4 <- unlist(lapply(best_sample_estimates, `[[`,4))
best_sample_dev_5 <- unlist(lapply(best_sample_estimates, `[[`,5))
best_sample_dev_6 <- unlist(lapply(best_sample_estimates, `[[`,6))
best_sample_dev_7 <- unlist(lapply(best_sample_estimates, `[[`,7))
best_sample_dev_8 <- unlist(lapply(best_sample_estimates, `[[`,8))
best_sample_dev_9 <- unlist(lapply(best_sample_estimates, `[[`,9))
best_sample_dev_10 <- unlist(lapply(best_sample_estimates, `[[`,10))
best_sample_dev_11 <- unlist(lapply(best_sample_estimates, `[[`,11))
shares <- data.frame(share = c(all_sample_dev_1, all_sample_dev_2, all_sample_dev_3,
                               all_sample_dev_4, all_sample_dev_5, all_sample_dev_6,
                               all_sample_dev_7, all_sample_dev_8, all_sample_dev_9,
                               all_sample_dev_10, all_sample_dev_11, best_sample_dev_1,
                               best_sample_dev_2, best_sample_dev_3, best_sample_dev_4,
                               best_sample_dev_5, best_sample_dev_6, best_sample_dev_7,
                               best_sample_dev_8, best_sample_dev_9, best_sample_dev_10,
                               best_sample_dev_11),
                     sample = factor(c(rep("all samples", 200000*11), 
                                       rep("selected samples", 1000*11))),
                     candidates = factor(c(rep("Dupont-Aignan",200000),
                                           rep("Le Pen",200000),
                                           rep("Macron",200000),
                                           rep("Hamon",200000),
                                           rep("Arthaud",200000),
                                           rep("Poutou",200000),
                                           rep("Cheminade",200000),
                                           rep("Lassalle",200000),
                                           rep("Mélenchon",200000),
                                           rep("Asselineau",200000),
                                           rep("Fillon",200000),
                                           rep("Dupont-Aignan",1000),
                                           rep("Le Pen",1000),
                                           rep("Macron",1000),
                                           rep("Hamon",1000),
                                           rep("Arthaud",1000),
                                           rep("Poutou",1000),
                                           rep("Cheminade",1000),
                                           rep("Lassalle",1000),
                                           rep("Mélenchon",1000),
                                           rep("Asselineau",1000),
                                           rep("Fillon",1000))))
shares <- shares[which(shares$candidates %in% c("Le Pen","Macron","Mélenchon","Fillon")),]

# adjust to fit x scales (very few data point) ------------------------------------------
shares2 <- shares[-which(shares$candidates == "Fillon" & shares$share > 0.30),]
shares2 <- shares2[-which(shares2$candidates == "Fillon" & shares2$share < 0.14),]
shares2 <- shares2[-which(shares2$candidates == "Le Pen" & shares2$share > 0.30),]
shares2 <- shares2[-which(shares2$candidates == "Le Pen" & shares2$share < 0.14),]
shares2 <- shares2[-which(shares2$candidates == "Macron" & shares2$share > 0.30),]
shares2 <- shares2[-which(shares2$candidates == "Mélenchon" & shares2$share < 0.14),]

# adjust individual breaks --------------------------------------------------------------
shares2[which(shares2$candidates == "Fillon"),]$share %>% max
shares2[which(shares2$candidates == "Le Pen"),]$share %>% max
shares2[which(shares2$candidates == "Macron"),]$share %>% max
shares2[which(shares2$candidates == "Mélenchon"),]$share %>% max
shares2[which(shares2$candidates == "Fillon"),]$share %>% min
shares2[which(shares2$candidates == "Le Pen"),]$share %>% min
shares2[which(shares2$candidates == "Macron"),]$share %>% min
shares2[which(shares2$candidates == "Mélenchon"),]$share %>% min
shares2$share <- shares2$share*100
ind_breaks <- function(x) { if (max(x) > 20) seq(15, 30, 2) }
shares2$candidates <- factor(shares2$candidates, 
                             levels = c("Macron", "Le Pen", "Fillon", "Mélenchon"))

# build figure --------------------------------------------------------------------------
sims <- ggplot() +
  geom_density(data = shares2, aes(x = share, group = sample, fill = sample, alpha = 0.3),
               color = NA) +
  scale_fill_manual(values = c("gray70", "gray50")) +
  facet_wrap(~candidates, scales = "free") +
  geom_vline(data=filter(shares2, candidates=="Fillon"), aes(xintercept=20.0058695), colour="black",
             linetype = "longdash") +
  geom_vline(data=filter(shares2, candidates=="Le Pen"), aes(xintercept=21.2969631), colour="black",
             linetype = "longdash") +
  geom_vline(data=filter(shares2, candidates=="Macron"), aes(xintercept=24.0091291), colour="black",
             linetype = "longdash") +
  geom_vline(data=filter(shares2, candidates=="Mélenchon"), aes(xintercept=19.5813886), colour="black",
             linetype = "longdash") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        text=element_text(family="CMU Serif"),
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size=12)) +
  labs(x = "First-round vote share", 
       y = "Density") +
  scale_y_continuous(limits = c(0, .5), expand = c(0, 0), 
                     breaks = seq(0.1,0.4,0.1)) +
  scale_x_continuous(limits = c(14, 30),
                     expand = c(0, 0), 
                     breaks = ind_breaks)

# save figure ---------------------------------------------------------------------------
ggsave("./figures/sims.png", sims, width = 7, height = 4.5, dpi = 600)


#### FIGURE 2 ===========================================================================
# Poll-based forecasts of Macron’s second-round vote share.

# predictions from polls after first-round ----------------------------------------------
# source: https://en.wikipedia.org/wiki/Opinion_polling_for_the_French_presidential_election,_2017#Opinion_polls_for_the_second_round_of_voting
p <- c(.63, .62, .63, .62, .615, .62, .62, .61, .61, .61, .60, .60, .60, .59, .595, .59,
       .60, .59, .59, .61, .60, .59, .60, .59, .60, .61, .605, .59, .605, .63, .60, .61,
       .64, .61, .60, .62, .64)		

# sample size of polls ------------------------------------------------------------------
# source https://en.wikipedia.org/wiki/Opinion_polling_for_the_French_presidential_election,_2017#Opinion_polls_for_the_second_round_of_voting
n <- c(5331, 2270, 1861, 959, 1605, 1009, 2264, 1400, 2349, 2264, 1405, 1435, 1936, 3817, 
       1388, 8936, 1764, 1385, 1539, 1488, 918, 1438, 1399, 968, 1790, 940, 1407, 1800,
       1893, 1000, 2828, 1416, 967, 2222, 846, 1379, 2684)

# pollsters -----------------------------------------------------------------------------
# source https://en.wikipedia.org/wiki/Opinion_polling_for_the_French_presidential_election,_2017#Opinion_polls_for_the_second_round_of_voting
pollster <- c("with imputation", "without imputation", "Ipsos (May 5)", 
              "Harris (May 4-5)", "Ifop-Fiducial (May 2-5)", "Odoxa	(May 4)", 
              "Ipsos (May 4)", "Elabe	(May 4)", "OpinionWay	(May 2-4)",
              "Ifop-Fiducial (May 1-4)", "Harris (May 2-3)", 
              "OpinionWay	(May 1-3)", "Ifop-Fiducial (Apr 30-May 3)",
              "BVA (May 1-2)", "OpinionWay (Apr 30–May 2)",
              "Elabe (Apr 28–May 2)", "Ifop-Fiducial (Apr 28–May 2)",
              "Ipsos (Apr 30–May 1", "OpinionWay (Apr 29–May 1)",
              "Ifop-Fiducia (Apr 27–May 1)", "Kantar Sofres	(Apr 28-30)",
              "OpinionWay	(Apr 28-30)", "Ipsos (Apr 28-29)", "BVA	(Apr 26-28)",
              "Ifop-Fiducial (Apr 25-28)", "Odoxa	(Apr 26-27)", "OpinionWay	(Apr 25-27)",
              "Harris	Apr (25-27)", "Ifop-Fiducial (Apr 24-27)", "OpinionWay (Apr 24–26)",
              "Ifop-Fiducial (Apr 23–26)", "Odoxa	(Apr 24-25)", "OpinionWay	(Apr 23–25)",
              "Ifop-Fiducial (Apr 23–25)", "Elabe (Apr 24)", "OpinionWay	(Apr 23–24)",
              "Ifop-Fiducial (Apr 23–24)", "Ipsos (Apr 23)",
              "Harris	Apr 23")

# compute confidence intervals for poll predictions -------------------------------------
cis <- lapply(c(1.645, 1.96, 2.576), cint_poll, p = p, n = n)

# prepare data for figure ---------------------------------------------------------------
forecasts <- cbind(cis[[1]], cis[[2]][,-1], cis[[3]][,-1]) %>%
  rbind(as.numeric(shares_sc2_post[2,c(1,5,4,7,6,9,8)]),
        as.numeric(shares_sc1_post[2,c(1,5,4,7,6,9,8)]),
        .) %>%
  cbind(pollster = pollster) %>%
  arrange(desc(mean)) %>%
  cbind(poll = factor(nrow(.):1))
forecasts <- forecasts %>%
  mutate(mean = mean*100, top_1.645 = top_1.645*100, bot_1.645 = bot_1.645*100,
         top_1.96 = top_1.96*100, bot_1.96 = bot_1.96*100, top_2.576 = top_2.576*100,
         bot_2.576 = bot_2.576*100)
colors <- c(rep("black",1), rep("gray60", 37))
forecasts <- forecasts[-2,]

# build figure --------------------------------------------------------------------------
forecast_polls <- ggplot() +
  geom_vline(xintercept = 66.1, color = "gray60", linetype = "longdash") +
  geom_errorbarh(data = forecasts, aes(y = forecasts$poll, xmax = top_1.645, 
                                       xmin = bot_1.645, 
                                       height = 0), size = .9,colour = colors) +
  geom_errorbarh(data = forecasts, aes(y = forecasts$poll, xmax = top_1.96, 
                                       xmin = bot_1.96, 
                                       height = 0), size = .6, colour = colors) +
  geom_errorbarh(data = forecasts, aes(y = forecasts$poll, xmax = top_2.576, 
                                       xmin = bot_2.576, 
                                       height = 0), size = .3, colour = colors) +
  geom_point(data = forecasts, aes(x = forecasts$mean, y = forecasts$poll), size = 2, 
             colour = colors) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="CMU Serif"),
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        axis.ticks.x = element_line(color = "black", size =.25),
        axis.ticks.y = element_blank(), 
        axis.ticks.length = unit(0.25, "cm"),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_blank(),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_blank()) +
  labs(x = "Second-round vote share", y = NULL) +
  scale_x_continuous(expand = c(0, 0), limits = c(50,75), 
                     breaks = seq(055, 70, by = 5)) +
  scale_y_discrete(expand = c(0, 1)) +
  annotate("text", x = 53, y = 37.1, label = "Elabe (Apr 24)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 36.0, label = "Harris (Apr 23)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 35.1, label = "Ipsos (May 5)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 34.1, label = "Ifop-Fiducial (May 2-5)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 33.1, label = "Odoxa (Apr 24-25)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 32.1, label = "Harris (May 4-5)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 31.1, label = "Odoxa (May 4)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 30.1, label = "Elabe (May 4)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 29.1, label = "OpinionWay (May 2-4)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 28.1, label = "Ipsos (Apr 23)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 27.1, label = "Ipsos (May 4)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 26.1, label = "Ifop-Fiducial (May 1-4)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 25.1, label = "Harris (May 2-3)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 24.1, label = "OpinionWay (May 1-3)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 23.1, label = "OpinionWay (Apr 28-30)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 22.1, label = "Harris (Apr 25-27)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 21.1, label = "Ifop-Fiducial (Apr 23–25)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 20.1, label = "OpinionWay (Apr 23–24)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 19.1, label = "Ifop-Fiducial (Apr 24-27)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 18.1, label = "Ifop-Fiducial (Apr 23–26)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 17.1, label = "Ifop-Fiducial (Apr 30-May 3)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 16.1, label = "BVA (May 1-2)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 15.1, label = "OpinionWay (Apr 30–May 2)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 14.1, label = "OpinionWay (Apr 29–May 1)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 13.1, label = "Ipsos (Apr 28-29)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 12.1, label = "Ifop-Fiducial (Apr 25-28)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 11.1, label = "OpinionWay (Apr 25-27)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 10.1, label = "OpinionWay (Apr 23–25)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 9.1, label = "Ifop-Fiducial (Apr 23–24)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 8.1, label = "Ifop-Fiducial (Apr 28–May 2)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 7.1, label = "Elabe (Apr 28–May 2)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 6.1, label = "Ipsos (Apr 30–May 1)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 5.1, label = "Ifop-Fiducia (Apr 27–May 1)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 4.1, label = "Kantar Sofres (Apr 28-30)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 3.1, label = "BVA (Apr 26-28)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 2.1, label = "Odoxa (Apr 26-27)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = 53, y = 1.1, label = "OpinionWay (Apr 24–26)", family="CMU Serif", 
           size = 2)

# save figure ---------------------------------------------------------------------------
ggsave("./figures/forecast-polls.png", forecast_polls, width = 7, height = 5, 
       dpi = 600)


# FIGURE B1 =============================================================================
# Contour plot of the estimated candidate positions

# prepare data for figure ---------------------------------------------------------------
# flip dimension if necessary
# id_posterior2$beta.id[,,1] <- id_posterior2$beta.id[,,1]*-1 
selected_iter <- sample(1:(nrow(id_posterior2$beta.id)), 300, replace = TRUE)
# stack columns (candidate-specific estimates) and assign candidate id
dens <- data.frame(x = stack(data.frame(x = id_posterior2$beta.id[selected_iter, , 1]))[,1],
                   y = stack(data.frame(y = id_posterior2$beta.id[selected_iter, , 2]))[,1],
                   id = as.factor(rep(1:11, each = 300)))

# build figure --------------------------------------------------------------------------
spatial <- ggplot(dens, aes(x = x, y = y)) +
  geom_density_2d(aes(color = id, alpha = ..level..), h = rep(0.1, 2), size = 0.3) +
  scale_color_manual(values = rep("black", 11)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="CMU Serif"),
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black"),
        legend.position="none") +
  labs(x = "statist \u2013 pro-market", 
       y = "conservative \u2013 liberal") +
  scale_y_continuous(limits = c(-2.5,2.5), expand = c(0, 0), 
                     breaks = seq(-2,2,1)) +
  scale_x_continuous(limits = c(-2.5,2.5), expand = c(0, 0), breaks = seq(-2,2, by = 1)) +
  coord_fixed() +
  annotate("text", x = 2.2, y = 1.0, label = "Macron", family="CMU Serif", 
           size = 3) +
  annotate("text", x = -0.4, y = 0.9, label = "Arthaud", family="CMU Serif", 
           size = 3) +
  annotate("text", x = -0.75, y = 1.1, label = "Poutou", family="CMU Serif", 
           size = 3) +
  annotate("text", x = -1.8, y = 1.5, label = "Mélenchon", family="CMU Serif", 
           size = 3) +
  annotate("text", x = -0.1, y = 1.7, label = "Hamon", family="CMU Serif", 
           size = 3) +
  annotate("text", x = 1.8, y = -1.2, label = "Fillon", family="CMU Serif", 
           size = 3) +
  annotate("text", x = 0.85, y = -1.4, label = "Le Pen", family="CMU Serif", 
           size = 3) +
  annotate("text", x = 0.9, y = -0.85, label = "Aignan", family="CMU Serif", 
           size = 3) +
  annotate("text", x = 0.6, y = -0.2, label = "Cheminade", family="CMU Serif", 
           size = 3) +
  annotate("text", x = 0.4, y = -0.5, label = "Asselineau", family="CMU Serif", 
           size = 3) +
  annotate("text", x = 0.3, y = 0.2, label = "Lassalle", family="CMU Serif", 
           size = 3)

# save figure ---------------------------------------------------------------------------
ggsave("./figures/spatial.png", spatial, width = 4.5, height = 4.5, dpi = 1200)


#### FIGURE C1 ==========================================================================
# Choice of number of replications k

# prepare data for figure ---------------------------------------------------------------
kvar <- seq(0,300, by = 5) %>% replace(1, 1)
MSEs_k <- unlist(lapply(simulations_k, `[[`, 3))*100
MSEs_k <- data.frame(MSE = MSEs_k, k = factor(rep(kvar, each = 100)))
colors <- c(rep("gray60",40), rep("black",1), rep("gray60", 20))

# build figure --------------------------------------------------------------------------
choicek <- ggplot(data = MSEs_k, aes(x = k, y = MSE)) +
  geom_boxplot(color = colors, outlier.shape = 1) +
  geom_boxplot(aes(ymin = ..lower.., ymax = ..upper..), colour = "white", fill = colors,
               outlier.shape=NA) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="CMU Serif", color = "black"),
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black")) +
  labs(x = expression(paste("Number of replications ", italic("k"))), 
       y = expression(paste("MSE in 2012"))) +
  coord_cartesian(ylim=c(0, 0.010)) + 
  scale_y_continuous(expand = c(0, 0), 
                     breaks = seq(.001,.014,.001)) +
  scale_x_discrete(expand = c(0.001, 0.5), breaks = seq(10,290, by = 20))

# save figure ---------------------------------------------------------------------------
ggsave("./figures/choicek.png", choicek, width = 7, height = 4.5, dpi = 600)


#### FIGURE C3 ==========================================================================
# Bivariate distribution of Macron and Le Pen ratings before and after imputation

# prepare data for figure ---------------------------------------------------------------
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
prefcomb <- resp_data[,c(1,8,11,15,18,19,20)] %>%
  replace_na(list(Macron = 0, LePen = 0, choice = 0)) %>%
  filter(choice != 0) %>%
  mutate(inter = interaction(.$Macron, .$LePen)) %>%
  cbind(., model.matrix( ~ inter - 1, data = .))
ppsdesign1 <- svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = 
                          prefcomb)
ppsdesign_post1 <- postStratify(design = ppsdesign1, strata = ~choice, 
                                population = cand_data)
form <- as.formula(paste0("~", (paste(colnames(prefcomb[,9:72]), collapse = " + "))))
prefs <- svytotal(form, design = ppsdesign_post1, na.rm = TRUE, deff = TRUE)
prefs <- data.frame(Macron = as.factor(rep(0:7, times = 8)), LePen = as.factor(rep(0:7, each = 8)), 
                    Freq = as.data.frame(prefs)[,1])
prefcomb2 <- resp_data_sc2[,c(1,3,10,13,17,20,21,22)] %>%
  filter(!(is.na(choice))) %>%
  mutate(inter = interaction(.$Macron, .$LePen)) %>%
  cbind(., model.matrix( ~ inter - 1, data = .))
ppsdesign2 <- split(prefcomb2, f = prefcomb2$.imp) %>%
  lapply(function(x) {
    svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = x)
  })
ppsdesign_post2 <- lapply(ppsdesign2, function(x) {
  postStratify(design = x, strata = ~choice, population = cand_data)
})
form2 <- as.formula(paste0("~", (paste(colnames(prefcomb2[,10:58]), collapse = " + "))))
prefs2 <- lapply(ppsdesign_post2, function(x) {
  svytotal(form2, design = x, na.rm = TRUE, deff = TRUE)
})
combination <- rep(NA, 49)
for (i in 1:length(combination)) {
  combination[i] <- mean(unlist(lapply(prefs2, function(x) data.frame(x)[i,1])))
}
prefs2 <- data.frame(Macron = as.factor(rep(1:7, times = 7)), LePen = as.factor(rep(1:7, each = 7)), 
                     Freq = combination)
prefs2 <- rbind(prefs2, data.frame(Macron = as.factor(1:7), 
                                   LePen = as.factor(rep(0, times = 7)), 
                                   Freq = rep(0, times = 7)),
                data.frame(Macron = as.factor(rep(0, times = 8)),
                           LePen = as.factor(0:7),
                           Freq = rep(0, times = 8))) %>%
  mutate(Macron = as.numeric(Macron), LePen = as.numeric(LePen)) %>%
  mutate(Macron = ifelse(Macron == 8, 0, Macron), LePen = ifelse(LePen == 8, 0, LePen)) %>%
  arrange(LePen, Macron) %>%
  mutate(Macron = as.factor(Macron), LePen = as.factor(LePen))
prefs3 <- prefs2 %>%
  mutate(Freq = prefs2$Freq - prefs$Freq) %>%
  mutate(Freq = ifelse(Freq < 0, 0, Freq))

# build figure --------------------------------------------------------------------------
prefmap <- ggplot() +
  geom_tile(data = prefs, aes(x = LePen, y = Macron, fill = Freq), color = "gray40", size = 0.5) +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="CMU Serif"),
        panel.border = element_blank(),
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black"),
        legend.title = element_text(size=10)) +
  labs(x = "Le Pen", y = "Macron", fill = "Estimated \npopulation\ntotal") +
  scale_y_discrete(expand = c(0, 0), labels = c("DK", "1", "2", "3", "4", "5", "6", "7")) +
  scale_x_discrete(expand = c(0, 0), labels = c("DK", "1", "2", "3", "4", "5", "6", "7")) +
  ggtitle("                   No Imputation") +
  coord_fixed()
prefmap2 <- ggplot() +
  geom_tile(data = prefs2, aes(x = LePen, y = Macron, fill = Freq), color = "gray40", size = 0.5) +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="CMU Serif"),
        panel.border = element_blank(),
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black"),
        legend.title = element_text(size=10)) +
  labs(x = "Le Pen", y = "Macron", fill = "Estimated \npopulation\ntotal") +
  scale_y_discrete(expand = c(0, 0), labels = c("DK", "1", "2", "3", "4", "5", "6", "7")) +
  scale_x_discrete(expand = c(0, 0), labels = c("DK", "1", "2", "3", "4", "5", "6", "7")) +
  ggtitle("                     Imputation") +
  coord_fixed()
prefmap3 <- ggplot() +
  geom_tile(data = prefs3, aes(x = LePen, y = Macron, fill = Freq), color = "gray40", size = 0.5) +
  scale_fill_gradient(low = "white", high = "black") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text=element_text(family="CMU Serif"),
        panel.border = element_blank(),
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text = element_text(size = 10, color = "black"),
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black"),
        legend.title = element_text(size=10)) +
  labs(x = "Le Pen", y = "Macron", fill = "Estimated \npopulation\ntotal") +
  scale_y_discrete(expand = c(0, 0), labels = c("DK", "1", "2", "3", "4", "5", "6", "7")) +
  scale_x_discrete(expand = c(0, 0), labels = c("DK", "1", "2", "3", "4", "5", "6", "7")) +
  ggtitle("                    Difference") +
  coord_fixed()

# save figure ---------------------------------------------------------------------------
ggsave("./figures/prefmap.png", arrangeGrob(prefmap, prefmap2, prefmap3, ncol = 3), width = 14.5, height = 4, 
       dpi = 600)