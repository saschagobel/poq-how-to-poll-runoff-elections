# ---------------------------------------------------------------------------------------
# INSTANT RUNOFF FORECASTING
# Peter Selb, Romain Lachat, and Sascha Göbel
# R script written by Sascha Göbel and Peter Selb
# Figures script
# July 2017
# ---------------------------------------------------------------------------------------


# imports -------------------------------------------------------------------------------
c("./data/euclidean-2002-2007", "./data/euclidean-2007-2002", 
  "./data/euclidean-2007-2012", "./data/euclidean-2012-2007",
  "./data/euclidean-2012-2017", "./data/euclidean-2017-2012", "./data/Bs_val",
  "./data/validation_sample", "./data/Bs_k", "./data/Bs_m", "./data/polling-places.csv",
  "./data/resp_data_sc2_2", "./data/forecast-results/votes_sc1_2_post",
  "./data/forecast-results/votes_sc2_2_post", 
  "./data/forecast-results/abstentions_sc1_2_post", 
  "./data/forecast-results/abstentions_sc2_2_post", 
  "./data/forecast-results/shares_sc1_2_post",
  "./data/forecast-results/shares_sc2_2_post",
  "./data/Opt", "./data/id_posterior2")


# exports -------------------------------------------------------------------------------
c("./figures/distance.png", "./figures/voters.png", "./figures/validation.png",
  "./figures/choicek.png", "./figures/choicem.png", "./figures/map.png",
  "./figures/forecast.png", "./figures/forecast-polls.png", "/figures/prefmap.png", 
  "./figures/spatial.png")


#### PREPARATIONS =======================================================================

# clear workspace -----------------------------------------------------------------------
rm(list=ls(all=TRUE))

# set working directory -----------------------------------------------------------------
setwd("TOP LEVEL DIRECTORY")

# install and load packages -------------------------------------------------------------
source("./code/packages.R")
source("./code/functions.R")

# import fonts --------------------------------------------------------------------------
# to get the fonts as in the paper, run (on Windows OS):
# font_import()
# loadfonts()
# loadfonts(device = "postscript")
# loadfonts(device = "win")


#### SCATTERPLOT OF DISTANCE BETWEEN POLLING PLACE AND NATIONAL VOTE SHARES =============

# prepare data for ggplot ---------------------------------------------------------------
eu_dist_2002_2007 <- readRDS("./data/euclidean-2002-2007")
eu_dist_2007_2002 <- readRDS("./data/euclidean-2007-2002")
eu_dist_2007_2012 <- readRDS("./data/euclidean-2007-2012")
eu_dist_2012_2007 <- readRDS("./data/euclidean-2012-2007")
eu_dist_2012_2017 <- readRDS("./data/euclidean-2012-2017")
eu_dist_2017_2012 <- readRDS("./data/euclidean-2017-2012")
                     # import euclidean distances between polling place and national 
                     # vote shares
dist_2002_2007 <- merge(eu_dist_2002_2007, eu_dist_2007_2002)
                  # merge 2002 with 2007 data
dist_2007_2012 <- merge(eu_dist_2007_2012, eu_dist_2012_2007)
                  # merge 2007 with 2012 data
dist_2012_2017 <- merge(eu_dist_2012_2017, eu_dist_2017_2012)
                  # merge 2012 with 2017 data
dist_2002_2007$pair <- "2002 - 2007"
dist_2007_2012$pair <- "2007 - 2012"
dist_2012_2017$pair <- "2012 - 2017"
                       # create column identifying the election pair
colnames(dist_2002_2007) <- c("polling_station_unique", "votersx", "euclidx", "votersy", 
                              "euclidy", "pair")
colnames(dist_2007_2012) <- c("polling_station_unique", "votersx", "euclidx", "votersy", 
                              "euclidy", "pair")
colnames(dist_2012_2017) <- c("polling_station_unique", "votersx", "euclidx", "votersy", 
                              "euclidy", "pair")
                            # specify column names
dist <- rbind(dist_2002_2007, dist_2007_2012, dist_2012_2017)
        # stack data

# build plot ----------------------------------------------------------------------------
distance <- ggplot() +
            # create a new plot
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  # display diagonal reference line
  geom_point(data = dist, aes(x = euclidx, y = euclidy), shape = 1, alpha = 0.3) +
  # display euclidean distances
  facet_wrap(~pair) +
  # display plots by election pair
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12),
        # specify axis title size and margin
        strip.background = element_blank(),
        # remove box around plot titles
        strip.text = element_text(size=15)) +
        # specify plot title size
  labs(x = "distance at previous election", y = "distance at subsequent election") +
  # specify axis labels
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0), 
                     breaks = c(10, 30, 50, 70, 90)) +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0), 
                     breaks = c(10, 30, 50, 70, 90))
  # specify axis appearance

# save plot as .png ---------------------------------------------------------------------
ggsave("./figures/distance.png", distance, width = 8, height = 3.4, dpi = 600)


#### SCATTERPLOT OF VOTERS AT POLLING PLACES ============================================

# prepare data for ggplot ---------------------------------------------------------------
dist <- dist[-c(22522,33014),]
        # remove extreme outliers

# build plot ----------------------------------------------------------------------------
voters <- ggplot() +
          # create a new plot
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  # display diagonal reference line
  geom_point(data = dist, aes(x = votersx, y = votersy), shape = 1, alpha = 0.3) +
  # display voters at polling place
  facet_wrap(~pair) +
  # display plots by election pair
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black"),
        # specify axis title size and margin
        strip.background = element_blank(),
        # remove box around plot titles
        strip.text = element_text(size=15)) +
        # specify plot title size
  labs(x = "voters at previous election", y = "voters at subsequent election") +
  # specify axis labels
  scale_y_continuous(limits = c(0, 2550), expand = c(0, 0), 
                     breaks = c(500, 1000, 1500, 2000)) +
  scale_x_continuous(limits = c(0, 2550), expand = c(0, 0), 
                     breaks = c(500, 1000, 1500, 2000))
  # specify axis appearance

# save plot as .png ---------------------------------------------------------------------
ggsave("./figures/voters.png", voters, width = 8, height = 3.4, dpi = 600)


#### HISTOGRAM OF POLLING BIAS OF 2012 SAMPLES IN 2017 ==================================

# prepare data for ggplot ---------------------------------------------------------------
Bs_val <- data.frame(Bs = readRDS("./data/Bs_val"))
          # import 2017 polling bias for 4001 samples selected in 2012
validation_sample <- readRDS("./data/validation_sample")
                     # import the 4001 samples selected in 2012
minB <- which(unlist(validation_sample[[3]], recursive = FALSE) %in% validation_sample[[2]])
        # locate the 20 samples with minimum B
Bs_min <- Bs_val[minB,]
          # retrieve Bias B of the 20 samples
rugdat <- data.frame(x = c(Bs_min))
          # prepare Bias B of 20 samples for display as rug
colors <- c(rep("black", 20))
chosen <- data.frame(x = 0.06177438, y = 0, xend = 0.06177438, yend = 13)

# build plot ----------------------------------------------------------------------------
validation <- ggplot() + 
              # create a new plot
  geom_histogram(data = Bs_val, aes(x = Bs), breaks = seq(0.01,0.3, by = 0.004), 
                 colour = "white", fill = "gray60") +
  # display distribution of polling bias across 4001 samples
  #geom_vline(data = rugdat, aes(xintercept = x), color = "black", size = 0.2, linetype = "solid", alpha = 0.5) +
  #annotate("rect", xmin = 0.02065895, xmax = 0.11251356, ymin = 0, ymax = 200,
  #         alpha = .2, fill = "gray60") +
  geom_vline(xintercept = 0.08486617, color = "black", linetype = "dotdash") +
  geom_vline(xintercept = 0.05430233, color = "black", linetype = "longdash") +
  # display median polling bias
  # geom_vline(xintercept = 0.06177438, color = "red", linetype = "longdash") +
  # display polling bias of the selected sample
  geom_rug(data = rugdat, aes(x = x), sides = "b", color = colors, alpha = 0.8, lwd = 0.5) +
  geom_segment(data = chosen, aes(x = x, y = y, xend = xend, yend = yend), color = "black", lwd = 0.7, lineend = "round") +
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black")) +
        # specify axis title size and margin
  labs(x = expression(paste("Composite bias ", italic("B"))), 
       y = "Frequency") +
  # specify axis labels
  scale_y_continuous(limits = c(0, 200), expand = c(0, 0), 
                     breaks = seq(20,180,20)) +
  scale_x_continuous(limits = c(0, 0.3), expand = c(0, 0), 
                     breaks = seq(0.02,0.28, by = 0.02))
  # specify axis appearance

# save plot as .png ---------------------------------------------------------------------
ggsave("./figures/validation.png", validation, width = 7, height = 4.5, dpi = 600)


#### BOXPLOTS OF 2012 POLLING BIAS WITH VARYING K WHEN SAMPLING FROM 2012 ===============

# prepare data for ggplot ---------------------------------------------------------------
kvar <- seq(0,300, by = 5) %>% replace(1, 1)
        # specify range and variation in k
Bs_k <- data.frame(B = unlist(readRDS("./data/Bs_k")), k = factor(rep(kvar, each = 100)))
        # import 2012 polling bias for samples with kvar varying k
colors <- c(rep("gray60",40), rep("black",1), rep("gray60", 20))
          # specify color of boxplots

# build plot ----------------------------------------------------------------------------
choicek <- ggplot(data = Bs_k, aes(x = k, y = B)) +
           # create a new plot and specify data
  geom_boxplot(color = colors, outlier.shape = 1) +
  # display outliers and whiskers
  geom_boxplot(aes(ymin = ..lower.., ymax = ..upper..), colour = "white", fill = colors,
               outlier.shape=NA) +
  # display boxes
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif", color = "black"),
        # specify font family
        panel.border = element_rect(fill=NA, color = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black")) +
        # specify axis title size and margin
  labs(x = expression(paste("Number of replications ", italic("k"))), 
       y = expression(paste("Bias ", italic("B")," in 2012"))) +
  # specify axis labels
  scale_y_continuous(limits = c(0,0.1), expand = c(0, 0), 
                     breaks = seq(.01,.09,.01)) +
  scale_x_discrete(expand = c(0.001, 0.5), breaks = seq(10,290, by = 20))
  # specify axis appearance

# save plot as .png ---------------------------------------------------------------------
ggsave("./figures/choicek.png", choicek, width = 7, height = 4.5, dpi = 600)


#### BOXPLOTS OF 2012 POLLING BIAS WITH VARYING M WHEN SAMPLING FROM 2012 ===============

# prepare data for ggplot ---------------------------------------------------------------
m <- seq(2,50, by = 1)
     # specify range and variation in m
Bs_m <- split(readRDS("./data/Bs_m")[[1]], 
              ceiling(seq_along(readRDS("./data/Bs_m")[[1]])/100))
Bs_m <- data.frame(B = unlist(Bs_m), m = factor(rep(m, each = 100)))
        # import 2012 polling bias for samples with varying m
colors <- c(rep("gray60",18), rep("black",1), rep("gray60", 30))
          # specify color of boxplots

# build plot ----------------------------------------------------------------------------
choicem <- ggplot(data = Bs_m, aes(x = m, y = B)) +
           # create a new plot and specify data
  geom_boxplot(color = colors, outlier.shape = 1) +
  # display outliers and whiskers
  geom_boxplot(aes(ymin = ..lower.., ymax = ..upper..), colour = "white", fill = colors,
               outlier.shape=NA) +
  # display boxes
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black")) +
        # specify axis title size and margin
  labs(x = expression(paste("Sample size ", italic("m"))), 
       y = expression(paste("Bias ", italic("B")," in 2012"))) +
  # specify axis labels
  scale_y_continuous(limits = c(0,0.1), expand = c(0, 0), 
                     breaks = seq(.01,.09,.01)) +
  scale_x_discrete(expand = c(0.001, 0.5), breaks = seq(5,45, by = 5))
  # specify axis appearance

# save plot as .png ---------------------------------------------------------------------
ggsave("./figures/choicem.png", choicem, width = 7, height = 4.5, dpi = 600)


#### MAP OF SAMPLED POLLING PLACES ======================================================

# prepare data for ggplot ---------------------------------------------------------------
france_departments <- getData('GADM', country='FRA', level=2)
                      # import map of france with departmental boundaries
france_departments <- fortify(france_departments, region = "NAME_2")
                      # translate spatial polygons object into data frame
departments_sample <- c("Bouches-du-Rhône", "Côtes-d'Armor", "Haute-Garonne", "Gironde",
                        "Indre", "Maine-et-Loire", "Meurthe-et-Moselle", "Moselle",
                        "Oise", "Orne", "Rhône", "Haute-Savoie", "Seine-Maritime", "Var",
                        "Hauts-de-Seine", "Seine-Saint-Denis")
                      # pass names of sampled departments
france_departments$sample <- france_departments$id %in% departments_sample
                             # find sampled polling places and assign TRUE
france_municipalities <- read.csv2("./data/polling-places.csv") %>%
                         # import polling-places.csv
  use_series(municipality_name) %>%
  # select column with municipality names
  unique %>%
  # select unique names
  as.character %>%
  # translate to character vector
  str_replace_all("è|é", "e") %>%
  str_replace_all("ç", "c") %>%
  # adjust encoding
  geocode
  # geocode municipalities

# build plot ----------------------------------------------------------------------------
map_sample <- ggplot() +
              # create a new plot
  geom_polygon(data = france_departments, 
               aes(x = long, y = lat, 
  #                fill = sample,
                   group = group), 
               color = "gray40", fill = "white", size = 0.2) +
  # display map with departmental boundaries
  # scale_fill_manual(values = c("gray60", "black")) +
  # make sampled departments black and other gray
  geom_point(data = france_municipalities, 
             aes(x = lon, y = lat, group = NA), 
             color = "white", fill = "black", shape = 21, stroke = 0.3, size = 2) +
  # display location of sampled polling places
  coord_map() +
  # set correct projection
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        legend.position = "none",
        # remove legend
        text = element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black")) +
        # specify axis title size and margin
  labs(x = "Longitude", y = "Latitude") +
  # specify axis labels
  scale_y_continuous(limits = c(42, 51.2), expand = c(0, 0), breaks = seq(from = 42.5, to = 50,2.5)) +
  scale_x_continuous(limits = c(-5.3, 8.3), expand = c(0, 0), breaks = seq(from = -5, to = 10, by = 2.5)) +
  # specify axis appearance
  annotate("text", x = 5.5, y = 43.9, label = "Eygalières", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 6.15, y = 43.51, label = "Salon-de-Provence", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = -4.05, y = 48.95, label = "Trégastel", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 2.9, y = 43.25, label = "Avignonet-Lauragais", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 2.15, y = 43.8, label = "Castelginest", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 0.85, y = 43.38, label = "Pinsaguel", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = -0.25, y = 44.68, label = "Pessac", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 2.06, y = 47.02, label = "Valençay", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 1.35, y = 47.5, label = "Charcé-Saint-Ellier-sur-Aubance", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 6.55, y = 48.5, label = "Ludres", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 6.6, y = 49.23, label = "Terville", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 2.95, y = 49.48, label = "Bailleval", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = -0.1, y = 48.65, label = "Tinchebray", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 4.47, y = 45.85, label = "Lyon", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 6.95, y = 46.18, label = "Cluses", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 2.2, y = 49.9, label = "Aumale", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 5.5, y = 43, label = "Toulon", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 1.2, y = 49, label = "Rueil-Malmaison", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 2.98, y = 48.98, label = "Montreuil", family="CMU Serif", 
           size = 3, color = "black") +
  annotate("text", x = 3.4, y = 48.75, label = "Noisy-le-Grand", family="CMU Serif", 
           size = 3, color = "black")

# save plot as .png ---------------------------------------------------------------------
ggsave("./figures/map.png", map_sample, width = 7, height = 7, dpi = 600)


#### ALLUVIAL GRAPH OF VOTE TRANSITIONS (WITH IMPUTATION) ===============================

# prepare data for ggplot ---------------------------------------------------------------
resp_data_sc2 <- readRDS("./data/resp_data_sc2_2")
                 # import data used for forecasting  
vote_stream_sc2_post <- readRDS("./data/forecast-results/vote_stream_sc2_2_post")
                        # import forecasted vote transitions
abstention_stream_sc2_post <- readRDS("./data/forecast-results/abstention_stream_sc2_2_post")
                              # import forecasted abstentions
transitions <- data.frame(fr_candidates = factor(rep(c(colnames(resp_data_sc2[,c(8:13)]), 
                                                   "Others"), 3)),
                          # compile first-round candidate names
                      votes = c(vote_stream_sc2_post$choiceMacron[-c(1,2,9,10,11)], 
                                     sum(vote_stream_sc2_post$choiceMacron[c(1,2,9,10,11)]), 
                                     vote_stream_sc2_post$choiceLePen[-c(1,2,9,10,11)], 
                                     sum(vote_stream_sc2_post$choiceLePen[c(1,2,9,10,11)]), 
                                     abstention_stream_sc2_post$abstain[-c(1,2,9,10,11)], 
                                     sum(abstention_stream_sc2_post$abstain[c(1,2,9,10,11)])), 
                      # compile vote transitions from first-round voters to Macron, 
                      # Le Pen and Invalid/blank/abstention
                      sr_candidates = factor(rep(c("Macron", "Le Pen", 
                                                   "Invalid,\nblank,\nabstention"), 
                                                 each = 7)))
                      # compile second-round candidate names and category for invalid/
                      # blank/abstention
transitions$fr_candidates <- factor(transitions$fr_candidates, 
                                    levels = c("Others", "Aignan", "Hamon", "Melenchon", 
                                               "Fillon", "LePen", "Macron"))
                             # specify order of first-round candidate names in plot
levels(transitions$fr_candidates)[levels(transitions$fr_candidates) == "LePen"] <- "Le Pen"
                                 # specify spelling of "Le Pen"
levels(transitions$fr_candidates)[levels(transitions$fr_candidates) == "Melenchon"] <- "Mélenchon"
                                 # specify spelling of "Melenchon"

# build plot ----------------------------------------------------------------------------
par(family = "CMU Serif")
  # specify font family
alluvial(transitions[,c(1,3)], 
         # specify from-to columns
         freq = transitions$votes, 
         # specify column with vote frequencies
         hide = transitions$votes <1, 
         # hide empty rows
         cex = 1.5, 
         # specify scaling of fonts
         cex.axis = 1.5, 
         # specify scaling of axis labels
         axis_labels = c("First-round", "Second-round"), 
         # specify axis labels
         blocks = TRUE,
         # specify appearance of from-to columns
         col = ifelse(transitions$sr_candidates == "Macron", "black",
                      ifelse(transitions$sr_candidates == "Le Pen", "gray60", "coral")),
         border = ifelse(transitions$sr_candidates == "Macron", "white",
                        ifelse(transitions$sr_candidates == "Le Pen", "white", "white")),
         # specify colors
         alpha = .35)
         # specify degree of transparency


#### GRAPH OF FORECASTED VOTE TOTALS ----------------------------------------------------

# prepare data for ggplot ---------------------------------------------------------------
votes_sc1_2_post <- readRDS("./data/forecast-results/votes_sc1_2_post")
votes_sc2_2_post <- readRDS("./data/forecast-results/votes_sc2_2_post")
abstentions_sc1_2_post <- readRDS("./data/forecast-results/abstentions_sc1_2_post")
abstentions_sc2_2_post <- readRDS("./data/forecast-results/abstentions_sc2_2_post")
                          # import forecasted vote totals and abstentions
votes <- data.frame(vote_total = c(votes_sc1_2_post[1:2,1], abstentions_sc1_2_post[1,1], 
                                   votes_sc2_2_post[1:2,1], abstentions_sc2_2_post[1,1]),
                    # compile forecasted vote totals
                     bot90 = c(votes_sc1_2_post[1:2,4], abstentions_sc1_2_post[1,4],
                               votes_sc2_2_post[1:2,4], abstentions_sc2_2_post[1,4]),
                     top90 = c(votes_sc1_2_post[1:2,5], abstentions_sc1_2_post[1,5],
                               votes_sc2_2_post[1:2,5], abstentions_sc2_2_post[1,5]),
                     bot95 = c(votes_sc1_2_post[1:2,6], abstentions_sc1_2_post[1,6],
                               votes_sc2_2_post[1:2,6], abstentions_sc2_2_post[1,6]),
                     top95 = c(votes_sc1_2_post[1:2,7], abstentions_sc1_2_post[1,7],
                               votes_sc2_2_post[1:2,7], abstentions_sc2_2_post[1,7]),
                     bot99 = c(votes_sc1_2_post[1:2,8], abstentions_sc1_2_post[1,8],
                               votes_sc2_2_post[1:2,8], abstentions_sc2_2_post[1,8]),
                     top99 = c(votes_sc1_2_post[1:2,9], abstentions_sc1_2_post[1,9],
                               votes_sc2_2_post[1:2,9], abstentions_sc2_2_post[1,9]),
                     # compile confidence intervals
                     scenario = factor(c(rep("No imputation", 3), 
                                         rep("Imputation", 3))),
                     id = factor(c(2, 3, 1, 2, 3, 1)))

# build plot with forecast --------------------------------------------------------------
forecast <- ggplot(data = votes, aes(y = vote_total, x = scenario, group = id)) +
  #  create a new plot
  geom_hline(yintercept = 20743128, color = "gray60", linetype = "longdash") +
#  # display official result for Macron
  geom_hline(yintercept = 10638475, color = "gray60", linetype = "longdash") +
  # display official result for Le Pen
  geom_hline(yintercept = 4672791, color = "gray60", linetype = "longdash") +
  # display official abstention rate
  geom_errorbar(aes(ymax = top90, ymin = bot90, 
                                    width = 0), size = .9, position=position_dodge(width = .3)) +
  geom_errorbar(aes(ymax = top95, ymin = bot95, 
                                    width = 0), size = .6, position=position_dodge(width = .3)) +
  geom_errorbar(aes(ymax = top99, ymin = bot99, 
                                    width = 0), size = .3, position=position_dodge(width = .3)) +
  # display confidence intervals for forecasts
  geom_point(shape = c(16, 17, 15, 16, 17, 15), size = 2, position=position_dodge(width = .3)) +
  # display forecasts
  coord_flip()+
  # flip axis (necessary to make position dodge on discrete scale work)
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10, color = "black"),
        axis.text.y = element_text(margin = unit(c(0,1.5,0,0), "mm"), angle = 90, hjust = 0.5),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        # specify axis title size and margin
        plot.title = element_text(lineheight=.8, size = 10, color = "black")) +
        # specify plot title size and position
  labs(y = "Vote total", x = "") +
  # specify axis labels
  ggtitle("Abstention                                Le Pen                                                                   Macron") +
  # display plot titles
  scale_y_continuous(expand = c(0, 0), limits = c(4000000,22000000), 
                     labels = c("5M", "7M", "9M", "11M", "13M", "15M", "17M", "19M", "21M"),
                     breaks = c(5000000, 7000000, 9000000, 11000000, 13000000, 15000000,
                                17000000, 19000000, 21000000))
# specify axis appearance

# save plot as .png ---------------------------------------------------------------------
ggsave("./figures/forecast.png", forecast, width = 7, height = 4, dpi = 600)


#### GRAPH OF COMPARISON OF FORECAST WITH POLLS =========================================

# prepare data for ggplot ---------------------------------------------------------------
shares_sc1_2_post <- readRDS("./data/forecast-results/shares_sc1_2_post")
shares_sc2_2_post <- readRDS("./data/forecast-results/shares_sc2_2_post")
                     # import forecasted vote shares
p <- c(.63, .62, .63, .62, .615, .62, .62, .61, .61, .61, .60, .60, .60, .59, .595, .59,
       .60, .59, .59, .61, .60, .59, .60, .59, .60, .61, .605, .59, .605, .63, .60, .61,
       .64, .61, .60, .62, .64)		
     # pass predictions from polls after first-round
     # source: https://en.wikipedia.org/wiki/Opinion_polling_for_the_French_presidential_election,_2017#Opinion_polls_for_the_second_round_of_voting
n <- c(5331, 2270, 1861, 959, 1605, 1009, 2264, 1400, 2349, 2264, 1405, 1435, 1936, 3817, 
       1388, 8936, 1764, 1385, 1539, 1488, 918, 1438, 1399, 968, 1790, 940, 1407, 1800,
       1893, 1000, 2828, 1416, 967, 2222, 846, 1379, 2684)
     # pass sample size of polls
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
            # pass pollsters
cis <- lapply(c(1.645, 1.96, 2.576), cint_poll, p = p, n = n)
       # compute confidence intervals of pollster predictions
forecasts <- cbind(cis[[1]], cis[[2]][,-1], cis[[3]][,-1]) %>%
             # compile confidence intervals of pollster predictions
  rbind(as.numeric(shares_sc2_2_post[2,c(1,5,4,7,6,9,8)]),
        as.numeric(shares_sc1_2_post[2,c(1,5,4,7,6,9,8)]),
        .) %>%
  # add forecasts on top
  cbind(pollster = pollster) %>%
  # add column with pollster names
  arrange(desc(mean)) %>%
  # sort in descending oder of prediction
  cbind(poll = factor(nrow(.):1))
  # add id factor
colors <- c("black", rep("gray60",2), "black", rep("gray60", 35))
          # specify colors for plotting predictions

# build plot ----------------------------------------------------------------------------
forecast_polls <- ggplot() +
        # create a new plot
  geom_vline(xintercept = 0.661, color = "gray60", linetype = "longdash") +
  # display official result for Macron
  geom_errorbarh(data = forecasts, aes(y = forecasts$poll, x = mean,  xmax = top_1.645, 
                                       xmin = bot_1.645, 
                                    height = 0), size = .9,colour = colors) +
  geom_errorbarh(data = forecasts, aes(y = forecasts$poll, x = mean,  xmax = top_1.96, 
                                       xmin = bot_1.96, 
                                    height = 0), size = .6, colour = colors) +
  geom_errorbarh(data = forecasts, aes(y = forecasts$poll, x = mean,  xmax = top_2.576, 
                                       xmin = bot_2.576, 
                                    height = 0), size = .3, colour = colors) +
  # display confidence intervals for forecasts and poll predictions
  geom_point(data = forecasts, aes(x = forecasts$mean, y = forecasts$poll), size = 2, 
             colour = colors) +
  # display forecasts and poll predictions
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks.x = element_line(color = "black", size =.25),
        axis.ticks.y = element_blank(), 
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_blank(),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_blank()) +
        # specify axis title size and margin
  labs(x = "Second-round vote share", y = NULL) +
  # specify axis labels 
  scale_x_continuous(expand = c(0, 0), limits = c(.50,.75), 
                     breaks = seq(0.55, 0.70, by = .05)) +
  scale_y_discrete(expand = c(0, 1)) +
  # specify axis appearance
  annotate("text", x = .72, y = 39.0, label = "Imputation", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 38.1, label = "Elabe (Apr 24)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 37.1, label = "Harris (Apr 23)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .72, y = 36.0, label = "No imputation", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 35.1, label = "Ipsos (May 5)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 34.1, label = "Ifop-Fiducial (May 2-5)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 33.1, label = "Odoxa (Apr 24-25)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 32.1, label = "Harris (May 4-5)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 31.1, label = "Odoxa (May 4)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 30.1, label = "Elabe (May 4)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 29.1, label = "OpinionWay (May 2-4)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 28.1, label = "Ipsos (Apr 23)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 27.1, label = "Ipsos (May 4)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 26.1, label = "Ifop-Fiducial (May 1-4)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 25.1, label = "Harris (May 2-3)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 24.1, label = "OpinionWay (May 1-3)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 23.1, label = "OpinionWay (Apr 28-30)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 22.1, label = "Harris (Apr 25-27)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 21.1, label = "Ifop-Fiducial (Apr 23–25)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 20.1, label = "OpinionWay (Apr 23–24)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 19.1, label = "Ifop-Fiducial (Apr 24-27)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 18.1, label = "Ifop-Fiducial (Apr 23–26)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 17.1, label = "Ifop-Fiducial (Apr 30-May 3)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 16.1, label = "BVA (May 1-2)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 15.1, label = "OpinionWay (Apr 30–May 2)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 14.1, label = "OpinionWay (Apr 29–May 1)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 13.1, label = "Ipsos (Apr 28-29)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 12.1, label = "Ifop-Fiducial (Apr 25-28)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 11.1, label = "OpinionWay (Apr 25-27)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 10.1, label = "OpinionWay (Apr 23–25)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 9.1, label = "Ifop-Fiducial (Apr 23–24)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 8.1, label = "Ifop-Fiducial (Apr 28–May 2)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 7.1, label = "Elabe (Apr 28–May 2)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 6.1, label = "Ipsos (Apr 30–May 1)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 5.1, label = "Ifop-Fiducia (Apr 27–May 1)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 4.1, label = "Kantar Sofres (Apr 28-30)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 3.1, label = "BVA (Apr 26-28)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 2.1, label = "Odoxa (Apr 26-27)", family="CMU Serif", 
           size = 2) +
  annotate("text", x = .53, y = 1.1, label = "OpinionWay (Apr 24–26)", family="CMU Serif", 
           size = 2)
  # display name of pollsters and date of polls

# save plot as .png ---------------------------------------------------------------------
ggsave("./figures/forecast-polls.png", forecast_polls, width = 7, height = 5, 
       dpi = 600)


#### GRAPH OF OPTIMAL NUMBER OF SAMPLE ELEMENTS PER PSU =================================

# prepare data for ggplot ---------------------------------------------------------------
Opt <- readRDS("./data/Opt")
Opt <- data.frame(n = Opt$n, CV = Opt$CV)

# build plot ----------------------------------------------------------------------------
opt <- ggplot() +
  geom_line(data = Opt, aes(x = Opt$n, y = Opt$CV), color = "black", size = 0.5) +
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black")) +
        # specify axis title size and margin
  labs(x = "Average sample size per polling place", y = "Coefficient of variation") +
  # specify axis labels 
  scale_y_continuous(expand = c(0, 0), limits = c(0.053,0.102), 
                     breaks = seq(0.05, 0.1, by = .01)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0,303),
                     breaks = seq(20,180, by = 20)) +
  coord_cartesian(xlim = c(0, 200))

# save plot as .png ---------------------------------------------------------------------
ggsave("./figures/opt.png", opt, width = 7, height = 4.5, 
       dpi = 600)


#### GRAPH OF BIVARIATE PREFERENCES DISTRIBUTION FOR RUNOFF CANDIDATES ==================

# prepare data for ggplot ---------------------------------------------------------------
prefcomb <- readRDS("./data/resp_data")[,c(1,8,11,15,18,19,20)] %>%
  # import data with all 1540 respondents
  mutate_all(funs(replace(., is.na(.), 0))) %>%
  # replace don't knows with 0
  filter(choice != 0) %>%
  # remove voters that did not indicate their vote choice in round 1, 1391 remaining
  mutate(inter = interaction(.$Macron, .$LePen)) %>%
  # create factor variable of preference combinations
  cbind(., model.matrix( ~ inter - 1, data = .))
  # create dummy for each preference combinations
cand_data <- readRDS("./data/cand_data")
             # import official first round vote totals for poststratification
ppsdesign1 <- svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = 
                          prefcomb)
              # specify multistage PPS survey design
ppsdesign_post1 <- postStratify(design = ppsdesign1, strata = ~choice, 
                                     population = cand_data)
                   # adjust survey design via poststratification
form <- as.formula(paste0("~", (paste(colnames(prefcomb[,9:72]), collapse = " + "))))
        # prepare formula with linear combination of all preference combinations (64)
prefs <- svytotal(form, design = ppsdesign_post1, na.rm = TRUE, deff = TRUE)
         # estimate population totals
prefs <- data.frame(Macron = as.factor(rep(0:7, times = 8)), LePen = as.factor(rep(0:7, each = 8)), 
                    Freq = as.data.frame(prefs)[,1])
         # prepare dataframe for heatmap
prefcomb2 <- readRDS("./data/resp_data_sc2")[,c(1,3,10,13,17,20,21,22)] %>%
             # import imputed data (1391*50)
  mutate(inter = interaction(.$Macron, .$LePen)) %>%
  # create factor variable of preference combinations
  cbind(., model.matrix( ~ inter - 1, data = .))
  # create dummy for each preference combinations
cand_data <- readRDS("./data/cand_data")
             # import official first round vote totals for poststratification
ppsdesign2 <- split(prefcomb2, f = prefcomb2$.imp) %>%
              # split prefcomb into a list of imputed data frames
  lapply(function(x) {
    svydesign(id = ~id + resp_id, ~first_stage + second_stage, data = x)
    })
    # specify list of multistage PPS survey designs
ppsdesign_post2 <- lapply(ppsdesign2, function(x) {
  postStratify(design = x, strata = ~choice, population = cand_data)
  })
  # adjust list of survey designs via poststratification
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

# built plot ----------------------------------------------------------------------------
prefmap <- ggplot() +
           # create a new plot
  geom_tile(data = prefs, aes(x = LePen, y = Macron, fill = Freq), color = "gray40", size = 0.5) +
  # display preference distribution in tiles
  scale_fill_gradient(low = "white", high = "black") +
  # colour tiles accroding to frecuency of preference combination
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_blank(),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black"),
        legend.title = element_text(size=10)) +
  # specify axis title size and margin
  labs(x = "Le Pen", y = "Macron", fill = "Estimated \npopulation\ntotal") +
  # specify axis labels 
  scale_y_discrete(expand = c(0, 0), labels = c("DK", "1", "2", "3", "4", "5", "6", "7")) +
  scale_x_discrete(expand = c(0, 0), labels = c("DK", "1", "2", "3", "4", "5", "6", "7")) +
  ggtitle("                   No Imputation") +
  coord_fixed()
prefmap2 <- ggplot() +
  # create a new plot
  geom_tile(data = prefs2, aes(x = LePen, y = Macron, fill = Freq), color = "gray40", size = 0.5) +
  # display preference distribution in tiles
  scale_fill_gradient(low = "white", high = "black") +
  # colour tiles accroding to frecuency of preference combination
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_blank(),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black"),
        legend.title = element_text(size=10)) +
  # specify axis title size and margin
  labs(x = "Le Pen", y = "Macron", fill = "Estimated \npopulation\ntotal") +
  # specify axis labels 
  scale_y_discrete(expand = c(0, 0), labels = c("DK", "1", "2", "3", "4", "5", "6", "7")) +
  scale_x_discrete(expand = c(0, 0), labels = c("DK", "1", "2", "3", "4", "5", "6", "7")) +
  ggtitle("                     Imputation") +
  coord_fixed()
prefmap3 <- ggplot() +
  # create a new plot
  geom_tile(data = prefs3, aes(x = LePen, y = Macron, fill = Freq), color = "gray40", size = 0.5) +
  # display preference distribution in tiles
  scale_fill_gradient(low = "white", high = "black") +
  # colour tiles accroding to frecuency of preference combination
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_blank(),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black"),
        legend.title = element_text(size=10)) +
  # specify axis title size and margin
  labs(x = "Le Pen", y = "Macron", fill = "Estimated \npopulation\ntotal") +
  # specify axis labels 
  scale_y_discrete(expand = c(0, 0), labels = c("DK", "1", "2", "3", "4", "5", "6", "7")) +
  scale_x_discrete(expand = c(0, 0), labels = c("DK", "1", "2", "3", "4", "5", "6", "7")) +
  ggtitle("                    Difference") +
  coord_fixed()

# save plot as .png ---------------------------------------------------------------------
ggsave("./figures/prefmap.png", arrangeGrob(prefmap, prefmap2, prefmap3, ncol = 3), width = 14.5, height = 4, 
       dpi = 600)


#### GRAPH OF ESTIMATED CANDIDATE POSITIONS =============================================

# prepare data for ggplot ---------------------------------------------------------------
id_posterior2 <- readRDS("./data/id_posterior2")
                 # import estimates
id_posterior2$beta.id[,,1] <- id_posterior2$beta.id[,,1]*-1 
                              # flip dimension if necessary
selected_iter <- sample(1:(nrow(id_posterior2$beta.id)), 300, replace = TRUE)
                 # draw a sample of estimates and select indices
dens <- data.frame(x = stack(data.frame(x = id_posterior2$beta.id[selected_iter, , 1]))[,1],
                   y = stack(data.frame(y = id_posterior2$beta.id[selected_iter, , 2]))[,1],
                   id = as.factor(rep(1:11, each = 300)))
        # stack columns (candidate-specific estimates) and assign candidate id

# build plot ----------------------------------------------------------------------------
spatial <- ggplot(dens, aes(x = x, y = y)) +
           # create a new plot
  geom_density_2d(aes(color = id, alpha = ..level..), h = rep(0.1, 2), size = 0.3) +
  # display contour of two-dimensional kernel density of candidate positions
  scale_color_manual(values = rep("black", 11)) +
  # specify colors of candidate-specific colors
  theme_bw() +
  # set black and white theme
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # remove grids
        text=element_text(family="CMU Serif"),
        # specify font family
        panel.border = element_rect(fill=NA, colour = "black", size=.5),
        # create panel borders
        axis.ticks = element_line(color = "black", size =.25),
        axis.ticks.length = unit(0.25, "cm"),
        # specify appearance of ticks
        axis.text = element_text(size = 10, color = "black"),
        # specify axis text size
        axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm"), size = 12, color = "black"),
        axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"), size = 12, color = "black"),
        legend.position="none") +
  # specify axis title size and margin
  labs(x = "statist \u2013 pro-market", 
       y = "conservative \u2013 liberal") +
  # specify axis labels
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

# save plot as .png ---------------------------------------------------------------------
ggsave("./figures/spatial.png", spatial, width = 4.5, height = 4.5, dpi = 1200)