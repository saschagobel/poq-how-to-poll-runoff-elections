# ---------------------------------------------------------------------------------------
# HOW TO POLL RUNOFF ELECTIONS
# Peter Selb, Sascha Goebel, and Romain Lachat
# Packages script
# April 2020
# ---------------------------------------------------------------------------------------


#### INSTALL AND LOAD PACKAGES ==========================================================

# install pacman package if not installed -----------------------------------------------
suppressWarnings(if (!require("pacman")) install.packages("pacman"))

# load packages and install if not installed --------------------------------------------
pacman::p_load(stringr, # version 1.2.0
               openxlsx, # version 4.0.17
               plyr, # version 1.8.4
               dplyr, # version 0.5.0
               data.table, # version 1.10.4
               magrittr, # version 1.5
               mice, # version 2.30
               survey, # version 3.31-5
               sampling, # version 2.8
               pps, # version 0.94
               ggplot2, # version 2.2.1
               extrafont, # version 0.17
               foreach, # version 1.4.4
               doParallel, # version 1.0.14
               crayon, # version 1.3.4
               tidyr,
       install = TRUE, 
       update = FALSE)

# show loaded packages ------------------------------------------------------------------
cat("loaded packages\n")
print(pacman::p_loaded())
