##########################################################
##                       LIBRARIES                      ##
##########################################################
# clear workspacce
rm(list=ls())

# install packages if not installed already
required_packages <- c("tidyverse", "readr", "ggplot2", "ggpubr", "readxl", 
                       "Hmisc", "lme4", "rempsyc", "report", "smplot2",
                       "gridExtra", "ggalluvial", "FSA", "rcompanion")

# Install any missing packages
lapply(required_packages, function(pkg) {
  if (!pkg %in% installed.packages()[,"Package"]) {
    install.packages(pkg)
  }
})
rm(required_packages)

# load tidyverse
library(tidyverse)
library(readr)
library(ggplot2)
library(ggpubr)
library(readxl)
library(lme4)
library(lmerTest)
library(rempsyc)
library(report)
library(smplot2)
library(gridExtra)
library(viridis)
library(ggalluvial)

##########################################################
##                       SETTINGS                       ##
##########################################################

# set seed for reproducible results
set.seed(27)

# set general folder using the current location
workspace <- getwd()
setwd('../../..')
repoPath <- getwd()

# set name of script with functions
funScript <- "funs.R"

# load functions into workspace
source(paste(workspace, funScript, sep = "/"))

# set names of folders for input and output files, set output filename
inputFolder <- paste(workspace, "input", sep = "/")
outputFolder <- paste(workspace, "output", sep = "/")
modelingFolder <- file.path(repoPath, 'scripts/modelling/')
logFolder <- file.path(repoPath, 'data/logfiles/')
allLogFolder <- file.path(repoPath, 'data/logfiles/')

# set data input folder as default location of input files
setwd(workspace)


##########################################################
##                      PROCESSING                      ##
##########################################################

# 01: Prepare Data for Analyses --------------------------------------------
# load and merge the data files
source(paste(workspace, "1_prepare.R", sep = "/"))

# 02: Analyse Adults vs children ------------------------------------------
# explore and analyse the data
source(paste(workspace, "2_analyse_ad_vs_ch.R", sep = "/"))

# 03: Analyse discrimintative choice vs match recognition-------------------
# explore and analyse the data in children
source(paste(workspace, "3_analyse_discChoice_vs_matchRecog.R", sep = "/"))

# 04: Simulation and Parameter Recovery -------------------
# evaluation of simulation and parameter recovery from modelling
source(paste(workspace, "4_simulationOutput.R", sep = "/"))

# 05: modelling output children -------------------
# modelling output and analyses in children
source(paste(workspace, "5_modelingOutput.R", sep = "/"))

# 06: modelling output adults -------------------
# prepare modelling output adults
source(paste(workspace, "6_modelingOutput_adults.R", sep = "/"))

# 07: statistical analyses modeling parameters adults vs children -------------------
# compare modelling parameters between adults and children
source(paste(workspace, "7_modeling_AvC.R", sep = "/"))

# 08: surprise analyes -------------------
# analysing surprise
source(paste(workspace, "8_modeling_AvC.R", sep = "/"))

# 09: ROI analyses -------------------
# ROI analyses for both Adults vs Children, and Disc Choice vs Match Recog
source(paste(workspace, "9_ROI.R", sep = "/"))

# 10: Plots tactile frequencies -------------------
# plotting frequency patterns for tactile stimulation
source(paste(workspace, "10_plots.R", sep = "/"))




