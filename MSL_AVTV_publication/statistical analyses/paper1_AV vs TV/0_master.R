##########################################################
##                       LIBRARIES                      ##
##########################################################
# clear workspacce
rm(list=ls())

# install packages if not installed already
required_packages <- c("tidyverse", "readr", "ggplot2", "ggpubr", "readxl", 
                       "Hmisc", "plyr", "lme4", "rempsyc", "report", "smplot2",
                       "gridExtra", "viridis")

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
#library(plyr)
library(rempsyc)
library(report)
library(smplot2)
library(gridExtra)
library(viridis)

##########################################################
##                       SETTINGS                       ##
##########################################################

# set seed for reproducible results
set.seed(27)

# set general folder using the current location
workspace <- getwd()

# set name of script with functions
funScript <- "funs.R"

# load functions into workspace
source(paste(workspace, funScript, sep = "/"))

# set names of folders for input and output files, set output filename
inputFolder <- paste(workspace, "input", sep = "/")
outputFolder <- paste(workspace, "output", sep = "/")
modelingFolder <- file.path('A:/Projects/04-03-ChildBrainCircuits/Analyses/Modeling/CBC_Modeling_v3_paper1/analysed')
logFolder <- 'B:/Data/CBC_Data/analyses/multisensory_nr/preprocessing/paper1_AVvsTV/'

# set data input folder as default location of input files
setwd(workspace)


##########################################################
##                      PROCESSING                      ##
##########################################################

# 01: Read Redcap export --------------------------------------------
# load and merge the data files
source(paste(workspace, "1_prepare.R", sep = "/"))

# 02: Overview over MR Runs --------------------------------------------
# load and merge the data files
source(paste(workspace, "1_prepare.R", sep = "/"))

# 03: Analyse ------------------------------------------
# explore and analyse the data
# source(paste(workspace, "3_analyse.R", sep = "/"))
