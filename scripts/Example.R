## PROCESS GLSS DATA

## SETUP
###############################################################################################################################

# Clear workspace  
  rm(list=ls())

# Clear console 
  cat("\014")

# Install requisite packages (if needed)
  # install.packages(c("ggplot2", "dplyr", "tidyr", "raster", "magrittr", "foreign", "readstata13", "plyr", "rlist"))

# Load requisite packages
  library(ggplot2); library(dplyr); library(tidyr); library(plyr); library(Hmisc)
  library(raster); library(magrittr); library(foreign); library(readstata13); library(rlist)
  extract <- raster::extract # Ensure the 'magrittr' package does not mask the 'raster' package's 'extract' function

# Specify directory paths
  rootpath <- "C:/Users/cmaue/Dropbox/E-IPER/2016-17/QualsProposal/CH3_GhanaOP/Data/Survey_Data"
  input_glss <- paste0(rootpath,"/input/GLSS/Raw_Data")
  output_glss <- paste0(rootpath,"/output/GLSS")
  setwd(rootpath)

# ----

## WAVE 3 - GLSS 1991
###############################################################################################################################

# [1] Create list of dataframes from raw data using read.dta ----
  
  setwd(paste0(input_glss, "/GLSS3_1991"))
  files <- list.files(pattern = "\\.dta$", ignore.case=TRUE) 
  file.heads <- gsub( ".DTA", "", files )
  glss3 <- vector("list", length(files))
  for (i in 1:length(files)){
    glss3[[i]] <- read.dta13(files[i])
    names(glss3)[i] <- paste0(file.heads[i])
  }
  
# ----
  
  
  