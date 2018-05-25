## PROCESS GLSS6 DATA

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
  raw_data <- paste0(rootpath,"/input/GLSS/Raw_Data/GLSS6_2012.2013/STATA")
  output_glss <- paste0(rootpath,"/output/GLSS")
  setwd(rootpath)

# ----

## WAVE 6 - GLSS 2012-13
###############################################################################################################################

  
  
  # [1] Create list of dataframes from raw data using read.dta ----
    
    setwd(raw_data)
    r <- 1
    glss6 <- vector("list")
    directories <- list.dirs(path = raw_data, full.names = FALSE, recursive = FALSE)
    for (i in 1:length(directories)){
      setwd(paste0(raw_data,"/",directories[i],sep=""))
      files <- list.files(pattern = "\\.dta$", ignore.case=TRUE) 
      file.heads <- gsub( ".dta", "", files )
      for (j in 1:length(files)){
        glss6[[r]] <- read.dta13(files[j])
        names(glss6)[r] <- paste0(directories[i],".",file.heads[j],sep="")
        r <- r+1 
      }
    }

  # ----
  
  # [2] Individual-level dataset ----
    
    # Identify individual-level dataframe names
    # See "GLSS6_SectionNotes' on Google Drive 
      ind.list <- c("PARTA.SEC1", "PARTA.SEC2a","PARTA.SEC2b", 
                    "PARTA.SEC2c","PARTA.SEC3a","PARTA.SEC3b",
                    "PARTA.SEC3c","PARTA.SEC3d","PARTA.SEC3e",
                    "PARTA.SEC4a","PARTA.SEC4b","PARTA.SEC4c",
                    "PARTA.SEC4d","PARTA.SEC4e","PARTA.SEC4g",
                    "PARTA.SEC4g","PARTA.SEC4h","PARTA.SEC4hs",
                    "PARTA.SEC5a","PARTA.SEC5b","AGGREGATES.INC_WAGE_PID") 
      ind.names <- c("hh.roster","education.gen","education.career", 
                     "education.literacy","health.14d","health.insure",
                     "health.prevnt","health.fert","health.birthc.HIV",
                     "labor.mainjob.7d","labor.2ndjob.7d","labor.underempl.7d",
                     "labor.unempl.7d","labor.mainjob.12m","labor.2ndjob.12m",
                     "labor.jobsearch.12m","labor.housekeep.7d","labor.safety.12m",
                     "migration","tourism","wage.income")
    
    # For the subset of `glss6' dataframes with names in `ind.list`, create `indid` = individual identifier
    # and remove other identifiers (for clarity in merge below)
      ind.dfs <- glss6[names(glss6) %in% ind.list]
    
    # Create clust and nh vars for `AGGREGATES.INC_WAGE_PID'`
      ind.dfs[[1]]$clust <- as.numeric(substr(ind.dfs[[1]]$HID,1,5))
      ind.dfs[[1]]$nh <- as.numeric(substr(ind.dfs[[1]]$HID,7,8))
      indidgen <- function(df){
        df$indid <- 10000*df$clust + 100*df$nh + df$PID
        drops <- c("HID","PID","nh","clust")
        df <- df[, !(names(df)) %in% drops]
        return(df)
      }
      invisible(ind.dfs <- list.apply(ind.dfs,indidgen))
      
    # Create `individual` dataframe by merging dfs in `ind.dfs' on `indid`
      individual <- ind.dfs[[1]]
      for (i in 2:length(ind.dfs)){ 
        individual <- merge.data.frame(individual, ind.dfs[[i]],
                                       by.x = "indid", by.y = "indid",
                                       all = TRUE, sort = TRUE,
                                       suffixes = c("", paste0(".",names(ind.dfs)[i],sep="")),
                                       no.dups=TRUE)
      }
    
    # Reconstruct other identifiers 
      # clust = digits 1-5 of indid 
      individual$clust <- as.numeric(substr(individual$indid, 1, 5))
      # nh = digits 6-7 of indid 
      individual$nh <- as.numeric(substr(individual$indid, 6, 7))
      # pid = digits 8-9 of indid 
      individual$PID <- as.numeric(substr(individual$indid, 8, 9))
      # HID = digits 1-5 of indid / nh 
      individual$nhs <- as.character(individual$nh)
      individual$nhS[individual$nh<10] <- paste0("0",individual$nh,sep="")
      individual$HID <- paste0(as.character(individual$clust),"/",individual$nhs,sep="")
      drops <- "nhs"
      individual <- individual[,!(names(individual)) %in% drops]
    
    # Reorder dataset for readability AND
    # Create variables to indicate Q's from each section 
      for (i in 1:length(ind.list)){
        individual[[paste0(names(ind.dfs)[i])]] <- ind.names[i]
      }
      # Create variables to indicate `region' variables
      individual$region <- "regions"
      # Reorder
      varorder <- c()
      individual <- individual[,varorder]
  
  # ----    