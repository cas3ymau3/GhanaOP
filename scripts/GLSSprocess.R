## PROCESS GLSS DATA

## SETUP
###############################################################################################################################
# NOTE: This script borrows from Neal Jean's `ProcessSurveyData.R` file and `StudyLocation_GIS.R`

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

## WAVE 1 - GLSS 1987-88
###############################################################################################################################
    
  # [1] Create list of dataframes from raw data using read.dta ----
    setwd(paste0(input_glss, "/GLSS1_1987.1988"))
    files <- list.files(pattern = "\\.dta$", ignore.case=TRUE) 
    file.heads <- gsub( ".DTA", "", files )
    glss1 <- vector("list", length(files))
    for (i in 1:length(files)){
      glss1[[i]] <- read.dta13(files[i])
      names(glss1)[i] <- paste0(file.heads[i])
    }
  # ----
  
  # [2] Individual-level dataset ----
 
    # Identify individual-level dataframe names
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder
    
      ind.list <- c("PANELC", "Y01A", "Y01B", "Y03I", "Y03II", "Y04", "Y05A", "Y05B1", "Y05B2", 
                    "Y05B3","Y05B4", "Y05C1", "Y05C2", "Y05D", "Y05E1", "Y05E2", "Y05E3", "Y05E4", 
                   "Y05F", "Y05G1", "Y05G2", "Y05H", "Y06", "Y13A1A", "Y16A", "Y16B", "ZSCORE") 
    
      ind.names <- c("panel.ids","hh.roster","parent.info","educ.1","educ.2","health","labor.timeuse",
                     "labor.mainjob7d.1","labor.mainjob7d.2","labor.mainjob7d.3","labor.mainjob7d.4",
                     "labor.2ndjob7d.1","labor.2ndjob7d.2","labor.jobsearch.oth",
                     "labor.mainjob12m.1","labor.mainjob12m.2","labor.mainjob12m.3",
                     "labor.mainjob12m.4","labor.employhist","labor.2ndjob12m.1","labor.2ndjob12m.2",
                     "labor.otheractivity","migration","fertility.woman", "anthropometrics.1",
                     "anthropometrics.2", "cognitive.tests")
  
    # For the subset of `glss1' dataframes with names in `ind.list`, create `indid` = individual identifier
    # and remove other identifiers (for clarity in merge below)
      ind.dfs <- glss1[names(glss1) %in% ind.list]
      ind.dfs$PANELC <- rename(ind.dfs$PANELC, c("hid1"="hid", "pid1"="pid"))
      indidgen <- function(df){
        if ("pid" %in% colnames(df)){df$indid <- 100*df$hid+df$pid}
        if ("wid" %in% colnames(df)){df$indid <- 100*df$hid+df$wid}
        drops <- c("hid","pid","nh","clust")
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
      individual$indidstr <- as.character(individual$indid)
      # clust = digits 1-4 of indid 
      individual$clust <- as.numeric(substr(individual$indid, 1, 4))
      # nh = digits 5-6 of indid 
      individual$nh <- as.numeric(substr(individual$indid, 5, 6))
      # pid = digits 7-8 of indid 
      individual$pid <- as.numeric(substr(individual$indid, 7, 8))
      # hid = digits 1-6 of indid 
      individual$hid <- as.numeric(substr(individual$indid, 1, 6))
      # rename a couple variables for clarity
      colnames(individual)[colnames(individual)=="cid"] <- "cid.Y01A"
      colnames(individual)[colnames(individual)=="hipi"] <- "hipi.Y16A"
      colnames(individual)[colnames(individual)=="uniqueid"] <- "uniqueid.Y13A1A"
        
    # Reorder dataset for readability
      # Create variables to indicate Q's from each section 
      for (i in 1:length(ind.list)){
        individual[[paste0(names(ind.dfs)[i])]] <- ind.names[i]
      }
      # Create variables to indicate `ID` and `CID` variables 
      individual$IDS <- "IDS"
      individual$CIDS <- "CIDS"
      # Reorder
      varorder <- c(397,366,367,369,368,1,365, 
                    370,2,3,
                    371,5:20,
                    372,22:37,
                    373,39:57,
                    374,59:73,
                    375,75:97,
                    376,99:119,
                    377,121:135,
                    378,137:153,
                    379,155:169,
                    380,171:177,
                    381,179:194,
                    382,196:203,
                    383,205:212,
                    384,214:227,
                    385,229:245,
                    386,247:261,
                    387,263:268,
                    388,270:277,
                    389,279:291,
                    390,293:300,
                    391,302:309,
                    392,311:321,
                    393,323:327,
                    394,329:338,
                    395,340:349,
                    396,350:364,
                    398,4,21,38,58,74,98,120,136,154,170,178,
                    195,204,213,228,246,262,269,278,292,301,310,322,339)
      individual <- individual[,varorder]
      
  # ----        
          
  # [3] Household-level dataset ----
      
    # Identify individual-level dataframe names
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder
      hh.list <-c("EXPEND", "HEAD","Y00A", "Y02A", "Y02B", "Y07", "Y08", "Y09A",
                    "Y09D3B", "Y09H", "Y09J", "Y13A2", "Y15A", "Y15C")
      hh.names <-c("expenditure","hh.head","survey.info","housing.chars.1",
                   "housing.chars.2","filters","housing.chars.3","ag.land",
                   "ag.labor","ag.livestock.exten","ag.handtools","children",
                   "finance.credit","finance.saving")
      
    # For the subset of `glss1' dataframes with names in `hh.list`, remove other identifiers (for clarity 
    # in merge below)
      hh.dfs <- glss1[names(glss1) %in% hh.list]
      hhclean <- function(df){
        drops <- c("nh","clust")
        df <- df[, !(names(df)) %in% drops]
        return(df)
      }
      invisible(hh.dfs <- list.apply(hh.dfs,hhclean))
      
    # Create `household` dataframe by merging dfs in `hh.dfs' on `hid`
      household <- hh.dfs[[1]]
      for (i in 2:length(hh.dfs)){ 
        household <- merge.data.frame(household, hh.dfs[[i]],
                                      by.x = "hid", by.y = "hid",
                                      all = TRUE, sort = TRUE,
                                      suffixes = c("", paste0(".",names(hh.dfs)[i],sep="")),
                                      no.dups=TRUE)
      }
      
    # Reconstruct other identifiers 
      # hidstr = string version of `hid` variable
      household$hidstr <- as.character(household$hid)
      # clust = digits 1-4 of hid 
      household$clust <- as.numeric(substr(household$hid, 1, 4))
      # nh = digits 5-6 of indid 
      household$nh <- as.numeric(substr(household$hid, 5, 6))
      # rename a couple variables for clarity
      colnames(household)[colnames(household)=="cid"] <- "cid.Y00A"
      colnames(household)[colnames(household)=="ownhh"] <- "ownhh.EXPEND"
      colnames(household)[colnames(household)=="rented"] <- "rented.EXPEND"
      colnames(household)[colnames(household)=="mo2"] <- "mo2.EXPEND"
      colnames(household)[colnames(household)=="yr2"] <- "yr2.EXPEND"
      colnames(household)[colnames(household)=="pid"] <- "pid.HEAD"
      colnames(household)[colnames(household)=="langhd"] <- "langhd.HEAD"
      colnames(household)[colnames(household)=="religion"] <- "religion.HEAD"
      colnames(household)[colnames(household)=="dwater"] <- "dwater.HEAD"
      colnames(household)[colnames(household)=="toilet"] <- "toilet.HEAD"
      colnames(household)[colnames(household)=="light"] <- "light.HEAD"
      colnames(household)[colnames(household)=="idfarm"] <- "idfarm.Y07"
      
    # Reorder dataset for readability
      # Create variables to indicate Q's from each section 
      for (i in 1:length(hh.list)){
        household[[paste0(names(hh.dfs)[i])]] <- hh.names[i]
      }
      # Create variables to indicate `ID` and `CID` variables 
      household$IDS <- "IDS"
      household$CIDS <- "CIDS"
      # Reorder (See "GLSS 1987-88 Report.pdf" in Documentation folder for variable order) 
      varorder <- c(348,332,333,1,331,334,2:61,335,62:86,336,88:133,
                    337,135:146,338,148:199,339,201:221,340,223:228,
                    341,230:260,342,262:283,343,285:286,344,288:295,
                    345,297:313,346,315:320,347,322:330,349,87,134,
                    147,200,222,229,261,284,287,296,314,321)
      household <- household[,varorder]
  # ---- 

  # [4] Community-level dataset ----
    
    # Only 1 community-level dataset in the raw GLSS1 data 
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      comm.list <-c("COMM")
      community <- as.data.frame(glss1[names(glss1) %in% comm.list])
    
    # Rename columns 
      for (i in 1:length(colnames(community))){
        renamestr <- as.character(paste0(colnames(community)[i]))
        colnames(community)[i] <- substring(renamestr,6,nchar(renamestr))
      }
    
    # Community dataset has the following structure
      # `clust' indicates the cluster 
      # `no_clust` indicates the community within the cluster
     
    # Reconstruct `clust` identifier
      colnames(community)[colnames(community)=="clust"] <- "clust1"
      community$clust <- 1000 + community$clust1
      
    # Create unique community indentifier (`commid`)
      community$commid <- 10*community$clust + community$no_clst
    
    # Create indicator for community section 
      community$COMM <- "community"
      
    # Reorder
      community <- community[, c(312, 1, 313, 314, 2:311)]
      
    # Sample diagnostic:
      length(unique(community$commid)) # 137 communities in community dataset
      length(unique(community$clust)) # 102 clusters in community dataset
      length(unique(household$clust)) # 176 clusters in household dataset
      length(unique(individual$clust)) # 177 clusters in individual dataset
      
  # ----    
      
  # [5] Agricultural dataset - crops (household-crop level) ----
      
    # Create lists of (i) household-crop level dfs, (ii) names for these dfs,
    # and (iii) a list of the crop-identifier used in each df
      crops.list <- c("Y09B","Y09C","Y09D1A","Y09D1B","Y09D1C","Y09D2A","Y09D2B",
                        "Y09D2C","Y09D3A","Y09D4A","Y09D4B","Y09D4C","Y09D5")
      crops.names <- c("crops", "tree.age","seeds","fertilizer","manure",
                         "insecticide","transport","contracts","storage","sharecrop",
                         "share.lease","home.cons","extension")
      cropid.names <- c("crop","croptr","seedcr","fertcr","manucr","insccr","trancr",
                         "contcr","storcr","shcrcode","shlcrcod","crcdhomc","crcdae")
    
    # Identify and rename subset of `glss1' dataframes with names in `crops.list`
      crops.dfs <- glss1[names(glss1) %in% crops.list]

    # Create standardized `cropid' and `hcid' in all dataframes in `crops.dfs
      for (i in 1:length(crops.dfs)){
        colnames(crops.dfs[[i]])[colnames(crops.dfs[[i]])==cropid.names[i]] <-"cropid"
        colnames(crops.dfs[[i]])[colnames(crops.dfs[[i]])=="uniqueid"] <-"hcid"
      }
    
    # Remove duplicate identifiers 
      cropclean <- function(df){
        drops <- c("nh","clust","hid","cropid")
        df <- df[, !(names(df)) %in% drops]
        return(df)
      }
      invisible(crops.dfs <- list.apply(crops.dfs,cropclean))
    
    # Create `crops` dataframe by merging dfs in `crops.dfs' on `hcid`
      crops <- crops.dfs[[1]]
      for (i in 2:length(crops.dfs)){ 
        crops <- merge.data.frame(crops, crops.dfs[[i]],
                                  by.x = "hcid", by.y = "hcid",
                                       all = TRUE, sort = TRUE,
                                       suffixes = c("", paste0(".",names(crops.dfs)[i],sep="")),
                                       no.dups=TRUE)
      }
    
    # Reconstruct other identifiers 
      # clust = digits 1-4 of hcid 
      crops$clust <- as.numeric(substr(crops$hcid, 1, 4))
      # nh = digits 5-6 of hcid 
      crops$nh <- as.numeric(substr(crops$hcid, 5, 6))
      # cropid = digits 7-8 of hcid 
      crops$cropid <- as.numeric(substr(crops$hcid, 7, 8))
      # hid = digits 1-6 of hcid 
      crops$hid <- as.numeric(substr(crops$hcid, 1, 6))
      # rename a couple variables for clarity
      colnames(crops)[colnames(crops)=="cid"] <- "cid.Y09B"
    
    # Reorder dataset for readability
      # Create variables to indicate Q's from each section 
      for (i in 1:length(crops.list)){
        crops[[paste0(names(crops.dfs)[i])]] <- crops.names[i]
      }
      # Create variables to indicate `ID` and `CID` variables 
      crops$IDS <- "IDS"
      crops$CIDS <- "CIDS"
      # Reorder
      varorder <- c(104,87,88,90,89,1,91,3:18,92,20:26,93,28:32,
                    94,34:40,95,42:47,96,49:55,97,57:59,98,61:65,
                    99,67:69,100,71:74,101,76:79,102,81:82,103,
                    84:86,105,2,19,27,33,41,48,56,60,66,70,75,80,83)
      crops <- crops[,varorder]
      
    # Sample diagnostic
      length(unique(crops$hcid)) # 17106 household-crop obs
      length(unique(crops$hid)) # 2278 households (~7.5 crops/hh)
      length(unique(crops$cropid))  # 36 crops
      length(unique(crops$clust)) # 172 clusters
      
  # ----     
  
  # [6] Agricultural dataset - food crop sales (household-product level) ----
    
    # One section on sales of own-produced food crops in the raw GLSS1 data 
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      food.sales.list <- "Y09E"
      food.sales <- as.data.frame(glss1[names(glss1) %in% food.sales.list])
      
    # Rename columns 
    for (i in 1:length(colnames(food.sales))){
      renamestr <- as.character(paste0(colnames(food.sales)[i]))
      colnames(food.sales)[i] <- substring(renamestr,6,nchar(renamestr))
    }
    # rename cid variable for clarity
      colnames(food.sales)[colnames(food.sales)=="cid"] <- "cid.Y09E"
      
    # Create household-product identifier `hprid`
      food.sales$hprid <- 1000*food.sales$hid + food.sales$prcdsal
    
    # Check that both `hprid` and `uniqueid` are unique 
      length(unique(food.sales$hprid)) # 408
      length(unique(food.sales$uniqueid)) # 408
    
    # Create indicator for food sales section 
      food.sales$Y09E <- "food.sales"
    
    # Reorder
      food.sales <- food.sales[, c(17,2,8,5,3,16,15,4,6,7,9,10,11,12,13,14,1)]
    
    # Sample diagnostic:
      length(unique(food.sales$clust)) # 98 clusters
      length(unique(food.sales$hid)) # 322 households
      length(unique(food.sales$prcdsal)) # 11 food products

  # ----   
       
  # [7] Agricultural dataset - livestock (household-livestock-type level) ----
      
    # One section on livestock ownership in the raw GLSS1 data 
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      livestock.list <- "Y09F"
      livestock <- as.data.frame(glss1[names(glss1) %in% livestock.list])
    
    # Rename columns 
    for (i in 1:length(colnames(livestock))){
      renamestr <- as.character(paste0(colnames(livestock)[i]))
      colnames(livestock)[i] <- substring(renamestr,6,nchar(renamestr))
    }
    # rename cid variable for clarity
      colnames(livestock)[colnames(livestock)=="cid"] <- "cid.Y09F"
    
    # Create household-livestock identifier `hlivstid` (same as `uniqueid`)
      livestock$hlivstid <- 100*livestock$hid + livestock$livstcd
    
    # Check that both `hlivstid` and `uniqueid` are unique 
      length(unique(livestock$hlivstid)) # 3060
      length(unique(livestock$uniqueid)) # 3060
    
    # Create indicator for livestock section 
      livestock$Y09F <- "livestock.own"
    
    # Reorder
      livestock <- livestock[, c(20,2,13,3,14,19,18,4:12,15:17,1)]
    
    # Sample diagnostic:
      length(unique(livestock$clust)) # 161 clusters
      length(unique(livestock$hid)) # 1432 households
      length(unique(livestock$livstcd)) # 8 livestock types
    
  # ----   
   
  # [8] Agricultural dataset - animal products (household-product level) ---- 
   
    # One section on animal products in the raw GLSS1 data 
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      animal.prod.list <- "Y09G"
      animal.prod <- as.data.frame(glss1[names(glss1) %in% animal.prod.list])
    
    # Rename columns 
    for (i in 1:length(colnames(animal.prod))){
      renamestr <- as.character(paste0(colnames(animal.prod)[i]))
      colnames(animal.prod)[i] <- substring(renamestr,6,nchar(renamestr))
    }
    # rename cid variable for clarity
      colnames(animal.prod)[colnames(animal.prod)=="cid"] <- "cid.Y09G"
    
    # Create household-animal-product identifier `hanprid` (same as `uniqueid`)
      animal.prod$hanprid <- 100*animal.prod$hid + animal.prod$anprcd
    
    # Check that both `hanprid` and `uniqueid` are unique 
      length(unique(animal.prod$hanprid)) # 129
      length(unique(animal.prod$uniqueid)) # 129
    
    # Create indicator for animal products section 
      animal.prod$Y09G <- "animal.products"
    
    # Reorder
      animal.prod <- animal.prod[, c(12,2,5,3,6,11,10,7:9,1)]
    
    # Sample diagnostic:
      length(unique(animal.prod$clust)) # 67 clusters
      length(unique(animal.prod$hid)) # 127 households
      length(unique(animal.prod$anprcd)) # 3 animal-product types
    
  # ---- 
      
  # [9] Agricultural dataset - livestock expenditure (household-expenditure-type level) ----
    
    # One section on livestock expenditure in the raw GLSS1 data 
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      livestock.expend.list <- "Y09I"
      livestock.expend <- as.data.frame(glss1[names(glss1) %in% livestock.expend.list])
    
    # Rename columns 
    for (i in 1:length(colnames(livestock.expend))){
      renamestr <- as.character(paste0(colnames(livestock.expend)[i]))
      colnames(livestock.expend)[i] <- substring(renamestr,6,nchar(renamestr))
    }
    # rename cid variable for clarity
      colnames(livestock.expend)[colnames(livestock.expend)=="cid"] <- "cid.Y09I"
    
    # Create household-livestock-expenditure-type identifier `hlivexid` (same as `uniqueid`)
      livestock.expend$hlivexid <- 100*livestock.expend$hid + livestock.expend$excdliv
    
    # Check that both `hlivexid` and `uniqueid` are unique 
      length(unique(livestock.expend$hlivexid)) # 819
      length(unique(livestock.expend$uniqueid)) # 819
    
    # Create indicator for livestock expenditure section 
      livestock.expend$Y09I <- "livestock.expenditure"
    
    # Reorder
      livestock.expend <- livestock.expend[, c(10,2,5,3,6,9,8,4,7,1)]
    
    # Sample diagnostic:
      length(unique(livestock.expend$clust)) # 133 clusters
      length(unique(livestock.expend$hid)) # 481 households
      length(unique(livestock.expend$excdliv)) # 9 livestock-expenditure types
    
  # ---- 
     
  # [10] Agricultural dataset - farm equipment (household-equipment-type level) ----
   
    # One section on farm equipment in the raw GLSS1 data 
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      farm.equip.list <- "Y09K"
      farm.equip <- as.data.frame(glss1[names(glss1) %in% farm.equip.list])
    
    # Rename columns 
    for (i in 1:length(colnames(farm.equip))){
      renamestr <- as.character(paste0(colnames(farm.equip)[i]))
      colnames(farm.equip)[i] <- substring(renamestr,6,nchar(renamestr))
    }
    # rename cid variable for clarity
      colnames(farm.equip)[colnames(farm.equip)=="cid"] <- "cid.Y09K"
    
    # Create household-equipment-type identifier `heqid` (same as `uniqueid`)
      farm.equip$heqid <- 100*farm.equip$hid + farm.equip$eqcdown
    
    # Check that both `heqid` and `uniqueid` are unique 
      length(unique(farm.equip$heqid)) # 207
      length(unique(farm.equip$uniqueid)) # 207
    
    # Create indicator for farm equipment section 
      farm.equip$Y09K <- "farm.equipment"
    
    # Reorder
      farm.equip <- farm.equip[, c(18,2,11,6,12,17,16,3:5,7:10,13:15,1)]
    
    # Sample diagnostic:
      length(unique(farm.equip$clust)) # 80 clusters
      length(unique(farm.equip$hid)) # 176 households
      length(unique(farm.equip$eqcdown)) # 9 farm equipment types
    
  # ---- 
        
  # [11] Commodity Prices (cluster-item level) ----

    # One section on commodity prices in the raw GLSS1 data 
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      prices.list <- "PRICE"
      prices <- as.data.frame(glss1[names(glss1) %in% prices.list])
    
    # Rename columns 
    for (i in 1:length(colnames(prices))){
      renamestr <- as.character(paste0(colnames(prices)[i]))
      colnames(prices)[i] <- substring(renamestr,7,nchar(renamestr))
    }
    # rename moint and yrint variables for clarity
      colnames(prices)[colnames(prices)=="moint"] <- "moint.PRICE"
      colnames(prices)[colnames(prices)=="yrint"] <- "yrint.PRICE"
      
    # Create cluster-commodity identifier `clitemid` 
      prices$clitemid <- 100*prices$clust + prices$itemno
    
    # Check `clitemid` is unique 
      length(unique(prices$clitemid)) # 5948

    # Create indicator for commodity price section 
      prices$PRICE <- "commodity.prices"
    
    # Reorder
      prices <- prices[, c(18,1,4,17,5:16,2:3)]
    
    # Sample diagnostic:
      length(unique(prices$clust)) # 165 clusters
      length(unique(prices$itemno)) # 47 farm equipment types
        
  # ----
  
  # [12] Children (household-child level) ----
      
    # Create lists of (i) household-child level dfs and (ii) names for these dfs
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      children.list <- c("Y01C", "Y13A1B")
      children.names <- c("nonresident.chlds","child.birthhist")
    
    # Identify and rename subset of `glss1' dataframes with names in `nfes.list`
      children.dfs <- glss1[names(glss1) %in% children.list]
    
    # Remove duplicate identifiers and create section-specific ids
      childidgen <- function(df){
        if ("ordchl" %in% colnames(df)){df$hchldid <- 100*df$hid+df$ordchl}
        if ("orderc" %in% colnames(df)){df$hchldid <- 100*df$hid+df$orderc}
        drops <- c("nh","clust","hid","uniqueid")
        df <- df[, !(names(df)) %in% drops]
        return(df)
      }
      invisible(children.dfs <- list.apply(children.dfs,childidgen))
    
    # Create `children` dataframe by merging dfs in `children.dfs' on `hchldid`
      children <- children.dfs[[1]]
      for (i in 2:length(children.dfs)){ 
        children <- merge.data.frame(children, children.dfs[[i]],
                                 by.x = "hchldid", by.y = "hchldid",
                                 all = TRUE, sort = TRUE,
                                 suffixes = c("", paste0(".",names(children.dfs)[i],sep="")),
                                 no.dups=TRUE)
      }
    
    # Reconstruct other identifiers 
      # clust = digits 1-4 of hchldid 
      children$clust <- as.numeric(substr(children$hchldid, 1, 4))
      # nh = digits 5-6 of hchldid
      children$nh <- as.numeric(substr(children$hchldid, 5, 6))
      # childid = digit 7 of hchldid
      children$childid <- as.numeric(substr(children$hchldid, 7, 8))
      # hid = digits 1-6 of hchldid
      children$hid <- as.numeric(substr(children$hchldid, 1, 6))
      # rename a couple variables for clarity
      colnames(children)[colnames(children)=="cid"] <- "cid.Y01C"
    
    # Reorder dataset for readability
      # Create variables to indicate Q's from each section 
      for (i in 1:length(children.list)){
        children[[paste0(names(children.dfs)[i])]] <- children.names[i]
      }
      # Create variables to indicate `ID` and `CID` variables 
      children$IDS <- "IDS"
      children$CIDS <- "CIDS"
      # Reorder
      varorder <- c(33,27,28,30,29,1,31,3:14,32,16:26,34,2,15)
      children <- children[,varorder]
      
    # Sample diagnostic
      length(unique(children$hchldid)) # 9922 household-child obs
      length(unique(children$hid)) # 2585 households (4 children/hh)
      length(unique(children$clust)) # 177 clusters

  # ---- 
  
  # [13] Non-Farm Enterprises (household-enterprise level) ----
    
    # Create lists of (i) household-enterprise level dfs and (ii) names for these dfs
      nfes.list <- c("Y10A","Y10B","Y10C","Y10D")
      nfes.names <- c("working.conds","expenses","revenues","assets")
    
    # Identify and rename subset of `glss1' dataframes with names in `nfes.list`
      nfes.dfs <- glss1[names(glss1) %in% nfes.list]
    
    # Remove duplicate identifiers and create nfe-specific ids
      nfeidgen <- function(df){
        df$hbid <- 10*df$hid + df$bid
        if ("bexpcd" %in% colnames(df)){df$hbexpendid <- 100*df$hbid+df$bexpcd}
        if ("bassetcd" %in% colnames(df)){df$hbassetid <- 100*df$hbid+df$bassetcd}
        drops <- c("nh","clust","hid","bid","uniqueid")
        df <- df[, !(names(df)) %in% drops]
        return(df)
      }
      invisible(nfes.dfs <- list.apply(nfes.dfs,nfeidgen))
    
    # Create `nfes` dataframe by merging dfs in `nfes.dfs' on `hbid`
      nfes <- nfes.dfs[[1]]
      for (i in 2:length(nfes.dfs)){ 
        nfes <- merge.data.frame(nfes, nfes.dfs[[i]],
                                  by.x = "hbid", by.y = "hbid",
                                  all = TRUE, sort = TRUE,
                                  suffixes = c("", paste0(".",names(nfes.dfs)[i],sep="")),
                                  no.dups=TRUE)
      }
    
    # Reconstruct other identifiers 
      # clust = digits 1-4 of hbid 
      nfes$clust <- as.numeric(substr(nfes$hbid, 1, 4))
      # nh = digits 5-6 of hbid 
      nfes$nh <- as.numeric(substr(nfes$hbid, 5, 6))
      # bid = digit 7 of hbid 
      nfes$bid <- as.numeric(substr(nfes$hbid, 7, 8))
      # hid = digits 1-6 of hbid 
      nfes$hid <- as.numeric(substr(nfes$hbid, 1, 6))
      # rename a couple variables for clarity
      colnames(nfes)[colnames(nfes)=="cid"] <- "cid.Y10A"
    
    # Reorder dataset for readability
      # Create variables to indicate Q's from each section 
      for (i in 1:length(nfes.list)){
        nfes[[paste0(names(nfes.dfs)[i])]] <- nfes.names[i]
      }
      # Create variables to indicate `ID` and `CID` variables 
      nfes$IDS <- "IDS"
      nfes$CIDS <- "CIDS"
      # Reorder
      varorder <- c(67,59,60,62,61,1,
                    63,3:23,
                    64,26,30,25,27:29,
                    65,32:49,
                    66,54,58,51:53,55:57,
                    68,2,24,31,50)
      nfes <- nfes[,varorder]
      
    # Sample note: 
        # The unique identifier in this dataset is `hid-bid-bexpcd-bassetcd'
        # In other words, each row is a unique combination of the business level 
        # information in Y10A, and Y10C, the business-expenditure type level 
        # information in Y10B, and the business-asset-type level information 
        # in 10D. Be careful of this when aggregating. 
    
    # Sample diagnostic
      length(unique(nfes$hbid)) # 2271 household-business obs
      length(unique(nfes$hid)) # 1701 households (1.33 nfes/hh)
      length(unique(nfes$clust)) # 175 clusters
      length(unique(nfes$bexpcd))  # 13 types of business expenditure
      length(unique(nfes$bassetcd))  # 5 types of business assets

  # ----   
    
  # [14] Household Daily Expenditure (household-expenditure-type level) ----
    
    # One section on household daily expenditure. in the raw GLSS1 data. 
    # Note: expenditure codes are NOT the same as in the annual expenditure section  
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      hh.daily.expend.list <- "Y11A"
      hh.daily.expend <- as.data.frame(glss1[names(glss1) %in% hh.daily.expend.list])
    
    # Rename columns 
      for (i in 1:length(colnames(hh.daily.expend))){
        renamestr <- as.character(paste0(colnames(hh.daily.expend)[i]))
        colnames(hh.daily.expend)[i] <- substring(renamestr,6,nchar(renamestr))
      }
      # rename cid variable for clarity
      colnames(hh.daily.expend)[colnames(hh.daily.expend)=="cid"] <- "cid.Y11A"
    
    # Create hid-hhexpcd identifier `hdexpcd` 
      hh.daily.expend$hdexpcd <- 1000*hh.daily.expend$hid + hh.daily.expend$hhexpcd
    
    # Check `hdexpcd` is unique 
      length(unique(hh.daily.expend$hdexpcd)) # 13618/13618
    
    # Create indicator for daily expenditure section 
      hh.daily.expend$Y11A <- "hh.daily.expenditure"
    
    # Reorder
      hh.daily.expend <-hh.daily.expend[, c(9,2,6,4,3,8,7,5,1)]
    
    # Sample diagnostic:
      length(unique(hh.daily.expend$clust)) # 176 clusters
      length(unique(hh.daily.expend$hid)) # 3112 households
      length(unique(hh.daily.expend$hhexpcd)) # 9 expenditure-types 
      
  # ---- 
   
  # [15] Household Annual Expenditure (household-expenditure-type level) ----

    # One section on household annual expenditure. in the raw GLSS1 data. 
    # Note: expenditure codes are NOT the same as in the daily expenditure section  
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      hh.annual.expend.list <- "Y11B"
      hh.annual.expend <- as.data.frame(glss1[names(glss1) %in% hh.annual.expend.list])
    
    # Rename columns 
      for (i in 1:length(colnames(hh.annual.expend))){
        renamestr <- as.character(paste0(colnames(hh.annual.expend)[i]))
        colnames(hh.annual.expend)[i] <- substring(renamestr,6,nchar(renamestr))
      }
      # rename cid variable for clarity
      colnames(hh.annual.expend)[colnames(hh.annual.expend)=="cid"] <- "cid.Y11B"
    
    # Create hid-hhexpcd identifier `hdexpcd` 
      hh.annual.expend$haexpcd <- 1000*hh.annual.expend$hid + hh.annual.expend$hhexpcd
    
    # Check `haexpcd` is unique 
      length(unique(hh.annual.expend$haexpcd)) # 41356/41356
    
    # Create indicator for annual expenditure section 
      hh.annual.expend$Y11B <- "hh.annual.expenditure"
    
    # Reorder
      hh.annual.expend <- hh.annual.expend[, c(11,2,7,4,3,10,9,5,6,8,1)]
    
    # Sample diagnostic:
      length(unique(hh.annual.expend$clust)) # 176 clusters
      length(unique(hh.annual.expend$hid)) # 3130 households
      length(unique(hh.annual.expend$hhexpcd)) # 31 expenditure-types 
      
  # ----    
    
  # [16] Household Food Expenditure & Consumption (household-food-type level) ---- 
    
    # Create lists of (i) household-foodcode level dfs and (ii) names for these dfs
      hh.food.cons.list <- list <- c("Y12A","Y12B")
      hh.food.cons.names <- c("food.expend","own.food.cons")
    
    # Identify and rename subset of `glss1' dataframes with names in `nfes.list`
      hh.food.cons.dfs <- glss1[names(glss1) %in% hh.food.cons.list]
    
    # Remove duplicate identifiers and create nfe-specific ids
      hhfoodidgen <- function(df){
        df$hfoodid <- 1000*df$hid + df$foodcd
        drops <- c("nh","clust","hid","foodcd","uniqueid")
        df <- df[, !(names(df)) %in% drops]
        return(df)
      }
      invisible(hh.food.cons.dfs <- list.apply(hh.food.cons.dfs,hhfoodidgen))
    
    # Create `hh.food.cons` dataframe by merging dfs in `hh.food.cons.dfs' on `hfoodid`
      hh.food.cons <- hh.food.cons.dfs[[1]]
      for (i in 2:length(hh.food.cons.dfs)){ 
        hh.food.cons <- merge.data.frame(hh.food.cons, hh.food.cons.dfs[[i]],
                                 by.x = "hfoodid", by.y = "hfoodid",
                                 all = TRUE, sort = TRUE,
                                 suffixes = c("", paste0(".",names(hh.food.cons.dfs)[i],sep="")),
                                 no.dups=TRUE)
      }
    
    # Reconstruct other identifiers 
      # clust = digits 1-4 of hfoodid 
      hh.food.cons$clust <- as.numeric(substr(hh.food.cons$hfoodid, 1, 4))
      # nh = digits 5-6 of hfoodid 
      hh.food.cons$nh <- as.numeric(substr(hh.food.cons$hfoodid, 5, 6))
      # bid = digit 7 of hfoodid 
      hh.food.cons$foodcd <- as.numeric(substr(hh.food.cons$hfoodid, 7, 9))
      # hid = digits 1-6 of hfoodid 
      hh.food.cons$hid <- as.numeric(substr(hh.food.cons$hfoodid, 1, 6))
      # rename a couple variables for clarity
      colnames(hh.food.cons)[colnames(hh.food.cons)=="cid"] <- "cid.Y12A"
    
    # Reorder dataset for readability
      # Create variables to indicate Q's from each section 
      for (i in 1:length(hh.food.cons.list)){
        hh.food.cons[[paste0(names(hh.food.cons.dfs)[i])]] <- hh.food.cons.names[i]
      }
      # Create variables to indicate `ID` and `CID` variables 
      hh.food.cons$IDS <- "IDS"
      hh.food.cons$CIDS <- "CIDS"
      # Reorder
      varorder <- c(20,14,15,17,16,1,18,3:8,19,10:13,21,2,9)
      hh.food.cons <- hh.food.cons[,varorder]
    
    # Sample diagnostic
      length(unique(hh.food.cons$hid)) # 3134 households (29 food types/hh)
      length(unique(hh.food.cons$clust)) # 177 clusters
      length(unique(hh.food.cons$foodcd))  # 62 types of food consumption/expenditure
      
  # ----  

  # [17] Household Durable Inventory (household-good-type level) ----

    # One section on household durable goods inventory in the raw GLSS1 data. 
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      hh.durables.list <- "Y11C"
      hh.durables <- as.data.frame(glss1[names(glss1) %in% hh.durables.list])
    
    # Rename columns 
      for (i in 1:length(colnames(hh.durables))){
        renamestr <- as.character(paste0(colnames(hh.durables)[i]))
        colnames(hh.durables)[i] <- substring(renamestr,6,nchar(renamestr))
      }
      # rename cid variable for clarity
      colnames(hh.durables)[colnames(hh.durables)=="cid"] <- "cid.Y11C"
    
    # Create hid-itemno identifier `hhitemcd` 
      hh.durables$hhitemcd <- 100*hh.durables$hid + hh.durables$itemno
    
    # Check `hhitemcd` is unique 
      length(unique(hh.durables$hhitemcd)) # 3824/3824
      length(unique(hh.durables$uniqueid)) # 3824/3824
    
    # Create indicator for durables section 
      hh.durables$Y11B <- "hh.durable.goods"
    
    # Reorder
      hh.durables <- hh.durables[, c(12,2,7,4,8,11,10,3,5,6,9,1)]
    
    # Sample diagnostic:
      length(unique(hh.durables$clust)) # 176 clusters
      length(unique(hh.durables$hid)) # 1699 households
      length(unique(hh.durables$itemno)) # 16 durable items 
      length(unique(hh.durables$goodcd)) # 18 durable good types 
    
  # ----
      
  # [18] Household Birth Control (household-birth-control-type level) ----
  
    # One section on use of birth control in the raw GLSS1 data. 
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      birth.control.list <- "Y13B"
      birth.control <- as.data.frame(glss1[names(glss1) %in% birth.control.list])
    
    # Rename columns 
      for (i in 1:length(colnames(birth.control))){
        renamestr <- as.character(paste0(colnames(birth.control)[i]))
        colnames(birth.control)[i] <- substring(renamestr,6,nchar(renamestr))
      }
      # rename cid variable for clarity
      colnames(birth.control)[colnames(birth.control)=="cid"] <- "cid.Y13B"
    
    # Create hid-bcmeth identifier `hbcmeth` 
      birth.control$hbcmeth <- 100*birth.control$hid + birth.control$bcmeth
    
    # Check `hbcmeth` is unique 
      length(unique(birth.control$hbcmeth)) # 20084/20084
      length(unique(birth.control$uniqueid)) # 20084/20084
    
    # Create indicator for birth control section 
      birth.control$Y13B <- "birth.control"
    
    # Reorder
      birth.control <- birth.control[, c(24,2,6,4,7,23,22,3,5,8:21,1)]
    
    # Sample diagnostic:
      length(unique(birth.control$clust)) # 176 clusters
      length(unique(birth.control$hid)) # 2141 households
      length(unique(birth.control$bcmeth)) # 12 birth-control types
  
  # ----
      
  # [19] Finance - Loans (household-loan level) ----

    # One section on use of loans in the raw GLSS1 data. 
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      finance.loans.list <- "Y15B"
      finance.loans <- as.data.frame(glss1[names(glss1) %in% finance.loans.list])
    
    # Rename columns 
      for (i in 1:length(colnames(finance.loans))){
        renamestr <- as.character(paste0(colnames(finance.loans)[i]))
        colnames(finance.loans)[i] <- substring(renamestr,6,nchar(renamestr))
      }
      # rename cid variable for clarity
      colnames(finance.loans)[colnames(finance.loans)=="cid"] <- "cid.Y15B"
    
    # Create hid-line identifier `hloanid` 
      finance.loans$hloanid <- 10*finance.loans$hid + finance.loans$line
    
    # Check `hloanid` is unique 
      length(unique(finance.loans$hloanid)) # 1544/1544
      length(unique(finance.loans$uniqueid)) # 1544/1544
    
    # Create indicator for loans section 
      finance.loans$Y15B <- "loans"
    
    # Reorder
      finance.loans <- finance.loans[, c(26,2,8,3,9,25,24,4:7,10:23,1)]
    
    # Sample diagnostic:
      length(unique(finance.loans$clust)) # 159 clusters
      length(unique(finance.loans$hid)) # 954 households (1.6 loans/hh)
      
  # ----

  # [20] Finance - Remittances Out (household-remittance-destination level) ----
      # glss1.Y11D (remittances; uniqueid)
      # these are remittances sent by the househol
      
    # One section on use of remittances in the raw GLSS1 data. 
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      finance.remitt.list <- "Y11D"
      finance.remitt <- as.data.frame(glss1[names(glss1) %in% finance.remitt.list])
    
    # Rename columns 
      for (i in 1:length(colnames(finance.remitt))){
        renamestr <- as.character(paste0(colnames(finance.remitt)[i]))
        colnames(finance.remitt)[i] <- substring(renamestr,6,nchar(renamestr))
      }
      # rename cid variable for clarity
      colnames(finance.remitt)[colnames(finance.remitt)=="cid"] <- "cid.Y11D"
    
    # Create hid-source identifier `hsrcid` 
      finance.remitt$hsrcid <- 10*finance.remitt$hid + finance.remitt$ordper
    
    # Check `hsrcid` is unique 
      length(unique(finance.remitt$hsrcid)) # 2871/2871
      length(unique(finance.remitt$uniqueid)) # 2871/2871
    
    # Create indicator for loans section 
      finance.remitt$Y11D <- "remittances"
    
    # Reorder
      finance.remitt <- finance.remitt[, c(13,2,5,3,6,12,11,4,7:10,1)]
    
    # Sample diagnostic:
      length(unique(finance.remitt$clust)) # 173 clusters
      length(unique(finance.remitt$hid)) # X households (1.9 remittance sources/hh)
      
  # ----   
      
  # [21] Finance - Remittances In (household-remittance-source level) ----

    # One section on remittances sent by household in the raw GLSS1 data. 
    # See "GLSS 1987-88 Report.pdf" in Documentation folder or "GLSS1_SectionNotes.txt" in Data_Notes folder 
      finance.remitt.in.list <- "Y14A"
      finance.remitt.in <- as.data.frame(glss1[names(glss1) %in% finance.remitt.in.list])
      
    # Rename columns 
      for (i in 1:length(colnames(finance.remitt.in))){
        renamestr <- as.character(paste0(colnames(finance.remitt.in)[i]))
        colnames(finance.remitt.in)[i] <- substring(renamestr,6,nchar(renamestr))
      }
      # rename cid variable for clarity
      colnames(finance.remitt.in)[colnames(finance.remitt.in)=="cid"] <- "cid.Y14A"
    
    # Create hid-remitt-source identifier `hrsrcid` 
      finance.remitt.in$hrsrcid <- 10*finance.remitt.in$hid + finance.remitt.in$ordrinc
    
    # Check `hrsrcid` is unique 
      length(unique(finance.remitt.in$hrsrcid)) # 1658/1658
      length(unique(finance.remitt.in$uniqueid)) # 1658/1658
    
    # Create indicator for birth control section 
      finance.remitt.in$Y14A <- "remittances.in"
    
    # Reorder
      finance.remitt.in <- finance.remitt.in[, c(13,2,5,3,6,12,11,4,7:10,1)]
    
    # Sample diagnostic:
      length(unique(finance.remitt.in$clust)) # 170 clusters
      length(unique(finance.remitt.in$hid)) # 1008 households

  # ----  

  # [22] Finance - Other Income (household-income-type level) ----

    # One section on other hh income in the raw GLSS1 data. 
      other.inc.list <- "Y14B"
      other.inc <- as.data.frame(glss1[names(glss1) %in% other.inc.list])
    
    # Rename columns 
      for (i in 1:length(colnames(other.inc))){
        renamestr <- as.character(paste0(colnames(other.inc)[i]))
        colnames(other.inc)[i] <- substring(renamestr,6,nchar(renamestr))
      }
      # rename cid variable for clarity
      colnames(other.inc)[colnames(other.inc)=="cid"] <- "cid.Y14B"
    
    # Create hid-inc-source identifier `hoincid` 
      other.inc$hoincid <- 1000*other.inc$hid + other.inc$oincd
    
    # Check `hoincid` is unique 
      length(unique(other.inc$hoincid)) # 3290/3290
      length(unique(other.inc$uniqueid)) # 3290/3290
    
    # Create indicator for birth control section 
      other.inc$Y14B <- "other.income"
    
    # Reorder
      other.inc <- other.inc[, c(9,2,6,4,3,8,7,5,1)]
    
    # Sample diagnostic:
      length(unique(other.inc$clust)) # 173 clusters
      length(unique(other.inc$hid)) # 2061 households
      length(unique(other.inc$oincd)) # 19 other income types 
  
  # ----   
      