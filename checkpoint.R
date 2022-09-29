

############### Directory & folder ###############
user <- Sys.getenv("USERNAME")
directory <- paste0('c:/Users/',user,'/RLoader')
if (!dir.exists(directory)) {
  dir.create(directory)
  .libPaths(directory)
} else { 
  .libPaths(directory) }


############### Function: Package loader ###############
func_package <- function(pkg) {
  if((nzchar(system.file(package = pkg,lib.loc=directory)) == FALSE) && (pkg == "checkpoint")) {
    if(substr(paste(R.version$major,".",R.version$minor, sep=""),1,3) == "4.0"){
      install.packages("https://cran.r-project.org/bin/windows/contrib/4.0/checkpoint_1.0.2.zip",lib=directory, repos = NULL, type = "win.binary")
    } else {
      install.packages(pkg, repo="http://cran.rstudio.com/")
    }
    library(pkg, character.only = TRUE)
  } else if(nzchar(system.file(package = pkg)) == FALSE) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  } else {
    if(pkg == "checkpoint"){
      library(pkg, lib.loc=directory, character.only = TRUE)
    } else{
      library(pkg, character.only = TRUE)
    }
  }
}


libraryLoader <- function(input_date, scan_bool, mail.pkg = FALSE) {
  
  func_package("checkpoint")
  
   checkMRAN <- function(input_date){
    out <- tryCatch({
      checkpoint::setSnapshot(input_date)
    },
    error=function(cond){
      message(paste("checkpoint::setSnapshot(",input_date,") raised an error - no contact to MRAN"))
    },
    warning=function(cond){
      message(paste("checkpoint::setSnapshot(",input_date,") raised a warning - no contact to MRAN"))
    },
    finally={
      checkpoint_dir <- paste0(directory,"/.checkpoint/",input_date)
      return(checkpoint_dir)
    }
    )
    return(out)
  }      
  
  setSnapshotDir <- checkMRAN(input_date)
  
  
  if(scan_bool == FALSE & !dir.exists(setSnapshotDir)){ 
    dir.create(setSnapshotDir, recursive = TRUE)
    .libPaths(setSnapshotDir)
  } else if  (scan_bool == FALSE) {
    .libPaths(setSnapshotDir)
  }   
  
  checkpoint(input_date, scanForPackages = scan_bool,  checkpoint_location =  paste0(directory,"/.checkpoint/",input_date))
  
  # Base packages
  #pkg_vector <- c("RODBC","keyring","sendmailR","mailR","rJava")
  pkg_vector <- c("RODBC","dplyr", "tidyr", "readr", "stringr","readxl", "tidyverse", "XML", "RCurl", "httr", "rvest", "zoo", "openxlsx", "reticulate")
  lapply(X = pkg_vector,FUN = (func_package))
  
  source("F:/Research/_Performing Credit Quantitative Research/R/Lib_loader/func/sql.R")
  if(isTRUE(mail.pkg)){
  source("F:/Research/_Performing Credit Quantitative Research/R/Lib_loader/func/mail.R")
  }
  source("F:/Research/_Performing Credit Quantitative Research/R/scores_to_sql/PD_Score_Library/PD_rating_calc.R")
  source("F:/Research/_Performing Credit Quantitative Research/R/Lib_loader/func/sharepoint.R")
}
