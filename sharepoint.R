# Load the sharepoint library
check <- list.files(path = .libPaths()[1])
check <- check[(check %in% "Microsoft365R")]

if(length(check) == 0){
 install.packages("Microsoft365R",lib = .libPaths()[1])
}

library(Microsoft365R)


SharePoint_parse_model <- function(RmsId = NULL){
  if(is.null(RmsId)){stop("Specify RmsId; either a vector or single ID..")}
  print("First time, the developer has to authenticate manually. Follow the steps provided...")
  site <- Microsoft365R::get_sharepoint_site(site_url = "https://c4.sharepoint.com/sites/IMP")
  sp.list <- site$get_list("Issuers")
  sp.df <- data.frame(sp.list$list_items(), stringsAsFactors = F)
  sp.df <- sp.df %>% 
    filter(Issuer_IDCode %in% c(RmsId))
  
  for (i in 1:nrow(sp.df)){
    
    site <- Microsoft365R::get_sharepoint_site(site_url = sp.df$Issuer_Link$Url[i])
    doc <- site$get_list("Documents")
    doc.df <- doc$list_items()
    
    if(length(which(names(doc.df) %in% "Evobis_Document_Status"))==1){
      doc.df <- doc.df[doc.df$Evobis_Document_Status == 'Active' & doc.df$ContentType == 'Model',]
    }else{
      doc.df <- doc.df[doc.df$Document_Status == 'Active' & doc.df$ContentType == 'Model',]
    }
   
     doc.df <- doc.df[stringr::str_detect(doc.df$FileLeafRef, ".xlsx"),]
    
    if(nrow(doc.df)>0){
      
      model.df <- doc$get_item(paste0(doc.df$id))
      
      if(as.Date(model.df$properties$lastModifiedDateTime)<Sys.Date()-1){
        if(is.na(doc.df$Title)){
          doc$update_item(paste0(doc.df$id), Title = 'Updating modified date to parse model..')
          doc$update_item(paste0(doc.df$id), Title = '')
          print(paste0(sp.df$Issuer_IDCode[i], " ready to be parsed at next execution."))
        }else{
          doc$update_item(paste0(doc.df$id), Title = paste0(doc.df$Title, "."))
          print(paste0(sp.df$Issuer_IDCode[i], " ready to be parsed at next execution."))
        }
      }
    }
  }
}



SharePoint_get_log <- function(RmsId = NULL, path_folder = getwd()){
  if(is.null(RmsId)){stop("Specify RmsId; either a vector or single ID..")}
  print("First time, the developer has to authenticate manually. Follow the steps provided...")
  site <- Microsoft365R::get_sharepoint_site(site_url = "https://c4.sharepoint.com/sites/IMP")
  sp.list <- site$get_list("Issuers")
  sp.df <- data.frame(sp.list$list_items(), stringsAsFactors = F)
  sp.df <- sp.df %>% 
    filter(Issuer_IDCode %in% c(RmsId))
 
  for(i in 1:nrow(sp.df)){
    site <- try(Microsoft365R::get_sharepoint_site(site_url = sp.df$Issuer_Link$Url[i]), silent = T)
    
    if(length(site) == 1){next}
    
    doc <- site$get_list("Documents")
    doc.df <- doc$list_items()
    
    if(length(which(names(doc.df) %in% "Evobis_Document_Status"))==1){
      check <- doc.df[doc.df$Evobis_Document_Status == 'Active' & doc.df$ContentType == 'Model',]
    }else{
      check <- doc.df[doc.df$Document_Status == 'Active' & doc.df$ContentType == 'Model',]
    }
    
    check <- check[stringr::str_detect(check$FileLeafRef, ".xlsx"),]
    
    if(length(check)==0){next}
      
    
    mod.time <- as.Date(doc.df$Modified[doc.df$ContentType == 'Model' & doc.df$Document_Status == 'Active' & doc.df$DocIcon == 'xlsx'])
    if(length(mod.time)==0){ mod.time <- as.Date(doc.df$Modified[doc.df$ContentType == 'Model' & doc.df$Evobis_Document_Status == 'Active' & doc.df$DocIcon == 'xlsx'])}
    
    if(length(mod.time)==0){next}
    if(is.na(max(mod.time))){next}
    
    directory <- paste0(path_folder,"/LogFiles/")
    if (!dir.exists(directory)) {
      dir.create(directory)
      .libPaths(directory)
    } else { 
      .libPaths(directory) }
    
    log.path <- paste0(path_folder,"/LogFiles/",sp.df$Issuer_IDCode[i],".txt")
    download.time <- as.Date(file.info(log.path)$mtime)
    
    if(is.na(download.time)){download.time <- as.Date("1999-01-01")}
    
    if(download.time <= max(mod.time)){
      drv <- site$get_drive()
      log.file <- drv$list_files()[drv$list_files()$name == 'SharePointRmsModelSync.log',]
      
      drv$list_items()
      
      if(dim(log.file)[1]>0){
        drv$download_file(log.file$name,paste0(path_folder,"/LogFiles/",sp.df$Issuer_IDCode[i],".txt"), overwrite = T)
        print(paste0("RmsId: ", sp.df$Issuer_IDCode[i]))
      }
    }
  }
}


SharePoint_update_site <- function(RmsId = NULL){
  if(is.null(RmsId)){stop("Specify RmsId; either a vector or single ID..")}
  print("First time, the developer has to authenticate manually. Follow the steps provided...")
  site <- Microsoft365R::get_sharepoint_site(site_url = "https://c4.sharepoint.com/sites/IMP")
  sp.list <- site$get_list("Issuers")
  sp.df <- data.frame(sp.list$list_items(), stringsAsFactors = F)
  sp.df <- sp.df %>% 
    filter(Issuer_IDCode %in% c(RmsId))
  
  for (i in 1:nrow(sp.df)){
    sp.list$update_item(paste0(sp.df$id[i]), Issuer_ProvisioningStatus = 'Awaiting Update')
    print(paste0(sp.df$Issuer_IDCode[i], " ready to be updated at next execution."))
  }
}


SharePoint_update_analyst <- function(RmsId = NULL, New_analyst = NULL){
  if(is.null(RmsId)){stop("Specify RmsId; either a vector or single ID..")}
  if(length(New_analyst) > 1 | 
     ifelse(is.null(nrow(New_analyst)),0,nrow(New_analyst))){stop("Analyst cannot be a vector.. Correct to a singular string..")}
  
  print("First time, the developer has to authenticate manually. Follow the steps provided...")
  site <- Microsoft365R::get_sharepoint_site(site_url = "https://c4.sharepoint.com/sites/IMP")
  sp.list <- site$get_list("Issuers")
  sp.df <- data.frame(sp.list$list_items(), stringsAsFactors = F)

  risk.df <- MSsqlExecute("select distinct RmsId, Analyst from dailyoverview.researchcurrent where RmsId is not null")
  suppressWarnings(
  analyst <- left_join(sp.df %>% distinct(RmsId = as.numeric(Issuer_IDCode), Issuer_PrimaryAnalystLookupId), risk.df, by=c("RmsId" = "RmsId")) %>% 
    group_by(Issuer_PrimaryAnalystLookupId) %>% 
    count(Analyst) %>%
    filter(n == max(n)) %>% 
    distinct(Analyst, Issuer_PrimaryAnalystLookupId) %>% 
    filter(!is.na(Analyst))
  )
  sp.df <- sp.df %>% 
    filter(Issuer_IDCode %in% c(RmsId))
  
  if(length(which(analyst$Analyst == New_analyst)) == 0){stop(print(paste0("Analyst ", New_analyst, " is a new analyst not in SharePoint yet. Stops execution.")))}
  
  for (i in 1:nrow(sp.df)){
    sp.list$update_item(paste0(sp.df$id[i]), 
                        Issuer_PrimaryAnalystLookupId  = as.character(analyst$Issuer_PrimaryAnalystLookupId[analyst$Analyst == New_analyst]))
    print(paste0(sp.df$Issuer_IDCode[i], " analyst updated to: ", New_analyst[i]))
  }
}


SharePoint_parse_model_renaming <- function(RmsId = NULL){
  if(is.null(RmsId)){stop("Specify RmsId; either a vector or single ID..")}
  print("First time, the developer has to authenticate manually. Follow the steps provided...")
  site <- Microsoft365R::get_sharepoint_site(site_url = "https://c4.sharepoint.com/sites/IMP")
  sp.list <- site$get_list("Issuers")
  sp.df <- data.frame(sp.list$list_items(), stringsAsFactors = F)
  sp.df <- sp.df %>% 
    filter(Issuer_IDCode %in% c(RmsId))
  
  for (i in 1:nrow(sp.df)){
    
    site <- Microsoft365R::get_sharepoint_site(site_url = sp.df$Issuer_Link$Url[i])
    doc <- site$get_list("Documents")
    doc.df <- doc$list_items()
    
    if(length(which(names(doc.df) %in% "Evobis_Document_Status"))==1){
      doc.df <- doc.df[doc.df$Evobis_Document_Status == 'Active' & doc.df$ContentType == 'Model',]
    }else{
      doc.df <- doc.df[doc.df$Document_Status == 'Active' & doc.df$ContentType == 'Model',]
    }
    
    doc.df <- doc.df[stringr::str_detect(doc.df$FileLeafRef, ".xlsx"),]
    
    if(nrow(doc.df)>0){
      
      model.df <- doc$get_item(paste0(doc.df$id))
      
      if(!stringr::str_detect(toupper(doc.df$FileLeafRef), "MODEL")){
        doc$update_item(paste0(doc.df$id), FileLeafRef = paste0("Model ",doc.df$FileLeafRef))
        print(paste0(sp.df$Issuer_IDCode[i], " model renamed from: ", doc.df$FileLeafRef, " to: ", paste0("Model ",doc.df$FileLeafRef)))
      }
    }
  }
}



sharepoint_get_list <- function(list = NULL){
  
  if(is.null(list)){stop("Specify list")}
  print("First time, the developer has to authenticate manually. Follow the steps provided...")
  
  site <- Microsoft365R::get_sharepoint_site(site_url = "https://c4.sharepoint.com/sites/IMP")
  sp.list <- site$get_list(list)
  sp.cols <- data.frame(sp.list$get_column_info()) %>% select(name, displayName) %>% mutate(name = toupper(name))
  
  sp.df <- data.frame(sp.list$list_items())
  
  sp.df.col <- data.frame(orgname = colnames(sp.df)) %>% 
    mutate(name = ifelse(str_detect(toupper(orgname), "LOOKUPID"),str_remove(toupper(orgname), "LOOKUPID"), toupper(orgname))) %>%
    mutate(name = ifelse(str_detect(toupper(name), "X_"),str_remove(toupper(orgname), "X"), name)) %>%
    mutate(name = ifelse(str_detect(toupper(name), "X."),str_remove(toupper(orgname), "X."), name))
  
  df <- left_join(x = sp.cols, y = sp.df.col, by = c("name" = "name")) %>% 
    filter(!is.na(orgname))
  
  sp.df <- sp.df[,df$orgname]
  
  colnames(sp.df) <- df$displayName
  
  sp.df <- sp.df[!duplicated(colnames(sp.df))]
  
  return(sp.df)
}
