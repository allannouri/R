# Load the DCOM library
check <- list.files(path = .libPaths()[1])
check <- check[(check %in% "RDCOMClient")]
if(length(check) == 0){
  
  path <- "\\RDCOMClient"
  dest <-  gsub("/", "\\\\",.libPaths()[1])
  pwshl <- paste0('Copy-Item -Path "',path,'" -Destination "', dest, '" -Recurse')
  writeLines(pwshl, paste0(getwd(),"/shell.ps1"))
  print("First time using RDCOMCLIENT. Install package:")
  print("Run the following script:./shell.ps1, right-click and run with powershell..")
}

library(RDCOMClient)

# Mail function
mailoutlook <- function(TO = NULL, CC = NULL, SUBJECT = "Mail from R", TEXT = NULL, DATA = NULL, ATTACH = NULL){
  
  Outlook <- COMCreate("Outlook.Application")
  
  
  Email = Outlook$CreateItem(0)
  
  if(!is.null(TO)){
  
    Email[["to"]] = TO
    
    if(!is.null(CC)){Email[["cc"]] = CC}
    Email[["subject"]] = SUBJECT
    if(!is.null(TEXT)){Email[["body"]] = TEXT}
  }
  
  if(!is.null(DATA)){
    Email[["htmlbody"]] = paste0("<body>",ifelse(is.null(TEXT), "See table below", TEXT),"<body><br>",
                                 TABLE_HTML(as.matrix(DATA), header = colnames(DATA)),
                                 "<br>","<i>","This email was auto-generated by user ",Sys.getenv("USERNAME"),", ",Sys.time(),"</i><br>"
                                 )
  }
  
  if(!is.null(ATTACH)){
    Email[["attachments"]]$Add(ATTACH)
  }
  
    # Send the message
   Email$Send()
  
    # Close Outlook, clear the message
   rm(Outlook, Email)
}

TABLE_HTML <- function(matrix,header,rowcol=NULL){
  HTMLCODE <- paste("<table class='table table-striped' style='margin-left: auto'><FONT face='calibri' size=3><thead><tr><b>",sep="")
  for(h in 1:length(header)){
    HTMLCODE <- paste(HTMLCODE,"<th style='text-align:left;vertical-align:top;'>",header[h]," &nbsp &nbsp</th>",sep="")
  }
  HTMLCODE <- paste(HTMLCODE,"</b></tr></thead></FONT><tbody>",sep="")
  for(i in 1:dim(matrix)[1]){
    if(is.null(rowcol)){
      if((i %% 2)==0){
        rowCol <- "#F4F5F8"
      }else{
        rowCol <- "FFFFFF"
      }
    }else{
      if(matrix[i,rowcol[1]]>=rowcol[2]){
        rowCol <- "#F4F5F8"
      }else{
        rowCol <- "FFFFFF"
      }
    }
    HTMLCODE <- paste(HTMLCODE,"<FONT face='calibri' size=2><tr bgcolor='",rowCol,"'>",sep="")
    for(k in 1:dim(matrix)[2]){
      if(i==1){
        HTMLCODE <- paste(HTMLCODE,"<td style='text-align:left;vertical-align:top;white-space: nowrap;'>",matrix[i,k]," &nbsp &nbsp </td>",sep="")
      }else{
        HTMLCODE <- paste(HTMLCODE,"<td style='text-align:left;vertical-align:top;white-space: nowrap;'>",matrix[i,k]," &nbsp &nbsp</td>",sep="")
      }
    }
    HTMLCODE <- paste(HTMLCODE,"</tr></FONT>",sep="")
  }
  HTMLCODE <- paste(HTMLCODE,"</tbody></table>",sep="")
  getMailHTML <- HTMLCODE
}
