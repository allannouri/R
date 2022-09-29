MSsqlExecute <- function(SQL = NULL, DATABASE= "C4DW", closeODBC = TRUE){
  # Remember to close the odbc connection
  createODBC <- function(DATABASE){
    library("RODBC")
    conn <- odbcDriverConnect(paste0("Driver={SQL Server}; Server=db-everest-prod; Database=",
                                           DATABASE,
                                           "; Trusted_Connection=yes"))
  }
  
  conn <- createODBC(DATABASE)
  out <- if(!is.null(SQL)){sqlQuery(channel = conn, query = SQL)}
  MSsqlExecute <- out
  
  if(isTRUE(closeODBC)){
    odbcClose(conn)
  }
  return(MSsqlExecute)
}


MSsqlSave <- function(DATA = NULL, DATABASE= "C4DW", TABLENAME = NULL, closeODBC = TRUE, OVERWRITE = FALSE){
  
  createODBC <- function(DATABASE){
    library("RODBC")
    conn <- odbcDriverConnect(paste0("Driver={SQL Server}; Server=db-everest-prod; Database=",
                                     DATABASE,
                                     "; Trusted_Connection=yes"))
  }
  
 conn <- createODBC(DATABASE)
  
  if(!is.null(DATA) | dim(DATA)[1]>0){
    gsub("-","",colnames(DATA))
    gsub(" ","_",colnames(DATA))
    gsub("\\.", "", colnames(DATA))
    
    # Check if dataframe column names are in SQL table names
    if(!OVERWRITE){
      table_schema <- strsplit(TABLENAME, split=".", fixed = T)[[1]][1] # get first string before ".", i.e. the table schema
      table_name <- paste(strsplit(TABLENAME, split=".", fixed = T)[[1]][-1], collapse=".") # everything after ".", i.e. the table name
      
      SQL_table_names <- MSsqlExecute(SQL = paste("Select COLUMN_NAME from ",DATABASE, ".INFORMATION_SCHEMA.COLUMNS", #
                                                  " where TABLE_SCHEMA = '", table_schema, "'",
                                                  " AND TABLE_NAME = '", table_name,"'", sep =""))$COLUMN_NAME
      
      # print columns in SQL table that isn't in dataset:
      if(prod(SQL_table_names %in% colnames(DATA))==0){
        print(paste("Variables missing are:",paste(SQL_table_names[!(SQL_table_names %in% colnames(DATA))], collapse=", ")))
      }
      # print columns in dataset that isn't in SQL table:
      if(prod(SQL_table_names %in% colnames(DATA))==0){
        print(paste("Variables missing are:",paste(SQL_table_names[!(SQL_table_names %in% colnames(DATA))], collapse=", ")))
      }
    }
    
    path <- paste0("C:/Users/",Sys.getenv("USERNAME"),"/dataframe.txt")
    write.table(DATA, path, sep = ";", row.names = F, col.names = T)
    DATA <- read.table(path, header = T, sep = ";")
    unlink(path)
    
   
    if(isTRUE(OVERWRITE)){
      MSsqlExecute(paste0("DELETE FROM ",DATABASE,".",TABLENAME))
    }
    
    sqlSave(channel = conn, dat = DATA, tablename = TABLENAME, append = T, safer = T, rownames = F, verbose = TRUE)
  
  }
 
  if(isTRUE(closeODBC)){
    odbcClose(conn)
  }

}


UpdateLog <- function(ID, SUBID, TASK, FREQ, START = TRUE, END = FALSE){
  
  res <- MSsqlExecute(paste0("SELECT ID, SUB_ID, TASK FROM [CfResearch].[QR].[Log] WHERE TASK = '", TASK, "'"))
  
  if(dim(res)[1] == 0){
    ID <-  MSsqlExecute("SELECT 1+MAX(ID) FROM [CfResearch].[QR].[Log]")
    SUBID <- 1
    MSsqlExecute(paste0("INSERT INTO [CfResearch].[QR].[Log] VALUES (",paste0(ID),", ", SUBID, ", '",TASK,"', NULL, NULL,", 0, ", '", toupper(Sys.getenv("USERNAME")),"', '",FREQ,"' )"))
  print("Task is succesfully created")
  }
 
  res <- MSsqlExecute(paste0("SELECT ID, SUB_ID FROM [CfResearch].[QR].[Log] WHERE ID = ", ID, " AND SUB_ID = ", SUBID))
  if(dim(res)[1] == 0){
    SUBID <-  MSsqlExecute(paste0("SELECT 1+MAX(SUB_ID) FROM [CfResearch].[QR].[Log] WHERE ID = ", ID))
    MSsqlExecute(paste0("INSERT INTO [CfResearch].[QR].[Log] VALUES (",paste0(ID),", ", SUBID, ", '",TASK,"', NULL, NULL,", 0, ", '", toupper(Sys.getenv("USERNAME")),"', '",FREQ,"' )"))
  print("SUB task is succesfully created")
  }
  
  if(isTRUE(START) && isFALSE(END)){
    MSsqlExecute(paste0("UPDATE [CfResearch].[QR].[Log] SET START_TIME = '",
                        as.character(Sys.time()),"' , UPDATE_USER = '",toupper(Sys.getenv("USERNAME")),
                      "', END_TIME = NULL, EXEC_TIME = NULL WHERE ID = ", ID, " AND SUB_ID = ", SUBID))
  }
  
  if(isTRUE(END) && isFALSE(START)){
    EXEC_TIME <-  MSsqlExecute(paste0("SELECT START_TIME FROM [CfResearch].[QR].[Log] WHERE ID = ", ID))
    EXEC_TIME <- difftime(Sys.time(),EXEC_TIME$START_TIME,units = "mins")
    MSsqlExecute(paste0("UPDATE [CfResearch].[QR].[Log] SET END_TIME = '",
                        as.character(Sys.time()),"' , UPDATE_USER = '",toupper(Sys.getenv("USERNAME")),
                        "', EXEC_TIME = ",round(as.numeric(EXEC_TIME),4),
                        " WHERE ID = ", ID, " AND SUB_ID = ", SUBID))
  }
  
  print("Log is updated")
}
  


AzureExecute <- function(SQL = NULL, DATABASE= NULL, closeODBC = FALSE){
  
  createODBC <- function(DATABASE){
    library("RODBC")
    conn <- odbcDriverConnect(paste0("Driver={ODBC Driver 17 for SQL Server}; Server=mi-c4-share-register.public.c8c6f9050130.database.windows.net,3342; Database=",
                                     DATABASE,
                                     "; Trusted_Connection=yes"))
  }
  
  if(is.null(DATABASE)){
    DATABASE <- "cfrms_prod"
  }
  
  conn <- createODBC(DATABASE)
  out <- if(!is.null(SQL)){sqlQuery(channel = conn, query = SQL)}
  AzureExecute <- out
  
  if(isTRUE(closeODBC)){
    odbcClose(conn)
  }
  
  return(AzureExecute)
}


