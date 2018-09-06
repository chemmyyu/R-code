#sql
library(DBI)
library(rJava)
library(RJDBC)
library(dplyr)
library(plyr)

setwd("ccc")

source("./Script/GetData_AbnormalRate.R")

drv <-JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver","Other/sqljdbc4-2.0.jar")

conn <- dbConnect(drv, "jdbc:sqlserver:XXXX;databaseName=xxxx","xxxx","xxxx")

TN  = grep("R_",dbListTables(conn),value = TRUE)

data0=dbGetQuery(conn, 'SELECT xxxx')
DBI::dbDisconnect(conn)
message(paste0("Good!Get input data successfully"))

############  Oracle  #########

library(rJava)
library(DBI)
library(RJDBC)
library(lubridate)
library(dplyr)

drv <-JDBC("oracle.jdbc.driver.OracleDriver","./ojdbc7.jar") 
conn <-DBI:: dbConnect(drv, "jdbc:oracle:thin:@xxx","xxx","xxx") 
# get raw data, time period is: last two months
end_date=Sys.Date()
Raw_data=DBI::dbGetQuery(conn,"SELECT xxxx",end_date-180, end_date)

DBI::dbDisconnect(conn)

