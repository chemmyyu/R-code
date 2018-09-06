#Get input data
library(DBI)
library(rJava)
library(RJDBC)
library(dplyr)
library(plyr)

setwd("//bosch.com/dfsrb/DfsCN/loc/Wx/Dept/TEF/60_MFE_Manufacturing_Engineering/06_Data_Analytics/01_Project/MOE/MOE1/Smart tool change_Friendly user interface")

source("./Script/GetData_AbnormalRate.R")

drv <-JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver","Other/sqljdbc4-2.0.jar")

conn <- dbConnect(drv, "jdbc:sqlserver://WX-DB-TD01;databaseName=CutterManage2015","CutterManage_RS","CutterManage@28_RW")

TN  = grep("R_",dbListTables(conn),value = TRUE)

data0=dbGetQuery(conn, 'SELECT A.STL, A.Cost_Per_Cycle, A.FMR_Capacity, B.Presetting_No, B.Product_Type
FROM R_MASTER_Presetting A RIGHT JOIN R_MASTER_Presetting_Product B ON A.Presetting_No = B.Presetting_No')

data2=dbGetQuery(conn,'SELECT A.CutCode,SUM(A.Avg_Price) as New_Tool_Price, B.Presetting_No
                 FROM R_MASTER_CutCode A LEFT JOIN R_MASTER_CutCode_Presetting B ON A.CutCode = B.CutCode 
                 GROUP BY A.CutCode, A.Value_Stream,B.Presetting_No')

data_ValueStream=dbGetQuery(conn,'SELECT * FROM R_MASTER_Product')

DBI::dbDisconnect(conn)

#test1=dbGetQuery(conn,'select * from R_MASTER_Presetting_Product')

Product_Type=unique(data0$Product_Type)

VT=expand.grid(Product_Type=Product_Type,Presetting_No='Virtual',
               STL=200,Cost_Per_Cycle=0,FMR_Capacity=0)

data1=rbind(data0,VT)

data3=merge(data1,data2,by ="Presetting_No",all.x = T)

Data_Rate=read.csv("./Input/Data_Rate.csv")

data4=merge(data3,Data_Rate,by="Presetting_No",all.x =T)

data4$Breakage_Rate_pc=data4$Breakage_Rate/data4$STL

data4$Other_Abnormal_Rate_pc=data4$Other_Abnormal_Rate/data4$STL

Input1=data4[,c('Presetting_No','STL','Cost_Per_Cycle','FMR_Capacity',
               'Product_Type','New_Tool_Price','Breakage_Rate_pc',
               'Other_Abnormal_Rate_pc')]

Input1[is.na(Input1) & (Input1$Presetting_No=="Virtual")]=0

Input1=na.omit(Input1)
#Two spindles+Virtual tool
Input1$Spindle=1
Input2=Input1
Input2$Spindle=2
Input=rbind(Input1,Input2)
Input=plyr:: rename(Input,c("FMR_Capacity"="FMR_Capacity_Zeiss"))

write.csv(Input,"./Input/Input.csv")

message(paste0("Good!Get input data successfully"))


