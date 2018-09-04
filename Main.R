setwd("//bosch.com/dfsrb/DfsCN/loc/Wx/Dept/TEF/60_MFE_Manufacturing_Engineering/06_Data_Analytics/01_Project/MOE/MOE1/Smart tool change_Friendly user interface")

#source("./Script/GetData.R")# get real time input data

source("Script/FUNCTIONS.R",local = T)

InputData=read.csv("./Input/Input.csv")

#InputData=read.csv("C:/Users/YHC1WX/Desktop/InputData_Sport.csv")

#Product_Types = c("1647Op10", "1647Op20", "2097Op10", "2097Op20", "2415Op10", "2415Op20", "2780")

Product_Types=levels(InputData$Product_Type)

Product_Types="2415_OP10_V006"

MODES = c("SPORT","SIMPLE")

REACTION = 5# Operator reaction time

N = 100000 #  Production volume in simulation

M =1# Run 5 times simulation, use averge

AC2=seq(10,120,10)

AC2 = 70

Waste_star=seq(0,30,5)

Waste_star=20

VSTL = seq(50,200,50)

SCHEME_M_SIMPLE = rev(expand.grid(REACTION = 5,1:M,AC2,999,999,"SIMPLE",Product_Types))

SCHEME_M_SPORT = rev(expand.grid(REACTION = 5,1:M,999,VSTL,Waste_star,"SPORT",Product_Types))

SCHEME_M = rbind(SCHEME_M_SIMPLE,SCHEME_M_SPORT)

names(SCHEME_M) = c("Product_Type","MODE","Waste_star","VSTL","AC2","M","REACTION")

if(!"SIMPLE"%in% MODES){
  
  SCHEME_M=SCHEME_M[SCHEME_M$MODE!="SIMPLE",]
  
}

if(!"SPORT"%in% MODES){
  
  SCHEME_M=SCHEME_M[SCHEME_M$MODE!="SPORT",]
  
}

SCHEME_M$SCRIPT = paste0("Script/",SCHEME_M$MODE,".R")
SCHEME_M[] = lapply(SCHEME_M, as.character)
SCHEME_M$AC2 = as.numeric(SCHEME_M$AC2)
SCHEME_M$VSTL = as.numeric(SCHEME_M$VSTL)
SCHEME_M$Waste_star =as.numeric(SCHEME_M$Waste_star)

LL=list()

for(i in 1:nrow(SCHEME_M)){
  if(i == 1) LL = list()
  
  Product_Type = SCHEME_M[i,"Product_Type"]
  MODE = SCHEME_M[i,"MODE"]
  SCRIPT = SCHEME_M[i,"SCRIPT"]
  M = SCHEME_M[i,"M"]
  AC2=SCHEME_M[i,"AC2"]
  REACTION = SCHEME_M[i,"REACTION"]
  VSTL = SCHEME_M[i,"VSTL"]
  Waste_star = SCHEME_M[i,"Waste_star"]
  
  TEMP_FILE_PATH = paste0("Temps/",Product_Type,"_",MODE,".csv") # edit in scripts
  
  message(paste0(Product_Type, " ",SCRIPT,"    Monte Carlo Round ",M,":"))
  
  # print(AC2)
  # print(class(AC2))
  source(SCRIPT,local = TRUE)
  
  cat("---------------------------\n")
  LL[[i]] = SMR(read.csv(TEMP_FILE_PATH))
  
  print(LL[i])
  
  if(i == nrow(SCHEME_M)){
    LL_DF = do.call(rbind,LL)
    GRAND = data.frame(SCHEME_M, LL_DF, stringsAsFactors = FALSE)
  }
}

library(dplyr)

GRAND_SUMMARY = GRAND %>% group_by(Product_Type,MODE,AC2,VSTL,Waste_star) %>%dplyr::  summarise(
  Total_mean     = mean(Total),
  Total_std      = sd(Total),
  Normal_mean    = mean(Normal),
  Normal_std     = sd(Normal),
  Broken_mean    = mean(Broken),
  Broken_std     = sd(Broken),
  Abnormal_mean  = mean(Abnormal),
  Abnormal_std   = sd(Abnormal),
  Zeiss_mean     = mean(Zeiss),
  Zeiss_std      = sd(Zeiss),
  ChangeNum_mean = mean(ChangeNum),
  ChangeNum_std  = sd(ChangeNum),
  ToolNum_mean   =mean(ToolNum_per_event),
  ToolNum_std    =sd(ToolNum_per_event),
  Tool_Change_Over_Time_mean=mean(Tool_Change_Over_Time),
  Tool_Change_Over_Time_sd=sd(Tool_Change_Over_Time)
)
ROUNDING_NAMES = union(grep("mean", names(GRAND_SUMMARY)),grep("std", names(GRAND_SUMMARY)))

for(i in ROUNDING_NAMES) GRAND_SUMMARY[,i] = round(GRAND_SUMMARY[,i],3)

write.csv(GRAND_SUMMARY,"Result/GRAND_SUMMARY.csv",row.names = FALSE)

FINAL_SUMMARY=CombineCurve(read.csv("Result - 20180223//GRAND_SUMMARY.csv"))

GRAND_SUMMARY=read.csv("Result/GRAND_SUMMARY.csv")

FINAL_SUMMARY=FINAL_SUMMARY[,c("Cost_grid1","Product_Type","MODE","AC2","VSTL",
                               "Waste_star","Total_mean","Zeiss_mean",
                               "Tool_Change_Over_Time_mean")]
### for visualization

GRAND_SUMMARY=GRAND_SUMMARY[GRAND_SUMMARY$Waste_star<=30 | GRAND_SUMMARY$MODE=="SIMPLE",]

p=ggplot(FINAL_SUMMARY,aes(y=Zeiss_mean,x=Cost_grid1))+
  geom_point(data=GRAND_SUMMARY,aes(x=Total_mean,y=Zeiss_mean,color=MODE))+
  facet_wrap(~Product_Type,scales = "free")+
  geom_path()+
  scale_x_continuous(limits = c(0.1,0.8),breaks = seq(0,1,0.1))
p

write.csv(FINAL_SUMMARY,"Result - 20180223//FINAL_SUMMARY.csv",row.names = FALSE)
write.csv(GRAND_SUMMARY,"Result - 20180223//GRAND_SUMMARY.csv",row.names = FALSE)
