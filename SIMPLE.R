
Product_Type = Product_Type
REACTION = as.numeric(REACTION)
MODE = "SIMPLE"

#Data preparetion-Orignal
Input_Simple      = InputData[InputData$Presetting_No!="Virtual",]
Input             = Input_Simple[Input_Simple$Product_Type==Product_Type,]
Input             = Input[order(Input$Spindle),]
STL               = t(Input$STL)
SP                = t(Input$Spindle)
Presetting_No     = t(Input$Presetting_No)
Zeiss             = t(Input$FMR_Capacity_Zeiss)
New_Tool_Price    = t(Input$New_Tool_Price)
Cost_Per_Cycle    = t(Input$Cost_Per_Cycle)
Breakage_Rate_Pc  = t(Input$Breakage_Rate_pc)
Other_Abnormal_Pc = t(Input$Other_Abnormal_Rate_pc)

#Data preparetion-Statistic
L                 = length(Presetting_No)
Rate_Initial      = runif(L/2,0,1)
Rate_Initial      = c(Rate_Initial,Rate_Initial)
Initial_TL        = round(Rate_Initial*STL)
Life_realtime     = Initial_TL
Position_Sp1      = which(SP==1)
Position_Sp2      = which(SP==2)

#Output
Change_Record=list()

#Main
for (body in 1:N) {
  
  if( body %%20000 == 0) message(paste0("Round ",body,"."))
  
   line=rep(0,L)
  # Line_Waste=rep(0,L)
  # Line_ChangeLife=rep(0,L)
  
  # B:predict if broken happen
  B1=runif(L,0,1)
  B=(B1<=Breakage_Rate_Pc)
  
  # O:predict if other abnormal happen
  O1=runif(L,0,1)
  O=(O1<=Other_Abnormal_Pc)
  
  #find which tool remain tool life <=10
  Human_rule=round(runif(1,1,REACTION))
  
  l<-length(which(Life_realtime<= Human_rule))
  
  #alert rule for an tool change event:tool life reached/broken/other abnormal
  if(l>=1 | sum(B)>=1 | sum(O)>=1){
    # find the left tool whose remain tool life <=70
    Position1=which(Life_realtime <= AC2)
    # find which tool abnormal
    Position2=which(B=="TRUE")
    Position3=which(O=="TRUE")
    
    # For abnormal case:
    
    
    # Case1:1.Abnormal at 1st half: Life_real time>=0.5*setting tool life
    P_Broken_1sthalf=intersect(Position2,which(Life_realtime>=0.5*STL))
    P_Abnormal_1sthalf=intersect(Position3,which(Life_realtime>=0.5*STL))
    # 2.Only change this abnormal tool
    cost_case1_broken=sum(New_Tool_Price[P_Broken_1sthalf]*0.5+(STL[P_Broken_1sthalf]-Life_realtime[P_Broken_1sthalf])/STL[P_Broken_1sthalf]*Cost_Per_Cycle[P_Broken_1sthalf])
    cost_case1_abnormal=sum(Cost_Per_Cycle[P_Abnormal_1sthalf])
    
    # Case2:Abnormal at 2nd half
    P_Broken_2ndhalf=intersect(Position2,which(Life_realtime<0.5*STL))
    P_Abnormal_2ndhalf=intersect(Position3,which(Life_realtime<0.5*STL))
    # 2. abnormal on Sp1 or Sp2
    P_Broken_2ndhalf_Sp1=intersect(P_Broken_2ndhalf,Position_Sp1)
    P_Broken_2ndhalf_Sp2=intersect(P_Broken_2ndhalf,Position_Sp2)
    P_Abnormal_2ndhalf_Sp1=intersect(P_Abnormal_2ndhalf,Position_Sp1)
    P_Abnormal_2ndhalf_Sp2=intersect(P_Abnormal_2ndhalf,Position_Sp2)
    
    # 3. Change both tools on 2 spindles
    P_Broken_Parallel1=ifelse(length(P_Broken_2ndhalf_Sp1)>0,P_Broken_2ndhalf_Sp1+L/2,0)
    P_Broken_Parallel2=ifelse(length(P_Broken_2ndhalf_Sp2)>0,P_Broken_2ndhalf_Sp2-L/2,0)
    P_Abnormal_Parallel1=ifelse(length(P_Abnormal_2ndhalf_Sp1)>0,P_Abnormal_2ndhalf_Sp1+L/2,0)
    P_Abnormal_Parallel2=ifelse(length(P_Abnormal_2ndhalf_Sp2)>0,P_Abnormal_2ndhalf_Sp2-L/2,0)
    # Case2: position summary
    P_Broken_Parallel=union(P_Broken_Parallel1,P_Broken_Parallel2)
    P_Abnormal_Parallel=union(P_Abnormal_Parallel1,P_Abnormal_Parallel2)
    P_Abnormal_all=union(P_Abnormal_Parallel,P_Abnormal_2ndhalf)
    # case2: cost calculation
    cost_case2_broken=sum(New_Tool_Price[P_Broken_2ndhalf]*0.5)+sum(Life_realtime[P_Broken_Parallel]/STL[P_Broken_Parallel]*Cost_Per_Cycle[P_Broken_Parallel])
    cost_case2_abnormal=sum(Life_realtime[P_Abnormal_all]/STL[P_Abnormal_all]*Cost_Per_Cycle[P_Abnormal_all])
    
    # total cost for abnormal case
    
    cost_broken=cost_case1_broken+cost_case2_broken
    cost_abnormal=cost_case1_abnormal+cost_case2_abnormal
    
    
    # Tool change record for each tool change event:
    
    P_Abnormal_All=union(which(B==TRUE),which(O==TRUE))
    P_Parallel_All=union(P_Abnormal_Parallel,P_Broken_Parallel)
    All_Position=union(union(P_Abnormal_All,P_Parallel_All),Position1)
    All_Position_Reset=union(union(union(P_Abnormal_2ndhalf,P_Broken_2ndhalf),P_Parallel_All),Position1)
    

     line[Position1]="Tool life reached"
     line[Position2]="Broken"
     line[Position3]="Other abnormal"
     line[P_Parallel_All]="parallel exchange"
    
    # Calculate zeiss and total cost
    
    zeiss=max(Zeiss[All_Position])
    cost_normal=sum(Life_realtime[Position1]/STL[Position1]*Cost_Per_Cycle[Position1])
    # Line_Waste[Position1]=Life_realtime[Position1]*Cost_usage[Position1]
    # Line_ChangeLife[Position1]=Life_realtime[Position1]
     number_tool=length(which(line!=0))
    # 
    # if(sum(B)>=1){
    #   change_type="Broken"
    #   if(length(intersect(which(B==TRUE),Position_Trigger))>0){
    #     Trigger_tool="Trigger tool"
    #   }
    #   else{
    #     Trigger_tool="Nontrigger tool"
    #   }
    # 
    # }
    # if(sum(O)>=1){
    #   change_type="Abnormal"
    #   if(length(intersect(which(O==TRUE),Position_Trigger))>0){
    #     Trigger_tool="Trigger tool"
    #   }
    #   else{
    #     Trigger_tool="Nontrigger tool"
    #   }
    # 
    # }
    # 
    # if(l>=1){
    #   change_type="Normal"
    #   if(length(intersect(which(Life_realtime==10),Position_Trigger))>0){
    #     Trigger_tool="Trigger tool"
    #   }
    #   else{
    #     Trigger_tool="Nontrigger tool"
    #   }
    # }
     
     if(sum(B)>=1){
       change_type="Broken"
     }else if(sum(O)>=1){
       change_type="Abnormal"
     }else{
       change_type="normal"
     }
    
    #OEE calcualtion
    #Normal:y=106.3+46.4x   𝑥Abnormal:y=248.4+22.8x
    
    Tool_Change_Over_Time=ifelse(change_type=="Normal",
                                 number_tool/2*46.4+106.3,
                                 number_tool/2*22.8+248.4)
    
    line2=c(body,Tool_Change_Over_Time,change_type,zeiss,cost_normal,cost_broken,cost_abnormal,number_tool)
    
    # Change_Record=rbind(Change_Record,line2)
    # Change_Waste=rbind(Change_Waste,Line_Waste)
    # Change_Tool=rbind(Change_Tool,line)
    # Change_ToolLife=rbind(Change_ToolLife,Line_ChangeLife)
    
    # Change_ToolLife[[body]] = Line_ChangeLife
    Change_Record[[body]] = line2
    # Change_Waste[[body]] = Line_Waste
    # Change_Tool[[body]] = line
    # change a new tool
    Life_realtime[All_Position_Reset]=STL[All_Position_Reset]
    Life_realtime=Life_realtime-1
    
  }
  
  # if no need to change tool, all tool life -1
  else{
    Life_realtime=Life_realtime-1
  }
  #  All=rbind(All,Life_realtime)
  # All[[body]] = Life_realtime
}

Change_Record=do.call(rbind,Change_Record)

Change_Record=as.data.frame(Change_Record)

names(Change_Record)=c("body","Tool_Change_Over_Time","Change_type","Zeiss","cost_normal","cost_broken","cost_abnormal","number_tool")

TEMP_FILE_PATH = paste0("Temps/",Product_Type,"_",MODE,i,".csv")

message(paste0("File ",TEMP_FILE_PATH," written."))

write.csv(Change_Record, TEMP_FILE_PATH)

SMR(read.csv(TEMP_FILE_PATH))
