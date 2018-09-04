#SMR
SMR = function(D, N = 200000){
  
  ROUNDING_N = 3
  
  Normal = round(sum(D$cost_normal)/N,ROUNDING_N)
  
  Abnormal = round(sum(D$cost_abnormal)/N,ROUNDING_N)
  
  Broken = round(sum(D$cost_broken)/N,ROUNDING_N)
  
  Total = Normal + Abnormal + Broken  
  
  if("Only_Virtual" %in% names(D)){
    ChangeNum = sum(D$Only_Virtual=="Non-Virtual")
  } else{
    ChangeNum = nrow(D)
  }
  
  Zeiss = sum(D$Zeiss)
  
  Tool_Change_Over_Time=sum(D$Tool_Change_Over_Time)
  
  ToolNum_per_event=sum(D$number_tool)/nrow(D)
  
  data.frame(Total, Abnormal, Broken, Normal, Zeiss, ChangeNum,ToolNum_per_event,Tool_Change_Over_Time) 
}

#CombineCurve

CombineCurve=function(D){
  
  library(ggplot2)
  
  Cost_grid=seq(0,1,0.001)
  
  n=length(Cost_grid)
  
  Product_Types=unique(D$Product_Type)
  
  Data2=list()
  
  for(j in 1:length(Product_Types)){
    
    D1=D[D$Product_Type==Product_Types[j],]
    
    for(i in 1:n){
      
      #Position of the closest rule based on cost
      
      V1=Cost_grid[i]-D1$Total_mean
      
      n2=ifelse(max(V1)>=0,which(V1==min(V1[V1>=0])),which.min(D1$Total_mean))
      
      #Rule from that position
      Data_temp=cbind(Cost_grid=Cost_grid[i],D1[n2,])
      
      Data2[[(j-1)*n+i]]=Data_temp
    }
  }
  
  Data2=do.call(rbind,Data2)
  
  
  #FMR should decreasing as cost grid increase
  
  library(dplyr)
  
  Data3=Data2%>% group_by(Product_Type)%>% do({
    
    D=.
    
    mark1=D$Zeiss_mean[1]
    
    mark2=1
    
    n_result=1
    
    for(i in 2:nrow(D)){
      
      n_temp=ifelse((D$Zeiss_mean[i]>mark1),mark2,i)
      
      n_result=c(n_result,n_temp)
      
      mark1=D$Zeiss_mean[n_temp]
      
      mark2=n_temp
    }
    
    data.frame(Cost_grid1=Cost_grid,D[n_result,]) 
    
  })
  
  Data3
  
}


