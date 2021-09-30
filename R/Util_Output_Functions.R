#Functions for outputs


#Updating not consumed items. 
func_update_notcons<-function(df){
  df$Location[df$Location=="Not Shared"]<-"Discarded"
  df$Location[df$Location=="Not Consumed"]<-"Discarded"
  if(k==Food_Days && j==Service_No){
    df$Location[df$Location=="Selection Table"]<-"Discarded"
    df$Location[df$Location=="Shared"]<-"Discarded"
    df$Location[df$Location=="SharedAside"]<-"Discarded"
  }
  return(df)
}


# Code To add Services to Total 

func_Add_Services<-function(DF){
  for(i in 1:nrow(DF)){
    DF[i,colnames(DF)=="TotServices"]<-Func_Index_DF(DF,i,"TotServices")+1
  }
  return(DF)
}


#Selecting Outputs based on Options

func_mainloop_outs<-function(){
  if (Sim_Fruit ==1 && Sim_PRE ==0 && Sim_PSS ==0){
    Outputs_Student_Loop<-list(Fr_Data.Frame=Fr_Data.Frame) #Fruit on
  } else if (Sim_Fruit ==0 && Sim_PRE ==1 && Sim_PSS ==0){
    Outputs_Student_Loop<-list(Pre_Data.Frame=Pre_Data.Frame) #PRE on
  } else if (Sim_Fruit ==0 && Sim_PRE ==0 && Sim_PSS ==1){
    Outputs_Student_Loop<-list(Pss_Data.Frame=Pss_Data.Frame) #PSS on
  } else if (Sim_Fruit ==1 && Sim_PRE ==1 && Sim_PSS ==0){
    Outputs_Student_Loop<-list(Fr_Data.Frame=Fr_Data.Frame,Pre_Data.Frame=Pre_Data.Frame) #Fruit and PRE on
  } else if (Sim_Fruit ==1 && Sim_PRE ==0 && Sim_PSS ==1){
    Outputs_Student_Loop<-list(Fr_Data.Frame=Fr_Data.Frame,Pss_Data.Frame=Pss_Data.Frame) #Fruit and PSS on
  } else if (Sim_Fruit ==0 && Sim_PRE ==1 && Sim_PSS ==1){
    Outputs_Student_Loop<-list(Fr_Data.Frame=Fr_Data.Frame,Pss_Data.Frame=Pss_Data.Frame) #PRE and PSS on
  } else if (Sim_Fruit ==1 && Sim_PRE ==1 && Sim_PSS ==1){
    Outputs_Student_Loop<-list(Fr_Data.Frame=Fr_Data.Frame,Pss_Data.Frame=Pss_Data.Frame, Pre_Data.Frame = Pre_Data.Frame) #Fruit, PRE and PSS on
  }
  return (Outputs_Student_Loop)
}







































##Removed: 


# #Updated items from not consumed, not shared, etc to wasted. 
# Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Not Shared"]<-"Discarded"
# Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Not Consumed"]<-"Discarded"
# #####  Discard Items from the final service. 
# if(k==Food_Days && j==Service_No){
#   Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Selection Table"]<-"Discarded"
#   Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Shared"]<-"Discarded"
#   Fr_Data.Frame$Location[Fr_Data.Frame$Location=="SharedAside"]<-"Discarded"
# }
# 
# #Updated items from not consumed, not shared, etc to wasted.
# Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Not Shared"]<-"Discarded"
# Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Not Consumed"]<-"Discarded"
# ##### Discard Items from the final service.
# if(k==Food_Days && j==Service_No ){
#   Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Selection Table"]<-"Discarded"
#   Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Shared"]<-"Discarded"
#   Pss_Data.Frame$Location[Pss_Data.Frame$Location=="SharedAside"]<-"Discarded"
# }
# 
# #Updated items from not consumed, not shared, etc to wasted.
# Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Not Shared"]<-"Discarded"
# Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Not Consumed"]<-"Discarded"
# ##### Discard Items from the final service.
# if(k==Food_Days && j==Service_No ){
#   Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Selection Table"]<-"Discarded"
#   Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Shared"]<-"Discarded"
#   Pre_Data.Frame$Location[Pre_Data.Frame$Location=="SharedAside"]<-"Discarded"
#   
# }

