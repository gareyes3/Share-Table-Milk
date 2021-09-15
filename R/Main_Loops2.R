
#Week Iteration =============================================================
for (l in 1:Sens_Iterations){
  source("Input_RandomWeeks.R") #To change Methods every Week. eh Handwashing, Sandsanitizer.
  #Day Iteration=============================================================
  for (k in 1:Food_Days){
    #Service Iteration ======================================================
    for (j in 1:Service_No){
      #Creation of Data Frame for new Service. 
      source("Util_DataFrames.R")  
      #Beginning of Student Iteration========================================
      for (z in 1:N_Iterations){
        Outputs_Student_Loop<-Main_Loop()
        Fr_Data.Frame<-Outputs_Student_Loop$Fr_Data.Frame
        Pss_Data.Frame<-Outputs_Student_Loop$Pss_Data.Frame
        Pre_Data.Frame<-Outputs_Student_Loop$Pre_Data.Frame
      }
      
      #Updated items from not consumed, not shared, etc to wasted. 
      
      Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Not Shared"]<-"Discarded"
      
      Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Not Consumed"]<-"Discarded"

      
      #####
      if(k==Food_Days && j==Service_No){
        Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Selection Table"]<-"Discarded"
        Fr_Data.Frame$Location[Fr_Data.Frame$Location=="Shared"]<-"Discarded"
        Fr_Data.Frame$Location[Fr_Data.Frame$Location=="SharedAside"]<-"Discarded"
      }
      
      Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Not Shared"]<-"Discarded"
      
      Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Not Consumed"]<-"Discarded"

      
      #####
      if(k==Food_Days && j==Service_No ){
        Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Selection Table"]<-"Discarded"
        Pss_Data.Frame$Location[Pss_Data.Frame$Location=="Shared"]<-"Discarded"
        Pss_Data.Frame$Location[Pss_Data.Frame$Location=="SharedAside"]<-"Discarded"
      }
      
      Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Not Shared"]<-"Discarded"
      Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Not Consumed"]<-"Discarded"

      #####
      if(k==Food_Days && j==Service_No ){
        Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Selection Table"]<-"Discarded"
        Pre_Data.Frame$Location[Pre_Data.Frame$Location=="Shared"]<-"Discarded"
        Pre_Data.Frame$Location[Pre_Data.Frame$Location=="SharedAside"]<-"Discarded"
        
      }
      
      
      
      #Adding Services
      Fr_Data.Frame<-func_Add_Services(Fr_Data.Frame)
      Pss_Data.Frame<-func_Add_Services(Pss_Data.Frame)
      Pre_Data.Frame<-func_Add_Services(Pre_Data.Frame)
      
      #Adding the data to the datalist
      
      datalistFr[[j]]<-Fr_Data.Frame
      datalistPss[[j]]<-Pss_Data.Frame
      datalistPre[[j]]<-Pre_Data.Frame
      
      source("Output_Services.R") 
      
      List_Sens_Fr[[paste(l,k,j)]]<-Fr_Data.Frame
      List_Sens_Pss[[paste(l,k,j)]]<-Pss_Data.Frame
      List_Sens_Pre[[paste(l,k,j)]]<-Pre_Data.Frame
      
      #message("Service #", j)
      
    } #end of second loop
    
    #Creation of the Services Data Frames
    Fr_Data = do.call(rbind,datalistFr)
    Pss_Data = do.call(rbind,datalistPss)
    Pre_Data = do.call(rbind,datalistPre)
    
    #Adding Data into the Datalists
    datalistFr_days[[k]]<-Fr_Data
    datalistPss_days[[k]]<-Pss_Data
    datalistPre_days[[k]]<-Pre_Data
    
    source("Output_Days.R") 
    
    message("Day #", k)
    
  }#end of day loop k

  
  
  
  message("Done Gathering Data", l)
  end_time<-Sys.time()
  
  Total_time<-end_time-start_time
  print(Total_time)
} #end of l loop for iterations. 





                                      





