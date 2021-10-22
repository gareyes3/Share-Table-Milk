#Function for adding time and Adding Growth

Func_Time_Temp<-function(DF, Item_Picked, Temp){
  Time<- runif(1,(10/60),(15/60)) #Time between 10 and 15 minutes. Change this.
  #Parameters for milk spoilage
  b<-.03772
  Tmin<-(-6.1)
  Tmax<-(41.2)
  c<-.1719
  k<-(b*(Temp-Tmin)*(1-exp(c*(Temp-Tmax))))^2
  #How much growth
  Growth<-Time*k
  #N current contamination
  N<-DF[Item_Picked,colnames(DF)== "SpoilageCon"]
  Con_Final<-N + Growth #Growth in log. 
  #Refreshing the contamination in the RoW
  DF[Item_Picked,colnames(DF)== "SpoilageCon"]<-as.numeric(Con_Final)
  #Adding time to the Dataframe? 
  DF[Item_Picked,colnames(DF)== "TotTime"]<-Time
  DF[Item_Picked,colnames(DF)== "PickTS"]<-TRUE
  return(DF)
}


#Funct
DF8<-Func_Time_Temp(DF = Pre_Data.Frame, Item_Picked = 2,Temp = 25)



