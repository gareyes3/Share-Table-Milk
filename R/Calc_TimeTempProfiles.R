
#Negative Slopes
Neg_Slope_RTB<-(-0.0124)
Neg_Slope_Ref<-(-0.0141)


#Loading temperature for Room Temp Condition.
ST_Condition_DF<-read.csv("5Days-Adjusted-Temp-2hr-Cycles.csv")

#Determining room temperature vs refrigeration
ST_Condition_DF$Condition<-ifelse(ST_Condition_DF$Rtemp>10, "Room Temp", "Refrigeration")

##Functions

#Function to get the h (heat transfer coefficient)
get_h<-function(Neg_slope, rho = 1.033, C = 4.2 , V = 0.236, A = 0.004275){
  neg_h =  ((Neg_slope*rho*C*V)/A)
  return(- neg_h)
} 

get_temp<-function(h, To, Tinf, time,  A =0.004275,rho =1.033, C= 4.2, V = 0.236 ){
  #Time in minutes
  #Tinf = External Temp
  #To = Initial temperature of the milk
  # h = convection trans coeff for that codition
  return (exp(-(h*A/rho*C*V)*(time))*(To-Tinf)+Tinf)
}

#Function to get the temperature profile from an external temperature vector
predict_temp_fromProf<-function(Time_Temp_Prof, Initial_Temp,h_condition){
  Time_Temp_Profile<-Time_Temp_Prof
  Temp_Initial = Initial_Temp
  Temp_V<-c()
  for (i in 1:length(Time_Temp_Profile)){
    T_inf<-Time_Temp_Profile[i]
    if (i == 1){
      New_Temp = Temp_Initial
    }else if (i == 2){
      New_Temp = get_temp(h = h_condition, To = Temp_Initial, Tinf =  T_inf, time = 1)
    }else{
      New_Temp = get_temp(h = h_condition, To = New_Temp, Tinf =  T_inf, time = 1)
    }
    Temp_V<-c(Temp_V, New_Temp)
  }
  return(Temp_V)
}


### getting the h (Heat Transfer Coefficients)
#Room Temperature
h_RTB = get_h(Neg_slope = Neg_Slope_RTB)

#Refrigeration
h_ref = get_h(Neg_slope = Neg_Slope_Ref)

#Generate Temperature Conditions
#Temperatures are a normal distribution
Time_Temp_Creation_Var<-function(Total_Time, Interval, Mean_Temperature, SD_Temperature){
  Time_Temp_df<-rnorm(n =Total_Time+1, mean = Mean_Temperature, sd= SD_Temperature)
  return (Time_Temp_df)
}

Create_Temperature_Profile_days<-function(First_Cond, Second_Cond, 
                                          First_Cond_Total_Time,Second_Cond_Total_Time,
                                          First_Cond_Mean_Temperature, First_Cond_SD_Temperature,
                                          Second_Cond_Mean_Temperature, Second_Cond_SD_Temperature,
                                          Interval, Cycles){
  
    First_Cond_Temps<-Time_Temp_Creation_Var(Total_Time = First_Cond_Total_Time, Interval = Interval , 
                           Mean_Temperature = First_Cond_Mean_Temperature, 
                           SD_Temperature = First_Cond_SD_Temperature)
    Second_Cond_Temps<-Time_Temp_Creation_Var(Total_Time = Second_Cond_Total_Time, Interval = Interval , 
                                        Mean_Temperature = Second_Cond_Mean_Temperature, 
                                        SD_Temperature = Second_Cond_SD_Temperature)
    
    
    First_Cond_Temps_Desc<-rep(First_Cond,length(First_Cond_Temps))
    Second_Cond_Temps_Desc<-rep(Second_Cond,length(Second_Cond_Temps))
    
    Temp_Vector<-c()
    Cond_Vec<-c()
    for (i in 1:Cycles){
      Temp_Vector<-c(Temp_Vector,First_Cond_Temps,Second_Cond_Temps)
      Cond_Vec<-c(Cond_Vec,First_Cond_Temps_Desc,Second_Cond_Temps_Desc)
    }
    
    
    Simulated_Conditions<-data.frame("Rtemp" = Temp_Vector,
                                      "Condition" = Cond_Vec)
    return(Simulated_Conditions)
}


Time_Temp_Df<-Create_Temperature_Profile_days(First_Cond = "Room Temp", 
                                Second_Cond = "Refrigeration", 
                                First_Cond_Total_Time = 124,
                                Second_Cond_Total_Time = 1314,
                                First_Cond_Mean_Temperature = 22.1, 
                                First_Cond_SD_Temperature = 0.77,
                                Second_Cond_Mean_Temperature= 3.71, 
                                Second_Cond_SD_Temperature = 1.04,
                                Interval  =1, 
                                Cycles = 5)

Temp_Milk_Vectors<-c()
Temp_Initial = 4.2
Temp_Milk_Vectors<-c(Temp_Initial)
T_inf = Time_Temp_Df$Rtemp[1]
for (i in 2:length(Time_Temp_Df$Rtemp)){
  #Selecting the condition that applies
  if (Time_Temp_Df$Condition[i] == "Room Temp"){
    h_condition = h_RTB
  } else if (Time_Temp_Df$Condition[i] == "Refrigeration"){
    h_condition = h_ref
  }
  #Checking if condition changed
  if(Time_Temp_Df$Condition[i] != Time_Temp_Df$Condition[i-1]){
    Condition_Change = 1
    T_inf = Time_Temp_Df$Rtemp[i]
  } else {
    Condition_Change = 0
  }
  
  #Predicting the new temperature
  New_Temp = get_temp(h = h_condition, To = New_Temp, Tinf =  T_inf, time = 1)
  Temp_Milk_Vectors<-c(Temp_Milk_Vectors,New_Temp)
}

plot(Temp_Milk_Vectors, type = "S", col = "red")
lines(Time_Temp_Df$Rtemp, type = "S", col = "blue")





