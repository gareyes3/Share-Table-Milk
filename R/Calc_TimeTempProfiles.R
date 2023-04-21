
library(tidyverse)
library(reshape2)
library(cowplot)

#Negative Slopes
Neg_Slope_RTB<-(-0.0124) #Room Temperature
Neg_Slope_Ref<-(-0.0141) #Refrigeration
Neg_Slope_RT<-(-0.0088) #Refrigerated Tray
Neg_Slope_TIC<-(-0.0057) #Tray with Ice 
Neg_Slope_TIP<-(-0.0072) #Tray with Ice Packs
Neg_Slope_CI<-(-0.0019) #Cooler with Ice


#Determining room temperature vs refrigeration
#ST_Condition_DF$Condition<-ifelse(ST_Condition_DF$Rtemp>10, "Room Temp", "Refrigeration")

##Functions

#Function to get the h (heat transfer coefficient)
get_h<-function(Neg_slope, rho = 1.033, C = 4.2 , V = 0.236, A = 0.004275){
  neg_h =  ((Neg_slope*rho*C*V)/A)
  return(- neg_h)
} 


#Getting the Heat Transfer Coefficients for each Temp
h_RTB = get_h(Neg_slope = Neg_Slope_RTB)
h_ref = get_h(Neg_slope = Neg_Slope_Ref)
h_RT = get_h(Neg_slope = Neg_Slope_RT)
h_TIC = get_h(Neg_slope = Neg_Slope_TIC)
h_TIP = get_h(Neg_slope = Neg_Slope_TIP)
h_CI = get_h(Neg_slope = Neg_Slope_CI)


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


predict_full_milk_temp<-function(df, Temp_Initial){
  Temp_Milk_Vectors<-c()
  Temp_Milk_Vectors<-c(Temp_Initial)
  T_inf = df$Rtemp[1]
  New_Temp = Temp_Initial
  for (i in 2:length(df$Rtemp)){
    #Selecting the condition that applies
    if (df$Condition[i] == "Room Temp"){
      h_condition = h_RTB
    } else if (df$Condition[i] == "Refrigeration"){
      h_condition = h_ref
    } else if (df$Condition[i] == "Refrigerated Tray"){
      h_condition = h_RT
    } else if (df$Condition[i] == "Tray With Ice Packs"){
      h_condition = h_TIP
    } else if (df$Condition[i] == "Tray With Ice"){
      h_condition = h_TIC
    } else if (df$Condition[i] == "Cooler with Ice"){
      h_condition = h_CI
    }
    #Checking if condition changed
    if(df$Condition[i] != df$Condition[i-1]){
      Condition_Change = 1
      T_inf = df$Rtemp[i]
    } else {
      Condition_Change = 0
    }
    
    #Predicting the new temperature
    New_Temp = get_temp(h = h_condition, To = New_Temp, Tinf =  T_inf, time = 1)
    Temp_Milk_Vectors<-c(Temp_Milk_Vectors,New_Temp)
  }
  return(Temp_Milk_Vectors)
}



#Worst case scenario 124, 1314.125 min Room Temp - 21 hr 55 min Refrigerated Overnight"
Time_Temp_Df<-Create_Temperature_Profile_days(First_Cond = "Room Temp", 
                                Second_Cond = "Refrigeration", 
                                First_Cond_Total_Time = 124,
                                Second_Cond_Total_Time = 1314,
                                First_Cond_Mean_Temperature = 22.1, 
                                First_Cond_SD_Temperature = 0,#0.77,
                                Second_Cond_Mean_Temperature= 3.71, 
                                Second_Cond_SD_Temperature = 0,#1.04,
                                Interval  =1, 
                                Cycles = 5)


Temp_Milk_Vectors<-predict_full_milk_temp(df = Time_Temp_Df, Temp_Initial = 4.2)

Df_RT_MT<-data.frame("RoomTemp" = Time_Temp_Df$Rtemp,
                     "MilkTemp" = Temp_Milk_Vectors,
                     "Time" = 1:length(Temp_Milk_Vectors))
Df_RT_MT_melted<-melt(Df_RT_MT, id.vars = "Time")


ggplot(data = Df_RT_MT_melted, aes(x = Time, y = value, color = variable))+
  geom_line(size = 1)+
  theme_bw()+
  labs(x = "Time (min)", y = "Temperature (°C)", title= "125 min Room Temp - 21 hr 55 min Refrigerated Overnight")+
  scale_color_discrete(name = "Temperature Profile")
ggsave("Pedicted Time and Temp Profiles/PredictedRT.png", height = 3, width = 8, dpi = 300)


#Worst case scenario 124, 1314.125 min Ref Tray - 21 hr 55 min Refrigerated Overnight"
Time_Temp_Df_RT<-Create_Temperature_Profile_days(First_Cond = "Refrigerated Tray", 
                                              Second_Cond = "Refrigeration", 
                                              First_Cond_Total_Time = 124,
                                              Second_Cond_Total_Time = 1314,
                                              First_Cond_Mean_Temperature = 22.1, 
                                              First_Cond_SD_Temperature = 0,#0.77,
                                              Second_Cond_Mean_Temperature= 3.71, 
                                              Second_Cond_SD_Temperature = 0,#1.04,
                                              Interval  =1, 
                                              Cycles = 5)




Temp_Milk_RT<-predict_full_milk_temp(Time_Temp_Df_RT, Temp_Initial = 4)

Df_RT_MT_RT<-data.frame("RoomTemp" = Time_Temp_Df$Rtemp,
                     "MilkTemp" = Temp_Milk_RT,
                     "Time" = 1:length(Temp_Milk_Vectors))
Df_RT_MT_RT_melted<-melt(Df_RT_MT_RT, id.vars = "Time")


ggplot(data = Df_RT_MT_RT_melted, aes(x = Time, y = value, color = variable))+
  geom_line(size = 1)+
  theme_bw()+
  labs(x = "Time (min)", y = "Temperature (°C)", title= "125 min Refrigerated Tray - 21 hr 55 min Refrigerated Overnight")+
  scale_color_discrete(name = "Temperature Profile")


ggsave("Pedicted Time and Temp Profiles/PredictedRefTray.png", height = 3, width = 8, dpi = 300)


#Worst case scenario 124, 1314.125 min Tray with ice packs - 21 hr 55 min Refrigerated Overnight"
Time_Temp_Df_TIP<-Create_Temperature_Profile_days(First_Cond = "Tray With Ice Packs", 
                                                 Second_Cond = "Refrigeration", 
                                                 First_Cond_Total_Time = 124,
                                                 Second_Cond_Total_Time = 1314,
                                                 First_Cond_Mean_Temperature = 22.1, 
                                                 First_Cond_SD_Temperature = 0,#0.77,
                                                 Second_Cond_Mean_Temperature= 3.71, 
                                                 Second_Cond_SD_Temperature = 0,#1.04,
                                                 Interval  =1, 
                                                 Cycles = 5)




Temp_Milk_TIP<-predict_full_milk_temp(Time_Temp_Df_TIP, Temp_Initial = 4.2)

Df_RT_MT_TIP<-data.frame("RoomTemp" = Time_Temp_Df$Rtemp,
                        "MilkTemp" = Temp_Milk_TIP,
                        "Time" = 1:length(Temp_Milk_Vectors))
Df_RT_MT_TIP_melted<-melt(Df_RT_MT_TIP, id.vars = "Time")


ggplot(data = Df_RT_MT_TIP_melted, aes(x = Time, y = value, color = variable))+
  geom_line(size = 1)+
  theme_bw()+
  labs(x = "Time (min)", y = "Temperature (°C)", title= "125 min Tray with Ice Packs - 21 hr 55 min Refrigerated Overnight")+
  scale_color_discrete(name = "Temperature Profile")


ggsave("Pedicted Time and Temp Profiles/PredictedTIP.png", height = 3, width = 8, dpi = 300)


#Worst case scenario 124, 1314.125 min Tray with ice packs - 21 hr 55 min Refrigerated Overnight"
Time_Temp_Df_TIP<-Create_Temperature_Profile_days(First_Cond = "Tray With Ice Packs", 
                                                  Second_Cond = "Refrigeration", 
                                                  First_Cond_Total_Time = 124,
                                                  Second_Cond_Total_Time = 1314,
                                                  First_Cond_Mean_Temperature = 22.1, 
                                                  First_Cond_SD_Temperature = 0,#0.77,
                                                  Second_Cond_Mean_Temperature= 3.71, 
                                                  Second_Cond_SD_Temperature = 0,#1.04,
                                                  Interval  =1, 
                                                  Cycles = 5)




Temp_Milk_TIP<-predict_full_milk_temp(Time_Temp_Df_TIP, Temp_Initial = 4.2)

Df_RT_MT_TIP<-data.frame("RoomTemp" = Time_Temp_Df$Rtemp,
                         "MilkTemp" = Temp_Milk_TIP,
                         "Time" = 1:length(Temp_Milk_Vectors))
Df_RT_MT_TIP_melted<-melt(Df_RT_MT_TIP, id.vars = "Time")


ggplot(data = Df_RT_MT_TIP_melted, aes(x = Time, y = value, color = variable))+
  geom_line(size = 1)+
  theme_bw()+
  labs(x = "Time (min)", y = "Temperature (°C)", title= "125 min Tray with Ice Packs - 21 hr 55 min Refrigerated Overnight")+
  scale_color_discrete(name = "Temperature Profile")

ggsave("Pedicted Time and Temp Profiles/PredictedTIP.png", height = 3, width = 8, dpi = 300)


#Worst case scenario 124, 1314.125 min Tray with ice - 21 hr 55 min Refrigerated Overnight"
Time_Temp_Df_TIC<-Create_Temperature_Profile_days(First_Cond = "Tray With Ice", 
                                                  Second_Cond = "Refrigeration", 
                                                  First_Cond_Total_Time = 124,
                                                  Second_Cond_Total_Time = 1314,
                                                  First_Cond_Mean_Temperature = 22.1, 
                                                  First_Cond_SD_Temperature = 0,#0.77,
                                                  Second_Cond_Mean_Temperature= 3.71, 
                                                  Second_Cond_SD_Temperature = 0,#1.04,
                                                  Interval  =1, 
                                                  Cycles = 5)




Temp_Milk_TIC<-predict_full_milk_temp(Time_Temp_Df_TIC, Temp_Initial = 4.2)

Df_RT_MT_TIC<-data.frame("RoomTemp" = Time_Temp_Df$Rtemp,
                         "MilkTemp" = Temp_Milk_TIC,
                         "Time" = 1:length(Temp_Milk_TIC))
Df_RT_MT_TIC_melted<-melt(Df_RT_MT_TIC, id.vars = "Time")


ggplot(data = Df_RT_MT_TIC_melted, aes(x = Time, y = value, color = variable))+
  geom_line(size = 1)+
  theme_bw()+
  labs(x = "Time (min)", y = "Temperature (°C)", title= "125 min Tray with Ice - 21 hr 55 min Refrigerated Overnight")+
  scale_color_discrete(name = "Temperature Profile")

ggsave("Pedicted Time and Temp Profiles/PredictedTIC.png", height = 3, width = 8, dpi = 300)


#Worst case scenario 124, 1314.125 min Cooler with Ice - 21 hr 55 min Refrigerated Overnight"
Time_Temp_Df_CI<-Create_Temperature_Profile_days(First_Cond = "Cooler with Ice", 
                                                  Second_Cond = "Refrigeration", 
                                                  First_Cond_Total_Time = 124,
                                                  Second_Cond_Total_Time = 1314,
                                                  First_Cond_Mean_Temperature = 22.1, 
                                                  First_Cond_SD_Temperature = 0,#0.77,
                                                  Second_Cond_Mean_Temperature= 3.71, 
                                                  Second_Cond_SD_Temperature = 0,#1.04,
                                                  Interval  =1, 
                                                  Cycles = 5)




Temp_Milk_CI<-predict_full_milk_temp(Time_Temp_Df_CI, Temp_Initial = 4.2)

Df_RT_MT_CI<-data.frame("RoomTemp" = Time_Temp_Df$Rtemp,
                         "MilkTemp" = Temp_Milk_CI,
                         "Time" = 1:length(Temp_Milk_CI))
Df_RT_MT_CI_melted<-melt(Df_RT_MT_CI, id.vars = "Time")


ggplot(data = Df_RT_MT_CI_melted, aes(x = Time, y = value, color = variable))+
  geom_line(size = 1)+
  theme_bw()+
  labs(x = "Time (min)", y = "Temperature (°C)", title= "125 min Cooler with Ice - 21 hr 55 min Refrigerated Overnight")+
  scale_color_discrete(name = "Temperature Profile")

ggsave("Pedicted Time and Temp Profiles/PredictedCI.png", height = 3, width = 8, dpi = 300)



######### ----------------------------------------------------------------------
#Spoilage Model.

#Function for growth and lag phase
new_growth_rate<-function(newTemp, oldMu,oldTemp = 6, T0 = -4.15){
  newMu<-((newTemp-T0)/(oldTemp-T0))* oldMu
  return (newMu)
}

#Calculation of the new lag time.
new_lag_time <- function (newTemp, oldLag, oldTemp = 6, T0 = -4.15) {
  numerator <- oldTemp -T0
  denom <- newTemp - T0
  newLag <- ( (numerator / denom)^2) * oldLag
  return(newLag)
}

#This function calculates thee growth based on a time and temperature profile for 1 specific milk with R100084 P Paoae
Func_Growth_LagCon<-function(In_Lag_Consumed, Time_Temp_df,Interval, AF){
  #In_Lag_Consumed= Total lag time consumed
  #Time_Temp_df = dataframe with time and temperature conditions
  #Interval = time interval in the time_temp_df in hrs. 
  Total_Lag_Consumed = In_Lag_Consumed
  Total_Growth = 0
  old_lag = 0
  NMax = 8.14
  old_mumax = 0.083508
  Growth_V = c()
  for (i in 1:nrow(Time_Temp_df)){
    if (Total_Lag_Consumed <1 && old_lag!=0){
      Lag_t_interval<-new_lag_time(newTemp = Time_Temp_df$MilkTemp[i], oldLag = old_lag)
      Lag_Consumed<-Interval/Lag_t_interval
      Total_Lag_Consumed<-Total_Lag_Consumed+Lag_Consumed
      Growth = 0
    } else if (Total_Lag_Consumed>=1 | old_lag == 0){
      Growth = ((new_growth_rate(newTemp = Time_Temp_df$MilkTemp[i], oldMu = old_mumax))/2.303)* AF#0.684 #Converted log10 from log ln
      Total_Growth = Total_Growth + (Growth*Interval)
    }
    Growth_V = c(Growth_V,Total_Growth)
    #print(length(Growth_V))
  }
  return(list(Total_Growth,Total_Lag_Consumed,Growth_V))
}

#Buchanan spoilage function
Spoilage_Function_Single_Milk<-function(Cont, Pop_Max, Time_Temp_df, Interval =1/60, AF){
  Lag_Consumed = 0
  #this function provides two outputs, the total growth, and the new updated lag phase consumed. 
  Output_Milk<-Func_Growth_LagCon(In_Lag_Consumed = Lag_Consumed ,Time_Temp_df = Time_Temp_df,Interval = Interval, AF=AF)
  Lag_Consumed = Output_Milk[[2]]
  Cont<-Output_Milk[[1]]+Cont
  if( Cont>Pop_Max){
    Cont = Pop_Max
  }
  return (list(Cont,Output_Milk[[3]]))
}


#Export all Time and Temp Profiles.

Df_RT_MT
write.csv(Df_RT_MT, file = "Pedicted Time and Temp Profiles/Df_RT_MT.csv")

Df_RT_MT_RT
Df_RT_MT_TIC
Df_RT_MT_TIP
Df_RT_MT_CI

##Spoilage Prediction

#Room Temp 2 hr -----
Output_Milk<-Spoilage_Function_Single_Milk(Cont = 2.44, Pop_Max =8.14, Time_Temp_df = Df_RT_MT, Interval =1/60,AF = 1.32)
Changes_Over_Time<-2.44+Output_Milk[[2]]

Df_RT_MT_melted$Type<-"Temperature Profiles"

data_Milk_G_RTB<-data.frame("Time" = 1:length(Changes_Over_Time),
                            "variable" = "Population Change",
                            "value" = Changes_Over_Time,
                            "Type" = "Population Changes")

new_dat = rbind(Df_RT_MT_melted,data_Milk_G_RTB)

ggplot(data = new_dat, aes(x = Time))+
  geom_line(aes(y = value, color = variable),size = 1)+
  theme_bw()+
  labs(x = "Time (min)", y = "", title= "125 min Room Temp - 21 hr 55 min Refrigerated Overnight")+
  scale_color_discrete(name = "Line Description")+
  facet_wrap(~Type, scales = "free_y", ncol= 1,labeller = as_labeller(c(`Population Changes` = "Population (log CFU/g)", `Temperature Profiles` = "Temperature (°C)") ),strip.position = "left", )
ggsave("Pedicted Time and Temp Profiles/TempandChange_RTB.png", height = 4, width = 8, dpi = 300)

#Refrigerated Tray 2 hr -----
Output_Milk<-Spoilage_Function_Single_Milk(Cont = 2.44, Pop_Max =8.14, Time_Temp_df = Df_RT_MT_RT, Interval =1/60,AF = 1.32)
Changes_Over_Time_RT<-2.44+Output_Milk[[2]]

Df_RT_MT_RT_melted$Type<-"Temperature Profiles"

data_Milk_G_RT<-data.frame("Time" = 1:length(Changes_Over_Time_RT),
                            "variable" = "Population Change",
                            "value" = Changes_Over_Time_RT,
                            "Type" = "Population Changes")

new_dat_RT = rbind(Df_RT_MT_RT_melted,data_Milk_G_RT)

ggplot(data = new_dat_RT, aes(x = Time))+
  geom_line(aes(y = value, color = variable),size = 1)+
  theme_bw()+
  labs(x = "Time (min)", y = "", title= "125 min Room Temp - 21 hr 55 min Refrigerated Overnight")+
  scale_color_discrete(name = "Line Description")+
  facet_wrap(~Type, scales = "free_y", ncol= 1,labeller = as_labeller(c(`Population Changes` = "Population (log CFU/g)", `Temperature Profiles` = "Temperature (°C)") ),strip.position = "left", )
ggsave("Pedicted Time and Temp Profiles/TempandChange_RTB.png", height = 4, width = 8, dpi = 300)



