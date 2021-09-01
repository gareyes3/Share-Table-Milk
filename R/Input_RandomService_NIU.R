
  
##Extras, Not Used--------------------------------------------------------------------------------------------------------------
  #Remove the _PFU if ant to use
  Inputs_Cont_HuNov_Fr_PFU<-list(
    #Prevalence of NV in Fruit
    Prevalence <-rbetagen(1,0.79,1.03,0.0,0.2),
    #Genormic Copies per PFU NOTE: Not ised right now. 
    Genomic_copies_per_PFU<-rnormTrunc(1,3.65,.98,2.00,5.40), #add 3.66,
    #log HuNoV copies per/ g
    HuNoV_ContFruit<-rlnormTrunc(1,2.38,3.52, 0,6.97) #add 1.578 #
  )
