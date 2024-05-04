## --data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAWElEQVR42mNgGPTAxsZmJsVqQApgmGw1yApwKcQiT7phRBuCzzCSDSHGMKINIeDNmWQlA2IigKJwIssQkHdINgxfmBBtGDEBS3KCxBc7pMQgMYE5c/AXPwAwSX4lV3pTWwAAAABJRU5ErkJggg==---------------------------------bank model  - MD1-----------------------------------------------------------

##----------------------------------------- 1.  all functions ------------------------------------------------

#countertime calculates the time each entity waits at the counter
countertime <- function() {
  # random number between 0-1
  u <- runif(1, 0, 1)
  
  # create countertime according to function
  if (u <= 47 / 60) {
    countertime <- - 20 + sqrt(529 + 60 * u)
  }
  else{
    countertime <- (120 * u - 42) / 13
  }
  
  return(countertime)
}



# chooses the first treatment pets go to after reception
first_treatment_pets<-function() {
  typenumber<-function()runif(1, min =0, max = 1)
  if (typenumber()<0.31) {
    #emergency_vet=1
    print(paste("I should go to emeregency vet"))
    return(1)
  }
  if (typenumber()>0.61) {
    typenumber2<-function()runif(1, min =0, max = 1)
    if(typenumber2()<0.79) {
      #first_group treamtment=2
      return (2)
    }
    else {
      #second_group treatment=3
      return (3)
    }
  }
  else {
    #regular vet pets=4
    return(4)
  }
}

# chooses the first treatment horses go to after reception
first_treatment_horses<-function() {
  typenumber<-function()runif(1, min =0, max = 1)
  if (typenumber()<0.33) {
    return(1)
  }
  if (typenumber()>0.79) {
    #regular vet horses=5
    return(5)
  }
  else {
    typenumber3<-function()runif(1, min =0, max = 1)
    if(typenumber3()<0.89) {
      return (2)
    }
    else {
      return (3)
    }
  }
}

# chooses the first treatment wild animals go to- only emergency possible
first_treatment_wild<-function() {
  return(1)
}


# determine whether the surgery will succeed
surgery_fail_rate <- function(ZooHospital) {
  start1<- get_attribute(ZooHospital, "Start_time") #the time animal entered the queue
  end1<- get_attribute(ZooHospital, "End time") #the tima animal started the surgery
  t<- (end1-start1)/60 #convert to hours
  if (t >= 0 & t <= 16) {
    return(1 / (1 + exp(-(3 * t - 8))))
  }
  else {
    return(0)
  }
}

#Determine the order of the 3 possibilities- hair cut,wash,nail treatment
Cosmeticresource<-function(ZooHospital){
  hair_cuter <- get_attribute(ZooHospital,"hair_cuter")
  washer <- get_attribute(ZooHospital,"washer")
  nail_cuter <- get_attribute(ZooHospital,"nail_cuter")
  return_vec<- character(0) #empty vector, all possible treatments (that animal didn't do yet) are added
  
  if(identical(washer,0)){
    return_vec<-c(return_vec,"washer") #if washer wasn't used yet, its added to the possibilities vector
  }
  if(identical(hair_cuter,0)){
    return_vec<-c(return_vec,"hair_cuter") #if hair cuter wasn't used yet, its added to the possibilities vector
  }
  if(identical(nail_cuter,0)){
    return_vec<-c(return_vec,"nail_cuter") #if nail cuter wasn't used yet, its added to the possibilities vector
  }
  return(return_vec)
}




#return how long hair cut,wash or nail treatment take, according to what's needed
cosmetic_first_group_treatments_timeout <- function(resource_name) {
  if (identical(resource_name,c("hair_cuter"))) {
    return(function () rnorm(1,mean = 90,sd=20))  # Timeout for hair_cuter
  } else if (identical(resource_name,c("washer"))) {
    return(function () rnorm(1,mean = 60,sd=15))  # Timeout for washer
  } else {
    return(function () rnorm(1,mean = 15,sd=2))  # Timeout for nail_cuter
  }
}


#According to animal type choose the relevant vet- 1 for wild, 2 for pets and horses
what_vet <- function(ZooHospital) {
  animal<-get_attribute(ZooHospital,"type")
  if (animal == 3) {
    return(1)
  }
  else {
    return(2)
  }
}

# determine what visit summary trajectory to go to- 1 is for wild animals and 0 for others
what_sum <- function(ZooHospital) {
  animal<-get_attribute(ZooHospital,"type")
  if (animal == 3) {
    return(1)
  }
  else {
    return(0)
  }
}

#Function to help get emergency vet queue
avgQueue <- function(time, queueLength, simTime){
  Lavg = 0;
  L = queueLength[1];
  Tnow = time[1];
  Llast = time[1];
  TL = 0;
  Tmax = simTime;
  if (length(time) == length(queueLength)){
    for (i in 2:length(time)){
      if(queueLength[i] != queueLength[i-1]){
        Tnow = time[i];
        TL = TL+L*(Tnow-Llast);
        L = queueLength[i];
        Llast = Tnow;
      }
    }
  }
  TL=TL+L*(Tmax-Llast);
  Lavg = TL/Tmax;
  return (Lavg);
}

##----------------------------------------- 2.  all simulation parameters ------------------------------------------------

simulationTime<-16*60

##----------------------------------------- 3.  Init Simulation and add all resources  ------------------------------------------------
ZooHospital<- simmer()%>%
  #regular treatment & starting counter resources
  add_resource("counter",capacity=3,queue_size=Inf)%>%
  add_resource("regular_vet_wild",capacity=1,queue_size=Inf)%>%
  add_resource("regular_vet_other",capacity=1,queue_size=Inf)%>%
  
  #emergency resources
  add_resource("emergency_vet",capacity=1,queue_size=Inf)%>%
  add_resource("emergency_bed",capacity=15,queue_size=0)%>%  
  add_resource("emergency_operation_room",capacity=2,queue_size=Inf)%>%
  add_resource("door", capacity = Inf)%>% #When time is over, this will lock patients out
  
  #visit summary resources
  add_resource("sum_counter",capacity=2,queue_size=Inf)%>%
  
  #nurse resources
  add_resource("nurse_meds",capacity=1,queue_size=Inf,preemptive=TRUE)%>%
  add_resource("nurse_vaccine",capacity=1,queue_size=Inf,preemptive=TRUE)%>%
  
  #cosmetic first group treatments resources
  add_resource("hair_cuter",capacity=1,queue_size=Inf,preemptive=FALSE)%>%
  add_resource("washer",capacity=1,queue_size=Inf,preemptive=FALSE)%>%
  add_resource("nail_cuter",capacity=1,queue_size=Inf,preemptive=FALSE)%>%
  
  #cosmetic second group treatments resources
  add_resource("teeth_cleaner",capacity=1,queue_size=Inf,preemptive=FALSE)

##----------------------------------------- 4.  All trajectories, start from main trajectory and add sub-trajectories ABOVE IT it . ------------------------------------------------

#wild animals visit summary trajectory
visitsumwild<-
  trajectory("visitsum wild path")%>%
  log_("i arrived to visitsum wild animals")%>%
  set_attribute("Total_To_Pay", value = 0)%>%
  batch(n=5, timeout=60, permanent=FALSE,name="fiveWildAnimals")%>% #batch 5 wild animals and seize the sum counter together
  log_("we finished the batch")%>%
  seize("sum_counter",1)%>%
  timeout(function() rnorm(1,mean = 20,sd=4))%>%
  release("sum_counter",1)

#visit summary for all animals
visitsum<-
  trajectory("visitsum path")%>%
  log_("i arrived to visitsum")%>%
  branch(option = function() what_sum(ZooHospital),
         continue=c(FALSE),visitsumwild)%>% #Wild animals are sent to another traj inorder to batch there
  seize("sum_counter",1)%>%
  timeout_from_attribute("summary_time")%>%
  release("sum_counter",1)



#Three possible treatments that are chosen by the first available- wash, hair cut and nail treatment
first_group_treatment <-
  trajectory("first_group path") %>% 
  log_("i arrived to first_group") %>%
  set_attribute(keys = c("washer","hair_cuter","nail_cuter"), values= c(0,0,0))%>% #this is used in the function
  simmer::select(resources = function() Cosmeticresource(ZooHospital),
                 policy = "first-available",id = 0) %>% #choose the treatment
  log_("I chose first resource")%>%
  seize_selected(amount = 1, id = 0) %>%
  timeout(cosmetic_first_group_treatments_timeout(function() get_selected(ZooHospital)))%>% #use function to find the correct time for timeout
  release_selected(amount = 1, id = 0) %>%
  set_attribute(keys = function()(get_selected(ZooHospital)), value = 1) %>% # that way this treatment can't be chosen again
  log_("I had treatment")%>%
  simmer::rollback(target = 7, times = 2) %>%
  set_attribute("Total_To_Pay", value = 600, mod= "+" ) %>%
  join(visitsum) #After this you go to visit sum traj to pay and finish


#second cosmetic treatment- teeth treatment
second_group_treatment <-
  trajectory("second_group path") %>%
  log_("i arrived to second_group") %>% 
  set_attribute("Total_To_Pay", value = 700, mod= "+" ) %>%
  seize("teeth_cleaner",amount = 1) %>%
  timeout(function() exp(15)) %>%
  release("teeth_cleaner",amount = 1)  %>%
  seize("regular_vet_other",1)%>% #After this treatment you see a regular doctor, so seize from here
  timeout_from_attribute("vet_check_time")%>%
  release("regular_vet_other",1)%>%
  join(visitsum) #After finishing the treatment and seeing doctor, go to visit sum traj to pay and finish


#Nurse gives the animal medicine
  nursesgivemeds <-
  trajectory("nurses_give_meds path") %>%
  log_("i arrived to nurses_give_meds")%>%
  set_attribute("Total_To_Pay", value = 200, mod= "+" ) %>%
  seize("nurse_meds",1)%>%
  timeout_from_attribute("medication_time")%>% #different attribute for each type of animal
  release("nurse_meds",1)%>%
  join(visitsum) #After finishing the treatment and seeing doctor, go to visit sum traj to pay and finish

#nurse gives vaccine to animal
nursesgivevaccine <-
  trajectory("nurses_give_vaccine path") %>%
  log_("i arrived to nurses_give_vaccine")%>%
  set_attribute("Total_To_Pay", value = 30, mod= "+" ) %>%
  seize("nurse_vaccine",1)%>%
  timeout_from_attribute("vaccine_time")%>%
  release("nurse_vaccine",1)%>%  
  join(visitsum) #After finishing the treatment and seeing doctor, go to visit sum traj to pay and finish


#Animals that there surgery doesn't succeed get here, their payment becomes 0 and they leave
compensation <-
  trajectory("compensation path") %>%
  log_("delete price before leaving")%>%
  set_attribute("Total_To_Pay", value = 0)%>%
  leave(1)


#Wild animals that come to rest come here for there timeout and back to the general ishpuze traj
ishpuzetimewild <-
  trajectory("ishpuzewild path") %>%
  log_("i arrived to ishpuzetimewild")%>%
  seize("regular_vet_wild",1)%>%
  timeout_from_attribute("vet_check_time")%>%
  release("regular_vet_wild",1)

#Pets and horses that come to rest come here for their timeout and back to the general ishpuze traj
ishpuzetimeother <-
  trajectory("ishpuzeother path") %>%
  log_("i arrived to ishpuzetimeother")%>%
  seize("regular_vet_other",1)%>%
  timeout_from_attribute("vet_check_time")%>%
  release("regular_vet_other",1)

#send each type of animal to rest and timeout, after that they go to visitsum traj to pay and finish
ishpuzetimegeneral <-
  trajectory("ishpuzegeneral path") %>%
  log_("i arrived to ishpuzetimegeneral")%>%
  branch(option=function() what_vet(ZooHospital),continue= c(TRUE, TRUE),ishpuzetimewild,ishpuzetimeother)%>%
  #all animals go to there specific ishpuze traj and come back here after their timeout
  join(visitsum)

#whoever is sent to another hospital gets here and finishes their journey
otherhospital <-
  trajectory("otherhospital") %>%
  log_("i am in the otherhospital")%>%
  leave(1)


#Animals who need operation get here
operationroom <-
  trajectory("operationroom <-") %>%
  log_("i am in the operationroom") %>%
  
  set_attribute(key = "Start_time", function() now()) %>% #time of arrival
  seize("emergency_operation_room") %>%
  set_attribute(key = "End time", function() now()) %>% #time when surgery starts
  timeout(function() runif(1, 30, 50)) %>%
  release("emergency_operation_room") %>%
  
  set_attribute(key = "Total_To_Pay",
                values = function() { 2000 + get_attribute(ZooHospital, "Total_To_Pay") }) %>%
  
  branch(
    option = function() {
      prob <- surgery_fail_rate(ZooHospital)
      rdiscrete(1, c(1 - prob,prob), c(0,1))
    },
    continue = c(FALSE),
    otherhospital
  ) #animals that their surgery doesn't succeed leave the hospital, others return to emergencyvet traj that they came from


#animals that come late are locked out and leave
lockedout <-
  trajectory("lockedout path") %>%
  log_("i'm locked out")%>%
  leave(1)

#Lets us know if door is open or not
opendoor <-
  trajectory("opendoor path")%>%
  log_("the door is open so I'm in")

#Lets us know if bed was take or not
bedismine <-
  trajectory("bedismine path")%>%
  log_("I took the bed")

#animals are sent here in different paths
emergencyvet <-
  trajectory("emergencyvet path") %>%
  log_("i arrived to emergency ")%>%
  seize("door",1, continue = c(TRUE,FALSE), post.seize = opendoor, reject = lockedout)%>%   
  #Animals can only enter if it's early, if too late door is not abailable and they go to lockedout traj and leave
  release("door",1)%>%
  log_("I took and returned the door")%>%
  set_attribute("Total_To_Pay", value = 150, mod= "+" ) %>%
  seize("emergency_vet",1)%>% #Animals are first checked by vet
  timeout_from_attribute("vet_check_time")%>%
  release("emergency_vet",1)%>%
  branch(option=function() rdiscrete (1, c(0.87,0.13),c(0,1)),continue= c(TRUE),operationroom)%>%
  #Some of the animals are sent to surgery, other try to catch a bed
  log_("i arrived to getting a bed ")%>%
  seize("emergency_bed", 1, continue = c(TRUE,FALSE), post.seize=bedismine, reject = visitsum)%>%
  #If bed is caught continue here, if not go to visitsum traj, pay and leave
  timeout(function() rnorm(1, mean = 240, sd = 30)) %>%
  release("emergency_bed", 1) %>%
  log_("I returned my bed")%>%
  set_attribute("Total_To_Pay", value = 1500, mod= "+" ) %>%
  join(ishpuzetimegeneral) #Once done with emergency treatment, animal goes to ishpuze

#Some pets start their journey from regular treatment
regular_treatment_pets <-
  trajectory("regular_treatment pets") %>%
  log_("i arrived to regular_treatment_pets ")%>%
  renege_in(30)%>%
  #If pet is in line for over 30 minutes it leaves
  log_("i am leaving")%>%
  seize("regular_vet_other",1)%>%
  renege_abort()%>%                      
  timeout_from_attribute("vet_check_time")%>%   #אותו דבר אולי לעשות רגיל ואז בפנים לשנות
  release("regular_vet_other",1)%>%
  set_attribute("Total_To_Pay", value = 150, mod= "+" ) %>%
  branch(option=function() rdiscrete (1, c(0.27,0.13,0.25,0.35),c(1,2,3,4)),continue= c(FALSE, FALSE,FALSE,FALSE),nursesgivevaccine ,emergencyvet,nursesgivemeds,visitsum)
  #Animal continues to its next traj according to distribution

#Some horses start their journey from regular treatment
regular_treatment_horses <-
  trajectory("regular_treatment horses") %>%
  log_("i arrived to regular_treatment_horses ")%>%
  seize("regular_vet_other",1)%>%
  #The horses seize the vet and timeout according to their function
  timeout_from_attribute("vet_check_time")%>%
  release("regular_vet_other",1)%>%
  set_attribute("Total_To_Pay", value = 150, mod= "+" ) %>%
  branch(option=function() rdiscrete (1, c(0.27,0.13,0.25,0.35),c(1,2,3,4)),continue= c(FALSE, FALSE,FALSE,FALSE),nursesgivevaccine ,emergencyvet,nursesgivemeds,visitsum)
  #Animal continues to its next traj according to distribution

#Some wild animals start their journey from regular treatment
regular_treatment_wild <-
  trajectory("regular_treatment wild") %>%
  log_("i arrived to regular_treatment_wild ")%>%
  #The wild animals seize the vet and timeout according to their function
  seize("regular_vet_wild",1)%>%
  timeout_from_attribute("vet_check_time")%>%
  release("regular_vet_wild",1)%>%
  set_attribute("Total_To_Pay", value = 150, mod= "+" ) %>%
  branch(option=function() rdiscrete (1, c(0.8,0.2),c(1,2)),continue= c(FALSE, FALSE),nursesgivevaccine,visitsum)
  #Animal continues to its next traj according to distribution


#special reception designed to connect horses
reception_two_horses<-
  trajectory("two horses reception path") %>%
  log_("we, two horses, arrived to reception")%>%
  batch(n=2, timeout=Inf, permanent=FALSE, name="twoHorses")%>%
  #horses batch together to go through counter together
  log_("we are now two horses together")%>%
  seize("counter",1)%>%
  timeout(function () countertime())%>%
  release("counter",1)%>%
  separate()%>%
  #horses continue seperatly according to distribution
  branch(option = function() get_attribute(ZooHospital,"first_treatment_num"),
         continue=c(FALSE,FALSE,FALSE,FALSE,FALSE),emergencyvet,first_group_treatment,second_group_treatment,
         regular_treatment_pets,regular_treatment_horses)


#Entering the hospital-no 2 horses
reception_not_two_horses <-
  trajectory("reception path") %>%
  log_("i arrived to reception")%>%
  seize("counter",1)%>%
  timeout(function () countertime())%>%
  release("counter",1)%>%
  branch(option = function() get_attribute(ZooHospital,"first_treatment_num"),
         continue=c(FALSE,FALSE,FALSE,FALSE,FALSE),emergencyvet,first_group_treatment,second_group_treatment,
         regular_treatment_pets,regular_treatment_horses)
  #All other animals (including horses for emergency) go to the next treatment according to ditribution


#Trajectory for each type of animal to set all relevant attributes
#pets trajectory, pets is 1
pet_traj <-
  trajectory("Pet path") %>%
  log_("i arrived to pet traj")%>%
  set_attribute(c("type","vaccine_time","medication_time","vet_check_time","summary_time","Total_To_Pay"),
                value=c(1, rnorm(1,mean = 15,sd=4), runif(1, min =7, max = 13),
                        10, rnorm(1,4,0.5),0)) %>%
  set_attribute("first_treatment_num",value=function() first_treatment_pets())%>%
  join(reception_not_two_horses)


#horses trajectory, horses is 2
horse_traj <-
  trajectory("horse path") %>%
  log_("i arrived to horse traj")%>%
  set_attribute(c("type","vaccine_time","medication_time","vet_check_time","summary_time","Total_To_Pay"),
                value=c(2, rnorm(1,mean = 12,sd=3), runif(1, min =13, max = 16),
                        14, rnorm(1,4,0.5),0)) %>%
  set_attribute("first_treatment_num",value=function() first_treatment_horses())%>%
  branch(option = function() get_attribute(ZooHospital,"first_treatment_num"),
         continue=c(FALSE,FALSE,FALSE,FALSE,FALSE),reception_not_two_horses,reception_two_horses,reception_two_horses,
         reception_two_horses,reception_two_horses)
  #If the horse is not supposed to go to emergency, go to 2 horses reception to batch


#wild_animal_trajectory, wild is 3
wild_animal_traj <-
  trajectory("wild animal path") %>%
  log_("i arrived to wild animal traj")%>%
  set_attribute(keys=c("type","medication_time","vet_check_time","summary_time","Total_To_Pay"),
                value=c(3, rexp(1, 10),
                        9, rnorm(1,20,4),0)) %>%
  set_attribute("first_treatment_num",value=function() first_treatment_wild())%>%
  join(reception_not_two_horses)



#doorman comes to this traj to close the emergency services when it's late
closeDoor <- trajectory("door")%>%
  log_( function() paste("the door is closed"))%>%
  set_capacity("door",0)%>%
  set_queue_size("door",0)


##----------------------------------------- 5.  All Generators, ALWAYS LAST. ------------------------------------------------

ZooHospital%>%
  
  add_generator(name="doorman", trajectory=closeDoor,at(720),mon=2)%>% #Doorman will lock the door and stop emergencyvet at 19:00
  add_generator(name="pets",trajectory=pet_traj,distribution=function() rexp(1, 0.25),priority=1,preemptible=2,restart = FALSE, mon=2)%>%
  add_generator("horse", trajectory=horse_traj, from_to(300,960,  function ()runif(1,20,40), arrive = TRUE),priority=1,preemptible=2,restart = FALSE,mon=2)%>%
  add_generator(name="wildanimals",trajectory=wild_animal_traj,distribution=function() rexp(1,0.2636298),priority=2,preemptible=3,restart = FALSE,mon=2)


##----------------------------------------- 6.  reset, run, plots, outputs ------------------------------------------------

set.seed(456)
reset(ZooHospital)%>%run(until=simulationTime)
data_per_animal<-get_mon_arrivals(ZooHospital)
data_per_resource <- get_mon_arrivals(ZooHospital,per_resource = TRUE)
data_for_resource_per_resource<-get_mon_resources(ZooHospital)
data_per_attribute<-get_mon_attributes(ZooHospital)


#Measures

# Average payment for an animal
paste("avg payment in hospital:"
      ,sqldf("
SELECT 
  AVG(value)
FROM data_per_attribute
WHERE key = 'Total_To_Pay'

"))



# Average time at the hospital
paste("avg time at hospital: ", sqldf("
SELECT
  AVG(activity_time) AS avg_activity_time
FROM data_per_animal
"))



#Average time in emergency queue
time<-as.matrix(sqldf("SELECT time FROM data_for_resource_per_resource WHERE resource='emergency_vet'"))
queueLength <- as.matrix(sqldf("SELECT queue FROM data_for_resource_per_resource WHERE resource='emergency_vet'"))
avgResQueue <- avgQueue(time, queueLength, simulationTime)
paste("avg time at emergency vet queue: ", avgResQueue)


#Plots to show the chain of events in 3 different trajectories
plot(first_group_treatment)
plot(visitsum)
plot(operationroom)


##--------------------libraries----------------------------##
# install.packages("rlang", repos = "http://cran.us.r-project.org")
# install.packages("MASS", repos = "http://cran.us.r-project.org")
# install.packages("fitdistrplus", repos = "http://cran.us.r-project.org")
# install.packages("magrittr", repos = "http://cran.us.r-project.org")
# install.packages("simmer", repos = "http://cran.us.r-project.org")
# install.packages("simmer.plot", repos = "http://cran.us.r-project.org")
# install.packages("dplyr", repos = "http://cran.us.r-project.org")
# install.packages("lazyeval", repos = "http://cran.us.r-project.org")
# install.packages("parallel", repos = "http://cran.us.r-project.org")
# install.packages("e1071", repos = "http://cran.us.r-project.org")
# install.packages("plotly", repos = "http://cran.us.r-project.org")
# install.packages("ggplot2", repos = "http://cran.us.r-project.org")
# install.packages("triangle", repos = "http://cran.us.r-project.org")
# install.packages("sqldf", repos = "http://cran.us.r-project.org")
# install.packages("knitr", repos = "http://cran.us.r-project.org")
# install.packages("rmarkdown", repos = "http://cran.us.r-project.org")
# install.packages("readxl", repos = "http://cran.us.r-project.org")
# install.packages("reprex")
# install.packages("lubridate")
# install.packages("RSQLite")
# 
# 
# library(rlang)
# library(MASS)
# library(fitdistrplus)
# library(magrittr)
# library(dplyr)
# library(lazyeval)
# library(parallel)
# library(e1071)
# library(plotly)
# library(ggplot2)
# library(triangle)
# library(sqldf)
# library(readxl)
# library(knitr)
# library(rmarkdown)
# library(simmer)
# library(simmer.plot)
# library(reprex)
# library(lubridate)
# library(sqldf)
