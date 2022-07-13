#Install Packages
library(tidyverse)
library(magrittr)
library(readr)
library(ggplot2)


#Read the Hospital COVID CSV file
covid19_nat <- read.csv("covid19_NatEst.csv", header= TRUE,)
covid19_nat<- covid19_nat %>%
  mutate(collectionDate = as.Date(collectionDate, format="%m/%d/%y"))
         
#Clean the Data to reflect regional tables to graph differences in available inpatient beds

new_england <- covid19_nat%>%
    dplyr::filter(statename == "New Hampshire") %>%
   dplyr::select(statename,collectionDate,InpatBeds_Occ_AnyPat_Est,InpatBeds_Occ_AnyPat_Est_Avail,InBedsOccAnyPat__Numbeds_Est) 
 
##########################################
#Create Graphs per State by Day
#########################################


#Create a function to Repeat across States for inpatient beds

graph_state <- function(state_name){
  data_out <- covid19_nat%>%
  dplyr::filter(statename == state_name)%>%
  dplyr::select(statename,collectionDate,InpatBeds_Occ_AnyPat_Est,InpatBeds_Occ_AnyPat_Est_Avail,InBedsOccAnyPat__Numbeds_Est)%>%
     ggplot()+
    geom_point(aes(x=collectionDate, y=InpatBeds_Occ_AnyPat_Est))+
  labs(title= state_name, x= "Collection Date" , y="Number of Occupied Inpatient Beds")
  
     print(data_out)
     #ggsave(filename =("/State_data/state_bedplot_",.png"))
     ggsave(filename = paste0(state_name,".png"))

     }

#Create Graphs for States in New England for number of inpatient beds
plot_ct <- graph_state("Connecticut")
plot_me <- graph_state("Maine")
plot_nh <- graph_state("New Hampshire")
plot_vt <- graph_state("Vermont")
plot_US <- graph_state("United States")

#Create For Loop to Generate a graph for each state
inp_states <- unique(covid19_nat$statename)
output <- vector("list",length(inp_states))
for (i in seq_along(inp_states)) {            
  output[[i]] <- graph_state(inp_states[[i]])      
}
output

#################################################3
#Create Graphs Weekly
##############################################

#Create new variable to estimate weekly available beds
library(lubridate)
covid19_nat%<>%
  mutate(week_no = week(collectionDate),
         .after= "collectionDate")


#Create new Dataset that groups by state and week
covid_week <- covid19_nat %>%
  dplyr::select(collectionDate, state, statename,InpatBeds_Occ_AnyPat_Est, week_no)%>%
  group_by(statename,week_no, collectionDate)%>%
mutate(collectionDate = as.Date(collectionDate, format="%y-%m-%d"))%>%
  summarize (meanvalue = mean(InpatBeds_Occ_AnyPat_Est))


#Function to graph by week
graph_state_wk <- function(state_name){
  data_out_wk <- covid19_week%>%
    dplyr::filter(statename == state_name)%>%
    ggplot()+
    geom_point(aes(x= week_no, y=meanvalue))+
    labs(title= state_name, x= "Collection Week" , y="Number of Occupied Inpatient Beds")+
    geom_smooth(aes(x= week_no, y=meanvalue), method = "lm", se=FALSE)
     ggsave(filename = paste0(state_name,"byweek.png"))
  print(data_out_wk)
}
graph_state_wk("Maine")
# Create Loop to Graph all states by week with linear regression line
inp_states <- unique(covid19_nat$statename)
output <- vector("list",length(inp_states))
for (i in seq_along(inp_states)) {            
  output[[i]] <- graph_state_wk(inp_states[[i]])      
}
output

#############################
#Calculate Linear Regression
##############################

#Function to calculate coefficents
calc_lm <- function(state_name){
data_out_state <- covid19_week%>%
  dplyr::filter(statename == state_name)
lm_state <-lm(meanvalue ~ week_no, data = data_out_state)
lm_coef<- summary(lm_state)$coefficients["week_no", "Estimate"]
return(lm_coef)

}
calc_lm("United States")


#Graph weekly estimates with linear regression

Me_result <-graph_state_wk("Maine")
graph_state_wk("United States")

Me_result$coefficients

###################
#Calculate Linear Regression for overall data to practice calculating lm
#################

covid19_week %>% 
  group_by(statename,week_no)%>% 
lm_me <- lm(meanvalue ~ week_no, data = covid19_week)
summary(lm_me)

#Calculate Coefficients for each individual state
inp_states_lm <- unique(covid19_week$statename)
output <- vector("list",length(inp_states_lm))
for (i in seq_along(inp_states_lm)) {            
  output[[i]] <- calc_lm(inp_states_lm[[i]])
  }
output

#Store in a dataframe

lmdf <- data.frame(states = inp_states_lm)
lmdf%<>%
  mutate(coefficients = output,
         .after= "states")

#Calculate which states have increased Occupied Beds
lmdf <- lmdf%>%
mutate(st_increased = if_else(coefficients >= 0, "yes", "no"))


#Export Dataframe
lmdf <- apply(lmdf,2,as.character)
write.csv(lmdf,"/Users/nataliethomas/Documents/MPH/APE/Git_Repo/Data\\Regression Coefficient Table.csv", row.names = FALSE)

###################################### 
#Graph Using Population Data
#####################################

#Download Census Data
census <- read.csv("pop_est.csv", header= TRUE,)

#Join datasets
pop_estimates <- covid_week%>% inner_join(census, by= "statename")

#Create new variable that accounts for population estimates
pop_estimates <- pop_estimates%>%
mutate(April20_pop= as.numeric(gsub(",","",April20_pop)))%>%
mutate(meanvalue = as.numeric(meanvalue)) %>%
mutate(hos_bed = (meanvalue /April20_pop)*1000)

#Graph data using the population estimates

#Function to graph by week
graph_state_pop <- function(state_name){
  data_out_pop <- pop_estimates%>%
    dplyr::filter(statename == state_name)%>%
    ggplot()+
    geom_point(aes(x= week_no, y=hos_bed))+
    labs(title= state_name, x= "Collection Week" , y="Number of Occupied Inpatient Beds per 1000 people")+
    geom_smooth(aes(x= week_no, y=hos_bed), method = "loess", se=FALSE)
  ggsave(filename = paste0(state_name,"bypop.png"))
  print(data_out_pop)
}

graph_state_pop("Maine")

#Graph per State
inp_states_pop <- unique(pop_estimates$statename)
output <- vector("list",length(inp_states_pop))
for (i in seq_along(inp_states_pop)) {            
  output[[i]] <- graph_state_pop(inp_states_pop[[i]])      
}
output


#####################################
#Compare Data to existing COVID cases
###################################

#download dataset

covid_cases <- read.csv("covid_cases.csv", header= TRUE,)
covid_cases <- covid_cases %>%
  mutate(submission_date = as.Date(submission_date, format="%m/%d/%y"))

covid_cases <-covid_cases%>%
arrange(state, submission_date)
covid_cases <- covid_cases[covid_cases$submission_date > as.Date("2020-04-01") & covid_cases$submission_date < as.Date("2020-07-14"),] %>%
mutate(week_no = week(submission_date),
       .after= "submission_date")


#Calculate 7 day Incidence Rate
incidence_rate<- pop_estimates%>% inner_join(covid_cases, by= c("state", "collectionDate" = "submission_date"))%>%
dplyr::select(statename,meanvalue,April20_pop,new_case, collectionDate,week_no.x)%>%
   mutate(new_case= as.numeric(gsub(",","",new_case)))%>%
  mutate(new_case_rate= (new_case/April20_pop)*100000)%>%
  group_by(statename,week_no.x)%>%
       summarize(new_case_bywk = mean(new_case))
 
#Rollsum function (right alignment) and  need to use the group by: sum of new cases over 7 days
install.packages("zoo")
library("zoo")
rolling_ir<- pop_estimates%>% inner_join(covid_cases, by= c("state", "collectionDate" = "submission_date"))%>%
dplyr::select(statename,state,week_no.x,meanvalue,April20_pop,new_case, collectionDate)%>%
  mutate(new_case= as.numeric(gsub(",","",new_case)))%>%
      mutate(new_case_rate= (new_case/April20_pop)*100000)%>%
  group_by(state)%>%
 mutate (sum7_ir = rollsum(x= new_case_rate, k = 7, align="right", na.pad=TRUE))

       
#Export Dataframe
write.csv(rolling_ir,"/Users/nataliethomas/Documents/MPH/APE/Git_Repo/Data\\rolling_ir", row.names = FALSE)




#Graph the 7 day incidence rate
graph_ir <- function(state_name){
  data_out_ir <- incidence_rate%>%
    dplyr::filter(statename == state_name)%>%
    ggplot()+
    geom_point(aes(x= week_no, y=new_case_sum))+
    labs(title= state_name, x= "Collection Week" , y="Incidence Rate per 10,000 people")+
    geom_smooth(aes(x= week_no, y=new_case_sum), method = "loess", se=FALSE)
  ggsave(filename = paste0(state_name,"bycaserate.png"))
  print(data_out_ir)
}

#Graph incidence rate per state
inp_states_ir <- unique(incidence_rate$statename)
output <- vector("list",length(inp_states_ir))
for (i in seq_along(inp_states_ir)) {            
  output[[i]] <- graph_ir(inp_states_ir[[i]])      
}
output


# For next time: rolling sum in the other table before join
#send email with next steps
# outputs beds and staterates

  