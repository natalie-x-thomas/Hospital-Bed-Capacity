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


#Download hcris data
hcris <- read.csv("hcris.csv", header= TRUE,)

#Summarize the acute bed counts based on the Fiscal Year END Date
hcris2<- hcris %>%
  mutate(FY_END_DT = lubridate::year(FY_END_DT))%>%
   mutate(prvdr_num= as.character(prvdr_num))%>%
  group_by(state_name,prvdr_num, FY_END_DT)%>%
  
  summarise(beds_acute = max(beds_acute))%>%
  ungroup()


#Clean Data--------------------------------
summary(hcris$beds_acute)

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
#      1.0     25.0     63.0    143.9    169.0 144837.0       11 

summary(hcris$beds_total)

# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 1.0     30.0     80.0    163.3    192.0 144892.0 


# find which hospital has this maximum 
max_hospital <- hcris %>%
  filter(beds_total == max(hcris$beds_total)) %>%
  select(prvdr_num)


hcris %>%
  filter(prvdr_num == max_hospital$prvdr_num)
# prvdr_num  FY_END_DT hcris_year beds_acute beds_total ssa_code state_name
# 1    140158 2019-06-30       2018        203        258       14   Illinois
# 2    140158 2020-06-30       2019        203        258       14   Illinois
# 3    140158 2021-05-31       2020     144837     144892       14   Illinois

#Filter Illinois
hcris <- hcris %>%
  mutate(beds_acute = if_else(prvdr_num == "140158" & hcris_year == 2020,
                              as.double(203),
                              beds_acute),
         beds_total = if_else(prvdr_num == "140158" & hcris_year == 2020,
                             as.double(258),
                              beds_total))




#Filter Idaho
hcris <- hcris %>%
  mutate(beds_acute = ifelse(prvdr_num == "131316" & hcris_year == 2019,
                             21,
                             hcris$beds_acute),
         beds_total = ifelse(prvdr_num == "131316" & hcris_year == 2019,
                             89,
                             beds_total))


#Filter Oklahoma
hcris <- hcris %>%
  mutate(beds_acute = ifelse(prvdr_num == "370215" & hcris_year == 2019,
                             99,
                             beds_acute),
         beds_total = ifelse(prvdr_num == "370215" & hcris_year == 2019,
                             97,
                             beds_total))

#Filter Kansas
hcris <- hcris %>%
  mutate(beds_total = ifelse(prvdr_num == "171359" & hcris_year == 2019,
                             50,
                             beds_total))


#Filter New York
hcris <- hcris %>%
  mutate(beds_acute = ifelse(prvdr_num == "330201" & hcris_year == 2020,
                             161,
                             hcris$beds_acute),
         beds_total = ifelse(prvdr_num == "330201" & hcris_year == 2020,
                             748,
                             hcris$beds_total))
#data_cleaned-------------------------------------------------------------------

# hcris2%>%
# ggplot()+
#   aes(beds_acute)+
#   geom_boxplot()
# 
# Q1 <- quantile(hcris2$beds_acute, .25, na.rm=TRUE)
# Q3 <- quantile(hcris2$beds_acute, .75, na.rm=TRUE)
# IQR <- IQR(hcris2$beds_acute, na.rm=TRUE)
# 
# hcris2_cleaned <- subset(hcris2, hcris2$beds_acute > (Q1 - 1.5*IQR) & hcris2$beds_acute < (Q3 + 1.5*IQR))
# 
# hcris2_cleaned%>%
#   ggplot()+
#   aes(beds_acute)+
#   geom_boxplot()
# 
# boxplot(hcris2$beds_acute)$out

hist(hcris$beds_acute)
summary(hcris$beds_acute)

#Generate sizing for Hospitals

hcris <- hcris %>%
  mutate(beds_acute= as.numeric(beds_acute))
    
    
hcris<-hcris %>%
  mutate(size = case_when(
  beds_acute >=500 & beds_acute <1500 ~ "large",
  beds_acute >=100 & beds_acute <500 ~ "medium",
  beds_acute >=0 & beds_acute <100  ~ "small",
  TRUE ~ "Extra Large"))

# install the package 
install.packages("ggstatsplot")

# Load the package
library(ggstatsplot)

outliers <- boxplot(hcris2$beds_acute , plot=FALSE)$out
x<-hcris
x<- x[-which(x$beds_acute %in% outliers),]

#case_when()
#hospital size- plot histogram for results for year to see distribution
# test with different attributes- send Kelsey RQ 
hist(hcris$beds_total)



#by reigon
Northeast <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island","Vermont",
               "New Jersey", "New York", "Pennsylvania")
Midwest <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", 
"Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")
South<- c("Delaware", "Florida","Georgia", "Maryland", "North Carolina", "South Carolina",
          "Virginia", "Washington, D.C.", "West Virginia","Alabama", "Kentucky",
          "Mississippi", "Tennessee", "Arkansas", "Louisiana", "Oklahoma", "Texas")
West <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah",
          "Wyoming","Alaska", "California", "Hawaii", "Oregon", "Washington")

hcris<- hcris%>%
  mutate(reigon = case_when(
    state_name %in% Northeast ~ "Northeast",
    state_name %in% Midwest ~ "Midwest",
    state_name %in% South ~ "South",
    state_name %in% West ~ "West",
    TRUE ~ "Other"
  )
  )

# Create a Region 
hcris_reigion <- hcris%>%
  group_by(reigon)%>%
  summarize(beds_acute, prvdr_num, hcris_year)



#region
#Income
#size

            
#Graph different state's hospital 
graph_beds <- function(in_state_name){
  output_beds <- hcris2%>%
  dplyr::filter(state_name == in_state_name)%>%
   ggplot(aes(x = FY_END_DT,
               y = beds_acute,
              group = prvdr_num,
               color= prvdr_num)) + 
    geom_line(size = 1) + 
        labs(x = "Year reported",
         y = "Acute bed count",
         title = paste0("Individual hospitals in ",in_state_name))
     ggsave(filename = paste0(in_state_name ,"bedcount.png"))
  return(output_beds)
}

datavt <- graph_beds("Vermont")
datavt

graph_beds("Maine")

#Graph bed count by state
inp_states_bc <- unique(hcris2$state_name)
output <- vector("list",length(inp_states_bc))
for (i in seq_along(inp_states_bc)) {            
  output[[i]] <- graph_beds(inp_states_bc[[i]])      
}
output


#Input Urban Data

hos_urban <- read.csv("hospital_urban.csv", header= TRUE,)

#readr::read_csv code will not get rid of zeros

#Join Hcris and Hospital Data Together
hos_urban <- hcris%>% inner_join(hos_urban, by= "prvdr_num")
  
hos_urban<-hos_urban%>%
mutate(prvdr_num = stringr::str_pad(as.character(prvdr_num),side="left",pad="0",width=6))


# Average bed count by Hospital Type

hos_urban%>%
  group_by(HospitalType) %>%
  summarise(mean_hos_size = mean(beds_acute, na.rm=TRUE))

# HospitalType              mean_hos_size
# <chr>                             <dbl>
#   1 Acute Care Hospitals              215. 
# 2 Critical Access Hospitals          22.7
  

#Average Bed Count by Hospital Size

hcris %>%
  rename(region= reigon) %>%
  group_by(region,size) %>%
  summarise(mean_byregion = mean(beds_acute, na.rm=TRUE))



# region    size        mean_byregion
# <chr>     <chr>               <dbl>
#   1 Midwest   large               714. 
# 2 Midwest   medium              215. 
# 3 Midwest   small                34.2
# 4 Northeast Extra Large        2305. 
# 5 Northeast large               719. 
# 6 Northeast medium              216. 
# 7 Northeast small                47.8
# 8 Other     large               712  
# 9 Other     medium              196. 
# 10 Other     small                55.6
# 11 South     Extra Large        1946. 
# 12 South     large               735. 
# 13 South     medium              218. 
# 14 South     small                42.2
# 15 West      large               635. 
# 16 West      medium              228. 
# 17 West      small                36.1



hcris <- hcris %>%
  filter(!is.na(beds_acute)==TRUE)


hos_urban%>%
  filter(is.na(beds_acute)==TRUE)
          



#Average Bed Count by Rural vs Urban

hos_urban %>%
    group_by(UrbanFlag) %>%
  summarise(mean_byregion = mean(beds_acute, na.rm=TRUE))


# UrbanFlag mean_byregion
# <int>         <dbl>
#   1         0          50.1
# 2         1         238. 


