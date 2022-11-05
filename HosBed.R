#Install Packages
library(tidyverse)
library(magrittr)
library(readr)
library(ggplot2)
install.packages("gtsummary")
library(gtsummary)


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
hcris <- read.csv("hcris_data.csv", header= TRUE,)

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
  mutate(beds_acute = ifelse(prvdr_num == "140158" & hcris_year == 2020,
                              as.double (203),
                              beds_acute),
         beds_total = ifelse(prvdr_num == "140158" & hcris_year == 2020,
                             as.double (258),
                              beds_total))




#Filter Idaho
hcris <- hcris %>%
  mutate(beds_acute = if_else(prvdr_num == "131316" & hcris_year == 2019,
                             as.double(21),
                             hcris$beds_acute),
         beds_total = if_else(prvdr_num == "131316" & hcris_year == 2019,
                             as.double(89),
                             beds_total))


#Filter Oklahoma
hcris <- hcris %>%
  mutate(beds_acute = if_else(prvdr_num == "370215" & hcris_year == 2019,
                             as.double(99),
                             beds_acute),
         beds_total = if_else(prvdr_num == "370215" & hcris_year == 2019,
                             as.double(97),
                             beds_total))

#Filter Kansas
hcris <- hcris %>%
  mutate(beds_total = if_else(prvdr_num == "171359" & hcris_year == 2019,
                             as.double(50),
                             beds_total))


#Filter New York
hcris <- hcris %>%
  mutate(beds_acute = if_else(prvdr_num == "330201" & hcris_year == 2020,
                             as.double(161),
                             hcris$beds_acute),
         beds_total = if_else(prvdr_num == "330201" & hcris_year == 2020,
                             as.double(748),
                             hcris$beds_total))

#Get rid of duplicate providers

hcris <-hcris |> 
  rowwise() |> 
  mutate(report_in_hcris_year = Overlap(as.Date(c(paste0(hcris_year,"-01-01"),
                                                  paste0(hcris_year,"-12-31"))),
                                        c(FY_BGN_DT,FY_END_DT))) |> 
  ungroup()

data_hcris_unq <- data_hcris |> 
  group_by(prvdr_num,hcris_year) |> 
  dplyr::filter(report_in_hcris_year == max(report_in_hcris_year)) |> 
  ungroup() |> 
  distinct() |> 
  # still a few hospitals with 2 reports, will prioritize ones ending prior to hcris year end
  group_by(prvdr_num,hcris_year) |> 
  mutate(n_count = n()) |> 
  ungroup() |> 
  dplyr::filter(n_count == 1 | 
                  (FY_END_DT <= as.Date(paste0(hcris_year,"-12-31"))))

data_hcris_unq |> 
  group_by(hcris_year) |> 
  count(prvdr_num) |> 
  count(n)
# Storing counts in `nn`, as `n` already present in input
# i Use `name = "new_name"` to pick a new name.
# # A tibble: 4 x 3
# # Groups:   hcris_year [4]
# hcris_year     n    nn
# <dbl> <int> <int>
# 1       2018     1  5968
# 2       2019     1  5940
# 3       2020     1  5155
# 4       2021     1   766

# readr::write_csv(data_hcris,
#                  file = "I:/Shared drives/Programs/Data Science Intern/data/hcris_data.csv",
#                  na = "")










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
boxplot(hcris$beds_acute)$out

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
               "New Jersey", "New York", "Pennsylvania", "Washington D.C.")
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
  group_by(HospitalType, hcris_year) %>%
  summarise(mean_hos_size = mean(beds_acute, na.rm=TRUE))

# HospitalType              mean_hos_size
# <chr>                             <dbl>
#   1 Acute Care Hospitals              215. 
# 2 Critical Access Hospitals          22.7
  

#Average Bed Count by Hospital Size

hcris %>%
  rename(region= reigon) %>%
  group_by(size) %>%
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
    group_by(UrbanFlag,hcris_year) %>%
  summarise(mean_byregion = mean(beds_acute, na.rm=TRUE))


# UrbanFlag mean_byregion
# <int>         <dbl>
#   1         0          50.1
# 2         1         238. 



# Filter out Hospitals who didn't report 2021
hos_urban2<-hos_urban%>%
   mutate(FY_END_DT = lubridate::year(FY_END_DT))%>%
  mutate(prvdr_num= as.character(prvdr_num))%>%
  group_by(state_name,prvdr_num, FY_END_DT, reigon,UrbanFlag, HospitalType)%>%
  summarise(beds_acute = max(beds_acute))%>%
  ungroup()

acute_beds <- hos_urban2|> 
  tidyr::pivot_wider(names_from = FY_END_DT,
                     values_from = beds_acute,
                     names_prefix = "yrs_")



#Recalculate the Size Variable
acute_beds<-acute_beds %>%
  mutate(size = case_when(
    yrs_2019 >=500 & yrs_2019 <1500 ~ "large",
    yrs_2019 >=100 & yrs_2019 <500 ~ "medium",
    yrs_2019 >=0 & yrs_2019 <100  ~ "small",
    TRUE ~ "Extra Large"))


#remove duplicate prvdr numbers

beds_unique <- acute_beds[!duplicated(acute_beds$prvdr_num), ]
  
length(unique(hos_urban2$prvdr_num))



#Filter out all who didn't report 2021
acute_beds<-acute_beds%>%
  filter(!is.na(yrs_2021)== TRUE)


#Get total number of hospitals reigonally
hos_urban |> 
  group_by(reigon) |> 
  summarise(n = n_distinct(prvdr_num))


#Table 1 for hos_urban dataset
acute_beds%>%
  select(state_name,UrbanFlag,reigon, size, HospitalType)%>%
  tbl_summary(
  )
   
 ###################################
#Logistic Regression Change(Yes/No) and Urban, Size, Reigion
####################################

#Create change variable
acute_beds_byyr<- acute_beds_byyr%>%
  mutate(bed_change = yrs_2019 - yrs_2020)%>%
  mutate(bed_change= if_else(bed_change > 0 | bed_change < 0,
                             as.double(1), 
                             bed_change))%>%
  filter(!is.na(bed_change)== TRUE)

#install packages for regression
install.packages("aod")
library(aod)

#run regression

mylogit <- glm(bed_change ~ UrbanFlag, data = acute_beds_byyr, family = "binomial")
summary(mylogit)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.0348  -1.0348  -0.5401   1.3271   1.9985  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  -1.8512     0.1087  -17.04   <2e-16 ***
#   UrbanFlag     1.5060     0.1271   11.85   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2023.1  on 1669  degrees of freedom
# Residual deviance: 1859.9  on 1668  degrees of freedom
# AIC: 1863.9
# 
# Number of Fisher Scoring iterations: 4


# Get confidence Interval

confint.default(mylogit)

# 2.5 %    97.5 %
#   (Intercept) -2.064149 -1.638216
# UrbanFlag    1.256924  1.755125

#Get ORs and CIs

exp(cbind(OR = coef(mylogit), confint(mylogit)))

# OR     2.5 %    97.5 %
#   (Intercept) 0.1570513 0.1261976 0.1933025
# UrbanFlag   4.5087700 3.5276170 5.8080602


 #check how many urban vs. non-urban have bed changes 
table(acute_beds_byyr$UrbanFlag, acute_beds_byyr$bed_change)

##################################

#Run Ordinal Logistic Regression

################################

#Install and Run Packages
install.packages("foreign")
install.packages("MASS")
install.packages("Hmisc")
install.packages("reshape2")

library("foreign")
library("MASS")
library("Hmisc")
library("reshape2")


#change, which ones are more likely to increase/decrease

acute_beds<- acute_beds%>%
  mutate(bed_change2 = yrs_2019 - yrs_2020)

  mutate(bed_change2= case_when (
    bed_change2 > 0 ~ "increased", 
    bed_change2 == 0 ~ "stayed the same",
    bed_change2 < 0 ~ "decreased",
    TRUE ~ "other"
  )
  )



#Descriptive Statistics
lapply(acute_beds_byyr[, c("bed_change2", "HospitalType", "UrbanFlag", "reigon", "size")], table)

# $bed_change2
# 
# decreased       increased stayed the same 
# 268             223            1178 
# 
# $HospitalType
# 
# Acute Care Hospitals Critical Access Hospitals 
# 1177                       492 
# 
# $UrbanFlag
# 
# 0   1 
# 722 947 


#more descriptive stats
ftable(xtabs(~ UrbanFlag + HospitalType + bed_change+ reigon, data = acute_beds_byyr))

# reigon Midwest Northeast South West
# UrbanFlag HospitalType              bed_change                                    
# 0         Acute Care Hospitals      0                      57        26   156   20
# 1                      16         7    53    7
# Critical Access Hospitals 0                     184        29   108   44
# 1                       9         0     3    3
# 1         Acute Care Hospitals      0                      83        59   220   90
# 1                      75        54   193   62
# Critical Access Hospitals 0                      50         6    35   12
# 1                       6         0     1    2




# three way cross tabs (xtabs) and flatten the table
ftable(xtabs(~ UrbanFlag + HospitalType + bed_change2, data = acute_beds_byyr))
#                                   bed_change2 decreased increased other stayed the same
# UrbanFlag HospitalType                                                             
# 0         Acute Care Hospitals                         28        55             259
#           Critical Access Hospitals                     6         9             365
# 1         Acute Care Hospitals                        232       152             451
#           Critical Access Hospitals                     2         7             103



#Set data as a factor
acute_beds_byyr<-acute_beds_byyr%>%
  mutate(bed_change2= factor(bed_change2, levels= c("decreased", "stayed the same", "increased")))

levels(acute_beds_byyr$bed_change2)

## fit ordered logit model and store results 'm'
m <- polr(bed_change2 ~ UrbanFlag + HospitalType + reigon, data = acute_beds_byyr, Hess=TRUE)
summary(m)

# Coefficients:
#   Value Std. Error t value
# UrbanFlag                             -0.46652     0.1201 -3.8841
# HospitalTypeCritical Access Hospitals  0.05491     0.1305  0.4207
# reigonNortheast                       -0.07278     0.1935 -0.3760
# reigonSouth                            0.11598     0.1314  0.8827
# reigonWest                            -0.13764     0.1736 -0.7930
# 
# Intercepts:
#   Value    Std. Error t value 
# decreased|stayed the same  -1.8817   0.1504   -12.5121
# stayed the same|increased   1.6927   0.1481    11.4313
# 
# Residual Deviance: 2674.836 
# AIC: 2688.836 
# (41 observations deleted due to missingness)



## store table
(ctable <- coef(summary(m)))

# Value Std. Error     t value
# UrbanFlag                             -0.46651691  0.1201091  -3.8841085
# HospitalTypeCritical Access Hospitals  0.05491409  0.1305315   0.4206962
# reigonNortheast                       -0.07278109  0.1935475  -0.3760374
# reigonSouth                            0.11597918  0.1313883   0.8827208
# reigonWest                            -0.13763583  0.1735602  -0.7930147
# decreased|stayed the same             -1.88174298  0.1503938 -12.5121048
# stayed the same|increased              1.69265990  0.1480719  11.4313340

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

##Store CI
(ci <- confint(m))
confint.default(m)

# 2.5 %     97.5 %
#   UrbanFlag                             -0.7019265 -0.2311073
# HospitalTypeCritical Access Hospitals -0.2009229  0.3107511
# reigonNortheast                       -0.4521271  0.3065650
# reigonSouth                           -0.1415371  0.3734955
# reigonWest                            -0.4778077  0.2025360


## odds ratios
exp(coef(m))
# UrbanFlag HospitalTypeCritical Access Hospitals 
# 0.6271830                             1.0564499 
# reigonNortheast                           reigonSouth 
# 0.9298044                             1.1229725 
# reigonWest 
# 0.8714160 

## OR and CI
exp(cbind(OR = coef(m), ci))

#                                               OR     2.5 %    97.5 %
#   UrbanFlag                             0.6271830 0.4952778 0.7931974
# HospitalTypeCritical Access Hospitals 1.0564499 0.8180867 1.3647903
# reigonNortheast                       0.9298044 0.6365383 1.3592888
# reigonSouth                           1.1229725 0.8679370 1.4528670
# reigonWest                            0.8714160 0.6201486 1.2246308



#Test the Proportional Odds Assumption

# create binary variable for "Yellow" or "Red" versus "None"
acute_beds_byyr$decreased <- ifelse(acute_beds_byyr$bed_change2 == "decreased", 0, 1)

# create binary variable for "Red" versus "Yellow" or "None"
acute_beds_byyr$increased <- ifelse(acute_beds_byyr$bed_change2 == "increased", 1, 0)


# model for decreased
decreased_model <- glm(
  acute_beds_byyr$decreased ~ UrbanFlag + HospitalType + reigon+ size,
  data = acute_beds_byyr, 
  family = "binomial")
summary(decreased_model)


# model for increased
increased_model <- glm(
  acute_beds_byyr$increased ~ UrbanFlag + HospitalType + reigon+ size,
  data = acute_beds_byyr, 
  family = "binomial")


#check to see if coefficients are different

(coefficient_comparison <- data.frame(
  increased = summary(increased_model)$coefficients[ , "Estimate"],
  decreased = summary(decreased_model)$coefficients[ ,"Estimate"],
  diff = summary(decreased_model)$coefficients[ ,"Estimate"] - 
    summary(increased_model)$coefficients[ , "Estimate"]
))


#Large differences indicate that the Proportional Odds Assumption is violated
#               (stayed the same/increased)  (stayed same/increased)    diff
# (Intercept)                           -1.72746825  2.24737757  3.97484583
# UrbanFlag                              0.26735266 -1.35176775 -1.61912042
# HospitalTypeCritical Access Hospitals -1.71749503  2.30380349  4.02129852
# reigonNortheast                       -0.11061995  0.01721435  0.12783430
# reigonSouth                            0.07983788  0.14033124  0.06049336
# reigonWest                            -0.29811118  0.02009900  0.31821018

#####################
#Full Regression Details for Decreased 2019-2020#####
########################

#Create dataset for decreased

dec_beds <- hos_urban2|> 
  tidyr::pivot_wider(names_from = FY_END_DT,
                     values_from = beds_acute,
                     names_prefix = "yrs_")
  
 #filter dataset
  dec_beds<-dec_beds %>%
    filter(!is.na(yrs_2019)== TRUE)
    
    
    dec_beds<-dec_beds %>%
   filter(!is.na(yrs_2020)== TRUE)

#3848 beds to 3763 (2019) and 3716 (2020)
    
    dec_beds<-dec_beds %>%
  mutate(bed_change = yrs_2019 - yrs_2020)%>%
    mutate(dec_change= if_else(bed_change < 0,
                             as.double(1),
                             as.double(0)))
 
    dec_beds<-dec_beds %>%
      mutate(size = if_else(
        size== "Extra Large", "large",
        size))


 #Descriptive Statistics
lapply(dec_beds[, c("dec_change", "HospitalType", "UrbanFlag", "reigon", "size")], table)

# $dec_change
# 0    1 
# 3115  601 

# HospitalType
# 
# Acute Care Hospitals Critical Access Hospitals 
# 2707                      1009 
# 
# $UrbanFlag
# 
# 0    1 
# 1463 2253 
# 
# $reigon
# 
# Midwest Northeast     South      West 
# 1137       501      1389       689 
# 
# $size
# 
# large medium  small 
# 234   1594   1888 

#Run Logistic Regression
dec_beds%>%
dec_19t020 <- glm(dec_change ~ UrbanFlag + HospitalType + reigon + size, data = dec_beds, family = "binomial")
summary(dec_19t020)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.2378  -0.7069  -0.3891  -0.2012   2.8866  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           -0.33892    0.21806  -1.554  0.12013    
# UrbanFlag                              0.47983    0.15336   3.129  0.00176 ** 
#   HospitalTypeCritical Access Hospitals -1.34729    0.25149  -5.357 8.45e-08 ***
#   reigonNortheast                       -0.36443    0.15545  -2.344  0.01906 *  
#   reigonSouth                           -0.05242    0.12087  -0.434  0.66450    
# reigonWest                            -0.31288    0.14891  -2.101  0.03563 *  
#   sizemedium                            -1.08728    0.14469  -7.514 5.72e-14 ***
#   sizesmall                             -2.15150    0.19382 -11.101  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 3288.9  on 3715  degrees of freedom
# Residual deviance: 2798.3  on 3708  degrees of freedom
# AIC: 2814.3
# 
# Number of Fisher Scoring iterations: 6


confint.default(dec_19t020)
# 2.5 %      97.5 %
#   (Intercept)                           -0.7663156  0.08847357
# UrbanFlag                              0.1792480  0.78040871
# HospitalTypeCritical Access Hospitals -1.8402060 -0.85437448
# reigonNortheast                       -0.6691100 -0.05974960
# reigonSouth                           -0.2893308  0.18448318
# reigonWest                            -0.6047432 -0.02101665
# sizemedium                            -1.3708765 -0.80369185
# sizesmall                             -2.5313693 -1.77162635


exp(cbind(OR = coef(dec_19t020), confint(dec_19t020)))

#                                           OR      2.5 %    97.5 %
#   (Intercept)                           0.7125387 0.46375917 1.0907838
# UrbanFlag                             1.6157971 1.20013817 2.1906641
# HospitalTypeCritical Access Hospitals 0.2599437 0.15540209 0.4184428
# reigonNortheast                       0.6945926 0.51082054 0.9400093
# reigonSouth                           0.9489266 0.74933100 1.2038146
# reigonWest                            0.7313377 0.54509524 0.9776530
# sizemedium                            0.3371308 0.25376403 0.4477010
# sizesmall                            0.3371308 0.07921013 0.1694272

####################

#Decreased 2019-2020

###################

#Create dataset for decreased

dec_beds2 <- hos_urban2|> 
  tidyr::pivot_wider(names_from = FY_END_DT,
                     values_from = beds_acute,
                     names_prefix = "yrs_")

#filter dataset
dec_beds2<-dec_beds2 %>%
  filter(!is.na(yrs_2019)== TRUE)


dec_beds2<-dec_beds2 %>%
  filter(!is.na(yrs_2021)== TRUE)

#3848 beds to 3763 (2019) and 1678 (2021)

dec_beds2<-dec_beds2 %>%
  mutate(bed_change = yrs_2019 - yrs_2021)%>%
  mutate(dec_change2= if_else(bed_change < 0,
                             as.double(1),
                             as.double(0)))

dec_beds2<-dec_beds2 %>%
  mutate(size = if_else(
    size== "Extra Large", "large",
    size))


#Descriptive Statistics
lapply(dec_beds2[, c("dec_change2", "HospitalType", "UrbanFlag", "reigon", "size")], table)

# $dec_change2
# 
# 0    1 
# 1320  358 
# 
# $HospitalType
# 
# Acute Care Hospitals Critical Access Hospitals 
# 1186                       492 
# 
# $UrbanFlag
# 
# 0   1 
# 725 953 
# 
# $reigon
# 
# Midwest Northeast     South      West 
# 483       181       772       242 
# 
# $size
# 
# large medium  small 
# 103    660    915 

#Run Logistic Regression
  dec_19t021 <- glm(dec_change2 ~ UrbanFlag + HospitalType + reigon + size, data = dec_beds2, family = "binomial")
summary(dec_19t021)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.7316  -0.7224  -0.4099  -0.1997   2.8017  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                             0.2752     0.3214   0.856  0.39188    
# UrbanFlag                               0.5890     0.1949   3.021  0.00252 ** 
#   HospitalTypeCritical Access Hospitals  -1.4701     0.3244  -4.532 5.86e-06 ***
#   reigonNortheast                         0.1397     0.2452   0.570  0.56895    
# reigonSouth                             0.2196     0.1806   1.216  0.22417    
# reigonWest                              0.3823     0.2230   1.715  0.08642 .  
# sizemedium                             -1.7048     0.2407  -7.082 1.42e-12 ***
#   sizesmall                              -2.7099     0.2877  -9.418  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1739.6  on 1677  degrees of freedom
# Residual deviance: 1374.0  on 1670  degrees of freedom
# AIC: 1390
# 
# Number of Fisher Scoring iterations: 6


confint.default(dec_19t021)
# 2.5 %     97.5 %
#   (Intercept)                           -0.35473387  0.9050805
# UrbanFlag                              0.20689697  0.9710506
# HospitalTypeCritical Access Hospitals -2.10590743 -0.8342481
# reigonNortheast                       -0.34098397  0.6203725
# reigonSouth                           -0.13447210  0.5735872
# reigonWest                            -0.05470593  0.8193171
# sizemedium                            -2.17662300 -1.2330117
# sizesmall                             -3.27379551 -2.1459307


exp(cbind(OR = coef(dec_19t021), confint(dec_19t021)))

#                                             OR      2.5 %    97.5 %
#   (Intercept)                           1.31675887 0.70422804 2.4884787
# UrbanFlag                             1.80213809 1.23438862 2.6538610
# HospitalTypeCritical Access Hospitals 0.22990760 0.11708031 0.4217988
# reigonNortheast                       1.14992218 0.70838695 1.8555089
# reigonSouth                           1.24552553 0.87718917 1.7823145
# reigonWest                            1.46565988 0.94648084 2.2707312
# sizemedium                            0.18180559 0.11170627 0.2879395
# sizesmall                             0.06654591 0.03732206 0.1155383

########################

#Create Data set for Increased 2019-2020

#########################

#Create dataset for Increased 2019-2020

dec_beds<-dec_beds %>%
   mutate(inc_change= if_else(bed_change > 0,
                             as.double(1),
                             as.double(0)))


#Descriptive
lapply(dec_beds[, c("inc_change", "HospitalType", "UrbanFlag", "reigon", "size")], table)

# 0    1 
# 3231  485

#Run Logistic Regression
inc_19t020 <- glm(inc_change ~ UrbanFlag + HospitalType + reigon + size, data = dec_beds, family = "binomial")
summary(inc_19t020)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.8009  -0.6059  -0.5435  -0.2566   2.7708  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           -1.16090    0.22315  -5.202 1.97e-07 ***
#   UrbanFlag                              0.18836    0.14188   1.328  0.18432    
# HospitalTypeCritical Access Hospitals -1.59581    0.21158  -7.542 4.61e-14 ***
#   reigonNortheast                       -0.18434    0.16079  -1.146  0.25160    
# reigonSouth                           -0.06514    0.12362  -0.527  0.59824    
# reigonWest                            -0.41998    0.16105  -2.608  0.00911 ** 
#   sizemedium                            -0.44514    0.16580  -2.685  0.00726 ** 
#   sizesmall                             -0.64007    0.19613  -3.264  0.00110 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2878.9  on 3715  degrees of freedom
# Residual deviance: 2700.0  on 3708  degrees of freedom
# AIC: 2716
# 
# Number of Fisher Scoring iterations: 6


confint.default(inc_19t020)
# 2.5 %     97.5 %
#   (Intercept)                           -1.59827544 -0.7235305
# UrbanFlag                             -0.08972477  0.4664356
# HospitalTypeCritical Access Hospitals -2.01049799 -1.1811227
# reigonNortheast                       -0.49947902  0.1307985
# reigonSouth                           -0.30742368  0.1771476
# reigonWest                            -0.73562321 -0.1043294
# sizemedium                            -0.77010149 -0.1201737
# sizesmall                             -1.02447518 -0.2556634


exp(cbind(OR = coef(inc_19t020), confint(inc_19t020)))

#                                             OR     2.5 %    97.5 %
#   (Intercept)                           0.3132032 0.2013009 0.4830691
# UrbanFlag                             1.2072625 0.9154050 1.5969534
# HospitalTypeCritical Access Hospitals 0.2027442 0.1318824 0.3030662
# reigonNortheast                       0.8316528 0.6047627 1.1366627
# reigonSouth                           0.9369381 0.7360975 1.1954422
# reigonWest                            0.6570624 0.4772080 0.8979215
# sizemedium                            0.6407361 0.4653615 0.8922650
# sizesmall                             0.5272559 0.3596395 0.7765718

##############################
#Model for Increased 2019-2021
#############################


dec_beds2<-dec_beds2 %>%
  mutate(inc_change= if_else(bed_change > 0,
                             as.double(1),
                             as.double(0)))

#Descriptive 
lapply(dec_beds2[, c("inc_change", "HospitalType", "UrbanFlag", "reigon", "size")], table)

#Run Logistic Regression
inc_19t021 <- glm(inc_change ~ UrbanFlag + HospitalType + reigon + size, data = dec_beds2, family = "binomial")
summary(inc_19t021)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.7703  -0.7015  -0.6083  -0.3052   2.6437  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                           -1.66419    0.34039  -4.889 1.01e-06 ***
#   UrbanFlag                              0.14221    0.17864   0.796   0.4260    
# HospitalTypeCritical Access Hospitals -1.56130    0.25479  -6.128 8.91e-10 ***
#   reigonNortheast                       -0.07125    0.24103  -0.296   0.7675    
# reigonSouth                           -0.07149    0.17145  -0.417   0.6767    
# reigonWest                            -0.49206    0.24298  -2.025   0.0429 *  
#   sizemedium                             0.45905    0.28256   1.625   0.1043    
# sizesmall                              0.25377    0.31242   0.812   0.4166    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1487.2  on 1677  degrees of freedom
# Residual deviance: 1395.5  on 1670  degrees of freedom
# AIC: 1411.5
# 
# Number of Fisher Scoring iterations: 5

confint.default(inc_19t021)

#                                             2.5 %      97.5 %
#   (Intercept)                           -2.33135501 -0.99703291
# UrbanFlag                             -0.20792401  0.49234077
# HospitalTypeCritical Access Hospitals -2.06067787 -1.06191461
# reigonNortheast                       -0.54366761  0.40116030
# reigonSouth                           -0.40752800  0.26455121
# reigonWest                            -0.96829508 -0.01581949
# sizemedium                            -0.09476566  1.01286016
# sizesmall                             -0.35856869  0.86610835

exp(cbind(OR = coef(inc_19t021), confint(inc_19t021)))

#                                             OR      2.5 %    97.5 %
#   (Intercept)                           0.1893432 0.09502051 0.3625052
# UrbanFlag                             1.1528168 0.81276407 1.6382359
# HospitalTypeCritical Access Hospitals 0.2098639 0.12490179 0.3404915
# reigonNortheast                       0.9312257 0.57590362 1.4847727
# reigonSouth                           0.9310071 0.66721621 1.3077645
# reigonWest                            0.6113673 0.37577702 0.9766017
# sizemedium                            1.5825655 0.93113486 2.8368628
# sizesmall                             1.2888751 0.71083579 2.4337115


#Look at hospitals that were more likely to report data in 2021

##############################




confint.default(decreased_model)
#Get ORs and CIs
exp(cbind(OR = coef(decreased_model), confint(decreased_model)))

#                                           OR     2.5 %     97.5 %
#   (Intercept)                            9.4628875 6.1310159 15.0536190
# UrbanFlag                              0.2587824 0.1726725  0.3772949
# HospitalTypeCritical Access Hospitals 10.0121914 5.1054611 22.6711840
# reigonNortheast                        1.0173634 0.6266757  1.6669195
# reigonSouth                            1.1506549 0.7971666  1.6477484
# reigonWest                             1.0203023 0.6495514  1.6088978



####Full Regression Details for Increased 2019-2020

confint.default(increased_model)
#Get ORs and CIs
exp(cbind(OR = coef(increased_model), confint(increased_model)))

#                                           OR      2.5 %    97.5 %
#   (Intercept)                           0.1777338 0.11782886 0.2624758
# UrbanFlag                             1.3065011 0.94337670 1.8267037
# HospitalTypeCritical Access Hospitals 0.1795153 0.09965946 0.3035712
# reigonNortheast                       0.8952789 0.51993196 1.5099033
# reigonSouth                           1.0831115 0.75208648 1.5782384
# reigonWest                            0.7422188 0.43904165 1.2319437


########################
#regression model for 2019-2021
#########################

acute_beds_byyr<- acute_beds_byyr%>%
  mutate(bed_change3 = yrs_2019 - yrs_2021)%>%
  mutate(bed_change3= case_when (
    bed_change3 > 0 ~ "increased", 
    bed_change3 == 0 ~ "stayed the same",
    bed_change3 < 0 ~ "decreased",
    TRUE ~ "other"
  )
  )

# create binary variable for decreased
acute_beds_byyr$dec_2021 <- ifelse(acute_beds_byyr$bed_change3 == "decreased", 0, 1)

# create binary variable for increased
acute_beds_byyr$incr_2021<- ifelse(acute_beds_byyr$bed_change3 == "increased", 1, 0)

# model for decreased for 2019-2021
dec_model2 <- glm(
  acute_beds_byyr$dec_2021 ~ UrbanFlag + HospitalType + reigon,
  data = acute_beds_byyr, 
  family = "binomial")


# model for increased for 2019-2021
incr_model2 <- glm(
  acute_beds_byyr$increased ~ UrbanFlag + HospitalType + reigon,
  data = acute_beds_byyr, 
  family = "binomial")

#Full Regression Details for Decreased 2019-2020#####
confint.default(dec_model2)
#Get ORs and CIs
exp(cbind(OR = coef(dec_model2), confint(dec_model2)))


# (Intercept)                           7.5481333 5.1383494 11.3128594
# UrbanFlag                             0.2823937 0.2015775  0.3892696
# HospitalTypeCritical Access Hospitals 8.2574330 4.7698419 15.5632824
# reigonNortheast                       0.9356079 0.5946088  1.4815562
# reigonSouth                           0.8724458 0.6224862  1.2140044
# reigonWest                            0.7859889 0.5190912  1.1907168


####Full Regression Details for Increased 2019-2020
confint.default(incr_model2)
#Get ORs and CIs
exp(cbind(OR = coef(incr_model2), confint(incr_model2)))

#                                           OR      2.5 %    97.5 %
#   (Intercept)                           0.1777338 0.11782886 0.2624758
# UrbanFlag                             1.3065011 0.94337670 1.8267037
# HospitalTypeCritical Access Hospitals 0.1795153 0.09965946 0.3035712
# reigonNortheast                       0.8952789 0.51993196 1.5099033
# reigonSouth                           1.0831115 0.75208648 1.5782384
# reigonWest                            0.7422188 0.43904165 1.2319437



#factor link: https://swcarpentry.github.io/r-novice-inflammation/12-supp-factors/index.html


#change UrbanFlag as a factor
#run analysis from 2019 to 2021
#capital assets 
#broke into tertiles and broke into factors


#mutate(bed_change2 = factor(bed_chage2,levels = c("..."))

# #order(variable)
# levels(variable)
# factor, reorder

         



#ordinal regression- response is categorical variable with 3 levels
#could filter, by increased, decreased, and do separately
#can include multiple predictors
#gtsummary has a table regression

#https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html 
#tutorial for table regression

#tutorials on ordinal regressions in R
#Look AHA data dictionary 


#############
#Create COVID Graph
#################3



change_beds<- acute_beds%>%
  mutate(bed_change = yrs_2019 - yrs_2020)%>%
  mutate(per_change= (bed_change/yrs_2019)*100)%>%
  group_by(state_name)%>%
  summarize(sum_bedchange = sum(bed_change, na.rm = TRUE), 
            ave_per_change =mean(per_change, na.rm = TRUE))
  
  
change_beds<-change_beds%>%
  mutate(state_name = if_else(state_name == "Washington D.C.",
                             "District of Columbia",
                             state_name))
         
             
census<-census%>%
  rename(state_name = statename)

#DC is different use if_else statement to fix 
change_beds2<- census%>% left_join(change_beds, by= c("statename"= "state_name"))

change_beds2<-change_beds2%>%
  mutate(April20_pop= as.numeric(gsub(",","",April20_pop)))%>%
  mutate(pop_change= (sum_bedchange/April20_pop)*1000)


  
install.packages("maps")
install.packages("mapproj")
install.packahes("scales")
library(scales)
library("maps")
library(mapproj)
library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
theme_set(
  theme_void()
)


US_map <- map_data("state")
ggplot(US_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")


#Adjust names
change_beds2<-change_beds2%>%
  mutate(statename= tolower(statename))




# merge and sort (plots in order, sort ensures states filled in)
change_beds3<- US_map%>% left_join(change_beds2, by= c("region"= "statename"))

# plot bed change
ggplot(change_beds3, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = pop_change)) +
  scale_fill_gradient2(low = scales::muted("blue"), high =  scales::muted("red")  )+
  theme_void()+
  labs(fill = "Bed Change Per 1000 people")
  coord_map()

#plot percent change
  ggplot(change_beds3, aes(long, lat)) +
    geom_polygon(aes(group = group, fill = ave_per_change)) +
    scale_fill_gradient2(low = scales::muted("blue"), high =  scales::muted("red")  )+
    theme_void()+
    labs(fill = "Percent Change")
  coord_map()
  
  #save the images with ggsave EPS or TIF
  
  #You can also change from scientific notation in the legend after calling the scales package (library(scales)) and then adding labels = comma to scale_fill_viridis() (link). 


# merge and sort (plots in order, sort ensures states filled in)
arrests.geo <- merge(states, arrests, sort = FALSE, by = "region")
arrests.geo <- arrests.geo[order(arrests.geo$order), ]

# plot 
ggplot(arrests.geo, aes(long, lat)) +
  geom_polygon(aes(group = group, fill = assault)) +  
  coord_map()
scale_fill_viridis()

  