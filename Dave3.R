######Script for generating Auto-FB for Reports


#Process

library(tidyverse)
library(lubridate)
library(magrittr)


##Input Data

a <- read.csv("C:/Users/ACER/Desktop/WHO Consultancy/Extension Work/R Work/Process Automation/Actuals_Data_AOOct112021/Actuals_Data.csv", sep=";")


##Organize Data Set (Filter,Clean, etc...)

unique(a$periodvalue)
unique(a$indicatorname)

a1 <- a %>% filter(countryname!="Philippines") %>% transform(level=NA, periodtype=NA, period=NA)
a1[a1$provincename=="", "level"] <- "region"
a1[is.na(a1$level) & a1$districtname=="", "level"] <- "province"
a1[is.na(a1$level) & a1$facilityname=="", "level"] <- "municipality"
a1[is.na(a1$level), "level"] <- "facility"


##Calculate the value (current BC-previous BC = difference/previous BC = percent change)
###Region
a1 %>% filter(indicatorname=="Case notification rate, bacteriologically positive new+relapse", periodvalue %in% c("APR-JUN-2021", "JUL-SEP-2021"), level=="region") %>% select(numerator, periodvalue, countryname, indicatorname, provincename, districtname, level)%>% pivot_wider(names_from = periodvalue, values_from = numerator) %>% rename(Q2=`APR-JUN-2021`, Q3=`JUL-SEP-2021`) %>% mutate(pct.diff=(Q3-Q2)/Q3 *100) %>% view()

###Province
a1 %>% filter(indicatorname=="Case notification rate, bacteriologically positive new+relapse", periodvalue %in% c("APR-JUN-2021", "JUL-SEP-2021"), level=="province") %>% select(numerator, periodvalue, countryname, indicatorname, provincename, districtname, level)%>% pivot_wider(names_from = periodvalue, values_from = numerator) %>% rename(Q2=`APR-JUN-2021`, Q3=`JUL-SEP-2021`) %>% mutate(pct.diff=(Q3-Q2)/Q3 *100) %>% view()

###Municipality
a1 %>% filter(indicatorname=="Case notification rate, bacteriologically positive new+relapse", periodvalue %in% c("APR-JUN-2021", "JUL-SEP-2021"), level=="municipality") %>% select(numerator, periodvalue, countryname, indicatorname, provincename, districtname, level)%>% pivot_wider(names_from = periodvalue, values_from = numerator) %>% rename(Q2=`APR-JUN-2021`, Q3=`JUL-SEP-2021`) %>% mutate(pct.diff=(Q3-Q2)/Q3 *100) %>% view()

###Facility
a1 %>% filter(indicatorname=="Case notification rate, bacteriologically positive new+relapse", periodvalue %in% c("APR-JUN-2021", "JUL-SEP-2021"), level=="facility") %>% select(numerator, periodvalue, countryname, indicatorname, provincename, districtname, facilityname, level)%>% pivot_wider(names_from = periodvalue, values_from = numerator) %>% rename(Q2=`APR-JUN-2021`, Q3=`JUL-SEP-2021`) %>% mutate(pct.diff=(Q3-Q2)/Q3 *100) %>% view()

###All Levels
a1 %>% filter(indicatorname=="Case notification rate, bacteriologically positive new+relapse", periodvalue %in% c("APR-JUN-2021", "JUL-SEP-2021"), level %in% c("region", "province", "municipality", "facility")) %>% select(numerator, periodvalue, countryname, indicatorname, provincename, districtname, facilityname, level)%>% pivot_wider(names_from = periodvalue, values_from = numerator) %>% rename(Q2=`APR-JUN-2021`, Q3=`JUL-SEP-2021`) %>% mutate(pct.diff=(Q3-Q2)/Q3 *100) %>% view()


##BC Comment
###Region
a1 %>% filter(indicatorname=="Case notification rate, bacteriologically positive new+relapse", periodvalue %in% c("APR-JUN-2021", "JUL-SEP-2021"), level=="region") %>% select(numerator, periodvalue, countryname, indicatorname, provincename, districtname, level)%>% pivot_wider(names_from = periodvalue, values_from = numerator) %>% rename(Q2=`APR-JUN-2021`, Q3=`JUL-SEP-2021`) %>% mutate(pct.diff=(Q3-Q2)/Q3 *100, BC_Comment=case_when(
  pct.diff < -50 ~ "Your reported BC enrollment has dramatically reduced since last quarter.",
  pct.diff < 0 ~ "Your reported BC enrollment has reducedd since last quarter",
  pct.diff < 50 ~ "Your reported BC enrollment has increased increased since last quarter",
  pct.diff > 51 ~ "Your reported BC enrollment has dramatically increased since last quarter",
  TRUE ~ "Error or incomplete data.")) %>% view()

###Province
a1 %>% filter(indicatorname=="Case notification rate, bacteriologically positive new+relapse", periodvalue %in% c("APR-JUN-2021", "JUL-SEP-2021"), level=="province") %>% select(numerator, periodvalue, countryname, indicatorname, provincename, districtname, level)%>% pivot_wider(names_from = periodvalue, values_from = numerator) %>% rename(Q2=`APR-JUN-2021`, Q3=`JUL-SEP-2021`) %>% mutate(pct.diff=(Q3-Q2)/Q3 *100, BC_Comment=case_when(
  pct.diff < -50 ~ "Your reported BC enrollment has dramatically reduced since last quarter.",
  pct.diff < 0 ~ "Your reported BC enrollment has reducedd since last quarter",
  pct.diff < 50 ~ "Your reported BC enrollment has increased increased since last quarter",
  pct.diff > 51 ~ "Your reported BC enrollment has dramatically increased since last quarter",
  TRUE ~ "Error or incomplete data.")) %>% view()

###Municipality
a1 %>% filter(indicatorname=="Case notification rate, bacteriologically positive new+relapse", periodvalue %in% c("APR-JUN-2021", "JUL-SEP-2021"), level=="municipality") %>% select(numerator, periodvalue, countryname, indicatorname, provincename, districtname, level)%>% pivot_wider(names_from = periodvalue, values_from = numerator) %>% rename(Q2=`APR-JUN-2021`, Q3=`JUL-SEP-2021`) %>% mutate(pct.diff=(Q3-Q2)/Q3 *100, BC_Comment=case_when(
  pct.diff < -50 ~ "Your reported BC enrollment has dramatically reduced since last quarter.",
  pct.diff < 0 ~ "Your reported BC enrollment has reducedd since last quarter",
  pct.diff < 50 ~ "Your reported BC enrollment has increased increased since last quarter",
  pct.diff > 51 ~ "Your reported BC enrollment has dramatically increased since last quarter",
  TRUE ~ "Error or incomplete data.")) %>% view()

###Facility
a1 %>% filter(indicatorname=="Case notification rate, bacteriologically positive new+relapse", periodvalue %in% c("APR-JUN-2021", "JUL-SEP-2021"), level=="facility") %>% select(numerator, periodvalue, countryname, indicatorname, provincename, districtname, facilityname, level)%>% pivot_wider(names_from = periodvalue, values_from = numerator) %>% rename(Q2=`APR-JUN-2021`, Q3=`JUL-SEP-2021`) %>% mutate(pct.diff=(Q3-Q2)/Q3 *100, BC_Comment=case_when(
  pct.diff < -50 ~ "Your reported BC enrollment has dramatically reduced since last quarter.",
  pct.diff < 0 ~ "Your reported BC enrollment has reducedd since last quarter",
  pct.diff < 50 ~ "Your reported BC enrollment has increased increased since last quarter",
  pct.diff > 51 ~ "Your reported BC enrollment has dramatically increased since last quarter",
  TRUE ~ "Error or incomplete data.")) %>% view()

###All Levels
a1 %>% filter(indicatorname=="Case notification rate, bacteriologically positive new+relapse", periodvalue %in% c("APR-JUN-2021", "JUL-SEP-2021"), level %in% c("region", "province", "municipality", "facility")) %>% select(numerator, periodvalue, countryname, indicatorname, provincename, districtname, facilityname, level)%>% pivot_wider(names_from = periodvalue, values_from = numerator) %>% rename(Q2=`APR-JUN-2021`, Q3=`JUL-SEP-2021`) %>% mutate(pct.diff=(Q3-Q2)/Q3 *100, BC_Comment=case_when(
  pct.diff < -50 ~ "Your reported BC enrollment has dramatically reduced since last quarter.",
  pct.diff < 0 ~ "Your reported BC enrollment has reducedd since last quarter",
  pct.diff < 50 ~ "Your reported BC enrollment has increased increased since last quarter",
  pct.diff > 51 ~ "Your reported BC enrollment has dramatically increased since last quarter",
  TRUE ~ "Error or incomplete data.")) %>% view()

##DRTB
###Computation with BC Comment
####All Levels
a1 %>% filter(indicatorname=="Treatment Success, DRTB (inc. RR-,MDR-, and XDR-TB)", periodvalue %in% c("APR-JUN-2020", "JUL-SEP-2020"), level %in% c("region", "province", "municipality", "facility")) %>% select(numerator, periodvalue, countryname, indicatorname, provincename, districtname, facilityname, level) %>% pivot_wider(names_from = periodvalue, values_from = numerator) %>% rename(Q2=`APR-JUN-2020`, Q3=`JUL-SEP-2020`) %>% mutate(pct.diff=(Q3-Q2)/Q3 *100, BC_Comment=case_when(
  pct.diff < -50 ~ "Your reported DRTB Treatment Outcome has dramatically reduced since last quarter.",
  pct.diff < 0 ~ "Your reported DRTB Treatment Outcome has reducedd since last quarter",
  pct.diff < 50 ~ "Your reported DRTB Treatment Outcomet has increased increased since last quarter",
  pct.diff > 51 ~ "Your reported DRTB Treatment Outcome has dramatically increased since last quarter",
  TRUE ~ "Error or incomplete data.")) %>% view()

##POP Testing
###Computation with BC Comment
####All Levels
a1 %>% filter(indicatorname=="Population testing rate", periodvalue %in% c("APR-JUN-2021", "JUL-SEP-2021"), level %in% c("region", "province", "municipality", "facility")) %>% select(numerator, periodvalue, countryname, indicatorname, provincename, districtname, facilityname, level) %>% pivot_wider(names_from = periodvalue, values_from = numerator) %>% rename(Q2=`APR-JUN-2021`, Q3=`JUL-SEP-2021`) %>% mutate(pct.diff=(Q3-Q2)/Q3 *100, BC_Comment=case_when(
  pct.diff < -50 ~ "Your reported Population Testing has dramatically reduced since last quarter.",
  pct.diff < 0 ~ "Your reported Population Testing has reducedd since last quarter",
  pct.diff < 50 ~ "Your reported Population Testing has increased increased since last quarter",
  pct.diff > 51 ~ "Your reported Population Testing has dramatically increased since last quarter",
  TRUE ~ "Error or incomplete data.")) %>% view()













####

###diff is (-50% or lower) comment: "Your reported BC enrollment has dramatically reduced by xx% since the previous period. We suggest you work on...."
###diff is (0% or -50%) comment: "Your reported BC enrollment has reduced by xx% since the previous period. We suggest you work on...."
###diff is (0% or 50%) comment: "Your reported BC enrollment has increased by xx% since the previous period. Congratulations, keep up the good work!...."
###diff is (50% or higher) comment: "Your reported BC enrollment has increased by xx% since the previous period. You're one of the best in the country!..."

####EXtract then put into a text file / own column 

####Save each text file to folder structure

####Zip file or nested zips (all)

###Row(textfiles - levels)



###comments masterlist (import) - Start with hardcodes
###for Loop 

#Output


##Folder Structure


###Email Message (REady to Send)

###Text Msg for RACE (Profile)


##Google Drive Link / ITIS Cloud / Zip for now



####Sample Loop
for (variable in 1:10) {
  print(paste("this is the number", variable))
  
}


####Vectorized