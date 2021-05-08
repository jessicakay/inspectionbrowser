# quick reports for spot-checking and analysis
# jkant@bu.edu

library(dplyr)



df <- ds %>% 
  select(facility_type, facility, new_date, year, total_pop, total_violations) %>%
  filter(facility_type=="prison") %>%
  arrange(facility)

summary(lm(data=df, total_pop~total_violations, family="poisson"))

summary(lm(data=df, total_pop~total_violations+factor(facility)))

summary(glm(data=df, total_pop~total_violations+factor(facility),family = "poisson"))


summary(glm(data=df, total_pop~total_violations+factor(facility),family = "poisson"))


df<- ds %>% 
  filter(facility_type=="prison") %>% 
  select(facility, new_date, year, capacity, total_pop, total_violations, perc_overcap) %>% 
  arrange(facility)

x<-glm(data=df, total_pop~total_violations+
      factor(facility)+capacity, 
      family = "poisson")
summary(x)

# local variables

location <- "/Users/jessa/OneDrive/Documents/GitHub/misc/inspectionbrowser/datasets/"
filename <- "cleaned_04022020.csv"

# create tables for presentation

file_loc <- paste(location,filename,sep="")
dataset  <-read.csv(file_loc, stringsAsFactors = FALSE,check.names = F)
dataset  -> backup_ds 

dataset$repeat_string <- as.numeric(dataset$repeat_string)

statCol<-function(stat){
  paste(round(mean(stat,na.rm = T),2), " (", min(stat,na.rm = T),",",max(stat,na.rm = T),")")
}

  
dataset %>%
  filter(facility_type=="prison") %>%
  group_by(facility) %>%
  select(fc_total, repeat_string, facility, cmr_total, total_violations,year) %>%
  mutate(repeat_min=min(repeat_string,na.rm = T)) %>%
  mutate(repeat_max=max(repeat_string,na.rm = T)) %>%
  mutate(fc_tot_min=min(fc_total,na.rm = T)) %>%
  mutate(fc_tot_max=max(fc_total,na.rm = T)) %>% 
  mutate(cmr_tot_min=min(cmr_total,na.rm = T)) %>%
  mutate(cmr_tot_max=max(cmr_total,na.rm = T)) %>%
  mutate(yr_min=min(year,na.rm=T)) %>%
  mutate(yr_max=max(year,na.rm=T)) %>%
  mutate(yrs_available = paste(yr_min," - ",yr_max,sep="")) %>%
  select(-c(total_violations, year, repeat_string, cmr_total, fc_total)) %>% 
  select(-c(yr_min,yr_max)) -> table1

kbl(table1)


statCol2<-function(stat){paste("(", min(stat,na.rm = T),",",max(stat,na.rm = T),")")}

mn<-function(x){round(mean(x,na.rm=T),2)}

dataset %>%
  filter(facility_type=="prison") %>%
  group_by(facility) %>%
  select(fc_total, repeat_string, facility, cmr_total, total_violations,year) %>%
  mutate(yr_min=min(year,na.rm=T)) %>%
  mutate(yr_max=max(year,na.rm=T)) %>%
  mutate(yrs_available = paste(yr_min," - ",yr_max,sep="")) %>%
  select(-c(total_violations, year, repeat_string, cmr_total, fc_total)) %>% 
  select(-c(yr_min,yr_max)) %>%
  distinct() -> t1


dataset %>%
  filter(facility_type=="prison") %>%
  group_by(facility) %>%
  select(facility,year) %>%
  mutate(yr_min=min(year,na.rm=T)) %>%
  mutate(yr_max=max(year,na.rm=T)) %>%
  mutate(yrs_available = paste(yr_min," - ",yr_max,sep="")) %>%
  select(-c(yr_min,yr_max)) %>%
  summarise(n=n()) -> t1
  
         # note: the use of the filter above excludes prisons where type tag didn't apply see below: 
      
        dataset %>% select (facility, facility_type) %>% arrange(order_by=facility)

dataset %>%
  filter(facility_type=="prison") %>%
  group_by(facility) %>%
  select(fc_total, repeat_string, facility, cmr_total, total_violations,year) %>%
  mutate(rp=statCol2(repeat_string)) %>%
  mutate(repeat_max=mn(repeat_string)) %>%
  mutate(cm=statCol2(cmr_total)) %>%
  mutate(cmr_tot_max=mn(cmr_total)) %>%
  mutate(fc=statCol2(fc_total)) %>%
  mutate(fc_tot_max=mn(fc_total)) %>% 
  mutate(yr_min=min(year,na.rm=T)) %>%
  mutate(yr_max=max(year,na.rm=T)) %>%
  mutate(yrs_available = paste(yr_min," - ",yr_max,sep="")) %>%
  select(-c(total_violations, year, repeat_string, cmr_total, fc_total)) %>% 
  select(-c(yr_min,yr_max)) %>%
  distinct() -> t1

  knitr::kable(t1,align = rep('r',150))

  kbl(t1,align='r')
  




library(kableExtra)

kbl(table1)
