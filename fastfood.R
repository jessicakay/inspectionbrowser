# quick reports for spot-checking and analysis
# jkant@bu.edu

library(dplyr)
library(lubridate)
library(kableExtra)
library(knitr)

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

#  dataset %>%
#    filter(facility_type=="prison") %>%
#    group_by(facility) %>%
#    select(facility,year) %>%
#    mutate(yr_min=min(year,na.rm=T)) %>%
#    mutate(yr_max=max(year,na.rm=T)) %>%
#    mutate(yrs_available = paste(yr_min," - ",yr_max,sep="")) %>%
#    select(-c(yr_min,yr_max)) %>%
#    summarise(n=n()) -> t1
  
         # note: the use of the filter above excludes prisons where type tag didn't apply see below: 
      
        dataset %>% select(facility, facility_type) %>% arrange(order_by=facility)
        
        # this, when piped into a filter will capture the entries missed by the tagging function

        dataset$facility[dataset$facility_type=="prison"] %>% unique() -> madoc

dataset %>% 
  select(facility, new_date) %>%
  filter(facility!="") %>%
  group_by(facility) %>%
  arrange(order_by=new_date,.by_group=T)

  daydiffs<-NULL
  compmtrx<-NULL
  dataset %>% group_by(facility) %>% arrange(as.Date(new_date,"%m/%d/%Y"))%>% ungroup() %>% 
    filter(facility %in% madoc) -> dx
  for(md in madoc){
    flush.console()
    diff_array<-diff(as.Date(dx$new_date[dx$facility==md],"%m/%d/%Y"))
    print(md)
    print(diff_array)
    print(paste("mean = ",round(as.numeric(mean(diff_array)),2)," days"))
    dataset$insp_interval[dataset$facility==md] <- mean(diff_array)
  }

    mn<-function(x){round(mean(x,na.rm=T),2)}
    statCol<-function(stat){paste(round(mean(stat,na.rm = T),2), " (", min(stat,na.rm = T),",",max(stat,na.rm = T),")")}
    statCol2<-function(stat){paste("(", min(stat,na.rm = T),",",max(stat,na.rm = T),")")}
    dataset$facility[dataset$facility_type=="prison"] %>% unique() -> madoc
      
      dataset %>%
        filter(facility %in% madoc) %>%
        group_by(facility) %>%
        select(facility,year,total_violations,insp_interval) %>%
        mutate(numberdocs=n()) %>%
        mutate(totalvio_mean=mn(total_violations)) %>%
        mutate(totalviolations=statCol2(total_violations)) %>%
        mutate(yr_min=min(year,na.rm=T)) %>%
        mutate(yr_max=max(year,na.rm=T)) %>%
        mutate(yrs_available = paste(yr_min," - ",yr_max,sep="")) %>%
        select(-c(yr_min,yr_max,year,total_violations)) %>% 
        distinct() -> t1
      
      # table now facility name, number in sample, years available. 
      # now compute average interval between inspections
      
      dataset %>%
        filter(facility %in% madoc) %>%
        group_by(facility) %>%
        select(fc_total, repeat_string, facility, cmr_total, total_violations,year) %>%
        mutate(rp=statCol2(repeat_string)) %>%
        mutate(repeat_mean=mn(repeat_string)) %>%
        mutate(cm=statCol2(cmr_total)) %>%
        mutate(cmr_tot_mean=mn(cmr_total)) %>%
        mutate(fc=statCol2(fc_total)) %>%
        mutate(fc_tot_mean=mn(fc_total)) %>% 
        mutate(yr_min=min(year,na.rm=T)) %>%
        mutate(yr_max=max(year,na.rm=T)) %>%
        ungroup() %>%
        select(-c(total_violations, year, facility, repeat_string, cmr_total, fc_total)) %>% 
        select(-c(yr_min,yr_max)) %>%
        distinct() -> t2
      
      t1 %>% kbl("rst")
      t2 %>% kbl("rst")
      
    kbl(table1)
