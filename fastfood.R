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

library(kableExtra)

kbl(table1)
