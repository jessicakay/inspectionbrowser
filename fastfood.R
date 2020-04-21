# quick reports for spot-checking and analysis
# jkant@bu.edu


df<- ds %>% 
  filter(facility_type=="prison") %>% 
  select(facility, new_date, year, total_pop, total_violations, perc_overcap) %>% 
  arrange(facility)

summary(lm(data=df, total_pop~total_violations+facility))

