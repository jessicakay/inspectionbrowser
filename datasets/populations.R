# quick script to import capacity data
#
cap<-read.csv("~/../Documents/GitHub/inspectionbrowser/datasets/MA_capacities_cleaned.csv",header=TRUE)
cap$Facility<-tolower(cap$Facility)
cap$Facility<-gsub("-"," ",cap$Facility)

for(f in cap$Facility){
  ds$capacity[ds$facility==f]<-as.numeric(sum(cap$Capacity[cap$Facility==f]))
}


ds$over_cap<-ifelse(!is.na(ds$capacity)==TRUE,ds$total_pop-ds$capacity,"")


View(ds %>% select(facility, total_pop, capacity,max_capacity))

