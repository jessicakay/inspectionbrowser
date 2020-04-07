# quick script to import capacity data
#
cap<-read.csv("~/../Documents/GitHub/inspectionbrowser/datasets/capacities.csv",header=TRUE)

cap$Facility<-tolower(cap$Facility)
cap$Facility<-gsub("-"," ",cap$Facility)

fList<-c("mci norfolk","souza","worcester county", "norfolk county", "essex county", "dartmouth",
         "cedar junction", "mci walpole", "treatment center", "northeastern correctional",
         "north central", "bay state correctional", "dartmouth women", "dukes county",
         "souza","south middlesex correction","mci concord", "plymouth county", "old colony",
         "pondville","middlesex county","shirley","mci framingham", "mci plymouth",
         "berkshire county","barnstable county","suffolk county", "boston pre-release",
         "bridge water", "bridgewater","bristol county","hampden county", 
         "boston pre release", "old colony")


i<-1
iNum<-1
for(n in cap$Facility){
  for(f in fList){
    if(str_detect(cap$Facility[i],fList[iNum])==TRUE){
      cap$Facility[i]<-fList[iNum]
    }
    iNum<-iNum+1
  }
  i<-i+1
}
