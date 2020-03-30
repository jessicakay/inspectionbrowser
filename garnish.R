
# garnish.R
# companion to junkfood.py 
#
# https://github.com/jessicakay/inspectionbrowser
#
# jkant@bu.edu

# read file, load local variables

library(stringr)
library(ggplot2)
library(dplyr)
library(lubridate)


ds<-read.csv("~/../Do~/../Downloads/south bay/sheet.csv",
               stringsAsFactors = FALSE, 
               header = TRUE, 
               check.names = FALSE)

  
  ds<-ds[-which(nchar(colnames(ds))==1|nchar(colnames(ds))==2)]       # delete all cols 1-2 digits long
  ds<-ds[-which(substr(colnames(ds),1,1)==2&nchar(colnames(ds))==4)]  # remove dates (2xxx)
  ds$name<-tolower(ds$name)                                           # necessary for matching in fList vector

# following code extracts dates from ds$name column and identifies facility 
# indexValues() is for resetting matrices/index variables

ice<-c(" ice "," i.c.e. ")
cty<-c("couny jail","lockup","house of correction")
fList<-c("mci norfolk","souza","worcester county", "norfolk county", "essex county", 
           "cedar junction", "mci walpole", "treatment center", "northeastern correctional",
           "north central", "bay state correctional", "dartmouth women", "dukes county",
           "souza","south middlesex correction","mci concord", "plymouth county", "old colony",
           "pondville","middlesex county","shirley","mci framingham", "mci plymouth",
           "berkshire county","barnstable county","suffolk county", "boston pre-release",
           "bridge water", "bridgewater","bristol county","hampden county", "boston pre release")
ds$facility<-""
ds$report_date<-""
ds$facility_type<-""
i<-1


for(n in ds$name){
  i<-as.numeric(i)
  if(str_detect(ds$name[i],",")==TRUE & (str_detect(ds$name[i],"[:digit:]+")==TRUE)){
    ds$report_date[i]<-str_extract(ds$name[i],"[[:alpha:]]+\\s[[:digit:]]+,\\s[[:digit:]]+")  
  }
  if ((str_detect(ds$name[i],"[[:digit:]]+/.[[:digit:]]+/.[[:digit:]]+")==TRUE)){
    ds$report_date[i]<-str_extract(ds$name[i],"[[:digit:]]+/.[[:digit:]]+/.[[:digit:]]+")
  }
  if(str_detect(ds$name[i],"[[:alpha:]]+\\s[[:digit:]][[:digit:]][[:digit:]][[:digit:]]")==TRUE){
    ds$report_date[i]<-str_extract(ds$name[i],"[[:alpha:]]+\\s[[:digit:]][[:digit:]][[:digit:]][[:digit:]]")
  }
  if(str_detect(ds$name[i],"[[:alpha:]]+/[[:digit:]][[:digit:]][[:digit:]][[:digit:]]")==TRUE){
    ds$report_date[i]<-str_extract(ds$name[i],"[[:alpha:]]+/[[:digit:]][[:digit:]][[:digit:]][[:digit:]]")
  }
  if(str_detect(ds$name[i],"[[:digit:]]+/[[:digit:]][[:digit:]][[:digit:]][[:digit:]]")==TRUE){
    ds$report_date[i]<-str_extract(ds$name[i],"[[:digit:]]+/[[:digit:]][[:digit:]][[:digit:]][[:digit:]]")
  }
  iNum<-1
  for(f in fList){
    if(str_detect(ds$name[i],fList[iNum])==TRUE){
      ds$facility[i]<-fList[iNum]}
    iNum<-iNum+1}
  i<-i+1
}

# identify facility type

ds$facility_type[which(str_detect(ds$name,"ice|i.c.e")==TRUE)]<-"ice"      
ds$facility_type[which(str_detect(ds$name,"jail|lockup|house of correction|county")==TRUE)]<-"jail"
ds$facility_type[which(str_detect(ds$name,"mci|MCI|m.c.i.")==TRUE)]<-"prison"
ds$facility_type[which(str_detect(ds$name,"substance|treatment|rehab|rehabilitation")==TRUE)]<-"substance"

# import created_date data from junkfood.py into useable data

ds$new_date<-ds$date
ds$new_date<-as.Date(ds$new_date,"%m/%d/%Y")

sub<-ds %>% select(name, report_date, date, new_date, facility, facility_type)
View(sub)

grep("Facility Inspection", ds$name)


# data dictionary 

ds$ventilation<-ds$`451.14`
ds$slop_sink<-ds$`451.13` # plumbing not maintained, slop sink

# facilities: this function isolates out a specific facility and graphs the total violations

dataSource<-function(location){
  library
  fac<<-ds %>% filter(facility==location)
  vio<<-fac
  }

dataSource("mci norfolk")

vioPlot<-function(facility)
{
  ggplot(vio,aes(date_report,total_current))+
    labs(fill="violations")+
    geom_point(aes(y=repeat_violations,colour="repeat"),size=2)+
    geom_point(aes(y=total_current,colour="total"),size=2)+
    scale_color_discrete(name="violations")+
    scale_x_date(date_labels = "%m/%y",breaks=vio$date_report)+
    ylab("number of violations")+
    xlab("date of inspection")+
    ggtitle(label = "Code violations DPH NCCI Gardner")+
    theme(panel.grid.minor = element_blank())
}

vioPlot()
png(filename = '~/../Desktop/violations.png', 
    width= 800, height=500)
vioPlot()
dev.off()
