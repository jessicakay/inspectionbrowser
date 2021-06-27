
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

ds<-read.csv("/Users/jessa/Downloads/old computer/Downloads/south bay/sheet.csv",
                stringsAsFactors = FALSE, 
                header = TRUE, 
                check.names = FALSE)

  ds<-ds[-which(nchar(colnames(ds))==1|nchar(colnames(ds))==2)]       # delete all cols 1-2 digits long
  ds<-ds[-which(substr(colnames(ds),1,1)==2&nchar(colnames(ds))==4)]  # remove dates (2xxx)
  ds$name<-tolower(ds$name)                                           # necessary for matching in fList vector
  colnames(ds)
  toMove<-ds[1]                  # crude column cleaning, necessary for rowsum
  ds<-ds[-1]
  ds[length(ds)+1]<-toMove
  
# following code extracts dates from ds$name column and identifies facility 
  
ds$cmr_total<-rowSums(ds[which(str_detect(colnames(ds),"[:digit:]+\\.[:digit:]+"))],na.rm = TRUE)
ds$fc_total<-rowSums(ds[which(str_detect(colnames(ds),"[:digit:]+\\-[:digit:]+.[:digit:]+"))],na.rm = TRUE)
ds$total_violations<-ds$fc_total+ds$cmr_total
ds$repeat_string<-str_extract(ds$repeat_string,"[:digit:]+")
ds$repeat_string<-as.numeric(ds$repeat_string)
  
ice<-c(" ice "," i.c.e. ")
cty<-c("couny jail","lockup","house of correction")
fList<-c("mci norfolk","souza","worcester county", "norfolk county", "essex county", "dartmouth",
           "cedar junction", "mci walpole", "treatment center", "northeastern correctional",
           "north central", "bay state correctional", "dartmouth women", "dukes county",
           "souza","south middlesex correction","mci concord", "plymouth county", "old colony",
           "pondville","middlesex county","shirley","mci framingham", "mci plymouth",
           "berkshire county","barnstable county","suffolk county", "boston pre-release",
           "bridge water", "bridgewater","bristol county jail","hampden county", "boston pre release",
           "ash street","hampshire county")
ds$facility<-""
ds$report_date<-""
ds$facility_type<-""
i<-1


ds$name<-gsub("_"," ",ds$name)
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
      ds$facility[i]<-fList[iNum]
      }
    iNum<-iNum+1
    }
  i<-i+1
}

# identify facility type
ds$facility_type<-""

# note: ash street, NCCI, etc, are spelled out to detect specific instances that don't follow convention

ds$facility_type[which(str_detect(ds$name,"jail|lockup|house of correction|county|ash street")==TRUE)]<-"jail"
ds$facility_type[which(str_detect(ds$name,"mci|MCI|m.c.i.|correctional center|correctional institute|ncci")==TRUE)]<-"prison"
ds$facility_type[which(str_detect(ds$name,"substance|treatment|rehab|rehabilitation|alternative")==TRUE)]<-"substance"
ds$facility_type[which(str_detect(ds$name,"i\\.c\\.e|ice facility")==TRUE)]<-"ice"

# import created_date data from junkfood.py into useable data

ds$new_date<-ds$date
ds$new_date<-as.Date(ds$new_date,"%m/%d/%Y")

sub<-ds %>% select(name, date, new_date, facility, facility_type,cmr_total,fc_total, total_violations, repeat_string, report_date)



# Bristol County data needs cleaning, see below

sub<-ds %>% select(filename, new_date, facility, 
                   facility_type,cmr_total,fc_total, total_violations, 
                   repeat_string, ast_cnt, year, total_pop) %>%
            filter(facility=="bristol county") 


# data dictionary 

          ds$ventilation<-ds$`451.14`
          ds$slop_sink<-ds$`451.13` # plumbing not maintained, slop sink

# facilities: this function isolates out a specific facility and graphs the total violations

dataSource<-function(location){
  library
  fac<<-sub %>% filter(facility==location)
  vio<<-fac
  }

dataSource("mci framingham")

# basic plot 

vioPlot<-function(fac_name){
    fac_name<-paste("Code violations DPH, ",fac_name)
    ggplot(vio,aes(new_date,total_current))+
    labs(fill="violations")+
    geom_point(aes(y=fc_total,colour="FC"),size=2)+
    geom_point(aes(y=cmr_total,colour="CMR"),size=2)+
    scale_color_discrete(name="violations")+
    scale_x_date(date_labels = "%m/%y",breaks = vio$new_date)+
    ylab("number of violations")+
    xlab("date of inspection")+
    ggtitle(label = fac_name)+
    theme_grey()+
    theme(panel.grid.minor = element_blank())
}

png(filename = '~/../Desktop/violations.png', 
    width= 800, height=500)
vioPlot("MCI Framingham")
dev.off()

# new plot with total and repeat

vioPlot2<-function(fac_name){
  fac_name<-paste("Code violations DPH, ",fac_name)
  ggplot(vio,aes(new_date,total_current))+
    labs(fill="violations")+
    geom_line(aes(y=total_violations,colour="Total cited"),size=1)+
    geom_line(aes(y=repeat_string,colour="repeat violations"),size=1)+
    geom_line(aes(y=fc_total,colour="FDA (Food Code)"),size=1)+
    geom_line(aes(y=cmr_total,colour="CMR (MA state)"),size=1)+
    scale_color_discrete(name="violations")+
    scale_x_date(date_labels = "%m/%y",breaks = vio$new_date)+
    ylab("number of violations")+
    xlab("date of inspection")+
    ggtitle(label = fac_name)+
    theme_grey()+
    theme(panel.grid.minor = element_blank())
}

vioPlot2("MCI Framingham")