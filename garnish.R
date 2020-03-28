
# garnish.R
# jkant@bu.edu

# read file, load local variables

library(stringr)
library(ggplot2)


ds<-read.csv("~/../Do~/../Downloads/south bay/sheet.csv",
             stringsAsFactors = FALSE, 
             header = TRUE, 
             check.names = FALSE)

ds<-ds[-which(nchar(colnames(ds))==1|nchar(colnames(ds))==2)]       # delete all cols 1-2 digits long
ds<-ds[-which(substr(colnames(ds),1,1)==2&nchar(colnames(ds))==4)]  # remove dates (2xxx)
i<-1                                                                # preserve code formatting
for(n in length(colnames(ds))){
  z<-colnames(ds)[x]
  if(nchar(z)<8){
    colnames(ds)[x]<-str_extract(z,"[[:digit:]]+\\.*[[:digit:]]*+")}
  if(nchar(colnames(ds)[x])>7){
    colnames(ds)[x]<-str_extract(z,"[[:digit:]]*+-[[:digit:]]+\\.*[[:digit:]]*+")}}

# following code extracts dates from ds$name column

i<-1
fList<-c("norfolk","souza","worcester county",
         "souza","south middlesex","concord",
         "pondville","middlesex county","shirley","mci framingham",
         "berkshire county","barnstable county","suffolk county")
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

ds %>% select(name, report_date)
ds %>% select(name, facility)

# ds$name[grep("Facility Inspection",ds$name)]<-substr(ds$name,30,nchar(ds$name))



# str_extract(colnames(ds)[x],"[[:digit:]]+\\.*[[:digit:]]*+")                # CMR format
# str_extract(colnames(ds)[41],"[[:digit:]]*+-[[:digit:]]+\\.*[[:digit:]]*+") # FC format

vio<-vector()
vio$yr<-format(as.Date(vio$date_report,"%d-%b-%y"),"%Y")
vio<-read.csv("~/../Desktop/violations.csv",header=T)
vio$date_report <- as.Date(vio$date_report,"%d-%b-%y")
vio$total_current<-as.numeric(vio$total_current)

vio<-read.csv("~/../Desktop/violations.csv",header=T)
library(tidyverse)
vioPlot<-function()
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
