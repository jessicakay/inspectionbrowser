# jess kant
# jkant@bu.edu 2019

vio<-read.csv("~/../Desktop/violations.csv",header=T)
View(vio)
library(ggplot2)


# old

ggplot(vio,aes(date_report,total_current,xlab(vio$year)))+
  labs(fill="violations")+
  geom_point(aes(y=repeat_violations,colour="repeat"))+
  geom_point(aes(y=total_current,colour="total"))+
  scale_x_discrete(name="year of inspection", labels=vio$year)+
  scale_color_discrete(name="violations")+
  ylab("number of violations")+
  ggtitle(label = "Code violations DPH NCCI Gardner")

# new

vio$date_report <- as.Date(vio$date_report,"%d-%b-%y")
vio$yr <- format(as.Date(vio$date_report,"%d-%b-%y"),"%Y")
vio$date<-as.numeric(vio$date_report)

# same as above, but one more thing... and another..

vio<-read.csv("~/../Desktop/violations.csv",header=T)
vio$yr<-format(as.Date(vio$date_report,"%d-%b-%y"),"%Y")
vio$date_report <- as.Date(vio$date_report,"%d-%b-%y")
vio$total_current<-as.numeric(vio$total_current)

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
png(filename = '~/../Desktop/violations_800x500.png', width= 800, height=500)
vioPlot()
dev.off()

png(filename = '~/../Desktop/violations_1000x800.png', width= 1000, height=800)
vioPlot()
dev.off()

pdf(file = '~/../Desktop/violations_10x7.pdf', width = 10, height = 7)
vioPlot()
dev.off()

pdf(file = '~/../Desktop/violations_9x5.pdf', width = 9, height = 5)
vioPlot()
dev.off()
