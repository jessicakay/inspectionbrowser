# 
# garnish additional plots
# jkant@bu.edu 


ds %>%
  select(filename,year, facility, total_violations, cell_size) %>%
  ggplot(aes(x=toupper(facility),y=total_violations))+
    stat_boxplot(geom="errorbar",width=0.3)+
    geom_boxplot(aes(fill=facility))+
    scale_fill_brewer(palette = "Set2")+
    xlab("Facility")+
    ylab("Total violations per inspection")+
    labs(title="Variance in total violations, 2010-2020",
         subtitle = "Department of Public Health inspections of MDOC Facilities",
         caption = "jkant@bu.edu")



# scatter plot of all facilities, total violations

ds %>% 
  filter(facility_type=="prison") %>%
  arrange(year) %>%
  ggplot(aes(x=new_date,y=total_violations))+
    geom_point(mapping=aes(x=new_date,y=total_violations,color=facility))+
    ylab("number of violations")+
    xlab("year of inspection")+
    labs(title="Total health inspection violations, MCI Cedar Junction, Framingham & Concord",
         subtitle = "Massachusetts DPH Reports, 2010-2020",caption="jkant@bu.edu")+
         scale_color_brewer(palette = "Set2")

# scatter plot, all facilities, stratified

ds %>% 
  filter(facility_type=="prison") %>%
  arrange(year) %>%
  ggplot(aes(x=new_date,y=total_violations))+
  geom_point(mapping=aes(x=new_date,y=total_violations,color="Total"))+
  geom_point(mapping=aes(x=new_date,y=repeat_string,color="Repeat"))+
  ylab("number of violations")+
  xlab("year of inspection")+
  labs(title="Health Inspection Violations, Repeat violations",
       subtitle = "Massachusetts DPH Reports, 2010-2020",caption="jkant@bu.edu")+
  scale_color_brewer(palette = "Set1")


# compare three facilities

s <-sub %>% 
  filter(facility=="mci framingham"| facility=="cedar junction"|facility=="mci concord")

ggplot(s,aes(x=new_date,y=total_violations))+
  geom_line(s, mapping=aes(x=new_date,y=total_violations,color=facility))+
  ylab("number of violations")+
  xlab("year of inspection")+
  ggtitle("Total health inspection violations, MCI Norfolk, Framingham & Concord")

png(filename = '~/../Desktop/violations.png', 
    width= 800, height=500)
lineChart()
dev.off()


lineChart<-function(){
 s <-sub %>% 
  filter(year>2009) %>%
  filter(facility %in% c("mci framingham","cedar junction","mci concord"))
  ggplot(s,aes(x=new_date,y=total_violations))+
   geom_line(s, mapping=aes(x=new_date,y=total_violations,color=facility))+
   ylab("number of violations")+
   xlab("year of inspection")+
   labs(title="Total health inspection violations, MCI Cedar Junction, Framingham & Concord",
        subtitle = "Massachusetts DPH Reports, 2010-2020",caption="jkant@bu.edu")+
   scale_color_brewer(palette = "Set2")
}

  library(reshape2)


s<-ds %>%
  select(year, new_date, total_violations, repeat_string, facility) %>%
  filter(facility=="mci framingham") %>%
  filter(year>2013) %>%
  melt(melted, id.vars = c("new_date","facility"), 
       measure.vars =c("total_violations","repeat_string"),
       variable.name = "newORold") %>%
  arrange(new_date)
  s$new_date<-as.Date(s$new_date)

ggplot(data=s,aes(x=new_date,y=value,fill=newORold))+
  geom_bar(stat="identity",position = "stack")+
  scale_color_discrete(name="violations")+
  ylab("number of violations")+
  xlab("Date of report")+
  labs(title="MCI Framingham Total and Repeat Violations",
       subtitle = "Source: DPH Inspections, 2015-2020",
       caption = "deeperthanwater.org", fill="Type")+
  scale_fill_discrete(name="Type", labels=c("Total","Repeat"))+
  scale_x_date(date_labels = "%m/%y", breaks = s$new_date)




# box and whisker plot comparing 4 facilities, 
# framingham, norfolk, cedar junction and concord
# color

png(filename = '~/../Desktop/violations.png', 
    width= 800, height=500)

s<-sub %>%
  filter(facility %in% c("mci framingham","cedar junction","mci concord","mci norfolk"))
  s$facility<-with(s, reorder(facility, total_violations))
  ggplot(data=s, aes(x=toupper(facility),y=total_violations))+
    stat_boxplot(geom="errorbar",width=0.3)+
    geom_boxplot(aes(fill=facility))+
    scale_fill_brewer(palette = "Set2")+
    xlab("Facility")+
    ylab("Total violations per inspection")+
    labs(title="Variance in total violations, 2010-2020",
         subtitle = "Department of Public Health inspections of MDOC Facilities",
         caption = "jkant@bu.edu")

  
  dev.off()
  
# save as image 

png(filename = '~/../Desktop/violations.png', 
    width= 800, height=500)
    ggplot(sub,aes(x=new_date,y=total_violations))+
      geom_line(sub, mapping=aes(x=new_date,y=total_violations,color=facility))+
      ylab("number of violations")+
      xlab("year of inspection")+
      ggtitle("Total health inspection violations, MCI Norfolk, Framingham & Concord")
dev.off()



write.csv(ds,"~/../Desktop/cleaned_04022020.csv")

# mapping jails

ds %>% 
  select(new_date,year,total_violations,facility, facility_type) %>%
  filter(facility_type=="jail") %>%
  filter(facility != "") %>%
  arrange(year) %>%
  ggplot(aes(x=new_date,y=total_violations))+
  geom_point(mapping=aes(x=new_date,y=total_violations,color=facility))+
  ylab("number of violations")+
  xlab("year of inspection")+
  labs(title="Sample of total health inspection violations, Massachusetts Jails",
       subtitle = "Massachusetts DPH Reports, 2015-2020",caption="deeperthanwater.org")+
  scale_color_brewer(palette = "Spectral")

# jails v. prisons 2015-present

png(filename = '~/../Desktop/violations.png', 
    width= 800, height=500)

s<-ds %>% 
  select(new_date, year, total_violations, facility, facility_type) %>%
  filter(facility_type=="jail" | facility_type=="prison") %>%
  filter(year>2014) %>%
  filter(facility!="") %>%
  arrange(year)

        # simple plot

        ggplot(data=s,aes(x=new_date,y=total_violations))+
          geom_point(mapping=aes(x=new_date,y=total_violations,color=facility_type))+
          ylab("number of violations")+
          xlab("year of inspection")+
          labs(title="Health inspection violations, Massachusetts Jails and Prisons",
               subtitle = "Massachusetts DPH Reports, 2015-2020",caption="jkant@bu.edu",
               colour="Facility Type")+
          scale_color_brewer(palette = "Set1")


dev.off()

fham <- ds %>% filter(facility=="mci framingham") 

plot(glm(fham$repeat_string~fham$total_pop))

# base-R pop vs. violations

popVvio<-ds$total_pop~ds$total_violations
plot(popVvio)
abline(lm(popVvio))
ggplot(ds,total_pop~total_violations)

# relationship of violations to prison population, linear regression

lm(ds$total_pop~ds$total_violations+ds$facility)

png(filename = '~/../Desktop/violations.png', 
    width= 800, height=500)
ds %>%
  ggplot(aes(total_pop,total_violations))+
  geom_point()+
  geom_smooth(method="lm")+
  xlab("Facility census (population)")+
  ylab("number of violations")+
  labs(title="Health inspection violations, Jails & Prisons",
       subtitle = "Simple linear regression, all facilities, 2010-2020",
       caption="jkant@bu.edu",
       colour="Facility Type")

dev.off()

# histogram
# population vs. max capacity

s<-ds %>%
  select(year, new_date, total_violations, repeat_string, facility, facility_type, filename, total_pop, capacity, over_cap) %>%
  filter(facility_type=="prison") %>%
  filter(year==2019) %>%
  melt(melted, id.vars = c("facility", "filename"), 
       measure.vars =c("total_pop","capacity"),
       variable.name = "newORold")
ggplot(data=s,aes(x=toupper(facility),y=value,fill=newORold))+
  geom_bar(stat="identity",position = "dodge")+
  ylab("# of people in facility")+
  xlab("Facility")+
  scale_fill_discrete(name="Type", labels=c("Total","Capacity"))+
  labs(title="Population vs. Max Capacity",
       subtitle = "Source: DPH Inspections, 2019",
       caption = "deeperthwnater.org", fill="Type")

####### experimental cleveland plot

s<-ds %>%
  select(year, new_date, total_violations, repeat_string, facility, facility_type, total_pop, capacity, over_cap, filename) %>%
  filter(facility_type=="prison") %>%
  filter(year>2015 & year<2020) %>%
  filter(facility!="north central") %>%
  melt(melted, id.vars = c("facility", "filename", "year","total_pop"), 
       measure.vars =c("total_pop","capacity"),
       variable.name = "newORold")

s<-s %>% group_by(facility) %>% arrange(total_pop)

s$facility<-as.character(s$facility)
s$facility<-factor(s$facility,levels=unique(reorder(s$facility,s$total_pop)))

levels(s$facility)

ggplot(data=s,aes(x=fct_inorder(toupper(facility)),y=value,fill=newORold))+
  geom_point(stat="identity",aes(color=newORold), show.legend = FALSE)+
  geom_line(aes(group=facility), show.legend = FALSE)+
  ylab("# of people in facility")+
  xlab("Facility")+
  scale_fill_discrete(name="Type", labels=c("Total","Capacity"))+
  labs(title="Population vs. Max Capacity",
       subtitle = "Source: DPH Inspections, 2019",
       caption = "jkant@bu.edu", fill="Type")+
  coord_flip()+
  facet_grid(year~.)


######

s<-ds %>%
  select(year, new_date, total_violations, repeat_string, facility, facility_type, filename, total_pop, capacity, over_cap) %>%
  filter(facility_type=="prison") %>%
  filter(facility!="north central") %>%
  filter(year %in% c(2018,2019) ) %>%
  melt(melted, id.vars = c("facility", "filename","year"), 
       measure.vars =c("total_pop","capacity"),
       variable.name = "newORold")

s$newORold<-factor(s$newORold,levels=c("capacity","total_pop"))

png(filename = '~/../Desktop/population.png', 
    width= 800, height=500)

ggplot(data=s,aes(x=toupper(facility),y=value,fill=newORold))+
  geom_bar(stat="identity",position = "dodge")+
  ylab("# of people in facility")+
  xlab("Facility")+
  scale_fill_discrete(name="", labels=c("Capacity","Actual"))+
  labs(title="Population vs. Max Capacity",
       subtitle = "Source: DPH Inspections, 2019",
       caption = "deeperthanwater.org", fill="Type")+
  facet_grid(year~.)

dev.off()

arrange(s,facility)

# population by total violations

ds %>% 
  select(new_date, year, total_violations, facility, facility_type, total_pop, capacity) %>%
  filter(facility_type=="jail" | facility_type=="prison") %>%
  filter(year>2014) %>%
  filter(facility!="") %>%
  arrange(year) %>%
  ggplot(aes(total_pop,total_violations))+
  geom_point()+
  geom_smooth(method="lm")+
  ylab("number of violations")+
  xlab("Facility census at inspection")+
  labs(title="Simple linear regression, relationship of populaiton to code violations",
       subtitle = "Massachusetts DPH Reports, 2010-2020",caption="jkant@bu.edu",colour="Facility Type")+
  scale_color_brewer(palette = "Set1")+
  facet_grid(.~facility_type)

# percent over capacity

s<-ds %>%
  select(filename, facility, total_pop, capacity, perc_overcap, new_date, year, facility_type, name) %>%
  filter(facility_type=="prison") %>%

s$facility<-with(s, reorder(facility, as.numeric(perc_overcap)))
ggplot(data=s, aes(x=toupper(facility),y=as.numeric(perc_overcap)))+
  geom_hline(yintercept = 0, linetype="dashed",color="darkgrey")+
  stat_boxplot(geom="errorbar",width=0.3)+
  geom_boxplot(aes(), alpha=0.6)+
  coord_flip()+
  xlab("Facility | note: data still being added")+
  ylab("% over maximum rated capacity")+
  labs(title="Percentage over maximum capacity, 2010-2020",
       subtitle = "Department of Public Health inspections of MDOC Facilities",
       caption = "jkant@bu.edu")


# zoomed in % over capacity

s<-ds %>%
  select(filename, facility, total_pop, capacity, perc_overcap, new_date, year, facility_type, name) %>%
  filter(facility_type=="prison") %>%
  filter(facility!="north central") %>%
  filter(year>2017)
s$under<-ifelse(s$perc_overcap<0,1,0)
s<-s %>% group_by(facility) %>% mutate(mean_over = mean(as.numeric(perc_overcap)))
s$facility<-with(s, reorder(facility, as.numeric(perc_overcap)))
s$mean_over<-paste("m = ",round(s$mean_over,2),"%")
ggplot(data=s, aes(x=toupper(facility),y=as.numeric(perc_overcap)))+
  geom_text(size=3, aes(label=mean_over, y=70), check_overlap = TRUE)+
  geom_hline(yintercept = 0, linetype="dashed",color="darkgrey")+
  guides(fill=guide_legend(reverse=TRUE,title = element_blank()))+
  scale_fill_discrete(labels=c("over","under"))+
  geom_bar(stat="identity", alpha=0.6, width=0.5, aes(fill=factor(under)),position = position_identity())+
  coord_flip()+
  xlab("Facility")+
  ylab("% over maximum rated capacity")+
  labs(title="Percentage over maximum capacity, 2018-2020",
       subtitle = "Department of Public Health inspections of MDOC Facilities",
       caption = "jkant@bu.edu")

# total violations and repeat violations histogram
# with line and point overlap of population at time

s<-ds %>%
  select(year, new_date, filename, total_violations, repeat_string, facility, total_pop, over_cap, perc_overcap) %>%
  filter(facility=="mci framingham") %>%
  filter(year>2014) %>%
  melt(melted, id.vars = c("new_date","facility","total_pop"), 
       measure.vars =c("total_violations","repeat_string"),
       variable.name = "newORold") %>%
  arrange(new_date)
s$new_date<-as.Date(s$new_date)

ggplot(data=s,aes(x=new_date,y=value,fill=newORold))+
  geom_bar(stat="identity",position = "dodge")+
  geom_point(mapping=aes(x=new_date,y=as.numeric(total_pop)))+
  geom_text(mapping=aes(x=new_date,y=as.numeric(total_pop),label=total_pop),
            colour="blue",nudge_y = 20,size=2.5)+
  geom_line(mapping=aes(x=new_date,y=as.numeric(total_pop)),colour="gray")+
  scale_color_discrete(name="violations",labels=c("population"),)+
  ylab("number of violations")+
  xlab("Date of report")+
  labs(title="MCI Framingham Total and Repeat Violations",
       subtitle = "Source: DPH Inspections, 2015-2020",
       caption = "jkant@bu.edu", fill="Type")+
  scale_fill_discrete(name="Type", labels=c("Total","Repeat"))+
  scale_x_date(date_labels = "%m/%y", breaks = s$new_date)

