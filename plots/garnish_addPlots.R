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

  
  s <-sub %>% 
    filter(facility=="mci framingham"| facility=="cedar junction"|facility=="mci concord")
  
    melted<-s %>%
      select(year, new_date, total_violations, repeat_string, facility)
      s<-melt(melted,id=c("facility","year"))
    
      
    s %>% 
      filter(year>2014) %>%
      ggplot(aes(x=year,y=total_violations,fill=facility))+
        geom_bar(stat="identity",width = 60,position = "dodge")+
        scale_color_discrete(name="violations")+
        ylab("number of violations")+
        xlab("year of inspection")





# box and whisker plot comparing 4 facilities

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




# mapping jails

png(filename = '~/../Desktop/violations.png', 
    width= 800, height=500)

ds %>% 
  select(new_date,year,total_violations,facility, facility_type) %>%
  filter(facility_type=="jail") %>%
  filter(facility!="") %>%
  arrange(year) %>%
  ggplot(aes(x=new_date,y=total_violations))+
  geom_point(mapping=aes(x=new_date,y=total_violations,color=facility))+
  ylab("number of violations")+
  xlab("year of inspection")+
  labs(title="Total health inspection violations, Massachusetts Jails",
       subtitle = "Massachusetts DPH Reports, 2010-2020",caption="jkant@bu.edu")+
  scale_color_brewer(palette = "Spectral")

dev.off()