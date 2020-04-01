# 
# garnish additional plots
# jkant@bu.edu 

s<-sub %>% filter(facility=="mci framingham"| facility=="cedar junction"|facility=="mci concord")

ggplot(s,aes(x=new_date,y=total_violations))+
  geom_line(s, mapping=aes(x=new_date,y=total_violations,color=facility))+
  ylab("number of violations")+
  xlab("year of inspection")+
  ggtitle("Total health inspection violations, MCI Norfolk, Framingham & Concord")

# all set

ggplot(s,aes(x=new_date,y=total_violations,fill=facility))+
  geom_bar(stat="identity",width = 20,position = "dodge")+
  scale_color_discrete(name="violations")+
  ylab("number of violations")+
  xlab("year of inspection")

png(filename = '~/../Desktop/violations.png', 
    width= 800, height=500)

ggplot(s,aes(x=new_date,y=total_violations))+
  geom_line(s, mapping=aes(x=new_date,y=total_violations,color=facility))+
  ylab("number of violations")+
  xlab("year of inspection")+
  ggtitle("Total health inspection violations, MCI Norfolk, Framingham & Concord")

dev.off()
