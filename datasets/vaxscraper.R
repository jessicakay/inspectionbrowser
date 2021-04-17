
# vaccine and cell occupancy scraper
# jkant@bu.edu
# github,com/jesskay/inspectionbrowser

# pull code loosely based on https://bit.ly/3e1vgL9 and https://bit.ly/3wTLezI thanks friends!


  install.packages("pdftools")
  install.packages("tabulizer")
  install.packages("rJava")
  install.packages("svMisc")
  
  library(dplyr)
  library(tabulizer)
  library(rvest)
  library(ggplot2)
  
  # library(pdftools)
  # library(httr)
  # library(rJava)
  
  Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_281/bin/")
  
# scrapers

    # cell occupancy reports
    
        print("loading...\n")
        read_html("https://www.mass.gov/lists/doc-covid-19-institution-cell-housing-reports/") %>%
          html_nodes(".ma__download-link__file-spec") %>% html_text()
  
        read_html("https://www.mass.gov/lists/doc-covid-19-institution-cell-housing-reports/") %>%
          html_elements(".ma__download-link__file-link") %>% html_attr("href") -> file_list

        # dir.create("cell_reports") # run first time
        
        setwd("cell_reports/")
        for(f in file_list){
          file<-gsub(":","",gsub("/","",f))
          file<-substr(file,18,nchar(file))
          curl::curl_download(url  = f,destfile = paste(file,".pdf",sep=""))
        }
        
    # vaccination reports
        
      vHome<-"https://www.mass.gov/service-details/committee-for-public-counsel-services-v-chief-justice-of-the-trial-court-sjc-12926/"

      read_html(vHome) %>%
        html_elements(".ma__rich-text a") %>% html_attr("href") %>% as.data.frame() -> links
      names(links)[1]<-"urls"
      links %>% filter(stringr::str_detect(urls,"sjc-12926-special-masters-weekly-report")) -> url_list
      
      url_list<-paste("https://mass.gov",url_list$urls,"/download",sep="")
      
      # dir.create("special_masters") 
      setwd("special_masters/")
      n<-0
      for(f in url_list){
        if(n==0) message(paste("\n\n    downloading reports: ",toString(length(url_list))," PDFs identified. \n\n    contacting server, progress below...\n\n"))
        file<-gsub(":","",gsub("/","",f))
        file<-substr(file,12,nchar(file))
        curl::curl_download(url  = f,destfile = paste(file,".pdf",sep=""))
        Sys.sleep(5)
        progress(n,progress.bar = TRUE)
        n<-n+1
      }
          
    # mine downloaded reports... 
        
        setwd("~/../Desktop/cell_reports")

        collection<-as.data.frame(NULL)
        local_files <-list.files()
        n<-1
        for(l in local_files){
          curr_layout <- tabulizer::extract_tables(local_files[n])
          tab_1 <- as.data.frame(curr_layout[1])                # table 1 is the summary
          names(tab_1)<-as.vector(tab_1[1,])                    # rename headers
          tab_1[-c(1,6),] ->> tab_1
          collection<<-rbind(collection,tab_1)
          n<-n+1
        }
        
        # in progress (grabs table 2)
        
        n<-1
        for(l in local_files){
          curr_layout <- tabulizer::extract_tables(local_files[n])[2] # specifies table 2
          tab_1 <- as.data.frame(curr_layout[1])   
          names(tab_1)<-as.vector(tab_1[1,])                          # rename headers
          tab_1[-c(1,6),] ->> tab_1
          collection<<-rbind(collection,tab_1)
          n<-n+1
        }

        collection %>% as.data.frame() %>% 
          filter(`REPORT DATE`!="") -> c_alldata                     # filter out 'total' lines before converting to wide
  
        collection <- c_alldata
        
        collection %>% group_by(INSTITUTION,`CELL HOUSING`) %>%
          mutate(COUNT=as.numeric(COUNT)) %>% summarize(MEAN_COUNT=mean(COUNT)) %>%
          filter(`CELL HOUSING`!="") %>% filter(INSTITUTION!="OUT-OF-DOC Transfer") %>% 
          tidyr::pivot_wider(names_from = c(`CELL HOUSING`), 
                                                           values_from = c(MEAN_COUNT))  %>% View()
          
          
        # transform table 1 data
        
        collection %>% tidyr::pivot_wider(names_from = c(CELL_HOUSING), 
                                          values_from = c(PERCENT,COUNT)) %>% write.csv(file="data.csv")

        
        # table 2 (prototype)
        
        tabulizer::extract_tables(local_files[1])[2] -> coll_tab2
        coll_tab2 %>% tidyr::pivot_wider(names_from = c(CELL_HOUSING), 
                                         values_from = c(PERCENT,COUNT)) %>% write.csv(file="data.csv")
        
        # plot data
        
        coll_mean <- tapply(collection$PERCENT,collection$CELL_HOUSING,mean)

        collection %>%
          mutate(REPORT_DATE=lubridate::as_date(`REPORT DATE`,format="%m/%d/%Y")) %>%
          group_by(CELL_HOUSING) %>%
          mutate(mean_percent=mean(PERCENT)) %>%
          ungroup() -> collection
        
         collection_backup<-collection
         
        # collection<-collection_backup

        
        over1<-collection$PERCENT[which(collection$CELL_HOUSING=="With 1 other person")]+
        collection$PERCENT[which(collection$CELL_HOUSING=="2 or more other people")] %>% as.data.frame()
        names(over1)[1]<-"plusone"  
       
        collection %>% group_by(REPORT_DATE) %>%
          summarize(plus1=PERCENT[which(CELL_HOUSING=="2 or more other people")]+
                      PERCENT[which(CELL_HOUSING=="With 1 other person")]) %>%
          ggplot(aes(x=reorder(REPORT_DATE,REPORT_DATE),y=plus1,label=paste(plus1,"%")))+
          geom_point(aes(y=plus1))+
          geom_text(vjust=-3,size=3)+
          labs(title = "Actual population 2 or more in MA DOC")+
          ylab(label = "% >1")+
          ylim(... = c(45,58))+
          xlab(label=element_blank())+
          theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())->plot_a
        
        collection %>%  
          ggplot(aes(group=CELL_HOUSING)) +
          geom_line(aes(y=PERCENT,x=reorder(REPORT_DATE,REPORT_DATE)),
                    linetype="dashed",colour="grey")+
          geom_hline(aes(yintercept=mean_percent),color="red",linetype="dotted")+
          geom_point(aes(y=PERCENT,x=reorder(REPORT_DATE,desc(REPORT_DATE))),shape=3)+
          facet_wrap(CELL_HOUSING~.,scales = "free")+
          theme(axis.text.x = element_text(angle = 90))+
          theme(axis.title.x = element_blank())+
          labs(title = "Cell occupancy in the MA DOC",subtitle = "October 2020 - April 2021",
               caption = "github.com/jesskay")->plot_b
        
        gridExtra::grid.arrange(plot_a, plot_b,
        nrow=2,heights=c(1,3))
       
        # calculations
        
        collection %>%
          group_by(CELL_HOUSING) %>%
          summarise(m=mean(PERCENT)) -> cell_m
        collection %>%
          group_by(CELL_HOUSING) %>%
          summarise(med=median(PERCENT)) -> cell_med
        collection %>%
          group_by(CELL_HOUSING) %>%
          summarise(max=max(PERCENT)) -> cell_max
        cbind(cell_m,cell_med,cell_max) -> cell_tab
        cbind(">1 person",cell_tab$m[1]+cell_tab$m[4],
              ">1 person",cell_tab$med[1]+cell_tab$med[4],
              ">1 person",cell_tab$max[1]+cell_tab$max[4]) %>% unlist() %>% as.vector() -> cx
        rbind(cell_tab,as.vector("-~-~-~"),cx,"")
        
        abline(lm(data = collection, PERCENT~REPORT_DATE+factor(CELL_HOUSING)))
        
                # pdftools version 
        
        targ <- pdftools::pdf_text(u) 
        as.data.frame(pdf_data(u)[3])$text %>% table()
        pdftools::pdf_text(u)[1] %>% head()
        head(targ<-pdf_data(u))[3]
        as.data.frame(pdf_data(u)[3])$x
