
# vaccine and cell occupancy scraper
# jkant@bu.edu
# github,com/jesskay/inspectionbrowser

# pull code loosely based on https://bit.ly/3e1vgL9 and https://bit.ly/3wTLezI thanks friends!


  install.packages("pdftools")
  install.packages("tabulizer")
  install.packages("rJava")
  
  library(dplyr)
  library(tabulizer)
  library(rJava)
  library(rvest)
  library(pdftools)
  library(httr)

  Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_281/bin/")
  
  
# scrapers

    # cell occupancy reports

    
        print("loading...\n")
        read_html("https://www.mass.gov/lists/doc-covid-19-institution-cell-housing-reports/") %>%
          html_nodes(".ma__download-link__file-spec") %>% html_text()
  
        read_html("https://www.mass.gov/lists/doc-covid-19-institution-cell-housing-reports/") %>%
          html_elements(".ma__download-link__file-link") %>% html_attr("href") -> file_list

        dir.create("cell_reports")
        setwd("cell_reports/")
        for(f in file_list){
          file<-gsub(":","",gsub("/","",f))
          file<-substr(file,18,nchar(file))
          curl::curl_download(url  = f,destfile = paste(file,".pdf",sep=""))
        }
        
    # mine downloaded reports... 
        
        collection<-as.data.frame(NULL)
        local_files <-list.files()
        curr_target <-local_files[1]
        n<-1
        for(l in local_files){
          curr_layout <- tabulizer::extract_tables(local_files[n])
          tab_1 <- as.data.frame(curr_layout[1])                # table 1 is the summary
          names(tab_1)<-as.vector(tab_1[1,])                    # rename headers
          tab_1[-c(1,6),] -> tab_1
          collection<-rbind(collection,tab_1)
        }
        
        
        # pdftools version 
        
        targ <- pdftools::pdf_text(u) 
        as.data.frame(pdf_data(u)[3])$text %>% table()
        pdftools::pdf_text(u)[1] %>% head()
        head(targ<-pdf_data(u))[3]
        as.data.frame(pdf_data(u)[3])$x
