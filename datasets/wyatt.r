

install.packages("tesseract")
install.packages("pdftools")

library(tesseract)
library(pdftools)
library(dplyr)
library(stringr)

setwd("~/Downloads/wyatt/")
local_files <-list.files()

readOCR <- function(target){
  pdftools::pdf_ocr_text(target, pages = 1) ->> trg
  targ<<-str_split(trg, pattern="\n") %>% as.array()
  as.array(str_extract(targ[[1]][1],"[0-9]+/[0-9]+/[0-9]+")) -> newDate
  as.array(str_extract(targ[[1]][c(7,12,13,20)],"[0-9]+")) -> newObs
  newObs <<- append(newDate,newObs)
}

n<-1
for(l in local_files){
  if(n<2){net<-as.data.frame(NULL)}
  flush.console()
  if(grepl(".pdf",l)){
    writeLines(paste("processing... ",l,"\n"))
    readOCR(l)
    rbind(net,newObs) ->> net
  }
  n<-n+1
}





