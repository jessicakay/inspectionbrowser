#
# github.com/jessicakay
#

install.packages("tesseract")
install.packages("pdftools")

library(tesseract)
library(pdftools)
library(dplyr)
library(stringr)

setwd("~/Downloads/wyatt/")

local_files <-list.files()

# core code

readOCR <- function(target){
  pdftools::pdf_ocr_text(target, pages = 1) ->> trg
  targ<<-str_split(trg, pattern="\n") %>% as.array()
  as.array(str_extract(targ[[1]][1],"[0-9]+/[0-9]+/[0-9]+")) -> newDate
  as.array(str_extract(targ[[1]][c(7,12,13,20)],"[0-9]+")) -> newObs      # vector values are 
  newObs <<- append(newDate,newObs)                                       # positions in array
}

n<-1
for(l in local_files){if(n<2){net<-as.data.frame(NULL)}
  if(grepl(".pdf",l)){
    writeLines(paste("processing file ",n,": ",l,"...\n"))
    readOCR(l)
    rbind(net,newObs) ->> net 
    }
  n<-n+1
}

# added benchmarking

readOCR <- function(target){
  startT<-Sys.time()
  pdftools::pdf_ocr_text(target, pages = 1) ->> trg
  targ<<-str_split(trg, pattern="\n") %>% as.array()
  as.array(str_extract(targ[[1]][1],"[0-9]+/[0-9]+/[0-9]+")) -> newDate
  as.array(str_extract(targ[[1]][c(7,12,13,20)],"[0-9]+")) -> newObs
  newObs <<- append(newDate,newObs)
  endT<-Sys.time()
  flush.console()
  writeLines(paste(round(eval(endT-startT),2)," secs to process\n"))
}

n<-1
for(l in local_files){
  if(n<2){
    net<-as.data.frame(NULL)
    benchStart<-Sys.time()}
  flush.console()
  if(grepl(".pdf",l)){
    writeLines(paste("processing file ",n,": ",l,"...\n"))
    readOCR(l)
    rbind(net,newObs) ->> net}
  if(n==length(local_files)){
    writeLines(paste(round(eval((Sys.time()-benchStart)),2)," mins runtime\n."))
  }
  n<-n+1
}


# asd

as.data.frame(pdf_ocr_data("050420report.pdf",pages = 1)) -> x
as.data.frame(pdf_ocr_data("june8th.pdf",pages = 1)) -> y
as.data.frame(pdf_ocr_data("wdc2.pdf",pages = 1)) -> z


x[,1][45]
y[,1][45]
z[,1][45]
