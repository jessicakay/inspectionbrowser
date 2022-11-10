# 2021 prison data project
# jessica kant
# github.com/jessicakay
#


install.packages("tesseract")
install.packages("pdftools")
install.packages("magick")

library(tesseract)
library(pdftools)
library(dplyr)
library(stringr)
library(magick)

setwd("/Users/jessa/Downloads/wyatt/")

local_files <-list.files()

# core code

readOCR <- function(target){
  pdftools::pdf_ocr_text(target, pages = 1) ->> trg
  targ<<-str_split(trg, pattern="\n") %>% as.array()
  as.array(str_extract(targ[[1]][1],"[0-9]+/[0-9]+/[0-9]+")) -> newDate
  as.array(str_extract(targ[[1]][c(7,12,13,20)],"[0-9]+")) -> newObs          # vector values are 
  newObs <<- append(newDate,newObs)                                           # positions in array
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


# added benchmarking, optional arguments for better cha

convertBW<-function(picture){
    image_read(picture) %>% 
    image_convert(colorspace = "gray") %>% 
    image_modulate(brightness = 200, saturation = 50) %>% 
    image_threshold(type = c("black","white")) ->> tempImage
    (image_resize(tempImage,geometry = "15%"))
    }


readOCR <- function(target,gridVals,magnification){
  startT<-Sys.time()
  if(magnification==0){ pdftools::pdf_ocr_text(target, pages = 1) %>% suppressMessages() ->> trg }
  if(magnification==1){ 
    pdf_convert(target, pages = 1, dpi = 800, format = "tiff")
    fileHandle<-paste(substr(target,1,nchar(target)-4),"_1.tiff",sep="")
    convertBW(fileHandle)-> tempImage
      (image_resize(tempImage,geometry = "15%"))
    ocr(tempImage, engine = tesseract("eng")) %>% suppressMessages() ->> trg                                      
    file.remove(fileHandle)
    }
  if(magnification>=2){ 
    pdftools::pdf_ocr_text(target, pages = 1, dpi = 400 * magnification) ->> trg                       
    }                                                                                                  
  targ<<-str_split(trg, pattern="\n") %>% as.array()
  as.array(str_extract(targ[[1]][1],"[0-9]+/[0-9]+/[0-9]+")) ->> newDate
  as.array(str_extract(targ[[1]][gridVals],"[0-9]+")) -> newObs
  as.array(str_extract(targ[[1]],"[0-9]+%")) ->> newCap 
  newCap <<- cbind(newCap[which(str_detect(newCap,"%"))],newDate)
  newObs <<- append(newDate,newObs)
  endT<-Sys.time()
  flush.console()
  writeLines(paste("\n",round(eval(endT-startT),2)," secs to process\n"))
  writeLines(paste(" -> ", newCap[2],": facility capacity = ", newCap[1],"\n"))
}

   
runScan<-function(gridVals=c(7,12,13,20),magnification=1){
  n<-1
  for(l in local_files){
    if(n<2){
      net<-as.data.frame(NULL)
      benchStart<-Sys.time()
      scan_log<<-as.vector(NULL)
      cap_log<<-c("capacity","date")
      }
    flush.console()
    if(grepl(".pdf",l)){
      writeLines(paste("processing file ",n,": ",l,"...\n"))
      readOCR(l,gridVals,magnification)
      rbind(net,newObs) ->> net}
    if(n==length(local_files)){
      writeLines(paste(round(eval((Sys.time()-benchStart)),2)," mins runtime\n."))
    }
    append(scan_log,targ) ->> scan_log 
    n<-n+1
  }
}


  
  # run the scan with default, and custom peramaeters

  runScan()                                              # run with default perameters
  runScan(gridVals = c(1,6,9))                           # extract numeric data from array positions 1, 6, and 9

  runScan(gridVals = gridVals, magnification = 2)        # double DPI to 800

  
  # mag option 0 uses default pdftools settings: png, 72dpi
  # mag option 1 uses TIFF at at 800 DPI, then passes through imagemagick to convert image to "pure" black and white
  # mag option 2+ uses png @ dpi = 400 * mag
  
  
  