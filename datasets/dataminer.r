

setwd("special_masters/")

batch_grab

n<-0
for(f in url_list){
  if(n==0) message(paste("\n\n    downloading reports: ",toString(length(url_list)),
                         " PDFs identified. \n\n\tcontacting server, progress below...\n\n"))
  file<-gsub(":","",gsub("/","",f))
  file<-substr(file,12,nchar(file))
  curl::curl_download(url  = f,destfile = paste(file,".pdf",sep=""))
  Sys.sleep(5)
  progress(n,progress.bar = TRUE)
  n<-n+1
}