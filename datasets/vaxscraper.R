
# vaccine scraper
# jkant@bu.edu
# github,com/jesskay/inspectionbrowser

# pull code loosely based on https://bit.ly/3e1vgL9 and https://bit.ly/3wTLezI thanks friends!


# plan A

{
  install.packages("tabulizer")
  install.packages("rJava")
  library(tabulizer)
  library(rJava)
  Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_281/bin/")
}

# plan B

{
  install.packages("pdftools")
  library(pdftools)
}

u<-"https://archives.lib.state.ma.us/bitstream/handle/2452/840149/on1199306687-106-Masters-Weekly-Report.pdf?sequence=1&isAllowed=y"

targ <- pdftools::pdf_text(u) 

as.data.frame(pdf_data(u)[3])$text %>% table()
pdftools::pdf_text(u)[1] %>% head()
head(targ<-pdf_data(u))[3]
as.data.frame(pdf_data(u)[3])$x

# https://www.mass.gov/lists/doc-covid-19-institution-cell-housing-reports