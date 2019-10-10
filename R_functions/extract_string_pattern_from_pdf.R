library(pdftools)
library(stringr)
library(tidyverse)

text = pdf_text("J:\\My Documents\\NEEB\\2017-HE-Data-Element-Dictionary.pdf")
text2 = strsplit(text, "\n")
head(text2[[1]])
text2[[2]]

length(text2[[2]])
length(text2) ## page number 234

## extract the number from string
numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
} 

codes = list()
for (i in 1:length(text2)){
  elem_string = text2[[i]][grepl('ELEMENT NO.',text2[[i]])]
 codes[[i]] = numextract(elem_string)
  
}

he_code = unlist(codes)
matrix(he_code, ncol = 1)

##
elemName = list()
for (i in 1:length(text2)){
  elemName_string = text2[[i]][grepl('ELEMENT NAME:',text2[[i]])]
  elemName[[i]] = str_remove(str_remove(elemName_string, "ELEMENT NAME:"),'\r')
  
}

he_elemName  = unlist(elemName)

out = data.frame(code = he_code, elementName = he_elemName, stringsAsFactors = F)


###
text2[[2]][grepl('ELEMENT NO.',text2[[2]])]
text2[[2]][grepl('DESCRIPTION:',text2[[2]])]

s = text2[[2]][grepl('DESCRIPTION:',text2[[2]])]
strsplit(s, split=':', fixed=TRUE)[[1]][2]


str_remove(str_remove(s, "DESCRIPTION:"),'\r')


##
out1 <- extract_tables('J:\\My Documents\\NEEB\\2017-HE-Data-Element-Dictionary.pdf')
