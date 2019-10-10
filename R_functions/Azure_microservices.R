
### Azure API

library(jsonlite)
library(httr)
library(tidyverse)


req = GET(url = 'https://test.itspapi.education.gov.au/neeb/heri/v1/Heri/providerlocationlinkages/2016?groups=heri',add_headers('api-key' ='4aec341a5c034272b0aacd787611deca'))
#or
#GET(url ='https://test.itspapi.education.gov.au',path ='neeb/heri/v1/Heri/providerlocationlinkages/2016?groups=heri',add_headers('api-key' = '4aec341a5c034272b0aacd787611deca') )

req$url
req$headers
req$content

dat = fromJSON(rawToChar(req$content))

### or

base = 'https://test.itspapi.education.gov.au/neeb/heri/v1/Heri/providerlocationlinkages/'
year =  '2016'
heri = 'heri'


call = paste(base,year,"?","groups","=", heri, sep="")

GET(call, add_headers('api-key' ='4aec341a5c034272b0aacd787611deca'))


### for paging or mutliple years


base = 'https://test.itspapi.education.gov.au/neeb/heri/v1/Heri/providers/'
heri = 'heri'
years = c(2015,2016)
datList = list()

for (i in 1: length(years)){
#  i =1
  call = paste(base,years[i],"?","groups","=", heri, sep="")
  req = GET(call, add_headers('api-key' ='4aec341a5c034272b0aacd787611deca'))
  datList[[i]] = fromJSON(rawToChar(req$content))

}

dat = bind_rows(datList)
