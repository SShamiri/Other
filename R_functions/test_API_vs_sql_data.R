library(RODBC)
library(jsonlite)
library(httr)
library(tidyverse)

mycon = odbcDriverConnect('driver={SQL Server};server=Drdsqlts19.datasources.network;database=NEEB;trusted_connection=true')

## sql data
dataSQL = sqlQuery(mycon, "SELECT  * FROM [dbo].[SC_Integrated_School](2016)",stringsAsFactors = FALSE)  %>% tbl_df() %>% arrange(clientIK)

### Azure API
req = GET(url = 'https://test.itspapi.education.gov.au/schoolsdatamart/v1/Schools/2016',add_headers('api-key' ='4aec341a5c034272b0aacd787611deca',
                                                                                                    'user-key' = '45127773dsaedw32132'))
req2 = GET(url = 'https://test.itspapi.education.gov.au/schoolsdatamart/v1/Schools/2016?pagination-key=2',add_headers('api-key' ='4aec341a5c034272b0aacd787611deca',
                                                                                                     'user-key' = '45127773dsaedw32132'))

dat = content(req, "parsed")$data
dat2 = content(req2, "parsed")$data

#bind_rows(dat,dat2)%>% filter(clientIK == 13709) 
dataAPI = bind_rows(dat,dat2) %>% arrange(clientIK)


all_equal(dataAPI,dataSQL,convert = TRUE)

########  Napplan

reqt = GET(url = 'https://test.itspapi.education.gov.au/schoolsdatamart/v1/Results/naplan/2016',add_headers('api-key' ='4aec341a5c034272b0aacd787611deca',
                                                                                                    'user-key' = '45127773dsaedw32132'))

reqt2 = GET(url = 'https://test.itspapi.education.gov.au/schoolsdatamart/v1/Results/naplan/2016?&pagination-key=2',add_headers('api-key' ='4aec341a5c034272b0aacd787611deca',
                                                                                                            'user-key' = '45127773dsaedw32132'))

reqt3 = GET(url = 'https://test.itspapi.education.gov.au/schoolsdatamart/v1/Results/naplan/2016?page-size=6000',add_headers('api-key' ='4aec341a5c034272b0aacd787611deca',
                                                                                                                               'user-key' = '45127773dsaedw32132'))

naplan3 = content(reqt3, "parsed")$data

naplan1 = content(reqt, "parsed")$data
naplan2 = content(reqt2, "parsed")$data

naplan = bind_rows(naplan1,naplan2) %>% arrange(naplanSchoolResultSK)

d = data.frame(matrix(unlist(naplan1), nrow=length(naplan1), byrow=T),stringsAsFactors=FALSE)
dd = do.call(rbind.data.frame, naplan1,stringsAsFactors=FALSE)

#92155
#########################
identical(dataSQL,dataAPI)

dataSQL[4821,]
dataAPI[4821,]

#dat = fromJSON(rawToChar(req$content))$data %>% tbl_df() 
#dat2 = fromJSON(rawToChar(req2$content))$data %>% tbl_df() 

bind_rows(naplan1)

length(naplan1)

naplan1[[5000]]

dd = fromJSON(rawToChar(reqt3$content))$data %>% tbl_df() 

############################ Naplan #########
start = Sys.time()
reqt = GET(url = 'https://test.itspapi.education.gov.au/schoolsdatamart/v1/Results/naplan/2016',add_headers('api-key' ='4aec341a5c034272b0aacd787611deca',
                                                                                                            'user-key' = '45127773dsaedw32132'))
get_text = content(reqt, "text")

get_json = fromJSON(get_text, flatten = TRUE)
tlt_pages = ceiling(get_json$pagination$total/get_json$pagination$size)

dataList = list()
dataList[[1]] = get_json$data

for (page in 2:tlt_pages){
  baseURL = paste0('https://test.itspapi.education.gov.au/schoolsdatamart/v1/Results/naplan/2016?&pagination-key=',page)
  reqt = GET(url = baseURL,add_headers('api-key' ='4aec341a5c034272b0aacd787611deca','user-key' = '45127773dsaedw32132'))
  get_text = content(reqt, "text")
  get_json = fromJSON(get_text, flatten = TRUE)
  dataList[[page]] = get_json$data
}

data = bind_rows(dataList)

Sys.time() -start


#################### enrollment
start = Sys.time()
reqt = GET(url = 'https://test.itspapi.education.gov.au/neeb/schoolsdatamart/v1/Enrolments/2016',add_headers('api-key' ='4aec341a5c034272b0aacd787611deca',
                                                                                                            'user-key' = '45127773dsaedw32132'))
get_text = content(reqt, "text")

get_json = fromJSON(get_text, flatten = TRUE)
tlt_pages = ceiling(get_json$pagination$total/get_json$pagination$size)

dataList = list()
dataList[[1]] = get_json$data

for (page in 2:tlt_pages){
  baseURL = paste0('https://test.itspapi.education.gov.au/neeb/schoolsdatamart/v1/Enrolments/2016?&pagination-key=',page)
  reqt = GET(url = baseURL,add_headers('api-key' ='4aec341a5c034272b0aacd787611deca','user-key' = '45127773dsaedw32132'))
  get_text = content(reqt, "text")
  get_json = fromJSON(get_text, flatten = TRUE)
  dataList[[page]] = get_json$data
}

data = bind_rows(dataList)

Sys.time() -start


## sql data
dataSQL = sqlQuery(mycon, "SELECT  * FROM [dbo].[SC_Integrated_Enrolment](2016)",stringsAsFactors = FALSE)  %>% tbl_df()

all_equal(dataSQL,data)


names(data) = tolower(names(data))
names(dataSQL) = tolower(names(dataSQL))

data$attendanceratetext = as.numeric(data$attendanceratetext)
data$bridgingvisaheadcount = as.integer(data$bridgingvisaheadcount)
data$nonindigenousheadcount = as.integer(data$nonindigenousheadcount)
data$nontuitionboarderheadcount = as.integer(data$nontuitionboarderheadcount)
data$enrolmentdate = as.Date(data$enrolmentdate)
data$enrolmentdate = as.character(data$enrolmentdate)


all_equal(dataSQL,data)


dataSQL[263141,]
data %>% filter(enrolmentsk == 14449979)
