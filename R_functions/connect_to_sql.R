
install.packages("DBI")
install.packages("odbc")

## doesn't work with power bi
library(odbc)
sort(unique(odbcListDrivers()[[1]]))

con = dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "Drdsqlts17.datasources.network", 
                 Database = "HERIDatamart", 
                 Trusted_Connection = "True")

q1 = tbl(con, 'DimHEIMSProvider') %>% select(ProviderCode) %>% distinct()
show_query(q1)

q1 %>% collect()


#### connent to Azure database
con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "edu-de-dbs-auc1-neeb-srv.database.windows.net",
                 Database = "HERIDatamart",
                 UID = "User",
                 PWD = "m!3&wKy#wuD")


q1 = tbl(con, 'DimHEIMSProvider') 
show_query(q1)


head(q1)


###### works with power bi
library(RODBC)

mycon = odbcDriverConnect('driver={SQL Server};server=Drdsqlts19.datasources.network;database=NEEB;trusted_connection=true')

queryResult1 = sqlQuery(mycon, "SELECT  * FROM HE_Finance_Funding(2011)") 


### Parameterized SQL queries (works with power bi
library(RODBCext)
library(tidyverse)

connHandle = odbcDriverConnect('driver={SQL Server};server=Drdsqlts19.datasources.network;database=NEEB;trusted_connection=true')

from = 2011
to = 2016
Years = from : to

datList = list()
for (i in 1: length(Years)){
       datList[[i]] =  sqlExecute(connHandle, "SELECT  * FROM HE_Finance_Provider(?)", Years[i], fetch = TRUE)                
}    
                
Providers = bind_rows(datList)
View(Providers)
