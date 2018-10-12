#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Author: Samuel Shamiri
# Date: 12/10/2018
# Version: 0.1
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

library(haven)
library(Matching)
library(ggplot2)
library(broom)
library(xlsx)
library(tidyverse)

PSM = function(Treat, outcome ,xvars ,dat) {
  Y = dat[,outcome]
  Tr = dat[,Treat]
  Xvars = dat[,xvars]
  dat = cbind(Y,Tr,Xvars)
  
  ## Frequencies and proportions (one-sample test) for all HEAP variables vs outcome var
  prop = prop_freq(Tr,Y)
  ##
  compVars = unique(dat$HEAP)
  baseVar = '6.year 11 and below'
  compVars =compVars[!(compVars %in% '6.year 11 and below')]
  outList = list()
  for(i in 1: length(compVars)) { 
    modDat = dat %>% filter(HEAP == !!compVars[i] | HEAP== !!baseVar) %>% 
      mutate(HEAP_num = ifelse(HEAP == !!compVars[i],1,0))
    depn = colnames(Y)
    pscore = glm(HEAP_num ~ ., data = modDat %>% select(-HEAP), family = binomial() )
    modDat$Pscores = pscore$fitted.values
    matchMod = Match(Y = modDat %>% select(!!depn) %>% pull, Tr = modDat$HEAP_num, X = modDat$Pscores, replace =FALSE)
    matchedData = modDat[unlist(matchMod[c('index.treated','index.control')]),]
    
    X = colnames(modDat)
    rem = c(depn,'HEAP','Pscores')
    X = X[!X %in% rem]
    f = as.formula(paste(depn,'~', paste(X,collapse = '+') ))
    preMatch = glm(f, data = modDat, family = binomial() )
    postMatch = glm(f, data = matchedData, family = binomial() )
    
    outList[[i]] = list(DataPreMatch = modDat, preMatch = preMatch, postMatch = postMatch , matchMod =  matchMod, matchedData = matchedData)
    names(outList)[i] = compVars[i]
  }
  return(list(prop = prop, outcomeVar = outcome ,models = outList))
  
} 

############## help fun
prop_freq = function(Tr, Y) {
  freq = table(Tr %>% pull, Y %>% pull)
  prop = prop.table(table(Tr %>% pull,Y %>% pull),1)
  out = list()
  for(i in 1: nrow(prop)) {
    #i = 1 
    proptest = prop.test(freq[i,2],sum(freq[i,]))
    X_squared = proptest$statistic
    p_value = proptest$p.value
    conf = cbind(lwConf = proptest$conf.int[1],upConf = proptest$conf.int[2])
    out[[i]] = data.frame(X_squared, p_value, conf )
  }
  prop = data.frame(prop) %>% spread(Var2,Freq) %>% bind_cols(bind_rows(out)) %>% select(-Var1)
  prop = data.frame(freq) %>% spread(Var2,Freq) %>% bind_cols(bind_rows(prop))
  colnames(prop)[1] = 'HEAP'
  prop
  
}

#++++++++++++++++++++++++
# Helper function to add titles
#++++++++++++++++++++++++
# - sheet : sheet object to contain the title
# - rowIndex : numeric value indicating the row to 
#contain the title
# - title : the text to use as title
# - titleStyle : style object to use for title
xlsx.addTitle<-function(sheet, rowIndex,colIndex =1, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=colIndex)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}

########### end help fun


############# write to excel

exportToexcel = function(outputList){
  dd = outputList
  depn = dd$outcomeVar
  wb = createWorkbook(type="xlsx")

# Define some cell styles
# Title and sub title styles
TITLE_STYLE = CellStyle(wb)+ Font(wb,  heightInPoints=16, color="blue", isBold=TRUE, underline=1)
SUB_TITLE_STYLE = CellStyle(wb) + Font(wb,  heightInPoints=14, isItalic=TRUE, isBold=FALSE)
# Styles for the data table row/column names
TABLE_ROWNAMES_STYLE = CellStyle(wb) + Font(wb, isBold=TRUE)
TABLE_COLNAMES_STYLE = CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
  Border(color="black", position=c("TOP", "BOTTOM"), 
         pen=c("BORDER_THIN", "BORDER_THICK")) 

### create sheets in the workbook
# overall sheet
overallSheet = createSheet(wb, sheetName = "Overall Descriptive")
psmSheet = createSheet(wb, sheetName = "PSM results")
diagnostic = createSheet(wb, sheetName = "Assumption and Diagnostics")
oddRatio = createSheet(wb, sheetName = "Odd Ratio")

## title 
xlsx.addTitle(overallSheet, rowIndex=1, title="Overall Descriptive and test", titleStyle = TITLE_STYLE)
xlsx.addTitle(psmSheet, rowIndex=1, title="Models output", titleStyle = TITLE_STYLE)
xlsx.addTitle(diagnostic, rowIndex=1, title="Say somthing", titleStyle = TITLE_STYLE)
xlsx.addTitle(oddRatio, rowIndex=1, title="Say somthing", titleStyle = TITLE_STYLE)

# Add sub title
xlsx.addTitle(overallSheet, rowIndex=2, title="Frequencies, Proportions and statistic tests",titleStyle = SUB_TITLE_STYLE)
#xlsx.addTitle(psmSheet, rowIndex=2, title="Raw Dataset .",titleStyle = SUB_TITLE_STYLE)
# xlsx.addTitle(oddRatio, rowIndex=2, title="Raw Dataset .",titleStyle = SUB_TITLE_STYLE)

# Add the output
# sheet overall
addDataFrame(dd$prop, overallSheet, startRow=3, startColumn=1, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F)

#sheet psm
ps1 = data.frame(tidy(dd$models[[1]]$preMatch))
ps2 = data.frame(tidy(dd$models[[1]]$postMatch))
conf = data.frame(exp(cbind(OR = coef(dd$models[[1]]$postMatch), confint.default(dd$models[[1]]$postMatch, level = 0.95))))
names(conf)[2:3] = c('2.5%','97.5%') 


xlsx.addTitle(psmSheet, rowIndex=3, title= names(out$models)[1],titleStyle = TITLE_STYLE)

#xlsx.addTitle(psmSheet, rowIndex=4, colIndex = 1, title="Pre-Match model",titleStyle = SUB_TITLE_STYLE)

addDataFrame(data.frame(v = c('Pre-Match model') ), psmSheet, startRow=4, startColumn=1, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F, col.names = F)
addDataFrame(ps1, psmSheet, startRow=6, startColumn=1, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F)

#xlsx.addTitle(psmSheet, rowIndex=4, colIndex = 8, title="Post-Match model",titleStyle = SUB_TITLE_STYLE)
addDataFrame(data.frame(v = c('Post-Match model') ), psmSheet, startRow=4, startColumn=8, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F, col.names = F)
addDataFrame(ps2, psmSheet, startRow=6, startColumn=8, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F)

#xlsx.addTitle(psmSheet, rowIndex=4, colIndex = 15, title="Post-Match Odds Ratio ",titleStyle = SUB_TITLE_STYLE)
addDataFrame(data.frame(v = c('Post-Match Odds Ratio') ), psmSheet, startRow=4, startColumn=15, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F, col.names = F)
addDataFrame(conf, psmSheet, startRow=6, startColumn=15, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=T)

HEAP = c(dd$models[[1]]$matchedData$HEAP_num)
outcomeVar = dd$models[[1]]$matchedData %>% select(!!depn) %>% pull
propTest = prop.test(table(HEAP,outcomeVar))
x_squared = propTest$statistic
pValue = propTest$p.value
confL = propTest$conf.int[1]
confU = propTest$conf.int[2]

propTest = data.frame(v = c(x_squared,pValue,confL,confU))
row.names(propTest) = c('X squared','P-value','Lower-CI (95%)', 'Upper-CI (95%)' )

#xlsx.addTitle(psmSheet, rowIndex=4, colIndex = 21, title="Post-Match Chi squared ",titleStyle = SUB_TITLE_STYLE)
addDataFrame(data.frame(v = c('Post-Match Chi squared') ), psmSheet, startRow=4, startColumn=21, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F, col.names = F)
addDataFrame(propTest, psmSheet, startRow=6, startColumn=21, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=T,col.names=F)

#sheet Diagnistic
oc = dd$models[[1]]$DataPreMatch %>% select(!!depn) %>% pull
treat = oc[oc==1] 
contr = oc[oc==0] 
socres = dd$models[[1]]$DataPreMatch %>% select(Pscores) %>% pull
breaks =  seq(min(socres),max(socres),length.out = 15)
png("boxplot.png", height=800, width=800, res=250, pointsize=8)

out = histbackback(treat, contr, probability=TRUE, main = 'Area of common support') #, brks = breaks
barplot(-out$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
dev.off()
# Add the plot 
addPicture("boxplot.png", diagnostic, scale = 1, startRow = 4, startColumn = 5)
# Remove the plot from the disk
res = file.remove("boxplot.png")

# match diag
Original_num_obs = dd$models[[1]]$matchMod$est
match_num_obs =  nrow(dd$models[[1]]$matchedData) 
Estimate = dd$models[[1]]$matchMod$est
SE = dd$models[[1]]$matchMod$se.standard
t_stat = dd$models[[1]]$matchMod$orig.nobs
matchDiag = data.frame(v = c(Original_num_obs,match_num_obs,Estimate,SE,t_stat))
rownames(matchDiag) = c('Original number of observations', 'Matched number of observations', 'Estimate','SE','t-stat')
addDataFrame(matchDiag, diagnostic, startRow=3, startColumn=16, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=T,col.names = F)  

# loop through
n = nrow(tidy(dd$models[[1]]$preMatch)) +6
n0 = 0
m = 16 +3
m0 = 0
for(i in 2 : length(dd$models)) { #3
  n = n + n0
  m = m + m0
  ps = data.frame(tidy(dd$models[[i]]$preMatch))
  ps2 = data.frame(tidy(dd$models[[i]]$postMatch))
  conf1 = data.frame(exp(cbind(OR = coef(dd$models[[i]]$postMatch), confint.default(dd$models[[i]]$postMatch, level = 0.95))))
  names(conf1)[2:3] = c('2.5%','97.5%')
  
  oc = dd$models[[i]]$DataPreMatch %>% select(!!depn) %>% pull
  treat = oc[oc==1] 
  contr = oc[oc==0] 
  socres = dd$models[[i]]$DataPreMatch %>% select(Pscores) %>% pull
  breaks =  seq(min(socres),max(socres),length.out = 15)
  
  png("boxplot.png", height=800, width=800, res=250, pointsize=8)
  out = histbackback(treat, contr, probability=TRUE, main = 'Area of common support') #, brks = breaks
  barplot(-out$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
  barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
  dev.off()
  
  xlsx.addTitle(psmSheet, rowIndex=n-2, title= names(dd$models)[i],titleStyle = TITLE_STYLE)
  #xlsx.addTitle(psmSheet, rowIndex = n-1, colIndex = 1, title="Pre-Match model",titleStyle = SUB_TITLE_STYLE)
  addDataFrame(data.frame(v = c('Pre-Match model') ), psmSheet, startRow=n-1, startColumn=1, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F, col.names = F)
  addDataFrame(ps, psmSheet, startRow= n , startColumn=1, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F)
  
  #xlsx.addTitle(psmSheet, rowIndex = n-1, colIndex = 8, title="Post-Match model",titleStyle = SUB_TITLE_STYLE)
  addDataFrame(data.frame(v = c('Post-Match model') ), psmSheet, startRow=n-1, startColumn=8, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F, col.names = F)
  addDataFrame(ps2, psmSheet, startRow= n , startColumn=8, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F)
  
  #xlsx.addTitle(psmSheet, rowIndex = n-1, colIndex = 15, title="Post-Match Odds Ratio",titleStyle = SUB_TITLE_STYLE)
  addDataFrame(data.frame(v = c('Post-Match Odds Ratio') ), psmSheet, startRow=n-1, startColumn=15, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F, col.names = F)
  addDataFrame(conf1, psmSheet, startRow=n, startColumn=15, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=T)
  
  xlsx.addTitle(diagnostic, rowIndex=m-2, title= names(dd$models)[i],titleStyle = TITLE_STYLE)
  addPicture("boxplot.png", diagnostic, scale = 1, startRow = m, startColumn = 5)
  res = file.remove("boxplot.png")
  
  Original_num_obs = dd$models[[i]]$matchMod$est
  match_num_obs =  nrow(dd$models[[i]]$matchedData) 
  Estimate = dd$models[[i]]$matchMod$est
  SE = dd$models[[i]]$matchMod$se.standard
  t_stat = dd$models[[i]]$matchMod$orig.nobs
  
  matchDiag = data.frame(v = c(Original_num_obs,match_num_obs,Estimate,SE,t_stat))
  rownames(matchDiag) = c('Original number of observations', 'Matched number of observations', 'Estimate','SE','t-stat')
  
  addDataFrame(matchDiag, diagnostic, startRow=m, startColumn=16, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=T,col.names = F)  
  
  HEAP = c(dd$models[[i]]$matchedData$HEAP_num)
  outcomeVar = dd$models[[i]]$matchedData %>% select(!!depn) %>% pull
  propTest = prop.test(table(HEAP,outcomeVar))
  x_squared = propTest$statistic
  pValue = propTest$p.value
  confL = propTest$conf.int[1]
  confU = propTest$conf.int[2]
  
  propTest = data.frame(v = c(x_squared,pValue,confL,confU))
  row.names(propTest) = c('X squared','P-value','Lower-CI (95%)', 'Upper-CI (95%)' )
  
  #xlsx.addTitle(psmSheet, rowIndex=n-1, colIndex = 21, title="Post-Match Chi squared ",titleStyle = SUB_TITLE_STYLE)
  addDataFrame(data.frame(v = c('Post-Match Chi squared') ), psmSheet, startRow=n-1, startColumn=21, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=F, col.names = F)
  addDataFrame(propTest, psmSheet, startRow=n, startColumn=21, colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle = TABLE_ROWNAMES_STYLE,row.names=T,col.names=F)
  
  n0 = n -3
  m0 = m -3
  
}
# save the workbook
saveWorkbook(wb, "PSM output.xlsx")
}


