#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
# Author: Samuel Shamiri
# Date: 13/10/2018
# Version: 0.2
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

exportToexcel = function(outputList, fileName = 'psm_output'){
  pkg = c('Matching','broom','xlsx','Hmisc','tidyverse')
  lapply(pkg, require, character.only = TRUE)
  
  dd = outputList
  depn = dd$outcomeVar
  now = Sys.time()
  fileName = paste0(format(now,"%Y%m%d_%H%M%S_"),fileName,'.xlsx')
  
  wb = createWorkbook(type="xlsx")
  ### create sheets in the workbook
  cont = createSheet(wb, sheetName = "Content")
  overallSheet = createSheet(wb, sheetName = "Descriptives")
  psmSheet = createSheet(wb, sheetName = "PSM results")
  diagnostic = createSheet(wb, sheetName = "Assumption and match results")
  oddRatio = createSheet(wb, sheetName = "Odds ratio")
  
  ### Add data to sheets
  # content sheet
  xlsx.addTable(wb, cont, data.frame(v = c('Workbook') ),fontColor="darkblue", fontSize=14,startRow = 1 ,
                startCol=1,row.names =F,col.names = F)
  xlsx.addTable(wb, cont, data.frame(v = c('Test variable') ),fontColor="darkblue", fontSize=14,startRow =2 ,
                startCol=1,row.names =F,col.names = F)
  xlsx.addTable(wb, cont, data.frame(v = c('Sheetname') ), fontSize=14,startRow =5 ,
                startCol=1,row.names =F,col.names = F)
  xlsx.addTable(wb, cont, data.frame(v = c('Description') ), fontSize=14,startRow =5 ,
                startCol=2,row.names =F,col.names = F)
  xlsx.addTable(wb, cont, data.frame(v = c('Descriptive') ), fontSize=12,startRow =6 ,
                startCol=1,row.names =F,col.names = F) 
  xlsx.addTable(wb, cont, data.frame(v = c('PSM results') ), fontSize=12,startRow =7 ,
                startCol=1,row.names =F,col.names = F)
  xlsx.addTable(wb, cont, data.frame(v = c('Assumption and match results') ), fontSize=12,startRow =8 ,
                startCol=1,row.names =F,col.names = F)
  xlsx.addTable(wb, cont, data.frame(v = c('Odds ratio') ), fontSize=12,startRow =9 ,
                startCol=1,row.names =F,col.names = F)
  # add prog
  pg1 = 'This is the analysis for ....'
  pg2 = depn
  pg3 = 'This table shows basic frequencies and proportions.'
  pg4 = 'These tables show the coefficients, odds ratio and tests for pre-PSM and post-PSM'
  pg5 = 'This examin the common support for treatment and control and the match results from PSM'
  pg6 = 'this show the odds ratio for the treatment variable (HEAP) post-PSM'
  
  xlsx.addTable(wb, cont, data.frame(v = pg1), fontSize=12,startRow =1,startCol=2,row.names =F,col.names = F) 
  xlsx.addTable(wb, cont, data.frame(v = pg2), fontSize=12,startRow =2,startCol=2,row.names =F,col.names = F)
  xlsx.addTable(wb, cont, data.frame(v = pg3), fontSize=12,startRow =6,startCol=2,row.names =F,col.names = F)
  xlsx.addTable(wb, cont, data.frame(v = pg4), fontSize=12,startRow =7,startCol=2,row.names =F,col.names = F)
  xlsx.addTable(wb, cont, data.frame(v = pg5), fontSize=12,startRow =8,startCol=2,row.names =F,col.names = F)
  xlsx.addTable(wb, cont, data.frame(v = pg6), fontSize=12,startRow =9,startCol=2,row.names =F,col.names = F)
  
  # sheet overall
  xlsx.addHeader(wb, overallSheet, value="Descriptives and tests",level=2, startRow = 1, 
                 startCol =1,color="black", underline=2)
  xlsx.addHeader(wb, overallSheet, value="Frequencies and one sample proportion test",level=4, 
                 startRow = 2, startCol =1,color="black", underline=0)
  xlsx.addLineBreak(overallSheet, 1)
  prpTbl = outTest$prop
  names(prpTbl)[2:5] = c('control_freq','treatment_freq','control_prop','treatment_prop')
  xlsx.addTable(wb, overallSheet, prpTbl,fontColor="darkblue", fontSize=12,rowFill=c("white", "lightblue"),row.names=F)
  
  #sheet psm
  xlsx.addHeader(wb, psmSheet, value="PSM models output",level=2, color="black", underline=0,startRow = 1 ,startCol =1)
  xlsx.addHeader(wb, psmSheet, value=names(dd$models)[1] ,level=3, color="black", underline=2,startRow = 3 ,startCol =1)
  # header as table
  xlsx.addTable(wb, psmSheet, data.frame(v = c('Pre-Match model') ),fontColor="darkblue", fontSize=14,
                rowFill=c("white", "lightblue"),row.names=F,startRow = 5 ,startCol=1,col.names = F)
  xlsx.addTable(wb, psmSheet, data.frame(v = c('Post-Match model') ),fontColor="darkblue", fontSize=14,
                rowFill=c("white", "lightblue"),startRow = 5 ,startCol=8,row.names =F,col.names = F)
  xlsx.addTable(wb, psmSheet, data.frame(v = c('Post-Match odds ratio') ),fontColor="darkblue", fontSize=14,
                rowFill=c("white", "lightblue"),startRow = 5 ,startCol=15,row.names =F,col.names = F)
  xlsx.addTable(wb, psmSheet, data.frame(v = c('Post-Match 2 sample proption test') ),fontColor="darkblue", fontSize=14,
                rowFill=c("white", "lightblue"),startRow = 5 ,startCol=21,row.names =F,col.names = F)
  
  pre_psm = data.frame(tidy(dd$models[[1]]$preMatch))
  post_psm = data.frame(tidy(dd$models[[1]]$postMatch))
  conf = data.frame(exp(cbind(OR = coef(dd$models[[1]]$postMatch), confint.default(dd$models[[1]]$postMatch, level = 0.95))))
  names(conf)[2:3] = c('2.5%','97.5%') 
  or = list()
  or[[1]] = data.frame(HEAP = names(outTest$models)[1],conf %>% filter(row.names(conf) %in% c('HEAP_num')),stringsAsFactors = F)
  
  xlsx.addTable(wb, psmSheet, pre_psm,fontColor="darkblue", fontSize=12,rowFill=c("white", "lightblue"),row.names=F,startRow = 7 ,startCol =1)
  xlsx.addTable(wb, psmSheet, post_psm,fontColor="darkblue", fontSize=12,rowFill=c("white", "lightblue"),row.names=F,startRow = 7 ,startCol=8)
  xlsx.addTable(wb, psmSheet, conf,fontColor="darkblue", fontSize=12,rowFill=c("white", "lightblue"),row.names=F,startRow = 7,startCol=15)

  HEAP = c(dd$models[[1]]$matchedData$HEAP_num)
  outcomeVar = dd$models[[1]]$matchedData %>% select(!!depn) %>% pull
  propTest = prop.test(table(HEAP,outcomeVar))
  x_squared = propTest$statistic
  pValue = propTest$p.value
  confL = propTest$conf.int[1]
  confU = propTest$conf.int[2]
  propTest = data.frame(value = c(x_squared,pValue,confL,confU))
  row.names(propTest) = c('X squared','P-value','Lower-CI (95%)', 'Upper-CI (95%)' )
  
  xlsx.addTable(wb, psmSheet, conf,fontColor="darkblue", fontSize=12,rowFill=c("white", "lightblue"),row.names=T,startRow = 7 ,startCol=15)
  xlsx.addTable(wb, psmSheet, propTest,fontColor="darkblue", fontSize=12,rowFill=c("white", "lightblue"),row.names=T,startRow = 7 ,startCol=21)
 
   #sheet Diagnistic
  xlsx.addHeader(wb, diagnostic, value=" Examining the region of common support & match results",level=4, startRow = 1, 
                 startCol =1,color="black", underline=0)
  xlsx.addHeader(wb, diagnostic, value=names(dd$models)[1] ,level=3, color="black", underline=2,startRow = 3 ,startCol =1) 
 
   xlsx.addTable(wb, diagnostic, data.frame(v = c('Common support') ),fontColor="darkblue", fontSize=14,
                rowFill=c("white", "lightblue"),row.names=F,startRow = 5 ,startCol=1,col.names = F)
   xlsx.addTable(wb, diagnostic, data.frame(v = c('Match results') ),fontColor="darkblue", fontSize=14,
                 rowFill=c("white", "lightblue"),row.names=F,startRow = 5 ,startCol=12,col.names = F)
   
  # Add the plot 
   oc = dd$models[[1]]$DataPreMatch %>% select(!!depn) %>% pull
   treatment = oc[oc==1] 
   control = oc[oc==0] 
   socres = dd$models[[1]]$DataPreMatch %>% select(Pscores) %>% pull
   breaks =  seq(min(socres),max(socres),length.out = 15)
   png("psmPlot.png", height=800, width=800, res=250, pointsize=8)
   out = histbackback(treatment, control, probability=TRUE, main = 'Area of common support') #, brks = breaks
   barplot(-out$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
   barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
   dev.off()
   # Add the plot 
   addPicture("psmPlot.png", diagnostic, scale = 1, startRow = 7, startColumn = 3)
   # Remove the plot from the disk
   res = file.remove("psmPlot.png")
  # match diag
   Original_num_obs =  nrow(dd$models[[1]]$matchedData)
   match_num_obs =  dd$models[[1]]$matchMod$orig.nobs 
   Estimate = dd$models[[1]]$matchMod$est
   SE = dd$models[[1]]$matchMod$se.standard
   matchDiag = data.frame(value = c(Original_num_obs,match_num_obs,Estimate,SE))
   rownames(matchDiag) = c('Original number of observations', 'Matched number of observations', 'Estimate','SE')
   xlsx.addTable(wb, diagnostic, matchDiag,fontColor="darkblue", fontSize=12,rowFill=c("white", "lightblue"),startRow = 7 ,startCol=12)

   #sheet Odd ratio sheet
   xlsx.addHeader(wb, oddRatio, value="Odds ratio for treatment variable",level=2, startRow = 1, 
                  startCol =1,color="black", underline=0)
   
  # loop through
  N = nrow(tidy(dd$models[[1]]$preMatch)) +7
  n0 = 0
  M = 21
  m0 = 0
  h = 3
  p = 5
  t = 7
  for(i in 2 : length(dd$models)) { #3
    n = N + n0
    m = M + m0
    pre_psm = data.frame(tidy(dd$models[[i]]$preMatch))
    post_psm = data.frame(tidy(dd$models[[i]]$postMatch))
    conf1 = data.frame(exp(cbind(OR = coef(dd$models[[i]]$postMatch), confint.default(dd$models[[i]]$postMatch, level = 0.95))))
    names(conf1)[2:3] = c('2.5%','97.5%')
    or[[i]] = data.frame(HEAP = names(outTest$models)[i],conf1 %>% filter(row.names(conf) %in% c('HEAP_num')),stringsAsFactors = F)
    
    Original_num_obs =  nrow(dd$models[[i]]$matchedData)
    match_num_obs =  dd$models[[i]]$matchMod$orig.nobs 
    Estimate = dd$models[[i]]$matchMod$est
    SE = dd$models[[i]]$matchMod$se.standard
    matchDiag = data.frame(value = c(Original_num_obs,match_num_obs,Estimate,SE))
    rownames(matchDiag) = c('Original number of observations', 'Matched number of observations', 'Estimate','SE')
    
    HEAP = c(dd$models[[i]]$matchedData$HEAP_num)
    outcomeVar = dd$models[[i]]$matchedData %>% select(!!depn) %>% pull
    propTest = prop.test(table(HEAP,outcomeVar))
    x_squared = propTest$statistic
    pValue = propTest$p.value
    confL = propTest$conf.int[1]
    confU = propTest$conf.int[2]
    propTest = data.frame(value = c(x_squared,pValue,confL,confU))
    row.names(propTest) = c('X squared','P-value','Lower-CI (95%)', 'Upper-CI (95%)' )
    
    oc = dd$models[[i]]$DataPreMatch %>% select(!!depn) %>% pull
    treatment = oc[oc==1]
    control = oc[oc==0]
    socres = dd$models[[i]]$DataPreMatch %>% select(Pscores) %>% pull
    breaks =  seq(min(socres),max(socres),length.out = 15)
    png("psmPlot.png", height=800, width=800, res=250, pointsize=8)
    out = histbackback(treatment, control, probability=TRUE, main = 'Area of common support') #, brks = breaks
    barplot(-out$left, col="red" , horiz=TRUE, space=0, add=TRUE, axes=FALSE)
    barplot(out$right, col="blue", horiz=TRUE, space=0, add=TRUE, axes=FALSE)
    dev.off()
    

    xlsx.addHeader(wb, psmSheet, value=names(dd$models)[i] ,level=3, color="black", underline=2,
                   startRow = n + h ,startCol =1)
     xlsx.addTable(wb, psmSheet, data.frame(v = c('Pre-Match model') ),fontColor="darkblue", fontSize=14,
                  rowFill=c("white", "lightblue"),row.names=F,startRow = n + p ,startCol=1,col.names = F)
    xlsx.addTable(wb, psmSheet, data.frame(v = c('Post-Match model') ),fontColor="darkblue", fontSize=14,
                   rowFill=c("white", "lightblue"),startRow = n + p ,startCol=8,row.names =F,col.names = F)
    xlsx.addTable(wb, psmSheet, data.frame(v = c('Post-Match odds ratio') ),fontColor="darkblue", fontSize=14,
                   rowFill=c("white", "lightblue"),startRow = n + p ,startCol=15,row.names =F,col.names = F)
    xlsx.addTable(wb, psmSheet, data.frame(v = c('Post-Match 2 sample proption test') ),fontColor="darkblue", fontSize=14,
                   rowFill=c("white", "lightblue"),startRow = n + p ,startCol=21,row.names =F,col.names = F)
  #   
    xlsx.addTable(wb, psmSheet, pre_psm,fontColor="darkblue", fontSize=12,rowFill=c("white", "lightblue"),row.names=F,startRow = n+t ,startCol =1)
    xlsx.addTable(wb, psmSheet, post_psm,fontColor="darkblue", fontSize=12,rowFill=c("white", "lightblue"),row.names=F,startRow = n+t ,startCol=8)
    xlsx.addTable(wb, psmSheet, conf1,fontColor="darkblue", fontSize=12,rowFill=c("white", "lightblue"),row.names=T,startRow = n+t,startCol=15) 
    xlsx.addTable(wb, psmSheet, propTest,fontColor="darkblue", fontSize=12,rowFill=c("white", "lightblue"),row.names=T,startRow = n+t,startCol=21) 
    

    xlsx.addHeader(wb, diagnostic, value=names(dd$models)[i] ,level=3, color="black", underline=2,
                   startRow = m + h ,startCol =1)
    xlsx.addTable(wb, diagnostic, data.frame(v = c('Common support') ),fontColor="darkblue", fontSize=14,
                  rowFill=c("white", "lightblue"),row.names=F,startRow = m + p ,startCol=1,col.names = F)
    xlsx.addTable(wb, diagnostic, data.frame(v = c('Match results') ),fontColor="darkblue", fontSize=14,
                  rowFill=c("white", "lightblue"),row.names=F,startRow = m + p ,startCol=12,col.names = F)
    
    addPicture("psmPlot.png", diagnostic, scale = 1, startRow = m + t, startColumn = 3)
    xlsx.addTable(wb, diagnostic, matchDiag,fontColor="darkblue", fontSize=12,rowFill=c("white", "lightblue"),
                  startRow = m + t ,startCol=12)
    res = file.remove("psmPlot.png")
    n0 = n 
    m0 = m 
    
  }

  or = bind_rows(or)
  names(or)[3:4] = c('2.5%','97.5%') 
  xlsx.addTable(wb, oddRatio, or ,fontColor="darkblue", fontSize=12,
                rowFill=c("white", "lightblue"),row.names=F,startRow = 3 ,startCol=1,col.names = T)
  # save the workbook
  saveWorkbook(wb, fileName)
  cat('\n','#++++++THE RESULTS ARE SAVED in ++++++++++#','\n')
  cat('\n',paste0(getwd(),'/',fileName),'\n')
  cat('\n','#++++++++++++++++++++++++++++++++++++++++++#','\n')
}

### example
# outTest = PSM(Treat = 'HEAP', outcome = 'CONNAT',xvars = c('DSEIFAA','WKLYINCQ','Age'), baseVar = '6.year 11 and below' ,data = dat,
#               matchPar = list(estimand = "ATT", M = 1,ties=TRUE,caliper = NULL, replace=FALSE))
# exportToexcel(outTest,fileName = 'psm_output')


#### excel help functions
xlsx.addHeader<-function(wb, sheet, value="Header", level=1, color="#FFFFFF",
                         startRow=NULL, startCol=2, underline=c(0,1,2))
{
  library("xlsx")
  
  if(color=="black") color="white"# black and white color are inversed in xlsx package. don't know why
  # Define some cell styles within that workbook
  H1_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=22,color=color, isBold=TRUE, underline=underline[1])
  H2_STYLE <- CellStyle(wb) + Font(wb, heightInPoints=18, color=color, isItalic=FALSE, isBold=TRUE, underline=underline[1])
  H3_STYLE <- CellStyle(wb) + Font(wb, heightInPoints=16, color=color, isItalic=TRUE, isBold=TRUE, underline=underline[1])
  H4_STYLE <- CellStyle(wb) + Font(wb, heightInPoints=16, color=color, isItalic=TRUE, isBold=FALSE, underline=underline[1])
  H5_STYLE <- CellStyle(wb) + Font(wb, heightInPoints=14, color=color, isItalic=TRUE, isBold=FALSE, underline=underline[1])
  H6_STYLE <- CellStyle(wb) + Font(wb, heightInPoints=12, color=color, isItalic=TRUE, isBold=FALSE, underline=underline[1])
  
  #Append row to sheet
  if(is.null(startRow)){
    rows<- getRows(sheet) #list of row object
    startRow=length(rows)+1  
  } 
  
  # Create the Sheet title and subtitle
  rows <- createRow(sheet,rowIndex=startRow)
  sheetTitle <- createCell(rows, colIndex=startCol)
  setCellValue(sheetTitle[[1,1]], value)
  if(level==1) xlsx::setCellStyle(sheetTitle[[1,1]], H1_STYLE)
  else if(level==2) xlsx::setCellStyle(sheetTitle[[1,1]], H2_STYLE)
  else if(level==3) xlsx::setCellStyle(sheetTitle[[1,1]], H3_STYLE)
  else if(level==4) xlsx::setCellStyle(sheetTitle[[1,1]], H4_STYLE)
  else if(level==5) xlsx::setCellStyle(sheetTitle[[1,1]], H5_STYLE)
  else if(level==6) xlsx::setCellStyle(sheetTitle[[1,1]], H6_STYLE)  
}

xlsx.addParagraph<-function(wb,sheet, value, fontColor="#FFFFFF", fontSize=12, backGroundColor="#FFFFFF",
                            isBold=FALSE, isItalic=FALSE,
                            startRow=NULL, startCol=2, colSpan=10, rowSpan=5)
{
  library("xlsx") 
  #Append table to sheet
  if(is.null(startRow)){
    rows<- getRows(sheet) #list of row object
    startRow=length(rows)+1
  }
  rows <- createRow(sheet,rowIndex=startRow)
  sheetParagraph <- createCell(rows, colIndex=startCol)
  setCellValue(sheetParagraph[[1,1]], value)
  #style
  PARAGRAPH_STYLE <- CellStyle(wb)+ 
    Font(wb,  heightInPoints=fontSize,color=fontColor, isBold=isBold, isItalic=isItalic)+                          
    Alignment(wrapText=TRUE, horizontal="ALIGN_JUSTIFY", 
              vertical="VERTICAL_CENTER")
  #background fill
  if(!backGroundColor %in% c("white", "#FFFFFF")) 
    PARAGRAPH_STYLE+Fill(backgroundColor=backGroundColor, foregroundColor=backGroundColor) 
  xlsx::setCellStyle(sheetParagraph[[1,1]], PARAGRAPH_STYLE)
  #Spanning region : -1, because we start to count from zero. 
  #if not, an additionnal row or column are added to merged region
  addMergedRegion(sheet, startRow, endRow=startRow+rowSpan-1, startCol, endColumn=startCol+colSpan-1) 
  xlsx.addLineBreak(sheet, rowSpan) 
}

xlsx.addHyperlink<-function(wb,sheet, address, friendlyName, 
                            fontColor="blue", fontSize=12,
                            isBold=FALSE, isItalic=FALSE, startRow=NULL, startCol=2)                      
  
{
  library("xlsx") 
  #Append table to sheet
  if(is.null(startRow)){
    rows<- getRows(sheet) #list of row object
    startRow=length(rows)+1
  }
  rows <- createRow(sheet,rowIndex=startRow)
  linkCell <- createCell(rows, colIndex=startCol)
  setCellValue(linkCell[[1,1]], friendlyName)
  addHyperlink(linkCell[[1,1]], address)
  
  #style
  HYPERLINK_STYLE <- CellStyle(wb)+ Font(wb,  heightInPoints=fontSize,color=fontColor, isBold=isBold, isItalic=isItalic)+                          
    Alignment(wrapText=FALSE, horizontal="ALIGN_JUSTIFY")      
  xlsx::setCellStyle(linkCell[[1,1]], HYPERLINK_STYLE)
}

xlsx.addLineBreak<-function(sheet, numberOfLine=1)
{
  library("xlsx")
  
  nrows<-length(getRows(sheet)) #list of row object
  startRow=nrows
  for(i in 1:numberOfLine){
    #Append row to sheet
    startRow=startRow+1
    # Create the Sheet title and subtitle
    rows <- createRow(sheet,rowIndex=startRow)
    sheetLineBreak <- createCell(rows, colIndex=1)
    setCellValue(sheetLineBreak[[1,1]], "  ") 
  }
}


xlsx.addTable<-function(wb, sheet, data, startRow=NULL,startCol=2,
                        col.names=TRUE, row.names=TRUE, columnWidth=14,
                        fontColor="#FFFFFF", fontSize=12, 
                        rownamesFill="white", colnamesFill="white", 
                        rowFill=c("white", "white")){
  
  library("xlsx")
  #++++++++++++++++++++++++++++++++++++++
  #Define table style
  #++++++++++++++++++++++++++++++++++++++
  #***Border position and pen value*****
  #Border(color="black", position="BOTTOM", pen="BORDER_THIN"
  #position :  Valid values are "BOTTOM", "LEFT", "TOP", "RIGHT"
  # pen : valid values are BORDER_DASH_DOT,BORDER_DASH_DOT_DOT,BORDER_DASHED,BORDER_DOTTED,BORDER_DOUBLE,BORDER_HAIR,BORDER_MEDIUM,BORDER_MEDIUM_DASH_DOT,BORDER_MEDIUM_DASH_DOT_DOT,BORDER_MEDIUM_DASHED,BORDER_NONE,BORDER_SLANTED_DASH_DOT,BORDER_THICK,BORDER_THIN
  #***Alignement value*****
  #Alignment(horizontal=NULL, vertical=NULL, wrapText=FALSE, rotation=0, indent=0)
  #HALIGN_STYLES_: "ALIGN_CENTER, ALIGN_JUSTIFY, ALIGN_LEFT, ALIGN_RIGHT"
  #VALIGN_STYLES_: "VERTICAL_BOTTOM, VERTICAL_CENTER, VERTICAL_JUSTIFY, VERTICAL_TOP"
  #Alignement :
  TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE, color=fontColor, 
                                               heightInPoints=fontSize)
  #rownames fill 
  if(rownamesFill!="white") {
    TABLE_ROWNAMES_STYLE <-TABLE_ROWNAMES_STYLE+
      Fill(foregroundColor = rownamesFill, 
           backgroundColor=rownamesFill)
  }
  
  
  TABLE_COLNAMES_STYLE <- CellStyle(wb) + 
    Font(wb, isBold=TRUE, color=fontColor, heightInPoints=fontSize) +
    Alignment(wrapText=TRUE, horizontal="ALIGN_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"), 
           pen=c("BORDER_THIN", "BORDER_THICK"))
  #colnames fill
  if(colnamesFill!="white") {
    TABLE_COLNAMES_STYLE <-TABLE_COLNAMES_STYLE+
      Fill(foregroundColor = colnamesFill,
           backgroundColor=colnamesFill)
  }
  
  #Append table to sheet
  #get current active row of sheet
  if(is.null(startRow)){
    rows<- getRows(sheet) #list of row object
    startRow=length(rows)+1
  } 
  
  #font color
  col.n=ncol(data)
  column.style=NULL
  for(i in 1:col.n){
    column.style[[i]]=CellStyle(wb, font=Font(wb, color=fontColor, heightInPoints=fontSize))
  }
  names(column.style)<-as.character(1:ncol(data))
  
  # Add the table  to the sheet
  addDataFrame(data, sheet, startRow=startRow, startColumn=startCol,
               col.names=col.names, row.names=row.names,
               colnamesStyle = TABLE_COLNAMES_STYLE, rownamesStyle=TABLE_ROWNAMES_STYLE,
               colStyle=column.style) 
  #Column width
  #+++++++++++++++++++++++++++++++++++++++
  colIndex=1:(ncol(data)+startCol)
  xlsx::setColumnWidth(sheet, colIndex=colIndex, colWidth=columnWidth)
  
  #Table styling
  #+++++++++++++++++++++++++++++++++++++++
  if(!all(rowFill==c("white", "white"))){
    col.n =ncol(data)
    row.n=nrow(data)
    if(col.names==TRUE) col.n<-col.n+1
    if(row.names==TRUE) row.n<-row.n+1
    cb<-CellBlock(sheet, startRow=startRow, startColumn=startCol, 
                  noRows=row.n, noColumns=col.n, create=FALSE )
    #color pair row for styling
    for(i in 1: nrow(data)){ 
      if(i%%2==0) CB.setFill( cb, fill=Fill(foregroundColor = rowFill[2], backgroundColor=rowFill[2]),
                              rowIndex=i, colIndex=1:col.n)
      else CB.setFill( cb, fill=Fill(foregroundColor = rowFill[1], backgroundColor=rowFill[1]),
                       rowIndex=i, colIndex=1:col.n)
    }
    
  }
}


xlsx.addPlot<-function( wb, sheet, plotFunction, startRow=NULL,startCol=2,
                        width=480, height=480,... )
  
{
  library("xlsx")
  png(filename = "plot.png", width = width, height = height,...)
  plotFunction()
  dev.off() 
  #Append plot to the sheet
  if(is.null(startRow)){
    rows<- getRows(sheet) #list of row object
    startRow=length(rows)+1
  } 
  # Add the file created previously
  addPicture("plot.png", sheet=sheet,  startRow = startRow, startColumn = startCol) 
  xlsx.addLineBreak(sheet, round(width/20)+1)
  res<-file.remove("plot.png")
}

xlsx.writeFile<-function(data, file, sheetName="Sheet1",
                         col.names=TRUE, row.names=TRUE, append=FALSE, ...){
  write.xlsx2(data, file=file, sheetName=sheetName,
              col.names=col.names, row.names=row.names, append=append, ...)
}

xlsx.writeMultipleData <- function (file, ...)
{
  require(xlsx, quietly = TRUE)
  objects <- list(...)
  fargs <- as.list(match.call(expand.dots = TRUE))
  objnames <- as.character(fargs)[-c(1, 2)]
  nobjects <- length(objects)
  for (i in 1:nobjects) {
    if (i == 1)
      write.xlsx(objects[[i]], file, sheetName = objnames[i])
    else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                    append = TRUE)
  }
}

xlsx.readFile<-function(file, sheetIndex=1, startRow=1, 
                        colIndex=NULL, endRow=NULL, header=TRUE,...)
{
  library("xlsx")
  res<-read.xlsx2(file=file, sheetIndex=sheetIndex, startRow=1, colIndex=colIndex, 
                  endRow=endRow,header=header, ...)
  res          
}


getOS<-function(){ 
  OS=.Platform$OS.type
  if(OS=="unix"){
    if(Sys.info()["sysname"]=="Linux") OS="linux"
    else OS="mac"
  }
}

xlsx.openFile<-function(filename=NULL)
{  
  absolute.path=paste(getwd(), "/", filename, sep="")
  if(.Platform$OS.type=="windows"){
    shell.exec(absolute.path)
  }
  else if(.Platform$OS.type=="unix"){
    system(paste("open ", absolute.path, sep=""))
  }    
}

