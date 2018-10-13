
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #
  # Author: Samuel Shamiri
  # Date: 13/10/2018
  # Version: 0.2
  #+++++++++++++++++++++++++++++++++++++++++++++++++++++++++ #

PSM = function(Treat, outcome ,xvars ,baseVar, data, 
               matchPar = list(estimand = "ATT", M = 1,ties=TRUE,
                               caliper = NULL, replace=FALSE)) {
 pkg = c('Matching','broom','xlsx','Hmisc','tidyverse')
 lapply(pkg, require, character.only = TRUE)
 
  Y = data[,outcome]
  Tr = data[,Treat]
  Xvars = data[,xvars]
  dat = cbind(Y,Tr,Xvars)
  
  ## Frequencies and proportions (one-sample test) for all HEAP variables vs outcome var
  prop = prop_freq(Tr,Y)
  ##
  compVars = unique(dat$HEAP)
  compVars =compVars[!(compVars %in% baseVar)]
  outList = list()
  for(i in 1: length(compVars)) { 
    modDat = dat %>% filter(HEAP == !!compVars[i] | HEAP== !!baseVar) %>% 
                     mutate(HEAP_num = ifelse(HEAP == !!compVars[i],1,0))
    depn = colnames(Y)
    pscore = glm(HEAP_num ~ ., data = modDat %>% select(-HEAP), family = binomial() )
    modDat$Pscores = pscore$fitted.values
    matchMod = Match(Y = modDat %>% select(!!depn) %>% pull, Tr = modDat$HEAP_num, X = modDat$Pscores,
                     estimand = matchPar$estimand, replace =matchPar$replace, M = matchPar$M,
                     ties = matchPar$ties, caliper = matchPar$caliper)
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

########### end help fun

### example
# outTest = PSM(Treat = 'HEAP', outcome = 'CONNAT',xvars = c('DSEIFAA','WKLYINCQ','Age'), baseVar = '6.year 11 and below' ,data = dat,
#               matchPar = list(estimand = "ATT", M = 1,ties=TRUE,caliper = NULL, replace=FALSE))