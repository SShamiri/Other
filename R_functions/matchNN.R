# Author: Samuel Shamiri
# date : 19/10/2018
# purpose : propensity score matching 
# output: returns a vector of indexs that has a match
#

match_nn = function(predValues, psTreated, psUntreated, caliper = 0.2 ) {
  # find matches, require be within 20% of a sd of the unmatched ps
  matches  =  vector()
  limit  =  caliper * sd(predValues)
  for (i in 1:length(psTreated)) {
    x  =  psTreated[i]
    distance  =  (x - psUntreated)^2
    if (min(distance) <= limit) {
      nn  =  which.min(distance)
      psUntreated[nn]  =  .Machine$integer.max
      matches[i]  =  c(nn) 
    } else {
      matches[i]  =  c(NA)
    }
    
  }
  matches = matches[!is.na(matches)]
}

# compile into bytecode
require(compiler)
macthNN = cmpfun(match_nn)

## example
# predValues  =  predict(pscore)
# psTreated  =  predValues[postgrad$HEAP_num == 1]
# psUntreated  =  predValues[postgrad$HEAP_num == 0]
# 
# outcpp = macthNN(predValues, psTreated, psUntreated)
# Sys.time() - start
# head(outcpp)
# length(outcpp)