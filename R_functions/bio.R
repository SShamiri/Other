

library(haven)
library(Matching)
library(wakefield)
library(ggplot2)
library(broom)
library(tidyverse)



datRow= read_sas('path_to_file') 

# select desired variables 
dat = dat %>% select(CONNAT, ADVENG, EDATTAIN,DSEIFAA,WKLYINCQ,SEX,REMOTE,UNDER15,MESC,AGEGRP,MARSTAT,LFSTATUS,OCC1DIG)

# sim data
n = 1000
dat = r_data_frame(n = n,
             dummy(name ='CONNAT',prob = c(0.5,0.5)),
             dummy(name ='ADVENG'),
             age(x = 18:65, name='Age'),
             dummy(name = 'SEX'),
             dummy(name = 'UNDER15'),
             dummy(name = 'MESC'),
             dummy(name = 'MARSTAT'),
             sentence(c('1.post-grad','2.bachelors','3.diploma/adv diploma','4.cert II/IV','5.year12','6.year 11 and below'),
                         name = 'HEAP', prob = c(0.2,0.2,0.15,0.15,0.1,0.25)),
             DSEIFAA = sample(1:10,n,replace = T),
             WKLYINCQ = sample(1:5,n,replace = T),
             REMOTE = sample(1:5,n,replace = T),
             AGEGRP = sample(1:14,n,replace = T),
             LFSTATUS = sample(1:3,n,replace = T),
             OCC1DIG = sample(1:3,n,replace = T)
             
             
)

dat = dat %>% tbl_df()

# # data prep 
# dat = dat %>% mutate(CONNAT = ifelse(CONNAT==1,1,0),
#                      ADVENG = ifelse(ADVENG==1,1,0),
#                      HEAP = case_when(
#                        EDATTAIN==1 ~ 'post-grad',
#                        EDATTAIN %in% c(2,3) ~ 'bachelors',
#                        EDATTAIN == 4 ~ 'diploma/adv diploma',
#                        EDATTAIN %in% c(5,7) ~'cert II/IV',
#                        EDATTAIN == 8 ~ 'year12',
#                        EDATTAIN %in% c(6,9,10,11,12,14) ~ 'year 11 and below',
#                        TRUE ~ 'Unknown'
#                      ),
#                      SEX = ifelse(SEX==1,1,0),
#                      UNDER15 = ifelse(UNDER15==1,1,0),
#                      MESC = ifelse(MESC %in% c(1,2),1,0),
#                      MARSTAT = ifelse(MARSTAT==1,1,0)
# )




## Frequencies and proportions (one-sample test)
freq = table(dat$HEAP, dat$CONNAT)
prop = prop.table(table(dat$HEAP, dat$CONNAT),1)

out = list()
for(i in 1: nrow(prop)) {
 #i = 1 
proptest = prop.test(freq[i,2],sum(freq[i,]))
X_squared = proptest$statistic
p_value = proptest$p.value
conf = cbind(lwConf = proptest$conf.int[1],upConf = proptest$conf.int[2])
out[[i]] = data.frame(X_squared, p_value, conf )
}

prop = data.frame(prop) %>% spread(Var2,Freq) %>% bind_cols(bind_rows(out))
colnames(prop)[1] = 'HEAP'

## PSM PhD vs year 11 and below

postgrad = dat %>% filter(HEAP == '1.post-grad' | HEAP=='6.year 11 and below') %>% mutate(HEAP_num = ifelse(HEAP == '1.post-grad',1,0))

pscore = glm(HEAP_num ~ ADVENG + DSEIFAA+WKLYINCQ+SEX+REMOTE+UNDER15+MESC+AGEGRP+MARSTAT+LFSTATUS+OCC1DIG, data = postgrad, family = binomial() )
summary(pscore)

postgrad$Pscore = pscore$fitted.values
match = Match(Y = postgrad$CONNAT, Tr = postgrad$HEAP_num, X = postgrad$Pscore, replace =FALSE)
summary(match)

mb = MatchBalance(HEAP_num ~ ADVEN+ DSEIFAA+WKLYINCQ+SEX+REMOTE+UNDER15+MESC+AGEGRP+MARSTAT+LFSTATUS+OCC1DIG, nboots=500, data=postgrad, match.out=match)
summary(mb)

