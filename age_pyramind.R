library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)

dat = read_csv('age.csv')

ageG = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69")

dat = dat %>% filter(Age %in% ageG) %>% 
              mutate(Age = ifelse(Age=='65-69','65+',Age), n = pct/100)


gg = ggplot(data = dat, aes(x=Age,fill = sector ))

male = gg + 
  geom_bar( data=subset(dat,Gender == 'M'), aes( y = n), stat="identity", position=position_dodge()) +
  scale_y_continuous('', labels = scales::percent) +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        plot.title = element_text(size = 11),
        #plot.margin=unit(c(0.1,0.2,0.1,-.1),"cm"),
        plot.margin=unit(c(0.1,0,0.1,-0.05),"cm"),
        axis.ticks.y = element_blank(), 
        axis.text.y = theme_economist_white()$axis.text.y,
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.background = element_rect(fill = "white",colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "gray")) + 
    
  
  ggtitle("Male") + 
  coord_flip() #+ theme_economist_white()   

male

female =  gg + 
  geom_bar( data=subset(dat,Gender == 'F'), 
            aes( y = n), stat="identity", position=position_dodge()) +
  scale_y_continuous('', labels = scales::percent, trans = 'reverse') + 
  theme(legend.position = 'none',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(size = 11),
        plot.margin=unit(c(0.1,0,0.1,0.05),"cm"),
        panel.background = element_rect(fill = "white",colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "gray")) + 
  ggtitle("Female") + 
  coord_flip() #+ theme_economist_white() 
  #ylab("Age") 

female
## Plutting it together
grid.arrange(female,
             male,
             widths=c(0.2,0.2),
             ncol=2
)



## other way
dat = dat %>% mutate(v = ifelse(Gender=='M',-1*n,n))



dat %>% ggplot(aes(x= Age, y=v, fill=sector)) + 
  geom_bar(data = subset(dat, Gender == "M"), stat="identity", position=position_dodge()) +
  geom_bar(data = subset(dat, Gender == "F"), stat="identity", position=position_dodge()) + 
  scale_y_continuous('', labels = scales::percent) + 
  coord_flip()
####

