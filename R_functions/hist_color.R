library(tidyverse)


cars1 = cars[1:30, ]  # original data
cars_outliers = data.frame(speed=c(19,19,20,20,20,50,80), dist=c(190, 186, 210, 220, 218,500,700))  # introduce outliers.
cars2 = rbind(cars1, cars_outliers)  # data with outliers.

boxplot(cars2$speed, main="With outliers")
out = boxplot.stats(cars2$speed)$out

hist(cars2$speed)

cond = ifelse(cars2$speed %in% out,TRUE,FALSE)

ggplot(cars2,aes(x=speed)) + 
  geom_histogram(data=subset(cars2, cond ==FALSE ), binwidth = 0.5,fill = "red", alpha = 0.2) +
  geom_histogram(data=subset(cars2, cond ==TRUE ), binwidth = 0.5,fill = "blue", alpha = 0.2) #+
  scale_x_continuous(breaks=cars2$speed +0.25, labels=cars2$speed )





