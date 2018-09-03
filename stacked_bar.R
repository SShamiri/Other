
library(ggplot2)
library(tidyverse)

dat = read_csv('foe2.csv')
# order foe then convert to factors
dat = dat %>% arrange(desc(rn))
dat$name = factor(unique(dat$name), levels = unique(dat$name))
cols = gl(2, 1, length = 22, labels = c( "white","black")) 

dat %>% ggplot(aes(x = name, y = value)) +
  geom_bar(stat = "identity",aes(fill = gender)) +
  scale_fill_manual(values = alpha(c('F' = "#c2dff5", 'M' ="#165788"))) +
  geom_text(aes(label=value),size = 3, position = position_stack(vjust = 0.5),
            colour = cols) +
  coord_flip() +
  theme(legend.position = 'none',
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  xlab('') +
  ylab('')





g <- ggplot(mpg, aes(class, fill ))
g + geom_bar(aes(fill = drv), position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme(legend.position = "top")

Year      <- c(rep(c("2006-07", "2007-08", "2008-09", "2009-10"), each = 4))
Category  <- c(rep(c("A", "B", "C", "D"), times = 4))
Frequency <- c(168, 259, 226, 340, 216, 431, 319, 368, 423, 645, 234, 685, 166, 467, 274, 251)
Data      <- data.frame(Year, Category, Frequency)

ggplot(Data, aes(x = Year, y = Frequency,  color = Category)) +
  geom_bar(stat = "identity",aes(fill = Category)) +
  scale_fill_manual(values = alpha(c("#000000", "#FF5733","#000000", "#FF5733"))) +
  geom_text(size = 3, position = position_stack(vjust = 0.5))


Area <- c("Option1", "Option2", "Option3")
Count <- c(193, 56, 4,240, 10, 25)
Type <- c("car", "car", "car", "bike", "bike", "bike")
p <- data.frame(Area, Count, Type)

ggplot(Data, aes(x=Year, y=Frequency, color=Category)) + 
  geom_bar(stat="identity", position="dodge", aes(fill=Category))  +
  scale_fill_manual(values = alpha(c("#000000", "#FF5733","#000000", "#FF5733"))) +
  geom_text(aes(label=Frequency), position=position_dodge(width = 0.9), vjust=-0.40)



ggplot(p, aes(x=Area, y=Count, color=Type)) + 
  geom_bar(stat="identity", position="dodge", aes(fill=Type))  +
  scale_fill_manual(values = alpha(c("#000000", "#FF5733"))) +
  geom_text(aes(label=Count), position=position_dodge(width = 0.9), vjust=-0.40)


ggplot(p, aes(x=Area, y=Count, color=Type)) + 
  geom_bar(stat="identity", position="dodge", aes(fill=Type), color="black")  +
  scale_fill_manual(values = alpha(c("#000000", "#FF5733"))) +
  geom_text(aes(label=Count, colour=Type), 
            position=position_dodge(width = 0.9), 
            vjust=-0.40) +
  scale_colour_manual(values=c("#000000", "#FF5733"))
