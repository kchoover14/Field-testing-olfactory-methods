library(readxl); library(dplyr)
library(ggplot2); library(gridExtra); library(ggpubr)

#load data
con.field=read_excel("data-field methods.xlsx", sheet="con.field")
con.lab=read_excel("data-field methods.xlsx", sheet="con.lab")

#factors
con.field$Sex <- factor(con.field$Sex)
con.lab$Sex <- factor(con.lab$Sex)

###############Wilcox/MannU field
wilcox.test(Consistencyn ~ Sex, data=con.field,
  correct = TRUE, conf.int=TRUE, conf.level=0.95)
#summarize field by sex
con.field %>%
    group_by(Sex) %>%
    summarise(count=sum(Consistencyn))

#plot
ggplot(con.field, aes(x=Odor, y=Consistencyn, fill=Sex)) +
    geom_bar(stat="identity") +
    theme_classic2()

################wilcox/MannU lab
wilcox.test(Consistencyn ~ Sex, data=con.lab,
  correct = TRUE, conf.int=TRUE, conf.level=0.95)
#summarize field by sex
con.lab %>%
  group_by(Sex) %>%
  summarise(count=sum(Consistencyn))


#Figure 6
con.field.sum <- con.field %>%
  group_by(Odor, Sex, Consistencyn) %>%
  summarise(count=n()) %>%
  mutate(perc=count/sum(count))
conf.field.plot=ggplot(con.field.sum, aes(x = Odor, y = perc*100,
    fill = factor(Consistencyn, levels=c(1, 0)))) +
  geom_bar(stat="identity", width = 0.7) +
  scale_fill_manual(name="",
    labels = c("Consistent", "Inconsistent"),
    values = c("0"="navy","1"="blue violet"),) +
  labs(x = "Odor-Field", y = "Percent", fill="Consistency") +
  facet_grid(~Sex)+
  theme_minimal(base_size = 14)

con.lab.sum <- con.lab %>%
  group_by(Odor,Sex, Consistencyn) %>%
  summarise(count=n()) %>%
  mutate(perc=count/sum(count)) %>%
  arrange(desc(Consistencyn))
con.lab.plot=ggplot(con.lab.sum, aes(x = Odor, y = perc*100,
   fill = factor(Consistencyn, levels=c(1, 0)))) +
  geom_bar(stat="identity", width = 0.7, ) +
  scale_fill_manual(name="",
    labels = c("Consistent", "Inconsistent"),
    values=c("0"="navy","1"="blue violet"))+
  labs(x = "Odor-Lab", y = "Percent", fill="Consistencyn") +
  facet_grid(~Sex)+
  scale_color_gradient()+
  theme_minimal(base_size = 14)

grid.arrange(conf.field.plot,con.lab.plot,nrow=2)

