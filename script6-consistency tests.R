options(prompt="R>")
options("scipen"=100, "digits"=4)

library(readxl)
ms2con=read_excel("3-data-consistency.xlsx", sheet="ms2-con")
cat("************FIELD MANN U************")
wilcox.test(Consistencybinnms2 ~ Sexms2, data=ms2con, exact=TRUE, 
            conf.int=TRUE, conf.level=0.95)
wilcox.test(Scorenms2 ~ Sexms2, data=ms2con, exact=TRUE, 
            conf.int=TRUE, conf.level=0.95)
wilcox.test(Odornms2~Consistencybinnms2, data=ms2con, exact=TRUE, 
            conf.int=TRUE, conf.level=0.95)

ms3con=read_excel("3-data-consistency.xlsx", sheet="ms3-con")
cat("************LAB MANN U************")
wilcox.test(Consistencynms3 ~ Sexms3, data=ms3con, exact=TRUE, 
            conf.int=TRUE, conf.level=0.95)
wilcox.test(Scorenms3 ~ Sexms3, data=ms3con, exact=TRUE, 
            conf.int=TRUE, conf.level=0.95)
wilcox.test(Odornms3~Consistencyms3, data=ms3con, exact=TRUE, 
            conf.int=TRUE, conf.level=0.95)

con=read_excel("3-data-consistency.xlsx", sheet="con")
cat("************ALL MANN U************")
wilcox.test(Consistencyn ~ Sex, data=con, exact=TRUE, 
            conf.int=TRUE, conf.level=0.95)
wilcox.test(Scoren ~ Sex, data=con, exact=TRUE, 
            conf.int=TRUE, conf.level=0.95)
wilcox.test(Odorn~Consistency, data=con, exact=TRUE, 
            conf.int=TRUE, conf.level=0.95)

library(ordinal)
library(readxl)
cat("************FIELD MODEL************")
ms2con=read_excel("3-data-consistency.xlsx", sheet="ms2-con")
Sex2=as.factor(ms2con$Sexms2)
Odor2=as.factor(ms2con$Odorms2)
Consistency2=ordered(ms2con$Consistencybinnms2)
ms2con.model=clm(Consistency2 ~ Sex2 + Odor2, 
               Hess = TRUE, nAGQ = 10, threshold = "flexible")
summary(ms2con.model)
#provides odds ratios
exp(coef(ms2con.model)[1])
exp(coef(ms2con.model)[2])
exp(coef(ms2con.model)[3])
exp(coef(ms2con.model)[4])
exp(coef(ms2con.model)[5])
exp(coef(ms2con.model)[6])
exp(coef(ms2con.model)[7])

cat("************LAB MODEL************")
ms3con=read_excel("3-data-consistency.xlsx", sheet="ms3-con")
Sex3=as.factor(ms3con$Sexms3)
Odor3=as.factor(ms3con$Odorms3)
Consistency3=ordered(ms3con$Consistencyms3)
ms3con.model=clm(Consistency3 ~ Sex3 + Odor3, 
                 Hess = TRUE, nAGQ = 10, threshold = "flexible")
summary(ms3con.model)
cat("************ALL MODEL************")
con=read_excel("3-data-consistency.xlsx", sheet="con")
Sexa=as.factor(con$Sex)
Odora=as.factor(con$Odor)
Consistencya=ordered(con$Consistency)
con.model=clm(Consistencya ~ Sexa + Odora, 
                 Hess = TRUE, nAGQ = 10, threshold = "flexible")
summary(con.model)

library(dplyr)
library(readxl)
library(ggplot2)
ms2con=read_excel("field methods.xlsx", sheet="ms2-con")
ms2con2 <- ms2con %>% 
  group_by(Odorms2,Sexms2, Consistencybinms2) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
ms2plot=ggplot(ms2con2, aes(x = Odorms2, y = perc*100, 
                    fill = factor(Consistencybinms2))) +
  geom_bar(stat="identity", width = 0.7) +
  scale_fill_manual("legend", values = c("Inconsistent"="navy",
                                         "Consistent"="blue violet"))+
  labs(x = "Odor-Field", y = "Percent", fill="Consistency") +
  facet_grid(~Sexms2)+
  theme_minimal(base_size = 14)
ms3con=read_excel("3-data-consistency.xlsx", sheet="ms3-con")
ms3con3 <- ms3con %>% 
  group_by(Odorms3,Sexms3, Consistencyms3) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))
ms3plot=ggplot(ms3con3, aes(x = Odorms3, y = perc*100, 
                    fill = factor(Consistencyms3))) +
  geom_bar(stat="identity", width = 0.7) +
  scale_fill_manual("legend", values = c("Inconsistent"="navy",
                                         "Consistent"="blue violet"))+
  labs(x = "Odor-Lab", y = "Percent", fill="Consistency") +
  facet_grid(~Sexms3)+
  scale_color_gradient()+
  theme_minimal(base_size = 14)
library(gridExtra)
grid.arrange(ms2plot,ms3plot,nrow=2)

