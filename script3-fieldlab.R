options(prompt="R>")
options("scipen"=100, "digits"=4)

library(readxl)
cf=read_excel("field methods.xlsx", sheet="cf")
Location2=factor(cf$Location2)
LocationName=facctor(cf$LocationName)
Age=factor(cf$Age)
Sex=factor(cf$Sex)
Olfactory_Ability=factor(cf$OA3Verbal)
library(ordinal)
cf.loc1= clm(Olfactory_Ability ~ LocationName + Sex + Age, data = cf,
         Hess = TRUE, nAGQ = 10, threshold = "flexible")
cf.loc11= clm(Olfactory_Ability ~ Location2 + Sex + Age, data = cf,
              Hess = TRUE, nAGQ = 10, threshold = "flexible")
anova(cf.loc1, cf.loc11)
summary(cf.loc1)
summary(cf.loc11)
#provides odds ratios
exp(coef(cf.loc1)[1])
exp(coef(cf.loc1)[2])
exp(coef(cf.loc1)[3])
exp(coef(cf.loc1)[4])
exp(coef(cf.loc1)[5])
exp(coef(cf.loc1)[6])
exp(coef(cf.loc1)[7])
exp(coef(cf.loc1)[8])

library(MASS)
Olfactory_Ability=factor(cf$OA3Verbal)
diff= polr(Olfactory_Ability ~ LocationName + Sex + Age, data = cf)
diff2= polr(Olfactory_Ability ~ Sex + LocationName + Age, data = cf)
summary(diff)
car::Anova(diff)

#PLOTS
loc=plot(effects::Effect(focal.predictors = c("LocationName"), 
                     diff), rug=FALSE, style="stacked",
         lines=list(col=c("navy", "blue violet", "plum"),
     main = "", ylab = "Probability"))
sex= plot(effects::Effect(focal.predictors = c("Sex"), 
                     diff2), rug=FALSE, style="stacked",
          lines=list(col=c("navy", "blue violet", "plum"),
     main = "", ylab = "Probability"))
library(gridExtra)
grid.arrange(loc,sex,ncol=2)
