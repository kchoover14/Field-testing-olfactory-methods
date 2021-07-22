library(readxl); library(ordinal); library(gridExtra)

#load data
lab.v.field=read_excel("data-field methods.xlsx", sheet="labVfield")

#factors
lab.v.field$Location=factor(lab.v.field$LocationName)
lab.v.field$Age=factor(lab.v.field$Age)
lab.v.field$Sex=factor(lab.v.field$Sex)
lab.v.field$Olfactory_Ability=factor(lab.v.field$OA3Verbal)

library(MASS)
diff= polr(Olfactory_Ability ~ LocationName + Sex + Age, data = lab.v.field)
diff2= polr(Olfactory_Ability ~ Sex + LocationName + Age, data = lab.v.field)

#Figure 5
loc=plot(effects::Effect(focal.predictors = c("LocationName"),
                     diff), rug=FALSE, style="stacked",
         lines=list(col=c("navy", "blue violet", "plum"),
     main = "", ylab = "Probability"))
sex= plot(effects::Effect(focal.predictors = c("Sex"),
                     diff2), rug=FALSE, style="stacked",
          lines=list(col=c("navy", "blue violet", "plum"),
     main = "", ylab = "Probability"))
grid.arrange(loc,sex,ncol=2)
