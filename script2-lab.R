options(prompt="R>")
options("scipen"=100, "digits"=4)

library(readxl)
lab=read_excel("field methods.xlsx", sheet="lab")
library(DescTools)
KendallTauB(lab$OA3FreeNum, lab$OA3VerbalNum, conf.level=0.95)#no p
cor.test(lab$OA3FreeNum, lab$OA3VerbalNum,method=("kendall"))

Location=factor(lab$LocationName)
Age=factor(lab$Age)
Sex=factor(lab$Sex)
Olfactory_Ability=ordered(lab$OA3Verbal)
library(ordinal)
lab.random= clmm(Olfactory_Ability ~ Location + Sex + Age + (1|Ind),
                 data = lab, threshold = "flexible", doFit=TRUE,
                 Hess=TRUE, nAGQ=10, link= "logit")
lab.norandom=clm(Olfactory_Ability ~ Location + Sex + Age, 
                 data=lab,threshold="flexible", doFit=TRUE,
                 Hess=TRUE, nAGQ=10, link="logit")
#test if within-subjects is significant
anova(lab.random, lab.norandom)

#provides odds ratios
exp(coef(lab.random)[1])
exp(coef(lab.random)[2])
exp(coef(lab.random)[3])
exp(coef(lab.random)[4])
exp(coef(lab.random)[5])

summary(lab.random)

#shows ind random effects--can then see who inc/dec
randeff= ranef(lab.random)
randeff=unlist(randeff)
sqstderr= condVar(lab.random)
stderr= condVar(lab.random)$Ind[, 1]^.5
randeff
stderr

#Plot 1
Locations=factor(lab$LocationName)
Sex=factor(lab$Sex)
Age=factor(lab$Age)
Olfactory_Ability=ordered(lab$OA3Verbal)
library(ordinal)
lab.random= clmm(Olfactory_Ability ~ Locations + Sex + Age + (1|Ind),
                 data = lab, threshold = "flexible", doFit=TRUE,
                 Hess=TRUE, nAGQ=10, link= "logit")
summary(lab.random)
library(MASS)
labprob= polr(Olfactory_Ability ~ Locations + Sex + Age, data = lab)
summary(labprob)
car::Anova(labprob)
effects::Effect(focal.predictors = c("Locations", "Sex"), lab.random)
plot(effects::Effect(focal.predictors = c("Locations", "Sex"), 
                     lab.random), rug=FALSE, style="stacked",
     lines=list(col=c("navy", "blue violet", "plum"),
     main="", ylab="Probability"))

#Plot 2
ci <- lab.random$ranef + qnorm(0.975) * sqrt(lab.random$condVar) %o% c(-1, 1)
ord.re= order(lab.random$ranef)
ci= ci[order(lab.random$ranef),]
plot(1:30, lab.random$ranef[ord.re], axes=FALSE, 
     xlab="Individual", ylab="Individual effect")
axis(1, at=1:30, labels = ord.re)
axis(2)
for(i in 1:30) segments(i, ci[i,1], i, ci[i, 2])

