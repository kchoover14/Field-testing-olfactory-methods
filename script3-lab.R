library(readxl); library(ordinal); library(MASS); library(BSDA)

#load data
lab=read_excel("data-field methods.xlsx", sheet="clm.lab")

#factors
lab$Location=factor(lab$LocationName)
lab$Age=factor(lab$Age)
lab$Sex=factor(lab$Sex)
lab$Olfactory_Ability=ordered(lab$OA3Verbal)

#models
lab.random= clmm(Olfactory_Ability ~ Location + Sex + Age + (1|Ind),
                 data = lab, threshold = "flexible", doFit=TRUE,
                 Hess=TRUE, nAGQ=10, link= "logit")
lab.norandom=clm(Olfactory_Ability ~ Location + Sex + Age,
                 data=lab,threshold="flexible", doFit=TRUE,
                 Hess=TRUE, nAGQ=10, link="logit")
#test if within-subjects is significant
anova(lab.random, lab.norandom)

#Table 1
#Model summary
summary(lab.random)
#provides odds ratios
exp(coef(lab.random)[1])
exp(coef(lab.random)[2])
exp(coef(lab.random)[3])
exp(coef(lab.random)[4])
exp(coef(lab.random)[5])

#Figure 3
labprob= polr(Olfactory_Ability ~ Location + Sex + Age, data = lab)
effects::Effect(focal.predictors = c("Location", "Sex"), labprob)
plot(effects::Effect(focal.predictors = c("Location", "Sex"),
                     labprob), rug=FALSE, style="stacked",
     lines=list(col=c("navy", "blue violet", "plum"),
     main="", ylab="Probability"))

#Figure 4
ci <- lab.random$ranef + qnorm(0.975) * sqrt(lab.random$condVar) %o% c(-1, 1)
ord.re= order(lab.random$ranef)
ci= ci[order(lab.random$ranef),]
plot(1:30, lab.random$ranef[ord.re], axes=FALSE,
     xlab="Individual", ylab="Individual effect")
axis(1, at=1:30, labels = ord.re)
axis(2)
for(i in 1:30) segments(i, ci[i,1], i, ci[i, 2])

#Table 2: Signed Ranks
labrt=read_excel("data-field methods.xlsx", sheet="wilcox.lab")
flabrt=subset(labrt, Sex=="Female")
mlabrt=subset(labrt, Sex=="Male")

SIGN.test(labrt$Control, labrt$Experimental, alternative = "two.sided", conf.level=0.95)
SIGN.test(flabrt$Control, flabrt$Experimental, alternative = "two.sided", conf.level=0.95)
SIGN.test(mlabrt$Control, mlabrt$Experimental, alternative = "two.sided", conf.level=0.95)

