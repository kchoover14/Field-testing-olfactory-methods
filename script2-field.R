library(readxl); library(ordinal); library(MASS); library(BSDA)

#load data
field <- read_excel("data-field methods.xlsx", sheet="clm.field")

#factors
field$Ind <- factor(field$Ind)
field$Day <- factor(field$Day)
field$Hour <- factor(field$Hour)
field$Location <- factor(field$LocationName)
field$Age <- factor(field$Age)
field$Sex <- factor(field$Sex)
field$Olfactory_Ability <- ordered(field$OA3,
  levels = c("Normal", "Indet", "Poor"))

#comparing models
field.random <-  clmm(Olfactory_Ability ~ Location + Sex + Age + Day + Hour + (1|Ind),
  data=field, threshold = "flexible", doFit=TRUE,
  Hess=TRUE, nAGQ=10, link= "logit")
field.norandom <- clm(Olfactory_Ability ~ Location + Sex + Age + Day + Hour,
  data=field,threshold="flexible", doFit=TRUE,
  Hess=TRUE, nAGQ=10, link="logit")
#test if within-subjects is significant
anova(field.random, field.norandom)

#Table 1, field
#model summary
summary(field.random)
#odds ratios
exp(coef(field.random)[1])
exp(coef(field.random)[2])
exp(coef(field.random)[3])
exp(coef(field.random)[4])
exp(coef(field.random)[5])
exp(coef(field.random)[6])
exp(coef(field.random)[7])
exp(coef(field.random)[8])

#Figure 1-effects
fieldprob= polr(Olfactory_Ability ~ Location + Sex + Age + Hour + Day, data=field)
effects::Effect(focal.predictors = c("Sex", "Location"), field.random)
plot(effects::Effect(focal.predictors = c("Sex","Location"),
                     field.random), rug=FALSE, style="stacked",
     lines=list(col=c("navy", "blue violet", "plum"),
     main="", ylab="Probability"))

#Figure 2-ind variance
ci <- field.random$ranef + qnorm(0.975) * sqrt(field.random$condVar) %o% c(-1, 1)
ord.re= order(field.random$ranef)
ci= ci[order(field.random$ranef),]
plot(1:29, field.random$ranef[ord.re], axes=FALSE,
     xlab="Individual", ylab="Individual effect")
axis(1, at=1:29, labels = ord.re)
axis(2)
for(i in 1:29) segments(i, ci[i,1], i, ci[i, 2])
abline(h = 0, lty=2)

#Table 2: Signed Ranks
fieldrt=read_excel("data-field methods.xlsx", sheet="wilcox.field")
ffieldrt=subset(fieldrt, Sex=="Female")
mfieldrt=subset(fieldrt, Sex=="Male")

SIGN.test(fieldrt$Tate, fieldrt$Borough, alternative = "two.sided", conf.level=0.95)
SIGN.test(ffieldrt$Tate, ffieldrt$Borough, alternative = "two.sided", conf.level=0.95)
SIGN.test(mfieldrt$Tate, mfieldrt$Borough, alternative = "two.sided", conf.level=0.95)

SIGN.test(fieldrt$Tate, fieldrt$Southwark, alternative = "two.sided", conf.level=0.95)
SIGN.test(ffieldrt$Tate, ffieldrt$Southwark, alternative = "two.sided", conf.level=0.95)
SIGN.test(mfieldrt$Tate, mfieldrt$Southwark, alternative = "two.sided", conf.level=0.95)

