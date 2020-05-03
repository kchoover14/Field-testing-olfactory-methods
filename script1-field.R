options(prompt="R>")
options("scipen"=100, "digits"=4)

library(readxl)
field=read_excel("field methods.xlsx", sheet="field")
library(DescTools)
KendallTauB(field$OA3Free, field$OA3Verbal, conf.level=0.95)#no p
cor.test(field$OA3FreeNum, field$OA3VerbalNum,method=("kendall"))

#factors
Day=factor(field$Day)
Hour=factor(field$Hour)
Location=factor(field$LocationName)
Age=factor(field$Age)
Sex=factor(field$Sex)
Olfactory_Ability=ordered(field$OA3Verbal)

#clmm
library(ordinal)
sink(file="results1-field.txt", append= FALSE)
field.random= clmm(Olfactory_Ability ~ Location + Sex + Age + Day + Hour + (1|Ind),
              data = field, threshold = "flexible", doFit=TRUE,
              Hess=TRUE, nAGQ=10, link= "logit")
field.norandom=clm(Olfactory_Ability ~ Location + Sex + Age + Day + Hour, 
                       data=field,threshold="flexible", doFit=TRUE,
                       Hess=TRUE, nAGQ=10, link="logit")
#test if within-subjects is significant
anova(field.random, field.norandom)

#provides odds ratios
exp(coef(field.random)[1])
exp(coef(field.random)[2])
exp(coef(field.random)[3])
exp(coef(field.random)[4])
exp(coef(field.random)[5])
exp(coef(field.random)[6])
exp(coef(field.random)[7])
exp(coef(field.random)[8])

summary(field.random)

#shows ind random effects--can then see who inc/dec
randeff= ranef(field.random)
randeff=unlist(randeff)
sqstderr= condVar(field.random)
stderr= condVar(field.random)$Ind[, 1]^.5
randeff
tate= field[ which(Location=='1-Tate'),]
stderr
sink()

#PLOT 1
library(ordinal)
field.random= clmm(Olfactory_Ability ~ Locations +Age+Sex+Day+Hour+(1|Ind),
                   data = field, threshold = "flexible", doFit=TRUE,
                   Hess=TRUE, nAGQ=10, link= "logit")
library(MASS)
fieldprob= polr(Olfactory_Ability ~ Locations + Sex +Age+Hour+Day)
summary(fieldprob)
car::Anova(fieldprob)
effects::Effect(focal.predictors = c("Sex", "Locations"), field.random)
plot(effects::Effect(focal.predictors = c("Sex","Locations"), 
                     field.random), rug=FALSE, style="stacked",
     lines=list(col=c("navy", "blue violet", "plum"),
     main="", ylab="Probability"))

#Plot2-ind variance
ci <- field.random$ranef + qnorm(0.975) * sqrt(field.random$condVar) %o% c(-1, 1)
ord.re= order(field.random$ranef)
ci= ci[order(field.random$ranef),]
plot(1:29, field.random$ranef[ord.re], axes=FALSE, 
     xlab="Individual", ylab="Individual effect")
axis(1, at=1:29, labels = ord.re)
axis(2)
for(i in 1:29) segments(i, ci[i,1], i, ci[i, 2])
abline(h = 0, lty=2)
