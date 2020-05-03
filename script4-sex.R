options(prompt="R>")
options("scipen"=100, "digits"=4)

library(readxl)
sex=read_excel("field methods.xlsx", sheet="sex")
oa= ordered(sexdata$OA3, levels=c(1,2,3),
            labels= c("Impaired", "Indet", "Normosmic"))
loc= ordered(sexdata$Loc)
location=ordered(sexdata$Location)
sex= ordered(sexdata$Sex, levels=c("Female", "Male"))

library(ordinal)
sex.clm1= clm(oa ~ sex , data = sexdata,
              Hess = TRUE, nAGQ = 10, threshold = "flexible")
sex.clm2= clm(oa ~ location, data = sexdata,
              Hess = TRUE, nAGQ = 10, threshold = "flexible")
sex.clm3= clm(oa ~ loc, data = sexdata,
              Hess = TRUE, nAGQ = 10, threshold = "flexible")
sex.clm4= clm(oa ~ sex + loc + location, data = sexdata,
              Hess = TRUE, nAGQ = 10, threshold = "flexible")
sex.clm5= clm(oa ~ sex + loc, data = sexdata,
              Hess = TRUE, nAGQ = 10, threshold = "flexible")
sex.clm6= clm(oa ~ sex + location, data = sexdata,
              Hess = TRUE, nAGQ = 10, threshold = "flexible")
anova(sex.clm1, sex.clm2, sex.clm3,sex.clm4, sex.clm5, sex.clm6)
summary(sex.clm4)

#non exp
oa1= ordered(sexdata$OA31, levels=c(1,2,3),
             labels= c("Impaired", "Indet", "Normosmic"))
loc1= ordered(sexdata$Loc1)
location1=ordered(sexdata$Location1)
sex1= ordered(sexdata$Sex1, levels=c("Female", "Male"))

sex.clma= clm(oa ~ sex1, data = sexdata,
              Hess = TRUE, nAGQ = 10, threshold = "flexible")
sex.clmb= clm(oa ~ loc1, data = sexdata,
              Hess = TRUE, nAGQ = 10, threshold = "flexible")
sex.clmc= clm(oa ~ location1, data = sexdata,
              Hess = TRUE, nAGQ = 10, threshold = "flexible")
sex.clmd= clm(oa ~ sex1 + loc1 + location1, data = sexdata,
              Hess = TRUE, nAGQ = 10, threshold = "flexible")
sex.clme=clm(oa ~ sex1 + location1 + loc1, data = sexdata,
             Hess = TRUE, nAGQ = 10, threshold = "flexible")
sex.clmf=clm(oa ~ sex1 + loc1 + location1, data = sexdata,
             Hess = TRUE, nAGQ = 10, threshold = "flexible")
anova(sex.clma, sex.clmb, sex.clmc,sex.clmd, sex.clme, sex.clmf)
summary(sex.clmd) 



