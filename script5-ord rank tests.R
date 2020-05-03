options(prompt="R>")
options("scipen"=100, "digits"=4)

library(readxl)
fieldrt=read_excel("field methods.xlsx")
ffieldrt=subset(fieldrt, Sex=="Female")
mfieldrt=subset(fieldrt, Sex=="Male")
labrt=read_excel("field methods.xlsx")
flabrt=subset(labrt, Sex=="Female")
mlabrt=subset(labrt, Sex=="Male")

library(BSDA)
SIGN.test(fieldrt$Tate, fieldrt$Southwark, alternative = "two.sided", conf.level=0.95)
SIGN.test(fieldrt$Tate, fieldrt$Borough, alternative = "two.sided", conf.level=0.95)
SIGN.test(ffieldrt$Tate, ffieldrt$Southwark, alternative = "two.sided", conf.level=0.95)
SIGN.test(ffieldrt$Tate, ffieldrt$Borough, alternative = "two.sided", conf.level=0.95)
SIGN.test(mfieldrt$Tate, mfieldrt$Southwark, alternative = "two.sided", conf.level=0.95)
SIGN.test(mfieldrt$Tate, mfieldrt$Borough, alternative = "two.sided", conf.level=0.95)

SIGN.test(labrt$Control, labrt$Experimental, alternative = "two.sided", conf.level=0.95)
SIGN.test(flabrt$Control, flabrt$Experimental, alternative = "two.sided", conf.level=0.95)
SIGN.test(mlabrt$Control, mlabrt$Experimental, alternative = "two.sided", conf.level=0.95)


