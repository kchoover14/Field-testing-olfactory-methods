options(prompt="R>")
options("scipen"=100, "digits"=4)

#kappa for observer agreement
library(readxl)
field=read_excel("field methods.xlsx", sheet="field")

library(irr)
ms1cain=read_excel("1b-all-kappa.xlsx", sheet="ms1cain")
kappa2(ms1cain)

ms2cain=read_excel("1b-all-kappa.xlsx", sheet="ms2cain")
kappa2(ms2cain)

ms3cain=read_excel("1b-all-kappa.xlsx", sheet="ms3cain")
kappa2(ms3cain)

