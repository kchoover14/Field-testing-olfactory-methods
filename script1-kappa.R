library(readxl); library(irr)

repeatmarkets.free=read_excel("data-kappa.xlsx",sheet="ms2-free")
kappa2(repeatmarkets.free)

repeatmarkets.verbal=read_excel("data-kappa.xlsx",sheet="ms2-verbal")
kappa2(repeatmarkets.verbal)

repeatlabs.free=read_excel("data-kappa.xlsx",sheet="ms3-free")
kappa2(repeatlabs.free)

repeatlabs.verbal=read_excel("data-kappa.xlsx",sheet="ms3-verbal")
kappa2(repeatlabs.verbal)
