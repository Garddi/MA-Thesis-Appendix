#### Translation Script

## In this script i demonstrate the code used to translate the Nemoto dataset

## Packages used

library(readxl)
library(translateR)
library(tidyverse)

## Loading the Nemoto dataset

KunNADataSMD <- readxl::read_excel(path = "NA Elections 1988-2016 dataset kuniakinemoto.xlsx",
                                   sheet = "SMD")

## Loading a google api key for use

my.api.key <- "Your API key is entered here"

## Using the translate function from the translateR package to translate
## the education field from the raw korean form
## Be mindful that if this is to be duplicated, running this code takes
## Around 40 minutes, and an api key

KunNADataSMDTrans1 <- translate(dataset = KunNADataSMD,
                                content.field = "education",
                                google.api.key = my.api.key,
                                source.lang = "ko",
                                target.lang = "en")

## I create a new dataset to see if translations match, take the matching
## observations and run the raw Korean through google translate manually
## to see if is correct.

EduTransCheck <- cbind(KunNADataSMDTrans1$education, KunNADataSMDTrans1$translatedContent)

EduTransCheck[10132,1]
EduTransCheck[10132,2]

table(is.na(KunNADataSMDTrans1$education))
table(is.na(KunNADataSMD$education))

EduTrans[5774]

## appears correct

save(KunNADataSMDTrans1, file = "TranslatedNemoto.RData")
