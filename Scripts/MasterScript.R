### Data exploration

rm(list = ls())

load("MA thesis/Reed-Smith-JHRED-CANDIDATES.Rdata")


load("MA thesis/SouthKorea Legislative Data/20160725_1320Ranked.dta")

library(tidyverse)

x$name_jp[1]


JPMMM <- x %>%
  filter(year > 1993)

table(JPMMM$ken)

?lm

lag(JPMMM, k=1, JPMMM$yr, name="previous")

table(Reed_Smith_JHRED_Data_1947_2017_publicrelease$ken,
      Reed_Smith_JHRED_Data_1947_2017_publicrelease$kunr)

JPMMM96 <- JPMMM %>%
  filter(yr == 19.0)

JPMMM00 <- JPMMM %>%
  filter(yr == 20.0)

JPMMM02 <- JPMMM %>%
  filter(yr == 21.0)

JPMMM05 <- JPMMM %>%
  filter(yr == 22.0)

JPMMM09 <- JPMMM %>%
  filter(yr == 23.0)

JPMMM12 <- JPMMM %>%
  filter(yr == 24.0)

JPMMM14 <- JPMMM %>%
  filter(yr == 25.0)

mean(JPMMM96$female[which(JPMMM96$ken == "NA")])

mean(JPMMM00$female[which(JPMMM00$ken == "NA")])

mean(JPMMM02$female[which(JPMMM02$ken == "NA")])

mean(JPMMM05$female[which(JPMMM05$ken == "NA")])

mean(JPMMM09$female[which(JPMMM09$ken == "NA")])

mean(JPMMM12$female[which(JPMMM12$ken == "NA")])

mean(JPMMM14$female[which(JPMMM14$ken == "NA")])


mean(JPMMM96$female[which(JPMMM96$prcode > 0)])

mean(JPMMM00$female[which(JPMMM00$prcode > 0)])

mean(JPMMM02$female[which(JPMMM02$prcode > 0)])

mean(JPMMM05$female[which(JPMMM05$prcode > 0)])

mean(JPMMM09$female[which(JPMMM09$prcode > 0)])

mean(JPMMM12$female[which(JPMMM12$prcode > 0)])

mean(JPMMM14$female[which(JPMMM14$prcode > 0)])
