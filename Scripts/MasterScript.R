#### Master script: Packages and dataset modification and merging


rm(list = ls())


library(translateR)
library(tidyverse)
library(tidylog)
library(haven)
library(readxl)
library(stargazer)
library(estimatr)
library(cowplot)
library(scales)
library(arm)
library(dotwhisker)
library(glmtoolbox)


#Loading datasets, Main datasets

x <- read_dta("Reed-Smith-JHRED-CANDIDATES.dta")


load("20160725_1320Ranked.dta")

load("TranslatedNemoto.Rdata")

## Reed-smith Supplemented data for 2021 and 2017

JPMMM21 <- readxl::read_xlsx("Candidates_2021_01Dec2021.xlsx")

Reed_Smith2017data <- read.csv("Reed-Smith-JHRED-Data-1947-2017.csv")

Reed_Smith2017data <- Reed_Smith_JHRED_Data_1947_2017_publicrelease %>%
  filter(year == 2017)

### Reed-Smith Dataset limited to relevant years

JPMMM <- x %>%
  filter(year > 1992)

## testing party personnel database compatability

PPDB <- readxl::read_xls("Japan_10-2021_partyPersonnelDatabase.xls")

x$ku_vote[x$yr == 22 & x$ken == "Chiba" & x$kunr == 1]

table(table(unique(x$ku_vote)))

### Merging data from Pary personnel and Reed-Smith

PPDBSMD %>% unique_id(year, ku_vote, kucode)

## Merging for use in only SMD districts

PPDBSMD <- PPDB %>%
  filter(district_id_nom != 999)

ReedSmithSMD <- JPMMM %>%
  filter(kucode != 0)

table(PPDB)

PPDBSMD$district_id_nom

PPDBSMD <- PPDBSMD %>%
  rename(kucode = district_id_nom)

PPDBSMD <- PPDBSMD %>%
  rename(year = election_year)

PPDBSMD <- PPDBSMD %>%
  rename(ku_vote = nomvote)

JP2017SMD <- Reed_Smith2017data %>%
  filter(kucode > 0)

table(JP2017SMD$party_en)

JP2017SMD <- JP2017SMD %>%
  mutate(camp = ifelse(party_en == "LDP", 1,
                       ifelse(party_en == "CDP", 4,
                              ifelse(party_en == "Ishin", 8 ,
                                     ifelse(party_en == "Kibo", 4,
                                            ifelse(party_en == "JCP", 5,
                                                   ifelse(party_en == "Independent", 9,
                                                          ifelse(party_en == "Minor Party", 9,
                                                                 ifelse(party_en == "SDP", 2,
                                                                        ifelse(party_en == "Komeito", 3,
                                                                               9))))))))))

table(MergedJP$party_en, MergedJP$camp)

## Checking for unique merger values
a <- KunNADataSMDI %>% select(dae_by, distid, party) %>% duplicated()

table(a)

Reed_Smith_JHRED_Data_1947_2017_publicrelease %>% unique_id(pid, yr)

## Merging datasets for a complete JP overview.

ReedSmithSMD <- bind_rows(ReedSmithSMD, JP2017SMD)

MergedJP <- left_join(ReedSmithSMD, PPDBSMD, by = c("year", "ku_vote",
                                                    "kucode"),
                      keep = TRUE)


table(MergedJP$country)


#### Sourcing descriptive datas

## subsetting for each major election

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

JPMMM17 <- Reed_Smith_JHRED_Data_1947_2017_publicrelease %>%
  filter(yr == 26.0)

JPMMM21 <- Reed_Smith2021

## Proportion of women for PR-only candidates

mean(JPMMM96$female[which(JPMMM96$ken == "NA")])

mean(JPMMM00$female[which(JPMMM00$ken == "NA")])

mean(JPMMM02$female[which(JPMMM02$ken == "NA")])

mean(JPMMM05$female[which(JPMMM05$ken == "NA")])

mean(JPMMM09$female[which(JPMMM09$ken == "NA")])

mean(JPMMM12$female[which(JPMMM12$ken == "NA")])

mean(JPMMM14$female[which(JPMMM14$ken == "NA")])

mean(JPMMM17$female[which(JPMMM17$kunr == 0)])

mean(JPMMM21$female[which(JPMMM21$ken == "PR")])

## Proportion of women in all PR lists

mean(JPMMM96$female[which(JPMMM96$prcode > 0)])

mean(JPMMM00$female[which(JPMMM00$prcode > 0)])

mean(JPMMM02$female[which(JPMMM02$prcode > 0)])

mean(JPMMM05$female[which(JPMMM05$prcode > 0)])

mean(JPMMM09$female[which(JPMMM09$prcode > 0)])

mean(JPMMM12$female[which(JPMMM12$prcode > 0)])

mean(JPMMM14$female[which(JPMMM14$prcode > 0)])

mean(JPMMM17$female[which(JPMMM17$prcode > 0)])

mean(JPMMM21$female[which(JPMMM21$prcode > 0)])

### Lee (2018) Dataset

table(X20160725_1320Ranked$sex)

table(is.na(X20160725_1320Ranked$sex))

table(X20160725_1320Ranked$female)

table(X20160725_1320Ranked$nth)

table(X20160725_1320Ranked$male[which(X20160725_1320Ranked$winner == 1)])

table(X20160725_1320Ranked$majorparty, X20160725_1320Ranked$male)

table(X20160725_1320Ranked$party == "grandnational")

table(X20160725_1320Ranked$partyclass, X20160725_1320Ranked$party)

str(X20160725_1320Ranked$snr_support)

table(MergedKR$party.y, MergedKR$partyclass)



### Kuniaki dataset

table(KunNADataSMD$dae)
table(X20160725_1320Ranked$nth)
table(KunNADataSMD$year)

table(KunNADataSMD$gender...29, KunNADataSMD$gender...22)

table(is.na(KunNADataSMD$gender...29))

KunNAData88 <- KunNAData %>%
  filter(dae == 13)

KunNAData92 <- KunNAData %>%
  filter(dae == 14)

KunNAData96 <- KunNAData %>%
  filter(dae == 15)

KunNAData20 <- KunNAData %>%
  filter(dae == 16)


mean(KunNAData88$gender)

mean(KunNAData92$gender)

mean(KunNAData96$gender)

mean(KunNAData20$gender)


#### Adding a common education variable
### TRANSLATION SCRIPT USED HERE SEPARATELY

table(grepl("University", KunNADataSMDTrans1$EduTrans))
table(grepl("Dropped out", KunNADataSMDTrans1$EduTrans),
      grepl("University", KunNADataSMDTrans1$EduTrans))



table(is.na(KunNADataSMD$education))

#### Merging SK datasets on SMD individuals

## Checking matching variables

table(X20160725_1320Ranked$nth)
table(KunNADataSMDTrans1Trim$dae)
table(X20160725_1320Ranked$regionen)
table(KunNADataSMDTrans1Trim$region)


KunNADataSMDTrans1Trim %>% unique_id(totalvotes, votes)
X20160725_1320Ranked %>% unique_id(total_vote, earn_votes)

VecCheck <- sort(X20160725_1320Ranked$earn_votes, decreasing = TRUE)
VecCheck2 <- sort(KunNADataSMDTrans1Trim$votes, decreasing = TRUE)

VecCheck[1:50]
VecCheck2[1:50]

table(MergedKR$other_exp)

MechVec <- which(KunNADataSMD$hanja == "(林利子)")
KunNADataSMD$canid[MechVec]
KunNADataSMD$female[MechVec]
KunNADataSMD$birthday[MechVec]
KunNADataSMD$prcan[MechVec]
KunNADataSMD$win[MechVec]
KunNADataSMD$`prmember t-1`[MechVec]

## merging on total votes and earned votes

X20160725_1320Ranked <- X20160725_1320Ranked %>%
  rename(totalvotes = total_vote)

X20160725_1320Ranked <- X20160725_1320Ranked %>%
  rename(votes = earn_votes)


MergedKR <- left_join(KunNADataSMDMerg, X20160725_1320Ranked,
                      by = c("votes", "totalvotes"),
                      keep = TRUE)

MergedKR$country <- c("South Korea")


## adapting to PPDB levels

table(MergedKR$edu3)

table(grepl("Dropped", MergedKR$EduTrans), grepl("University",
                                                 MergedKR$EduTrans))

WeirdCheck <- MergedKR %>%
  filter(grepl("Dropped", MergedKR$EduTrans),
         grepl("University", MergedKR$EduTrans))

# Creates a 3 value for South Korea on the PPDB scale.

MergedKR <- MergedKR %>%
  mutate(edu4 = ifelse(grepl("Dropped", MergedKR$EduTrans),
                       ifelse(grepl("University", MergedKR$EduTrans),
                              ifelse(grepl("Graduate", MergedKR$EduTrans), 4, 3),
                              1), ifelse(MergedKR$edu3 == 0 | MergedKR$edu3 == 1, MergedKR$edu3 + 1,
                                         ifelse(MergedKR$edu3 == 2 | MergedKR$edu3 == 3, MergedKR$edu3 + 2, NA))))

table(MergedKR$edu4)

### Creating a field eductation variable

MergedKR <- MergedKR %>%
  mutate(edufield = ifelse(grepl("Law", EduTrans),
                            1, ifelse(grepl("Politic", EduTrans),
                                      1, ifelse(grepl("Polic", EduTrans),
                                                1, ifelse(grepl("Administration", EduTrans),
                                                          11, ifelse(grepl("Economic", EduTrans),
                                                                    2, ifelse(grepl("Business", EduTrans),
                                                                              2, ifelse(grepl("Social Science", EduTrans),
                                                                                        3, ifelse(grepl("Science", EduTrans),
                                                                                                  4, ifelse(grepl("Technology", EduTrans),
                                                                                                            4, ifelse(grepl("Engineering", EduTrans),
                                                                                                                      4, ifelse(grepl("Agricultur", EduTrans),
                                                                                                                                5, ifelse(grepl("Medicine", EduTrans),
                                                                                                                                          6, ifelse(grepl("Literature", MergedKR$EduTrans), 7,
                                                                                                                                                                                          ifelse(grepl("Philosophy", EduTrans), 7,
                                                                                                                                                                                                 ifelse(grepl("Education", EduTrans), 7,
                                                                                                                                                                                                        ifelse(grepl("Sociology", EduTrans),
                                                                                                                                                                                                               3, ifelse(grepl("History", EduTrans),
                                                                                                                                                                                                                               7, 0))))))))))))))))))

table(MergedKR$edufield)

### Creating directly transitioning from PR variable. Different from the one
### used in Regressions.

MergedKRTryStuff <- MergedKR %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PRexp2 = dplyr::lag(`prmember t-1`, 1))

MergedKRTryStuff <- MergedKRTryStuff %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PRexp3 = dplyr::lag(`prmember t-1`, 2))

MergedKRTryStuff <- MergedKRTryStuff %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PRexp4 = dplyr::lag(`prmember t-1`, 3))

MergedKRTryStuff <- MergedKRTryStuff %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PRexp5 = dplyr::lag(`prmember t-1`, 4))

MergedKRTryStuff <- MergedKRTryStuff %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PRexp6 = dplyr::lag(`prmember t-1`, 5))

MergedKRTryStuff <- MergedKRTryStuff %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PRexp7 = dplyr::lag(`prmember t-1`, 6))

MergedKRTryStuff <- MergedKRTryStuff %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PRexp8 = dplyr::lag(`prmember t-1`, 7))

MergedKRTryStuff$PRexp2[is.na(MergedKRTryStuff$PRexp2)] <- 0
MergedKRTryStuff$PRexp3[is.na(MergedKRTryStuff$PRexp3)] <- 0
MergedKRTryStuff$PRexp4[is.na(MergedKRTryStuff$PRexp4)] <- 0
MergedKRTryStuff$PRexp5[is.na(MergedKRTryStuff$PRexp5)] <- 0
MergedKRTryStuff$PRexp6[is.na(MergedKRTryStuff$PRexp6)] <- 0
MergedKRTryStuff$PRexp7[is.na(MergedKRTryStuff$PRexp7)] <- 0
MergedKRTryStuff$PRexp8[is.na(MergedKRTryStuff$PRexp8)] <- 0


MergedKRTryStuff <- MergedKRTryStuff %>%
  mutate(PRexp = ifelse(PRexp2 == 1 |
                          PRexp3 == 1 |
                          PRexp4 == 1 |
                          PRexp5 == 1 |
                          PRexp6 == 1 |
                          PRexp7 == 1 |
                          PRexp8 == 1 |
                          `prmember t-1` == 1, 1, 0))


MergedKRTryStuff <- MergedKRTryStuff %>%
  mutate(PropExp = ifelse(PRexp == 1, "Yes", "No"))


MergedKR11 <- tibble(MergedKR, MergedKRTryStuff$PRexp, MergedKRTryStuff$PropExp)

MergedKR11 <- MergedKR11 %>%
  rename(PRexp = `MergedKRTryStuff$PRexp`,
         PropExp = `MergedKRTryStuff$PropExp`)

### Improved PR lag for all that i have data on.
## All Pr candidates are also in this list

GroupedKRMMM <- KunNADataSMDTrans1 %>%
  mutate(PrWin = ifelse(prcan == 1 & prmember == 1, 1, 0))

GroupedKRMMM <- GroupedKRMMM %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PrevPR1 = dplyr::lag(PrWin, 1))

GroupedKRMMM <- GroupedKRMMM %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PrevPR2 = dplyr::lag(PrWin, 2))

GroupedKRMMM <- GroupedKRMMM %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PrevPR3 = dplyr::lag(PrWin, 3))

GroupedKRMMM <- GroupedKRMMM %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PrevPR4 = dplyr::lag(PrWin, 4))

GroupedKRMMM <- GroupedKRMMM %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PrevPR5 = dplyr::lag(PrWin, 5))

GroupedKRMMM <- GroupedKRMMM %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PrevPR6 = dplyr::lag(PrWin, 6))

GroupedKRMMM <- GroupedKRMMM %>%
  group_by(canid) %>%
  arrange(year) %>%
  mutate(PrevPR7 = dplyr::lag(PrWin, 7))

GroupedKRMMM$PrevPR1[is.na(GroupedKRMMM$PrevPR1)] <- 0
GroupedKRMMM$PrevPR2[is.na(GroupedKRMMM$PrevPR2)] <- 0
GroupedKRMMM$PrevPR3[is.na(GroupedKRMMM$PrevPR3)] <- 0
GroupedKRMMM$PrevPR4[is.na(GroupedKRMMM$PrevPR4)] <- 0
GroupedKRMMM$PrevPR5[is.na(GroupedKRMMM$PrevPR5)] <- 0
GroupedKRMMM$PrevPR6[is.na(GroupedKRMMM$PrevPR6)] <- 0
GroupedKRMMM$PrevPR7[is.na(GroupedKRMMM$PrevPR7)] <- 0

GroupedKRMMM <- GroupedKRMMM %>%
  mutate(ImpPRexp = ifelse(PrevPR1 == 1 |
                             PrevPR2 == 1 |
                             PrevPR3 == 1 |
                             PrevPR4 == 1 |
                             PrevPR5 == 1 |
                             PrevPR6 == 1 |
                             PrevPR7 == 1, 1 , 0))

SubbedVarsMergKR <- GroupedKRMMM %>%
  filter(prcan == 0) %>%
  dplyr::select(canid, year, ImpPRexp)

## Merging it with the merged KR set at lines 400-420 in the master script

MergedKR11 <- left_join(MergedKR11, SubbedVarsMergKR, by = c("canid",
                                                             "year"))


## This now has only people who have previously been elected to PR,
## Evidently Lee (2018) is in conflict with this, but mine is based on the data

### Lagging Korea Data ###
# Filter to only SMD and one for no independents

KunNADataSMDMerg <- KunNADataSMDTrans1 %>%
  filter(prcan == 0)

KunNADataSMDTrans1Trim <- KunNADataSMDTrans1 %>%
  filter(prcan == 0, party != "무소속")

KunNADataSMDTrans1Trim %>% unique_id(year, party, distid)


# lagged  vote share, creates prevshare in final set.
KunNADataSMDTrans1Trim <- KunNADataSMDTrans1Trim %>%
  group_by(party, distid) %>% arrange(year) %>%
  mutate(prior_share = dplyr::lag(vshare, 1))

table(KunNADataSMDTrans1Trim$year)
table(KunNADataSMDTrans1Trim$day)



#### Merging both merged datasets

## first creating new sets consisting of the relevant variables
## Variables i want: Election District, election year, cand_votes,
## total_votes, gender, result: elected or not in SMD
## Incumbency, edu4 or similar, edufield, party or partyfamily,
## Right-left dichotomy. Personal characteristics are also kept
## such as name and age.

## First restricting the JP set to post 1994 reform.


## Renaming all variables to be equal for a top down merge.

##Adding political experience variable

MergedJP <- MergedJP %>%
  mutate(pol_exp4 = ifelse(assy == 1, 1,
                           ifelse(mayor.x == 1, 1, 0)))
MergedJP <- MergedJP %>%
  mutate(lawyerexp = ifelse(law == 1, 1, 0))

##Lagging voteshare by each candidate

MergedJP <- MergedJP %>%
  mutate(ku_voteshare = ku_vote.x/ku_totvote)

table(MergedJP$camp, MergedJP$party_en)

MergedJP5 <- MergedJP %>%
  group_by(camp, kucode.x, year.x) %>% arrange(year.x) %>%
  summarise(pshare = sum(ku_voteshare), topperf = max(ku_voteshare)) %>%
  mutate(prevshare = dplyr::lag(pshare, 1), prevtop = dplyr::lag(topperf, 1))

MergedJP8 <- left_join(MergedJP, MergedJP5, by = c("camp", "kucode.x", "year.x"))

##Creating a PR experience variable, This code for only those directly
## Transitioning to SMD lists, not used in regressions.

MergedJPPRlag <- MergedJP %>%
  group_by(pid.x) %>%
  arrange(year.x) %>%
  mutate(PRexp2 = dplyr::lag(inc, 1))

MergedJPPRlag <- MergedJPPRlag %>%
  group_by(pid.x) %>%
  arrange(year.x) %>%
  mutate(PRexp3 = dplyr::lag(inc, 2))

MergedJPPRlag <- MergedJPPRlag %>%
  group_by(pid.x) %>%
  arrange(year.x) %>%
  mutate(PRexp4 = dplyr::lag(inc, 3))

MergedJPPRlag <- MergedJPPRlag %>%
  group_by(pid.x) %>%
  arrange(year.x) %>%
  mutate(PRexp5 = dplyr::lag(inc, 4))

MergedJPPRlag <- MergedJPPRlag %>%
  group_by(pid.x) %>%
  arrange(year.x) %>%
  mutate(PRexp6 = dplyr::lag(inc, 5))

MergedJPPRlag <- MergedJPPRlag %>%
  group_by(pid.x) %>%
  arrange(year.x) %>%
  mutate(PRexp7 = dplyr::lag(inc, 6))

MergedJPPRlag <- MergedJPPRlag %>%
  group_by(pid.x) %>%
  arrange(year.x) %>%
  mutate(PRexp8 = dplyr::lag(inc, 7))


MergedJPPRlag$PRexp2[is.na(MergedJPPRlag$PRexp2)] <- 0
MergedJPPRlag$PRexp3[is.na(MergedJPPRlag$PRexp3)] <- 0
MergedJPPRlag$PRexp4[is.na(MergedJPPRlag$PRexp4)] <- 0
MergedJPPRlag$PRexp5[is.na(MergedJPPRlag$PRexp5)] <- 0
MergedJPPRlag$PRexp6[is.na(MergedJPPRlag$PRexp6)] <- 0
MergedJPPRlag$PRexp7[is.na(MergedJPPRlag$PRexp7)] <- 0
MergedJPPRlag$PRexp8[is.na(MergedJPPRlag$PRexp8)] <- 0


MergedJPPRlag <- MergedJPPRlag %>%
  mutate(PRexp = ifelse(
                        PRexp2 == 3 |
                          PRexp3 == 3 |
                          PRexp4 == 3 |
                          PRexp5 == 3 |
                          PRexp6 == 3 |
                          PRexp7 == 3 |
                          PRexp8 == 3 |
                          inc == 3, 1, 0))

table(MergedJPPRlag$inc, MergedJPPRlag$PRexp)
table(MergedJP9$PRexp, MergedJP9$inc)
table(MergedJP8$PRexp, MergedJP8$inc)

MergedJPPRlag <- MergedJPPRlag %>%
  mutate(PropExp = ifelse(PRexp == 1, "Yes", "No"))

MergedJPPRlagJoin <- MergedJPPRlag %>%
  dplyr::select(pid.x, year.x, PropExp, PRexp, name_jp)

MergedJP8 <- left_join(MergedJP8, MergedJPPRlagJoin, by = c("pid.x", "year.x"))

##Improved PR experience measure, lagging all individuals with PR experience.

GroupedJPMMM <- Reed_Smith_JHRED_Data_1947_2017_publicrelease %>%
  filter(year > 1992) %>%
  group_by(name_jp) %>%
  arrange(year) %>%
  mutate(PrevResult1 = dplyr::lag(result, 1))

GroupedJPMMM <- GroupedJPMMM %>%
  group_by(name_jp) %>%
  arrange(year) %>%
  mutate(PrevResult2 = dplyr::lag(result, 2))

GroupedJPMMM <- GroupedJPMMM %>%
  group_by(name_jp) %>%
  arrange(year) %>%
  mutate(PrevResult3 = dplyr::lag(result, 3))

GroupedJPMMM <- GroupedJPMMM %>%
  group_by(name_jp) %>%
  arrange(year) %>%
  mutate(PrevResult4 = dplyr::lag(result, 4))

GroupedJPMMM <- GroupedJPMMM %>%
  group_by(name_jp) %>%
  arrange(year) %>%
  mutate(PrevResult5 = dplyr::lag(result, 5))

GroupedJPMMM <- GroupedJPMMM %>%
  group_by(name_jp) %>%
  arrange(year) %>%
  mutate(PrevResult6 = dplyr::lag(result, 6))

GroupedJPMMM <- GroupedJPMMM %>%
  group_by(name_jp) %>%
  arrange(year) %>%
  mutate(PrevResult7 = dplyr::lag(result, 7))

GroupedJPMMM <- GroupedJPMMM %>%
  group_by(name_jp) %>%
  arrange(year) %>%
  mutate(PrevResult8 = dplyr::lag(result, 8))

GroupedJPMMM <- GroupedJPMMM %>%
  group_by(name_jp) %>%
  arrange(year) %>%
  mutate(PrevResult9 = dplyr::lag(result, 9))

## Replacing NA's with 0's to ensure that mutate function later works.

GroupedJPMMM$PrevResult1[is.na(GroupedJPMMM$PrevResult1)] <- 0
GroupedJPMMM$PrevResult2[is.na(GroupedJPMMM$PrevResult2)] <- 0
GroupedJPMMM$PrevResult3[is.na(GroupedJPMMM$PrevResult3)] <- 0
GroupedJPMMM$PrevResult4[is.na(GroupedJPMMM$PrevResult4)] <- 0
GroupedJPMMM$PrevResult5[is.na(GroupedJPMMM$PrevResult5)] <- 0
GroupedJPMMM$PrevResult6[is.na(GroupedJPMMM$PrevResult6)] <- 0
GroupedJPMMM$PrevResult7[is.na(GroupedJPMMM$PrevResult7)] <- 0
GroupedJPMMM$PrevResult8[is.na(GroupedJPMMM$PrevResult8)] <- 0
GroupedJPMMM$PrevResult9[is.na(GroupedJPMMM$PrevResult9)] <- 0

##

GroupedJPMMM <- GroupedJPMMM %>%
  mutate(ImpPRexp = ifelse(PrevResult1 == 3 |
                             PrevResult2 == 3 |
                             PrevResult3 == 3 |
                             PrevResult4 == 3 |
                             PrevResult5 == 3 |
                             PrevResult6 == 3 |
                             PrevResult7 == 3 |
                             PrevResult8 == 3 |
                             PrevResult9 == 3, 1, 0))

SubbedVarsMergJP <- GroupedJPMMM %>%
  filter(kucode > 0) %>%
  dplyr::select(name_jp, year, ImpPRexp)


SubbedVarsMergJP <- SubbedVarsMergJP %>%
  rename(year.x = year)


QualCheck <- left_join(SubbedVarsMergJP, MergedJPPRlagJoin, by = c("year.x",
                                                                   "name_jp"))

## Double checks for improvements over previous measure.

table(QualCheck$ImpPRexp, QualCheck$PRexp, useNA = "always")

QualCheck$name_jp[which(QualCheck$ImpPRexp == 0 & QualCheck$PRexp == 1)]

QualCheck2 <- x %>%
  filter(name_jp == "中条栄太郎" | name_jp == "保岡宏武" | name_jp =="地坂拓晃"|
           name_jp == "坂本洋史" | name_jp == "壹岐愛子" | name_jp == "宮下一郎"|
           name_jp == "小泉龍司" | name_jp == "樋口博康" | name_jp == "真白リョウ"|
           name_jp == "荒木章博" | name_jp == "辻清人")


## Outliers appear to be due to internal conflict between public version and
## What i Received through personal correspondence. I use the newest one,
## assumming that to be the superior.

MergedJP8 <- left_join(MergedJP8, SubbedVarsMergJP, by = c("year.x",
                                                           "name_jp"))




##Filtering out 1993, used for lagging

MergedJP9 <- MergedJP8 %>%
  filter(year.x != 1993)



##Adding a country vector
MergedJP9 <- MergedJP9 %>%
  mutate(country = "Japan")

table(MergedJP$inc)
##Adding an exact incumbency variable
MergedJP9 <- MergedJP9 %>%
  mutate(inc_exactly = ifelse(inc == 1, 1, 0))

MergedJP9 <- MergedJP9 %>%
  mutate(PRprev = ifelse(inc == 2 |
                           inc == 3 |
                           inc == 4, 1, 0))
MergedJP9 <- MergedJP9 %>%
  mutate(Camptxt = as.character(camp))



##creating a trimmed and unified dataframe
TrimVarJP <- MergedJP9 %>%
  dplyr::select(name = name_jp,
         distr = kuname,
         pid = pid.x,
         year = year.x,
         yr,
         byelection = byelection.x,
         Area = ken,
         el_distr = kunr,
         el_distr_code = kucode.x,
         region,
         party4 = party_en,
         total_vote = ku_totvote,
         personal_votes = ku_vote.x,
         vote_share = ku_voteshare,
         rank = ku_rank,
         female = female.x,
         age = age.x,
         education,
         edufield = educfield,
         inc,
         inc_exactly,
         resultRS = result.x,
         country,
         pol_exp4,
         lawyerexp,
         PRexp,
         PropExp,
         PRprev,
         prevshare,
         prevtop,
         pshare,
         topperf,
         ImpPRexp,
         Camp = Camptxt)

## Unifying classes and measurements in MergedKR


## Adding a local experience for KR
MergedKR <- MergedKR %>%
  mutate(pol_exp3 = ifelse(pol_exp == "local", 1,
                           0))
class(MergedKR$age.y)

table(MergedKR$inc_exactly, MergedKR$year, MergedKR$female.x)

MergedKR$party4 <- ifelse(MergedKR$party.y == "",
                          MergedKR$partyclass, MergedKR$party.y)

## Collapsing party variable from Nemoto into party families

MergedKR <- MergedKR %>%
  mutate(party5 = ifelse(party.x == "통합민주당" |
                           party.x == "평화민주당"|
                           party.x == "열린우리당"|
                           party.x == "신한당" |
                           party.x == "새천년민주당"|
                           party.x == "민주통합당"|
                           party.x == "민주당"|
                           party.x == "더불어민주당"|
                           party.x == "국민회의"|
                           party.x == "국민의당", "democratic",
                         ifelse(party.x == "민주자유당"|
                                  party.x == "민주정의당"|
                                  party.x == "새누리당"|
                                  party.x == "신민주공화당"|
                                  party.x == "신한국당"|
                                  party.x == "통일민주당"|
                                  party.x == "한나라당", "grandnational",
                                ifelse(party.x == "무소속", "independent",
                                       "minor"))))

##Lagging the voteshare for each party in each electoral district in KR

MergedKR5 <- MergedKR %>%
  group_by(party5, distid, year) %>% arrange(year) %>%
  summarise(pshare = sum(vshare), topperf = max(vshare)) %>%
  mutate(prevshare = dplyr::lag(pshare, 1), prevtop = dplyr::lag(topperf, 1))

MergedKR7 <- MergedKR5 %>%
  ungroup(party5, distid, year)

MergedKR8 <- left_join(MergedKR11, MergedKR5, by = c("party5", "distid", "year"))

WeirdCheck <- MergedKR8 %>%
  filter(distid == 10300)



##Isolating age as a number from the KR set
MergedKR <- MergedKR %>%
  mutate(age4 = readr::parse_number(age.x))

## Adding a country vector
MergedKR <- MergedKR %>%
  mutate(country = "South Korea")

##Adding a lawyerexperience variable
MergedKR <- MergedKR %>%
  mutate(lawyerexp = ifelse(grepl("변호사", MergedKR$job), 1, 0))



##Trimming and renaming the set
TrimVarKR <- MergedKR8 %>%
  dplyr::select(name = name_adjust,
         distr = distname,
         pid = id,
         year,
         yr = dae,
         byelection,
         Area = region,
         el_distr = dist_id,
         el_distr_code = distid,
         region,
         party4 = party5,
         total_vote = totalvotes.x,
         personal_votes = votes.x,
         vote_share = vshare,
         rank = rank.x,
         female = female.x,
         age = age4,
         education = edu4,
         edufield,
         inc,
         inc_exactly,
         result = win,
         country,
         pol_exp3,
         lawyerexp,
         PRexp,
         PropExp,
         PRprev = `prmember t-1`,
         prevshare,
         prevtop,
         pshare,
         topperf,
         ImpPRexp,
         Camp = party5)

### Merging top down for a complete set

UseData <- bind_rows(TrimVarJP, TrimVarKR)

UseData <- UseData %>%
  mutate(result = ifelse(is.na(resultRS), result,
                         ifelse(resultRS == 1, 1, 0)))

UseData <- UseData %>%
  mutate(PropExp2 = ifelse(ImpPRexp == 1, "Yes", "No"))

save(UseData, file = "MergedData.Rdata")

### Creating an overview of the Data for data chapter visualisation

stargazer(as.data.frame(UseData), type = "html", out = "UseDataSummary.htm")


## Creating the necessary dummy variables to run the set,
## and subsetting the data so that each DiD can be run separetly

## Adding per appendix for each party group. Independent dummy.

# Cleaning data for byelections
CleanData <- UseData %>%
  filter(ifelse(byelection == 0, TRUE, FALSE))

## Calculating averages of candidates each year and for every seat.

17486/16

table(CleanData$country, CleanData$year)

8872/8
8642/8

1124/300

(224+237+253+227+243+245+246+253)/8
1080.25/241
1167/243
1112/245

100/4.54
100/4.80

CleanData2P <- CleanData %>%
  group_by(year, el_distr_code) %>% arrange(rank) %>%
  slice_head(n=2)

table(CleanData2P$rank)
table(CleanData2P$party4)

table(CleanData$female)
table(CleanData2P$female)

##Adding a binary variable for political education
CleanData <- CleanData %>%
  mutate(PolEdu = ifelse(edufield == 1 | edufield == 11, 1, 0))

##Adding a common local experience variable
CleanData <- CleanData %>%
  mutate(localExp = ifelse(is.na(pol_exp3),
                           pol_exp4, pol_exp3))
## Adding a party performance measure as a control

CleanDataSums <- CleanData %>%
  group_by(year, country) %>%
  summarise(VoteYear = sum(personal_votes, na.rm = TRUE))

CleanDataPartySums <- CleanData %>%
  group_by(year, country, party4) %>%
  summarise(NatVoteParty = sum(personal_votes, na.rm = TRUE))

CleanDataNatSums <- left_join(CleanDataPartySums, CleanDataSums, by = c("year", "country"))

CleanData <- left_join(CleanData, CleanDataNatSums, by = c("year", "country", "party4"))

CleanData <- CleanData %>%
  mutate(NatVoteShare = NatVoteParty/VoteYear)

CleanData <- CleanData %>%
  group_by(party4, year) %>%
  mutate(AvgShare = mean(vote_share))

### Making the main table over CleanData, seen in table 3.1.

stargazer(as.data.frame(CleanData), omit.summary.stat = c("p25", "p75"))

stargazer(as.data.frame(CleanData), type = "html", out = "CleanDataSummary.htm",
          title = "Dataset Overview")

save(CleanData, file ="CleanData.Rdata")

## Creating Placebo dataframe.
df00 <- CleanData %>%
  filter(year == 2000 | year == 1996)

df00 <- df00 %>%
  mutate(treatdummy = ifelse(year == 2000, 1, 0))
df00 <- df00 %>%
  mutate(Rdummy = ifelse(country == "South Korea", 1, 0))

#### This leaves the reader with the dataframes and variables necessary to
#### Run regressions and create the same plots.

