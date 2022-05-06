#### ----Mechanisms Check ------

library(tidyverse)
library(tidylog)
library(haven)
library(readxl)
library(stargazer)
library(estimatr)
library(cowplot)

setwd("C:/Users/gardi/OneDrive/Documents/MA thesis/Data and R scripts/Survey Data")

JapanWave3 <- readxl::read_xlsx("F00008108-WV3_Data_Japan_Excel_v20221107.xlsx")

JapanWave4 <- readxl::read_xlsx("F00008001-WV4_Data_Japan_Excel_v20201117.xlsx")

JapanWave5 <- readxl::read_xlsx("F00007848-WV5_Data_Japan_Excel_v20201117.xlsx")

JapanWave6 <- readxl::read_xlsx("F00007597-WV6_Data_Japan_Excel_v20201117.xlsx")

SKWave3 <- readxl::read_xlsx("F00008129-WV3_Data_South_Korea_Excel_v20221107.xlsx")

SKWave4 <- readxl::read_xlsx("F00008018-WV4_Data_South_Korea_Excel_v20201117.xlsx")

SKWave5 <- readxl::read_xlsx("F00007866-WV5_Data_South_Korea_Excel_v20201117.xlsx")

SKWave6 <- readxl::read_xlsx("F00007621-WV6_Data_South_Korea_Excel_v20201117.xlsx")

JapanWave3sel <- JapanWave3 %>%
  select(gender = `V214: Sex`, PartyMember = `V32: Active/Inactive membership of political party`,
         PolDisc = `V37: How often discusses political matters with friends`,
         year = `V238: Year survey`, interest = `V117: Interest in politics`,
         whostility = `V101: Men make better political leaders than women do`)


SKWave3sel <- SKWave3 %>%
  select(gender = `V214: Sex`, PartyMember = `V32: Active/Inactive membership of political party`,
         PolDisc = `V37: How often discusses political matters with friends`,
         year = `V238: Year survey`, interest = `V117: Interest in politics`,
         whostility = `V101: Men make better political leaders than women do`)

JapanWave4sel <- JapanWave4 %>%
  select(gender = `V223: Sex`,
         PolDisc = `V32: How often discusses political matters with friends`,
         year = `V246: Year survey`, interest = `V133: Interest in politics`,
         whostility = `V118: Men make better political leaders than women do`)


SKWave4sel <- SKWave4 %>%
  select(gender = `V223: Sex`,
         PolDisc = `V32: How often discusses political matters with friends`,
         year = `V246: Year survey`, interest = `V133: Interest in politics`,
         whostility = `V118: Men make better political leaders than women do`)

JapanWave5sel <- JapanWave5 %>%
  select(gender = `V235: Sex`, PartyMember = `V28: Active/Inactive membership of political party`,
        interest = `V95: Interest in politics`,
         whostility = `V61: Men make better political leaders than women do`,
         voted = `V234: Voted in recent parliament elections`)


SKWave5sel <- SKWave5 %>%
  select(gender = `V235: Sex`, PartyMember = `V28: Active/Inactive membership of political party`,
        interest = `V95: Interest in politics`,
         whostility = `V61: Men make better political leaders than women do`,
         voted = `V234: Voted in recent parliament elections`)

JapanWave6sel <- JapanWave6 %>%
  select(gender = `V240: Sex`, PartyMember = `V29: Active/Inactive membership of political party`,
         interest = `V84: Interest in politics`, whostility = `V51: Men make better political leaders than women do`,
         voted = `V227: Vote in elections: National level`, year = `V262: Year survey`)

SKWave6sel <- SKWave6 %>%
  select(gender = `V240: Sex`, PartyMember = `V29: Active/Inactive membership of political party`,
         interest = `V84: Interest in politics`, whostility = `V51: Men make better political leaders than women do`,
         voted = `V227: Vote in elections: National level`, year = `V262: Year survey`)



JapanWave5sel$year <- c(2005)
SKWave5sel$year <- c(2005)

mean(SKWave3sel$whostility[which(SKWave3sel$whostility>0)])
mean(JapanWave3sel$whostility[which(JapanWave3sel$whostility>0)])

mean(SKWave4sel$whostility[which(SKWave4sel$whostility>0)])
mean(JapanWave4sel$whostility[which(JapanWave4sel$whostility>0)])

mean(SKWave5sel$whostility[which(SKWave5sel$whostility>0)])
mean(JapanWave5sel$whostility[which(JapanWave5sel$whostility>0)])

JapanSurvD <- bind_rows(JapanWave3sel, JapanWave4sel, JapanWave5sel, JapanWave6sel)
JapanSurvD$country <- c("Japan")

SKSurvD <- bind_rows(SKWave3sel, SKWave4sel, SKWave5sel, SKWave6sel)
SKSurvD$country <- c("South Korea")

SurvData <- bind_rows(JapanSurvD, SKSurvD)

rm(SKWave3, SKWave4, SKWave5, SKWave6, SKWave3sel, SKWave4sel, SKWave5sel,
   SKWave6sel, JapanWave3, JapanWave4, JapanWave5, JapanWave6, JapanWave3sel,
   JapanWave4sel, JapanWave5sel, JapanWave6sel, JapanSurvD, SKSurvD)

### Summarising info and plotting it
## filtering out missing

SurvDataClean <- SurvData %>%
  filter(whostility>0)

NatSurvData <- SurvDataClean %>%
  group_by(year, country) %>%
  summarise(WpoliticNat = mean(whostility))

groupedsurv <- SurvDataClean %>%
  group_by(year, country, gender) %>%
  summarise(Antiwpolitic = mean(whostility))

groupedsurv <- groupedsurv %>%
  mutate(Gender = ifelse(gender == 2, "Women", "Men"))

ggplot(groupedsurv, aes(x = year, y = Antiwpolitic, col = Gender)) +
  geom_point() +
  geom_line(size = 1) +
  labs(title = "x.6: Opinions on Women's suitability as politicians, by gender",
       x = "", y = "Average Respondent score") +
  facet_wrap(vars(country)) +
  theme_bw()


ggplot(NatSurvData, aes(x = year, y = WpoliticNat)) +
  geom_point() +
  geom_line(aes(linetype = country)) +
  labs(title = "x.5: Opinions on Womens' suitability as politicians",
       x = "", y = "Average Respondent Score") +
  theme_bw()

## DIDID estimator

df1 <- SurvDataClean %>%
  filter(year == 1995 | year == 1996 | year == 2000 | year == 2001)

df1 <- df1 %>%
  mutate(Rdummy = ifelse(country == "South Korea", 1, 0),
         tdummy = ifelse(year == 2000 | year == 2001, 1, 0),
         female = ifelse(gender == 2, 1, 0))

df2 <- SurvDataClean %>%
  filter(year == 2000 | year == 2001 | year == 2005)

df2 <- df2 %>%
  mutate(Rdummy = ifelse(country == "South Korea", 1, 0),
         tdummy = ifelse(year == 2005, 1, 0),
         female = ifelse(gender == 2, 1, 0))

df3 <- SurvDataClean %>%
  filter(year == 2005 | year == 2010)

df3 <- df3 %>%
  mutate(Rdummy = ifelse(country == "South Korea", 1, 0),
         tdummy = ifelse(year == 2010, 1, 0),
         female = ifelse(gender == 2, 1, 0))

## Regression models

M200lm <- lm(whostility ~ Rdummy + tdummy + female + Rdummy*tdummy + Rdummy*female +
               tdummy*female + Rdummy*tdummy*female, data = df1)

M205lm <- lm(whostility ~ Rdummy + tdummy + female + Rdummy*tdummy + Rdummy*female +
               tdummy*female + Rdummy*tdummy*female, data = df2)

M210lm <- lm(whostility ~ Rdummy + tdummy + female + Rdummy*tdummy + Rdummy*female +
               tdummy*female + Rdummy*tdummy*female, data = df3)

stargazer(M200lm, M205lm, M210lm, title = "Mechanism Table 2", type = "text")

## Compare results without triple difference

M2b00lm <- lm(whostility ~ Rdummy + tdummy + Rdummy*tdummy, data = df1)

M2b05lm <- lm(whostility ~ Rdummy + tdummy + Rdummy*tdummy, data = df2)

M2b10lm <- lm(whostility ~ Rdummy + tdummy + Rdummy*tdummy, data = df3)

stargazer(M2b00lm, M2b05lm, M2b10lm, title = "Regression Table 3",
          dep.var.labels = c("Less Hostility"), column.labels = c("95-00", "00-05", "05-10"),
          covariate.labels = c("Region Dummy", "Time Dummy",
                               "Interaction", "Constant"), type = "html",
          out = "ModelsGraphsSurvey/RegressionTableWVSHostility.htm")

?stargazer

### Aggregate data



