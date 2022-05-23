### Regression Script

DiDset <- CleanData %>%
  mutate(treatdummy = ifelse(year >= 2004, 1, 0),
         countdummy1 = ifelse(year == 2004 | year == 2005, 1, 0),
         countdummy2 = ifelse(year == 2008 | year == 2009, 1, 0),
         countdummy3 = ifelse(year == 2012, 1, 0),
         countdummy4 = ifelse(year == 2016 | year == 2017, 1, 0))

DiDset <- DiDset %>%
  mutate(Rdummy = ifelse(country == "South Korea", 1, 0))


#Creates a dataset where the time dummy is 1 for after 2004, and South Korea
# Has Rdummy = 1.

## First the DiD for more candidates.

#This is the Linear Probability Model found in the Appendix, for candidates

DiDc <- lm(female ~ Rdummy + treatdummy + Rdummy*treatdummy, data = DiDset)

DiDcplac <- lm(female ~ Rdummy + treatdummy + Rdummy*treatdummy, data = df00)

stargazer(DiDc, DiDcplac, type = "text")

stargazer(DiDc, DiDcplac, type = "html", out = "Graphs and Tables/DiDc.htm",
          dep.var.labels = "Women", column.labels = c("Model", "Placebo"),
          covariate.labels = c("Region Dummy", "Time Dummy", "DiD"), title = "Regression Table 1")

## DiD estimation for Candidates but with logit regression, creates regression
## seen in table 4.1

DiDcB <- glm(female ~ Rdummy + treatdummy + Rdummy*treatdummy, data = DiDset,
             family = "binomial")

DiDcplacB <- glm(female ~ Rdummy + treatdummy + Rdummy*treatdummy, data = df00,
                 family = "binomial")

stargazer(DiDcB, DiDcplacB, covariate.labels = c("Region Dummy", "Time Dummy", "DiD"),
          title = "Regression Table 1", column.labels = c("Estimation", "Placebo"),
          type = "html", out = "Graphs and Tables/DiDWithBinomial.htm",
          dep.var.labels = "Probability that a Candidate is a woman")

stargazer(DiDcB, DiDcplacB, covariate.labels = c("Region Dummy", "Time Dummy", "DiD"),
          title = "DiD Estimation for Women Candidates", column.labels = c("Model 1", "Placebo"),
          type = "latex",
          dep.var.labels = "Probability that a Candidate is a woman")

# Predicted probabilities, this creates the information in table 4.2

Predictframe <- tibble(treatdummy = c(0,0,1,1),
                       Rdummy = c(0,1,0,1))

Predictframe$predictedprob <- DiDcB %>%
  predict(Predictframe)

Predictframe$probabilities <- logit2prob(Predictframe$predictedprob)

# The results of this prediction leads us to mean predicted probabilities
# in each observed instance. Which roughly corresponds to the actual averages

# Without treatment probability

-1.972 - 1.949 + 0.240

-1.972 + 0.240 + 1.330

logittreat <- c(-3.681, -0.402)

logit2prob(logittreat)

# Predicted without treatment for SK is 0.02457844,
# predicted with treatment for JP is 40.083191

## Predicting CI at 95%

ilink1 <- DiDcB$family$linkinv

ndata1 <- bind_cols(Predictframe, setNames(as_tibble(predict(DiDcB,
                                                               Predictframe,
                                                               se.fit = TRUE)[1:2]),
                                             c("fit_link", "se_link")))

ndata1 <- mutate(ndata1,
                fit_resp  = ilink1(fit_link),
                right_upr = ilink1(fit_link + (2 * se_link)),
                right_lwr = ilink1(fit_link - (2 * se_link)))

## Upper and lower SK estimates

summary(DiDcB)

#Upper log odds

log(0.09619657/(1-0.09619657)) - (1.33008 - (2*0.13705))

#Lower

log(0.07860396/(1-0.07860396)) - (1.33008 + (2*0.13705))

CIprobSK1 <- c(-3.296198, -4.065648)

# CI in probability

logit2prob(CIprobSK1)

### CI for Japan

# Upper log odds

log(0.16033546/(1-0.16033546)) + (1.33008 + (2*0.13705))

# Lower

log(0.14075216/(1-0.14075216)) + (1.33008 - (2*0.13705))

CIprobJP1 <- c(-0.05155421, -0.7530768)

# CI 95%

logit2prob(CIprobJP1)

## DDD in winning chances, this creates the data seen in regression table 2,
## table 4.3.

DDDw <- glm(result ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
              Rdummy*female + Rdummy*female*treatdummy + NatVoteShare,
            data = DiDset, family = "binomial")

DDDwAvg <- glm(result ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
                 Rdummy*female + Rdummy*female*treatdummy + AvgShare,
               data = DiDset, family = "binomial")

DDDwplac <- glm(result ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
                  Rdummy*female + Rdummy*female*treatdummy + NatVoteShare,
                data = df00, family = "binomial")

DDDwAvgplac <- glm(result ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
                     Rdummy*female + Rdummy*female*treatdummy + AvgShare,
                   data = df00, family = "binomial")

stargazer(DDDwAvg, DDDw, type = "text")

stargazer(DDDwAvg, DDDwAvgplac, type = "text")

stargazer(DDDwAvg, DDDwAvgplac, type = "html", out = "Graphs and Tables/DDDwinAvg.htm",
          dep.var.labels = "Win", column.labels = c("Model", "Placebo"),
          covariate.labels = c("Region Dummy", "Time Dummy", "Female",
                               "Party Performance",
                               "DiD",
                               "Region x Female",
                               "Time x Female", "DiD x Female"),
          title = "Regression table 2")

stargazer(DDDwAvg, DDDwAvgplac, type = "latex",
          dep.var.labels = "Win in SMD", column.labels = c("Model 2", "Placebo"),
          covariate.labels = c("Region Dummy", "Time Dummy", "Female",
                               "Party Performance",
                               "DiD",
                               "Region x Female",
                               "Time x Female", "DiD x Female"),
          title = "DiD with Gender Interaction")

## The following code checks the Linear Probability performance, creates the
## table in the appendix for more winner sin linear probability.

DDDwlm <- lm(result ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
               Rdummy*female + Rdummy*female*treatdummy + NatVoteShare,
             data = DiDset)

DDDwAvglm <- lm(result ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
                  Rdummy*female + Rdummy*female*treatdummy + AvgShare,
                data = DiDset)

DDDwplaclm <- lm(result ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
                   Rdummy*female + Rdummy*female*treatdummy + NatVoteShare,
                 data = df00)

DDDwAvgplaclm <- lm(result ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
                      Rdummy*female + Rdummy*female*treatdummy + AvgShare,
                    data = df00)

stargazer(DDDwlm, DDDwAvglm, DDDwplaclm, DDDwAvgplaclm, type = "text")

stargazer(DDDwAvglm, DDDwAvgplaclm, type = "html", dep.var.labels = "Winning SMD Seat",
          title = "Linear Probability Model for Success", column.labels = c("Model", "Placebo"),
          covariate.labels = c("Region Dummy", "Time Dummy", "Female", "Party Performance",
                               "DiD", "Region x Female", "Time x Female", "DiD x Female"),
          out = "Graphs and Tables/DDDLinearProb.htm")

## Estimating treatment effect on the basis of the glm logit models. This info
## creates the table 4.4 on predicted probability.

## predicted probabilities

mean(DiDset$AvgShare, na.rm = TRUE)

PredictframeDDD <- tibble(treatdummy = c(0,0,0,0,1,1,1,1),
                          Rdummy = c(0,0,1,1,0,0,1,1),
                          female = c(0,1,0,1,0,1,0,1),
                          AvgShare = c(0.2451335,0.2451335,0.2451335,0.2451335,
                                       0.2451335,0.2451335,0.2451335,0.2451335))


predict.glm(DDDwAvg, newdata = PredictframeDDD, type = "response",
            se.fit = TRUE)

PredictframeDDD$predictlogit <- DDDwAvg %>%
  predict(PredictframeDDD)

PredictframeDDD$predictprob <- logit2prob(PredictframeDDD$predictlogit)


ilink <- DDDwAvg$family$linkinv

ndata <- bind_cols(PredictframeDDD, setNames(as_tibble(predict(DDDwAvg,
                                                               PredictframeDDD,
                                                               se.fit = TRUE)[1:2]),
                                             c("fit_link", "se_link")))

ndata <- mutate(ndata,
                fit_resp  = ilink(fit_link),
                right_upr = ilink(fit_link + (2 * se_link)),
                right_lwr = ilink(fit_link - (2 * se_link)))

## Predicted without treatment values for SK and with treatment values for JP
summary(DDDwAvg)
#Sk values, for men and women

#Estimate
log(0.14445328/(1-0.14445328)) - (-0.31560) - (1.25561)

logit2prob(-2.718795)
#Lower Women SK
log(0.10656759/(1-0.10656759)) - (-0.31560) - (1.25561)

#Upper women SK
log(0.1928997/(1-0.1928997)) - (-0.31560) - (1.25561)

logit2prob(-3.066301)
logit2prob(-2.371288)

#SK Men estimate
log(0.14300494/(1-0.14300494)) - (-0.31560)

logit2prob(-1.474953)

#Lower Men SK

log(0.13073471/(1-0.13073471)) - (-0.31560)

logit2prob(-1.578878)

#Upper men SK

log(0.1562198/(1-0.1562198)) - (-0.31560)

logit2prob(-1.371028)


## JP

#Women Estimate JP

log(0.05729677/(1-0.05729677)) + (-0.31560) + (1.25561)

logit2prob(-1.860497)

# Upper Women JP

log(0.0734769/(1-0.0734769)) + (-0.31560) + (1.25561)

logit2prob(-1.594458)


#Lower women JP

log(0.04450847/(1-0.04450847)) + (-0.31560) + (1.25561)

logit2prob(-2.126536)

## Men estimate JP

log(0.11507666/(1-0.11507666)) + (-0.31560)

logit2prob(-2.355503)

#Upper Men JP

log(0.1260366/(1-0.1260366)) + (-0.31560)

logit2prob(-2.252066)

#Lower Men JP

log(0.10495531/(1-0.10495531)) + (-0.31560)

logit2prob(-2.458939)


##### To creat individual estimations for the countries, I separate them.

SkCleanData <- CleanData %>%
  filter(country == "South Korea")

JpCleanData <- CleanData %>%
  filter(country == "Japan")

### Following creates the final models and estimations, reported in table 5.1

M3lmAvg <- glm(result ~ AvgShare + female + ImpPRexp + female*ImpPRexp + inc_exactly,
               data = SkCleanData, family = "binomial")

M3lmJAvg <-glm(result ~ AvgShare + female + ImpPRexp + female*ImpPRexp + inc_exactly,
               data = JpCleanData, family = "binomial")

M3lmLAvg <- glm(result ~ AvgShare + female + ImpPRexp + female*ImpPRexp + inc_exactly +
                  prevshare,
                data = SkCleanData, family = "binomial")

M3lmJLAvg <- glm(result ~ AvgShare + female + ImpPRexp + female*ImpPRexp + inc_exactly +
                   prevshare,
                 data = JpCleanData, family = "binomial")

stargazer(M3lmAvg, M3lmLAvg, M3lmJAvg, M3lmJLAvg, type = "text")

stargazer(M3lmAvg, M3lmLAvg, M3lmJAvg, M3lmJLAvg,
          column.labels = c("South Korea", "South Korea", "Japan", "Japan"), dep.var.labels = c("Winning SMD seat"),
          covariate.labels = c("Party Performance", "Female", "PR Experience",
                               "Incumbent", "Previous Share", "PR Experience x Female", "Constant"),
          title = "Regression table x", type = "html", out = "Graphs and Tables/TotRegression.htm")

stargazer(M3lmAvg, M3lmLAvg, M3lmJAvg, M3lmJLAvg,
          column.labels = c("South Korea", "South Korea", "Japan", "Japan"), dep.var.labels = c("Winning SMD seat"),
          covariate.labels = c("Party Performance", "Female", "PR Experience",
                               "Incumbent", "Previous Share", "PR Experience x Female", "Constant"),
          title = "Estimating Chances of winning", type = "latex")


## Assessing the model using predicted values
#For South Korea

probabilities <- M3lmLAvg %>%
  predict(SkCleanData, type = "response")

predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

mean(predicted.classes == SkCleanData$result, na.rm = TRUE)

#For Japan

probabilitiesJP <- M3lmJLAvg %>%
  predict(JpCleanData, type = "response")

predicted.classesJP <- ifelse(probabilitiesJP > 0.5, 1, 0)

mean(predicted.classesJP == JpCleanData$result, na.rm = TRUE)

table(predicted.classes, SkCleanData$result)

## Predicted probability accuracy of 84% for both


#### Linear Probability Models

M4lmAvg <- lm(result ~ AvgShare + female + PRexp + female*PRexp + inc_exactly,
              data = SkCleanData)

M4lmJAvg <- lm(result ~ AvgShare + female + PRexp + female*PRexp + inc_exactly,
               data = JpCleanData)

M4lmLAvg <- lm(result ~ AvgShare + female + PRexp + female*PRexp + inc_exactly +
                 prevshare,
               data = SkCleanData)

M4lmJLAvg <- lm(result ~ AvgShare + female + PRexp + female*PRexp + inc_exactly +
                  prevshare,
                data = JpCleanData)


stargazer(M4lmAvg, M4lmLAvg, M4lmJAvg, M4lmJLAvg, type = "text")

#### DDD for women's placements violation of parallel trends, reported in
#### Appendix

DDDsp <- lm(prevshare ~ female + Rdummy + female*Rdummy + treatdummy + AvgShare +
              treatdummy*female + treatdummy*female*Rdummy, data = DiDset)

DDDspplac <- lm(prevshare ~ female + Rdummy + female*Rdummy + treatdummy + AvgShare +
                  treatdummy*female + treatdummy*female*Rdummy, data = df00)


stargazer(DDDsp, DDDspplac, type = "text")

table(is.na(DiDset$prevshare), DiDset$year, DiDset$female, DiDset$country)

### Local regression, demonstrating the slope number, not significant itself
##

LRdata <- PlotData2 %>%
  mutate(yr = year - 2004)

LRpreSK <- lm(sharew ~ yr, data = LRdata %>%
                filter(country == "South Korea", yr < 0))

LRpreJP <- lm(sharew ~ yr, data = LRdata %>%
                filter(country == "Japan", yr < 0))


LRpostSK <- lm(sharew ~ yr, data = LRdata %>%
                 filter(country == "South Korea", yr >= 0))

LRpostJP <- lm(sharew ~ yr, data = LRdata %>%
                 filter(country == "Japan", yr >= 0))

stargazer(LRpreSK, LRpreJP, LRpostSK, LRpostJP, type = "html",
          column.labels = c("SK Pre", "JP Pre", "SK post", "JP post"),
          omit = c("F Statistic"),
          dep.var.labels = "Candidates",
          covariate.labels = c("Year Effect", "Constant"),
          out = "Graphs and Tables/LRCandidates.htm")

ggplot(PlotData2, aes(x = year, y = sharew)) +
  geom_point() +
  facet_wrap(vars(country)) +
  theme_bw() +
  geom_smooth(data = PlotData2 %>% filter(year >= 2004), aes(x = year, y = sharew),
              method = "lm", se = FALSE) +
  geom_smooth(data = PlotData2 %>% filter(year < 2004), aes(x = year, y = sharew),
              method = "lm", se = FALSE) +
  geom_vline(xintercept = 2004, linetype = "dashed") +
  labs(title = "Local Regression of Candidates increase", y = "Share Women",
       x = "")

##### Discarded Regressions, produces the output seen in some parts of the
##### Appendix.


### Robust regressions, used on candidates.

DiDcRob <- lm_robust(female ~ Rdummy + treatdummy + Rdummy*treatdummy,
                     data = DiDset, clusters = country)

summary(DiDcRob)


### Regressions for Quality, creates regression models found in the appendix

## Increased level of education


Edulm <- lm(education ~ Rdummy + treatdummy + Rdummy*treatdummy,
            data = DiDset)

Edulmplac <- lm(education ~ Rdummy + treatdummy + Rdummy*treatdummy,
                data = df00)

stargazer(Edulm, Edulmplac, type = "html",
          dep.var.labels = "Education Level",
          column.labels = c("Model", "Placebo"),
          covariate.labels = c("Region Dummy", "Time Dummy", "DiD", "Constant"),
          out = "Graphs and Tables/DiDeducation.htm")

## With Gender interactions.


Edulmg <- lm(education ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
               female*Rdummy + female*treatdummy + female*Rdummy*treatdummy,
            data = DiDset)

Edulmpglac <- lm(education ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
                   female*Rdummy + female*treatdummy + female*Rdummy*treatdummy,
                data = df00)

stargazer(Edulmg, Edulmpglac, type = "html", dep.var.labels = "Education Level",
          column.labels = c("Model", "Placebo"),
          covariate.labels = c("Region Dummy", "Time Dummy", "Female",
                               "DiD", "Female x Region", "Female x Time",
                               "DiD x Gender", "Constant"),
          out = "Graphs and Tables/DDDeducationapp.htm")


### Educational Field

PolEdulm <- lm(PolEdu ~ Rdummy + treatdummy + Rdummy*treatdummy,
            data = DiDset)

PolEdulmplac <- lm(PolEdu ~ Rdummy + treatdummy + Rdummy*treatdummy,
                data = df00)

stargazer(PolEdulm, PolEdulmplac, type = "html",
          dep.var.labels = "Political Education",
          column.labels = c("Model", "Placebo"),
          covariate.labels = c("Region Dummy", "Time Dummy", "DiD", "Constant"),
          out = "Graphs and Tables/DiDPolEduAppendix.htm")

## With Gender interactions.


PolEdulmg <- lm(PolEdu ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
               female*Rdummy + female*treatdummy + female*Rdummy*treatdummy,
             data = DiDset)

PolEdulmpglac <- lm(PolEdu ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
                   female*Rdummy + female*treatdummy + female*Rdummy*treatdummy,
                 data = df00)

stargazer(PolEdulmg, PolEdulmpglac, type = "html", dep.var.labels = "Political Education",
          column.labels = c("Model", "Placebo"),
          covariate.labels = c("Region Dummy", "Time Dummy", "Female",
                               "DiD", "Female x Region", "Female x Time",
                               "DiD x Gender", "Constant"),
          out = "Graphs and Tables/DDDPoleducationapp.htm")


## DiD for Local Experience as dependent variable.
### Educational Field

LocalExplm <- lm(localExp ~ Rdummy + treatdummy + Rdummy*treatdummy,
               data = DiDset)

LocalExplmplac <- lm(localExp ~ Rdummy + treatdummy + Rdummy*treatdummy,
                   data = df00)

stargazer(LocalExplm, LocalExplmplac, type = "html",
          dep.var.labels = "Local Experience",
          column.labels = c("Model", "Placebo"),
          covariate.labels = c("Region Dummy", "Time Dummy", "DiD", "Constant"),
          out = "Graphs and Tables/DiDLocalExpAppendix.htm")

## With Gender interactions.


LocalExplmg <- lm(localExp ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
                  female*Rdummy + female*treatdummy + female*Rdummy*treatdummy,
                data = DiDset)

LocalExplmpglac <- lm(localExp ~ Rdummy + treatdummy + female + Rdummy*treatdummy +
                      female*Rdummy + female*treatdummy + female*Rdummy*treatdummy,
                    data = df00)

stargazer(LocalExplmg, LocalExplmpglac, type = "html", dep.var.labels = "Local Experience",
          column.labels = c("Model", "Placebo"),
          covariate.labels = c("Region Dummy", "Time Dummy", "Female",
                               "DiD", "Female x Region", "Female x Time",
                               "DiD x Gender", "Constant"),
          out = "Graphs and Tables/DDDLocalExpapp.htm")

