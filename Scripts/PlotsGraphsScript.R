### Plots and Graphs scripts, For both countries


## Code for summarising each year, each variation of PlotData builds on the
## Previous.

# Prbability of winning for each gender

PlotData <- CleanData %>%
  group_by(country, year, female) %>%
  summarise(pwin = mean(result), sharew = mean(female))

PlotData <- PlotData %>%
  mutate(gender = ifelse(female == 1, "Woman", "Man"))

# Share of candidates that are women

PlotData2 <- CleanData %>%
  group_by(country, year) %>%
  summarise(sharew = mean(female)) %>%
  mutate(sharetype = "Candidates")

# Winners that are women

PlotData3 <- CleanData %>%
  filter(result == 1) %>%
  group_by(country, year) %>%
  summarise(sharew = mean(female)) %>%
  mutate(sharetype = "Winners")

##Adding supplemental data from the latest elections, and merging to single
## df

PlotData2 <- rbind(PlotData2, tibble(country = c("South Korea", "Japan"),
                                     year = c(2020, 2021),
                                     sharew = c(0.1898, 0.1657),
                                     sharetype = c("Candidates", "Candidates")))


PlotData3 <- rbind(PlotData3, tibble(country = c("South Korea", "Japan"),
                                     year = c(2020, 2021),
                                     sharew = c(0.1146, 0.08304),
                                     sharetype = c("Winners", "Winners")))

# combining sharetypes so they can be plotted together

PlotData4 <- rbind(PlotData2, PlotData3)

# Plotdata for some of the other discarded hypotheses

PlotData5 <- CleanData %>%
  group_by(country, year, female) %>%
  summarise(GroupEdu = mean(education, na.rm = TRUE), GroupField = mean(PolEdu, na.rm = TRUE),
            GroupExp = mean(localExp, na.rm = TRUE), GroupLawyer = mean(lawyerexp, na.rm = TRUE),
            Groupprevshare = mean(prevshare, na.rm = TRUE)) %>%
  mutate(gender = ifelse(female == 1, "Women", "Men"))



## Part 1 Demonstrating the trend of share of candidates. Creats plot 4.1

ggplot(data = PlotData2, aes(x = year, y = sharew)) +
  geom_point() +
  geom_line(aes(linetype = country)) +
  theme_bw() +
  labs(title = "", x = "",
       y = "Share Women") +
  geom_vline(xintercept = 2004, linetype = "dashed") +
  scale_x_continuous(breaks = c(1988, 1992, 1996, 2000,  2004, 2008, 2012,
                                2016, 2020))

## Part 2, Share of winnes that are women. Creates plot 4.2

ggplot(PlotData3, aes(x = year, y = sharew)) +
  geom_point() +
  geom_line(aes(linetype = country)) +
  theme_bw() +
  geom_vline(xintercept = 2004, linetype = "dashed") +
  labs(title = "", x = "",
       y = "Share Women") +
  scale_x_continuous(breaks = c(1988, 1992, 1996, 2000,  2004, 2008, 2012,
                                2016, 2020))


## Part 3 Demonstrating the difference in winners and candidates for both
## countries. Makes figure 4.3


ggplot(data = PlotData4, aes(x = year, y = sharew, col = sharetype)) +
  geom_point() +
  geom_line(size = 1) +
  theme_bw() +
  geom_vline(xintercept = 2004, linetype = "dashed") +
  labs(title = "",
       x = "", y = "Share Women") +
  facet_wrap(vars(country)) +
  scale_x_continuous(breaks = c(1988, 1996, 2004, 2012, 2020))



## Part 4 Demonstrating the difference in probability of being elected


ggplot(PlotData, aes(x = year, y = pwin, col = gender)) +
  geom_point() +
  geom_line(size = 1) + theme_bw() +
  geom_vline(xintercept = 2004, linetype = "dashed") +
  facet_wrap(vars(country)) +
  labs(title = "",
       x = "", y = "Probability of Winning") +
  scale_x_continuous(breaks = c(1988, 1996, 2004, 2012, 2020))

## Part 4 Demonstrating differences in placement, average vote share in
## relevant district in previous election. Creates plot 4.12

ggplot(PlotData5, aes(x = year, y = Groupprevshare, col = gender)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title ="", x = "",
       y = "Previous performance") +
  geom_vline(xintercept = 2004, linetype = "dashed") +
  facet_wrap(vars(country)) +
  scale_x_continuous(breaks = c(1988, 1996, 2004, 2012))

## Part 5 Demonstrating electoral re-election rates.

# Creating dataframes for the amount of winners that are incumbents in the same
# district they run in

Relectframe <- CleanData %>%
  filter(inc_exactly == 1) %>%
  group_by(year, country) %>%
  summarise(RelectionRate = mean(result))

Relectframe2 <- CleanData %>%
  filter(result == 1) %>%
  group_by(year, country) %>%
  summarise(OldLegis = mean(inc_exactly))

## Winners that are incumbents, Not used.

ggplot(Relectframe2 %>%
         filter(year != 1988), aes(x = year, y= OldLegis, col = country)) +
  geom_point() +
  geom_line() +
  labs(title = "5.1 Share of winners that are incumbent", y = "Incumbency Share") +
  theme_bw()

## Re-election rates Creates figure 5.1

ggplot(Relectframe %>%
         filter(year != 1988), aes(x = year, y= RelectionRate, col = country)) +
  geom_point() +
  geom_line() +
  labs(title = "", y = "Win Rate") +
  theme_bw() +
  scale_x_continuous(breaks = c(1992, 1996, 2000, 2004, 2008, 2012, 2016, 2020))

## Number of re-elected average for each country

RelectAvg <- Relectframe2 %>%
  group_by(country) %>%
  summarise(avgrelect = mean(OldLegis))

t.test(OldLegis ~ as.factor(country),
       data = Relectframe2,
       alternative = "two.sided",
       var.equal = TRUE)

t.test(RelectionRate ~ as.factor(country),
       data = Relectframe,
       alternative = "two.sided",
       var.equal = TRUE)

## t- test returns a p-value of 0.7408, not statistically significant


#### Appendix figures

### Quality Measures

## Educational attainment, creats the figures in Appendix section on discarded
## Quality measures.

#Education level
ggplot(PlotData5, aes(x = year, y = GroupEdu, col = gender)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title ="Average Education Divided by Gender", x = "",
       y = "Education Level") +
  facet_wrap(vars(country)) +
  scale_x_continuous(breaks = c(1992, 2000, 2008, 2016)) +
  geom_vline(xintercept = 2004, linetype = "dashed")

ggplot(CleanData, aes(x = year, y = education)) +
  stat_summary(geom = "pointrange", size = 1, color = "red",
               fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  facet_wrap(vars(country)) +
  labs(title = "Average Education Level For all Candidates", x = "", y = "Educational level") +
  theme_bw() +
  scale_x_continuous(breaks = c(1992, 2000, 2008, 2016)) +
  geom_vline(xintercept = 2004, linetype = "dashed")

# Political Education

ggplot(PlotData5, aes(x = year, y = GroupField, col = gender)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title ="Share of Education field", x = "",
       y = "Share of candidates with Political/Law education") +
  facet_wrap(vars(country))

# Local office experience

ggplot(PlotData5, aes(x = year, y = GroupExp, col = gender)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  labs(title ="Share of candidates with elected experience By Gender", x = "",
       y = "Share of candidates with experience in elected office") +
  geom_vline(xintercept = 2004, linetype = "dashed") +
  facet_wrap(vars(country)) +
  scale_x_continuous(breaks = c(1992, 2000, 2008, 2016))


ggplot(CleanData, aes(x = year, y = localExp)) +
  stat_summary(geom = "pointrange", size = 1, color = "red",
               fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  facet_wrap(vars(country)) +
  labs(title = "Share of Candidates with Local Exp", x = "",
       y = "Share Local Experience") +
  theme_bw() +
  scale_x_continuous(breaks = c(1992, 2000, 2008, 2016))


