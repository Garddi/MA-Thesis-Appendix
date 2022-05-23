######## South Korean plot data for info on women #########


## Creating a limited dataframe based on dataframes from the Master script

ImpMechanismCheck1 <- MergedKR8 %>%
  filter(win == 1, female.x == 1)
ImpMechanismCheck1 <- ImpMechanismCheck1 %>%
  mutate(ImpPropExp = ifelse(ImpPRexp == 1, "Yes", "No"))
ImpMechanismCheck1 <- ImpMechanismCheck1 %>%
  mutate(ImpPropExp2 = factor(ImpPropExp, levels = c("Yes", "No")))
ImpMechanismCheck1 <- ImpMechanismCheck1 %>%
  group_by(year) %>%
  add_count()


## Adding latest elections with supplemented data from NEC.

SK2020DataExp <- data.frame(year = c(2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020,
                                     2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020,
                                     2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020,
                                     2020, 2020), female.x = c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                               1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                            PropExp2 = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
                                         "Yes", "Yes", "Yes", "Yes", "Yes", "No",
                                         "No", "No", "No", "No", "No", "No", "No",
                                         "No", "No", "No", "No", "No", "No", "No",
                                         "No", "No", "No"),
                            ImpPropExp2 = c("Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
                                            "Yes", "Yes", "Yes", "Yes", "Yes", "No",
                                            "No", "No", "No", "No", "No", "No", "No",
                                            "No", "No", "No", "No", "No", "No", "No",
                                            "No", "No", "No"),
                            PRexp = c(1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
                            ImpPRexp = c(1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))

SK2020DataExp <- SK2020DataExp %>%
  mutate(PropExp2 = factor(PropExp2, levels = c("Yes", "No")),
         ImpPropExp2 = factor(ImpPropExp2, levels = c("Yes", "No")))


ImpMechanismCheck1 <- rbind(ImpMechanismCheck1, SK2020DataExp)

## Filtering to only general elections, and filling in NA for 0 values

##
ImpMechanismCheck1b <- ImpMechanismCheck1 %>%
  filter(year == 1996 | year == 2000 | year == 2004 | year == 2008 |
           year == 2012 | year == 2016 | year == 2020) %>%
  group_by(year) %>%
  summarise(Elected = sum(female.x), Experience = sum(ImpPRexp))

ImpMechanismCheck1b$NoExperience <- ImpMechanismCheck1b$Elected - ImpMechanismCheck1b$Experience

ImpMechanismCheck1b$Experience[ImpMechanismCheck1b$Experience == 0] <- NA

## Plot for demonstrating winners that have been elected to PR before.
## Creates plot, 4.10

ExpShareMech <- ggplot(ImpMechanismCheck1 %>%
         filter(year == 1996 | year == 2000 | year == 2004 |
                  year == 2008 | year == 2012 | year == 2016 | year == 2020), aes(x = year)) +
  geom_bar(aes(fill = ImpPropExp2)) +
  labs(title = "", y = "Number of Women",
       fill = "PR Experience:", x = "") +
  geom_text(data = ImpMechanismCheck1b, aes(x = year, y = NoExperience/2, label = NoExperience)) +
  geom_text(data = ImpMechanismCheck1b, aes(x = year, y = NoExperience + Experience/2, label = Experience)) +
  theme_bw() +
  scale_x_continuous(breaks = c(1996, 2000, 2004, 2008, 2012, 2016, 2020)) +
  theme(legend.position = "top")

ggsave("Graphs and Tables/WomenWithExp.png", plot = ExpShareMech)

### Demonstrating the connection between PR experience and gender,
### Plot was discarded due to women being too small to see properly on the
### Graph.

ImpMechanismCheck2 <- MergedKR8 %>%
  mutate(ImpPropExp = ifelse(ImpPRexp == 1, "Yes", "No"))

ImpMechanismCheck2 <- ImpMechanismCheck2 %>%
  mutate(ImpPropExp2 = factor(ImpPropExp, levels = c("Yes","No")))
ImpMechanismCheck2 <- ImpMechanismCheck2 %>%
  group_by(year) %>%
  add_count()
ImpMechanismCheck2 <- ImpMechanismCheck2 %>%
  mutate(gender = ifelse(female.x == 1, "Women", "Men"))


ImpMechanismCheck2b <- ImpMechanismCheck2 %>%
  filter(year == 1996 | year == 2000 | year == 2004 | year == 2008 |
           year == 2012 | year == 2016 | year == 2020) %>%
  group_by(year) %>%
  summarise(Elected = mean(n), Experience = sum(ImpPRexp))

ImpMechanismCheck2b$NoExperience <- ImpMechanismCheck2b$Elected - ImpMechanismCheck2b$Experience

CandExpShare <- ggplot(ImpMechanismCheck2 %>%
         filter(year == 1996 | year == 2000 | year == 2004 |
                  year == 2008 | year == 2012 | year == 2016 | year == 2020), aes(x = year)) +
  geom_bar(aes(fill = ImpPropExp2)) +
  labs(title = "Number of candidates with experience in PR, by gender", y = "Number of Candidates",
       fill = "PR Experience:", x = "") +
  facet_wrap(vars(gender)) +
  theme_bw() +
  scale_x_continuous(breaks = c(1996, 2000, 2004, 2008, 2012, 2016)) +
  theme(legend.position = "top")

ggsave("Graphs and Tables/CandWExp.png", plot = CandExpShare)


## Mean candidates with PR experience, creates figure 4.9.


ImpMechanismCheck2c <- ImpMechanismCheck2 %>%
  group_by(year, female.x) %>%
  summarise(MeanExp = mean(ImpPRexp, na.rm = TRUE))

ImpMechanismCheck2c$gender <- ifelse(ImpMechanismCheck2c$female.x == 1, "Women", "Men")

CandExpShareAsMean <- ggplot(ImpMechanismCheck2c %>%
         filter(year == 1996 | year == 2000 | year == 2004 | year == 2008 |
                  year == 2012 | year == 2016), aes(x = year, y = MeanExp)) +
  geom_point() +
  geom_line() +
  labs(title = "", y = "Share",
       x = "") +
  facet_wrap(vars(gender)) +
  scale_x_continuous(breaks = c(1996, 2000, 2004, 2008, 2012, 2016)) +
  theme_bw() +
  scale_y_continuous(breaks = c(0.05,0.10,0.15,0.20,0.25,0.30))

ggsave("Graphs and Tables/CandWExpMeans.png", plot = CandExpShareAsMean)





