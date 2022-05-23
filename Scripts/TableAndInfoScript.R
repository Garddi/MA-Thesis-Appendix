#### Script for general means of some of the measures


### Creating the plots for Chapter 3, that demonstrates the development.

XLframe <- readxl::read_xlsx("Manually Compiled data/Descriptive Stats PR Candidates.xlsx", sheet = "Ark2")

XLJPA <- XLframe %>%
  dplyr::select(Year, `All Japan PR candidates`)
XLJPP <- XLframe %>%
  dplyr::select(Year, `Pure PR candidates Japan`)
XLSK <- XLframe %>%
  dplyr::select(Year, `South Korea`)

XLJPA <- XLJPA %>%
  filter(Year == 1996 | Year == 2000 | Year == 2003 | Year == 2005 | Year == 2009 |
           Year == 2012 | Year == 2014 | Year == 2017 | Year == 2021) %>%
  rename(sharew = `All Japan PR candidates`) %>%
  mutate(type = "Japan All Candidates")
XLJPP <- XLJPP %>%
  filter(Year == 1996 | Year == 2000 | Year == 2003 | Year == 2005 | Year == 2009 |
           Year == 2012 | Year == 2014 | Year == 2017 | Year == 2021) %>%
  rename(sharew = `Pure PR candidates Japan`) %>%
  mutate(type = "Japan Pure PR")
XLSK <- XLSK %>%
  filter(Year == 1988 | Year == 1992 | Year == 1996 | Year == 2000 | Year == 2004 |
           Year == 2008 | Year == 2012 | Year == 2016 | Year == 2020) %>%
  rename(sharew = `South Korea`) %>%
    mutate(type = "South Korea")

PRrepframe <- bind_rows(XLJPA, XLJPP, XLSK)

rm(XLJPA, XLJPP, XLSK, XLframe)

## plotting Creates figure 3.4.

ggplot(PRrepframe, aes(x = Year, y = sharew, col = type)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  geom_vline(xintercept = 2004, linetype = "dashed") +
  labs(title = "", y = "Share Women",
       x = "") +
  theme(legend.position = "top") +
  scale_x_continuous(breaks = c(1988, 1992, 1996, 2000, 2004, 2008, 2012,
                                2016, 2020))

## South Korea

RepSK <- KunNADataSMDTrans1 %>%
  filter(byelection == 0) %>%
  group_by(year, prcan) %>%
  summarise(sharew = mean(female))

RepSK <- RepSK %>%
  mutate(CandType = ifelse(prcan == 1, "PR", "SMD"))

RepSKw <- KunNADataSMDTrans1 %>%
  filter(byelection == 0, win > 0) %>%
  group_by(year, prcan) %>%
  summarise(sharew = mean(female))

RepSKw <- RepSKw %>%
  mutate(CandType = ifelse(prcan == 1, "PR", "SMD"))

totwin <- KunNADataSMD %>%
  filter(byelection == 0, win > 0) %>%
  group_by(year) %>%
  summarise(sharew = mean(female))

totwin <- totwin %>%
  mutate(CandType = "Total")

SKRep <- bind_rows(RepSKw, totwin)


ggplot(SKRep, aes(x = year, y = sharew, col = CandType)) +
  geom_point() +
  geom_line() +
  theme_bw()

# South Korea goes from 1.9% women before, to around 8% average post treatent.
# Japan goes from average 12% women to 15% women the same time frame.
# Fourfolds increase for SK average, but could be confounded.



### Creating plots for the national trends in representation

SkFrame <- readxl::read_xlsx("Manually Compiled data/DescriptiveRepresentationstats.xlsx", sheet = "Ark4")

#Creates figure 3.1
ggplot(SkFrame, aes(x = SKORYear)) +
  geom_line(aes(y = TotRep, colour = "Total"), size = 1) +
  geom_line(aes(y = PRRep, colour = "PR"), size = 1) +
  geom_line(aes(y = SMDRep, colour = "SMD"),size = 1) +
  geom_line(aes(y = SNTVRep, colour = "SNTV"), size = 1) +
  scale_y_continuous(breaks=seq(0.0, 0.6, 0.1), limits=c(0, 0.6)) +
  scale_x_continuous(breaks = c(1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012,
                                2016, 2020)) +
  scale_color_manual(name = "List Type",
                     values = c("Total" = "black", "PR" = "blue", "SMD" = "red",
                                "SNTV" = "yellow")) +
  labs(title = "", x = "", y = "Share women") +
  theme_bw()

#Japan
JpFrame <-  readxl::read_xlsx("Manually Compiled data/DescriptiveRepresentationstats.xlsx", sheet = "Ark3")

#Creates figure 3.2
ggplot(JpFrame, aes(x = JapanYear)) +
  geom_line(aes(y = TotRep, colour = "Total"), size = 1) +
  geom_line(aes(y = PRRep, colour = "PR"), size = 1) +
  geom_line(aes(y = SMDRep, colour = "SMD"),size = 1) +
  scale_y_continuous(breaks=seq(0.0, 0.6, 0.1), limits=c(0, 0.6)) +
  scale_x_continuous(breaks = c(1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012,
                                2016, 2020)) +
  scale_color_manual(name = "List Type",
                     values = c("Total" = "black", "PR" = "blue", "SMD" = "red")) +
  labs(title = "", x = "", y = "Share women") +
  theme_bw()



### Creating Electoral Volatility averages

VolFrameJP <- MergedJP %>%
  filter(year.x == 1996 | year.x == 2000 | year.x == 2003 | year.x == 2005 |
           year.x == 2009 | year.x == 2012 | year.x == 2014 | year.x == 2017 |
           year.x == 1993) %>%
  group_by(year.x, camp) %>%
  summarise(Votes = sum(ku_vote.x, na.rm = TRUE))
VolFrameJPb <- MergedJP %>%
  group_by(year.x) %>%
  summarise(TotVotes = sum(ku_vote.x, na.rm = TRUE))

VolFrameJP <- left_join(VolFrameJP, VolFrameJPb, by = c("year.x"))

## Share for each year

VolFrameJP <- VolFrameJP %>%
  mutate(Vshare = (Votes/TotVotes))

VolFrameJP <- VolFrameJP %>%
  group_by(camp) %>%
  arrange(year.x) %>%
  mutate(prevshare = dplyr::lag(Vshare, 1))

VolFrameJP <- VolFrameJP %>%
  mutate(change = abs(Vshare-prevshare))

VolatilFrameJP <- VolFrameJP %>%
  group_by(year.x) %>%
  summarise(Vol = ((sum(change, na.rm = TRUE))/2))

VolatilFrameJP[1,2] <- NA

mean(VolatilFrameJP$Vol, na.rm = TRUE)

### Volatility for South Korea

table(MergedJP8$ImpPRexp[which(MergedJP8$female.x == 1)])



VolFrameSK <- MergedKR11 %>%
  filter(year == 1988 | year == 1992 | year == 1996 | year == 2000 |
           year == 2004 | year == 2008 | year == 2012 | year == 2016) %>%
  group_by(year, party5) %>%
  summarise(Votes = sum(votes.x, na.rm = TRUE))
VolFrameSKb <- MergedKR11 %>%
  group_by(year) %>%
  summarise(TotVotes = sum(votes.x, na.rm = TRUE))

VolFrameSK <- left_join(VolFrameSK, VolFrameSKb, by = c("year"))

## Share for each year

VolFrameSK <- VolFrameSK %>%
  mutate(Vshare = (Votes/TotVotes))

VolFrameSK <- VolFrameSK %>%
  group_by(party5) %>%
  arrange(year) %>%
  mutate(prevshare = dplyr::lag(Vshare, 1))

VolFrameSK <- VolFrameSK %>%
  mutate(change = abs(Vshare-prevshare))

VolatilFrameSK <- VolFrameSK %>%
  group_by(year) %>%
  summarise(Vol = ((sum(change))/2))

mean(VolatilFrameSK$Vol, na.rm = TRUE)
VolatilFrameSK$Vol
### T-test between them

t.test(VolatilFrameJP$Vol, VolatilFrameSK$Vol,
       alternative = "two.sided",
       var.equal = TRUE)

VolatilFrameJP$Vol

t.test(VolatilFrameJP$Vol[5:9],
       VolatilFrameSK$Vol[5:8],
       alternative = "two.sided")


t.test(VolatilFrameJP$Vol[1:4],
       VolatilFrameSK$Vol[1:4],
       alternative = "two.sided")

### Plotting volatility

ggplot() +
  geom_line(data = VolatilFrameJP, aes(x = year.x, y = Vol, color = "Japan")) +
  geom_line(data = VolatilFrameSK, aes(x = year, y = Vol, color = "South Korea")) +
  theme_bw() +
  scale_colour_manual(name = "Country", values = c("Japan" = "blue",
                                                   "South Korea" = "red")) +
  labs(title = "Electoral Volatility", x = "", y = "Volatility score") +
  scale_x_continuous(breaks = c(1988, 1992, 1996, 2000, 2004, 2008, 2012, 2016)) +
  scale_y_continuous(breaks=seq(0.0, 0.4, 0.05), limits=c(0, 0.4))


## p-value of 0.1548, not significant.


## Party spread

table(MergedKR11$party5, MergedKR11$year, MergedKR11$female.x)


