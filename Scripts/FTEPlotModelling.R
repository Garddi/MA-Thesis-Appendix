library(tidyverse)
library(tidylog)

library(margins)
library(estimatr)

setwd("/Users/oyvindskorge/Dropbox/Students/Gard/")
theme_set(theme_minimal())

load("FTEmodelling.Rdata")
dt <- FTEmodelling

with(dt, table(year, yr))
with(dt, table(year, countdummy1))
with(dt, table(year, countdummy2))
with(dt, table(year, countdummy3))
with(dt, table(year, countdummy4))

# Create factor variables for post treatment year groups
# and for all pre and post treatment year groups
dt <- dt %>%
  ungroup() %>%
  mutate(year_treat = sjmisc::rec(year, rec=
            "min:2003=1996-2003;
            2004:2005=2004/5;
            2006:2009=2008/9;
            2012=2012;
            2016:2017=2016/7",
            to.factor=TRUE)) %>%
  mutate(year_elec = sjmisc::rec(year, rec=
           "1996=1996;
            2000=2000;
            2004:2005=2004/5;
            2006:2009=2008/9;
            2012=2012;
            2016:2017=2016/7;
            else=NA",
           to.factor=TRUE))
# dt %>% flat_table(year_treat, year) #OK

# Create treatment variables that take on the value of 1
# for treated groups in treated years (S-Korea year>2000)

dt <- dt %>%
  mutate(year_treat_n = as.numeric(year_treat)-1) %>%
  mutate(year_treat_n_korea = year_treat_n*Rdummy)

with(dt, table(year_treat, year_treat_n))
with(dt %>% filter(country=="South Korea"), table(year_treat, year_treat_n_korea))
with(dt %>% filter(country=="Japan"), table(year_treat, year_treat_n_korea))

# Create same as previous but including 2000 as "placebo" year
dt <- dt %>%
  mutate(year_elec_n = as.numeric(year_elec)-1) %>%
  mutate(year_elec_n_korea = year_elec_n*Rdummy)

with(dt, table(year_elec, year_elec_n))

#### Results ####

# Run results separately for women and men to make model less complicated
# and to make sure that gender is interacted with ALL variables,
# and also includes full set of country and election fixed effects (not just post treatment elections)
glm.m.2 <- glm(result ~ factor(year_treat_n_korea) + Rdummy + year_elec + NatVoteShare,
               data=dt %>% filter(female==0),
               family="binomial")

glm.w.2 <- glm(result ~ factor(year_treat_n_korea) + Rdummy + year_elec + NatVoteShare,
               data=dt %>% filter(female==1),
               family="binomial")

# glm.2 <- glm(result ~ factor(year_treat_n_korea)*female + Rdummy + year_elec + NatVoteShare,
#              data=dt,
#              family="binomial")
# summary(glm.2)

# Using margins to obtain average marginal effects (predicted probabilities)
# https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html#Motivation
# and https://cran.r-project.org/web/packages/margins/vignettes/TechnicalDetails.pdf

margins.w <- margins(glm.w.2, variable="year_treat_n_korea", type="response")
margins.w.out <- summary(margins.w)

margins.m <- margins(glm.m.2, variable="year_treat_n_korea", type="response")
margins.m.out <- summary(margins.m)

# Create data frame for plotting

dt.p <- bind_rows(
  margins.w.out %>%
    mutate(factor = as.factor(c("2004/5","2008/9","2012","2016/7")),
           gender= "Female"),
  margins.m.out %>%
    mutate(factor = as.factor(c("2004/5","2008/9","2012","2016/7")),
           gender= "Male"))

# Plotting

dt.p %>%
  ggplot(aes(x=factor, y=AME, group=gender, color=gender, shape=gender)) +
  geom_hline(yintercept=0) +
  scale_color_manual(values=c("darkgrey","black")) +
  geom_point(position=position_dodge(width=.5)) +
  geom_linerange(aes(ymin=lower, ymax=upper),
                 position=position_dodge(width=.5)) +
  theme(legend.title = element_blank()) +
  theme_bw()


#### Version 3: with placebo for year 2000 (1996 as reference year) ####

glm.m.3 <- glm(result ~ factor(year_elec_n_korea) + Rdummy + year_elec + NatVoteShare,
               data=dt %>% filter(female==0),
               family="binomial")

glm.w.3 <- glm(result ~ factor(year_elec_n_korea) + Rdummy + year_elec + NatVoteShare,
               data=dt %>% filter(female==1),
               family="binomial")

# glm.2 <- glm(result ~ factor(year_elec_n_korea)*female + Rdummy + year_elec + NatVoteShare,
#              data=dt,
#              family="binomial")
# summary(glm.2)

margins.w3 <- margins(glm.w.3, variable="year_elec_n_korea", type="response")
margins.w3.out <- summary(margins.w3)

margins.m3 <- margins(glm.m.3, variable="year_elec_n_korea", type="response")
margins.m3.out <- summary(margins.m3)


dt.p3 <- bind_rows(
  margins.w3.out %>%
    mutate(factor = as.factor(c("2000","2004/5","2008/9","2012","2016/7")),
           gender= "Female"),
  margins.m3.out %>%
    mutate(factor = as.factor(c("2000","2004/5","2008/9","2012","2016/7")),
           gender= "Male"))

dt.p3 %>%
  ggplot(aes(x=factor, y=AME, group=gender, color=gender, shape=gender)) +
  geom_hline(yintercept=0) +
  scale_color_manual(values=c("darkgrey","black")) +
  geom_point(position=position_dodge(width=.5)) +
  geom_linerange(aes(ymin=lower, ymax=upper),
                 position=position_dodge(width=.5)) +
  theme(legend.title = element_blank()) +
  theme_bw()

