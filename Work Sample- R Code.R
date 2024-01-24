#Clean and Describe Data
library(tidyverse)

couchto5k <- read.csv("https://uoepsy.github.io/data/couchto5k.csv")

couchto5k <- 
  couchto5k |> 
  mutate(
    # ages >100, make NA
    age = ifelse(age>100, NA, age),
    # selfmot scores <5, make NA
    selfmot = ifelse(selfmot < 5, NA, selfmot),
    # change autunm to autumn
    season = ifelse(season == "autunm","autumn",season),
    # and then make it a factor (lets set the levels too)
    season = factor(season, levels = c("spring","summer","autumn","winter")),
    # make city a factor
    city = factor(city),
    # weekstopped if >9, then NA
    week_stopped = ifelse(week_stopped > 9, NA, week_stopped)
  )

summary(couchto5k)

couchto5k <- na.omit(couchto5k)

#Testing City Dropouts
couchto5k <- 
  couchto5k |>
  mutate(
    dropout = ifelse(week_stopped < 5, "early",
                     ifelse(week_stopped <9, "late", "not at all"))
  )

table(couchto5k$dropout, couchto5k$week_stopped)

table(couchto5k$dropout, couchto5k$city)
chisq.test(table(couchto5k$dropout, couchto5k$city), simulate.p.value = TRUE)

#Testing City Differences in Age
hist(couchto5k$age[couchto5k$city=="Edinburgh"], 
     main = "Edinburgh", xlab = "Age")
hist(couchto5k$age[couchto5k$city=="Glasgow"], 
     main = "Glasgow", xlab = "Age")

qqnorm(couchto5k$age[couchto5k$city=="Edinburgh"], main = "Edinburgh")
qqline(couchto5k$age[couchto5k$city=="Edinburgh"])

qqnorm(couchto5k$age[couchto5k$city=="Glasgow"], main = "Glasgow")
qqline(couchto5k$age[couchto5k$city=="Glasgow"])

var.test(age ~ city, data = couchto5k)

t.test(age ~ city, data = couchto5k, var.equal = TRUE)


#Fitting Happiness and Health Models
couchto5k <- couchto5k |>
  mutate(
    healthZ = (health-mean(health))/sd(health),
  )

hhmod <- lm(happiness ~ age + season + week_stopped * healthZ, couchto5k)

#Checking Model Assumptions & Testing
plot(hhmod)
anova(hhmod)
summary(hhmod)

#Model Visualization
plotdat <- expand_grid(
  age = mean(couchto5k$age),
  season = "spring",
  week_stopped=1:9,
  healthZ = c(-1, 0, 1)
)

broom::augment(hhmod, newdata = plotdat, interval="confidence") |>
  mutate(
    health = factor(healthZ, levels=c("-1","0","1"),
                    labels=c("Poor health (-1 SD)", 
                             "Average health",
                             "Good health (+1 SD)")
    )
  ) |> ggplot(aes(x=week_stopped,y=.fitted, col=health,fill=health))+
  geom_line(aes(lty=health)) +
  geom_ribbon(aes(ymin=.lower,ymax=.upper), alpha=.2) +
  scale_color_viridis_d(option="C")+
  scale_fill_viridis_d(option="C")+
  scale_x_continuous(breaks=1:9)+
  labs(y="Happiness (0-100)",x="- Week Stopped -") +
  guides(col="none",fill="none")

#Standardized Coefficients
hhmod2 <- lm(scale(happiness) ~ scale(age) + season + scale(week_stopped) * healthZ, data = couchto5k)
summary(hhmod2)

#Fitting Predictors of Drop-Out Models
couchto5k <- couchto5k |>
  mutate(
    completed = ifelse(week_stopped == 9, 1, 0)
  )

couchto5k <-
  couchto5k |> 
  mutate(
    accountabilityZ = scale(accountability)[,1],
    selfmotZ = scale(selfmot)[,1]
  )

compmod <- glm(completed ~ season + age + city + accountabilityZ + selfmotZ, 
               data = couchto5k, family=binomial)

#Testing the Influence of Psychological Factors
compmod <- glm(completed ~ season + age + city + accountabilityZ + selfmotZ, 
               data = couchto5k, family=binomial)

compmod0 <- glm(completed ~ season + age + city, data = couchto5k, family=binomial)

anova(compmod0, compmod, test="Chisq")
summary(compmod)

#Model Visualization
library(effects)
p1 <- effect(c("selfmotZ","season"), compmod, xlevels=20) |>
  as.data.frame() |>
  filter(season=="spring") |>
  ggplot(aes(x=selfmotZ,y=fit))+
  geom_line()+
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.2) +
  labs(title="Predicted probability of\ncouch-to-5k completion",x="Self Motivation (Z-scored)",y="P(Completed)")

p2 <- effect(c("accountabilityZ","season"), compmod, xlevels=20) |>
  as.data.frame() |>
  filter(season=="spring") |>
  ggplot(aes(x=accountabilityZ,y=fit))+
  geom_line()+
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.2) +
  labs(title="Predicted probability of\ncouch-to-5k completion",x="Accountability (Z-scored)",y="P(Completed)")

p1
p2