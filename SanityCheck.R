library(lme4)
library(tidyverse)

just_308 <- sleepstudy %>%
  filter(Subject == "308")

ggplot(just_308, aes(x = Days, y = Reaction)) +
  geom_point() +
  scale_x_continuous(breaks = 0:9)

sleep2 <- sleepstudy %>%
  filter(Days >= 2L) %>%
  mutate(days_deprived = Days - 2L)
sleep2 %>%
  count(days_deprived, Days)


cp_model <- lm(Reaction ~ days_deprived, sleep2)

summary(cp_model)




np_model <- lm(Reaction ~ days_deprived + Subject + days_deprived:Subject,
               data = sleep2)

summary(np_model)




pp_mod <- lmer(Reaction ~ days_deprived + (days_deprived | Subject), sleep2)

summary(pp_mod)

mx <- VarCorr(pp_mod)[["Subject"]]

## if cov = rho * t00 * t11, then
## rho = cov / (t00 * t11).
mx[1, 2] / (sqrt(mx[1, 1]) * sqrt(mx[2, 2]))


# modify sleep2 data 
sleep2 <- sleep2 %>% mutate(treatment = rep(c("one","two","three"), each=48) %>% as.factor())

mod1 <- lmer(Reaction ~ days_deprived:treatment + (days_deprived | Subject), sleep2)
summary(mod1)

mod2 <- lmer(Reaction ~ days_deprived + days_deprived:treatment + (days_deprived | Subject), sleep2)
summary(mod2)


fixef(mod1)

fixef(mod2)
