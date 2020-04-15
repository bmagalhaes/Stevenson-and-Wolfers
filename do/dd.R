library(tidyverse)
library(haven)
library(stargazer)
library(lmtest)
library(lfe)
# library(multiwayvcov)
# library(sandwich)
# library(plm)

divorce = read_dta("https://github.com/bmagalhaes/Stevenson-and-Wolfers/blob/master/data/sw_nofault_divorce.dta?raw=true")

divorce = divorce %>%
  mutate(year = as_factor(year)) %>%
  mutate(stfips = as_factor(stfips)) %>%
  mutate(exp = as_factor(exp))

fe_simple = felm(asmrh ~ post | year + stfips | 0 | stfips, data=divorce)
summary(fe_simple)

divorce = divorce %>%
  mutate(trend = as.numeric(divorce$year))

fe_trend = felm(asmrh ~ post | year + stfips + stfips:trend | 0 | stfips, data=divorce)
summary(fe_trend)

names(divorce) <- make.names(names(divorce))

fe_hoekstra = felm(asmrh ~ + X_Texp_1 + X_Texp_2 + X_Texp_3 + X_Texp_4 + X_Texp_5
        + X_Texp_6 + X_Texp_7 + X_Texp_8 + X_Texp_10 + X_Texp_11 + X_Texp_12 + X_Texp_13
        + X_Texp_14 + X_Texp_15 + X_Texp_16 + X_Texp_17 + X_Texp_18
        + X_Texp_19 | year + stfips | 0 | stfips, data=divorce)
summary(fe_hoekstra)

summ_hoekstra = as.data.frame(fe_hoekstra$coefficients)
summ_hoekstra$se = fe_hoekstra$cse
summ_hoekstra$label = c(-9, -8, -7, -6, -5, -4, -3, -2, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

summ_hoekstra %>%
  ggplot(aes(x = label, y = asmrh,
             ymin = asmrh-1.96*se, 
             ymax = asmrh+1.96*se)) +
  geom_hline(yintercept = 0, color = "red") +
  geom_pointrange() +
  theme_bw() +
  xlab("Years before and after treatment") +
  ylab("Blabla") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks=c(-9, -8, -7, -6, -5, -4, -3, -2, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  theme(panel.grid = element_blank())
