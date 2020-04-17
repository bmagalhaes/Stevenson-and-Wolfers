library(tidyverse)
library(haven)
library(stargazer)
library(lmtest)
library(lfe)
library(xtable)
library(tidytable)
# library(plspm)
# library(multiwayvcov)
# library(sandwich)
# library(plm)
# library(tidyr)

divorce = read_dta("C:/Users/Bernardo/Documents/GitHub/Stevenson-and-Wolfers/data/sw_nofault_divorce.dta")

table_01 = divorce %>%
  group_by(statename) %>%
  summarize(yearlaw = unique(nfd))

table_01$statename[table_01$yearlaw == 1969]

table_01 = get_dummies.(table_01, yearlaw, prefix = FALSE)
table_01$yearlaw = NULL
table_01[table_01 == 0] <- ""
table_01[table_01 == 1] <- "X"
table_01 = table_01 %>% select(statename, "1969", "1970", "1971", "1972", "1973", "1974",
                               "1975", "1976", "1977", "1980", "1984", "1985", NRS, PRE)
table_print = xtable(table_01)
align(table_print) <- "ll|cccccccccccccc"
print(table_print, include.rownames = FALSE)

divorce = divorce %>%
  mutate(year = as_factor(year)) %>%
  mutate(stfips = as_factor(stfips)) %>%
  mutate(exp = as_factor(exp)) %>%
  mutate(region = as_factor(region))

fe_simple_asmrh = felm(asmrh ~ post | year + stfips | 0 | stfips, data=divorce)
summary(fe_simple_asmrh)

fe_simple_asmrs = felm(asmrs ~ post | year + stfips | 0 | stfips, data=divorce)
summary(fe_simple_asmrs)

divorce = divorce %>%
  mutate(trend = as.numeric(divorce$year))

fe_trend_asmrh = felm(asmrh ~ post | year + stfips + stfips:trend | 0 | stfips, data=divorce)
summary(fe_trend_asmrh)

fe_trend_asmrs = felm(asmrs ~ post | year + stfips + stfips:trend | 0 | stfips, data=divorce)
summary(fe_trend_asmrs)

names(divorce) <- make.names(names(divorce))

divorce = mutate(divorce, X_Texp_9 = ifelse(exp == -1, 1, 0))

fe_hoekstra_asmrh = felm(asmrh ~ + X_Texp_1 + X_Texp_2 + X_Texp_3 + X_Texp_4 + X_Texp_5
        + X_Texp_6 + X_Texp_7 + X_Texp_8 + X_Texp_9 + X_Texp_11 + X_Texp_12 + X_Texp_13
        + X_Texp_14 + X_Texp_15 + X_Texp_16 + X_Texp_17 + X_Texp_18 + X_Texp_19 + X_Texp_20
        + X_Texp_21 + X_Texp_22 + X_Texp_23 + X_Texp_24 + X_Texp_25 + X_Texp_26
        + X_Texp_27 + X_Texp_28 | year + stfips | 0 | stfips, data=divorce)
summary(fe_hoekstra_asmrh)

fe_hoekstra_asmrs = felm(asmrs ~ + X_Texp_1 + X_Texp_2 + X_Texp_3 + X_Texp_4 + X_Texp_5
                         + X_Texp_6 + X_Texp_7 + X_Texp_8 + X_Texp_9 + X_Texp_11 + X_Texp_12 + X_Texp_13
                         + X_Texp_14 + X_Texp_15 + X_Texp_16 + X_Texp_17 + X_Texp_18 + X_Texp_19 + X_Texp_20
                         + X_Texp_21 + X_Texp_22 + X_Texp_23 + X_Texp_24 + X_Texp_25 + X_Texp_26
                         + X_Texp_27 + X_Texp_28 | year + stfips | 0 | stfips, data=divorce)
summary(fe_hoekstra_asmrs)

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
