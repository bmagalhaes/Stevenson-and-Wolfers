library(tidyverse)
library(haven)
library(stargazer)
library(lmtest)
library(lfe)
library(xtable)
library(tidytable)
library(bacondecomp)

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

# RUN ONLY WHEN USING LM, NOT NECESSARY WITH FELM
divorce = divorce %>%
  mutate(year = as_factor(year)) %>%
  mutate(stfips = as_factor(stfips)) %>%
  mutate(exp = as_factor(exp)) %>%
  mutate(region = as_factor(region))

fe_simple_asmrh = felm(asmrh ~ post | year + stfips | 0 | stfips, data=divorce)
fe_simple_asmrs = felm(asmrs ~ post | year + stfips | 0 | stfips, data=divorce)

divorce = divorce %>%
  mutate(trend = as.numeric(divorce$year))

fe_trend_asmrh = felm(asmrh ~ post | year + stfips + stfips:trend | 0 | stfips, data=divorce)
fe_trend_asmrs = felm(asmrs ~ post | year + stfips + stfips:trend | 0 | stfips, data=divorce)

stargazer(fe_simple_asmrh, fe_trend_asmrh, fe_simple_asmrs, fe_trend_asmrs,
          covariate.labels = "Unilateral Divorce Law",
          dep.var.labels = c("Homicide Mortality", "Suicide Mortality"),
          omit.stat = c("ser","rsq", "f"),
          add.lines = list(c("State Fixed effects", "Yes", "Yes", "Yes", "Yes"),
                           c("Year Fixed effects", "Yes", "Yes", "Yes", "Yes"),
                           c("State Specific Time Trend", "", "Yes", "", "Yes")),
          notes = "Standard errors clustered by State")

names(divorce) <- make.names(names(divorce))

divorce = mutate(divorce, X_Texp_9 = ifelse(exp == -1, 1, 0))

fe_hoekstra_asmrh = felm(asmrh ~ + X_Texp_1 + X_Texp_2 + X_Texp_3 + X_Texp_4 + X_Texp_5
        + X_Texp_6 + X_Texp_7 + X_Texp_8 + X_Texp_9 + X_Texp_11 + X_Texp_12 + X_Texp_13
        + X_Texp_14 + X_Texp_15 + X_Texp_16 + X_Texp_17 + X_Texp_18 + X_Texp_19 + X_Texp_20
        + X_Texp_21 + X_Texp_22 + X_Texp_23 + X_Texp_24 + X_Texp_25 + X_Texp_26
        + X_Texp_27 + X_Texp_28 | year + stfips | 0 | stfips, data=divorce)
fe_hoekstra_asmrs = felm(asmrs ~ + X_Texp_1 + X_Texp_2 + X_Texp_3 + X_Texp_4 + X_Texp_5
        + X_Texp_6 + X_Texp_7 + X_Texp_8 + X_Texp_9 + X_Texp_11 + X_Texp_12 + X_Texp_13
        + X_Texp_14 + X_Texp_15 + X_Texp_16 + X_Texp_17 + X_Texp_18 + X_Texp_19 + X_Texp_20
        + X_Texp_21 + X_Texp_22 + X_Texp_23 + X_Texp_24 + X_Texp_25 + X_Texp_26
        + X_Texp_27 + X_Texp_28 | year + stfips | 0 | stfips, data=divorce)

plot_order <- c("X_Texp_1", "X_Texp_2", "X_Texp_3", "X_Texp_4", "X_Texp_5", "X_Texp_6", "X_Texp_7",
                "X_Texp_8", "X_Texp_9", "X_Texp_11", "X_Texp_12", "X_Texp_13", "X_Texp_14", 
                "X_Texp_15", "X_Texp_16", "X_Texp_17", "X_Texp_18", "X_Texp_19", "X_Texp_20",
                "X_Texp_21", "X_Texp_22", "X_Texp_23", "X_Texp_24", "X_Texp_25", "X_Texp_26", 
                "X_Texp_27", "X_Texp_28")

leadslags_asmrh <- tibble(
  sd = c(fe_hoekstra_asmrh$cse[plot_order], 0),
  mean = c(coef(fe_hoekstra_asmrh)[plot_order], 0),
  label = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 0)
)

leadslags_asmrs <- tibble(
  sd = c(fe_hoekstra_asmrs$cse[plot_order], 0),
  mean = c(coef(fe_hoekstra_asmrs)[plot_order], 0),
  label = c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 0)
)

round(fe_simple_asmrh[["coefficients"]], 2)
round(fe_simple_asmrh[["cse"]], 2)
text_asmrh = "DD Coefficient = -0.15 (se = 0.15)"

round(fe_simple_asmrs[["coefficients"]], 2)
round(fe_simple_asmrs[["cse"]], 2)
text_asmrs = "DD Coefficient = -3.08 (se = 2.43)"

leadslags_asmrh %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  geom_pointrange(show.legend = TRUE, fatten = 2, color = "dark blue") +
  theme_bw() +
  xlab("Years before and after Unilateral Divorce Law") +
  ylab("Homicide Mortality") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark grey") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "dark grey") +
  scale_x_continuous(breaks= c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

leadslags_asmrh %>%
  ggplot(aes(x = label, y = mean, ymin = mean-1.96*sd, ymax = mean+1.96*sd)) +
  geom_point(color = "dark blue") +
  geom_line(aes(y = mean+1.96*sd, x=label), colour = 'dark blue', linetype = "dashed") +
  geom_line(aes(y = mean-1.96*sd, x=label), colour = 'dark blue', linetype = "dashed")+
  geom_line(color = "dark blue") +
  geom_ribbon(alpha = 0.4, fill = "light blue") +
  theme_bw() +
  xlab("Years before and after Unilateral Divorce Law") +
  ylab("Homicide Mortality") +
  geom_hline(yintercept = 0, color = "dark grey", size = 0.8) +
  geom_vline(xintercept = 0, color = "dark grey", size = 0.8) +
  geom_hline(yintercept = fe_simple_asmrh[["coefficients"]], color = "red", size = 1.2) +
  scale_x_continuous(breaks= c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
  annotation_custom(grid.text(text_asmrh, x=0.52,  y=0.11,
                              gp=gpar(col="black", fontsize=8, fontface="bold")))

leadslags_asmrs %>%
  ggplot(aes(x = label, y = mean,
             ymin = mean-1.96*sd, 
             ymax = mean+1.96*sd)) +
  geom_pointrange(show.legend = TRUE, fatten = 2, color = "dark blue") +
  theme_bw() +
  xlab("Years before and after Unilateral Divorce Law") +
  ylab("Suicide Mortality") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark grey") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "dark grey") +
  scale_x_continuous(breaks= c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())

a = leadslags_asmrs %>%
  ggplot(aes(x = label, y = mean, ymin = mean-1.96*sd, ymax = mean+1.96*sd)) +
  geom_point(color = "dark blue") +
  geom_line(aes(y = mean+1.96*sd, x=label), colour = 'dark blue', linetype = "dashed") +
  geom_line(aes(y = mean-1.96*sd, x=label), colour = 'dark blue', linetype = "dashed")+
  geom_line(color = "dark blue") +
  geom_ribbon(alpha = 0.4, fill = "light blue") +
  theme_bw() +
  xlab("Years before and after Unilateral Divorce Law") +
  ylab("Suicide Mortality") +
  geom_hline(yintercept = 0, color = "dark grey", size = 0.8) +
  geom_vline(xintercept = 0, color = "dark grey", size = 0.8) +
  geom_hline(yintercept = fe_simple_asmrs[["coefficients"]], color = "red", size = 1.2) +
  scale_x_continuous(breaks= c(-9, -8, -7, -6, -5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank()) +
  annotation_custom(grid.text(text_asmrs, x=0.52,  y=0.11,
                              gp=gpar(col="black", fontsize=8, fontface="bold")))

ggsave(a, filename = "abab.png")

df_bacon_asmrh <- bacon(formula = asmrh ~ post,
                  data = divorce, id_var = "stfips",
                  time_var = "trend")

bacon_asmrh = df_bacon_asmrh %>%
  group_by(type) %>%
  summarize(Weigth = sum(weight), "Average DD Estimate" = weighted.mean(estimate, weight))

bacon_asmrh$`DD Estimate` = bacon_asmrh$Weigth * bacon_asmrh$`Average DD Estimate`

table_print = xtable(bacon_asmrh)
align(table_print) <- "llccc"
print(table_print, include.rownames = FALSE)

df_bacon_asmrs <- bacon(formula = asmrs ~ post,
                        data = divorce, id_var = "stfips",
                        time_var = "trend")

bacon_asmrs = df_bacon_asmrs %>%
  group_by(type) %>%
  summarize(Weigth = sum(weight), "Average DD Estimate" = weighted.mean(estimate, weight))

bacon_asmrs$`DD Estimate` = bacon_asmrs$Weigth * bacon_asmrs$`Average DD Estimate`

table_print = xtable(bacon_asmrs)
align(table_print) <- "llccc"
print(table_print, include.rownames = FALSE)

df_bacon_asmrh %>%
  ggplot(aes(x = weight, y = estimate, group = type)) +
  geom_point(aes(shape = type, color = type)) +
  geom_hline(yintercept = 0, color = "dark grey") +
  geom_hline(yintercept = fe_simple_asmrh[["coefficients"]], color = "red", size = 0.8) +
  xlab("Weight") +
  ylab("DD Estimate")+
  guides(shape = guide_legend(""), color = guide_legend("")) +
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position="bottom")

df_bacon_asmrs %>%
  ggplot(aes(x = weight, y = estimate, group = type)) +
  geom_point(aes(shape = type, color = type)) +
  geom_hline(yintercept = 0, color = "dark grey") +
  geom_hline(yintercept = fe_simple_asmrs[["coefficients"]], color = "red", size = 0.8) +
  xlab("Weight") +
  ylab("DD Estimate") +
  guides(shape = guide_legend(""), color = guide_legend("")) +
  theme_bw()+
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position="bottom")
