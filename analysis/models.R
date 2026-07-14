library(tidyverse)
library(fixest)
library(jtools)
library(plm)
library(lmtest)
library(modelsummary)

panel = read_csv("data/processed/finland_panel.csv")

theme_set(theme_bw())

# we want to look at how a change in page view is determined by a change in native speaker numbers and change in the utility
# of the wikipedia proxied by articlces

# remove IDO since it only has one entry
panel = panel %>%
  filter(ISO6393 != "ido")

# log transform all variables
safe_log = function(x) log(x + 1)

panel = panel %>%
  mutate(log_page_views = safe_log(page_views),
         log_speakers = safe_log(speakers),
         log_articles = safe_log(articles),
         year = as.factor(year))


get_qq_plot = function(mdl){
  res = resid(mdl)
  qqnorm(res)
  qqline(res)
}


# cross sectional regression
cs = panel %>%
  filter(year == 2024)

cs_lm_speaker = lm(data = cs, log_page_views ~ log_speakers)
summary(cs_lm_speaker)
get_qq_plot(cs_lm_speaker)

cs_lm_articles = lm(data = cs, log_page_views ~ log_articles)
summary(cs_lm_articles)
get_qq_plot(cs_lm_articles)

cs_lm = lm(data = cs, log_page_views ~ log_speakers + log_articles)
summary(cs_lm)
get_qq_plot(cs_lm)
plot(cs_lm)
VIF(cs_lm)


# we start by fitting a pooled OLS for each variable
lm_speakers = lm(data = panel, log_page_views ~ log_speakers)
summary(lm_speakers)# r^2 = 0.49
get_qq_plot(lm_speakers)
AIC(lm_speakers)

lm_articles = lm(data = panel, log_page_views ~ log_articles)
summary(lm_articles) # r^2 = 0.63
get_qq_plot(lm_articles)
AIC(lm_articles)

#we fit a pooled ols as baseline
pooled_ols = feols(data = panel, log_page_views ~ log_speakers + log_articles)
summary(pooled_ols)
get_qq_plot(pooled_ols)
AIC(pooled_ols)
#plot(pooled_ols)

plot_coefs(pooled_ols)

# we now want to take the panel structure into account, we fit a fixed effect regression model with language and time fixed effects
fe = feols(data = panel, log_page_views ~ log_speakers + log_articles | language_name + year)
AIC(fe)
summary(fe)
get_qq_plot(fe)
plot_coefs(pooled_ols, fe)

# remove outliers e.g. Finnish English and Swedish + akan afar and ndonga as a robustness check
fe_robust = feols(data = panel %>% 
                    filter(!ISO6393 %in% c("eng", "fin", "swe", "aka", "aar", "ndo")), log_page_views ~ log_speakers + log_articles | language_name + year)
get_qq_plot(fe_robust)
summary(fe_robust)
coefplot(fe_robust)

# low & high income languages
panel_low_inc = panel %>%
  filter(income_grp == "low")
panel_high_inc = panel %>%
  filter(income_grp == "high")

fe_high_income = feols(data = panel_high_inc, log_page_views ~ log_speakers + log_articles | language_name + year)
summary(fe_high_income)
get_qq_plot(fe_high_income)

fe_low_income = feols(data = panel_low_inc, log_page_views ~ log_speakers + log_articles | language_name + year)
summary(fe_low_income)
get_qq_plot(fe_low_income)



coef_plots = plot_coefs(
  pooled_ols, fe, fe_low_income, fe_high_income,
  coefs = c("Speakers (ln)" = "log_speakers",
            "Articles (ln)" = "log_articles"),
  model.names = c(
    "Pooled OLS",
    "Fixed Effects",
    "Fixed Effects, Low Income",
    "Fixed Effects, High Income"
  )
) +
  labs(
    title = "Effect on Wikipedia page views",
    subtitle = "Coefficient estimates with 95% confidence intervals",
    x = "Estimated effect",
    y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size = 11),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
  )
coef_plots
ggsave("analysis/plots/coef_plots.png", coef_plots, width = 12, height = 8, dpi = 300)

modelsummary(list(pooled_ols, fe, fe_low_income, fe_high_income))
esttex(list(pooled_ols, fe, fe_low_income, fe_high_income))


# normalize to per capita page views and per article page views
panel = panel %>%
  group_by(year, language_name) %>%
  mutate(pw_pc = page_views / speakers,
         pw_pa = page_views / articles) %>%
  mutate(log_pw_pc = log(pw_pc),
         log_pw_pa = log(pw_pa))

# if we remove population from the equation
fe_pw_pc = feols(data = panel, log_pw_pc ~ log_articles | language_name + year)
summary(fe_pw_pc)

# remove articles from the equation
fe_pw_pa = feols(data = panel, log_pw_pa ~ log_speakers | language_name + year)
summary(fe_pw_pa)

# results are robust

# is the number of articles dependent on the number of speakers?
feols(data = panel, log_articles ~ log_speakers | language_name + year)

# random vs fixed effect
fe_plm = plm(data = panel, log_page_views ~ log_speakers + log_articles,  model = "within", index = c("language_name", "year"), effect = "twoways")
summary(fe_plm)

rand_plm = plm(data = panel, log_page_views ~ log_speakers + log_articles,  model = "random", index = c("language_name", "year"), effect = "twoways")
summary(rand_plm)


plm::phtest(fe_plm, rand_plm)


# Reverse regression
feols(data = panel, log_articles ~ log_speakers + log_page_views | language_name + year) # speakers are not a significant predictor articles

feols(data = panel, log_speakers ~ log_articles + log_page_views | language_name + year) # articles does not predict speakers --> correct to modell them as independent


# time lag as robustness check

# add lag
panel_lag = panel %>%
  ungroup() %>%
  arrange(language_name, year) %>%
  group_by(language_name) %>%
  mutate(lag_log_page_views_1 = dplyr::lag(log_page_views, 1),
         lag_log_speakers_1 = dplyr::lag(log_speakers, 1),
         lag_log_articles_1 = dplyr::lag(log_articles, 1)) %>%
  mutate(lag_log_page_views_2 = dplyr::lag(log_page_views, 2),
         lag_log_speakers_2 = dplyr::lag(log_speakers, 2),
         lag_log_articles_2 = dplyr::lag(log_articles, 2)) %>%
  mutate(lag_log_page_views_3 = dplyr::lag(log_page_views, 3),
         lag_log_speakers_3 = dplyr::lag(log_speakers, 3),
         lag_log_articles_3 = dplyr::lag(log_articles, 3))

# filter na from no previous year
panel_lag = panel_lag %>%
  filter(!if_any(everything(), is.na))

# simple
fe_lag_1 = feols(data = panel_lag, log_page_views ~ lag_log_speakers_1 + lag_log_articles_1 | language_name + year)
summary(fe_lag_1)
get_qq_plot(fe_lag_1)

fe_lag_2 = feols(data = panel_lag, log_page_views ~ lag_log_speakers_2 + lag_log_articles_2 | language_name + year)
summary(fe_lag_2)
get_qq_plot(fe_lag_2)

fe_lag_3 = feols(data = panel_lag, log_page_views ~ lag_log_speakers_3 + lag_log_articles_3 | language_name + year)
summary(fe_lag_3)
get_qq_plot(fe_lag_3)

plot_coefs(fe_lag_1, fe_lag_2, fe_lag_3)

# speakers is a robust lagged predictor, articles are not. 


# reversed direction: does time lagged articles predict page views better than timelagged pageviews predict articles?


fe_reverse_1 = feols(data = panel_lag, log_articles ~ lag_log_speakers_1 + lag_log_page_views_1 | language_name + year)
summary(fe_reverse_1)

plot_coefs(fe_lag_1, fe_reverse_1) # no significant difference

fe_reverse_2 = feols(data = panel_lag, log_articles ~ lag_log_speakers_2 + lag_log_page_views_2 | language_name + year)
summary(fe_reverse_2)

plot_coefs(fe_lag_2, fe_reverse_2) # at a lag of 2, articles remains a significant predictor of page views, but page views is not a significant predictor of articles 
# --> argument against reverse causality
