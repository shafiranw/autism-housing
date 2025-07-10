rm(list=ls())

library(foreign)
library(base)
library(dplyr)
library(haven)
library(nnet)
library(MNP)
library(htmltools)
library(webshot2)
library(mlogit)
library(broom)
library(knitr)
library(kableExtra)
library(xfun)
library(tidyr)

PathIn <- file.path("Documents", "PhD", "Projects", "autism_housing", "data")
setwd(PathIn)

# Load dataset
acs13 <- read.csv("~/Documents/PhD/Projects/autism_housing/data/acs13_071025.csv")
acs15 <- read.csv("~/Documents/PhD/Projects/autism_housing/data/acs15_071025.csv")
acs20 <- read.csv("~/Documents/PhD/Projects/autism_housing/data/acs20_071025.csv")

# Multinomial Logit -----------------------------------------------------------#
# Threshold: 13
acs13$housing <- factor(acs13$housing) # To let R know that these numbers are discrete categories and not continuous numbers

model13_logit <- multinom(housing ~MALE + AGE + MARST + HIEDUC +BLACK + ASIAN + NATIVE + HISPANIC + OTHER + HINSCAID + SPEAKENG + state_exp_capita + state_support_capita, data = acs13)
summary(model13_logit)

# Nice looking table
housing_labels <- c("hh_alone", "hh_withspouse", "hh_withotherrel", 
                    "hh_withnonrel", "GQ_inst", "GQ_noninst", "GQ_other")

# Tidy model output
tidy_model <- tidy(model13_logit) %>%
  mutate(
    signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    coef_se = paste0(round(estimate, 3), signif, "\n(", round(std.error, 3), ")")
  )

# Add readable labels to y.level
tidy_model$y.level <- factor(tidy_model$y.level,
                             levels = sort(unique(tidy_model$y.level)),
                             labels = housing_labels)
# Pivot to wide format
wide_table <- tidy_model %>%
  select(term, y.level, coef_se) %>%
  pivot_wider(
    names_from = y.level,
    values_from = coef_se
  ) %>%
  rename(Variable = term)

# HTML
kable(wide_table, format = "html", align = "l", caption = "Multinomial Logit Results: 13 yo Child") %>%
  kable_styling(full_width = FALSE, font_size = 12) %>%
  save_kable("model13_logit_wide.html")

# Convert to PNG
webshot("model13_logit_wide.html", file = "model13_logit_wide.png", vwidth = 1600, vheight = 1000)

# Threshold: 15 ---------------------------------------------------------------#
acs15$housing <- factor(acs15$housing) # To let R know that these numbers are discrete categories and not continuous numbers

model15_logit <- multinom(housing ~MALE + AGE + MARST + HIEDUC +BLACK + ASIAN + NATIVE + HISPANIC + OTHER + HINSCAID + SPEAKENG + state_exp_capita + state_support_capita, data = acs15)
summary(model15_logit)

# Nice looking table
housing_labels <- c("hh_alone", "hh_withspouse", "hh_withotherrel", 
                    "hh_withnonrel", "GQ_inst", "GQ_noninst", "GQ_other")

# Tidy model output
tidy_model <- tidy(model15_logit) %>%
  mutate(
    signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    coef_se = paste0(round(estimate, 3), signif, "\n(", round(std.error, 3), ")")
  )

# Add readable labels to y.level
tidy_model$y.level <- factor(tidy_model$y.level,
                             levels = sort(unique(tidy_model$y.level)),
                             labels = housing_labels)
# Pivot to wide format
wide_table <- tidy_model %>%
  select(term, y.level, coef_se) %>%
  pivot_wider(
    names_from = y.level,
    values_from = coef_se
  ) %>%
  rename(Variable = term)

# HTML
kable(wide_table, format = "html", align = "l", caption = "Multinomial Logit Results: 15 yo Child") %>%
  kable_styling(full_width = FALSE, font_size = 12) %>%
  save_kable("model15_logit_wide.html")

# Convert to PNG
webshot("model15_logit_wide.html", file = "model15_logit_wide.png", vwidth = 1600, vheight = 1000)

# Threshold: 20 ---------------------------------------------------------------#
acs20$housing <- factor(acs20$housing) # To let R know that these numbers are discrete categories and not continuous numbers

model20_logit <- multinom(housing ~MALE + AGE + MARST + HIEDUC +BLACK + ASIAN + NATIVE + HISPANIC + OTHER + HINSCAID + SPEAKENG + state_exp_capita + state_support_capita, data = acs20)
summary(model20_logit)

# Nice looking table
housing_labels <- c("hh_alone", "hh_withspouse", "hh_withotherrel", 
                    "hh_withnonrel", "GQ_inst", "GQ_noninst", "GQ_other")

# Tidy model output
tidy_model <- tidy(model20_logit) %>%
  mutate(
    signif = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      p.value < 0.1   ~ ".",
      TRUE            ~ ""
    ),
    coef_se = paste0(round(estimate, 3), signif, "\n(", round(std.error, 3), ")")
  )

# Add readable labels to y.level
tidy_model$y.level <- factor(tidy_model$y.level,
                             levels = sort(unique(tidy_model$y.level)),
                             labels = housing_labels)
# Pivot to wide format
wide_table <- tidy_model %>%
  select(term, y.level, coef_se) %>%
  pivot_wider(
    names_from = y.level,
    values_from = coef_se
  ) %>%
  rename(Variable = term)

# HTML
kable(wide_table, format = "html", align = "l", caption = "Multinomial Logit Results: 20 yo Child") %>%
  kable_styling(full_width = FALSE, font_size = 12) %>%
  save_kable("model20_logit_wide.html")

# Convert to PNG
webshot("model20_logit_wide.html", file = "model20_logit_wide.png", vwidth = 1600, vheight = 1000)
