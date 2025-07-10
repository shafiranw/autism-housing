rm(list=ls())

library(foreign)
library(base)
library(dplyr)
library(haven)
library(readxl)
library(stringr)
library(expss)
library(vtable)
library(huxtable)
library(ggplot2)
library(readr)
library(ipumsr)
library(tidyr)

PathIn <- file.path("Documents", "PhD", "Projects", "autism_housing", "data")
setwd(PathIn)

# CLEAN UP ACS ----------------------------------------------------------------#
# Load Data
acs0 <- read.csv("~/Documents/PhD/Projects/autism_housing/data/usa_00024.csv.gz")

# Identify single parents with cognitive disabilities
acs0$hh_head <- ifelse(acs0$RELATE==1,1,0)
acs0$hh_child <- ifelse(acs0$RELATE==3,1,0)

single <- acs0 %>%
  filter(AGE >= 18 & AGE <= 60,
         DIFFREM == 2,
         hh_head == 1,
         SPLOC == 0, MOMLOC == 0, POPLOC == 0,
         FAMSIZE > 1)
target_serials <- single$SERIAL

head_ages <- single %>%
  select(SERIAL, head_age = AGE)

# Keep only household members in HH with valid single parents above
single_w_child <- acs0 %>%
  filter(SERIAL %in% target_serials,
         (hh_head == 1 & AGE >= 18 & AGE <= 60 &
            DIFFREM == 2 &
            SPLOC == 0 & MOMLOC == 0 & POPLOC == 0 &
            FAMSIZE > 1) |
           hh_child == 1)

single_w_child <- single_w_child %>%
  left_join (head_ages, by = "SERIAL")

single_w_child <- single_w_child %>%
  filter(!(hh_child == 1 & AGE >= head_age)) #Removed children older than heads

# See only children
child <- single_w_child %>%
  filter(hh_child == 1)

summary(child)
hist(child$AGE)
axis_ticks <- pretty(child$AGE)  # default pretty ticks
custom_ticks <- sort(unique(c(axis_ticks, 13, 15, 20)))  # add custom ages
axis(1, at = custom_ticks, labels = custom_ticks)
abline(v = 13, col = "red", lwd = 2, lty = 2)  
abline(v = 15, col = "blue", lwd = 2, lty = 2)  
abline(v = 20, col = "green", lwd = 2, lty = 2)

# Three categories 
child_age_by_serial <- single_w_child %>%
  filter(hh_child == 1) %>%
  group_by(SERIAL) %>%
  summarise(oldest_child_age = max(AGE, na.rm = TRUE), .groups = "drop")

single_w_child <- single_w_child %>%
  left_join(child_age_by_serial, by = "SERIAL") %>%
  mutate(
    single_child_less13 = if_else(oldest_child_age <= 13, 1, 0, missing = 0),
    single_child_less15 = if_else(oldest_child_age <= 15, 1, 0, missing = 0),
    single_child_less20 = if_else(oldest_child_age <= 20, 1, 0, missing = 0)
  )

# Now only keep hh_heads
single <- single_w_child %>%
  filter(hh_head == 1)

# Join with cleaned dataset from last week
acs <- read.csv("~/Documents/PhD/Projects/autism_housing/data/acs_070325.csv")

new_vars <- single %>%
  select(SERIAL, PERNUM, single_child_less13, single_child_less15, single_child_less20)

acs <- acs %>%
  left_join(new_vars, by = c("SERIAL", "PERNUM"))

# 13 yo threshold
acs13 <- acs

acs13$hh_alone_13 <- ifelse(acs13$hh_head == 1 & acs13$FAMSIZE == 1, 1,
                            ifelse(acs13$single_child_less13 == 1, 1, 0))



acs13$hh_withotherrel_13 <- ifelse(acs13$RELATE %in% 5:10 &
                                     acs13$hh_alone == 0 &
                                     acs13$hh_withspouse == 0 &
                                     acs13$hh_withparent == 0 &
                                     acs13$hh_withnonrel == 0, 1,
                                   ifelse(acs13$hh_alone_13 == 0, 1, 0))

acs13$hh_alone_13 <- replace_na(acs13$hh_alone_13, 0)
acs13$hh_withotherrel_13 <- replace_na(acs13$hh_withotherrel_13, 0)


acs13 <- acs13 %>%
  mutate(housing = case_when(
    hh_withparent == 1 & GQ == 0 ~ 1,
    hh_alone_13 == 1 & GQ == 0 ~ 2,
    hh_withspouse == 1 & GQ == 0 ~ 3,
    hh_withotherrel_13 == 1 & GQ == 0 ~ 4,
    hh_withnonrel == 1 & GQ == 0 ~ 5,
    GQ_inst == 1 ~ 6,
    GQ_noninst == 1 ~ 7,
    GQ_other == 1 ~ 8,
    TRUE ~ 0
  ))

table(acs13$housing)

# 15 yo
acs15 <- acs

# Define hh_alone_15
acs15$hh_alone_15 <- ifelse(acs15$hh_head == 1 & acs15$FAMSIZE == 1, 1,
                            ifelse(acs15$single_child_less15 == 1, 1, 0))

# Define hh_withotherrel_15
acs15$hh_withotherrel_15 <- ifelse(acs15$RELATE %in% 5:10 &
                                     acs15$hh_alone == 0 &
                                     acs15$hh_withspouse == 0 &
                                     acs15$hh_withparent == 0 &
                                     acs15$hh_withnonrel == 0, 1,
                                   ifelse(acs15$hh_alone_15 == 0, 1, 0))

# Replace NA with 0
acs15$hh_alone_15 <- replace_na(acs15$hh_alone_15, 0)
acs15$hh_withotherrel_15 <- replace_na(acs15$hh_withotherrel_15, 0)

# Define housing categories
acs15 <- acs15 %>%
  mutate(housing = case_when(
    hh_withparent == 1 & GQ == 0 ~ 1,
    hh_alone_15 == 1 & GQ == 0 ~ 2,
    hh_withspouse == 1 & GQ == 0 ~ 3,
    hh_withotherrel_15 == 1 & GQ == 0 ~ 4,
    hh_withnonrel == 1 & GQ == 0 ~ 5,
    GQ_inst == 1 ~ 6,
    GQ_noninst == 1 ~ 7,
    GQ_other == 1 ~ 8,
    TRUE ~ 0
  ))

# 20 yo
acs20 <- acs

# Define hh_alone_20
acs20$hh_alone_20 <- ifelse(acs20$hh_head == 1 & acs20$FAMSIZE == 1, 1,
                            ifelse(acs20$single_child_less20 == 1, 1, 0))

# Define hh_withotherrel_20
acs20$hh_withotherrel_20 <- ifelse(acs20$RELATE %in% 5:10 &
                                     acs20$hh_alone == 0 &
                                     acs20$hh_withspouse == 0 &
                                     acs20$hh_withparent == 0 &
                                     acs20$hh_withnonrel == 0, 1,
                                   ifelse(acs20$hh_alone_20 == 0, 1, 0))

# Replace NA with 0
acs20$hh_alone_20 <- replace_na(acs20$hh_alone_20, 0)
acs20$hh_withotherrel_20 <- replace_na(acs20$hh_withotherrel_20, 0)

# Define housing categories
acs20 <- acs20 %>%
  mutate(housing = case_when(
    hh_withparent == 1 & GQ == 0 ~ 1,
    hh_alone_20 == 1 & GQ == 0 ~ 2,
    hh_withspouse == 1 & GQ == 0 ~ 3,
    hh_withotherrel_20 == 1 & GQ == 0 ~ 4,
    hh_withnonrel == 1 & GQ == 0 ~ 5,
    GQ_inst == 1 ~ 6,
    GQ_noninst == 1 ~ 7,
    GQ_other == 1 ~ 8,
    TRUE ~ 0
  ))

# Save Dataset
write.csv(acs13, "acs13_071025.csv", row.names = FALSE)
write.csv(acs15, "acs15_071025.csv", row.names = FALSE)
write.csv(acs20, "acs20_071025.csv", row.names = FALSE)