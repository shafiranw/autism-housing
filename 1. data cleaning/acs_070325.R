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

PathIn <- file.path("Documents", "PhD", "Projects", "autism_housing", "data")
setwd(PathIn)

# CLEAN UP ACS ----------------------------------------------------------------#
# Load Data
acs0 <- read.csv("~/Documents/PhD/Projects/autism_housing/data/usa_00024.csv.gz")

# See sample size -------------------------------------------------------------#
# Restrict age to 18-60
acs0 <- acs0[acs0$AGE >= 18 & acs0$AGE <= 60, ]

#Only people with cognitive difficulties
acs1 <- acs0[!(acs0$DIFFREM == 1),]
acs1 <- mutate(acs1, DIFFREM = ifelse(DIFFREM==2, 1,0))

# Expenditure -----------------------------------------------------------------#
acs1$state_support <- 

# Demographic ------------------------------------------------------------------#
# Fix MARSTAT
acs1$MARST <- ifelse(acs1$MARST<=2,1,0)

# Fix MALE
acs1$MALE <- ifelse(acs1$SEX==1,1,
                    ifelse(acs1$SEX==2,0,NA))

# Race
acs1$WHITE <- ifelse(acs1$RACE==1,1,0) #72%
acs1$BLACK <- ifelse(acs1$RACE==2,1,0) #15%
acs1$ASIAN <- ifelse(acs1$RACE %in% c(4, 5, 6), 1, 0) #2.4%
acs1$NATIVE <- ifelse(acs1$RACE==3,1,0) #1.7%
acs1$HISPANIC <- ifelse(acs1$HISPAN>0 & acs1$WHITE==0,1,0) #5.8%
acs1$OTHER <- ifelse(acs1$WHITE == 0 & acs1$BLACK == 0 & acs1$ASIAN == 0 & acs1$NATIVE == 0 & acs1$HISPANIC == 0,1,0) #3.7%

# Educ (Mean = 12.29)
acs1$HIEDUC <- ifelse(acs1$EDUCD == 014, 1,
                     ifelse(acs1$EDUCD == 015, 2,
                            ifelse(acs1$EDUCD == 016, 3,
                                   ifelse (acs1$EDUCD == 017, 4,
                                           ifelse(acs1$EDUCD == 022, 5,
                                                  ifelse(acs1$EDUCD == 023, 6,
                                                         ifelse(acs1$EDUCD == 025, 7,
                                                                ifelse(acs1$EDUCD == 026, 8,
                                                                       ifelse(acs1$EDUC == 03, 9,
                                                                              ifelse(acs1$EDUC == 04, 10,
                                                                                     ifelse(acs1$EDUC == 05, 11,
                                                                                            ifelse(acs1$EDUC == 06, 12,
                                                                                                   ifelse(acs1$EDUC == 07, 13,
                                                                                                          ifelse(acs1$EDUC == 08, 14,
                                                                                                                 ifelse(acs1$EDUC == 09, 15,
                                                                                                                        ifelse(acs1$EDUC == 10, 16,
                                                                                                                               ifelse(acs1$EDUCD == 114, 18,
                                                                                                                                      ifelse(acs1$EDUCD == 116, 21, NA))))))))))))))))))

# Citizenship (Not useful i think)

# Speak english (99% speaks english)
acs1$SPEAKENG <- ifelse(acs1$SPEAKENG == 1, 0,
                        ifelse(acs1$SPEAKENG %in% 2:6, 1, NA))

# Medicaid (41% has Medicaid)
acs1$HINSCAID <- ifelse(acs1$HINSCAID == 2, 1,0)

# Living Situation ------------------------------------------------------------#
# See living arrangement
acs2 <- acs1
unique(acs2$GQ) # 1 2 3 4 5
unique(acs2$GQTYPE) # 0 1 5 9 
unique(acs2$RELATE) # 1 2 3 4 5 6 7 8 9 10 11 12 13
unique(acs2$RELATED)

# Separate variables
## Housing type
acs2 <- acs2 %>% 
  rename(RES = GQ)

acs2$HH <- ifelse(acs2$RES<=2,1,0)
acs2$GQ <- ifelse(acs2$RES>=3,1,0)

## In Household
acs2$hh_head <- ifelse(acs2$RELATE==1,1,0)
acs2$hh_spouse <- ifelse(acs2$RELATE==2,1,0)
acs2$hh_child <- ifelse(acs2$RELATE==3,1,0)
acs2$hh_childil <- ifelse(acs2$RELATE==4,1,0)
acs2$hh_parent <- ifelse(acs2$RELATE==5,1,0)
acs2$hh_parentil <- ifelse(acs2$RELATE==6,1,0)
acs2$hh_sibling <- ifelse(acs2$RELATE==7,1,0)
acs2$hh_siblingil <- ifelse(acs2$RELATE==8,1,0)
acs2$hh_gchild <- ifelse(acs2$RELATE==9,1,0)
acs2$hh_otherrel <- ifelse(acs2$RELATE==10,1,0)
acs2$hh_partner <- ifelse(acs2$RELATED==1114,1,0)
acs2$hh_mate <- ifelse(acs2$RELATE==1115,1,0)
acs2$hh_othernon <- ifelse(acs2$RELATE==12,1,0)
acs2$hh_insmate <- ifelse(acs2$RELATE==13,1,0)

# Conditional on being HH head, how many people live with other family members?
#acs_head <- acs2[acs2$hh_head == 1, ]
  ## How many of them live alone?
    #nrow(acs_head[acs_head$FAMSIZE == "1",]) #29,587 (so 43,281 has a family)

acs2$hh_alone <- ifelse(acs2$hh_head==1 & acs2$FAMSIZE==1,1,
                        ifelse(acs2$hh_head ==1 & acs2$SPLOC==0 & acs2$MOMLOC==0 & acs2$POPLOC==0 & acs2$FAMSIZE>1,1,0))
acs2$hh_withspouse <- ifelse(acs2$RELATE==2,1,
                             ifelse(acs2$RELATE==1 & acs2$SPLOC>0,1,0)) #Spouse + Head and married
acs2$hh_withparent <- ifelse(acs2$hh_withspouse == 0 & acs2$RELATE==3,1,
                             ifelse(acs2$hh_withspouse == 0 & acs2$RELATE==4,1,
                                    ifelse(acs2$hh_withspouse == 0 & acs2$MOMLOC>0,1,
                                           ifelse(acs2$hh_withspouse == 0 & acs2$POPLOC>0,1,0)))) #Child + child-in-law + mom in hh + dad in hh
acs2$hh_withnonrel <- ifelse(acs2$RELATE==11 & acs2$hh_alone == 0 & acs2$hh_withspouse == 0 & acs2$hh_withparent == 0,1,
                             ifelse(acs2$RELATE==12 & acs2$hh_alone == 0 & acs2$hh_withspouse == 0 & acs2$hh_withparent == 0,1,
                                    ifelse(acs2$RELATE==13 & acs2$hh_alone == 0 & acs2$hh_withspouse == 0 & acs2$hh_withparent == 0,1,0))) #Partner/friend/visitor + other non relatives + institutional inmate
acs2$hh_withotherrel <- ifelse(acs2$RELATE==5 & acs2$hh_alone == 0 & acs2$hh_withspouse == 0 & acs2$hh_withparent == 0 & acs2$hh_withnonrel== 0, 1,
                               ifelse(acs2$RELATE == 6 & acs2$hh_alone == 0 & acs2$hh_withspouse == 0 & acs2$hh_withparent == 0 & acs2$hh_withnonrel== 0, 1,
                                      ifelse(acs2$RELATE == 7 & acs2$hh_alone == 0 & acs2$hh_withspouse == 0 & acs2$hh_withparent == 0 & acs2$hh_withnonrel== 0, 1,
                                             ifelse(acs2$RELATE == 8 & acs2$hh_alone == 0 & acs2$hh_withspouse == 0 & acs2$hh_withparent == 0 & acs2$hh_withnonrel== 0, 1,
                                                    ifelse(acs2$RELATE == 9 & acs2$hh_alone == 0 & acs2$hh_withspouse == 0 & acs2$hh_withparent == 0 & acs2$hh_withnonrel== 0, 1,
                                                           ifelse(acs2$RELATE == 10 & acs2$hh_alone == 0 & acs2$hh_withspouse == 0 & acs2$hh_withparent == 0 & acs2$hh_withnonrel== 0, 1,
                                                                  ifelse(acs2$RELATE == 1 & acs2$hh_alone == 0 & acs2$hh_withspouse == 0 & acs2$hh_withparent == 0 & acs2$hh_withnonrel== 0 & acs2$FAMSIZE>1, 1, 0)))))))

#nrow(acs2[acs2$hh_alone == "1",]) #43,007 / 23%
#nrow(acs2[acs2$hh_withspouse == "1",]) #52,509 / 28.1%
#nrow(acs2[acs2$hh_withparent == "1",]) #40,749 / 21.81%
#nrow(acs2[acs2$hh_withotherrel == "1",]) #22,251 / 11.91%
#nrow(acs2[acs2$hh_withnonrel == "1",]) #41,781 / 22.36%

## In GQ
acs2$GQ_inst <- ifelse(acs2$GQTYPE==1,1,0)
acs2$GQ_noninst <- ifelse(acs2$GQTYPE==5,1,0)
acs2$GQ_other <- ifelse(acs2$GQTYPE==9,1,0)

#nrow(acs2[acs2$GQ_inst == "1",]) #13,423 / 7.2%
#nrow(acs2[acs2$GQ_noninst == "1",]) #15,609 / 8.3%
#nrow(acs2[acs2$GQ_other == "1",]) #60 / 0.03%

# Age distributions
acs_head <- acs2[acs2$hh_head == 1, ]
acs_singleparent <- acs_head[acs_head$SPLOC==0 & acs_head$MOMLOC==0 & acs_head$POPLOC==0 & acs_head$FAMSIZE>1, ] #13,420
acs_alone <- acs_head[acs_head$FAMSIZE==1, ]
hist(acs_singleparent$AGE)
acs_live_alone <- acs2[acs2$hh_alone == 1, ]
hist(acs_live_alone$AGE)
hist(acs_alone$AGE)

# Housing types for multinomial logit -----------------------------------------#
acs2 <- acs2 %>%
  mutate(housing = case_when(
    hh_withparent == 1 & GQ == 0 ~ 1,
    hh_alone == 1 & GQ == 0 ~ 2,
    hh_withspouse == 1 & GQ == 0 ~ 3,
    hh_withotherrel == 1 & GQ == 0 ~ 4,
    hh_withnonrel == 1 & GQ == 0 ~ 5,
    GQ_inst == 1 ~ 6,
    GQ_noninst == 1 ~ 7,
    GQ_other ==1 ~ 8,
    TRUE ~ 0  # fallback value
  ))

#unique(acs2$housing)
#nrow(acs2[acs2$housing == "0",]) #0
#summary(acs2)
# STATE EXP -------------------------------------------------------------------#
# Load Dataset
states <- read.csv("~/Documents/PhD/Projects/autism_housing/data/states_070325.csv")

# Summarize all people with cognitive disabilities (PERWT) for each state and each year
cogdis_pop <- acs2 %>%
  group_by(STATEFIP, YEAR) %>%
  summarise(
    cogdis_pop = sum(PERWT),
    .groups = "drop"
  )

# Add into "states" dataset
states <- states %>%
  left_join(cogdis_pop, by = c("STATEFIP", "YEAR"))

# Add per capita variables
states$state_exp_capita <- states$state_exp/states$cogdis_pop
states$state_support_capita <- states$state_support/states$cogdis_pop

states <- states[, !(names(states) %in% c("state_exp", "state_support","cogdis_pop"))]

# Merge ACS and STATES --------------------------------------------------------#
data <- left_join(acs2, states, by = c("STATEFIP", "YEAR"))

# Save dataset -------------------------------------------------------------------------#
write.csv(data, "acs_070325.csv", row.names = FALSE)
