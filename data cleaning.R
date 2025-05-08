# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(progress)

# Load your data
data <- read.csv("data/subset vars all rounds.csv")

# ---- 1. Define variable sets ----

library(dplyr)

# Define variables of interest
socio_econ_vars <- c("gndr", "agea", "eisced", "domicil", "emplrel", "wrkctra", "tporgwk",
                     "wrkac6m", "uemp12m", "hinctnta", "hincfel", "health")

wellbeing_vars <- c("sclmeet", "aesfdrk", "hlthhmp")

personal_values_vars <- c("ipcrtiv", "imprich", "ipeqopt", "ipshabt", "impsafe", "impdiff",
                          "ipfrule", "ipudrst", "ipmodst", "ipgdtim", "impfree", "iphlppl",
                          "ipsuces", "ipstrgv", "ipadvnt", "ipbhprp", "iprspot", "iplylfr",
                          "impenv", "imptrad", "impfun")

 
all_vars <- c("happy", socio_econ_vars, wellbeing_vars, personal_values_vars)

# Cleaned dataset with all filtering applied
clean_data <- data %>%
  select(all_of(all_vars)) %>%
  filter(happy %in% 0:10) %>%
  mutate(
    gndr     = na_if(gndr, 9),
    agea     = na_if(agea, 999),
    eisced   = na_if(eisced, 55),
    eisced   = na_if(eisced, 77),
    eisced   = na_if(eisced, 88),
    eisced   = na_if(eisced, 99),
    domicil  = na_if(domicil, 7),
    domicil  = na_if(domicil, 8),
    domicil  = na_if(domicil, 9),
    emplrel  = na_if(emplrel, 6),
    emplrel  = na_if(emplrel, 7),
    emplrel  = na_if(emplrel, 8),
    emplrel  = na_if(emplrel, 9),
    wrkctra  = na_if(wrkctra, 6),
    wrkctra  = na_if(wrkctra, 7),
    wrkctra  = na_if(wrkctra, 8),
    wrkctra  = na_if(wrkctra, 9),
    tporgwk  = if_else(tporgwk >= 66 & tporgwk <= 99, NA, tporgwk),
    wrkac6m  = if_else(wrkac6m >= 6 & wrkac6m <= 9, NA, wrkac6m),
    uemp12m  = if_else(uemp12m >= 6 & uemp12m <= 9, NA, uemp12m),
    hinctnta = na_if(hinctnta, 77),
    hinctnta = na_if(hinctnta, 88),
    hinctnta = na_if(hinctnta, 99),
    hincfel  = na_if(hincfel, 7),
    hincfel  = na_if(hincfel, 8),
    hincfel  = na_if(hincfel, 9),
    health   = na_if(health, 7),
    health   = na_if(health, 8),
    health   = na_if(health, 9),
    sclmeet  = na_if(sclmeet, 77),
    sclmeet  = na_if(sclmeet, 88),
    sclmeet  = na_if(sclmeet, 99),
    aesfdrk  = na_if(aesfdrk, 7),
    aesfdrk  = na_if(aesfdrk, 8),
    aesfdrk  = na_if(aesfdrk, 9),
    hlthhmp  = na_if(hlthhmp, 7),
    hlthhmp  = na_if(hlthhmp, 8),
    hlthhmp  = na_if(hlthhmp, 9),
  ) %>%
  # Clean personal values (remove 7–9)
  mutate(across(all_of(personal_values_vars), ~if_else(. >= 7 & . <= 9, NA, .))) %>%
  drop_na()

