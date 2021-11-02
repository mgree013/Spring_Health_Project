#Matthew Douglas Green
#November 9, 2021
#Project: Spring Health Data Scientist  Assessment

#Part 1: Data Prepare: Load and clean data

################################################################################################################################################
#Load Packages
Packages <- c("tidyverse", "ggplot2",  "viridis", "performance","bbmle")
lapply(Packages, library, character.only = TRUE)

################################################################################################################################################
#Load Data

data_apt_hashed<-read.csv(file="Data/appointments_hashed.csv")
data_providers_hased<-read.csv(file="Data/providers_hashed.csv")
summary(data_apt_hashed)
summary(data_providers_hased)
str(data_apt_hashed)
str(data_providers_hased)

#Data Clean and Merge
#1) Added column names using metadata information