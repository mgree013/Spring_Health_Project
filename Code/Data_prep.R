#Matthew Douglas Green
#October 9, 2021
#Project: Dataiku Data Scientist Technical Assessment

#part 1: Data Prepare: Load and clean data

################################################################################################################################################
#Load Packages
Packages <- c("tidyverse", "ggplot2", "scales", "viridis", "FD","multcomp","semPlot","lavaan","stringr",  "performance","bbmle","cluster","mapview","sf","leaflet","rnaturalearth","rnaturalearthdata","ggpubr","MuMIn","randomForest","InformationValue")
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