#Ustawianie working directory
#setwd("/Users/emmapanasiuk/Licencjat_WNE/dane")

#Biblioteki
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse) 
library(dplyr)

options(scipen=999)

#Wczytanie danych z WVS
dane <- read.csv("WVS_Cross-National_Wave_7_csv_v6_0.csv",
				header=T,
				sep=",",
				dec=".")

#Dobór danych z WVS (rok, kraj, odpowiedzi na pytania)
dane2 <- select(dane, A_YEAR, B_COUNTRY_ALPHA, Q106:Q110)
dane2 <- na.omit(dane2)
dane2 <- subset(dane2, A_YEAR == 2018)
dane2 <- rename(dane2, Country.Code = B_COUNTRY_ALPHA)

#Dodanie zmiennej polaryzacji
dane_model <- dane2 %>%
  group_by(Country.Code) %>%
  summarise(across(Q106:Q110, sd, na.rm = TRUE))
dane_model

#Wczytanie PKB per capita
gdp <- read.csv("GDP.csv",
                header=T,
                sep=",",
                dec=".")
gdp<- select(gdp, Country.Code, X2018)
gdp <- rename(gdp, GDP = X2018)
kraje_model <- unique(dane2$Country.Code)
gdp <- filter(gdp, Country.Code %in% kraje_model)
dane_model <- merge(dane_model, gdp, by = "Country.Code")
View(dane_model)

#Wczytanie Giniego
gini <- read.csv("Gini.csv",
                header=T,
                sep=",",
                dec=".")
gini <- select(gini, Country.Code, X2018)
gini <- filter(gini, Country.Code %in% kraje_model)
gini <- rename(gini, gini_index = X2018)
dane_model <- merge(dane_model, gini, by = "Country.Code")

#Wczytanie wydatkow rzadowych
expenses <- read.csv("expenses.csv",
                 header=T,
                 sep=",",
                 dec=".")
expenses <- select(expenses, Country.Code, X2018)
expenses <- filter(expenses, Country.Code %in% kraje_model)
expenses <- rename(expenses, govt_expenses = X2018)
dane_model <- merge(dane_model, expenses, by = "Country.Code")

#Wczytanie globalizacji inwestycji zagr.
investments <- read.csv("investments.csv",
                     header=T,
                     sep=",",
                     dec=".")
investments <- select(investments, Country.Code, X2018)
investments <- filter(investments, Country.Code %in% kraje_model)
investments <- rename(investments, foreign_investments = X2018)
dane_model <- merge(dane_model, investments, by = "Country.Code")

#demokracja
democracy <- read.csv("freedom_score.csv",
                        header=T,
                        sep=",",
                        dec=".")
democracy <- select(democracy, Code, Total.democracy.score, Year)
democracy <- filter(democracy, democracy$Year == 2018)
democracy <- filter(democracy, Code %in% kraje_model)
democracy <- select(democracy, -c(Year))
democracy <- rename(democracy, Country.Code = Code)
dane_model <- merge(dane_model, democracy, by = "Country.Code")

#gestosc zaludnienia
pop_density <- read.csv("pop_density.csv",
                        header=T,
                        sep=",",
                        dec=".")
pop_density <- select(pop_density, Country.Code, X2018)
pop_density <- filter(pop_density, Country.Code %in% kraje_model)
pop_density <- rename(pop_density, density = X2018)
dane_model <- merge(dane_model, pop_density, by = "Country.Code")

#bezrobocie
unemployment <- read.csv("unemployment.csv",
                        header=T,
                        sep=",",
                        dec=".")
unemployment <- select(unemployment, Country.Code, X2018)
unemployment <- filter(unemployment, Country.Code %in% kraje_model)
unemployment <- rename(unemployment, unemployment_rate = X2018)
dane_model <- merge(dane_model, unemployment, by = "Country.Code")

#imigranci
immigrants <- read.csv("immigrants.csv",
                         header=T,
                         sep=",",
                         dec=".")
immigrants <- select(immigrants, Country.Code, X2020)
immigrants <- filter(immigrants, Country.Code %in% kraje_model)
immigrants <- rename(immigrants, immigrants_percent = X2020)
dane_model <- merge(dane_model, immigrants, by = "Country.Code")

#internet
internet <- read.csv("internet.csv",
                       header=T,
                       sep=",",
                       dec=".")
internet <- select(internet, Country.Code, X2018)
internet <- filter(internet, Country.Code %in% kraje_model)
internet <- rename(internet, internet_access = X2018)
dane_model <- merge(dane_model, internet, by = "Country.Code")

#edukacja
education <- read.csv("expenses_education.csv",
                     header=T,
                     sep=",",
                     dec=".")
education <- select(education, Country.Code, X2018)
education <- filter(education, Country.Code %in% kraje_model)
education <- rename(education, education_expenses = X2018)
dane_model <- merge(dane_model, education, by = "Country.Code")


lm(Q106 ~ GDP+gini_index+govt_expenses+foreign_investments+Total.democracy.score
   +density+unemployment_rate+immigrants_percent+internet_access+education_expenses
   , data = dane_model2)
