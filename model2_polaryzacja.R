#Ustawianie working directory
#setwd("/Users/emmapanasiuk/Licencjat_WNE/dane")
setwd("C:/Users/Emma/Ekonometria")
#Biblioteki
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse) 
library(dplyr)
library(tidyr)

options(scipen=999)

#Wczytanie danych z WVS
dane <- read.csv("WVS_Cross-National_Wave_7_csv_v6_0.csv",
				header=T,
				sep=",",
				dec=".")

#Dobór danych z WVS (rok, kraj, odpowiedzi na pytania)
dane2 <- select(dane, A_YEAR, B_COUNTRY_ALPHA, Q106:Q110)
dane2 <- na.omit(dane2)
dane2 <- rename(dane2, Country.Code = B_COUNTRY_ALPHA)
dane2 <- rename(dane2, YEAR = A_YEAR)

#Dodanie zmiennej polaryzacji
dane_model <- dane2 %>%
  group_by(Country.Code, YEAR) %>%
  summarise(across(Q106:Q110, sd, na.rm = TRUE))
view(dane_model)

#Wczytanie danych z World Bank
dane_wb <- read.csv("Data_WB4.csv",
                header=T,
                sep=",",
                dec=".")
dane_wb <- select(dane_wb, -c(Time.Code, Country.Name))
dane_wb <- rename(dane_wb, YEAR=Time)
view(dane_wb)

#Polaczenie danych
dane_model2 <- merge(dane_model, dane_wb, by = c("Country.Code", "YEAR"))
dane_model2 <- select(dane_model2, -c(International.migrant.stock....of.population...SM.POP.TOTL.ZS.))
na.omit(dane_model2)
view(dane_model2)

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


lm(Q106 ~ GDP+gini_index+govt_expenses+foreign_investments+Total.democracy.score
   +density+unemployment_rate+immigrants_percent+internet_access+education_expenses
   , data = dane_model)
