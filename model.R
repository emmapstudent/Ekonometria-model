#Ustawianie working directory
setwd("/Users/emmapanasiuk/Licencjat_WNE/dane")
#Biblioteki
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse) 
library(dplyr)
#Wczytanie danych z WVS
dane <- read.csv("WVS_Cross-National_Wave_7_csv_v6_0.csv",
				header=T,
				sep=",",
				dec=".")
#Dobór danych z WVS (rok, kraj, odpowiedzi na pytania)
dane_model <- select(dane, A_YEAR, B_COUNTRY_ALPHA, Q106:Q110)
dane_model <- na.omit(dane_model)
dane_model <- subset(dane_model, A_YEAR == 2018)
dane_model <- rename(dane_model, Country.Code = B_COUNTRY_ALPHA)
dane_model <- select(dane_model, -c(Q106, Q107, Q108, Q109, Q110))
#Dodanie zmiennej polaryzacji
country_sd <- dane_model %>%
  group_by(B_COUNTRY_ALPHA) %>%
  summarise(across(Q106:Q110, sd, na.rm = TRUE))
country_sd
dane_model <- dane_model %>%
  left_join(country_sd, by = "Country.Code")
#Wczytanie PKB per capita
gdp <- read.csv("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_2518.csv",
                header=T,
                sep=",",
                dec=".")
gdp<- select(gdp, Country.Code, X2018)
kraje_model <- unique(dane_model$B_COUNTRY_ALPHA)
gdp <- filter(gdp, Country.Code %in% kraje_model)
dane_model <- dane_model %>%
  left_join(gdp, by = "Country.Code")
#Wczytanie Giniego
gini <- read.csv("API_NY.GDP.PCAP.CD_DS2_en_csv_v2_2518.csv",
                header=T,
                sep=",",
                dec=".")
gini <- select(gini, Country.Code, X2018)
gini <- filter(gdp, Country.Code %in% kraje_model)
dane_model <- left_join(dane_model, gdp, by = "Country.Code") #cos z tym nie tak
#jeszcze pobrac: wskaznik demokracji, % imigrantow, poziom dostepu do internetu