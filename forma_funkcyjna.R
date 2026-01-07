#Biblioteki
library(tidyverse) 
library(dplyr)
library(tidyr)
library(haven)
library(ggplot2)
#install.packages("stargazer")
library(stargazer)
#install.packages("viridis")
library(viridis)
#install.packages("car")
library(car)
library(lmtest)
library(tseries)
#install.packages("modelsummary")
library (modelsummary)

options(scipen=999)

#Ustawienie working directory
setwd("/Users/emmapanasiuk/Ekonometria")

#Wczytanie danych
dane <- read.csv("dane_gminy_kobiety_2021.csv",
                 header=T,
                 sep=",",
                 dec=".")
view(dane)
dane <- rename(dane, nazwa = gmina_name)

#Przeksztalcenie bazy danych
#Dodanie współczynnika wykształcenia kobiet
dane <- mutate(dane, 
               kobiety_dorosle = kobiety_w_wieku_produkcyjnym +
                 kobiety_w_wieku_poprodukcyjnym)
dane <- mutate(dane, 
               wspolczynnik_wyksztalcenia = kobiety_wyksztalcenie_wyzsze/
                 kobiety_dorosle)
#Dodanie ogólnego współczynnika płodności
dane <- mutate(dane, plodnosc = 
                 urodzenia_zywe_na_1000/kobiety_w_wieku_produkcyjnym*1000)
#Dodanie zmiennej dyskretnej: miasto czy wieś
dane$czy_miasto <- ifelse(dane$wskaznik_urbanizacji == 100, 1, 0)
dane$czy_miasto <- factor(dane$czy_miasto, levels = c(0,1), 
                          labels = c("wies", "miasto"))
#Dodanie współczynnika korzystania ze świadczeń wychowawczych
swiadczenia <- read.csv("swiadczenia.csv",
                        header=T,
                        sep=",",
                        dec=".")
dane <- merge(dane, swiadczenia, by = c("gmina_id", "rok"))
dane <- mutate(dane,
               swiadczenia_wychowawcze =
                 swiadczenia_rodzinne/liczba_dzieci_do_17_lat)
#Usunięcie brakujących danych
dane <- filter(dane, zasilek_rodzinny_wspolczynnik != 0)
dane <- filter(dane, stopa_bezrobocia_ogolem != 0)
zero <- subset(dane, obciazenie_demograficzne == 0 |
                 stopa_bezrobocia_ogolem == 0 |
                 malzenstwa_na_1000 == 0 |
                 plodnosc == 0 |
                 wspolczynnik_wyksztalcenia == 0 |
                 aktywnosc_zawodowa_kobiet == 0 |
                 kobiety_w_wieku_produkcyjnym == 0 |
                 kobiety_w_wieku_poprodukcyjnym == 0)
dim(zero)
view(zero)
zero2 <- subset(dane, dzieci_w_zlobkach_na_1000 == 0)
dim(zero2)
dziwne <- subset(dane, swiadczenia_wychowawcze > 1)
dim(dziwne)
#Wyodrębnienie zbioru do ostatecznej regresji
dane2 <- subset(dane, select = c(gmina_id, nazwa, aktywnosc_zawodowa_kobiet, 
                                 zasilek_rodzinny_wspolczynnik, 
                                 dzieci_w_zlobkach_na_1000,
                                 dzieci_w_przedszkolach_na_1000,
                                 malzenstwa_na_1000,
                                 obciazenie_demograficzne,
                                 stopa_bezrobocia_ogolem,
                                 plodnosc,
                                 czy_miasto,
                                 wspolczynnik_wyksztalcenia,
                                 swiadczenia_wychowawcze))
view(dane2)
dim(dane2)

sum(is.na(dane2))
#Analiza danych
#Histogram zmiennej objaśnianej
hist(dane2$aktywnosc_zawodowa_kobiet,
     col="lightblue",
     border="black",
     xlab = " ",
     main = "Histogram współczynnika aktywności zawodowej kobiet")
#Tabela zmiennych
objasniana <- subset(dane2, select = c(aktywnosc_zawodowa_kobiet))
stargazer(
  objasniana,
  type = "html",
  out  = "dane.html",
  covariate.labels = c("Aktywność zawodowa kobiet"))
objasniajace <- subset(dane2, 
                       select =
                         c(zasilek_rodzinny_wspolczynnik:swiadczenia_wychowawcze))
objasniajace <- subset(objasniajace, 
                       select = -c(czy_miasto))
stargazer(
  objasniajace,
  type = "html",
  out  = "objasniajace.html",
  covariate.labels = c("Współczynnik pobierania zasiłku rodzinnego",
                       "Dzieci w żłobkach na 1000 dzieci",
                       "Dzieci w przedszkolach na 1000 dzieci",
                       "Małżeństwa zawarte na 1000 ludności",
                       "Obciążenie demograficzne starszymi",
                       "Stopa bezrobocia",
                       "Wskaźnik płodności",
                       "Współczynnik wykształcenia",
                       "Współczynnik pobierania świadczeń wychowawczych"))
wsie <- filter(dane2, dane2$czy_miasto == "wies")
wsie <- subset(wsie, select = c(aktywnosc_zawodowa_kobiet))
miasta <- filter(dane2, dane2$czy_miasto == "miasto")
miasta <- subset(miasta, select = c(aktywnosc_zawodowa_kobiet))
stargazer(
  wsie,
  type = "html",
  out  = "wsie.html",
  covariate.labels = c("Aktywność zawodowa dla wsi"))
stargazer(
  miasta,
  type = "html",
  out  = "miasta.html",
  covariate.labels = c("Aktywność zawodowa dla miast"))
#Przeprowadzenie regresji liniowej
reg1 <- lm(aktywnosc_zawodowa_kobiet ~ zasilek_rodzinny_wspolczynnik + 
             dzieci_w_zlobkach_na_1000 + dzieci_w_przedszkolach_na_1000 + 
             plodnosc + malzenstwa_na_1000 + 
             obciazenie_demograficzne + stopa_bezrobocia_ogolem + 
             wspolczynnik_wyksztalcenia + czy_miasto + 
             swiadczenia_wychowawcze, data = dane2)
summary(reg1)
linearHypothesis(reg1, c("dzieci_w_zlobkach_na_1000"))
#Usunięcie zmiennej nieistotnej: żłobków
reg2 <- lm(aktywnosc_zawodowa_kobiet ~ zasilek_rodzinny_wspolczynnik + 
             dzieci_w_przedszkolach_na_1000 + 
             plodnosc + malzenstwa_na_1000 + 
             obciazenie_demograficzne + stopa_bezrobocia_ogolem + 
             wspolczynnik_wyksztalcenia + czy_miasto + 
             swiadczenia_wychowawcze, data = dane2)
summary(reg2)
#Zastąpienie dzieci w żłobkach zmienną binarną
dane2$zlobek_dostep <- ifelse(dane2$dzieci_w_zlobkach_na_1000 > 0, 1, 0)
reg3 <- lm(aktywnosc_zawodowa_kobiet ~ zasilek_rodzinny_wspolczynnik + 
             dzieci_w_przedszkolach_na_1000 + 
             plodnosc + malzenstwa_na_1000 + 
             obciazenie_demograficzne + stopa_bezrobocia_ogolem + 
             wspolczynnik_wyksztalcenia + czy_miasto +
             zlobek_dostep+ 
             swiadczenia_wychowawcze, data = dane2)
summary(reg3)
linearHypothesis(reg3, c("zlobek_dostep")) #Zmienna binarna nadal nieistotna
#Dodanie interakcji zmiennych
reg4 <- lm(aktywnosc_zawodowa_kobiet ~ zasilek_rodzinny_wspolczynnik + 
             dzieci_w_przedszkolach_na_1000 + 
             plodnosc + malzenstwa_na_1000 + 
             obciazenie_demograficzne + stopa_bezrobocia_ogolem + 
             wspolczynnik_wyksztalcenia + czy_miasto +
             dzieci_w_przedszkolach_na_1000:czy_miasto +
             wspolczynnik_wyksztalcenia:stopa_bezrobocia_ogolem + 
             swiadczenia_wychowawcze, data = dane2)
summary(reg4)
linearHypothesis(reg4, c("dzieci_w_przedszkolach_na_1000:czy_miastomiasto"))
linearHypothesis(reg4, c("czy_miastomiasto"))
linearHypothesis(reg4, c("czy_miastomiasto", 
                         "dzieci_w_przedszkolach_na_1000:czy_miastomiasto"))

reg5 <- lm(aktywnosc_zawodowa_kobiet ~ zasilek_rodzinny_wspolczynnik + 
             dzieci_w_przedszkolach_na_1000 + 
             plodnosc + malzenstwa_na_1000 + 
             obciazenie_demograficzne + stopa_bezrobocia_ogolem + 
             wspolczynnik_wyksztalcenia + czy_miasto +
             wspolczynnik_wyksztalcenia:stopa_bezrobocia_ogolem + 
             swiadczenia_wychowawcze, data = dane2)
summary(reg5)
reg6 <- lm(aktywnosc_zawodowa_kobiet ~ zasilek_rodzinny_wspolczynnik + 
             dzieci_w_przedszkolach_na_1000 + 
             plodnosc + malzenstwa_na_1000 + 
             obciazenie_demograficzne + stopa_bezrobocia_ogolem + 
             wspolczynnik_wyksztalcenia + czy_miasto +
             czy_miasto:dzieci_w_przedszkolach_na_1000 +
             wspolczynnik_wyksztalcenia:stopa_bezrobocia_ogolem + 
             swiadczenia_wychowawcze, data = dane2)
summary(reg6)
reg6a <- lm(
  aktywnosc_zawodowa_kobiet ~ 
    zasilek_rodzinny_wspolczynnik +
    dzieci_w_przedszkolach_na_1000 +
    plodnosc +
    malzenstwa_na_1000 +
    obciazenie_demograficzne +
    stopa_bezrobocia_ogolem +
    I(stopa_bezrobocia_ogolem^2) +
    wspolczynnik_wyksztalcenia +
    czy_miasto +
    czy_miasto:dzieci_w_przedszkolach_na_1000 +
    wspolczynnik_wyksztalcenia:stopa_bezrobocia_ogolem +
    swiadczenia_wychowawcze,
  data = dane2
)
resettest(reg6a, power = 2:3, type = "fitted")
reg6b <- lm(
  aktywnosc_zawodowa_kobiet ~ 
    zasilek_rodzinny_wspolczynnik +
    dzieci_w_przedszkolach_na_1000 +
    plodnosc +
    malzenstwa_na_1000 + 
    I(malzenstwa_na_1000^2) +
    obciazenie_demograficzne + 
    stopa_bezrobocia_ogolem +
    I(stopa_bezrobocia_ogolem^2) +
    wspolczynnik_wyksztalcenia +
    czy_miasto +
    czy_miasto:dzieci_w_przedszkolach_na_1000 +
    wspolczynnik_wyksztalcenia:stopa_bezrobocia_ogolem +
    swiadczenia_wychowawcze,
  data = dane2
)
resettest(reg6b, power = 2:3, type = "fitted")
reg6c <- lm(
  aktywnosc_zawodowa_kobiet ~ 
    zasilek_rodzinny_wspolczynnik +
    dzieci_w_przedszkolach_na_1000 +
    plodnosc +
    malzenstwa_na_1000 + 
    I(malzenstwa_na_1000^2) +
    obciazenie_demograficzne + 
    I(obciazenie_demograficzne^2) +
    stopa_bezrobocia_ogolem +
    I(stopa_bezrobocia_ogolem^2) +
    wspolczynnik_wyksztalcenia +
    czy_miasto +
    czy_miasto:dzieci_w_przedszkolach_na_1000 +
    wspolczynnik_wyksztalcenia:stopa_bezrobocia_ogolem +
    swiadczenia_wychowawcze,
  data = dane2
)
summary(reg6c)
resettest(reg6c, power = 2:3, type = "fitted") #spelnia RESET, ale malzenstwa,
#malzenstwa ^2, miasto i obie interakcje okazuja sie nieistotne
linearHypothesis(reg6c, c("czy_miastomiasto"))
linearHypothesis(reg6c, c("malzenstwa_na_1000", "czy_miastomiasto"))
linearHypothesis(reg6c, c("malzenstwa_na_1000", "czy_miastomiasto",
                          "dzieci_w_przedszkolach_na_1000:czy_miastomiasto"))
linearHypothesis(reg6c, c("malzenstwa_na_1000", "czy_miastomiasto",
                          "stopa_bezrobocia_ogolem:wspolczynnik_wyksztalcenia"))
reg6d <- lm(
  aktywnosc_zawodowa_kobiet ~ 
    zasilek_rodzinny_wspolczynnik +
    dzieci_w_przedszkolach_na_1000 +
    plodnosc +
    obciazenie_demograficzne + 
    I(obciazenie_demograficzne^2) +
    stopa_bezrobocia_ogolem +
    I(stopa_bezrobocia_ogolem^2) +
    wspolczynnik_wyksztalcenia +
    swiadczenia_wychowawcze +
    czy_miasto:dzieci_w_przedszkolach_na_1000,
    data = dane2
)
summary(reg6d)
resettest(reg6d, power = 2:3, type = "fitted")
lmtest::bptest(reg6d)