library(httr)
library(jsonlite)
library(data.table)
library(lubridate)
library(dplyr)
library(purrr)
library(tidyr)
# kursy walut -------------------------------------------------------------
#kursy dla poprzedniego dnia roboczego
waluta <- 'eur'
od <- '2019-12-30'
do <- '2020-12-31'

get_kursy_poprz <- function(waluta, od, do){
  url <- sprintf('http://api.nbp.pl/api/exchangerates/rates/A/%s/%s/%s/', waluta,od,do)
  
  response <- GET(url)
  class(response)
  
  data
  
  res2 <- content(response, as = 'parsed' )
  class(res2)
  
  kursy <- bind_rows(map(res2$rates, bind_cols))
  kursy$effectiveDate = ymd(kursy$effectiveDate)
  
  #uzuepelnianie missing
  kursy2 <- kursy %>%
    arrange(effectiveDate) %>% 
    mutate(lag1 = lag(mid))  %>%
    complete(effectiveDate = seq.Date(min(effectiveDate), max(effectiveDate), by="day")) %>% 
    mutate(filled = lag1) %>% 
    tidyr::fill(filled, .direction = 'down') %>% 
    filter(row_number() != 1)
  
  kursy_poprz <- kursy2 %>% 
    select(data = effectiveDate, kurs_poprz = filled) %>% 
    mutate(waluta = toupper(waluta))
  
  kursy_poprz
}

kursy_walut <- bind_rows(
  get_kursy_poprz('eur', od, do),
  get_kursy_poprz('usd', od, do)
  )

# transakcje --------------------------------------------------------------


transakcje <- fread('./Transactions.csv')
transakcje <- janitor::clean_names(transakcje)

transakcje[,datetime := dmy_hms( paste(data, czas))]
transakcje[,data := dmy(data)]
transakcje <- transakcje[year(data) == 2020,]

produkty <- unique(transakcje$produkt)

transakcje2 <- transakcje %>% 
  filter(produkt != 'EUR/PLN') %>% 
  arrange(produkt, datetime ) %>% 
  group_by(produkt) %>% 
  mutate(cumsum = cumsum(liczba))

transakcje2

transakcje3 <- transakcje2 %>% 
  select(produkt, data, datetime, liczba, kurs, waluta = v9) %>% 
  mutate(times = abs(liczba), znak = ifelse(liczba <= 0, -1, 1))

transakcje3

ind <- map2(1:nrow(transakcje3),transakcje3$times,  ~rep(.x, times = .y))
ind2 <- unlist(ind)

#rozwiniecie do wiersz per akcja
long <- transakcje3 %>% 
  ungroup() %>% 
  dplyr::slice(ind2) %>% 
  arrange(produkt, datetime) %>% 
  group_by(produkt) %>% 
  mutate(cumsum = cumsum(znak)) %>% 
  select( -liczba, -times)

  balance_2020 <- long %>% 
    group_by(produkt) %>% 
    slice(n()) %>% 
    select(produkt, balance_2020 = cumsum)
  balance_2020$balance_2020 %>% sum()
 
long2 <- long %>% 
  group_by(znak, produkt) %>% 
  mutate(nr_tr = row_number()) %>% 
  left_join(kursy_walut, by = c("data","waluta")) %>% 
  mutate(kurs_poprz = ifelse(waluta == 'PLN',1,kurs_poprz))
####


####
sprzedaz <- long2 %>% 
  ungroup() %>% 
  filter(znak == -1)  %>% 
  select(-znak)

kupno <- long2 %>% 
  ungroup() %>% 
  filter(znak == 1) %>% 
  select(produkt, data_kupna = data, nr_tr, kurs_kupna = kurs, kurs_poprz_kupna = kurs_poprz)

zestawienie <- sprzedaz %>% 
  left_join(kupno, by = c("produkt", "nr_tr")) %>% 
  mutate(sprzedaz_pln = kurs * kurs_poprz, kupno_pln =  kurs_kupna * kurs_poprz) %>% 
  mutate(zysk_pln = sprzedaz_pln - kupno_pln)

podsumowanie_prod <- zestawienie %>% 
  group_by(produkt) %>% 
  summarize(zysk_pln = sum(zysk_pln), dochod_pln = sum(sprzedaz_pln), koszt_pln = sum(kupno_pln))

podsumowanie <- zestawienie %>% 
  ungroup() %>% 
  summarize(zysk_pln = sum(zysk_pln), sprzedaz_pln = sum(sprzedaz_pln), koszt_pln = sum(kupno_pln))

podsumowanie


oplaty <- transakcje %>% 
  select(data, op_l_ata_transakcyjna) %>% 
  left_join(kursy_walut, by = "data") %>% 
  mutate(kurs_poprz = ifelse(waluta == 'PLN',1,kurs_poprz)) %>% 
  mutate(oplata_pln = op_l_ata_transakcyjna * kurs_poprz)
#dodatkowe oplaty
sum(oplaty$oplata_pln, na.rm = T)



# DYWIDENDY ---------------------------------------------------------------

rachunek <- fread("./Account.csv", header = T) 


rachunek[tolower(Opis) %like% 'dywi']
