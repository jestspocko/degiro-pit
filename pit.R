library(httr)
library(jsonlite)
library(data.table)
library(lubridate)
library(dplyr)
library(purrr)
library(tidyr)
# kursy walut -------------------------------------------------------------


eur_url <-  'http://api.nbp.pl/api/exchangerates/rates/A/eur/2019-12-30/2020-12-31/'


response <- GET(eur_url)
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
  select(produkt, datetime, liczba) %>% 
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


transakcje3 %>% 
  filter(produkt %like% 'WALT')

# kupno i sprzedaz ------------------------------------------------------
# cala sprzedaz z roku 2020 jest rozliczana

transakcje_sprzedaz <- long %>% 
  filter(znak == -1) %>% 
  mutate(czy_obecny_rok_rozl = 1)

# jesli na koniec roku mam x akcji, to nie uwzgledniam x ostatnich zakupionych akcji
transakcje_kupno <- long %>% 
  filter(znak == 1) %>% 
  left_join(balance_2020, by =  "produkt") %>% 
  mutate(row = row_number(),
         last_row = n()) %>% 
  mutate(czy_obecny_rok_rozl = ifelse(row <= (last_row - balance_2020),
                                      1,
                                      0)
         )
